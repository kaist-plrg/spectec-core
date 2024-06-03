open Syntax.Ast
open Runtime.Domain
open Runtime.Base
open Runtime.Object
open Runtime.Context

(* Global store *)

let sto = ref Sto.empty
let init _sto = sto := _sto

(* Helper to fetch "apply" for pbl/table or extern method *)

let fetch_extern_method (func : Func.t) =
  match func with
  | ExternF { vis_obj; tparams; params } -> (vis_obj, tparams, params)
  | _ -> assert false

let fetch_pbl_method (func : Func.t) =
  match func with
  | MethodF { vis_obj; tparams; params; body } ->
      (vis_obj, tparams, params, body)
  | _ -> assert false

let fetch_action (func : Func.t) =
  match func with
  | ActionF { vis_obj; params; body } -> (vis_obj, params, body)
  | _ -> assert false

let fetch_table_method (func : Func.t) =
  match func with TableF { vis_obj } -> vis_obj | _ -> assert false

(* Helper to access aggregate value *)

let fetch_struct_field (value : Value.t) (field : string) =
  match value with
  | StructV fields -> List.assoc field fields
  | _ -> assert false

let fetch_enum_member (value : Value.t) =
  match value with EnumFieldV member -> member | _ -> assert false

(* Interpreter for expressions
   assume: evaluation of bit width should never change the context *)

let rec interp_type (ctx : Ctx.t) (typ : typ) : Type.t =
  match typ with
  | BoolT -> BoolT
  | ErrT -> ErrT
  | StrT -> StrT
  | AIntT -> AIntT
  | IntT width ->
      let width = interp_expr ctx width |> snd |> Runtime.Ops.extract_bigint in
      IntT width
  | BitT width ->
      let width = interp_expr ctx width |> snd |> Runtime.Ops.extract_bigint in
      BitT width
  | VBitT width ->
      let width = interp_expr ctx width |> snd |> Runtime.Ops.extract_bigint in
      VBitT width
  | NameT (Top name) -> Ctx.find_td_glob name ctx |> Option.get
  | NameT (Bare name) -> Ctx.find_td name ctx |> Option.get
  (* (TODO) Handle specialized types *)
  | SpecT (name, _) -> interp_type ctx (NameT name)
  | TupleT typs ->
      let typs = List.map (interp_type ctx) typs in
      TupleT typs
  | _ ->
      Format.asprintf "(TODO: eval_type) %a" Syntax.Print.print_type typ
      |> failwith

and interp_expr (ctx : Ctx.t) (expr : expr) : Ctx.t * Value.t =
  match expr with
  | BoolE b -> (ctx, BoolV b)
  | StrE str -> (ctx, StrV str)
  | NumE (value, width_signed) ->
      let value =
        match width_signed with
        | Some (width, signed) ->
            if signed then Value.IntV (width, value)
            else Value.BitV (width, value)
        | None -> AIntV value
      in
      (ctx, value)
  | VarE (Top name) ->
      let value = Ctx.find_var_glob name ctx |> Option.get |> snd in
      (ctx, value)
  | VarE (Bare name) ->
      let value = Ctx.find_var name ctx |> Option.get |> snd in
      (ctx, value)
  | ListE values ->
      let ctx, values = interp_exprs ctx values in
      let value = Value.TupleV values in
      (ctx, value)
  | RecordE fields ->
      let names = List.map fst fields in
      let ctx, values = List.map snd fields |> interp_exprs ctx in
      let fields = List.map2 (fun name value -> (name, value)) names values in
      let value = Value.StructV fields in
      (ctx, value)
  | UnE (op, arg) ->
      let ctx, varg = interp_expr ctx arg in
      let value = Runtime.Ops.eval_unop op varg in
      (ctx, value)
  | BinE (op, arg_fst, arg_snd) ->
      let ctx, value_fst = interp_expr ctx arg_fst in
      let ctx, value_snd = interp_expr ctx arg_snd in
      let value = Runtime.Ops.eval_binop op value_fst value_snd in
      (ctx, value)
  | CastE (typ, arg) ->
      let typ = interp_type ctx typ |> Eval.eval_simplify_type ctx in
      let ctx, value = interp_expr ctx arg in
      let value = Runtime.Ops.eval_cast typ value in
      (ctx, value)
  | ExprAccE (base, name) -> (
      let ctx, value_base = interp_expr ctx base in
      match value_base with
      | HeaderV (_, fields) | StructV fields ->
          let value = List.assoc name fields in
          (ctx, value)
      | _ -> assert false)
  | CallE (func, targs, args) ->
      let ctx, value = interp_call ctx func targs args in
      let value = Option.get value in
      (ctx, value)
  | InstE _ ->
      Format.eprintf
        "(interp_expr) Instantiation expression should have been evaluated in \
         instantiation.";
      assert false
  | TernE _ | MaskE _ | RangeE _ | ArrAccE _ | BitAccE _ | TypeAccE _
  | ErrAccE _ ->
      Format.asprintf "(TODO: interp_expr) %a" Syntax.Print.print_expr expr
      |> failwith

and interp_exprs (ctx : Ctx.t) (exprs : expr list) : Ctx.t * Value.t list =
  List.fold_left
    (fun (ctx, values) expr ->
      let ctx, value = interp_expr ctx expr in
      (ctx, values @ [ value ]))
    (ctx, []) exprs

(* Interpreter for declarations *)

and interp_decl (ctx : Ctx.t) (decl : decl) =
  match decl with
  | VarD { name; typ; init = None } ->
      let typ = interp_type ctx typ in
      let value = Runtime.Ops.eval_default_value typ in
      Ctx.add_var_loc name typ value ctx
  | VarD { name; typ; init = Some value } ->
      let typ = interp_type ctx typ in
      let ctx, value = interp_expr ctx value in
      Ctx.add_var_loc name typ value ctx
  | _ ->
      Format.eprintf "(TODO: interp_decl) %a" Syntax.Print.print_decl (0, decl);
      assert false

(* Interpreter for statements *)

(* lvalue
   : prefixedNonTypeName | lvalue "." member
   | lvalue "[" expression "]" | lvalue "[" expression ":" expression "]" *)
and interp_write (ctx : Ctx.t) (lvalue : expr) (value : Value.t) =
  match lvalue with
  | VarE (Bare name) ->
      let typ = Ctx.find_var name ctx |> Option.get |> fst in
      let value = Runtime.Ops.eval_cast typ value in
      Ctx.update_var name typ value ctx
  | ExprAccE (base, name) -> (
      let ctx, value_base = interp_expr ctx base in
      let update_field fields name =
        List.map (fun (k, v) -> if k = name then (k, value) else (k, v)) fields
      in
      match value_base with
      | StructV fields ->
          let fields = update_field fields name in
          interp_write ctx base (StructV fields)
      | HeaderV (valid, fields) ->
          let fields = update_field fields name in
          interp_write ctx base (HeaderV (valid, fields))
      | _ -> assert false)
  | _ ->
      Format.eprintf "(TODO: interp_write) %a" Syntax.Print.print_expr lvalue;
      assert false

and interp_stmt (ctx : Ctx.t) (stmt : stmt) =
  match stmt with
  | EmptyI -> ctx
  | AssignI (lhs, rhs) -> interp_assign ctx lhs rhs
  | IfI (cond, tru, fls) -> interp_if ctx cond tru fls
  | BlockI block -> interp_block ctx block
  | CallI (func, targs, args) -> interp_call ctx func targs args |> fst
  | TransI next -> interp_trans ctx next
  | SelectI (exprs, cases) -> interp_select ctx exprs cases
  | DeclI decl -> interp_decl ctx decl
  | SwitchI (expr, cases) -> interp_switch ctx expr cases
  | ExitI | RetI _ ->
      Format.eprintf "(TODO: interp_stmt) %a" Syntax.Print.print_stmt (0, stmt);
      assert false

and interp_assign (ctx : Ctx.t) (lhs : expr) (rhs : expr) =
  let ctx, value = interp_expr ctx rhs in
  interp_write ctx lhs value

and interp_if (ctx : Ctx.t) (cond : expr) (tru : stmt) (fls : stmt) =
  let ctx, cond = interp_expr ctx cond in
  let cond = Runtime.Ops.eval_cast Type.BoolT cond in
  let cond = match cond with BoolV b -> b | _ -> assert false in
  if cond then interp_stmt ctx tru else interp_stmt ctx fls

and interp_block (ctx : Ctx.t) (block : block) =
  let ctx = Ctx.enter_frame ctx in
  let ctx = List.fold_left interp_stmt ctx block in
  Ctx.exit_frame ctx

(* (TODO) For state transitions, do not change object visibility,
   treating them as real "transitions", because states can be mutually recursive *)
and interp_trans (ctx : Ctx.t) (next : string) =
  if next = "accept" || next = "reject" then ctx
  else
    let state_next = Ctx.find_func next ctx |> Option.get in
    let body =
      match state_next with StateF { body } -> body | _ -> assert false
    in
    let ctx_next =
      let env_loc = (TDEnv.empty, []) in
      { ctx with env_loc }
    in
    let ctx_next = interp_block ctx_next body in
    { ctx with env_obj = ctx_next.env_obj }

(* assume: evaluation of match case should never change the context *)
and interp_select (ctx : Ctx.t) (exprs : expr list) (cases : select_case list) =
  let ctx, values =
    List.fold_left
      (fun (ctx, values) expr ->
        let ctx, value = interp_expr ctx expr in
        (ctx, value :: values))
      (ctx, []) exprs
  in
  let select_cases (next_found : string option) (case : select_case) =
    match next_found with
    | Some _ -> next_found
    | None ->
        let mtchs, next = case in
        let select_mtch (mtch : mtch) (value : Value.t) =
          match mtch with
          | DefaultM | AnyM -> true
          | ExprM expr -> interp_expr ctx expr |> snd = value
        in
        if List.for_all2 select_mtch mtchs values then Some next else None
  in
  let next = List.fold_left select_cases None cases |> Option.get in
  interp_trans ctx next

(* (TODO) assume switch on table apply result only,
   for case with expression is not supported in Petr4 parser *)
and interp_switch (ctx : Ctx.t) (expr : expr) (cases : switch_case list) =
  let ctx, value = interp_expr ctx expr in
  let value = fetch_enum_member value in
  let switch_cases (block_found : bool * block option) (case : switch_case) =
    let case, block = case in
    match block_found with
    (* match complete *)
    | true, Some _ -> block_found
    (* during fallthrough *)
    | true, None -> (
        match case with
        | CaseC _ -> (true, Some block)
        | FallC _ -> (true, None)
        | DefaultC -> (true, Some block))
    (* match not found *)
    | false, _ -> (
        match case with
        | CaseC case when case = value -> (true, Some block)
        | FallC case when case = value -> (true, None)
        | DefaultC -> (true, Some block)
        | _ -> block_found)
  in
  let _, block = List.fold_left switch_cases (false, None) cases in
  let block = match block with None -> [] | Some block -> block in
  interp_block ctx block

(* Call semantics *)

(* align parameters by argument order *)
and align_params_with_args (params : param list) (args : arg list) =
  (* (TODO) assume there is no default argument *)
  assert (List.length params = List.length args);
  Instance.Instantiate.check_args args;
  let module PMap = Map.Make (String) in
  let params_map =
    List.fold_left
      (fun params_map param ->
        let pname, _, _, _ = param in
        PMap.add pname param params_map)
      PMap.empty params
  in
  List.fold_left2
    (fun (params, args) param arg ->
      match arg with
      | ExprA arg -> (params @ [ param ], args @ [ arg ])
      | NameA (pname, arg) ->
          let param = PMap.find pname params_map in
          (params @ [ param ], args @ [ arg ])
      | _ ->
          Format.eprintf "(TODO: align_params_with_args) %a"
            Syntax.Print.print_arg arg;
          assert false)
    ([], []) params args

(* adder determines where to add the argument, either object or local scope *)
(* (TODO; MAJOR) don't evaluate and copyin one-by-one,
   instead, evaluate all arguments first and then copyin
   because the evaluation of arguments can have side-effects on ctx_caller
   so ctx_callee can be safely constructed only after all arguments are evaluated *)
and copyin adder (ctx_callee : Ctx.t) (params : param list)
    (values : Value.t list) =
  let copyin' (ctx_callee : Ctx.t) (param : param) (value : Value.t) =
    let pname, dir, typ, _ = param in
    (* (TODO) Is it correct to evaluate the type at callee? *)
    match dir with
    | No | In | InOut ->
        let typ = interp_type ctx_callee typ in
        adder pname typ value ctx_callee
    | Out ->
        let typ = interp_type ctx_callee typ in
        adder pname typ value ctx_callee
  in
  List.fold_left2 copyin' ctx_callee params values

and copyout (ctx_caller : Ctx.t) (ctx_callee : Ctx.t) (params : param list)
    (exprs : expr list) =
  let copyout' (ctx_caller : Ctx.t) (param : param) (expr : expr) =
    let pname, dir, _, _ = param in
    match dir with
    | InOut | Out ->
        let value = Ctx.find_var pname ctx_callee |> Option.get |> snd in
        interp_write ctx_caller expr value
    | _ -> ctx_caller
  in
  List.fold_left2 copyout' ctx_caller params exprs

(* adder determines where to add the type argument, either object or local scope *)
and interp_targs adder (ctx_caller : Ctx.t) (ctx_callee : Ctx.t)
    (tparams : string list) (targs : typ list) =
  assert (List.length tparams = List.length targs);
  List.fold_left2
    (fun ctx_callee tparam targ ->
      let targ = interp_type ctx_caller targ in
      adder tparam targ ctx_callee)
    ctx_callee tparams targs

and interp_args (ctx_caller : Ctx.t) (exprs : expr list) =
  interp_exprs ctx_caller exprs

and interp_extern_method_call (ctx_caller : Ctx.t) (ctx_callee : Ctx.t)
    (mname : string) (tparams : Var.t list) (params : param list)
    (targs : typ list) (args : arg list) =
  (* Enter the function frame *)
  let ctx_callee = Ctx.enter_frame ctx_callee in
  (* Align the parameters with the arguments *)
  let params, args = align_params_with_args params args in
  (* Evaluate the arguments *)
  let ctx_caller, values = interp_args ctx_caller args in
  (* Bind the type parameters *)
  let ctx_callee =
    interp_targs Ctx.add_td_loc ctx_caller ctx_callee tparams targs
  in
  (* Copy-in to the object environment *)
  let ctx_callee = copyin Ctx.add_var_loc ctx_callee params values in
  (* Execute the body *)
  let ctx_callee = Core.interp_builtin ctx_callee mname in
  (* Copy-out from the object environment *)
  let ctx_caller = copyout ctx_caller ctx_callee params args in
  (* No need to account for global mutations since globals are immutable *)
  (ctx_caller, None)

and interp_pbl_apply_call (ctx_caller : Ctx.t) (ctx_callee : Ctx.t)
    (_tparams : Var.t list) (params : param list) (_targs : typ list)
    (args : arg list) (body : block) =
  (* Align the parameters with the arguments *)
  let params, args = align_params_with_args params args in
  (* Evaluate the arguments *)
  let ctx_caller, values = interp_args ctx_caller args in
  (* Copy-in to the object environment *)
  let ctx_callee = copyin Ctx.add_var_obj ctx_callee params values in
  (* Execute the body *)
  let ctx_callee = interp_block ctx_callee body in
  (* Copy-out from the object environment *)
  let ctx_caller = copyout ctx_caller ctx_callee params args in
  (* No need to account for global mutations since globals are immutable *)
  (ctx_caller, None)

and interp_action_call (ctx_caller : Ctx.t) (ctx_callee : Ctx.t)
    (params : param list) (args : arg list) (body : block) =
  (* Enter the function frame *)
  let ctx_callee = Ctx.enter_frame ctx_callee in
  (* Align the parameters with the arguments *)
  let params, args = align_params_with_args params args in
  (* Evaluate the arguments *)
  let ctx_caller, values = interp_args ctx_caller args in
  (* Copy-in to the local environment *)
  let ctx_callee = copyin Ctx.add_var_loc ctx_callee params values in
  (* Execute the body *)
  let ctx_callee = interp_block ctx_callee body in
  (* Copy-out from the local environment *)
  let ctx_caller = copyout ctx_caller ctx_callee params args in
  (* Account for object-local mutations *)
  let ctx_caller = { ctx_caller with env_obj = ctx_callee.env_obj } in
  (ctx_caller, None)

and interp_table_apply_call (ctx_caller : Ctx.t) (ctx_callee : Ctx.t)
    (key : table_key list) (actions : table_action list)
    (entries : table_entry list) (default : table_default option)
    (custom : table_custom list) =
  (* Invoke the match-action table to get an action *)
  let ctx_caller, key =
    List.fold_left
      (fun (ctx_caller, key) (expr, mtch) ->
        let ctx_caller, value = interp_expr ctx_caller expr in
        (ctx_caller, key @ [ (value, mtch) ]))
      (ctx_caller, []) key
  in
  let action, value =
    Control.match_action ctx_callee key actions entries default custom
  in
  let hit =
    match fetch_struct_field value "hit" with BoolV b -> b | _ -> assert false
  in
  let value = Some value in
  if not hit then (ctx_caller, value)
  else
    let action, args = Option.get action in
    (* Find the action *)
    (* (TODO) support inter-object call to global action *)
    let action_name =
      match action with
      | Top action ->
          Format.eprintf
            "(TODO: interp_table_call) Implement top-level action call to %s\n"
            action;
          assert false
      | Bare action -> action
    in
    let action = Ctx.find_func_obj action_name ctx_callee |> Option.get in
    let vis_obj, params, body = fetch_action action in
    (* Construct the callee context *)
    let ctx_callee_callee =
      Ctx.init ctx_callee.env_glob ctx_callee.env_obj (TDEnv.empty, [])
    in
    let ctx_callee_callee = { ctx_callee_callee with vis_obj } in
    (* Execute the action *)
    let ctx_callee, _ =
      interp_action_call ctx_callee ctx_callee_callee params args body
    in
    (* Propagate object-local mutations *)
    let ctx_caller = { ctx_caller with env_obj = ctx_callee.env_obj } in
    (ctx_caller, value)

and interp_object_method_call (ctx : Ctx.t) (obj : Object.t) (mname : Var.t)
    (targs : typ list) (args : arg list) =
  (* Difference between calling an extern method and a parser/control method
     is that parameters are passed to the method-local scope for the former
     and for the latter, it is the object-local scope *)
  match obj with
  | ExternO { vis_glob; env_obj } ->
      (* Construct the callee context *)
      let ctx_callee = Ctx.init ctx.env_glob env_obj (TDEnv.empty, []) in
      (* Find the method *)
      let mthd = Ctx.find_func mname ctx_callee |> Option.get in
      let vis_obj, tparams, params = fetch_extern_method mthd in
      (* Restrict the callee's visibility *)
      let ctx_callee = { ctx_callee with vis_glob; vis_obj } in
      (* Evaluate the body *)
      interp_extern_method_call ctx ctx_callee mname tparams params targs args
  | ParserO { vis_glob; env_obj; mthd } | ControlO { vis_glob; env_obj; mthd }
    ->
      (* Construct the callee context *)
      let ctx_callee = Ctx.init ctx.env_glob env_obj (TDEnv.empty, []) in
      (* Find the method *)
      assert (mname = "apply");
      let vis_obj, tparams, params, body = fetch_pbl_method mthd in
      (* Restrict the callee's visibility *)
      let ctx_callee = { ctx_callee with vis_glob; vis_obj } in
      (* Evaluate the body *)
      interp_pbl_apply_call ctx ctx_callee tparams params targs args body
  | TableO { key; actions; entries; default; custom; mthd } ->
      (* Construct the callee context *)
      let ctx_callee = Ctx.init ctx.env_glob ctx.env_obj (TDEnv.empty, []) in
      (* Find the method *)
      assert (mname = "apply" && targs = [] && args = []);
      let vis_obj = fetch_table_method mthd in
      (* Restrict the callee's visibility *)
      let ctx_callee = { ctx_callee with vis_obj } in
      (* Evaluate the body *)
      interp_table_apply_call ctx ctx_callee key actions entries default custom
  | _ ->
      Format.eprintf "(TODO: interp_inter_call) %a\n" Object.pp obj;
      assert false

and interp_method_call (ctx : Ctx.t) (value : Value.t) (mname : string)
    (targs : typ list) (args : arg list) =
  match value with
  | HeaderV (valid, _) when mname = "isValid" ->
      let value = Some (Value.BoolV valid) in
      (ctx, value)
  | RefV path ->
      let obj = Sto.find path !sto |> Option.get in
      interp_object_method_call ctx obj mname targs args
  | _ ->
      Format.eprintf "(TODO: interp_method_call) %a with %s\n" Value.pp value
        mname;
      assert false

and interp_call (ctx : Ctx.t) (func : expr) (targs : typ list) (args : arg list)
    =
  match func with
  (* method call *)
  | ExprAccE (expr, mname) ->
      let ctx, value = interp_expr ctx expr in
      interp_method_call ctx value mname targs args
  (* (TODO) function call *)
  | _ -> assert false
