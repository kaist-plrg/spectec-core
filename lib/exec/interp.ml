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
  match func with
  | TableF { vis_obj } -> vis_obj
  | _ -> assert false

(* Interpreter for declarations *)

let interp_decl (ctx : Ctx.t) (decl : decl) =
  match decl with
  | VarD { name; typ; init = None } ->
      let typ = Eval.eval_type ctx typ in
      let value = Runtime.Ops.eval_default_value typ in
      Ctx.add_var_loc name typ value ctx
  | VarD { name; typ; init = Some value } ->
      let typ = Eval.eval_type ctx typ in
      let value = Eval.eval_expr ctx value in
      Ctx.add_var_loc name typ value ctx
  | _ ->
      Format.eprintf "(TODO: interp_decl) %a" Syntax.Print.print_decl (0, decl);
      assert false

(* Interpreter for statements *)

(* lvalue
   : prefixedNonTypeName | lvalue "." member
   | lvalue "[" expression "]" | lvalue "[" expression ":" expression "]" *)
let rec interp_write (ctx : Ctx.t) (lvalue : expr) (value : Value.t) =
  match lvalue with
  | VarE (Bare name) ->
      let typ = Ctx.find_var name ctx |> Option.get |> fst in
      let value = Runtime.Ops.eval_cast typ value in
      Ctx.update_var name typ value ctx
  | ExprAccE (base, name) -> (
      let vbase = Eval.eval_expr ctx base in
      match vbase with
      | StructV fields ->
          let fields =
            List.map
              (fun (k, v) -> if k = name then (k, value) else (k, v))
              fields
          in
          interp_write ctx base (StructV fields)
      | _ -> assert false)
  | _ ->
      Format.eprintf "(TODO: interp_write) %a" Syntax.Print.print_expr lvalue;
      assert false

let rec interp_stmt (ctx : Ctx.t) (stmt : stmt) =
  match stmt with
  | EmptyI -> ctx
  | AssignI (lhs, rhs) -> interp_assign ctx lhs rhs
  | IfI (cond, tru, fls) -> interp_if ctx cond tru fls
  | BlockI block -> interp_block ctx block
  | CallI (func, targs, args) -> interp_call ctx func targs args
  | TransI next -> interp_trans ctx next
  | SelectI (exprs, cases) -> interp_select ctx exprs cases
  | DeclI decl -> interp_decl ctx decl
  | SwitchI (expr, cases) -> interp_switch ctx expr cases
  | ExitI | RetI _ ->
      Format.eprintf "(TODO: interp_stmt) %a" Syntax.Print.print_stmt (0, stmt);
      assert false

and interp_assign (ctx : Ctx.t) (lhs : expr) (rhs : expr) =
  let value = Eval.eval_expr ctx rhs in
  interp_write ctx lhs value

and interp_if (ctx : Ctx.t) (cond : expr) (tru : stmt) (fls : stmt) =
  let cond = Eval.eval_expr ctx cond |> Runtime.Ops.eval_cast Type.BoolT in
  let cond = match cond with BoolV b -> b | _ -> assert false in
  if cond then interp_stmt ctx tru else interp_stmt ctx fls

and interp_block (ctx : Ctx.t) (block : block) =
  let ctx = Ctx.enter_frame ctx in
  let ctx = List.fold_left interp_stmt ctx block in
  Ctx.exit_frame ctx

and interp_call (ctx : Ctx.t) (func : expr) (targs : typ list) (args : arg list) =
  match func with
  | ExprAccE (ref, mname) ->
      let ref = Eval.eval_expr ctx ref in
      let path = match ref with RefV path -> path | _ -> assert false in
      let obj = Sto.find path !sto |> Option.get in
      interp_method_call ctx obj mname targs args
  | _ -> assert false

(* (TODO) For state transitions, do not change object visibility,
   treating them as real "transitions", because states can be mutually recursive *)
and interp_trans (ctx : Ctx.t) (next : string) =
  if next = "accept" || next = "reject" then ctx
  else
    let state_next = Ctx.find_func next ctx |> Option.get in
    let body = match state_next with StateF { body } -> body | _ -> assert false in
    let ctx_next =
      let env_loc = (TDEnv.empty, []) in
      { ctx with env_loc }
    in
    let ctx_next = interp_block ctx_next body in
    { ctx with env_obj = ctx_next.env_obj }

and interp_select (ctx : Ctx.t) (exprs : expr list) (cases : select_case list) =
  let values = List.map (fun expr -> Eval.eval_expr ctx expr) exprs in
  let select_cases (next_found : string option) (case : select_case) =
    match next_found with
    | Some _ -> next_found
    | None ->
        let mtchs, next = case in
        let select_mtch (mtch : mtch) (value : Value.t) =
          match mtch with
          | DefaultM | AnyM -> true
          | ExprM expr -> Eval.eval_expr ctx expr = value
        in
        if List.for_all2 select_mtch mtchs values then Some next else None
  in
  let next = List.fold_left select_cases None cases |> Option.get in
  interp_trans ctx next

(* (TODO) assume switch on table apply result only,
   for case with expression is not supported in Petr4 parser *)
and interp_switch (ctx : Ctx.t) (expr : expr) (cases : switch_case list) =
  let value = Eval.eval_expr ctx expr in
  let value = match value with StrV s -> s | _ -> assert false in
  let switch_cases (block_found : (bool * block option)) (case : switch_case) =
    let case, block = case in
    match block_found with
    (* match complete *)
    | (true, Some _) -> block_found
    (* during fallthrough *)
    | (true, None) -> (
        match case with
        | CaseC _ -> (true, Some block)
        | FallC _ -> (true, None)
        | DefaultC -> (true, Some block))
    (* match not found *)
    | (false, _) -> (
        match case with
        | CaseC case when case = value -> (true, Some block)
        | FallC case when case = value -> (true, None)
        | DefaultC -> (true, Some block)
        | _ -> block_found)
  in
  let _, block = List.fold_left switch_cases (false, None) cases in
  let block = match block with None -> [] | Some block -> block in
  interp_block ctx block

(* adder determines where to add the type argument, either object or local scope *)
and interp_targs adder (ctx_caller : Ctx.t) (ctx_callee : Ctx.t)
    (tparams : string list) (targs : typ list) =
  assert (List.length tparams = List.length targs);
  List.fold_left2
    (fun ctx_callee tparam targ ->
      let targ = Eval.eval_type ctx_caller targ in
      adder tparam targ ctx_callee)
    ctx_callee tparams targs

(* adder determines where to add the argument, either object or local scope *)
and copyin adder (ctx_caller : Ctx.t) (ctx_callee : Ctx.t) (params : param list)
    (args : arg list) =
  (* (TODO) Assume there is no default argument *)
  assert (List.length params = List.length args);
  Instance.Instantiate.check_args args;
  let copyin' (ctx_callee : Ctx.t) (param : param) (arg : arg) =
    (* Resolve the argument-parameter order *)
    let pname, dir, typ, _ = param in
    let pname, value =
      match arg with
      | ExprA value -> (pname, value)
      | NameA (pname, value) -> (pname, value)
      | _ ->
          Format.eprintf "(TODO: copyin) %a" Syntax.Print.print_arg arg;
          assert false
    in
    (* (TODO) Is it correct to evaluate the type at callee? *)
    match dir with
    | No | In | InOut ->
        let typ = Eval.eval_type ctx_callee typ in
        let value = Eval.eval_expr ctx_caller value in
        adder pname typ value ctx_callee
    | Out ->
        let typ = Eval.eval_type ctx_callee typ in
        let value = Runtime.Ops.eval_default_value typ in
        adder pname typ value ctx_callee
  in
  List.fold_left2 copyin' ctx_callee params args

and copyout (ctx_caller : Ctx.t) (ctx_callee : Ctx.t) (params : param list)
    (args : arg list) =
  let copyout' (ctx_caller : Ctx.t) (param : param) (arg : arg) =
    let pname, dir, _, _ = param in
    let pname, lvalue =
      match arg with
      | ExprA value -> (pname, value)
      | NameA (pname, value) -> (pname, value)
      | _ ->
          Format.eprintf "(TODO: copyout) %a" Syntax.Print.print_arg arg;
          assert false
    in
    match dir with
    | InOut | Out ->
        let value = Ctx.find_var pname ctx_callee |> Option.get |> snd in
        interp_write ctx_caller lvalue value
    | _ -> ctx_caller
  in
  List.fold_left2 copyout' ctx_caller params args

and interp_extern_call (ctx_caller : Ctx.t) (ctx_callee : Ctx.t)
    (mname : string) (tparams : Var.t list) (params : param list)
    (targs : typ list) (args : arg list) =
  (* Enter the function frame *)
  let ctx_callee = Ctx.enter_frame ctx_callee in
  (* Bind the type parameters *)
  let ctx_callee =
    interp_targs Ctx.add_td_loc ctx_caller ctx_callee tparams targs
  in
  (* Copy-in to the object environment *)
  let ctx_callee = copyin Ctx.add_var_loc ctx_caller ctx_callee params args in
  (* Execute the body *)
  let ctx_callee = Core.interp_builtin ctx_callee mname in
  (* Copy-out from the object environment *)
  copyout ctx_caller ctx_callee params args

and interp_pbl_call (ctx_caller : Ctx.t) (ctx_callee : Ctx.t)
    (_tparams : Var.t list) (params : param list) (_targs : typ list)
    (args : arg list) (body : block) =
  (* Copy-in to the object environment *)
  let ctx_callee = copyin Ctx.add_var_obj ctx_caller ctx_callee params args in
  (* Execute the body *)
  let ctx_callee = interp_block ctx_callee body in
  (* Copy-out from the object environment *)
  copyout ctx_caller ctx_callee params args

and interp_action_call (ctx_caller : Ctx.t) (ctx_callee : Ctx.t)
    (params : param list) (args : arg list) (body : block) =
  (* Enter the function frame *)
  let ctx_callee = Ctx.enter_frame ctx_callee in
  (* Copy-in to the local environment *)
  let ctx_callee = copyin Ctx.add_var_loc ctx_caller ctx_callee params args in
  (* Execute the body *)
  let ctx_callee = interp_block ctx_callee body in
  (* Copy-out from the local environment *)
  let ctx_caller = copyout ctx_caller ctx_callee params args in
  (* Account for object-local mutations *)
  { ctx_caller with env_obj = ctx_callee.env_obj }

and interp_table_call (ctx_caller : Ctx.t) (ctx_callee : Ctx.t)
    (key : table_key list) (actions : table_action list) (entries : table_entry list)
    (default : table_default option) (custom : table_custom list) =
  (* Invoke the match-action table to get an action *)
  let action = Control.match_action ctx_callee key actions entries default custom in
  match action with
  | None -> ctx_caller
  | Some (action, args) ->
    (* Find the action *)
    (* (TODO) support inter-object call to global action *)
    let action_name =
      (match action with
      | Top action ->
          Format.eprintf "(TODO: interp_table_call) Implement top-level action call to %s\n" action;
          assert false
      | Bare action -> action)
    in
    let action = Ctx.find_func_obj action_name ctx_callee |> Option.get in
    let vis_obj, params, body = fetch_action action in
    (* Construct the callee context *)
    let ctx_callee_callee =
      Ctx.init ctx_callee.env_glob ctx_callee.env_obj (TDEnv.empty, [])
    in
    let ctx_callee_callee = { ctx_callee_callee with vis_obj } in
    (* Execute the action *)
    let ctx_callee = interp_action_call ctx_callee ctx_callee_callee params args body in
    (* Account for object-local mutations *)
    { ctx_caller with env_obj = ctx_callee.env_obj }

(* Entry point of a P4 object method execution, including "apply" on programmable block *)

and interp_method_call (ctx : Ctx.t) (obj : Object.t) (mname : Var.t)
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
      interp_extern_call ctx ctx_callee mname tparams params targs args
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
      interp_pbl_call ctx ctx_callee tparams params targs args body
  | TableO { key; actions; entries; default; custom; mthd } ->
      (* Construct the callee context *)
      let ctx_callee = Ctx.init ctx.env_glob ctx.env_obj (TDEnv.empty, []) in
      (* Find the method *)
      assert (mname = "apply" && targs = [] && args = []);
      let vis_obj = fetch_table_method mthd in
      (* Restrict the callee's visibility *)
      let ctx_callee = { ctx_callee with vis_obj } in
      (* Evaluate the body *)
      interp_table_call ctx ctx_callee key actions entries default custom
  | _ ->
      Format.eprintf "(TODO: interp_inter_call) %a\n" Object.pp obj;
      assert false
