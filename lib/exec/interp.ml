open Syntax.Ast
open Runtime.Domain
open Runtime.Base
open Runtime.Object
open Runtime.Context

(* Global environment *)

let gctx = ref GCtx.empty
let init (_gctx : GCtx.t) = gctx := _gctx

(* Helper to fetch "apply" or extern method *)

let fetch_apply_method (func : Func.t) =
  match func with
  | MethodF { vis_obj; tparams; params; body } ->
      (vis_obj, tparams, params, body)
  | _ -> assert false

let fetch_extern_method (func : Func.t) =
  match func with
  | ExternF { vis_obj; tparams; params } -> (vis_obj, tparams, params)
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
  | AssignI (lhs, rhs) ->
      let value = Eval.eval_expr ctx rhs in
      interp_write ctx lhs value
  | BlockI block -> interp_block ctx block
  | CallI (func, targs, args) ->
      let obj, mname =
        match func with
        | ExprAccE (ref, mname) ->
            let ref = Eval.eval_expr ctx ref in
            let path = match ref with RefV path -> path | _ -> assert false in
            let obj = GCtx.find_obj path !gctx |> Option.get in
            (obj, mname)
        | _ -> assert false
      in
      interp_method_call ctx obj mname targs args
  | TransI next -> interp_trans ctx next
  | SelectI (exprs, cases) -> interp_select ctx exprs cases
  | DeclI decl -> interp_decl ctx decl
  | _ ->
      Format.eprintf "(TODO: interp_stmt) %a" Syntax.Print.print_stmt (0, stmt);
      assert false

and interp_block (ctx : Ctx.t) (block : block) =
  let ctx = Ctx.enter_frame ctx in
  let ctx = List.fold_left interp_stmt ctx block in
  Ctx.exit_frame ctx

and interp_trans (ctx : Ctx.t) (next : string) =
  if next = "accept" || next = "reject" then ctx
  else
    let state_next = Ctx.find_func next ctx |> Option.get in
    (* (TODO) Visibility should be managed by the object *)
    let _vis_obj, body =
      match state_next with
      | StateF { vis_obj; body } -> (vis_obj, body)
      | _ -> assert false
    in
    let ctx_next =
      let env_glob = ctx.glob in
      let env_obj = ctx.obj in
      let env_loc = (TDEnv.empty, []) in
      Ctx.init env_glob env_obj env_loc
    in
    (* (TODO) This is a tail-call *)
    let ctx_next = interp_block ctx_next body in
    { ctx with obj = ctx_next.obj }

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

(* Interpreter: entry point of a P4 programmable block execution *)

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

and interp_extern_mthd (ctx_caller : Ctx.t) (ctx_callee : Ctx.t)
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

and interp_apply (ctx_caller : Ctx.t) (ctx_callee : Ctx.t)
    (_tparams : Var.t list) (params : param list) (_targs : typ list)
    (args : arg list) (body : block) =
  (* Copy-in to the object environment *)
  let ctx_callee = copyin Ctx.add_var_obj ctx_caller ctx_callee params args in
  (* Execute the body *)
  let ctx_callee = interp_block ctx_callee body in
  (* Copy-out from the object environment *)
  copyout ctx_caller ctx_callee params args

and interp_method_call (ctx : Ctx.t) (obj : Object.t) (mname : Var.t)
    (targs : typ list) (args : arg list) =
  match obj with
  | ExternO { vis_glob; env_obj } ->
      (* Find the method *)
      let env_glob = env_from_vis !gctx.glob vis_glob in
      let env_loc = (TDEnv.empty, []) in
      let ctx_callee = Ctx.init env_glob env_obj env_loc in
      let mthd = Ctx.find_func mname ctx_callee |> Option.get in
      (* Restrict the method's visibility *)
      let vis_obj, tparams, params = fetch_extern_method mthd in
      let ctx_callee =
        let env_obj = env_from_vis ctx_callee.obj vis_obj in
        { ctx_callee with obj = env_obj }
      in
      (* Evaluate the body *)
      interp_extern_mthd ctx ctx_callee mname tparams params targs args
  | ParserO { vis_glob; env_obj; mthd } | ControlO { vis_glob; env_obj; mthd }
    ->
      assert (mname = "apply");
      let vis_obj, tparams, params, body = fetch_apply_method mthd in
      let env_glob = env_from_vis !gctx.glob vis_glob in
      let env_obj = env_from_vis env_obj vis_obj in
      let env_loc = (TDEnv.empty, []) in
      let ctx_callee = Ctx.init env_glob env_obj env_loc in
      interp_apply ctx ctx_callee tparams params targs args body
  | _ -> assert false
