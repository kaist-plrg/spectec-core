open Syntax.Ast
open Runtime_.Domain
open Runtime_.Base
open Runtime_.Object
open Runtime_.Context

(* Global environment *)

let env_glob = ref (TDEnv.empty, Env.empty, FEnv.empty)

let init (_env_glob: env_glob) =
  env_glob := _env_glob

(* Helper to fetch "apply" method *)

let fetch_apply_method (func: Func.t) =
  match func with
  | MethodF { vis_obj; tparams; params; body } ->
      (vis_obj, tparams, params, body)
  | _ -> assert false

(* Interpreter for declarations *)

let interp_decl (ctx: Ctx.t) (decl: decl) =
  match decl with
  | VarD { name; typ; init = None } ->
      let typ = Eval.eval_type ctx typ in
      let value = Runtime_.Ops.eval_default_value typ in
      Ctx.add_var_loc name typ value ctx
  | VarD { name; typ; init = Some value } ->
      let typ = Eval.eval_type ctx typ in
      let value = Eval.eval_expr ctx value in
      Ctx.add_var_loc name typ value ctx
  | _ -> failwith "TODO"

(* Interpreter for statements *)

(* lvalue
   : prefixedNonTypeName | lvalue "." member
   | lvalue "[" expression "]" | lvalue "[" expression ":" expression "]" *)

let interp_write (ctx: Ctx.t) (lvalue: expr) (value: Value.t) =
  match lvalue with
  | VarE (Bare name) ->
      let typ = Ctx.find_var name ctx |> Option.get |> fst in
      let value = Runtime_.Ops.eval_cast typ value in
      Ctx.update_var name typ value ctx
  | _ ->
      Format.printf "Ctx before fail: %a@." Ctx.pp ctx;
      failwith "TODO"

let rec interp_stmt (ctx: Ctx.t) (stmt: stmt) =
  Format.printf "Start stmt with ctx: %a@." Ctx.pp ctx;
  match stmt with
  | AssignI (lhs, rhs) ->
      let value = Eval.eval_expr ctx rhs in
      interp_write ctx lhs value
  | BlockI block -> interp_block ctx block
  | DeclI decl -> interp_decl ctx decl
  | _ ->
      Format.printf "Ctx before fail: %a@." Ctx.pp ctx;
      failwith "TODO"

and interp_block (ctx: Ctx.t) (block: block) =
  let ctx = Ctx.enter_frame ctx in
  let ctx = List.fold_left interp_stmt ctx block in
  Ctx.exit_frame ctx

(* Interpreter: entry point of a P4 programmable block execution *)

let copyin (ctx_caller: Ctx.t) (ctx_callee: Ctx.t)
    (params: param list) (args: arg list) =
  (* (TODO) Assume there is no default argument *)
  assert (List.length params = List.length args);
  Instance_.Instantiate.check_args args;
  let copyin' (ctx_callee: Ctx.t) (param: param) (arg: arg) =
    (* Resolve the argument-parameter order *)
    let pname, dir, typ, _ = param in
    let pname, value = match arg with
    | ExprA value -> pname, value
    | NameA (pname, value) -> pname, value
    | _ -> failwith "TODO"
    in
    match dir with
    | No | In | InOut ->
        let typ = Eval.eval_type ctx_caller typ in
        let value = Eval.eval_expr ctx_caller value in
        (* (TODO) this is specific for control/parser *)
        Ctx.add_var_obj pname typ value ctx_callee
    | Out ->
        let typ = Eval.eval_type ctx_caller typ in
        let value = Runtime_.Ops.eval_default_value typ in
        (* (TODO) this is specific for control/parser *)
        Ctx.add_var_obj pname typ value ctx_callee
  in
  List.fold_left2 copyin' ctx_callee params args

let copyout (ctx_caller: Ctx.t) (ctx_callee: Ctx.t)
    (params: param list) (args: arg list) =
  let copyout' (ctx_caller: Ctx.t) (param: param) (arg: arg) =
    let pname, dir, _, _ = param in
    let pname, lvalue = match arg with
    | ExprA value -> pname, value
    | NameA (pname, value) -> pname, value
    | _ -> failwith "TODO"
    in
    match dir with
    | InOut | Out ->
        (* (TODO) this is specific for control/parser *)
        let value = Ctx.find_var pname ctx_callee |> Option.get |> snd in
        interp_write ctx_caller lvalue value
    | _ -> ctx_caller
  in
  List.fold_left2 copyout' ctx_caller params args

let interp_apply (ctx_caller: Ctx.t) (ctx_callee: Ctx.t)
    (_tparams: Var.t list) (params: param list)
    (_targs: typ list) (args: arg list) (body: block) =
  (* Copy-in to the object environment *)
  let ctx_callee = copyin ctx_caller ctx_callee params args in
  (* Execute the body *)
  let ctx_callee = interp_block ctx_callee body in
  Format.printf "Time to copyout with ctx: %a@." Ctx.pp ctx_callee;
  (* Copy-out from the object environment *)
  copyout ctx_caller ctx_callee params args

let interp_method_call (ctx: Ctx.t) (obj: Object.t)
    (mname: Var.t) (targs: typ list) (args: arg list) =
  match obj with
  | ParserO { vis_glob; env_obj; sto_obj; mthd }
  | ControlO { vis_glob; env_obj; sto_obj; mthd } ->
      assert (mname = "apply");
      let vis_obj, tparams, params, body = fetch_apply_method mthd in
      let env_glob = env_from_vis !env_glob vis_glob in
      let env_obj = env_from_vis env_obj vis_obj in
      let env_loc = (TDEnv.empty, []) in
      let ctx_callee = Ctx.init env_glob env_obj env_loc sto_obj in
      interp_apply ctx ctx_callee tparams params targs args body
  | _ -> assert false
