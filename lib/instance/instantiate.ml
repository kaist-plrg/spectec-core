open Syntax
open Ast
open Runtime

(* Environments *)

type env = Env.t 
type cenv = Cenv.t
type store = Store.t 


(* Loading the result of a declaration
   (other than instantiation) to env or cenv *)

let rec load
  (env: env) (cenv: cenv)
  (decl: Declaration.t): (env * cenv) =
  match decl with
  (* Loading constructor closures *)
  | PackageType { name; params; _ } ->
      let name = name.str in
      let cclos = Cclosure.Package { params; } in
      let cenv = Cenv.insert name cclos cenv in
      (env, cenv)
  | Parser { name; params; constructor_params; locals; states; _ } ->
      let name = name.str in
      let cclos =
        Cclosure.Parser {
          params;
          cparams = constructor_params;
          locals;
          states; }
      in
      let cenv = Cenv.insert name cclos cenv in
      (env, cenv)
  | Control { name; params; constructor_params; locals; apply; _ } ->
      let name = name.str in
      let cclos = 
        Cclosure.Control {
          params;
          cparams = constructor_params;
          locals;
          apply; }
      in
      let cenv = Cenv.insert name cclos cenv in
      (env, cenv)
  | ExternObject { name; _ } ->
      let name = name.str in
      let cclos = Cclosure.Extern in
      let cenv = Cenv.insert name cclos cenv in
      (env, cenv)
  | Function { name; _ } ->
      Printf.sprintf "(TODO: load) Loading function %s" name.str
      |> print_endline;
      (env, cenv)
  (* Loading constants *)
  | Constant { name; value; _ } ->
      let value = eval_static_expr env value in
      let env = Env.insert name.str value env in
      (env, cenv)
  | _ -> (env, cenv)



(* Evaluating instantiation arguments,
   which may contain nameless instantiation *)

and eval_expr
  (env: env) (cenv: cenv) (store: store)
  (path: string list) (expr: Expression.t): (Value.t * store) =
  match expr with
  | NamelessInstantiation { typ; args; _ } ->
      let _, _, store = instantiate_expr env cenv store path typ args in
      let value = Value.Ref path in
      (value, store)
  | _ ->
      let value = eval_static_expr env expr in
      (value, store)

(* Evaluating compile-time known values *)

and eval_static_expr
  (env: env) (expr: Expression.t): Value.t =
  match expr with
  | True _ ->
      let bvalue = Value.Bool true in
      Value.Base bvalue
  | False _ ->
      let bvalue = Value.Bool false in
      Value.Base bvalue
  | Int { i; _ } ->
      let value = i.value in
      let bvalue =
        begin match i.width_signed with
        | Some (width, signed) ->
            if signed then Value.Int { value; width }
            else Value.Bit { value; width }
        | None -> Value.AInt value
        end
      in
      Value.Base bvalue
  | String { text; _ } ->
      let bvalue = Value.String text.str in
      Value.Base bvalue
  | Name { name = BareName text; _ } ->
      let var = text.str in
      Env.find var env
  | BitStringAccess { bits; lo; hi; _ } ->
      let vbits = eval_static_expr env bits in
      let vlo = eval_static_expr env lo in
      let vhi = eval_static_expr env hi in
      Numerics.eval_bitstring_access vbits vlo vhi
  | UnaryOp { op; arg; _ } ->
      let varg = eval_static_expr env arg in
      Numerics.eval_unop op varg
  | BinaryOp { op; args = (arg_fst, arg_snd); _ } ->
      let varg_fst = eval_static_expr env arg_fst in
      let varg_snd = eval_static_expr env arg_snd in
      Numerics.eval_binop op varg_fst varg_snd
  | _ ->
      Printf.sprintf
        "(TODO: eval_static_expr) %s" (Pretty.print_expr expr)
      |> failwith 

and eval_args
  (env: env) (cenv: cenv) (store: store)
  (path: string list)
  (params: Parameter.t list) (args: Argument.t list): (env * store) =
  (* TODO: assume there is no default argument *)
  (assert (List.length params = List.length args));
  let params =
    List.map
      (fun (param: Parameter.t) -> param.variable.str)
      params
  in
  (* TODO: assume arguments are nameless,
   but in reality named arguments can be in arbitrary order *)
  let args =
    List.map
      (fun (arg: Argument.t) ->
         match arg with
         | Expression { value; _ } -> value
         | KeyValue _ -> failwith "(TODO) Named arguments are not supported."
         | _ -> failwith "Instantiation argument must not be missing.")
      args
  in
  List.fold_left2
    (fun (env', store) param arg ->
      let value, store = eval_expr env cenv store (path @ [ param ]) arg in
      let env' = Env.insert param value env' in
      (env', store))
    (env, store) params args 

(* Instantiation of a constructor closure *)

and instantiate_cclos
  (env: env) (cenv: cenv) (store: store)
  (path: string list)
  (cclos: Cclosure.t) (args: Argument.t list): store =
  match cclos with
  (* The instantiation of a parser or control block recursively
     evaluates all stateful instantiations declared in the block (16.2) *)
  (* Every time a parser is instantiated, it causes:
     every extern and parser instantiation that is in the parser source code
     at its top level is instantiated 1 time (p4guide) *)
  | Cclosure.Parser { params; cparams; locals; states } ->
      let env_parser = Env.enter env in
      let cenv_parser = Cenv.enter cenv in
      let env_parser, store =
        eval_args env_parser cenv_parser store path cparams args
      in
      let env_parser, cenv_parser, store =
        List.fold_left
          (fun (env, cenv, store) local ->
            instantiate_decl env cenv store path local)
          (env_parser, cenv_parser, store) locals
      in
      let stmts =
        List.concat_map
          (fun (state: Parser.state) -> state.statements)
          states
      in
      let env_parser, _cenv_parser, store =
        List.fold_left
          (fun (env, cenv, store) stmt ->
            instantiate_stmt env cenv store path stmt)
          (env_parser, cenv_parser, store) stmts
      in
      let obj =
        Object.Parser { scope = env_parser; params; locals; states; }
      in
      let store = Store.insert path obj store in
      store
  (* Every time a control is instantiated, it causes:
     1 instantiation of each table defined within it
     every extern and control instantiation that is in the control source code
     at its top level is instantiated 1 time (p4guide) *)
  | Cclosure.Control { params; cparams; locals; apply; _ } ->
      let env_control = Env.enter env in
      let cenv_control = Cenv.enter cenv in
      let env_control, store =
        eval_args env_control cenv_control store path cparams args
      in
      let env_control, cenv_control, store =
        List.fold_left
          (fun (env, cenv, store) local ->
            instantiate_decl env cenv store path local)
          (env_control, cenv_control, store) locals
      in
      let stmts = apply.statements in
      let env_control, _cenv_control, store =
        List.fold_left
         (fun (env, cenv, store) stmt ->
           instantiate_stmt env cenv store path stmt)
         (env_control, cenv_control, store) stmts
      in
      let obj =
        Object.Control { scope = env_control; params; locals; apply; }
      in
      let store = Store.insert path obj store in
      store
  (* Others do not involve recursive instantiation other than the args *)
  | Cclosure.Package { params } ->
      let env_package = Env.enter env in
      let env_package, store = eval_args env_package cenv store path params args in
      let obj = Object.Package { scope = env_package } in
      let store = Store.insert path obj store in
      store
  | Cclosure.Extern ->
      let obj = Object.Extern in
      let store = Store.insert path obj store in
      store

and instantiate_expr
  (env: env) (cenv: cenv) (store: store)
  (path: string list) (typ: Type.t) (args: Argument.t list): (env * cenv * store) =
    let typ = Pretty.print_type typ in
    let cclos = Cenv.find typ cenv in
    let store = instantiate_cclos env cenv store path cclos args in
    (env, cenv, store)

and instantiate_stmt
  (env: env) (cenv: cenv) (store: store)
  (path: string list) (stmt: Statement.t): (env * cenv * store) =
    match stmt with
    | BlockStatement { block; _ } ->
        let stmts = block.statements in
        List.fold_left
          (fun (env, cenv, store) stmt -> instantiate_stmt env cenv store path stmt)
          (env, cenv, store) stmts
    | DirectApplication { typ; args; _ } ->
        let typ = Pretty.print_type typ in
        let cclos = Cenv.find typ cenv in
        let store = instantiate_cclos env cenv store (path @ [ typ ]) cclos args in
        (env, cenv, store)
    | _ ->
        (env, cenv, store)

and instantiate_decl
  (env: env) (cenv: cenv) (store: store)
  (path: string list) (decl: Declaration.t): (env * cenv * store) =
  match decl with
  (* Explicit instantiation *)
  | Instantiation { typ; args; name; _ } ->
      let name = name.str in
      let typ = Pretty.print_type typ in
      let cclos = Cenv.find typ cenv in
      let store =
        instantiate_cclos env cenv store (path @ [ name ]) cclos args
      in
      let value = Value.Ref (path @ [ name ]) in
      let env = Env.insert name value env in
      (env, cenv, store)
  (* Each table evaluates to a table instance (18.2) *)
  (* There is no syntax for specifying parameters that are tables
     Tables are only intended to be used from within the control
     where they are defined (Appendix F) *)
  | Table { name; properties; _ } ->
      let name = name.str in
      let obj = Object.Table { scope = env; properties; } in
      let store = Store.insert (path @ [ name ]) obj store in
      let value = Value.Ref (path @ [ name ]) in
      let env = Env.insert name value env in
      (env, cenv, store)
  (* (TODO) is it correct to instantiate a value set at its declaration? *)
  (* There is no syntax for specifying parameters that are value-sets 
     (Appendix F) *)
  | ValueSet { name; _ } ->
      let name = name.str in
      let obj = Object.ValueSet in
      let store = Store.insert (path @ [ name ]) obj store in
      let value = Value.Ref (path @ [ name ]) in
      let env = Env.insert name value env in
      (env, cenv, store)
  (* Load declarations to either env or cenv *)
  | _ ->
      let env, cenv = load env cenv decl in
      (env, cenv, store)

let instantiate_program (program: program) =
  let Program decls = program in
  let env = Env.empty in
  let cenv = Cenv.empty in
  let store = Store.empty in
  let _, _, store =
    List.fold_left
      (fun (env, cenv, store) decl -> instantiate_decl env cenv store [] decl)
      (env, cenv, store) decls
  in 
  store
