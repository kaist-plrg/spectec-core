open Syntax
open Ast

type env = Value.env
type cenv = Cclosure.cenv
type store = Object.store

(* Loading the result of a declaration
   (other than instantiation) to env or cenv *)

let rec load
  (env: env) (cenv: cenv)
  (decl: Declaration.t): (env * cenv) =
  match decl with
  (* Loading constructor closures *)
  | PackageType { name; params; _ } ->
      Printf.sprintf "Loading package type %s" name.str |> print_endline;
      let name = name.str in
      let cclos = Cclosure.Package { params; } in
      let cenv = Cclosure.insert_cenv [ name ] cclos cenv in
      (env, cenv)
  | Parser { name; params; constructor_params; locals; states; _ } ->
      Printf.sprintf "Loading parser %s" name.str |> print_endline;
      let name = name.str in
      let cclos =
        Cclosure.Parser {
          params;
          cparams = constructor_params;
          locals;
          states; }
      in
      let cenv = Cclosure.insert_cenv [ name ] cclos cenv in
      (env, cenv)
  | Control { name; params; constructor_params; locals; apply; _ } ->
      Printf.sprintf "Loading control %s" name.str |> print_endline;
      let name = name.str in
      let cclos = 
        Cclosure.Control {
          params;
          cparams = constructor_params;
          locals;
          apply; }
      in
      let cenv = Cclosure.insert_cenv [ name ] cclos cenv in
      (env, cenv)
  | ExternObject { name; _ } ->
      Printf.sprintf "Loading extern object %s" name.str |> print_endline;
      let name = name.str in
      let cclos = Cclosure.Extern in
      let cenv = Cclosure.insert_cenv [ name ] cclos cenv in
      (env, cenv)
  | Function { name; _ } ->
      Printf.sprintf "(TODO) Loading function %s" name.str
      |> print_endline;
      (env, cenv)
  (* Loading constants *)
  | Constant { name; value; _ } ->
      Printf.sprintf "Loading constant %s" name.str |> print_endline;
      let value = eval_static_expr env value in
      let env = Value.insert_env [ name.str ] value env in
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
      let base = Value.Bool true in
      Value.Base base
  | False _ ->
      let base = Value.Bool false in
      Value.Base base
  | Int { i; _ } ->
      let base = Value.Integer i.value in
      Value.Base base
  | String { text; _ } ->
      let base = Value.String text.str in
      Value.Base base
  | Name { name = BareName text; _ } ->
      let text = text.str in
      Value.find_env [ text ] env
  | _ ->
      Printf.sprintf
        "(TODO: eval_static_expr) %s" (Print.print_expr expr)
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
      let env' = Value.insert_env [ param ] value env' in
      (env', store))
    (env, store) params args 

(* Instantiation of a constructor closure *)

and instantiate_cclosure
  (env: env) (cenv: cenv) (store: store)
  (path: string list)
  (cclosure: Cclosure.t) (args: Argument.t list): (env * cenv * store) =
  Printf.sprintf "Instantiating %s with args %s @ %s"
    (Cclosure.print cclosure) (String.concat ", " (List.map Print.print_arg args))
    (String.concat "." path)
  |> print_endline;
  match cclosure with
  (* The instantiation of a parser or control block recursively
     evaluates all stateful instantiations declared in the block (16.2) *)
  (* Every time a parser is instantiated, it causes:
     every extern and parser instantiation that is in the parser source code
     at its top level is instantiated 1 time (p4guide) *)
  | Cclosure.Parser { params; cparams; locals; states } ->
      let env, store = eval_args env cenv store path cparams args in
      let env, _, store =
        List.fold_left
          (fun (env, cenv, store) local ->
            instantiate_decl env cenv store path local)
          (env, cenv, store) locals
      in
      let stmts =
        List.concat_map
          (fun (state: Parser.state) -> state.statements)
          states
      in
      let env, _, store =
        List.fold_left
          (fun (env, cenv, store) stmt ->
            instantiate_stmt env cenv store path stmt)
          (env, cenv, store) stmts
      in
      let obj =
        Object.Parser { scope = env; params; locals; states; }
      in
      let store = Object.insert_store path obj store in
      (env, cenv, store)
  (* Every time a control is instantiated, it causes:
     1 instantiation of each table defined within it
     every extern and control instantiation that is in the control source code
     at its top level is instantiated 1 time (p4guide) *)
  | Cclosure.Control { params; cparams; locals; apply; _ } ->
      let env, store = eval_args env cenv store path cparams args in
      let env, _, store =
        List.fold_left
          (fun (env, cenv, store) local ->
            instantiate_decl env cenv store path local)
          (env, cenv, store) locals
      in
      let stmts = apply.statements in
      let env, _, store =
        List.fold_left
         (fun (env, cenv, store) stmt ->
           instantiate_stmt env cenv store path stmt)
         (env, cenv, store) stmts
      in
      let obj =
        Object.Control { scope = env; params; locals; apply; }
      in
      let store = Object.insert_store path obj store in
      (env, cenv, store)
  (* Others do not involve recursive instantiation other than the args *)
  | Cclosure.Package { params } ->
      let env, store = eval_args env cenv store path params args in
      let obj = Object.Package { scope = env } in
      let store = Object.insert_store path obj store in
      (env, cenv, store)
  | Cclosure.Extern ->
      let obj = Object.Extern in
      let store = Object.insert_store path obj store in
      (env, cenv, store)

and instantiate_expr
  (env: env) (cenv: cenv) (store: store)
  (path: string list) (typ: Type.t) (args: Argument.t list): (env * cenv * store) =
    let typ = Print.print_type typ in
    let cclosure = Cclosure.find_cenv [ typ ] cenv in
    instantiate_cclosure env cenv store path cclosure args

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
        let typ = Print.print_type typ in
        let cclosure = Cclosure.find_cenv [ typ ] cenv in
        instantiate_cclosure env cenv store (path @ [ typ ]) cclosure args
    | _ ->
        (env, cenv, store)

and instantiate_decl
  (env: env) (cenv: cenv) (store: store)
  (path: string list) (decl: Declaration.t): (env * cenv * store) =
  match decl with
  (* Explicit instantiation *)
  | Instantiation { typ; args; name; _ } ->
      let name = name.str in
      let typ = Print.print_type typ in
      let cclosure = Cclosure.find_cenv [ typ ] cenv in
      let env, cenv, store =
        instantiate_cclosure env cenv store (path @ [ name ]) cclosure args
      in
      let value = Value.Ref (path @ [ name ]) in
      let env = Value.insert_env [ name ] value env in
      (env, cenv, store)
  (* Each table evaluates to a table instance (18.2) *)
  (* There is no syntax for specifying parameters that are tables
     Tables are only intended to be used from within the control
     where they are defined (Appendix F) *)
  | Table { name; properties; _ } ->
      let name = name.str in
      let obj = Object.Table { scope = env; properties; } in
      let store = Object.insert_store (path @ [ name ]) obj store in
      let value = Value.Ref (path @ [ name ]) in
      let env = Value.insert_env [ name ] value env in
      (env, cenv, store)
  (* (TODO) is it correct to instantiate a value set at its declaration? *)
  (* There is no syntax for specifying parameters that are value-sets 
     (Appendix F) *)
  | ValueSet { name; _ } ->
      let name = name.str in
      let obj = Object.ValueSet in
      let store = Object.insert_store (path @ [ name ]) obj store in
      let value = Value.Ref (path @ [ name ]) in
      let env = Value.insert_env [ name ] value env in
      (env, cenv, store)
  (* Load declarations to either env or cenv *)
  | _ ->
      let env, cenv = load env cenv decl in
      (env, cenv, store)

let instantiate_program (program: program): Object.store =
  let Program decls = program in
  let env = Value.empty_env in
  let cenv = Cclosure.empty_cenv in
  let store = Object.empty_store in
  let _, _, store =
    List.fold_left
      (fun (env, cenv, store) decl -> instantiate_decl env cenv store [] decl)
      (env, cenv, store) decls
  in
  print_endline "\nInstantiation done.";
  Object.print_store store |> print_endline;
  store
