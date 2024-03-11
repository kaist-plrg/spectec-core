open Syntax
open Ast

(* Environment for instantiating a program,
   holding closures of instantiable declarations *)

module Path = struct
  type t = string list
  let compare = compare
end

module Map = Map.Make(Path)

module CClosure = struct
  type t =
    | CPackage of
        { params: Parameter.t list; }
    | CParser of
        { params: Parameter.t list;
          constructor_params: Parameter.t list;
          locals: Declaration.t list;
          states: Parser.state list; }
    | CControl of
        { params: Parameter.t list;
          constructor_params: Parameter.t list;
          locals: Declaration.t list;
          apply: Block.t; }
    | CExtern
    | CTable of
        { properties: Table.property list; }
    | CValueSet
    | CFunction
end

let print_cclosure (cclosure: CClosure.t) =
  match cclosure with
  | CClosure.CPackage { params } ->
      Printf.sprintf "CPackage { params = (%s) }"
        (String.concat ", " (List.map Print.print_param params))
  | CClosure.CParser { params; constructor_params; _ } ->
      Printf.sprintf "CParser { params = (%s); constructor_params = (%s) }"
        (String.concat ", " (List.map Print.print_param params))
        (String.concat ", " (List.map Print.print_param constructor_params))
  | CClosure.CControl { params; constructor_params; _ } ->
      Printf.sprintf "CControl { params = (%s); constructor_params = (%s) }"
        (String.concat ", " (List.map Print.print_param params))
        (String.concat ", " (List.map Print.print_param constructor_params))
  | CClosure.CExtern -> "CExtern"
  | CClosure.CTable { properties } ->
      Print.print_inline
        (Printf.sprintf "CTable { properties = (%s) }"
          (String.concat ", " (List.map (Print.print_table_property 0) properties)))
  | CClosure.CValueSet -> "CValueSet"
  | CClosure.CFunction -> "CFunction"

type env = CClosure.t Map.t


let load
  (env: env) (decl: Declaration.t): env =
  match decl with
  | PackageType { name; params; _ } ->
      Printf.sprintf "Loading package type %s" name.str |> print_endline;
      let name = name.str in
      let closure = CClosure.CPackage { params; } in
      Map.add [ name ] closure env
  | Parser { name; params; constructor_params; locals; states; _ } ->
      Printf.sprintf "Loading parser %s" name.str |> print_endline;
      let name = name.str in
      let closure =
        CClosure.CParser { params; constructor_params; locals; states; }
      in
      Map.add [ name ] closure env
  | Control { name; params; constructor_params; locals; apply; _ } ->
      Printf.sprintf "Loading control %s" name.str |> print_endline;
      let name = name.str in
      let closure = 
        CClosure.CControl { params; constructor_params; locals; apply; }
      in
      Map.add [ name ] closure env
  | ExternObject { name; _ } ->
      Printf.sprintf "Loading extern object %s" name.str |> print_endline;
      let name = name.str in
      Map.add [ name ] CClosure.CExtern env
  | Table { name; properties; _ } ->
      Printf.sprintf "Loading table %s" name.str |> print_endline;
      let name = name.str in
      let closure = CClosure.CTable { properties; } in
      Map.add [ name ] closure env
  | ValueSet { name; _ } ->
      Printf.sprintf "Loading value set %s" name.str |> print_endline;
      let name = name.str in
      Map.add [ name ] CClosure.CValueSet env
  | Function { name; _ } ->
      Printf.sprintf "(TODO) Loading function %s" name.str
      |> print_endline;
      env
  | Constant { name; value; _ } ->
      Printf.sprintf "(TODO) Loading constant %s = %s"
        name.str (Print.print_expr value)
      |> print_endline;
      env
  | _ -> env


(* Instantiation produces a global store *)

(* The stateful types of objects in P4_16 are
   packages, parsers, controls, externs, tables, and value-sets
   P4_16 functions are also considered to be in that group,
   even if they happen to be pure functions of their arguments (Appendix F) *)

module Instance = struct
  type t =
    | IPackage 
    | IParser of
        { params: Parameter.t list;
          locals: Declaration.t list;
          states: Parser.state list; }
    | IControl of
        { params: Parameter.t list;
          locals: Declaration.t list;
          apply: Block.t; }
    | IExtern
    | ITable of
        { properties: Table.property list; }
    | IValueSet
    | IFunction
end

(* Instances should be distinguishable by their paths,
   or fully-qualified names, so a store can be a flat map *)

type store = Instance.t Map.t

let print_store (store: store) =
  Map.iter
    (fun key value ->
       Printf.printf "%s -> %s\n" (String.concat "." key)
         (match value with
          | Instance.IPackage -> "IPackage"
          | Instance.IParser _ -> "IParser"
          | Instance.IControl _ -> "IControl"
          | Instance.IExtern -> "IExtern"
          | Instance.ITable _ -> "ITable"
          | Instance.IValueSet -> "IValueSet"
          | Instance.IFunction -> "IFunction"))
    store


(* Evaluating instantiation arguments,
   which may contain nameless instantiation. *)

let rec eval_arg
  (env: env) (store: store)
  (path: string list) (expr: Expression.t): (env * store) =
  match expr with
  | NamelessInstantiation { typ; args; _ } ->
      let _, store = instantiate_expr env store path typ args in
      (env, store)
  | _ -> (env, store)

and eval_args
  (env: env) (store: store)
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
  let store =
    List.fold_left2
      (fun store param arg ->
        let _, store = eval_arg env store (path @ [ param ]) arg in
        store)
      store params args 
  in
  (env, store)

(* Instantiation of a constructor closure *)

and instantiate
  (env: env) (store: store)
  (path: string list)
  (cclosure: CClosure.t) (args: Argument.t list): (env * store) =
  Printf.sprintf "Instantiating %s with args %s @ %s"
    (print_cclosure cclosure) (String.concat ", " (List.map Print.print_arg args))
    (String.concat "." path)
  |> print_endline;
  match cclosure with
  (* The instantiation of a parser or control block recursively
     evaluates all stateful instantiations declared in the block (16.2) *)
  (* Every time a parser is instantiated, it causes:
     every extern and parser instantiation that is in the parser source code
     at its top level is instantiated 1 time (p4guide) *)
  | CClosure.CParser { params; constructor_params; locals; states; _ } ->
      let env, store = eval_args env store path constructor_params args in
      let _, store =
        List.fold_left
          (fun (env, store) local -> instantiate_decl env store path local)
          (env, store) locals
      in
      let stmts =
        List.concat_map
          (fun (state: Parser.state) -> state.statements)
          states
      in
      let _, store =
        List.fold_left
          (fun (env, store) stmt -> instantiate_stmt env store path stmt)
          (env, store) stmts
      in
      let iparser = Instance.IParser { params; locals; states; } in
      let store = Map.add path iparser store in
      (env, store)
  (* Every time a control is instantiated, it causes:
     1 instantiation of each table defined within it
     every extern and control instantiation that is in the control source code
     at its top level is instantiated 1 time (p4guide) *)
  | CClosure.CControl { params; constructor_params; locals; apply; _ } ->
      let env, store = eval_args env store path constructor_params args in
      let _, store =
        List.fold_left
          (fun (env, store) local -> instantiate_decl env store path local)
          (env, store) locals
      in
      let stmts = apply.statements in
      let _, store =
        List.fold_left
         (fun (env, store) stmt -> instantiate_stmt env store path stmt)
         (env, store) stmts
      in
      let icontrol = Instance.IControl { params; locals; apply; } in
      let store = Map.add path icontrol store in
      (env, store)
  (* Others do not have its body,
     so we only need to evaluate the arguments *)
  | CClosure.CPackage { params } ->
      let _, store = eval_args env store path params args in
      let store = Map.add path Instance.IPackage store in
      (env, store)
  | CClosure.CExtern ->
      let store = Map.add path Instance.IExtern store in
      (env, store)
  (* There is no syntax for specifying parameters that are tables
     Tables are only intended to be used from within the control
     where they are defined (Appendix F) *)
  | CClosure.CTable { properties } ->
      let itable = Instance.ITable { properties; } in
      let store = Map.add path itable store in
      (env, store)
  (* There is no syntax for specifying parameters that are value-sets 
     (Appendix F) *)
  | CClosure.CValueSet ->
      let store = Map.add path Instance.IValueSet store in
      (env, store)
  | CClosure.CFunction ->
      failwith "(TODO) Instantiating a function is not supported."

and instantiate_expr
  (env: env) (store: store)
  (path: string list) (typ: Type.t) (args: Argument.t list): (env * store) =
    let typ = Print.print_type typ in
    let cclosure = Map.find [ typ ] env in
    instantiate env store path cclosure args

and instantiate_stmt
  (env: env) (store: store)
  (path: string list) (stmt: Statement.t): (env * store) =
    match stmt with
    | BlockStatement { block; _ } ->
        let stmts = block.statements in
        List.fold_left
          (fun (env, store) stmt -> instantiate_stmt env store path stmt)
          (env, store) stmts
    | DirectApplication { typ; args; _ } ->
        let typ = Print.print_type typ in
        let cclosure = Map.find [ typ ] env in
        instantiate env store (path @ [ typ ]) cclosure args
    | _ ->
        (env, store)

and instantiate_decl
  (env: env) (store: store)
  (path: string list) (decl: Declaration.t): (env * store) =
  match decl with
  | Instantiation { typ; args; name; _ } ->
      let name = name.str in
      let typ = Print.print_type typ in
      let cclosure = Map.find [ typ ] env in
      instantiate env store (path @ [ name ]) cclosure args
  (* Each table evaluates to a table instance (18.2) *)
  (* There is no syntax for specifying parameters that are tables
     Tables are only intended to be used from within the control
     where they are defined (Appendix F) *)
  | Table { name; _ } ->
      let env = load env decl in
      let name = name.str in
      let cclosure = Map.find [ name ] env in
      instantiate env store (path @ [ name ]) cclosure []
  (* (TODO) is it correct to instantiate a value set at its declaration? *)
  (* There is no syntax for specifying parameters that are value-sets 
     (Appendix F) *)
  | ValueSet { name; _ } ->
      let env = load env decl in
      let name = name.str in
      let cclosure = Map.find [ name ] env in
      instantiate env store (path @ [ name ]) cclosure []
  | _ ->
      (load env decl, store)

let instantiate_program (program: program): store =
  let Program decls = program in
  let env = Map.empty in
  let store = Map.empty in
  let _, store =
    List.fold_left
      (fun (env, store) decl -> instantiate_decl env store [] decl)
      (env, store) decls
  in
  print_endline "Instantiation done.";
  print_store store;
  store
