open Syntax
open Ast

(* Environment for instantiating a program,
   holding closures of instantiable declarations *)

module Map = Map.Make(String)

module CClosure = struct
  type t =
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
    | CPackage of
        { params: Parameter.t list; }
    | CExtern
end

let print_cclosure (cclosure: CClosure.t) =
  match cclosure with
  | CClosure.CParser { params; constructor_params; _ } ->
      Printf.sprintf "CParser { params = (%s); constructor_params = (%s) }"
        (String.concat ", " (List.map Print.print_param params))
        (String.concat ", " (List.map Print.print_param constructor_params))
  | CClosure.CControl { params; constructor_params; _ } ->
      Printf.sprintf "CControl { params = (%s); constructor_params = (%s) }"
        (String.concat ", " (List.map Print.print_param params))
        (String.concat ", " (List.map Print.print_param constructor_params))
  | CClosure.CPackage { params } ->
      Printf.sprintf "CPackage { params = (%s) }"
        (String.concat ", " (List.map Print.print_param params))
  | CExtern -> "CExtern"

type env = CClosure.t Map.t


let load
  (env: env) (decl: Declaration.t): env =
  match decl with
  | Parser { name; params; constructor_params; locals; states; _ } ->
      Printf.sprintf "Loading parser %s" name.str |> print_endline;
      let name = name.str in
      let closure =
        CClosure.CParser { params; constructor_params; locals; states; }
      in
      Map.add name closure env
  | Control { name; params; constructor_params; locals; apply; _ } ->
      Printf.sprintf "Loading control %s" name.str |> print_endline;
      let name = name.str in
      let closure = 
        CClosure.CControl { params; constructor_params; locals; apply; }
      in
      Map.add name closure env
  | PackageType { name; params; _ } ->
      Printf.sprintf "Loading package type %s" name.str |> print_endline;
      let name = name.str in
      let closure = CClosure.CPackage { params; } in
      Map.add name closure env
  | ExternFunction { name; _ } ->
      Printf.sprintf "Loading extern function %s" name.str |> print_endline;
      let name = name.str in
      Map.add name CClosure.CExtern env
  | ExternObject { name; _ } ->
      Printf.sprintf "Loading extern object %s" name.str |> print_endline;
      let name = name.str in
      Map.add name CClosure.CExtern env
  | _ -> env


(* Instantiation produces a global store *)

module Instance = struct
  type t =
    | IParser
    | IControl
    | IPackage 
    | IExtern
end

type store = Instance.t Map.t

let print_store (store: store) =
  Map.iter
    (fun key value ->
       Printf.printf "%s -> %s\n" key
         (match value with
          | Instance.IParser -> "IParser"
          | Instance.IControl -> "IControl"
          | Instance.IPackage -> "IPackage"
          | Instance.IExtern -> "IExtern"))
    store

(* Instantiation of a declaration *)


(* Evaluating instantiation arguments,
   which may contain nameless instantiation. *)

let rec eval_arg
  (env: env) (store: store)
  (path: string list) (expr: Expression.t): (env * store) =
  match expr with
  | NamelessInstantiation { typ; args; _ } ->
      instantiate_expr env store path typ args
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
  List.fold_left2
    (fun (env, store) param arg -> eval_arg env store (path @ [ param ]) arg)
    (env, store) params args 

(* Instantiation of a constructor closure *)

and instantiate
  (env: env) (store: store)
  (path: string list)
  (cclosure: CClosure.t) (args: Argument.t list): (env * store) =
  Printf.sprintf "Instantiating %s with args %s"
    (print_cclosure cclosure) (String.concat ", " (List.map Print.print_arg args))
  |> print_endline;
  match cclosure with
  | CClosure.CParser { constructor_params; locals; _ } ->
      let env, store = eval_args env store path constructor_params args in
      List.fold_left
        (fun (env, store) local -> instantiate_decl env store path local)
        (env, store) locals
  | CClosure.CControl { constructor_params; locals; _ } ->
      let env, store = eval_args env store path constructor_params args in
      List.fold_left
        (fun (env, store) local -> instantiate_decl env store path local)
        (env, store) locals
  (* A package does not have its body,
     so we only need to evaluate its parameters *)
  | CClosure.CPackage { params } ->
      eval_args env store path params args
  | CClosure.CExtern ->
      (env, Map.add (String.concat "." path) Instance.IExtern store)

and instantiate_expr
  (env: env) (store: store)
  (path: string list)(typ: Type.t) (args: Argument.t list): (env * store) =
    (* TODO: print_type may not be the right function *)
    let typ = Print.print_type typ in
    let cclosure = Map.find typ env in
    instantiate env store path cclosure args

and instantiate_decl
  (env: env) (store: store)
  (path: string list) (decl: Declaration.t): (env * store) =
  match decl with
  | Instantiation { typ; args; name; _ } ->
      let name = name.str in
      (* TODO: print_type may not be the right function *)
      let typ = Print.print_type typ in
      (* TODO: assuming this is package instantiation *)
      let cclosure = Map.find typ env in
      instantiate env store (path @ [ name ]) cclosure args
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
