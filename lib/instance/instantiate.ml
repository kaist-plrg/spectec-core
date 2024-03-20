open Syntax
open Ast
open Runtime
open Utils

(* Environments *)

type env = Env.t
type tenv = Tenv.t
type cenv = Cenv.t
type store = Store.t

(* Utils *)

let rec name_from_type (typ : Type.t) : string =
  match typ with
  | TypeName { name = BareName text; _ } -> text.str
  (* (TODO) how to consider type arguments? *)
  | SpecializedType { base; _ } -> name_from_type base
  | _ ->
      Printf.sprintf "(name_from_type) Unexpected type: %s"
        (Pretty.print_type typ)
      |> failwith

(* Instantiation *)

(* Instantiating a declaration *)

(* Loading the result of a declaration
   (other than instantiation) to env, tenv, or cenv *)

let rec load (env : env) (tenv : tenv) (cenv : cenv) (decl : Declaration.t) :
    env * tenv * cenv =
  match decl with
  (* Loading constructor closures to cenv *)
  | PackageType { name; params; _ } ->
      let name = name.str in
      let cclos = Cclosure.Package { params } in
      let cenv = Cenv.insert name cclos cenv in
      (env, tenv, cenv)
  | Parser { name; params; constructor_params; locals; states; _ } ->
      let name = name.str in
      let cclos =
        Cclosure.Parser { params; cparams = constructor_params; locals; states }
      in
      let cenv = Cenv.insert name cclos cenv in
      (env, tenv, cenv)
  | Control { name; params; constructor_params; locals; apply; _ } ->
      let name = name.str in
      let cclos =
        Cclosure.Control { params; cparams = constructor_params; locals; apply }
      in
      let cenv = Cenv.insert name cclos cenv in
      (env, tenv, cenv)
  | ExternObject { name; _ } ->
      let name = name.str in
      let cclos = Cclosure.Extern in
      let cenv = Cenv.insert name cclos cenv in
      (env, tenv, cenv)
  (* Loading types to tenv *)
  (* (TODO) assume type declarations are at top level only *)
  | TypeDef { name; typ_or_decl; _ } -> (
      let name = name.str in
      match typ_or_decl with
      | Alternative.Left typ ->
          let typ = Static.eval_typ env tenv typ in
          let tenv = Tenv.insert name typ tenv in
          (env, tenv, cenv)
      | Alternative.Right _decl ->
          Printf.eprintf "(TODO: load) Loading typedef with decl %s\n" name;
          (env, tenv, cenv))
  | NewType { name; typ_or_decl; _ } -> (
      let name = name.str in
      match typ_or_decl with
      | Alternative.Left typ ->
          let typ = Static.eval_typ env tenv typ in
          let tenv = Tenv.insert name typ tenv in
          (env, tenv, cenv)
      | Alternative.Right _decl ->
          Printf.eprintf "(TODO: load) Loading newtype with decl %s\n" name;
          (env, tenv, cenv))
  | Enum { name; members; _ } ->
      let name = name.str in
      let entries = List.map (fun (member : Text.t) -> member.str) members in
      let btyp = Typ.Enum { entries } in
      let typ = Typ.Base btyp in
      let tenv = Tenv.insert name typ tenv in
      (env, tenv, cenv)
  | SerializableEnum { name; typ; members; _ } ->
      let name = name.str in
      let typ = Static.eval_base_typ env tenv typ in
      let entries =
        List.map
          (fun member ->
            let member : Text.t = fst member in
            member.str)
          members
      in
      let btyp = Typ.SEnum { typ; entries } in
      let typ = Typ.Base btyp in
      let tenv = Tenv.insert name typ tenv in
      (env, tenv, cenv)
  | Header { name; fields; _ } ->
      let name = name.str in
      let entries =
        List.map
          (fun (field : Declaration.field) ->
            let name = field.name.str in
            let typ = Static.eval_base_typ env tenv field.typ in
            (name, typ))
          fields
      in
      let btyp = Typ.Header { entries } in
      let typ = Typ.Base btyp in
      let tenv = Tenv.insert name typ tenv in
      (env, tenv, cenv)
  | HeaderUnion { name; fields; _ } ->
      let name = name.str in
      let entries =
        List.map
          (fun (field : Declaration.field) ->
            let name = field.name.str in
            let typ = Static.eval_base_typ env tenv field.typ in
            (name, typ))
          fields
      in
      let btyp = Typ.Union { entries } in
      let typ = Typ.Base btyp in
      let tenv = Tenv.insert name typ tenv in
      (env, tenv, cenv)
  | Struct { name; fields; _ } ->
      let name = name.str in
      let entries =
        List.map
          (fun (field : Declaration.field) ->
            let name = field.name.str in
            let typ = Static.eval_base_typ env tenv field.typ in
            (name, typ))
          fields
      in
      let btyp = Typ.Struct { entries } in
      let typ = Typ.Base btyp in
      let tenv = Tenv.insert name typ tenv in
      (env, tenv, cenv)
  (* Loading constants to env *)
  | Function { name; _ } ->
      Printf.eprintf "(TODO: load) Loading function %s\n" name.str;
      (env, tenv, cenv)
  (* Loading constants *)
  (* (TODO) consider implicit casts? *)
  | Constant { name; typ; value; _ } ->
      let typ = Static.eval_base_typ env tenv typ in
      let value = Static.eval_expr env tenv value in
      let value = Ops.eval_cast typ value in
      let env = Env.insert name.str value env in
      (env, tenv, cenv)
  | _ -> (env, tenv, cenv)

(* Evaluating instantiation arguments,
   which may contain nameless instantiation *)

and eval_expr (env : env) (tenv : tenv) (cenv : cenv) (store : store)
    (path : string list) (expr : Expression.t) : Value.t * store =
  match expr with
  | NamelessInstantiation { typ; args; _ } ->
      let store = instantiate_expr env tenv cenv store path typ args in
      let value = Value.Ref path in
      (value, store)
  | _ ->
      let value = Static.eval_expr env tenv expr in
      (value, store)

and eval_args (env : env) (tenv : tenv) (cenv : cenv) (store : store)
    (path : string list) (params : Parameter.t list) (args : Argument.t list) :
    env * store =
  (* TODO: assume there is no default argument *)
  assert (List.length params = List.length args);
  assert (
    List.for_all
      (fun (arg : Argument.t) ->
        match arg with Expression _ -> true | _ -> false)
      args
    || List.for_all
         (fun (arg : Argument.t) ->
           match arg with KeyValue _ -> true | _ -> false)
         args);
  let params, args =
    List.fold_left2
      (fun (params, args) (param : Parameter.t) (arg : Argument.t) ->
        match arg with
        | Expression { value; _ } ->
            (params @ [ param.variable.str ], args @ [ value ])
        | KeyValue { key; value; _ } -> (params @ [ key.str ], args @ [ value ])
        | _ ->
            failwith "(eval_args) Instantiation argument must not be missing.")
      ([], []) params args
  in
  List.fold_left2
    (fun (env', store) param arg ->
      let value, store = eval_expr env tenv cenv store (path @ [ param ]) arg in
      let env' = Env.insert param value env' in
      (env', store))
    (env, store) params args

(* Instantiation of a constructor closure *)

and instantiate_cclos (env : env) (tenv : tenv) (cenv : cenv) (store : store)
    (path : string list) (cclos : Cclosure.t) (args : Argument.t list) : store =
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
        eval_args env_parser tenv cenv_parser store path cparams args
      in
      let env_parser, _tenv_parser, cenv_parser, store =
        List.fold_left
          (fun (env, tenv, cenv, store) local ->
            instantiate_decl env tenv cenv store path local)
          (env_parser, tenv, cenv_parser, store)
          locals
      in
      let stmts =
        List.concat_map (fun (state : Parser.state) -> state.statements) states
      in
      let env_parser, _cenv_parser, store =
        List.fold_left
          (fun (env, cenv, store) stmt ->
            instantiate_stmt env tenv cenv store path stmt)
          (env_parser, cenv_parser, store)
          stmts
      in
      let obj = Object.Parser { scope = env_parser; params; locals; states } in
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
        eval_args env_control tenv cenv_control store path cparams args
      in
      let env_control, _tenv_control, cenv_control, store =
        List.fold_left
          (fun (env, tenv, cenv, store) local ->
            instantiate_decl env tenv cenv store path local)
          (env_control, tenv, cenv_control, store)
          locals
      in
      let stmts = apply.statements in
      let env_control, _cenv_control, store =
        List.fold_left
          (fun (env, cenv, store) stmt ->
            instantiate_stmt env tenv cenv store path stmt)
          (env_control, cenv_control, store)
          stmts
      in
      let obj = Object.Control { scope = env_control; params; locals; apply } in
      let store = Store.insert path obj store in
      store
  (* Others do not involve recursive instantiation other than the args *)
  | Cclosure.Package { params } ->
      let env_package = Env.enter env in
      let env_package, store =
        eval_args env_package tenv cenv store path params args
      in
      let obj = Object.Package { scope = env_package } in
      let store = Store.insert path obj store in
      store
  | Cclosure.Extern ->
      let obj = Object.Extern in
      let store = Store.insert path obj store in
      store

and instantiate_expr (env : env) (tenv : tenv) (cenv : cenv) (store : store)
    (path : string list) (typ : Type.t) (args : Argument.t list) : store =
  let cclos = Cenv.find_from_type typ cenv in
  let store = instantiate_cclos env tenv cenv store path cclos args in
  store

and instantiate_stmt (env : env) (tenv : tenv) (cenv : cenv) (store : store)
    (path : string list) (stmt : Statement.t) : env * cenv * store =
  match stmt with
  | BlockStatement { block; _ } ->
      let stmts = block.statements in
      List.fold_left
        (fun (env, cenv, store) stmt ->
          instantiate_stmt env tenv cenv store path stmt)
        (env, cenv, store) stmts
  | DirectApplication { typ; args; _ } ->
      let name = name_from_type typ in
      let cclos = Cenv.find_from_type typ cenv in
      let store = instantiate_cclos env tenv cenv store (path @ [ name ]) cclos args in
      let value = Value.Ref (path @ [ name ]) in
      let env = Env.insert name value env in
      (env, cenv, store)
  | _ -> (env, cenv, store)

and instantiate_decl (env : env) (tenv : tenv) (cenv : cenv) (store : store)
    (path : string list) (decl : Declaration.t) : env * tenv * cenv * store =
  match decl with
  (* Explicit instantiation *)
  | Instantiation { name; typ; args; _ } ->
      let name = name.str in
      let cclos = Cenv.find_from_type typ cenv in
      let store = instantiate_cclos env tenv cenv store (path @ [ name ]) cclos args in
      let value = Value.Ref (path @ [ name ]) in
      let env = Env.insert name value env in
      (env, tenv, cenv, store)
  (* Each table evaluates to a table instance (18.2) *)
  (* There is no syntax for specifying parameters that are tables
     Tables are only intended to be used from within the control
     where they are defined (Appendix F) *)
  | Table { name; properties; _ } ->
      let name = name.str in
      let obj = Object.Table { scope = env; properties } in
      let store = Store.insert (path @ [ name ]) obj store in
      let value = Value.Ref (path @ [ name ]) in
      let env = Env.insert name value env in
      (env, tenv, cenv, store)
  (* (TODO) is it correct to instantiate a value set at its declaration? *)
  (* There is no syntax for specifying parameters that are value-sets
     (Appendix F) *)
  | ValueSet { name; _ } ->
      let name = name.str in
      let obj = Object.ValueSet in
      let store = Store.insert (path @ [ name ]) obj store in
      let value = Value.Ref (path @ [ name ]) in
      let env = Env.insert name value env in
      (env, tenv, cenv, store)
  (* Load declarations to either env or cenv *)
  | _ ->
      let env, tenv, cenv = load env tenv cenv decl in
      (env, tenv, cenv, store)

let instantiate_program (program : program) =
  let (Program decls) = program in
  let env = Env.empty in
  let tenv = Tenv.empty in
  let cenv = Cenv.empty in
  let store = Store.empty in
  let _, _, _, store =
    List.fold_left
      (fun (env, tenv, cenv, store) decl ->
        instantiate_decl env tenv cenv store [] decl)
      (env, tenv, cenv, store) decls
  in
  store
