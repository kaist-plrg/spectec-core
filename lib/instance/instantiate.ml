open Syntax
open Ast
open Runtime
open Envs
open Utils

(* Store *)

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

let rec cclos_from_type (typ : Type.t) (ccenv : ccenv) :
    Cclosure.t * Type.t list =
  match typ with
  | Type.TypeName { name = Name.BareName text; _ } ->
      (CCEnv.find text.str ccenv, [])
  | Type.TypeName { name = Name.QualifiedName ([], text); _ } ->
      (CCEnv.find_toplevel text.str ccenv, [])
  | Type.SpecializedType { base; args; _ } ->
      let cclos, _ = cclos_from_type base ccenv in
      (cclos, args)
  | _ -> assert false

(* Instantiation *)

(* Loading the result of a declaration
   (other than instantiation) to cenv, tenv, tdenv, or ccenv *)

let rec load (cenv : cenv) (tenv : tenv) (tdenv : tdenv) (ccenv : ccenv)
    (decl : Declaration.t) : cenv * tenv * tdenv * ccenv =
  match decl with
  (* Loading constructor closures to ccenv *)
  | PackageType { name; params; type_params; _ } ->
      let name = name.str in
      let tparams = List.map (fun (param : Text.t) -> param.str) type_params in
      let cclos = Cclosure.Package { params; tparams } in
      let ccenv = CCEnv.add name cclos ccenv in
      (cenv, tenv, tdenv, ccenv)
  | Parser { name; params; type_params; constructor_params; locals; states; _ }
    ->
      let name = name.str in
      let tparams = List.map (fun (param : Text.t) -> param.str) type_params in
      let cparams = constructor_params in
      let cclos =
        Cclosure.Parser { params; tparams; cparams; locals; states }
      in
      let ccenv = CCEnv.add name cclos ccenv in
      (cenv, tenv, tdenv, ccenv)
  | Control { name; params; type_params; constructor_params; locals; apply; _ }
    ->
      let name = name.str in
      let tparams = List.map (fun (param : Text.t) -> param.str) type_params in
      let cparams = constructor_params in
      let cclos =
        Cclosure.Control { params; tparams; cparams; locals; apply }
      in
      let ccenv = CCEnv.add name cclos ccenv in
      (cenv, tenv, tdenv, ccenv)
  | ExternObject { name; _ } ->
      let name = name.str in
      let cclos = Cclosure.Extern in
      let ccenv = CCEnv.add name cclos ccenv in
      (cenv, tenv, tdenv, ccenv)
  (* Loading types to tdenv *)
  (* (TODO) assume type declarations are at top level only *)
  | TypeDef { name; typ_or_decl; _ } -> (
      let name = name.str in
      match typ_or_decl with
      | Alternative.Left typ ->
          let typ = Static.eval_typ cenv tdenv typ in
          let tdenv = TDEnv.add name typ tdenv in
          (cenv, tenv, tdenv, ccenv)
      | Alternative.Right _decl ->
          Printf.eprintf "(TODO: load) Loading typedef with decl %s\n" name;
          (cenv, tenv, tdenv, ccenv))
  | NewType { name; typ_or_decl; _ } -> (
      let name = name.str in
      match typ_or_decl with
      | Alternative.Left typ ->
          let typ = Static.eval_typ cenv tdenv typ in
          let tdenv = TDEnv.add name typ tdenv in
          (cenv, tenv, tdenv, ccenv)
      | Alternative.Right _decl ->
          Printf.eprintf "(TODO: load) Loading newtype with decl %s\n" name;
          (cenv, tenv, tdenv, ccenv))
  | Enum { name; members; _ } ->
      let name = name.str in
      let entries = List.map (fun (member : Text.t) -> member.str) members in
      let typ = Typ.Enum { entries } in
      let tdenv = TDEnv.add name typ tdenv in
      (cenv, tenv, tdenv, ccenv)
  | SerializableEnum { name; typ; members; _ } ->
      let name = name.str in
      let typ = Static.eval_typ cenv tdenv typ in
      let entries =
        List.map
          (fun member ->
            let member : Text.t = fst member in
            member.str)
          members
      in
      let typ = Typ.SEnum { typ; entries } in
      let tdenv = TDEnv.add name typ tdenv in
      (cenv, tenv, tdenv, ccenv)
  | Header { name; fields; _ } ->
      let name = name.str in
      let entries =
        List.map
          (fun (field : Declaration.field) ->
            let name = field.name.str in
            let typ = Static.eval_typ cenv tdenv field.typ in
            (name, typ))
          fields
      in
      let typ = Typ.Header { entries } in
      let tdenv = TDEnv.add name typ tdenv in
      (cenv, tenv, tdenv, ccenv)
  | HeaderUnion { name; fields; _ } ->
      let name = name.str in
      let entries =
        List.map
          (fun (field : Declaration.field) ->
            let name = field.name.str in
            let typ = Static.eval_typ cenv tdenv field.typ in
            (name, typ))
          fields
      in
      let typ = Typ.Union { entries } in
      let tdenv = TDEnv.add name typ tdenv in
      (cenv, tenv, tdenv, ccenv)
  | Struct { name; fields; _ } ->
      let name = name.str in
      let entries =
        List.map
          (fun (field : Declaration.field) ->
            let name = field.name.str in
            let typ = Static.eval_typ cenv tdenv field.typ in
            (name, typ))
          fields
      in
      let typ = Typ.Struct { entries } in
      let tdenv = TDEnv.add name typ tdenv in
      (cenv, tenv, tdenv, ccenv)
  (* (TODO) A better representation of parser/control object types? *)
  | ParserType { name; _ } | ControlType { name; _ } ->
      let name = name.str in
      let typ = Typ.Ref in
      let tdenv = TDEnv.add name typ tdenv in
      (cenv, tenv, tdenv, ccenv)
  | Function { name; _ } ->
      Printf.eprintf "(TODO: load) Loading function %s\n" name.str;
      (cenv, tenv, tdenv, ccenv)
  (* Loading constants to cenv *)
  | Constant { name; typ; value; _ } ->
      let typ = Static.eval_typ cenv tdenv typ in
      let value = Static.eval_expr cenv tdenv value in
      let value = Ops.eval_cast typ value in
      let cenv = CEnv.add name.str value cenv in
      let tenv = TEnv.add name.str typ tenv in
      (cenv, tenv, tdenv, ccenv)
  | _ -> (cenv, tenv, tdenv, ccenv)

(* Evaluating instantiation arguments,
   which may contain nameless instantiation *)

and eval_targs (cenv : cenv) (tdenv : tdenv) (tparams : string list)
    (typs : Type.t list) : tdenv =
  print_endline "Evaluating type arguments";
  String.concat ", " tparams |> print_endline;
  String.concat ", " (List.map (fun typ -> Pretty.print_type typ) typs)
  |> print_endline;
  assert (List.length tparams = List.length typs);
  List.fold_left2
    (fun tdenv tparam typ ->
      let typ = Static.eval_typ cenv tdenv typ in
      TDEnv.add tparam typ tdenv)
    tdenv tparams typs

and eval_expr (cenv : cenv) (tenv : tenv) (tdenv : tdenv) (ccenv : ccenv)
    (store : store) (path : string list) (expr : Expression.t) : Value.t * store
    =
  match expr with
  | NamelessInstantiation { typ; args; _ } ->
      let store = instantiate_expr cenv tenv tdenv ccenv store path typ args in
      let value = Value.Ref path in
      (value, store)
  | _ ->
      let value = Static.eval_expr cenv tdenv expr in
      (value, store)

and eval_args (cenv : cenv) (tenv : tenv) (tdenv : tdenv) (ccenv : ccenv)
    (store : store) (path : string list) (params : Parameter.t list)
    (args : Argument.t list) : cenv * tenv * store =
  (* (TODO) assume there is no default argument *)
  assert (List.length params = List.length args);
  (* It is illegal to use names only for some arguments:
     either all or no arguments must specify the parameter name. (8.20) *)
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
            (params @ [ (param.variable.str, param.typ) ], args @ [ value ])
        | KeyValue { key; value; _ } ->
            (params @ [ (key.str, param.typ) ], args @ [ value ])
        | _ ->
            failwith "(eval_args) Instantiation argument must not be missing.")
      ([], []) params args
  in
  List.fold_left2
    (fun (cenv, tenv, store) param arg ->
      let param, typ = param in
      let value, store =
        eval_expr cenv tenv tdenv ccenv store (path @ [ param ]) arg
      in
      let typ = Static.eval_typ cenv tdenv typ in
      let cenv = CEnv.add param value cenv in
      let tenv = TEnv.add param typ tenv in
      (cenv, tenv, store))
    (cenv, tenv, store) params args

(* Instantiation of a constructor closure *)

and instantiate_cclos (cenv : cenv) (tenv : tenv) (tdenv : tdenv) (ccenv : ccenv)
    (store : store) (path : string list) (cclos : Cclosure.t)
    (args : Argument.t list) (_typs : Type.t list) : store =
  match cclos with
  (* The instantiation of a parser or control block recursively
     evaluates all stateful instantiations declared in the block (16.2) *)
  (* Every time a parser is instantiated, it causes:
     every extern and parser instantiation that is in the parser source code
     at its top level is instantiated 1 time (p4guide) *)
  | Cclosure.Parser { params; tparams = _tparams; cparams; locals; states } ->
      let cenv_parser = CEnv.enter cenv in
      let tenv_parser = TEnv.enter tenv in
      let tdenv_parser = tdenv in
      let lenv_parser = LEnv.empty in
      let ccenv_parser = CCEnv.enter ccenv in
      (* let tdenv_parser = eval_targs cenv tdenv_parser tparams typs in *)
      let cenv_parser, tenv_parser, store =
        eval_args cenv_parser tenv_parser tdenv_parser ccenv_parser store path
          cparams args
      in
      let cenv_parser, tenv_parser, tdenv_parser, lenv_parser, ccenv_parser, store =
        List.fold_left
          (fun (cenv, tenv, tdenv, lenv, ccenv, store) local ->
            instantiate_parser_local_decl cenv tenv tdenv lenv ccenv store path local)
          (cenv_parser, tenv_parser, tdenv_parser, lenv_parser, ccenv_parser, store)
          locals
      in
      let stmts =
        List.concat_map (fun (state : Parser.state) -> state.statements) states
      in
      let cenv_parser, tenv_parser, store =
        List.fold_left
          (fun (cenv, tenv, store) stmt ->
            instantiate_stmt cenv tenv tdenv ccenv_parser store path stmt)
          (cenv_parser, tenv_parser, store)
          stmts
      in
      let obj =
        Object.Parser
          {
            cenv = cenv_parser;
            tenv = tenv_parser;
            tdenv = tdenv_parser;
            lenv = lenv_parser;
            params;
            locals;
            states;
          }
      in
      let store = Store.insert path obj store in
      store
  (* Every time a control is instantiated, it causes:
     1 instantiation of each table defined within it
     every extern and control instantiation that is in the control source code
     at its top level is instantiated 1 time (p4guide) *)
  | Cclosure.Control { params; tparams = _tparams; cparams; locals; apply; _ }
    ->
      let cenv_control = CEnv.enter cenv in
      let tenv_control = TEnv.enter tenv in
      let tdenv_control = tdenv in
      let lenv_control = LEnv.empty in
      let ccenv_control = CCEnv.enter ccenv in
      (* let tdenv_control = eval_targs cenv tdenv_control tparams typs in *)
      let cenv_control, tenv_control, store =
        eval_args cenv_control tenv_control tdenv_control ccenv_control store
          path cparams args
      in
      let cenv_control, tenv_control, tdenv_control, lenv_control, ccenv_control, store =
        List.fold_left
          (fun (cenv, tenv, tdenv, lenv, ccenv, store) local ->
            instantiate_control_local_decl cenv tenv tdenv lenv ccenv store path local)
          (cenv_control, tenv_control, tdenv_control, lenv_control, ccenv_control, store)
          locals
      in
      let stmts = apply.statements in
      let cenv_control, tenv_control, store =
        List.fold_left
          (fun (cenv, tenv, store) stmt ->
            instantiate_stmt cenv tenv tdenv ccenv_control store path stmt)
          (cenv_control, tenv_control, store)
          stmts
      in
      let obj =
        Object.Control
          {
            cenv = cenv_control;
            tenv = tenv_control;
            tdenv = tdenv_control;
            lenv = lenv_control;
            params;
            locals;
            apply;
          }
      in
      let store = Store.insert path obj store in
      store
  (* Others do not involve recursive instantiation other than the args *)
  | Cclosure.Package { params; tparams = _tparams } ->
      let cenv_package = CEnv.enter cenv in
      let tenv_package = TEnv.enter tenv in
      let tdenv_package = tdenv in
      (* let tdenv_package = eval_targs cenv tdenv_package tparams typs in *)
      let cenv_package, _tenv_package, store =
        eval_args cenv_package tenv_package tdenv_package ccenv store path params
          args
      in
      let obj =
        Object.Package
          { cenv = cenv_package; tenv = tenv_package; tdenv = tdenv_package }
      in
      let store = Store.insert path obj store in
      store
  | Cclosure.Extern ->
      let obj = Object.Extern in
      let store = Store.insert path obj store in
      store

(* Instantiate from expression *)

and instantiate_expr (cenv : cenv) (tenv : tenv) (tdenv : tdenv) (ccenv : ccenv)
    (store : store) (path : string list) (typ : Type.t) (args : Argument.t list)
    : store =
  let cclos, targs = cclos_from_type typ ccenv in
  let store =
    instantiate_cclos cenv tenv tdenv ccenv store path cclos args targs
  in
  store

(* Instantiate from statement *)

and instantiate_stmt (cenv : cenv) (tenv : tenv) (tdenv : tdenv) (ccenv : ccenv)
    (store : store) (path : string list) (stmt : Statement.t) :
    cenv * tenv * store =
  match stmt with
  | BlockStatement { block; _ } ->
      let stmts = block.statements in
      List.fold_left
        (fun (cenv, tenv, store) stmt ->
          instantiate_stmt cenv tenv tdenv ccenv store path stmt)
        (cenv, tenv, store) stmts
  | DirectApplication { typ; args; _ } ->
      let name = name_from_type typ in
      let cclos, targs = cclos_from_type typ ccenv in
      let store =
        instantiate_cclos cenv tenv tdenv ccenv store (path @ [ name ]) cclos
          args targs
      in
      let value = Value.Ref (path @ [ name ]) in
      let typ = Typ.Ref in
      let cenv = CEnv.add name value cenv in
      let tenv = TEnv.add name typ tenv in
      (cenv, tenv, store)
  | _ -> (cenv, tenv, store)

(* Instantiate from declaration *)

and instantiate_parser_local_decl (cenv : cenv) (tenv : tenv) (tdenv : tdenv) (lenv : lenv) (ccenv : ccenv)
    (store : store) (path : string list) (decl : Declaration.t) :
    cenv * tenv * tdenv * lenv * ccenv * store =
  (* parserLocalElement
    : constantDeclaration
    | instantiation
    | variableDeclaration
    | valueSetDeclaration; (13.2) *)
  match decl with
  (* (TODO) is it correct to instantiate a value set at its declaration? *)
  (* There is no syntax for specifying parameters that are value-sets
     (Appendix F) *)
  | ValueSet { name; _ } ->
      let name = name.str in
      let obj = Object.ValueSet in
      let store = Store.insert (path @ [ name ]) obj store in
      let value = Value.Ref (path @ [ name ]) in
      let typ = Typ.Ref in
      let cenv = CEnv.add name value cenv in
      let tenv = TEnv.add name typ tenv in
      (cenv, tenv, tdenv, lenv, ccenv, store)
  | Variable { name; _ } ->
      let name = name.str in
      let lenv = LEnv.add name lenv in
      (cenv, tenv, tdenv, lenv, ccenv, store)
  | _ ->
      let cenv, tenv, tdenv, ccenv, store =
        instantiate_decl cenv tenv tdenv ccenv store path decl
      in
      (cenv, tenv, tdenv, lenv, ccenv, store)

and instantiate_control_local_decl (cenv : cenv) (tenv : tenv) (tdenv : tdenv) (lenv : lenv) (ccenv : ccenv)
    (store : store) (path : string list) (decl : Declaration.t) :
    cenv * tenv * tdenv * lenv * ccenv * store =
  (* controlLocalDeclaration
    : constantDeclaration
    | actionDeclaration
    | tableDeclaration
    | instantiation
    | variableDeclaration; (14) *)
  match decl with
  (* Each table evaluates to a table instance (18.2) *)
  (* There is no syntax for specifying parameters that are tables
     Tables are only intended to be used from within the control
     where they are defined (Appendix F) *)
  | Table { name; properties; _ } ->
      let name = name.str in
      let obj = Object.Table { lenv; properties } in
      let store = Store.insert (path @ [ name ]) obj store in
      let value = Value.Ref (path @ [ name ]) in
      let typ = Typ.Ref in
      let cenv = CEnv.add name value cenv in
      let tenv = TEnv.add name typ tenv in
      (cenv, tenv, tdenv, lenv, ccenv, store)
  | Variable { name; _ } ->
      let name = name.str in
      let lenv = LEnv.add name lenv in
      (cenv, tenv, tdenv, lenv, ccenv, store)
  | _ ->
      let cenv, tenv, tdenv, ccenv, store =
        instantiate_decl cenv tenv tdenv ccenv store path decl
      in
      (cenv, tenv, tdenv, lenv, ccenv, store)

and instantiate_decl (cenv : cenv) (tenv : tenv) (tdenv : tdenv) (ccenv : ccenv)
    (store : store) (path : string list) (decl : Declaration.t) :
    cenv * tenv * tdenv * ccenv * store =
  match decl with
  (* Explicit instantiation *)
  | Instantiation { name; typ; args; _ } ->
      let name = name.str in
      let cclos, targs = cclos_from_type typ ccenv in
      let store =
        instantiate_cclos cenv tenv tdenv ccenv store (path @ [ name ]) cclos
          args targs
      in
      let value = Value.Ref (path @ [ name ]) in
      let cenv = CEnv.add name value cenv in
      (cenv, tenv, tdenv, ccenv, store)
  (* Load declarations to either cenv or ccenv *)
  | _ ->
      let cenv, tenv, tdenv, ccenv = load cenv tenv tdenv ccenv decl in
      (cenv, tenv, tdenv, ccenv, store)

let instantiate_program (program : program) =
  let (Program decls) = program in
  let cenv = CEnv.empty in
  let tenv = TEnv.empty in
  let tdenv = TDEnv.empty in
  let ccenv = CCEnv.empty in
  let store = Store.empty in
  let _, _, _, _, store =
    List.fold_left
      (fun (cenv, tenv, tdenv, ccenv, store) decl ->
        instantiate_decl cenv tenv tdenv ccenv store [] decl)
      (cenv, tenv, tdenv, ccenv, store)
      decls
  in
  store
