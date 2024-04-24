open Syntax
open Syntax.Ast
open Runtime
open Runtime.Domain
open Runtime.Scope
open Utils

(* Utils *)

let rec fold_left3 f acc xs ys zs =
  match (xs, ys, zs) with
  | [], [], [] -> acc
  | x :: xs', y :: ys', z :: zs' ->
      let acc' = f acc x y z in
      fold_left3 f acc' xs' ys' zs'
  | _, _, _ -> failwith "(fold_left3) Lists have different lengths"

let rec name_from_type (typ : Type.t) : string =
  match typ with
  | TypeName { name = BareName text; _ } -> text.str
  (* (TODO) how to consider type arguments? *)
  | SpecializedType { base; _ } -> name_from_type base
  | _ ->
      Printf.sprintf "(name_from_type) Unexpected type: %s" "TODO" |> failwith

let rec cclos_from_type (typ : Type.t) (ccenv : ccenv) : cclos * Type.t list =
  match typ with
  | Type.TypeName { name = Name.BareName text; _ }
  | Type.TypeName { name = Name.QualifiedName ([], text); _ } ->
      (Env.find text.str ccenv, [])
  | Type.SpecializedType { base; args; _ } ->
      let cclos, _ = cclos_from_type base ccenv in
      (cclos, args)
  | _ -> assert false

(* Checkers *)

(* It is illegal to use names only for some arguments:
   either all or no arguments must specify the parameter name. (8.20) *)

let check_args (args : Argument.t list) =
  assert (
    List.for_all
      (fun (arg : Argument.t) ->
        match arg with Expression _ -> true | _ -> false)
      args
    || List.for_all
         (fun (arg : Argument.t) ->
           match arg with KeyValue _ -> true | _ -> false)
         args)

(* Instantiation *)

(* Loading the result of a declaration (other than instantiation),
   to typedef/global/closure environment, and type / value store *)

let load_local_const (tdenv : tdenv) (genv : env) (lenv : env) (tsto : tsto)
    (vsto : vsto) (name : string) (typ : Type.t) (value : Expression.t) :
    env * tsto * vsto =
  let typ = Eval.eval_typ tdenv genv vsto typ in
  let value = Eval.eval_expr tdenv genv vsto value |> Ops.eval_cast typ in
  let lenv, tsto, vsto = add_var name typ value lenv tsto vsto in
  (lenv, tsto, vsto)

let load_local_variable (tdenv : tdenv) (genv : env) (lenv : env) (tsto : tsto)
    (vsto : vsto) (name : string) (typ : Type.t) : env * tsto =
  (* When using an expression for the size, the expression
     must be parenthesized and compile-time known. (7.1.6.2) *)
  let typ = Eval.eval_typ tdenv genv vsto typ in
  (* (TODO) this statically allocates "some" local variables to the heap,
     any better way to deal with block-local variable declaration? *)
  let lenv, tsto = add_var_without_value name typ lenv tsto in
  (lenv, tsto)

let load_local_params (tdenv : tdenv) (genv : env) (lenv : env) (tsto : tsto)
    (vsto : vsto) (params : Parameter.t list) : env * tsto =
  List.fold_left
    (fun (lenv, tsto) (param : Parameter.t) ->
      let name = param.variable.str in
      let typ = Eval.eval_typ tdenv genv vsto param.typ in
      (* (TODO) this statically allocates "some" parameters to the heap,
         any better way to deal with block-local variable declaration? *)
      let lenv, tsto = add_var_without_value name typ lenv tsto in
      (lenv, tsto))
    (lenv, tsto) params

let load_global_const (tdenv : tdenv) (genv : env) (tsto : tsto) (vsto : vsto)
    (name : string) (typ : Type.t) (value : Expression.t) : env * tsto * vsto =
  let typ = Eval.eval_typ tdenv genv vsto typ in
  let value = Eval.eval_expr tdenv genv vsto value |> Ops.eval_cast typ in
  let genv, tsto, vsto = add_var name typ value genv tsto vsto in
  (genv, tsto, vsto)

let load_record_fields (tdenv : tdenv) (genv : env) (vsto : vsto)
    (fields : Declaration.field list) : (string * typ) list =
  List.map
    (fun (field : Declaration.field) ->
      let name = field.name.str in
      let typ = Eval.eval_typ tdenv genv vsto field.typ in
      (name, typ))
    fields

let rec load_decl (tdenv : tdenv) (genv : env) (tsto : tsto) (vsto : vsto)
    (ccenv : ccenv) (decl : Declaration.t) : tdenv * env * tsto * vsto * ccenv =
  match decl with
  (* Loading constants to global constant environment and type/value stores *)
  | Constant { name; typ; value; _ } ->
      let genv, tsto, vsto =
        load_global_const tdenv genv tsto vsto name.str typ value
      in
      (tdenv, genv, tsto, vsto, ccenv)
  (* Loading constructor closures to ccenv *)
  (* For package type declaration, also load to tdenv *)
  | PackageType { name; params; type_params; _ } ->
      let name = name.str in
      let typ = TRef in
      let tdenv = Env.add name typ tdenv in
      let tparams = List.map (fun (param : Text.t) -> param.str) type_params in
      let cclos = CCPackage { params; tparams } in
      let ccenv = Env.add name cclos ccenv in
      (tdenv, genv, tsto, vsto, ccenv)
  | Parser { name; params; type_params; constructor_params; locals; states; _ }
    ->
      let name = name.str in
      let tparams = List.map (fun (param : Text.t) -> param.str) type_params in
      let cparams = constructor_params in
      let cclos =
        CCParser
          { tdenv; genv; tsto; vsto; params; tparams; cparams; locals; states }
      in
      let ccenv = Env.add name cclos ccenv in
      (tdenv, genv, tsto, vsto, ccenv)
  | Control { name; params; type_params; constructor_params; locals; apply; _ }
    ->
      let name = name.str in
      let tparams = List.map (fun (param : Text.t) -> param.str) type_params in
      let cparams = constructor_params in
      let cclos =
        CCControl
          { tdenv; genv; tsto; vsto; params; tparams; cparams; locals; apply }
      in
      let ccenv = Env.add name cclos ccenv in
      (tdenv, genv, tsto, vsto, ccenv)
  (* For package type declaration, also load to tdenv *)
  | ExternObject { name; type_params; methods; _ } ->
      let name = name.str in
      let tparams = List.map (fun (param : Text.t) -> param.str) type_params in
      let cons, methods =
        List.partition
          (fun (mthd : MethodPrototype.t) ->
            match mthd with
            | Constructor { name = name_cons; _ } when name_cons.str = name ->
                true
            | _ -> false)
          methods
      in
      (* (TODO) support overloaded constructors *)
      assert (List.length cons <= 1);
      let cparams =
        match cons with Constructor { params; _ } :: [] -> params | _ -> []
      in
      let cclos =
        CCExtern { tdenv; genv; tsto; vsto; tparams; cparams; methods }
      in
      let ccenv = Env.add name cclos ccenv in
      let typ = TRef in
      let tdenv = Env.add name typ tdenv in
      (tdenv, genv, tsto, vsto, ccenv)
  (* Loading types to type definition environment *)
  | TypeDef { name; typ_or_decl; _ } -> (
      let name = name.str in
      match typ_or_decl with
      | Alternative.Left typ ->
          let typ = Eval.eval_typ tdenv genv vsto typ in
          let tdenv = Env.add name typ tdenv in
          (tdenv, genv, tsto, vsto, ccenv)
      | Alternative.Right _decl ->
          Printf.printf "(TODO: load) Loading typedef with decl %s\n" name;
          (tdenv, genv, tsto, vsto, ccenv))
  | NewType { name; typ_or_decl; _ } -> (
      let name = name.str in
      match typ_or_decl with
      | Alternative.Left typ ->
          let typ = Eval.eval_typ tdenv genv vsto typ in
          let tdenv = Env.add name typ tdenv in
          (tdenv, genv, tsto, vsto, ccenv)
      | Alternative.Right _decl ->
          Printf.printf "(TODO: load) Loading newtype with decl %s\n" name;
          (tdenv, genv, tsto, vsto, ccenv))
  | Enum { name; members; _ } ->
      let name = name.str in
      let entries = List.map (fun (member : Text.t) -> member.str) members in
      let typ = TEnum { entries } in
      let tdenv = Env.add name typ tdenv in
      (tdenv, genv, tsto, vsto, ccenv)
  | SerializableEnum { name; typ; members; _ } ->
      let name = name.str in
      let typ = Eval.eval_typ tdenv genv vsto typ in
      let entries =
        List.map
          (fun member ->
            let member : Text.t = fst member in
            member.str)
          members
      in
      let typ = TSEnum { typ; entries } in
      let tdenv = Env.add name typ tdenv in
      (tdenv, genv, tsto, vsto, ccenv)
  | Header { name; fields; _ } ->
      let name = name.str in
      let entries = load_record_fields tdenv genv vsto fields in
      let typ = THeader { entries } in
      let tdenv = Env.add name typ tdenv in
      (tdenv, genv, tsto, vsto, ccenv)
  | HeaderUnion { name; fields; _ } ->
      let name = name.str in
      let entries = load_record_fields tdenv genv vsto fields in
      let typ = TUnion { entries } in
      let tdenv = Env.add name typ tdenv in
      (tdenv, genv, tsto, vsto, ccenv)
  | Struct { name; fields; _ } ->
      let name = name.str in
      let entries = load_record_fields tdenv genv vsto fields in
      let typ = TStruct { entries } in
      let tdenv = Env.add name typ tdenv in
      (tdenv, genv, tsto, vsto, ccenv)
  (* (TODO) A better representation of parser/control object types? *)
  | ParserType { name; _ } | ControlType { name; _ } ->
      let name = name.str in
      let typ = TRef in
      let tdenv = Env.add name typ tdenv in
      (tdenv, genv, tsto, vsto, ccenv)
  | Function { name; _ } ->
      Printf.printf "(TODO: load) Loading function %s\n" name.str;
      (tdenv, genv, tsto, vsto, ccenv)
  | _ -> (tdenv, genv, tsto, vsto, ccenv)

(* Evaluating instantiation arguments,
   which may contain nameless instantiation *)

and eval_targs (tdenv : tdenv) (tdenv_local : tdenv) (genv : env) (vsto : vsto)
    (tparams : string list) (typs : Type.t list) : tdenv =
  assert (List.length tparams = List.length typs);
  List.fold_left2
    (fun tdenv_local tparam typ ->
      let typ = Eval.eval_typ tdenv genv vsto typ in
      Env.add tparam typ tdenv_local)
    tdenv_local tparams typs

and eval_expr (tdenv : tdenv) (genv : env) (tsto : tsto) (vsto : vsto)
    (ccenv : ccenv) (ienv : ienv) (path : string list) (expr : Expression.t) :
    value * ienv =
  match expr with
  | NamelessInstantiation { typ; args; _ } ->
      let ienv =
        instantiate_expr tdenv genv tsto vsto ccenv ienv path typ args
      in
      let value = VRef path in
      (value, ienv)
  | _ ->
      let value = Eval.eval_expr tdenv genv vsto expr in
      (value, ienv)

and eval_args (genv : env) (tsto : tsto) (vsto : vsto) (tdenv : tdenv)
    (ccenv : ccenv) (ienv : ienv) (path : string list)
    (params : Parameter.t list) (args : Argument.t list) :
    string list * typ list * value list * ienv =
  (* (TODO) assume there is no default argument *)
  assert (List.length params = List.length args);
  check_args args;
  let align_args (params, args) (param : Parameter.t) (arg : Argument.t) =
    match arg with
    | Expression { value; _ } ->
        (params @ [ (param.variable.str, param.typ) ], args @ [ value ])
    | KeyValue { key; value; _ } ->
        (params @ [ (key.str, param.typ) ], args @ [ value ])
    | _ -> failwith "(eval_args) Instantiation argument must not be missing."
  in
  let params, args = List.fold_left2 align_args ([], []) params args in
  List.fold_left2
    (fun (names, typs, values, ienv) (param, typ) arg ->
      let typ = Eval.eval_typ tdenv genv vsto typ in
      let value, ienv =
        eval_expr tdenv genv tsto vsto ccenv ienv (path @ [ param ]) arg
      in
      (names @ [ param ], typs @ [ typ ], values @ [ value ], ienv))
    ([], [], [], ienv) params args

and eval_cargs (tdenv : tdenv) (genv : env) (tsto : tsto) (vsto : vsto)
    (lenv_local : env) (tsto_local : tsto) (vsto_local : vsto) (ccenv : ccenv)
    (ienv : ienv) (path : string list) (cparams : Parameter.t list)
    (args : Argument.t list) : env * tsto * vsto * ienv =
  let names, typs, values, ienv =
    eval_args genv tsto vsto tdenv ccenv ienv path cparams args
  in
  let lenv_local, tsto_local, vsto_local =
    fold_left3
      (fun (lenv, tsto, vsto) name typ value ->
        add_var name typ value lenv tsto vsto)
      (lenv_local, tsto_local, vsto_local)
      names typs values
  in
  (lenv_local, tsto_local, vsto_local, ienv)

(* Instantiation of a constructor closure *)

and instantiate_cclos (tdenv : tdenv) (genv : env) (tsto : tsto) (vsto : vsto)
    (ccenv : ccenv) (ienv : ienv) (path : string list) (cclos : cclos)
    (args : Argument.t list) (targs : Type.t list) : ienv =
  let var_decl_to_stmt (decl : Declaration.t) : Statement.t option =
    match decl with
    | Variable { tags; _ } -> Some (DeclarationStatement { tags; decl })
    | _ -> None
  in
  match cclos with
  (* The instantiation of a parser or control block recursively
     evaluates all stateful instantiations declared in the block (16.2) *)
  | CCParser
      {
        tdenv = tdenv_local;
        genv = genv_local;
        tsto = tsto_local;
        vsto = vsto_local;
        params;
        tparams;
        cparams;
        locals;
        states;
      } ->
      let lenv_local = Env.empty in
      (* Add constructor arguments to local environment and store *)
      let tdenv_local = eval_targs tdenv tdenv_local genv vsto tparams targs in
      let lenv_local, tsto_local, vsto_local, ienv =
        eval_cargs tdenv genv tsto vsto lenv_local tsto_local vsto_local ccenv
          ienv path cparams args
      in
      (* Load parameters into local environment and store *)
      let lenv_local, tsto_local =
        load_local_params tdenv_local genv_local lenv_local tsto_local
          vsto_local params
      in
      (* Instantiate local instantiations, load constants and local variables *)
      let genv_local, lenv_local, tsto_local, vsto_local, ienv =
        List.fold_left
          (fun (genv, lenv, tsto, vsto, ienv) local ->
            instantiate_parser_local_decl tdenv_local genv lenv tsto vsto ccenv
              ienv path local)
          (genv_local, lenv_local, tsto_local, vsto_local, ienv)
          locals
      in
      (* Build methods out of states *)
      let funcs_state, tsto_local, vsto_local, ienv =
        List.fold_left
          (fun (funcs, tsto_local, vsto_local, ienv) (state : Parser.state) ->
            let name = state.name.str in
            let body = state.statements in
            let transition = state.transition in
            let lenv_local, tsto_local, vsto_local, ienv =
              List.fold_left
                (fun (lenv, tsto, vsto, ienv) stmt ->
                  instantiate_stmt tdenv lenv tsto vsto ccenv ienv path stmt)
                (lenv_local, tsto_local, vsto_local, ienv)
                body
            in
            let func =
              FParser
                {
                  name;
                  params = [];
                  genv = genv_local;
                  lenv = lenv_local;
                  body;
                  transition;
                }
            in
            (funcs @ [ func ], tsto_local, vsto_local, ienv))
          ([], tsto_local, vsto_local, ienv)
          states
      in
      (* Build a method apply *)
      (* Conceptually, parser is an object with a single method, apply *)
      let body = List.filter_map var_decl_to_stmt locals in
      let func_apply =
        let tags = Info.M "" in
        let transition_start =
          Parser.Direct { tags; next = { tags; str = "start" } }
        in
        FParser
          {
            name = "apply";
            params;
            genv = genv_local;
            lenv = lenv_local;
            body;
            transition = transition_start;
          }
      in
      let obj =
        OParser
          {
            tdenv = tdenv_local;
            tsto = tsto_local;
            vsto = vsto_local;
            funcs = func_apply :: funcs_state;
          }
      in
      let ienv = Env.add (Path.concat path) obj ienv in
      ienv
  (* The instantiation of a parser or control block recursively
     evaluates all stateful instantiations declared in the block (16.2) *)
  | CCControl
      {
        tdenv = tdenv_local;
        genv = genv_local;
        tsto = tsto_local;
        vsto = vsto_local;
        params;
        tparams;
        cparams;
        locals;
        apply;
        _;
      } ->
      let lenv_local = Env.empty in
      (* Add constructor arguments to local environment and store *)
      let tdenv_local = eval_targs tdenv tdenv_local genv vsto tparams targs in
      let lenv_local, tsto_local, vsto_local, ienv =
        eval_cargs tdenv genv tsto vsto lenv_local tsto_local vsto_local ccenv
          ienv path cparams args
      in
      (* Load parameters into local environment and store *)
      let lenv_local, tsto_local =
        load_local_params tdenv_local genv_local lenv_local tsto_local
          vsto_local params
      in
      (* Instantiate local instantiations, load constants and local variables *)
      let genv_local, lenv_local, tsto_local, vsto_local, ienv =
        List.fold_left
          (fun (genv, lenv, tsto, vsto, ienv) local ->
            instantiate_control_local_decl tdenv_local genv lenv tsto vsto ccenv
              ienv path local)
          (genv_local, lenv_local, tsto_local, vsto_local, ienv)
          locals
      in
      (* Build a method apply *)
      (* Conceptually, control is an object with a single method, apply *)
      let init = List.filter_map var_decl_to_stmt locals in
      let lenv_local, tsto_local, vsto_local, ienv =
        List.fold_left
          (fun (lenv, tsto, vsto, ienv) stmt ->
            instantiate_stmt tdenv lenv tsto vsto ccenv ienv path stmt)
          (lenv_local, tsto_local, vsto_local, ienv)
          apply.statements
      in
      let body =
        init @ [ BlockStatement { tags = apply.tags; block = apply } ]
      in
      let func_apply =
        FNormal
          { name = "apply"; params; genv = genv_local; lenv = lenv_local; body }
      in
      let obj =
        OControl
          {
            tdenv = tdenv_local;
            tsto = tsto_local;
            vsto = vsto_local;
            funcs = [ func_apply ];
          }
      in
      let ienv = Env.add (Path.concat path) obj ienv in
      ienv
  (* Others do not involve recursive instantiation other than the args *)
  | CCPackage { params; tparams } ->
      let tdenv_package = tdenv in
      let genv_package = genv in
      let tsto_package = tsto in
      let vsto_package = vsto in
      (* Add constructor arguments to constant environment and value ienv *)
      let tdenv_package =
        eval_targs tdenv tdenv_package genv vsto tparams targs
      in
      let names, typs, values, ienv =
        eval_args genv tsto vsto tdenv ccenv ienv path params args
      in
      let genv_package, tsto_package, vsto_package =
        fold_left3
          (fun (genv, tsto, vsto) name typ value ->
            add_var name typ value genv tsto vsto)
          (genv_package, tsto_package, vsto_package)
          names typs values
      in
      let obj =
        OPackage
          {
            tdenv = tdenv_package;
            genv = genv_package;
            tsto = tsto_package;
            vsto = vsto_package;
          }
      in
      let ienv = Env.add (Path.concat path) obj ienv in
      ienv
  | CCExtern { tdenv; genv; tsto; vsto; methods; _ } ->
      let funcs =
        List.fold_left
          (fun funcs (mthd : MethodPrototype.t) ->
            match mthd with
            | Method { name; params; type_params; _ } ->
                let name = name.str in
                let tparams =
                  List.map (fun (param : Text.t) -> param.str) type_params
                in
                funcs @ [ FExtern { name; tparams; params; genv } ]
            | _ ->
                Printf.printf "(TODO: instantiate) Method %s\n" "TODO";
                funcs)
          [] methods
      in
      let obj = OExtern { tdenv; tsto; vsto; funcs } in
      let ienv = Env.add (Path.concat path) obj ienv in
      ienv

(* Instantiate from expression *)

and instantiate_expr (tdenv : tdenv) (genv : env) (tsto : tsto) (vsto : vsto)
    (ccenv : ccenv) (ienv : ienv) (path : string list) (typ : Type.t)
    (args : Argument.t list) : ienv =
  let cclos, targs = cclos_from_type typ ccenv in
  let ienv =
    instantiate_cclos tdenv genv tsto vsto ccenv ienv path cclos args targs
  in
  ienv

(* Instantiate from statement *)

(* Controls and parsers are often instantiated exactly once.
   As a light syntactic sugar, control and parser declarations with no
   constructor parameters may be applied directly, as if they were an instance.
   This has the effect of creating and applying a local instance of that type. (15.1) *)

and instantiate_stmt (tdenv : tdenv) (lenv : env) (tsto : tsto) (vsto : vsto)
    (ccenv : ccenv) (ienv : ienv) (path : string list) (stmt : Statement.t) :
    env * tsto * vsto * ienv =
  match stmt with
  | BlockStatement { block; _ } ->
      let stmts = block.statements in
      List.fold_left
        (fun (lenv, tsto, vsto, ienv) stmt ->
          instantiate_stmt tdenv lenv tsto vsto ccenv ienv path stmt)
        (lenv, tsto, vsto, ienv) stmts
  | DirectApplication { typ; _ } ->
      let name = name_from_type typ in
      let cclos, targs = cclos_from_type typ ccenv in
      let ienv =
        instantiate_cclos tdenv Env.empty Heap.empty Heap.empty ccenv ienv
          (path @ [ name ]) cclos [] targs
      in
      let typ = TRef in
      let value = VRef (path @ [ name ]) in
      let lenv, tsto, vsto = add_var name typ value lenv tsto vsto in
      (lenv, tsto, vsto, ienv)
  | _ -> (lenv, tsto, vsto, ienv)

(* Instantiate from declaration *)

and instantiate_instantiation_decl (tdenv : tdenv) (genv : env) (tsto : tsto)
    (vsto : vsto) (ccenv : ccenv) (ienv : ienv) (path : string list)
    (name : string) (typ : Type.t) (args : Argument.t list) :
    env * tsto * vsto * ienv =
  let name = name in
  let cclos, targs = cclos_from_type typ ccenv in
  let ienv =
    instantiate_cclos tdenv genv tsto vsto ccenv ienv (path @ [ name ]) cclos
      args targs
  in
  let typ = TRef in
  let value = VRef (path @ [ name ]) in
  let genv, tsto, vsto = add_var name typ value genv tsto vsto in
  (genv, tsto, vsto, ienv)

(* parserLocalElement
   : constantDeclaration | instantiation
   | variableDeclaration | valueSetDeclaration; (13.2) *)

and instantiate_parser_local_decl (tdenv : tdenv) (genv : env) (lenv : env)
    (tsto : tsto) (vsto : vsto) (ccenv : ccenv) (ienv : ienv)
    (path : string list) (decl : Declaration.t) : env * env * tsto * vsto * ienv
    =
  match decl with
  | Instantiation { name; typ; args; _ } ->
      let genv, tsto, vsto, ienv =
        instantiate_instantiation_decl tdenv genv tsto vsto ccenv ienv path
          name.str typ args
      in
      (genv, lenv, tsto, vsto, ienv)
  | Constant { name; typ; value; _ } ->
      let lenv, tsto, vsto =
        load_local_const tdenv genv lenv tsto vsto name.str typ value
      in
      (genv, lenv, tsto, vsto, ienv)
  | Variable { name; typ; _ } ->
      let lenv, tsto =
        load_local_variable tdenv genv lenv tsto vsto name.str typ
      in
      (genv, lenv, tsto, vsto, ienv)
  (* (TODO) is it correct to instantiate a value set at its declaration? *)
  (* There is no syntax for specifying parameters that are value-sets
     (Appendix F) *)
  | ValueSet { name; _ } ->
      let name = name.str in
      let obj = OValueSet in
      let path = path @ [ name ] in
      let ienv = Env.add (Path.concat path) obj ienv in
      let typ = TRef in
      let value = VRef path in
      let lenv, tsto, vsto = add_var name typ value lenv tsto vsto in
      (genv, lenv, tsto, vsto, ienv)
  | _ -> failwith "(instantiate_parser_local_decl) Unexpected declaration."

(* controlLocalDeclaration
   : constantDeclaration | actionDeclaration
   | tableDeclaration | instantiation
   | variableDeclaration; (14) *)

and instantiate_control_local_decl (tdenv : tdenv) (genv : env) (lenv : env)
    (tsto : tsto) (vsto : vsto) (ccenv : ccenv) (ienv : ienv)
    (path : string list) (decl : Declaration.t) : env * env * tsto * vsto * ienv
    =
  match decl with
  | Instantiation { name; typ; args; _ } ->
      let genv, tsto, vsto, ienv =
        instantiate_instantiation_decl tdenv genv tsto vsto ccenv ienv path
          name.str typ args
      in
      (genv, lenv, tsto, vsto, ienv)
  | Constant { name; typ; value; _ } ->
      let lenv, tsto, vsto =
        load_local_const tdenv genv lenv tsto vsto name.str typ value
      in
      (genv, lenv, tsto, vsto, ienv)
  | Variable { name; typ; _ } ->
      let lenv, tsto =
        load_local_variable tdenv genv lenv tsto vsto name.str typ
      in
      (genv, lenv, tsto, vsto, ienv)
  | Action _ ->
      Printf.printf "(TODO: instantiate) action\n";
      (genv, lenv, tsto, vsto, ienv)
  (* Each table evaluates to a table instance (18.2) *)
  (* There is no syntax for specifying parameters that are tables
     Tables are only intended to be used from within the control
     where they are defined (Appendix F) *)
  | Table { name; properties; _ } ->
      let name = name.str in
      let path = path @ [ name ] in
      let obj = OTable { genv; lenv; properties } in
      let ienv = Env.add (Path.concat path) obj ienv in
      let typ = TRef in
      let value = VRef path in
      let lenv, tsto, vsto = add_var name typ value lenv tsto vsto in
      (genv, lenv, tsto, vsto, ienv)
  | _ -> failwith "(instantiate_control_local_decl) Unexpected declaration."

and instantiate_toplevel_decl (tdenv : tdenv) (genv : env) (tsto : tsto)
    (vsto : vsto) (ccenv : ccenv) (ienv : ienv) (path : string list)
    (decl : Declaration.t) : tdenv * env * tsto * vsto * ccenv * ienv =
  match decl with
  (* Explicit instantiation *)
  | Instantiation { name; typ; args; _ } ->
      let genv, tsto, vsto, ienv =
        instantiate_instantiation_decl tdenv genv tsto vsto ccenv ienv path
          name.str typ args
      in
      (tdenv, genv, tsto, vsto, ccenv, ienv)
  (* Load declarations *)
  | _ ->
      let tdenv, genv, tsto, vsto, ccenv =
        load_decl tdenv genv tsto vsto ccenv decl
      in
      (tdenv, genv, tsto, vsto, ccenv, ienv)

let instantiate_program (program : program) =
  let (Program decls) = program in
  let tdenv, _, _, _, ccenv, ienv =
    List.fold_left
      (fun (tdenv, genv, tsto, vsto, ccenv, ienv) decl ->
        instantiate_toplevel_decl tdenv genv tsto vsto ccenv ienv [] decl)
      (Env.empty, Env.empty, Heap.empty, Heap.empty, Env.empty, Env.empty)
      decls
  in
  (tdenv, ccenv, ienv)
