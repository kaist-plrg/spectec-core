open Syntax
open Syntax.Ast

(* A hack to avoid module name conflict *)
module P4Type = Type
open Domain
open Domain.Scope
open Domain.Ccenv
open Domain.Ienv
open Utils

(* Utils *)

let rec fold_left3 f acc xs ys zs =
  match (xs, ys, zs) with
  | [], [], [] -> acc
  | x :: xs', y :: ys', z :: zs' ->
      let acc' = f acc x y z in
      fold_left3 f acc' xs' ys' zs'
  | _, _, _ -> failwith "(fold_left3) Lists have different lengths"

let rec name_from_type (typ : P4Type.t) : string =
  match typ with
  | P4Type.TypeName { name = BareName text; _ } -> text.str
  (* (TODO) how to consider type arguments? *)
  | P4Type.SpecializedType { base; _ } -> name_from_type base
  | _ ->
      Printf.sprintf "(name_from_type) Unexpected type: %s" "TODO" |> failwith

let rec cclos_from_type (typ : P4Type.t) (ccenv : CcEnv.t) :
    Cclos.t * P4Type.t list =
  match typ with
  | P4Type.TypeName { name = Name.BareName text; _ }
  | P4Type.TypeName { name = Name.QualifiedName ([], text); _ } ->
      (Env.find text.str ccenv, [])
  | P4Type.SpecializedType { base; args; _ } ->
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

let load_local_const (tdenv : TDEnv.t) (genv : Env.t) (lenv : Env.t)
    (sto : Sto.t) (name : string) (typ : P4Type.t) (value : Expression.t) :
    Env.t * Sto.t =
  let typ = Eval.eval_typ tdenv genv sto typ in
  let value = Eval.eval_expr tdenv genv sto value |> Ops.eval_cast typ in
  let lenv, sto = add_var name typ value lenv sto in
  (lenv, sto)

let pre_load_local_variable (tdenv : TDEnv.t) (genv : Env.t) (lenv : Env.t)
    (sto : Sto.t) (name : string) (typ : P4Type.t) : Env.t * Sto.t =
  (* When using an expression for the size, the expression
     must be parenthesized and compile-time known. (7.1.6.2) *)
  let typ = Eval.eval_typ tdenv genv sto typ in
  (* (TODO) this statically allocates "some" local variables to the heap,
     any better way to deal with block-local variable declaration? *)
  let lenv, sto = add_var_without_value name typ lenv sto in
  (lenv, sto)

let pre_load_local_params (tdenv : TDEnv.t) (genv : Env.t) (lenv : Env.t)
    (sto : Sto.t) (params : Parameter.t list) : Env.t * Sto.t =
  List.fold_left
    (fun (lenv, sto) (param : Parameter.t) ->
      let name = param.variable.str in
      let typ = Eval.eval_typ tdenv genv sto param.typ in
      (* (TODO) this statically allocates "some" parameters to the heap,
         any better way to deal with block-local variable declaration? *)
      let lenv, sto = add_var_without_value name typ lenv sto in
      (lenv, sto))
    (lenv, sto) params

let load_global_const (tdenv : TDEnv.t) (genv : Env.t) (sto : Sto.t)
    (name : string) (typ : P4Type.t) (value : Expression.t) : Env.t * Sto.t =
  let typ = Eval.eval_typ tdenv genv sto typ in
  let value = Eval.eval_expr tdenv genv sto value |> Ops.eval_cast typ in
  let genv, sto = add_var name typ value genv sto in
  (genv, sto)

let load_record_fields (tdenv : TDEnv.t) (genv : Env.t) (sto : Sto.t)
    (fields : Declaration.field list) : (string * Type.t) list =
  List.map
    (fun (field : Declaration.field) ->
      let name = field.name.str in
      let typ = Eval.eval_typ tdenv genv sto field.typ in
      (name, typ))
    fields

let rec load_decl (tdenv : TDEnv.t) (genv : Env.t) (sto : Sto.t)
    (ccenv : CcEnv.t) (decl : Declaration.t) : TDEnv.t * Env.t * Sto.t * CcEnv.t
    =
  match decl with
  (* Loading constants to global constant environment and type/value stores *)
  | Constant { name; typ; value; _ } ->
      let genv, sto = load_global_const tdenv genv sto name.str typ value in
      (tdenv, genv, sto, ccenv)
  (* Loading constructor closures to ccenv *)
  (* For package type declaration, also load to tdenv *)
  | PackageType { name; params; type_params; _ } ->
      let name = name.str in
      let typ = Type.TRef in
      let tdenv = TDEnv.add name typ tdenv in
      let tparams = List.map (fun (param : Text.t) -> param.str) type_params in
      let cclos = Cclos.CCPackage { params; tparams } in
      let ccenv = CcEnv.add name cclos ccenv in
      (tdenv, genv, sto, ccenv)
  | Parser { name; params; type_params; constructor_params; locals; states; _ }
    ->
      let name = name.str in
      let tparams = List.map (fun (param : Text.t) -> param.str) type_params in
      let cparams = constructor_params in
      let cclos =
        Cclos.CCParser
          { tdenv; genv; sto; params; tparams; cparams; locals; states }
      in
      let ccenv = CcEnv.add name cclos ccenv in
      (tdenv, genv, sto, ccenv)
  | Control { name; params; type_params; constructor_params; locals; apply; _ }
    ->
      let name = name.str in
      let tparams = List.map (fun (param : Text.t) -> param.str) type_params in
      let cparams = constructor_params in
      let cclos =
        Cclos.CCControl
          { tdenv; genv; sto; params; tparams; cparams; locals; apply }
      in
      let ccenv = CcEnv.add name cclos ccenv in
      (tdenv, genv, sto, ccenv)
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
        Cclos.CCExtern { tdenv; genv; sto; tparams; cparams; methods }
      in
      let ccenv = CcEnv.add name cclos ccenv in
      let typ = Type.TRef in
      let tdenv = TDEnv.add name typ tdenv in
      (tdenv, genv, sto, ccenv)
  (* Loading types to type definition environment *)
  | TypeDef { name; typ_or_decl; _ } -> (
      let name = name.str in
      match typ_or_decl with
      | Alternative.Left typ ->
          let typ = Eval.eval_typ tdenv genv sto typ in
          let tdenv = TDEnv.add name typ tdenv in
          (tdenv, genv, sto, ccenv)
      | Alternative.Right _decl ->
          Printf.printf "(TODO: load) Loading typedef with decl %s\n" name;
          (tdenv, genv, sto, ccenv))
  | NewType { name; typ_or_decl; _ } -> (
      let name = name.str in
      match typ_or_decl with
      | Alternative.Left typ ->
          let typ = Eval.eval_typ tdenv genv sto typ in
          let tdenv = TDEnv.add name typ tdenv in
          (tdenv, genv, sto, ccenv)
      | Alternative.Right _decl ->
          Printf.printf "(TODO: load) Loading newtype with decl %s\n" name;
          (tdenv, genv, sto, ccenv))
  | Enum { name; members; _ } ->
      let name = name.str in
      let entries = List.map (fun (member : Text.t) -> member.str) members in
      let typ = Type.TEnum { entries } in
      let tdenv = TDEnv.add name typ tdenv in
      (tdenv, genv, sto, ccenv)
  | SerializableEnum { name; typ; members; _ } ->
      let name = name.str in
      let typ = Eval.eval_typ tdenv genv sto typ in
      let entries =
        List.map
          (fun member ->
            let member : Text.t = fst member in
            member.str)
          members
      in
      let typ = Type.TSEnum { typ; entries } in
      let tdenv = TDEnv.add name typ tdenv in
      (tdenv, genv, sto, ccenv)
  | Header { name; fields; _ } ->
      let name = name.str in
      let entries = load_record_fields tdenv genv sto fields in
      let typ = Type.THeader { entries } in
      let tdenv = TDEnv.add name typ tdenv in
      (tdenv, genv, sto, ccenv)
  | HeaderUnion { name; fields; _ } ->
      let name = name.str in
      let entries = load_record_fields tdenv genv sto fields in
      let typ = Type.TUnion { entries } in
      let tdenv = TDEnv.add name typ tdenv in
      (tdenv, genv, sto, ccenv)
  | Struct { name; fields; _ } ->
      let name = name.str in
      let entries = load_record_fields tdenv genv sto fields in
      let typ = Type.TStruct { entries } in
      let tdenv = TDEnv.add name typ tdenv in
      (tdenv, genv, sto, ccenv)
  (* (TODO) A better representation of parser/control object types? *)
  | ParserType { name; _ } | ControlType { name; _ } ->
      let name = name.str in
      let typ = Type.TRef in
      let tdenv = TDEnv.add name typ tdenv in
      (tdenv, genv, sto, ccenv)
  | Function { name; _ } ->
      Printf.printf "(TODO: load) Loading function %s\n" name.str;
      (tdenv, genv, sto, ccenv)
  | _ -> (tdenv, genv, sto, ccenv)

(* Evaluating instantiation arguments,
   which may contain nameless instantiation *)

and eval_targs (tdenv : TDEnv.t) (tdenv_local : TDEnv.t) (genv : Env.t)
    (sto : Sto.t) (tparams : string list) (typs : P4Type.t list) : TDEnv.t =
  assert (List.length tparams = List.length typs);
  List.fold_left2
    (fun tdenv_local tparam typ ->
      let typ = Eval.eval_typ tdenv genv sto typ in
      TDEnv.add tparam typ tdenv_local)
    tdenv_local tparams typs

and eval_expr (tdenv : TDEnv.t) (genv : Env.t) (sto : Sto.t) (ccenv : CcEnv.t)
    (ienv : IEnv.t) (path : string list) (expr : Expression.t) :
    Value.t * IEnv.t =
  match expr with
  | NamelessInstantiation { typ; args; _ } ->
      let ienv = instantiate_expr tdenv genv sto ccenv ienv path typ args in
      let value = Value.VRef path in
      (value, ienv)
  | _ ->
      let value = Eval.eval_expr tdenv genv sto expr in
      (value, ienv)

and eval_args (tdenv : TDEnv.t) (genv : Env.t) (sto : Sto.t) (ccenv : CcEnv.t)
    (ienv : IEnv.t) (path : string list) (params : Parameter.t list)
    (args : Argument.t list) : string list * Type.t list * Value.t list * IEnv.t
    =
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
      let typ = Eval.eval_typ tdenv genv sto typ in
      let value, ienv =
        eval_expr tdenv genv sto ccenv ienv (path @ [ param ]) arg
      in
      (names @ [ param ], typs @ [ typ ], values @ [ value ], ienv))
    ([], [], [], ienv) params args

and eval_cargs (tdenv : TDEnv.t) (genv : Env.t) (sto : Sto.t)
    (lenv_local : Env.t) (sto_local : Sto.t) (ccenv : CcEnv.t) (ienv : IEnv.t)
    (path : string list) (cparams : Parameter.t list) (args : Argument.t list) :
    Env.t * Sto.t * IEnv.t =
  let names, typs, values, ienv =
    eval_args tdenv genv sto ccenv ienv path cparams args
  in
  let lenv_local, sto_local =
    fold_left3
      (fun (lenv, sto) name typ value -> add_var name typ value lenv sto)
      (lenv_local, sto_local) names typs values
  in
  (lenv_local, sto_local, ienv)

(* Instantiation of a constructor closure *)

and instantiate_cclos (tdenv : TDEnv.t) (genv : Env.t) (sto : Sto.t)
    (ccenv : CcEnv.t) (ienv : IEnv.t) (path : string list) (cclos : Cclos.t)
    (args : Argument.t list) (targs : P4Type.t list) : IEnv.t =
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
        sto = sto_local;
        params;
        tparams;
        cparams;
        locals;
        states;
      } ->
      let lenv_local = Env.empty in
      (* Add constructor arguments to local environment and store *)
      let tdenv_local = eval_targs tdenv tdenv_local genv sto tparams targs in
      let lenv_local, sto_local, ienv =
        eval_cargs tdenv genv sto lenv_local sto_local ccenv ienv path cparams
          args
      in
      (* Load parameters into local environment and store *)
      let lenv_local, sto_local =
        pre_load_local_params tdenv_local genv_local lenv_local sto_local params
      in
      (* Instantiate local instantiations, load constants and local variables *)
      let genv_local, lenv_local, sto_local, ienv =
        List.fold_left
          (fun (genv, lenv, sto, ienv) local ->
            instantiate_parser_local_decl tdenv_local genv lenv sto ccenv ienv
              path local)
          (genv_local, lenv_local, sto_local, ienv)
          locals
      in
      (* Build methods out of states *)
      let funcs_state, sto_local, ienv =
        List.fold_left
          (fun (funcs, sto_local, ienv) (state : Parser.state) ->
            let name = state.name.str in
            let body = state.statements in
            let transition = state.transition in
            let lenv_local, sto_local, ienv =
              List.fold_left
                (fun (lenv, sto, ienv) stmt ->
                  instantiate_stmt tdenv lenv sto ccenv ienv path stmt)
                (lenv_local, sto_local, ienv)
                body
            in
            let func =
              Func.FParser
                {
                  name;
                  params = [];
                  genv = genv_local;
                  lenv = lenv_local;
                  body;
                  transition;
                }
            in
            (funcs @ [ func ], sto_local, ienv))
          ([], sto_local, ienv) states
      in
      (* Build a method apply *)
      (* Conceptually, parser is an object with a single method, apply *)
      let body = List.filter_map var_decl_to_stmt locals in
      let func_apply =
        let tags = Info.M "" in
        let transition_start =
          Parser.Direct { tags; next = { tags; str = "start" } }
        in
        Func.FParser
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
        Object.OParser
          {
            tdenv = tdenv_local;
            sto = sto_local;
            funcs = func_apply :: funcs_state;
          }
      in
      let ienv = Env.add (Ds.Path.concat path) obj ienv in
      ienv
  (* The instantiation of a parser or control block recursively
     evaluates all stateful instantiations declared in the block (16.2) *)
  | CCControl
      {
        tdenv = tdenv_local;
        genv = genv_local;
        sto = sto_local;
        params;
        tparams;
        cparams;
        locals;
        apply;
        _;
      } ->
      let lenv_local = Env.empty in
      (* Add constructor arguments to local environment and store *)
      let tdenv_local = eval_targs tdenv tdenv_local genv sto tparams targs in
      let lenv_local, sto_local, ienv =
        eval_cargs tdenv genv sto lenv_local sto_local ccenv ienv path cparams
          args
      in
      (* Load parameters into local environment and store *)
      let lenv_local, sto_local =
        pre_load_local_params tdenv_local genv_local lenv_local sto_local params
      in
      (* Instantiate local instantiations, load constants and local variables *)
      let genv_local, lenv_local, sto_local, ienv =
        List.fold_left
          (fun (genv, lenv, sto, ienv) local ->
            instantiate_control_local_decl tdenv_local genv lenv sto ccenv ienv
              path local)
          (genv_local, lenv_local, sto_local, ienv)
          locals
      in
      (* Build a method apply *)
      (* Conceptually, control is an object with a single method, apply *)
      let init = List.filter_map var_decl_to_stmt locals in
      let lenv_local, sto_local, ienv =
        List.fold_left
          (fun (lenv, sto, ienv) stmt ->
            instantiate_stmt tdenv lenv sto ccenv ienv path stmt)
          (lenv_local, sto_local, ienv)
          apply.statements
      in
      let body =
        init @ [ BlockStatement { tags = apply.tags; block = apply } ]
      in
      let func_apply =
        Func.FNormal
          { name = "apply"; params; genv = genv_local; lenv = lenv_local; body }
      in
      let obj =
        Object.OControl
          { tdenv = tdenv_local; sto = sto_local; funcs = [ func_apply ] }
      in
      let ienv = IEnv.add (Ds.Path.concat path) obj ienv in
      ienv
  (* Others do not involve recursive instantiation other than the args *)
  | CCPackage { params; tparams } ->
      let tdenv_package = tdenv in
      let genv_package = genv in
      let sto_package = sto in
      (* Add constructor arguments to constant environment and value ienv *)
      let tdenv_package =
        eval_targs tdenv tdenv_package genv sto tparams targs
      in
      let names, typs, values, ienv =
        eval_args tdenv genv sto ccenv ienv path params args
      in
      let genv_package, sto_package =
        fold_left3
          (fun (genv, sto) name typ value -> add_var name typ value genv sto)
          (genv_package, sto_package)
          names typs values
      in
      let obj =
        Object.OPackage
          { tdenv = tdenv_package; genv = genv_package; sto = sto_package }
      in
      let ienv = IEnv.add (Ds.Path.concat path) obj ienv in
      ienv
  | CCExtern { tdenv; genv; sto; methods; _ } ->
      let funcs =
        List.fold_left
          (fun funcs (mthd : MethodPrototype.t) ->
            match mthd with
            | Method { name; params; type_params; _ } ->
                let name = name.str in
                let tparams =
                  List.map (fun (param : Text.t) -> param.str) type_params
                in
                funcs @ [ Func.FExtern { name; tparams; params; genv } ]
            | _ ->
                Printf.printf "(TODO: instantiate) Method %s\n" "TODO";
                funcs)
          [] methods
      in
      let obj = Object.OExtern { tdenv; sto; funcs } in
      let ienv = IEnv.add (Ds.Path.concat path) obj ienv in
      ienv

(* Instantiate from expression *)

and instantiate_expr (tdenv : TDEnv.t) (genv : Env.t) (sto : Sto.t)
    (ccenv : CcEnv.t) (ienv : IEnv.t) (path : string list) (typ : P4Type.t)
    (args : Argument.t list) : IEnv.t =
  let cclos, targs = cclos_from_type typ ccenv in
  let ienv =
    instantiate_cclos tdenv genv sto ccenv ienv path cclos args targs
  in
  ienv

(* Instantiate from statement *)

(* Controls and parsers are often instantiated exactly once.
   As a light syntactic sugar, control and parser declarations with no
   constructor parameters may be applied directly, as if they were an instance.
   This has the effect of creating and applying a local instance of that type. (15.1) *)

and instantiate_stmt (tdenv : TDEnv.t) (lenv : Env.t) (sto : Sto.t)
    (ccenv : CcEnv.t) (ienv : IEnv.t) (path : string list) (stmt : Statement.t)
    : Env.t * Sto.t * IEnv.t =
  match stmt with
  | BlockStatement { block; _ } ->
      let stmts = block.statements in
      List.fold_left
        (fun (lenv, sto, ienv) stmt ->
          instantiate_stmt tdenv lenv sto ccenv ienv path stmt)
        (lenv, sto, ienv) stmts
  | DirectApplication { typ; _ } ->
      let name = name_from_type typ in
      let cclos, targs = cclos_from_type typ ccenv in
      let ienv =
        instantiate_cclos tdenv Env.empty Sto.empty ccenv ienv (path @ [ name ])
          cclos [] targs
      in
      let typ = Type.TRef in
      let value = Value.VRef (path @ [ name ]) in
      let lenv, sto = add_var name typ value lenv sto in
      (lenv, sto, ienv)
  | _ -> (lenv, sto, ienv)

(* Instantiate from declaration *)

and instantiate_instantiation_decl (tdenv : TDEnv.t) (genv : Env.t)
    (sto : Sto.t) (ccenv : CcEnv.t) (ienv : IEnv.t) (path : string list)
    (name : string) (typ : P4Type.t) (args : Argument.t list) :
    Env.t * Sto.t * IEnv.t =
  let name = name in
  let cclos, targs = cclos_from_type typ ccenv in
  let ienv =
    instantiate_cclos tdenv genv sto ccenv ienv (path @ [ name ]) cclos args
      targs
  in
  let typ = Type.TRef in
  let value = Value.VRef (path @ [ name ]) in
  let genv, sto = add_var name typ value genv sto in
  (genv, sto, ienv)

(* parserLocalElement
   : constantDeclaration | instantiation
   | variableDeclaration | valueSetDeclaration; (13.2) *)

and instantiate_parser_local_decl (tdenv : TDEnv.t) (genv : Env.t)
    (lenv : Env.t) (sto : Sto.t) (ccenv : CcEnv.t) (ienv : IEnv.t)
    (path : string list) (decl : Declaration.t) : Env.t * Env.t * Sto.t * IEnv.t
    =
  match decl with
  | Instantiation { name; typ; args; _ } ->
      let genv, sto, ienv =
        instantiate_instantiation_decl tdenv genv sto ccenv ienv path name.str
          typ args
      in
      (genv, lenv, sto, ienv)
  | Constant { name; typ; value; _ } ->
      let lenv, sto = load_local_const tdenv genv lenv sto name.str typ value in
      (genv, lenv, sto, ienv)
  | Variable { name; typ; _ } ->
      let lenv, sto = pre_load_local_variable tdenv genv lenv sto name.str typ in
      (genv, lenv, sto, ienv)
  (* (TODO) is it correct to instantiate a value set at its declaration? *)
  (* There is no syntax for specifying parameters that are value-sets
     (Appendix F) *)
  | ValueSet { name; _ } ->
      let name = name.str in
      let obj = Object.OValueSet in
      let path = path @ [ name ] in
      let ienv = IEnv.add (Ds.Path.concat path) obj ienv in
      let typ = Type.TRef in
      let value = Value.VRef path in
      let lenv, sto = add_var name typ value lenv sto in
      (genv, lenv, sto, ienv)
  | _ -> failwith "(instantiate_parser_local_decl) Unexpected declaration."

(* controlLocalDeclaration
   : constantDeclaration | actionDeclaration
   | tableDeclaration | instantiation
   | variableDeclaration; (14) *)

and instantiate_control_local_decl (tdenv : TDEnv.t) (genv : Env.t)
    (lenv : Env.t) (sto : Sto.t) (ccenv : CcEnv.t) (ienv : IEnv.t)
    (path : string list) (decl : Declaration.t) : Env.t * Env.t * Sto.t * IEnv.t
    =
  match decl with
  | Instantiation { name; typ; args; _ } ->
      let genv, sto, ienv =
        instantiate_instantiation_decl tdenv genv sto ccenv ienv path name.str
          typ args
      in
      (genv, lenv, sto, ienv)
  | Constant { name; typ; value; _ } ->
      let lenv, sto = load_local_const tdenv genv lenv sto name.str typ value in
      (genv, lenv, sto, ienv)
  | Variable { name; typ; _ } ->
      let lenv, sto = pre_load_local_variable tdenv genv lenv sto name.str typ in
      (genv, lenv, sto, ienv)
  | Action _ ->
      Printf.printf "(TODO: instantiate) action\n";
      (genv, lenv, sto, ienv)
  (* Each table evaluates to a table instance (18.2) *)
  (* There is no syntax for specifying parameters that are tables
     Tables are only intended to be used from within the control
     where they are defined (Appendix F) *)
  | Table { name; properties; _ } ->
      let name = name.str in
      let path = path @ [ name ] in
      let obj = Object.OTable { genv; lenv; properties } in
      let ienv = IEnv.add (Ds.Path.concat path) obj ienv in
      let typ = Type.TRef in
      let value = Value.VRef path in
      let lenv, sto = add_var name typ value lenv sto in
      (genv, lenv, sto, ienv)
  | _ -> failwith "(instantiate_control_local_decl) Unexpected declaration."

and instantiate_toplevel_decl (tdenv : TDEnv.t) (genv : Env.t) (sto : Sto.t)
    (ccenv : CcEnv.t) (ienv : IEnv.t) (path : string list)
    (decl : Declaration.t) : TDEnv.t * Env.t * Sto.t * CcEnv.t * IEnv.t =
  match decl with
  (* Explicit instantiation *)
  | Instantiation { name; typ; args; _ } ->
      let genv, sto, ienv =
        instantiate_instantiation_decl tdenv genv sto ccenv ienv path name.str
          typ args
      in
      (tdenv, genv, sto, ccenv, ienv)
  (* Load declarations *)
  | _ ->
      let tdenv, genv, sto, ccenv = load_decl tdenv genv sto ccenv decl in
      (tdenv, genv, sto, ccenv, ienv)

let instantiate_program (program : program) =
  let (Program decls) = program in
  let tdenv, _, _, ccenv, ienv =
    List.fold_left
      (fun (tdenv, genv, sto, ccenv, ienv) decl ->
        instantiate_toplevel_decl tdenv genv sto ccenv ienv [] decl)
      (TDEnv.empty, Env.empty, Sto.empty, CcEnv.empty, IEnv.empty)
      decls
  in
  (tdenv, ccenv, ienv)
