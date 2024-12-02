open Domain.Dom
open Tdom

type theta = typ TIdMap.t

(* Capture-avoiding substitution *)

(* alpha-conversion for forall (generic) types

   (forall X. tau) theta = forall X'. (tau theta')
   -- where
        X' not in U_{T in FV(tau) \ {X}} FV(theta(T))
        theta' = theta[X -> X'] *)

let rec fresh_tvar (tvar : TId.t) (free : TIdSet.t) : TId.t * TIdSet.t =
  let tvar_fresh = tvar ^ "'" in
  if TIdSet.mem tvar_fresh free then fresh_tvar tvar_fresh free
  else (tvar_fresh, TIdSet.add tvar_fresh free)

let subst_forall (theta : theta) (tvars : TId.t list) (frees_in : TIdSet.t) :
    theta * TId.t list =
  (* Collect type variables that will be substituted into *)
  let frees_into =
    TIdSet.elements frees_in
    |> List.map (fun free_in -> TIdMap.find_opt free_in theta)
    |> List.filter_map Fun.id |> List.map Free.free_typ
    |> List.fold_left TIdSet.union TIdSet.empty
  in
  (* Rename quantifiers to avoid capturing *)
  let theta, tvars, _ =
    List.fold_left
      (fun (theta, tvars, frees_into) tvar ->
        let tvar_new, frees_into =
          if TIdSet.mem tvar frees_into then fresh_tvar tvar frees_into
          else (tvar, frees_into)
        in
        let theta = TIdMap.add tvar (VarT tvar_new) theta in
        (theta, tvars @ [ tvar_new ], frees_into))
      (theta, [], frees_into) tvars
  in
  (theta, tvars)

(* Parameters *)

let rec subst_param (theta : theta) (param : param) : param =
  let id, dir, typ, value_default = param in
  let typ = subst_typ theta typ in
  (id, dir, typ, value_default)

and subst_params (theta : theta) (params : param list) : param list =
  List.map (subst_param theta) params

(* Constructor parameters *)

and subst_cparam (theta : theta) (cparam : cparam) : cparam =
  subst_param theta cparam

and subst_cparams (theta : theta) (cparams : cparam list) : cparam list =
  subst_params theta cparams

(* Types *)

and subst_typ (theta : theta) (typ : typ) : typ =
  match typ with
  | VoidT | ErrT | MatchKindT | StrT | BoolT | IntT | FIntT _ | FBitT _
  | VBitT _ ->
      typ
  | VarT id -> if TIdMap.mem id theta then TIdMap.find id theta else typ
  | SpecT (td, typs_inner) ->
      let td = subst_typdef theta td in
      let typs_inner = subst_typs theta typs_inner in
      SpecT (td, typs_inner)
  | NewT (id, typ_inner) ->
      let typ_inner = subst_typ theta typ_inner in
      NewT (id, typ_inner)
  | EnumT _ -> typ
  | SEnumT (id, typ_inner, fields) ->
      let typ_inner = subst_typ theta typ_inner in
      SEnumT (id, typ_inner, fields)
  | ListT typ_inner ->
      let typ_inner = subst_typ theta typ_inner in
      ListT typ_inner
  | TupleT typs_inner ->
      let typs_inner = subst_typs theta typs_inner in
      TupleT typs_inner
  | StackT (typ_inner, size) ->
      let typ_inner = subst_typ theta typ_inner in
      StackT (typ_inner, size)
  | StructT (id, fields) ->
      let fields =
        List.map (fun (id, typ) -> (id, subst_typ theta typ)) fields
      in
      StructT (id, fields)
  | HeaderT (id, fields) ->
      let fields =
        List.map (fun (id, typ) -> (id, subst_typ theta typ)) fields
      in
      HeaderT (id, fields)
  | UnionT (id, fields) ->
      let fields =
        List.map (fun (id, typ) -> (id, subst_typ theta typ)) fields
      in
      UnionT (id, fields)
  | ExternT (id, fdenv) ->
      let fdenv = FIdMap.map (fun fd -> subst_funcdef theta fd) fdenv in
      ExternT (id, fdenv)
  | ParserT params ->
      let params = subst_params theta params in
      ParserT params
  | ControlT params ->
      let params = subst_params theta params in
      ControlT params
  | PackageT typs_inner ->
      let typs_inner = subst_typs theta typs_inner in
      PackageT typs_inner
  | TableT typ_inner ->
      let typ_inner = subst_typ theta typ_inner in
      TableT typ_inner
  | AnyT | TableEnumT _ -> typ
  | TableStructT (id, fields) ->
      let fields =
        List.map (fun (id, typ) -> (id, subst_typ theta typ)) fields
      in
      TableStructT (id, fields)
  | SeqT typs_inner ->
      let typs_inner = subst_typs theta typs_inner in
      SeqT typs_inner
  | SeqDefaultT typs_inner ->
      let typs_inner = subst_typs theta typs_inner in
      SeqDefaultT typs_inner
  | RecordT fields ->
      let fields =
        List.map (fun (id, typ) -> (id, subst_typ theta typ)) fields
      in
      RecordT fields
  | RecordDefaultT fields ->
      let fields =
        List.map (fun (id, typ) -> (id, subst_typ theta typ)) fields
      in
      RecordDefaultT fields
  | DefaultT | InvalidT -> typ
  | SetT typ_inner ->
      let typ_inner = subst_typ theta typ_inner in
      SetT typ_inner
  | StateT -> typ

and subst_typs (theta : theta) (typs : typ list) : typ list =
  List.map (subst_typ theta) typs

(* Type definitions *)

and subst_typdef (theta : theta) (td : typdef) : typdef =
  let subst_typdef' (theta : theta) (frees_in : TIdSet.t)
      (tparams : tparam list) (tparams_hidden : tparam list) :
      theta * tparam list * tparam list =
    let frees_in =
      TIdSet.diff frees_in (TIdSet.of_list (tparams @ tparams_hidden))
    in
    let theta, tparams =
      subst_forall theta (tparams @ tparams_hidden) frees_in
    in
    let tparams, tparams_hidden =
      let is_hidden i = i >= List.length tparams - List.length tparams_hidden in
      ( List.filteri (fun i _ -> not (is_hidden i)) tparams,
        List.filteri (fun i _ -> is_hidden i) tparams )
    in
    (theta, tparams, tparams_hidden)
  in
  match td with
  | DefD typ_inner ->
      let typ_inner = subst_typ theta typ_inner in
      DefD typ_inner
  | NewD (id, typ_inner) ->
      let typ_inner = subst_typ theta typ_inner in
      NewD (id, typ_inner)
  | EnumD _ -> td
  | SEnumD (id, typ_inner, fields) ->
      let typ_inner = subst_typ theta typ_inner in
      SEnumD (id, typ_inner, fields)
  | ListD (tparam, typ_inner) ->
      let frees_in = Free.free_typ typ_inner in
      let theta, tparams, tparams_hidden =
        subst_typdef' theta frees_in [ tparam ] []
      in
      assert (List.length tparams = 1 && tparams_hidden = []);
      let tparam = List.hd tparams in
      let typ_inner = subst_typ theta typ_inner in
      ListD (tparam, typ_inner)
  | TupleD (tparams, typs_inner) ->
      let frees_in = Free.free_typs typs_inner in
      let theta, tparams, tparams_hidden =
        subst_typdef' theta frees_in tparams []
      in
      assert (tparams_hidden = []);
      let typs_inner = subst_typs theta typs_inner in
      TupleD (tparams, typs_inner)
  | StackD (tparam, typ_inner, size) ->
      let frees_in = Free.free_typ typ_inner in
      let theta, tparams, tparams_hidden =
        subst_typdef' theta frees_in [ tparam ] []
      in
      assert (List.length tparams = 1 && tparams_hidden = []);
      let tparam = List.hd tparams in
      let typ_inner = subst_typ theta typ_inner in
      StackD (tparam, typ_inner, size)
  | StructD (id, tparams, tparams_hidden, fields) ->
      let frees_in = List.map snd fields |> Free.free_typs in
      let theta, tparams, tparams_hidden =
        subst_typdef' theta frees_in tparams tparams_hidden
      in
      let fields =
        List.map (fun (id, typ) -> (id, subst_typ theta typ)) fields
      in
      StructD (id, tparams, tparams_hidden, fields)
  | HeaderD (id, tparams, tparams_hidden, fields) ->
      let frees_in = List.map snd fields |> Free.free_typs in
      let theta, tparams, tparams_hidden =
        subst_typdef' theta frees_in tparams tparams_hidden
      in
      let fields =
        List.map (fun (id, typ) -> (id, subst_typ theta typ)) fields
      in
      HeaderD (id, tparams, tparams_hidden, fields)
  | UnionD (id, tparams, tparams_hidden, fields) ->
      let frees_in = List.map snd fields |> Free.free_typs in
      let theta, tparams, tparams_hidden =
        subst_typdef' theta frees_in tparams tparams_hidden
      in
      let fields =
        List.map (fun (id, typ) -> (id, subst_typ theta typ)) fields
      in
      UnionD (id, tparams, tparams_hidden, fields)
  | ExternD (id, tparams, tparams_hidden, fdenv) ->
      let frees_in =
        FIdMap.bindings fdenv |> List.map snd |> List.map Free.free_funcdef
        |> List.fold_left TIdSet.union TIdSet.empty
      in
      let theta, tparams, tparams_hidden =
        subst_typdef' theta frees_in tparams tparams_hidden
      in
      let fdenv = FIdMap.map (fun fd -> subst_funcdef theta fd) fdenv in
      ExternD (id, tparams, tparams_hidden, fdenv)
  | ParserD (tparams, tparams_hidden, params) ->
      let frees_in = Free.free_params params in
      let theta, tparams, tparams_hidden =
        subst_typdef' theta frees_in tparams tparams_hidden
      in
      let params = List.map (subst_param theta) params in
      ParserD (tparams, tparams_hidden, params)
  | ControlD (tparams, tparams_hidden, params) ->
      let frees_in = Free.free_params params in
      let theta, tparams, tparams_hidden =
        subst_typdef' theta frees_in tparams tparams_hidden
      in
      let params = List.map (subst_param theta) params in
      ControlD (tparams, tparams_hidden, params)
  | PackageD (tparams, tparams_hidden, typs_inner) ->
      let frees_in = Free.free_typs typs_inner in
      let theta, tparams, tparams_hidden =
        subst_typdef' theta frees_in tparams tparams_hidden
      in
      let typs_inner = subst_typs theta typs_inner in
      PackageD (tparams, tparams_hidden, typs_inner)

(* Function types *)

and subst_functyp (theta : theta) (ft : functyp) : functyp =
  match ft with
  | ActionT params ->
      let params = subst_params theta params in
      ActionT params
  | ExternFunctionT (params, typ_ret) ->
      let params = subst_params theta params in
      let typ_ret = subst_typ theta typ_ret in
      ExternFunctionT (params, typ_ret)
  | FunctionT (params, typ_ret) ->
      let params = subst_params theta params in
      let typ_ret = subst_typ theta typ_ret in
      FunctionT (params, typ_ret)
  | ExternMethodT (params, typ_ret) ->
      let params = subst_params theta params in
      let typ_ret = subst_typ theta typ_ret in
      ExternMethodT (params, typ_ret)
  | ExternAbstractMethodT (params, typ_ret) ->
      let params = subst_params theta params in
      let typ_ret = subst_typ theta typ_ret in
      ExternAbstractMethodT (params, typ_ret)
  | ParserApplyMethodT params ->
      let params = subst_params theta params in
      ParserApplyMethodT params
  | ControlApplyMethodT params ->
      let params = subst_params theta params in
      ControlApplyMethodT params
  | BuiltinMethodT (params, typ_ret) ->
      let params = subst_params theta params in
      let typ_ret = subst_typ theta typ_ret in
      BuiltinMethodT (params, typ_ret)
  | TableApplyMethodT typ_ret ->
      let typ_ret = subst_typ theta typ_ret in
      TableApplyMethodT typ_ret

(* Function definitions *)

and subst_funcdef (theta : theta) (fd : funcdef) : funcdef =
  let subst_funcdef' (theta : theta) (tparams : tparam list)
      (tparams_hidden : tparam list) (params : param list) (typ_ret : typ) :
      tparam list * tparam list * param list * typ =
    let frees_in =
      TIdSet.union (Free.free_params params) (Free.free_typ typ_ret)
    in
    let frees_in = TIdSet.diff frees_in (TIdSet.of_list tparams) in
    let theta, tparams =
      subst_forall theta (tparams @ tparams_hidden) frees_in
    in
    let tparams, tparams_hidden =
      let is_hidden i = i >= List.length tparams - List.length tparams_hidden in
      ( List.filteri (fun i _ -> not (is_hidden i)) tparams,
        List.filteri (fun i _ -> is_hidden i) tparams )
    in
    let params = List.map (subst_param theta) params in
    let typ_ret = subst_typ theta typ_ret in
    (tparams, tparams_hidden, params, typ_ret)
  in
  match fd with
  | ActionD params ->
      let params = List.map (subst_param theta) params in
      ActionD params
  | ExternFunctionD (tparams, tparams_hidden, params, typ_ret) ->
      let tparams, tparams_hidden, params, typ_ret =
        subst_funcdef' theta tparams tparams_hidden params typ_ret
      in
      ExternFunctionD (tparams, tparams_hidden, params, typ_ret)
  | FunctionD (tparams, tparams_hidden, params, typ_ret) ->
      let tparams, tparams_hidden, params, typ_ret =
        subst_funcdef' theta tparams tparams_hidden params typ_ret
      in
      FunctionD (tparams, tparams_hidden, params, typ_ret)
  | ExternMethodD (tparams, tparams_hidden, params, typ_ret) ->
      let tparams, tparams_hidden, params, typ_ret =
        subst_funcdef' theta tparams tparams_hidden params typ_ret
      in
      ExternMethodD (tparams, tparams_hidden, params, typ_ret)
  | ExternAbstractMethodD (tparams, tparams_hidden, params, typ_ret) ->
      let tparams, tparams_hidden, params, typ_ret =
        subst_funcdef' theta tparams tparams_hidden params typ_ret
      in
      ExternAbstractMethodD (tparams, tparams_hidden, params, typ_ret)
  | ParserApplyMethodD params ->
      let params = List.map (subst_param theta) params in
      ParserApplyMethodD params
  | ControlApplyMethodD params ->
      let params = List.map (subst_param theta) params in
      ControlApplyMethodD params

(* Constructor types *)

and subst_constyp (theta : theta) (ct : constyp) : constyp =
  let cparams, typ_ret = ct in
  let cparams = subst_cparams theta cparams in
  let typ_ret = subst_typ theta typ_ret in
  (cparams, typ_ret)

(* Typedef specialization *)

let specialize_typdef (td : typdef) (targs : typ list) : typ =
  let check_arity tparams =
    if List.length targs <> List.length tparams then (
      Format.printf
        "(specialize_typdef) Type definition %a expects %d type arguments but \
         %d were given\n"
        Pp.pp_typdef td (List.length tparams) (List.length targs);
      assert false)
  in
  match td with
  | DefD typ_inner ->
      check_arity [];
      typ_inner
  | NewD (id, typ_inner) ->
      check_arity [];
      NewT (id, typ_inner)
  | EnumD (id, members) ->
      check_arity [];
      EnumT (id, members)
  | SEnumD (id, typ_inner, fields) ->
      check_arity [];
      SEnumT (id, typ_inner, fields)
  | ListD (tparam, typ_inner) ->
      check_arity [ tparam ];
      let theta = TIdMap.singleton tparam (List.hd targs) in
      let typ_inner = subst_typ theta typ_inner in
      ListT typ_inner
  | TupleD (tparams, typs_inner) ->
      check_arity tparams;
      let theta = List.combine tparams targs |> TIdMap.of_list in
      let typs_inner = subst_typs theta typs_inner in
      TupleT typs_inner
  | StackD (tparam, typ_inner, size) ->
      check_arity [ tparam ];
      let theta = TIdMap.singleton tparam (List.hd targs) in
      let typ_inner = subst_typ theta typ_inner in
      StackT (typ_inner, size)
  | StructD (id, tparams, tparams_hidden, fields) ->
      let tparams = tparams @ tparams_hidden in
      check_arity tparams;
      let theta = List.combine tparams targs |> TIdMap.of_list in
      let fields =
        List.map (fun (id, typ) -> (id, subst_typ theta typ)) fields
      in
      StructT (id, fields)
  | HeaderD (id, tparams, tparams_hidden, fields) ->
      let tparams = tparams @ tparams_hidden in
      check_arity tparams;
      let theta = List.combine tparams targs |> TIdMap.of_list in
      let fields =
        List.map (fun (id, typ) -> (id, subst_typ theta typ)) fields
      in
      HeaderT (id, fields)
  | UnionD (id, tparams, tparams_hidden, fields) ->
      let tparams = tparams @ tparams_hidden in
      check_arity tparams;
      let theta = List.combine tparams targs |> TIdMap.of_list in
      let fields =
        List.map (fun (id, typ) -> (id, subst_typ theta typ)) fields
      in
      UnionT (id, fields)
  | ExternD (id, tparams, tparams_hidden, fdenv) ->
      let tparams = tparams @ tparams_hidden in
      check_arity tparams;
      let theta = List.combine tparams targs |> TIdMap.of_list in
      let fdenv = FIdMap.map (subst_funcdef theta) fdenv in
      ExternT (id, fdenv)
  | ParserD (tparams, tparams_hidden, params) ->
      let tparams = tparams @ tparams_hidden in
      check_arity tparams;
      let theta = List.combine tparams targs |> TIdMap.of_list in
      let params = subst_params theta params in
      ParserT params
  | ControlD (tparams, tparams_hidden, params) ->
      let tparams = tparams @ tparams_hidden in
      check_arity tparams;
      let theta = List.combine tparams targs |> TIdMap.of_list in
      let params = subst_params theta params in
      ControlT params
  | PackageD (tparams, tparams_hidden, typs_inner) ->
      let tparams = tparams @ tparams_hidden in
      check_arity tparams;
      let theta = List.combine tparams targs |> TIdMap.of_list in
      let typs_inner = subst_typs theta typs_inner in
      PackageT typs_inner

(* Funcdef specialization *)

let specialize_funcdef (fresh : unit -> int) (fd : funcdef) (targs : typ list) :
    functyp * TId.t list =
  let check_arity tparams =
    if List.length targs <> List.length tparams then (
      Format.printf
        "(specialize_funcdef) Function %a expects %d type arguments but %d \
         were given\n"
        Pp.pp_funcdef fd (List.length tparams) (List.length targs);
      assert false)
  in
  let fresh_tid () = "__WILD_" ^ string_of_int (fresh ()) in
  let fresh_targ tid = VarT tid in
  let specialize_funcdef' ctor tparams tparams_hidden targs params typ_ret =
    let targs, tids_fresh =
      (* Insert fresh type variables if omitted
         Otherwise, check the arity *)
      if
        List.length targs = 0
        && List.length tparams + List.length tparams_hidden > 0
      then
        let tids_fresh =
          List.init
            (List.length tparams + List.length tparams_hidden)
            (fun _ -> fresh_tid ())
        in
        let targs = List.map fresh_targ tids_fresh in
        (targs, tids_fresh)
      else if
        List.length targs > 0
        && List.length tparams = List.length targs
        && List.length tparams_hidden > 0
      then
        let tids_fresh =
          List.init (List.length tparams_hidden) (fun _ -> fresh_tid ())
        in
        let targs = targs @ List.map fresh_targ tids_fresh in
        (targs, tids_fresh)
      else (
        check_arity targs;
        (targs, []))
    in
    let tparams = tparams @ tparams_hidden in
    let theta = List.combine tparams targs |> TIdMap.of_list in
    let params = subst_params theta params in
    let typ_ret = subst_typ theta typ_ret in
    let ft = ctor params typ_ret in
    (ft, tids_fresh)
  in
  match fd with
  | ActionD params ->
      check_arity [];
      (ActionT params, [])
  | ExternFunctionD (tparams, tparams_hidden, params, typ_ret) ->
      let ctor params typ_ret = ExternFunctionT (params, typ_ret) in
      specialize_funcdef' ctor tparams tparams_hidden targs params typ_ret
  | FunctionD (tparams, tparams_hidden, params, typ_ret) ->
      let ctor params typ_ret = FunctionT (params, typ_ret) in
      specialize_funcdef' ctor tparams tparams_hidden targs params typ_ret
  | ExternMethodD (tparams, tparams_hidden, params, typ_ret) ->
      let ctor params typ_ret = ExternMethodT (params, typ_ret) in
      specialize_funcdef' ctor tparams tparams_hidden targs params typ_ret
  | ExternAbstractMethodD (tparams, tparams_hidden, params, typ_ret) ->
      let ctor params typ_ret = ExternAbstractMethodT (params, typ_ret) in
      specialize_funcdef' ctor tparams tparams_hidden targs params typ_ret
  | ParserApplyMethodD params ->
      check_arity [];
      (ParserApplyMethodT params, [])
  | ControlApplyMethodD params ->
      check_arity [];
      (ControlApplyMethodT params, [])

(* Constructor definition specialization *)

and specialize_consdef (fresh : unit -> int) (cd : consdef) (targs : typ list) :
    constyp * TId.t list =
  let check_arity tparams =
    if List.length targs <> List.length tparams then (
      Format.printf
        "(specialize_consdef) Constructor %a expects %d type arguments but %d \
         were given\n"
        Pp.pp_consdef cd (List.length tparams) (List.length targs);
      assert false)
  in
  let fresh_tid () = "__WILD_" ^ string_of_int (fresh ()) in
  let fresh_targ tid = VarT tid in
  let tparams, tparams_hidden, cparams, typ = cd in
  (* Insert fresh type variables if omitted
     Otherwise, check the arity *)
  let targs, tids_fresh =
    (* Insert fresh type variables if omitted
       Otherwise, check the arity *)
    if
      List.length targs = 0
      && List.length tparams + List.length tparams_hidden > 0
    then
      let tids_fresh =
        List.init
          (List.length tparams + List.length tparams_hidden)
          (fun _ -> fresh_tid ())
      in
      let targs = List.map fresh_targ tids_fresh in
      (targs, tids_fresh)
    else if
      List.length targs > 0
      && List.length tparams = List.length targs
      && List.length tparams_hidden > 0
    then
      let tids_fresh =
        List.init (List.length tparams_hidden) (fun _ -> fresh_tid ())
      in
      let targs = targs @ List.map fresh_targ tids_fresh in
      (targs, tids_fresh)
    else (
      check_arity tparams;
      (targs, []))
  in
  let tparams = tparams @ tparams_hidden in
  let theta = List.combine tparams targs |> TIdMap.of_list in
  let cparams = subst_cparams theta cparams in
  let typ = subst_typ theta typ in
  let ct = (cparams, typ) in
  (ct, tids_fresh)

(* Saturation: recursive specialization *)

let rec saturate_typ (typ : typ) : typ =
  match typ with
  | SpecT (td, typs_inner) -> specialize_typdef td typs_inner |> saturate_typ
  | _ -> typ

(* (1* Saturation: specialize all type definitions *1) *)

(* (1* Parameters *1) *)

(* let rec saturate_param (param : param) : param = *)
(*   let id, dir, typ, value_default = param in *)
(*   let typ = saturate_typ typ in *)
(*   (id, dir, typ, value_default) *)

(* and saturate_params (params : param list) : param list = *)
(*   List.map saturate_param params *)

(* (1* Types *1) *)

(* and saturate_typ (typ : typ) : typ = *)
(*   match typ with *)
(*   | VoidT | ErrT | MatchKindT | StrT | BoolT | IntT | FIntT _ | FBitT _ *)
(*   | VBitT _ | VarT _ -> *)
(*       typ *)
(*   | SpecT (td, typs_inner) -> specialize_typdef td typs_inner |> saturate_typ *)
(*   | NewT (id, typ_inner) -> *)
(*       let typ_inner = saturate_typ typ_inner in *)
(*       NewT (id, typ_inner) *)
(*   | EnumT _ -> typ *)
(*   | SEnumT (id, typ_inner, fields) -> *)
(*       let typ_inner = saturate_typ typ_inner in *)
(*       SEnumT (id, typ_inner, fields) *)
(*   | ListT typ_inner -> *)
(*       let typ_inner = saturate_typ typ_inner in *)
(*       ListT typ_inner *)
(*   | TupleT typs_inner -> *)
(*       let typs_inner = saturate_typs typs_inner in *)
(*       TupleT typs_inner *)
(*   | StackT (typ_inner, size) -> *)
(*       let typ_inner = saturate_typ typ_inner in *)
(*       StackT (typ_inner, size) *)
(*   | StructT (id, fields) -> *)
(*       let fields = List.map (fun (id, typ) -> (id, saturate_typ typ)) fields in *)
(*       StructT (id, fields) *)
(*   | HeaderT (id, fields) -> *)
(*       let fields = List.map (fun (id, typ) -> (id, saturate_typ typ)) fields in *)
(*       HeaderT (id, fields) *)
(*   | UnionT (id, fields) -> *)
(*       let fields = List.map (fun (id, typ) -> (id, saturate_typ typ)) fields in *)
(*       UnionT (id, fields) *)
(*   | ExternT (id, fdenv) -> *)
(*       let fdenv = FIdMap.map (fun fd -> saturate_funcdef fd) fdenv in *)
(*       ExternT (id, fdenv) *)
(*   | ParserT params -> *)
(*       let params = saturate_params params in *)
(*       ParserT params *)
(*   | ControlT params -> *)
(*       let params = saturate_params params in *)
(*       ControlT params *)
(*   | PackageT typs_inner -> *)
(*       let typs_inner = saturate_typs typs_inner in *)
(*       PackageT typs_inner *)
(*   | TableT typ_inner -> *)
(*       let typ_inner = saturate_typ typ_inner in *)
(*       TableT typ_inner *)
(*   | AnyT | TableEnumT _ -> typ *)
(*   | TableStructT (id, fields) -> *)
(*       let fields = List.map (fun (id, typ) -> (id, saturate_typ typ)) fields in *)
(*       TableStructT (id, fields) *)
(*   | SeqT typs_inner -> *)
(*       let typs_inner = saturate_typs typs_inner in *)
(*       SeqT typs_inner *)
(*   | SeqDefaultT typs_inner -> *)
(*       let typs_inner = saturate_typs typs_inner in *)
(*       SeqDefaultT typs_inner *)
(*   | RecordT fields -> *)
(*       let fields = List.map (fun (id, typ) -> (id, saturate_typ typ)) fields in *)
(*       RecordT fields *)
(*   | RecordDefaultT fields -> *)
(*       let fields = List.map (fun (id, typ) -> (id, saturate_typ typ)) fields in *)
(*       RecordDefaultT fields *)
(*   | DefaultT | InvalidT -> typ *)
(*   | SetT typ_inner -> *)
(*       let typ_inner = saturate_typ typ_inner in *)
(*       SetT typ_inner *)
(*   | StateT -> typ *)

(* and saturate_typs (typs : typ list) : typ list = List.map saturate_typ typs *)

(* (1* Function definitions *1) *)

(* and saturate_funcdef (fd : funcdef) : funcdef = *)
(*   match fd with *)
(*   | ActionD params -> *)
(*       let params = saturate_params params in *)
(*       ActionD params *)
(*   | ExternFunctionD (tparams, tparams_hidden, params, typ_ret) -> *)
(*       let params = saturate_params params in *)
(*       let typ_ret = saturate_typ typ_ret in *)
(*       ExternFunctionD (tparams, tparams_hidden, params, typ_ret) *)
(*   | FunctionD (tparams, tparams_hidden, params, typ_ret) -> *)
(*       let params = saturate_params params in *)
(*       let typ_ret = saturate_typ typ_ret in *)
(*       FunctionD (tparams, tparams_hidden, params, typ_ret) *)
(*   | ExternMethodD (tparams, tparams_hidden, params, typ_ret) -> *)
(*       let params = saturate_params params in *)
(*       let typ_ret = saturate_typ typ_ret in *)
(*       ExternMethodD (tparams, tparams_hidden, params, typ_ret) *)
(*   | ExternAbstractMethodD (tparams, tparams_hidden, params, typ_ret) -> *)
(*       let params = saturate_params params in *)
(*       let typ_ret = saturate_typ typ_ret in *)
(*       ExternAbstractMethodD (tparams, tparams_hidden, params, typ_ret) *)
(*   | ParserApplyMethodD params -> *)
(*       let params = saturate_params params in *)
(*       ParserApplyMethodD params *)
(*   | ControlApplyMethodD params -> *)
(*       let params = saturate_params params in *)
(*       ControlApplyMethodD params *)
