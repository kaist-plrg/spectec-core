open Runtime.Domain
module Types = Runtime.Types
module Type = Types.Type
module Envs = Runtime.Envs

(* Capture-avoiding substitution *)

module Theta = MakeTIdEnv (Type)

(* alpha-conversion for forall (generic) types

   (forall X. tau) theta = forall X'. (tau theta')
   -- where
        X' not in U_{T in FV(tau) \ {X}} FV(theta(T))
        theta' = theta[X -> X'] *)

let rec fresh_tvar (tvar : TId.t) (free : TIdSet.t) : TId.t * TIdSet.t =
  let tvar_fresh = tvar ^ "'" in
  if TIdSet.mem tvar_fresh free then fresh_tvar tvar_fresh free
  else (tvar_fresh, TIdSet.add tvar_fresh free)

let subst_forall (theta : Theta.t) (tvars : TId.t list) (frees_in : TIdSet.t) :
    Theta.t * TId.t list =
  (* Collect type variables that will be substituted into *)
  let frees_into =
    TIdSet.elements frees_in
    |> List.map (fun free_in -> Theta.find_opt free_in theta)
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
        let theta = Theta.add tvar (Types.VarT tvar_new) theta in
        (theta, tvars @ [ tvar_new ], frees_into))
      (theta, [], frees_into) tvars
  in
  (theta, tvars)

let rec subst_typ (theta : Theta.t) (typ : Type.t) : Type.t =
  match typ with
  | VoidT | ErrT | MatchKindT | StrT | BoolT | IntT | FIntT _ | FBitT _
  | VBitT _ ->
      typ
  | VarT id -> if Theta.mem id theta then Theta.find id theta else typ
  | NewT (id, typ_inner) ->
      let typ_inner = subst_typ theta typ_inner in
      NewT (id, typ_inner)
  | EnumT _ -> typ
  | SEnumT (id, typ_inner) ->
      let typ_inner = subst_typ theta typ_inner in
      SEnumT (id, typ_inner)
  | TupleT typs_inner ->
      let typs_inner = List.map (subst_typ theta) typs_inner in
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
      let fdenv = Envs.FDEnv.map (fun fd -> subst_funcdef theta fd) fdenv in
      ExternT (id, fdenv)
  | ParserT params ->
      let params = List.map (subst_param theta) params in
      ParserT params
  | ControlT params ->
      let params = List.map (subst_param theta) params in
      ControlT params
  | PackageT | TopT -> typ
  | SeqT typs_inner ->
      let typs_inner = List.map (subst_typ theta) typs_inner in
      SeqT typs_inner
  | RecordT fields ->
      let fields =
        List.map (fun (id, typ) -> (id, subst_typ theta typ)) fields
      in
      RecordT fields
  | SetT typ_inner ->
      let typ_inner = subst_typ theta typ_inner in
      SetT typ_inner
  | StateT -> typ
  | TableT typ_inner ->
      let typ_inner = subst_typ theta typ_inner in
      TableT typ_inner

and subst_param (theta : Theta.t) (param : Types.param) : Types.param =
  let id, dir, typ, value_default = param in
  let typ = subst_typ theta typ in
  (id, dir, typ, value_default)

and subst_funcdef (theta : Theta.t) (fd : Types.funcdef) : Types.funcdef =
  let subst_funcdef' (theta : Theta.t) (tparams : Types.tparam list)
      (params : Types.param list) (typ_ret : Type.t) :
      Types.tparam list * Types.param list * Type.t =
    let frees_in =
      List.map Free.free_param params @ [ Free.free_typ typ_ret ]
      |> List.fold_left TIdSet.union TIdSet.empty
    in
    let frees_in = TIdSet.diff frees_in (TIdSet.of_list tparams) in
    let theta, tparams = subst_forall theta tparams frees_in in
    let params = List.map (subst_param theta) params in
    let typ_ret = subst_typ theta typ_ret in
    (tparams, params, typ_ret)
  in
  match fd with
  | ExternFunctionD (tparams, params, typ_ret) ->
      let tparams, params, typ_ret =
        subst_funcdef' theta tparams params typ_ret
      in
      ExternFunctionD (tparams, params, typ_ret)
  | FunctionD (tparams, params, typ_ret) ->
      let tparams, params, typ_ret =
        subst_funcdef' theta tparams params typ_ret
      in
      FunctionD (tparams, params, typ_ret)
  | ActionD params ->
      let params = List.map (subst_param theta) params in
      ActionD params
  | ExternMethodD (tparams, params, typ_ret) ->
      let tparams, params, typ_ret =
        subst_funcdef' theta tparams params typ_ret
      in
      ExternMethodD (tparams, params, typ_ret)
  | ExternAbstractMethodD (tparams, params, typ_ret) ->
      let tparams, params, typ_ret =
        subst_funcdef' theta tparams params typ_ret
      in
      ExternAbstractMethodD (tparams, params, typ_ret)

and subst_cparam (theta : Theta.t) (cparam : Types.cparam) : Types.cparam =
  subst_param theta cparam
