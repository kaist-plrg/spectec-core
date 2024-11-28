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
  let subst_theta (theta : Theta.t) (theta_inner : Theta.t) : Theta.t =
    Theta.fold
      (fun tid typ theta_inner ->
        Theta.add tid (subst_typ theta typ) theta_inner)
      theta_inner Theta.empty
  in
  match typ with
  | VoidT | ErrT | MatchKindT | StrT | BoolT | IntT | FIntT _ | FBitT _
  | VBitT _ ->
      typ
  | VarT id -> if Theta.mem id theta then Theta.find id theta else typ
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
      let typs_inner = List.map (subst_typ theta) typs_inner in
      TupleT typs_inner
  | StackT (typ_inner, size) ->
      let typ_inner = subst_typ theta typ_inner in
      StackT (typ_inner, size)
  | StructT (id, fields, theta_inner) ->
      let fields =
        List.map (fun (id, typ) -> (id, subst_typ theta typ)) fields
      in
      let theta_inner = subst_theta theta theta_inner in
      StructT (id, fields, theta_inner)
  | HeaderT (id, fields, theta_inner) ->
      let fields =
        List.map (fun (id, typ) -> (id, subst_typ theta typ)) fields
      in
      let theta_inner = subst_theta theta theta_inner in
      HeaderT (id, fields, theta_inner)
  | UnionT (id, fields, theta_inner) ->
      let fields =
        List.map (fun (id, typ) -> (id, subst_typ theta typ)) fields
      in
      let theta_inner = subst_theta theta theta_inner in
      UnionT (id, fields, theta_inner)
  | ExternT (id, fdenv, theta_inner) ->
      let fdenv = Envs.FDEnv.map (fun fd -> subst_funcdef theta fd) fdenv in
      let theta_inner = subst_theta theta theta_inner in
      ExternT (id, fdenv, theta_inner)
  | ParserT (params, theta_inner) ->
      let params = List.map (subst_param theta) params in
      let theta_inner = subst_theta theta theta_inner in
      ParserT (params, theta_inner)
  | ControlT (params, theta_inner) ->
      let params = List.map (subst_param theta) params in
      let theta_inner = subst_theta theta theta_inner in
      ControlT (params, theta_inner)
  | PackageT (typs_inner, theta_inner) ->
      let typs_inner = List.map (subst_typ theta) typs_inner in
      let theta_inner = subst_theta theta theta_inner in
      PackageT (typs_inner, theta_inner)
  | AnyT -> typ
  | TableEnumT _ | TableStructT _ -> typ
  | SeqT typs_inner ->
      let typs_inner = List.map (subst_typ theta) typs_inner in
      SeqT typs_inner
  | SeqDefaultT typs_inner ->
      let typs_inner = List.map (subst_typ theta) typs_inner in
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
  | TableT typ_inner ->
      let typ_inner = subst_typ theta typ_inner in
      TableT typ_inner

and subst_param (theta : Theta.t) (param : Types.param) : Types.param =
  let id, dir, typ, value_default = param in
  let typ = subst_typ theta typ in
  (id, dir, typ, value_default)

and subst_functyp (theta : Theta.t) (ft : Types.functyp) : Types.functyp =
  match ft with
  | ActionT params ->
      let params = List.map (subst_param theta) params in
      ActionT params
  | ExternFunctionT (params, typ_ret) ->
      let params = List.map (subst_param theta) params in
      let typ_ret = subst_typ theta typ_ret in
      ExternFunctionT (params, typ_ret)
  | FunctionT (params, typ_ret) ->
      let params = List.map (subst_param theta) params in
      let typ_ret = subst_typ theta typ_ret in
      FunctionT (params, typ_ret)
  | ExternMethodT (params, typ_ret) ->
      let params = List.map (subst_param theta) params in
      let typ_ret = subst_typ theta typ_ret in
      ExternMethodT (params, typ_ret)
  | ExternAbstractMethodT (params, typ_ret) ->
      let params = List.map (subst_param theta) params in
      let typ_ret = subst_typ theta typ_ret in
      ExternAbstractMethodT (params, typ_ret)
  | ParserApplyMethodT params ->
      let params = List.map (subst_param theta) params in
      ParserApplyMethodT params
  | ControlApplyMethodT params ->
      let params = List.map (subst_param theta) params in
      ControlApplyMethodT params
  | BuiltinMethodT (params, typ_ret) ->
      let params = List.map (subst_param theta) params in
      let typ_ret = subst_typ theta typ_ret in
      BuiltinMethodT (params, typ_ret)
  | TableApplyMethodT typ_ret ->
      let typ_ret = subst_typ theta typ_ret in
      TableApplyMethodT typ_ret

and subst_funcdef (theta : Theta.t) (fd : Types.funcdef) : Types.funcdef =
  let subst_funcdef' (theta : Theta.t) (tparams : Types.tparam list)
      (tparams_hidden : Types.tparam list) (params : Types.param list)
      (typ_ret : Type.t) :
      Types.tparam list * Types.tparam list * Types.param list * Type.t =
    let frees_in =
      List.map Free.free_param params @ [ Free.free_typ typ_ret ]
      |> List.fold_left TIdSet.union TIdSet.empty
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

and subst_cparam (theta : Theta.t) (cparam : Types.cparam) : Types.cparam =
  subst_param theta cparam

and subst_constyp (theta : Theta.t) (ct : Types.constyp) : Types.constyp =
  let cparams, typ = ct in
  let cparams = List.map (subst_cparam theta) cparams in
  let typ = subst_typ theta typ in
  (cparams, typ)
