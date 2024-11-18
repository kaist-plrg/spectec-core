open Runtime.Domain
module Value = Runtime.Value
module Types = Runtime.Types
module Type = Types.Type
module FuncDef = Types.FuncDef
module E = Lang.Eq

let rec eq_typ_alpha (typ_a : Type.t) (typ_b : Type.t) : bool =
  match (typ_a, typ_b) with
  | VoidT, VoidT
  | ErrT, ErrT
  | MatchKindT, MatchKindT
  | StrT, StrT
  | BoolT, BoolT
  | IntT, IntT ->
      true
  | FIntT width_a, FIntT width_b
  | FBitT width_a, FBitT width_b
  | VBitT width_a, VBitT width_b ->
      Bigint.(width_a = width_b)
  | VarT id_a, VarT id_b -> id_a = id_b
  | NewT (id_a, typ_inner_a), NewT (id_b, typ_inner_b) ->
      Lang.Eq.eq_id' id_a id_b && eq_typ_alpha typ_inner_a typ_inner_b
  | EnumT (id_a, members_a), EnumT (id_b, members_b) ->
      Lang.Eq.eq_id' id_a id_b
      && List.for_all2 Lang.Eq.eq_member' members_a members_b
  | SEnumT (id_a, typ_inner_a, fields_a), SEnumT (id_b, typ_inner_b, fields_b)
    ->
      Lang.Eq.eq_id' id_a id_b
      && eq_typ_alpha typ_inner_a typ_inner_b
      && List.for_all2
           (fun (member_a, value_a) (member_b, value_b) ->
             Lang.Eq.eq_member' member_a member_b && Value.eq value_a value_b)
           fields_a fields_b
  | ListT typ_inner_a, ListT typ_inner_b -> eq_typ_alpha typ_inner_a typ_inner_b
  | TupleT typs_inner_a, TupleT typs_inner_b ->
      List.for_all2 eq_typ_alpha typs_inner_a typs_inner_b
  | StackT (typ_inner_a, size_a), StackT (typ_inner_b, size_b) ->
      eq_typ_alpha typ_inner_a typ_inner_b && Bigint.(size_a = size_b)
  | StructT (id_a, fields_a), StructT (id_b, fields_b)
  | HeaderT (id_a, fields_a), HeaderT (id_b, fields_b)
  | UnionT (id_a, fields_a), UnionT (id_b, fields_b) ->
      Lang.Eq.eq_id' id_a id_b
      && List.for_all2
           (fun (member_a, typ_inner_a) (member_b, typ_inner_b) ->
             Lang.Eq.eq_member' member_a member_b
             && eq_typ_alpha typ_inner_a typ_inner_b)
           fields_a fields_b
  | ExternT (id_a, fdenv_a), ExternT (id_b, fdenv_b) ->
      Lang.Eq.eq_id' id_a id_b && FIdMap.eq eq_funcdef_alpha fdenv_a fdenv_b
  | ParserT params_a, ParserT params_b | ControlT params_a, ControlT params_b ->
      List.for_all2 eq_param_alpha params_a params_b
  | PackageT, PackageT | AnyT, AnyT -> true
  | SeqT typs_inner_a, SeqT typs_inner_b ->
      List.for_all2 eq_typ_alpha typs_inner_a typs_inner_b
  | RecordT fields_a, RecordT fields_b ->
      List.for_all2
        (fun (member_a, typ_inner_a) (member_b, typ_inner_b) ->
          Lang.Eq.eq_member' member_a member_b
          && eq_typ_alpha typ_inner_a typ_inner_b)
        fields_a fields_b
  | InvalidT, InvalidT -> true
  | SetT typ_inner_a, SetT typ_inner_b -> eq_typ_alpha typ_inner_a typ_inner_b
  | StateT, StateT -> true
  | _ -> false

and eq_param_alpha (param_a : Types.param) (param_b : Types.param) : bool =
  let _, dir_a, typ_a, _ = param_a in
  let _, dir_b, typ_b, _ = param_b in
  Lang.Eq.eq_dir' dir_a dir_b && eq_typ_alpha typ_a typ_b

and eq_funcdef_alpha (funcdef_a : FuncDef.t) (funcdef_b : FuncDef.t) : bool =
  let eq_funcdef' tparams_a params_a typ_ret_a tparams_b params_b typ_ret_b =
    assert (List.length tparams_a = List.length tparams_b);
    let frees_a =
      Free.free_fd funcdef_a |> TIdSet.union (TIdSet.of_list tparams_a)
    in
    let frees_b =
      Free.free_fd funcdef_b |> TIdSet.union (TIdSet.of_list tparams_b)
    in
    let frees = TIdSet.union frees_a frees_b in
    let theta_a, theta_b, _, _ =
      List.fold_left2
        (fun (theta_a, theta_b, tparams_fresh, frees) tparam_a tparam_b ->
          let tparam_fresh, frees =
            let tparam_fresh =
              "Fresh" ^ string_of_int (List.length tparams_fresh)
            in
            if TIdSet.mem tparam_fresh frees then
              Subst.fresh_tvar tparam_fresh frees
            else (tparam_fresh, frees)
          in
          let theta_a =
            Subst.Theta.add tparam_a (Types.VarT tparam_fresh) theta_a
          in
          let theta_b =
            Subst.Theta.add tparam_b (Types.VarT tparam_fresh) theta_b
          in
          (theta_a, theta_b, tparams_fresh @ [ tparam_fresh ], frees))
        (Subst.Theta.empty, Subst.Theta.empty, [], frees)
        tparams_a tparams_b
    in
    let params_a = List.map (Subst.subst_param theta_a) params_a in
    let typ_ret_a = Subst.subst_typ theta_a typ_ret_a in
    let params_b = List.map (Subst.subst_param theta_b) params_b in
    let typ_ret_b = Subst.subst_typ theta_b typ_ret_b in
    List.length params_a = List.length params_b
    && List.for_all2 eq_param_alpha params_a params_b
    && eq_typ_alpha typ_ret_a typ_ret_b
  in
  match (funcdef_a, funcdef_b) with
  | ( ExternFunctionD (tparams_a, params_a, typ_a),
      ExternFunctionD (tparams_b, params_b, typ_b) )
  | ( FunctionD (tparams_a, params_a, typ_a),
      FunctionD (tparams_b, params_b, typ_b) )
  | ( ExternMethodD (tparams_a, params_a, typ_a),
      ExternMethodD (tparams_b, params_b, typ_b) )
  | ( ExternAbstractMethodD (tparams_a, params_a, typ_a),
      ExternAbstractMethodD (tparams_b, params_b, typ_b) ) ->
      List.length tparams_a = List.length tparams_b
      && eq_funcdef' tparams_a params_a typ_a tparams_b params_b typ_b
  | _ -> false

let check_eq_typ_alpha (typ_l : Type.t) (typ_r : Type.t) : unit =
  if not (eq_typ_alpha typ_l typ_r) then (
    Format.printf "(check_eq_typ_alpha) Types %a and %a are not equal\n" Type.pp
      typ_l Type.pp typ_r;
    assert false)
  else ()
