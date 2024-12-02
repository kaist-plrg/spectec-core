open Domain.Dom
open Tdom
module E = Lang.Eq

(* Equality *)

(* Type parameters *)

let rec eq_tparam tparam_a tparam_b = E.eq_tparam' tparam_a tparam_b
and eq_tparams tparams_a tparams_b = E.eq_list eq_tparam tparams_a tparams_b

(* Parameters *)

and eq_param param_a param_b =
  let id_a, dir_a, typ_a, value_default_a = param_a in
  let id_b, dir_b, typ_b, value_default_b = param_b in
  E.eq_id' id_a id_b && E.eq_dir' dir_a dir_b && eq_typ typ_a typ_b
  && E.eq_option Value.eq value_default_a value_default_b

and eq_params params_a params_b = E.eq_list eq_param params_a params_b

(* Constructor parameters *)

and eq_cparam cparam_a cparam_b = eq_param cparam_a cparam_b
and eq_cparams cparams_a cparams_b = E.eq_list eq_cparam cparams_a cparams_b

(* Types *)

and eq_typ typ_a typ_b =
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
  | VarT id_a, VarT id_b -> E.eq_id' id_a id_b
  | SpecT (typdef_a, typs_a), SpecT (typdef_b, typs_b) ->
      eq_typdef typdef_a typdef_b && eq_typs typs_a typs_b
  | NewT (id_a, typ_a), NewT (id_b, typ_b) ->
      E.eq_id' id_a id_b && eq_typ typ_a typ_b
  | EnumT (id_a, members_a), EnumT (id_b, members_b) ->
      E.eq_id' id_a id_b && E.eq_list E.eq_member' members_a members_b
  | SEnumT (id_a, typ_a, fields_a), SEnumT (id_b, typ_b, fields_b) ->
      E.eq_id' id_a id_b && eq_typ typ_a typ_b
      && E.eq_pairs E.eq_member' Value.eq fields_a fields_b
  | ListT typ_a, ListT typ_b -> eq_typ typ_a typ_b
  | TupleT typs_a, TupleT typs_b -> eq_typs typs_a typs_b
  | StackT (typ_a, size_a), StackT (typ_b, size_b) ->
      eq_typ typ_a typ_b && Bigint.(size_a = size_b)
  | StructT (id_a, fields_a), StructT (id_b, fields_b)
  | HeaderT (id_a, fields_a), HeaderT (id_b, fields_b)
  | UnionT (id_a, fields_a), UnionT (id_b, fields_b) ->
      E.eq_id' id_a id_b && E.eq_pairs E.eq_member' eq_typ fields_a fields_b
  | ExternT (id_a, fdenv_a), ExternT (id_b, fdenv_b) ->
      E.eq_id' id_a id_b && FIdMap.eq eq_funcdef fdenv_a fdenv_b
  | ParserT params_a, ParserT params_b | ControlT params_a, ControlT params_b ->
      eq_params params_a params_b
  | PackageT typs_a, PackageT typs_b -> eq_typs typs_a typs_b
  | AnyT, AnyT -> true
  | TableEnumT (id_a, members_a), TableEnumT (id_b, members_b) ->
      E.eq_id' id_a id_b && E.eq_list E.eq_member' members_a members_b
  | TableStructT (id_a, fields_a), TableStructT (id_b, fields_b) ->
      E.eq_id' id_a id_b && E.eq_pairs E.eq_member' eq_typ fields_a fields_b
  | SeqT typs_a, SeqT typs_b | SeqDefaultT typs_a, SeqDefaultT typs_b ->
      eq_typs typs_a typs_b
  | RecordT fields_a, RecordT fields_b
  | RecordDefaultT fields_a, RecordDefaultT fields_b ->
      E.eq_pairs E.eq_member' eq_typ fields_a fields_b
  | DefaultT, DefaultT | InvalidT, InvalidT -> true
  | SetT typ_a, SetT typ_b -> eq_typ typ_a typ_b
  | StateT, StateT -> true
  | _ -> false

and eq_typs typs_a typs_b = E.eq_list eq_typ typs_a typs_b

(* Type definitions *)

and eq_typdef typdef_a typdef_b =
  match (typdef_a, typdef_b) with
  | DefD typ_a, DefD typ_b -> eq_typ typ_a typ_b
  | NewD (id_a, typ_a), NewD (id_b, typ_b) ->
      E.eq_id' id_a id_b && eq_typ typ_a typ_b
  | EnumD (id_a, members_a), EnumD (id_b, members_b) ->
      E.eq_id' id_a id_b && E.eq_list E.eq_member' members_a members_b
  | SEnumD (id_a, typ_a, fields_a), SEnumD (id_b, typ_b, fields_b) ->
      E.eq_id' id_a id_b && eq_typ typ_a typ_b
      && E.eq_pairs E.eq_member' Value.eq fields_a fields_b
  | ListD (tparam_a, typ_a), ListD (tparam_b, typ_b) ->
      eq_tparam tparam_a tparam_b && eq_typ typ_a typ_b
  | TupleD (tparams_a, typs_a), TupleD (tparams_b, typs_b) ->
      eq_tparams tparams_a tparams_b && eq_typs typs_a typs_b
  | StackD (tparam_a, typ_a, size_a), StackD (tparam_b, typ_b, size_b) ->
      eq_tparam tparam_a tparam_b
      && eq_typ typ_a typ_b
      && Bigint.(size_a = size_b)
  | ( StructD (id_a, tparams_a, tparams_hidden_a, fields_a),
      StructD (id_b, tparams_b, tparams_hidden_b, fields_b) )
  | ( HeaderD (id_a, tparams_a, tparams_hidden_a, fields_a),
      HeaderD (id_b, tparams_b, tparams_hidden_b, fields_b) )
  | ( UnionD (id_a, tparams_a, tparams_hidden_a, fields_a),
      UnionD (id_b, tparams_b, tparams_hidden_b, fields_b) ) ->
      E.eq_id' id_a id_b
      && eq_tparams tparams_a tparams_b
      && eq_tparams tparams_hidden_a tparams_hidden_b
      && E.eq_pairs E.eq_member' eq_typ fields_a fields_b
  | ( ExternD (id_a, tparams_a, tparams_hidden_a, fdenv_a),
      ExternD (id_b, tparams_b, tparams_hidden_b, fdenv_b) ) ->
      E.eq_id' id_a id_b
      && eq_tparams tparams_a tparams_b
      && eq_tparams tparams_hidden_a tparams_hidden_b
      && FIdMap.eq eq_funcdef fdenv_a fdenv_b
  | ( ParserD (tparams_a, tparams_hidden_a, params_a),
      ParserD (tparams_b, tparams_hidden_b, params_b) )
  | ( ControlD (tparams_a, tparams_hidden_a, params_a),
      ControlD (tparams_b, tparams_hidden_b, params_b) ) ->
      eq_tparams tparams_a tparams_b
      && eq_tparams tparams_hidden_a tparams_hidden_b
      && eq_params params_a params_b
  | ( PackageD (tparams_a, tparams_hidden_a, typs_a),
      PackageD (tparams_b, tparams_hidden_b, typs_b) ) ->
      eq_tparams tparams_a tparams_b
      && eq_tparams tparams_hidden_a tparams_hidden_b
      && eq_typs typs_a typs_b
  | _ -> false

(* Function definitions *)

and eq_funcdef fd_a fd_b =
  match (fd_a, fd_b) with
  | ActionD params_a, ActionD params_b -> eq_params params_a params_b
  | ( ExternFunctionD (tparams_a, tparams_hidden_a, params_a, typ_a),
      ExternFunctionD (tparams_b, tparams_hidden_b, params_b, typ_b) )
  | ( FunctionD (tparams_a, tparams_hidden_a, params_a, typ_a),
      FunctionD (tparams_b, tparams_hidden_b, params_b, typ_b) )
  | ( ExternMethodD (tparams_a, tparams_hidden_a, params_a, typ_a),
      ExternMethodD (tparams_b, tparams_hidden_b, params_b, typ_b) )
  | ( ExternAbstractMethodD (tparams_a, tparams_hidden_a, params_a, typ_a),
      ExternAbstractMethodD (tparams_b, tparams_hidden_b, params_b, typ_b) ) ->
      eq_tparams tparams_a tparams_b
      && eq_tparams tparams_hidden_a tparams_hidden_b
      && eq_params params_a params_b
      && eq_typ typ_a typ_b
  | ParserApplyMethodD params_a, ParserApplyMethodD params_b
  | ControlApplyMethodD params_a, ControlApplyMethodD params_b ->
      eq_params params_a params_b
  | _ -> false

(* Alpha-equivalence *)

(* Parameters *)

let rec eq_param_alpha (param_a : param) (param_b : param) : bool =
  let _, dir_a, typ_a, _ = param_a in
  let _, dir_b, typ_b, _ = param_b in
  E.eq_dir' dir_a dir_b && eq_typ_alpha typ_a typ_b

and eq_params_alpha (params_a : param list) (params_b : param list) : bool =
  E.eq_list eq_param_alpha params_a params_b

(* Types *)

and eq_typ_alpha (typ_a : typ) (typ_b : typ) : bool =
  let is_nominal (typ : typ) : bool =
    match typ with
    | EnumT _ | SEnumT _ | StructT _ | HeaderT _ | UnionT _ | ExternT _ -> true
    | _ -> false
  in
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
  | VarT id_a, VarT id_b -> E.eq_id' id_a id_b
  | SpecT ((DefD _ as td), typs_inner_a), _ ->
      let typ_inner_a = Subst.specialize_typdef td typs_inner_a in
      eq_typ_alpha typ_inner_a typ_b
  | _, SpecT ((DefD _ as td), typs_inner_b) ->
      let typ_inner_b = Subst.specialize_typdef td typs_inner_b in
      eq_typ_alpha typ_a typ_inner_b
  | SpecT (td_a, typs_inner_a), SpecT (td_b, typs_inner_b) ->
      let typ_inner_a = Subst.specialize_typdef td_a typs_inner_a in
      let typ_inner_b = Subst.specialize_typdef td_b typs_inner_b in
      eq_typ_alpha typ_inner_a typ_inner_b
      &&
      if is_nominal typ_inner_a && is_nominal typ_inner_b then
        eq_typs_alpha typs_inner_a typs_inner_b
      else true
  | NewT (id_a, typ_inner_a), NewT (id_b, typ_inner_b) ->
      E.eq_id' id_a id_b && eq_typ_alpha typ_inner_a typ_inner_b
  | EnumT (id_a, members_a), EnumT (id_b, members_b) ->
      E.eq_id' id_a id_b && E.eq_list E.eq_member' members_a members_b
  | SEnumT (id_a, typ_inner_a, fields_a), SEnumT (id_b, typ_inner_b, fields_b)
    ->
      E.eq_id' id_a id_b
      && eq_typ_alpha typ_inner_a typ_inner_b
      && E.eq_pairs E.eq_member' Value.eq fields_a fields_b
  | ListT typ_inner_a, ListT typ_inner_b -> eq_typ_alpha typ_inner_a typ_inner_b
  | TupleT typs_inner_a, TupleT typs_inner_b ->
      eq_typs_alpha typs_inner_a typs_inner_b
  | StackT (typ_inner_a, size_a), StackT (typ_inner_b, size_b) ->
      eq_typ_alpha typ_inner_a typ_inner_b && Bigint.(size_a = size_b)
  | StructT (id_a, fields_a), StructT (id_b, fields_b)
  | HeaderT (id_a, fields_a), HeaderT (id_b, fields_b)
  | UnionT (id_a, fields_a), UnionT (id_b, fields_b) ->
      E.eq_id' id_a id_b
      && E.eq_pairs E.eq_member' eq_typ_alpha fields_a fields_b
  | ExternT (id_a, fdenv_a), ExternT (id_b, fdenv_b) ->
      E.eq_id' id_a id_b && FIdMap.eq eq_funcdef_alpha fdenv_a fdenv_b
  | ParserT params_a, ParserT params_b | ControlT params_a, ControlT params_b ->
      eq_params_alpha params_a params_b
  | PackageT typs_inner_a, PackageT typs_inner_b ->
      eq_typs_alpha typs_inner_a typs_inner_b
  | AnyT, AnyT -> true
  | SeqT typs_inner_a, SeqT typs_inner_b ->
      eq_typs_alpha typs_inner_a typs_inner_b
  | RecordT fields_a, RecordT fields_b ->
      E.eq_pairs E.eq_member' eq_typ_alpha fields_a fields_b
  | InvalidT, InvalidT -> true
  | SetT typ_inner_a, SetT typ_inner_b -> eq_typ_alpha typ_inner_a typ_inner_b
  | StateT, StateT -> true
  | _ -> false

and eq_typs_alpha (typs_a : typ list) (typs_b : typ list) : bool =
  E.eq_list eq_typ_alpha typs_a typs_b

(* (1* Type definitions *1) *)

(* and eq_typdef_alpha (td_a : typdef) (td_b : typdef) : bool = *)
(*   let eq_typdef'' tparams_a tparams_hidden_a tparams_b tparams_hidden_b = *)
(*     let tparams_a = tparams_a @ tparams_hidden_a in *)
(*     let tparams_b = tparams_b @ tparams_hidden_b in *)
(*     assert (List.length tparams_a = List.length tparams_b); *)
(*     let frees_a = *)
(*       Free.free_typdef td_a |> TIdSet.union (TIdSet.of_list tparams_a) *)
(*     in *)
(*     let frees_b = *)
(*       Free.free_typdef td_b |> TIdSet.union (TIdSet.of_list tparams_b) *)
(*     in *)
(*     let frees = TIdSet.union frees_a frees_b in *)
(*     let theta_a, theta_b, _, _ = *)
(*       List.fold_left2 *)
(*         (fun (theta_a, theta_b, tparams_fresh, frees) tparam_a tparam_b -> *)
(*           let tparam_fresh, frees = *)
(*             let tparam_fresh = *)
(*               "Fresh" ^ string_of_int (List.length tparams_fresh) *)
(*             in *)
(*             if TIdSet.mem tparam_fresh frees then *)
(*               Subst.fresh_tvar tparam_fresh frees *)
(*             else (tparam_fresh, frees) *)
(*           in *)
(*           let theta_a = TIdMap.add tparam_a (VarT tparam_fresh) theta_a in *)
(*           let theta_b = TIdMap.add tparam_b (VarT tparam_fresh) theta_b in *)
(*           (theta_a, theta_b, tparams_fresh @ [ tparam_fresh ], frees)) *)
(*         (TIdMap.empty, TIdMap.empty, [], frees) *)
(*         tparams_a tparams_b *)
(*     in *)
(*     (theta_a, theta_b) *)
(*   in *)
(*   let eq_typdef' tparams_a tparams_hidden_a tparams_b tparams_hidden_b = *)
(*     if *)
(*       List.length tparams_a = List.length tparams_b *)
(*       && List.length tparams_hidden_a = List.length tparams_hidden_b *)
(*     then *)
(*       Some (eq_typdef'' tparams_a tparams_hidden_a tparams_b tparams_hidden_b) *)
(*     else None *)
(*   in *)
(*   match (td_a, td_b) with *)
(*   | DefD typ_inner_a, DefD typ_inner_b -> eq_typ_alpha typ_inner_a typ_inner_b *)
(*   | NewD (id_a, typ_inner_a), NewD (id_b, typ_inner_b) -> *)
(*       E.eq_id' id_a id_b && eq_typ_alpha typ_inner_a typ_inner_b *)
(*   | EnumD (id_a, members_a), EnumD (id_b, members_b) -> *)
(*       E.eq_id' id_a id_b && E.eq_list E.eq_member' members_a members_b *)
(*   | SEnumD (id_a, typ_inner_a, fields_a), SEnumD (id_b, typ_inner_b, fields_b) *)
(*     -> *)
(*       E.eq_id' id_a id_b *)
(*       && eq_typ_alpha typ_inner_a typ_inner_b *)
(*       && E.eq_pairs E.eq_member' Value.eq fields_a fields_b *)
(*   | ListD (tparam_a, typ_inner_a), ListD (tparam_b, typ_inner_b) -> ( *)
(*       match eq_typdef' [ tparam_a ] [] [ tparam_b ] [] with *)
(*       | Some (theta_a, theta_b) -> *)
(*           let typ_inner_a = Subst.subst_typ theta_a typ_inner_a in *)
(*           let typ_inner_b = Subst.subst_typ theta_b typ_inner_b in *)
(*           eq_typ_alpha typ_inner_a typ_inner_b *)
(*       | None -> false) *)
(*   | TupleD (tparams_a, typs_inner_a), TupleD (tparams_b, typs_inner_b) -> ( *)
(*       match eq_typdef' tparams_a [] tparams_b [] with *)
(*       | Some (theta_a, theta_b) -> *)
(*           let typs_inner_a = Subst.subst_typs theta_a typs_inner_a in *)
(*           let typs_inner_b = Subst.subst_typs theta_b typs_inner_b in *)
(*           eq_typs_alpha typs_inner_a typs_inner_b *)
(*       | None -> false) *)
(*   | ( StackD (tparam_a, typ_inner_a, size_a), *)
(*       StackD (tparam_b, typ_inner_b, size_b) ) -> ( *)
(*       match eq_typdef' [ tparam_a ] [] [ tparam_b ] [] with *)
(*       | Some (theta_a, theta_b) -> *)
(*           let typ_inner_a = Subst.subst_typ theta_a typ_inner_a in *)
(*           let typ_inner_b = Subst.subst_typ theta_b typ_inner_b in *)
(*           eq_typ_alpha typ_inner_a typ_inner_b && Bigint.(size_a = size_b) *)
(*       | None -> false) *)
(*   | ( StructD (id_a, tparams_a, tparams_hidden_a, fields_a), *)
(*       StructD (id_b, tparams_b, tparams_hidden_b, fields_b) ) *)
(*   | ( HeaderD (id_a, tparams_a, tparams_hidden_a, fields_a), *)
(*       HeaderD (id_b, tparams_b, tparams_hidden_b, fields_b) ) *)
(*   | ( UnionD (id_a, tparams_a, tparams_hidden_a, fields_a), *)
(*       UnionD (id_b, tparams_b, tparams_hidden_b, fields_b) ) -> ( *)
(*       match *)
(*         eq_typdef' tparams_a tparams_hidden_a tparams_b tparams_hidden_b *)
(*       with *)
(*       | Some (theta_a, theta_b) -> *)
(*           let fields_a = *)
(*             List.map *)
(*               (fun (member_a, typ_inner_a) -> *)
(*                 let typ_inner_a = Subst.subst_typ theta_a typ_inner_a in *)
(*                 (member_a, typ_inner_a)) *)
(*               fields_a *)
(*           in *)
(*           let fields_b = *)
(*             List.map *)
(*               (fun (member_b, typ_inner_b) -> *)
(*                 let typ_inner_b = Subst.subst_typ theta_b typ_inner_b in *)
(*                 (member_b, typ_inner_b)) *)
(*               fields_b *)
(*           in *)
(*           E.eq_id' id_a id_b *)
(*           && E.eq_pairs E.eq_member' eq_typ_alpha fields_a fields_b *)
(*       | None -> false) *)
(*   | ( ExternD (id_a, tparams_a, tparams_hidden_a, fdenv_a), *)
(*       ExternD (id_b, tparams_b, tparams_hidden_b, fdenv_b) ) -> ( *)
(*       match *)
(*         eq_typdef' tparams_a tparams_hidden_a tparams_b tparams_hidden_b *)
(*       with *)
(*       | Some (theta_a, theta_b) -> *)
(*           let fdenv_a = FIdMap.map (Subst.subst_funcdef theta_a) fdenv_a in *)
(*           let fdenv_b = FIdMap.map (Subst.subst_funcdef theta_b) fdenv_b in *)
(*           E.eq_id' id_a id_b && FIdMap.eq eq_funcdef_alpha fdenv_a fdenv_b *)
(*       | None -> false) *)
(*   | ( ParserD (tparams_a, tparams_hidden_a, params_a), *)
(*       ParserD (tparams_b, tparams_hidden_b, params_b) ) *)
(*   | ( ControlD (tparams_a, tparams_hidden_a, params_a), *)
(*       ControlD (tparams_b, tparams_hidden_b, params_b) ) -> ( *)
(*       match *)
(*         eq_typdef' tparams_a tparams_hidden_a tparams_b tparams_hidden_b *)
(*       with *)
(*       | Some (theta_a, theta_b) -> *)
(*           let params_a = Subst.subst_params theta_a params_a in *)
(*           let params_b = Subst.subst_params theta_b params_b in *)
(*           eq_params_alpha params_a params_b *)
(*       | None -> false) *)
(*   | ( PackageD (tparams_a, tparams_hidden_a, typs_a), *)
(*       PackageD (tparams_b, tparams_hidden_b, typs_b) ) -> ( *)
(*       match *)
(*         eq_typdef' tparams_a tparams_hidden_a tparams_b tparams_hidden_b *)
(*       with *)
(*       | Some (theta_a, theta_b) -> *)
(*           let typs_a = Subst.subst_typs theta_a typs_a in *)
(*           let typs_b = Subst.subst_typs theta_b typs_b in *)
(*           eq_typs_alpha typs_a typs_b *)
(*       | None -> false) *)
(*   | _ -> false *)

(* Function definitions *)

and eq_funcdef_alpha (fd_a : funcdef) (fd_b : funcdef) : bool =
  let eq_funcdef'' tparams_a tparams_hidden_a params_a typ_ret_a tparams_b
      tparams_hidden_b params_b typ_ret_b =
    let tparams_a = tparams_a @ tparams_hidden_a in
    let tparams_b = tparams_b @ tparams_hidden_b in
    assert (List.length tparams_a = List.length tparams_b);
    let frees_a =
      Free.free_funcdef fd_a |> TIdSet.union (TIdSet.of_list tparams_a)
    in
    let frees_b =
      Free.free_funcdef fd_b |> TIdSet.union (TIdSet.of_list tparams_b)
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
          let theta_a = TIdMap.add tparam_a (VarT tparam_fresh) theta_a in
          let theta_b = TIdMap.add tparam_b (VarT tparam_fresh) theta_b in
          (theta_a, theta_b, tparams_fresh @ [ tparam_fresh ], frees))
        (TIdMap.empty, TIdMap.empty, [], frees)
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
  let eq_funcdef' tparams_a tparams_hidden_a params_a typ_ret_a tparams_b
      tparams_hidden_b params_b typ_ret_b =
    List.length tparams_a = List.length tparams_b
    && List.length tparams_hidden_a = List.length tparams_hidden_b
    && eq_funcdef'' tparams_a tparams_hidden_a params_a typ_ret_a tparams_b
         tparams_hidden_b params_b typ_ret_b
  in
  match (fd_a, fd_b) with
  | ActionD params_a, ActionD params_b -> eq_params params_a params_b
  | ( ExternFunctionD (tparams_a, tparams_hidden_a, params_a, typ_ret_a),
      ExternFunctionD (tparams_b, tparams_hidden_b, params_b, typ_ret_b) )
  | ( FunctionD (tparams_a, tparams_hidden_a, params_a, typ_ret_a),
      FunctionD (tparams_b, tparams_hidden_b, params_b, typ_ret_b) )
  | ( ExternMethodD (tparams_a, tparams_hidden_a, params_a, typ_ret_a),
      ExternMethodD (tparams_b, tparams_hidden_b, params_b, typ_ret_b) )
  | ( ExternAbstractMethodD (tparams_a, tparams_hidden_a, params_a, typ_ret_a),
      ExternAbstractMethodD (tparams_b, tparams_hidden_b, params_b, typ_ret_b) )
    ->
      eq_funcdef' tparams_a tparams_hidden_a params_a typ_ret_a tparams_b
        tparams_hidden_b params_b typ_ret_b
  | ParserApplyMethodD params_a, ParserApplyMethodD params_b
  | ControlApplyMethodD params_a, ControlApplyMethodD params_b ->
      eq_params params_a params_b
  | _ -> false
