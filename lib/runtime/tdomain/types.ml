include Tdom
open Pp
open Eq
open Free
open Subst

(* Modules *)

module Type = struct
  type t = typ

  let pp = pp_typ
  let eq = eq_typ
  let eq_alpha = eq_typ_alpha
  let free = free_typ
  let subst = subst_typ
  let unroll = unroll_typ
  let canon = canon_typ

  let rec get_width typ =
    let typ = canon_typ typ in
    match typ with
    | SpecT _ | DefT _ -> assert false
    | FIntT width | FBitT width | VBitT width ->
        width |> Bigint.to_int |> Option.get
    | NewT (_, typ_inner) -> get_width typ_inner
    | _ ->
        Format.printf "(get_width) %a must be a numeric type\n" pp typ;
        assert false

  let is_numeric typ =
    let typ = canon_typ typ in
    match typ with
    | SpecT _ | DefT _ -> assert false
    | IntT | FIntT _ | FBitT _ -> true
    | _ -> false

  let rec is_ground typ =
    let typ = canon_typ typ in
    match typ with
    | SpecT _ | DefT _ -> assert false
    | VoidT | ErrT | MatchKindT | StrT | BoolT | IntT | FIntT _ | FBitT _
    | VBitT _ ->
        true
    | VarT _ -> false
    | NewT (_, typ_inner) -> is_ground typ_inner
    | EnumT _ -> true
    | SEnumT (_, typ_inner, _) -> is_ground typ_inner
    | ListT typ_inner -> is_ground typ_inner
    | TupleT typs_inner -> List.for_all is_ground typs_inner
    | StackT (typ_inner, _) -> is_ground typ_inner
    | StructT (_, fields) | HeaderT (_, fields) | UnionT (_, fields) ->
        List.map snd fields |> List.for_all is_ground
    | ExternT _ | ParserT _ | ControlT _ | PackageT _ | TableT _ | AnyT -> true
    | TableEnumT _ | TableStructT _ -> true
    | SeqT typs_inner | SeqDefaultT typs_inner ->
        List.for_all is_ground typs_inner
    | RecordT fields | RecordDefaultT fields ->
        List.map snd fields |> List.for_all is_ground
    | DefaultT | InvalidT -> true
    | SetT typ_inner -> is_ground typ_inner
    | StateT -> true

  let rec is_assignable typ =
    let typ = canon_typ typ in
    match typ with
    | SpecT _ | DefT _ -> assert false
    | VoidT -> false
    | ErrT | MatchKindT | StrT | BoolT | IntT | FIntT _ | FBitT _ | VBitT _
    | VarT _ ->
        true
    | NewT (_, typ_inner) -> is_assignable typ_inner
    | EnumT _ | SEnumT _ | ListT _ | TupleT _ | StackT _ | StructT _ | HeaderT _
    | UnionT _ ->
        true
    | ExternT _ | ParserT _ | ControlT _ | PackageT _ | TableT _ | AnyT
    | TableEnumT _ | TableStructT _ | SeqT _ | SeqDefaultT _ | RecordT _
    | RecordDefaultT _ | DefaultT | InvalidT | SetT _ | StateT ->
        false

  let rec is_defaultable typ =
    let typ = canon_typ typ in
    match typ with
    | SpecT _ | DefT _ -> assert false
    | VoidT -> false
    | ErrT -> true
    | MatchKindT -> false
    | StrT | BoolT | IntT | FIntT _ | FBitT _ | VBitT _ -> true
    | VarT _ -> false
    | NewT (_, typ_inner) -> is_defaultable typ_inner
    | EnumT _ -> true
    | SEnumT (_, typ_inner, _) -> is_defaultable typ_inner
    | ListT _ -> false
    | TupleT typs_inner -> List.for_all is_defaultable typs_inner
    | StackT (typ_inner, _) -> is_defaultable typ_inner
    | StructT (_, fields) | HeaderT (_, fields) | UnionT (_, fields) ->
        List.map snd fields |> List.for_all is_defaultable
    | ExternT _ | ParserT _ | ControlT _ | PackageT _ | TableT _ | AnyT
    | TableEnumT _ | TableStructT _ | SeqT _ | SeqDefaultT _ | RecordT _
    | RecordDefaultT _ | DefaultT | InvalidT | SetT _ | StateT ->
        false

  let rec can_equals typ =
    let typ = canon_typ typ in
    match typ with
    | SpecT _ | DefT _ -> assert false
    | VoidT -> false
    | ErrT | MatchKindT | StrT | BoolT | IntT | FIntT _ | FBitT _ | VBitT _ ->
        true
    | VarT _ -> false
    | NewT (_, typ_inner) -> can_equals typ_inner
    | EnumT _ -> true
    | SEnumT (_, typ_inner, _) | ListT typ_inner -> can_equals typ_inner
    | TupleT typs_inner -> List.for_all can_equals typs_inner
    | StackT (typ_inner, _) -> can_equals typ_inner
    | StructT (_, fields) | HeaderT (_, fields) | UnionT (_, fields) ->
        List.map snd fields |> List.for_all can_equals
    | ExternT _ | ParserT _ | ControlT _ | PackageT _ | TableT _ | AnyT
    | TableEnumT _ | TableStructT _ ->
        false
    | SeqT typs_inner -> List.for_all can_equals typs_inner
    | SeqDefaultT _ -> false
    | RecordT fields -> List.map snd fields |> List.for_all can_equals
    | RecordDefaultT _ -> false
    | DefaultT -> false
    | InvalidT -> true
    | SetT _ | StateT -> false
end

module TypeDef = struct
  type t = typdef

  let pp = pp_typdef
  let eq = eq_typdef
  let free = free_typdef
  let subst = subst_typdef
  let specialize = specialize_typdef
end

module FuncType = struct
  type t = functyp

  let pp = pp_functyp
  let eq = eq_functyp
  let eq_alpha = eq_functyp_alpha

  let eq_kind ft_a ft_b =
    match (ft_a, ft_b) with
    | ActionT _, ActionT _ -> true
    | ExternFunctionT _, ExternFunctionT _
    | FunctionT _, FunctionT _
    | ExternMethodT _, ExternMethodT _
    | ExternMethodT _, ExternAbstractMethodT _
    | ExternAbstractMethodT _, ExternMethodT _
    | ExternAbstractMethodT _, ExternAbstractMethodT _
    | ParserApplyMethodT _, ParserApplyMethodT _
    | ControlApplyMethodT _, ControlApplyMethodT _
    | BuiltinMethodT _, BuiltinMethodT _
    | TableApplyMethodT _, TableApplyMethodT _ ->
        true
    | _ -> false

  let subst = subst_functyp
  let is_action = function ActionT _ -> true | _ -> false

  let get_params = function
    | ActionT params
    | ExternFunctionT (params, _)
    | FunctionT (params, _)
    | ExternMethodT (params, _)
    | ExternAbstractMethodT (params, _)
    | ParserApplyMethodT params
    | ControlApplyMethodT params
    | BuiltinMethodT (params, _) ->
        params
    | TableApplyMethodT _ -> []

  let get_typ_ret = function
    | ActionT _ -> VoidT
    | ExternFunctionT (_, typ_ret)
    | FunctionT (_, typ_ret)
    | ExternMethodT (_, typ_ret)
    | ExternAbstractMethodT (_, typ_ret) ->
        typ_ret
    | ParserApplyMethodT _ | ControlApplyMethodT _ -> VoidT
    | TableApplyMethodT typ_ret -> typ_ret
    | BuiltinMethodT (_, typ_ret) -> typ_ret
end

module FuncDef = struct
  type t = funcdef

  let pp = pp_funcdef
  let eq = eq_funcdef
  let eq_alpha = eq_funcdef_alpha

  let eq_kind fd_a fd_b =
    match (fd_a, fd_b) with
    | MonoFD ft_a, MonoFD ft_b | PolyFD (_, _, ft_a), PolyFD (_, _, ft_b) ->
        FuncType.eq_kind ft_a ft_b
    | _ -> false

  let free = free_funcdef
  let subst = subst_funcdef
  let specialize = specialize_funcdef

  let get_tparams = function
    | MonoFD _ -> ([], [])
    | PolyFD (tparams, tparams_hidden, _) -> (tparams, tparams_hidden)

  let get_params = function
    | MonoFD ft | PolyFD (_, _, ft) -> FuncType.get_params ft

  let get_typ_ret = function
    | MonoFD ft | PolyFD (_, _, ft) -> FuncType.get_typ_ret ft
end

module ConsType = struct
  type t = constyp

  let pp = pp_constyp
  let subst = subst_constyp
end

module ConsDef = struct
  type t = consdef

  let pp = pp_consdef

  let eq_kind cd_a cd_b =
    let _, _, _, typ_ret_a = cd_a in
    let _, _, _, typ_ret_b = cd_b in
    eq_typ typ_ret_a typ_ret_b

  let specialize = specialize_consdef
end
