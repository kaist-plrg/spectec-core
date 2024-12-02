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
  let saturate = saturate_typ

  let rec get_width typ =
    let typ = saturate_typ typ in
    match typ with
    | FIntT width | FBitT width | VBitT width ->
        width |> Bigint.to_int |> Option.get
    | NewT (_, typ_inner) -> get_width typ_inner
    | _ ->
        Format.printf "(get_width) %a must be a numeric type\n" pp typ;
        assert false

  let is_numeric typ =
    let typ = saturate_typ typ in
    match typ with IntT | FIntT _ | FBitT _ -> true | _ -> false

  let rec is_ground typ =
    let typ = saturate_typ typ in
    match typ with
    | VoidT | ErrT | MatchKindT | StrT | BoolT | IntT | FIntT _ | FBitT _
    | VBitT _ ->
        true
    | VarT _ -> false
    | SpecT _ -> assert false
    | NewT (_, typ_inner) -> is_ground typ_inner
    | EnumT _ -> true
    | SEnumT (_, typ_inner, _) -> is_ground typ_inner
    | ListT typ_inner -> is_ground typ_inner
    | TupleT typs_inner -> List.for_all is_ground typs_inner
    | StackT (typ_inner, _) -> is_ground typ_inner
    | StructT (_, fields) | HeaderT (_, fields) | UnionT (_, fields) ->
        List.map snd fields |> List.for_all is_ground
    | ExternT _ | ParserT _ | ControlT _ | PackageT _ | AnyT -> true
    | TableEnumT _ | TableStructT _ -> true
    | SeqT typs_inner | SeqDefaultT typs_inner ->
        List.for_all is_ground typs_inner
    | RecordT fields | RecordDefaultT fields ->
        List.map snd fields |> List.for_all is_ground
    | DefaultT | InvalidT -> true
    | SetT typ_inner -> is_ground typ_inner
    | StateT -> true
    | TableT _ -> true

  let rec is_defaultable typ =
    let typ = saturate_typ typ in
    match typ with
    | VoidT -> false
    | ErrT -> true
    | MatchKindT -> false
    | StrT | BoolT | IntT | FIntT _ | FBitT _ | VBitT _ -> true
    | VarT _ -> false
    | SpecT _ -> assert false
    | NewT (_, typ_inner) -> is_defaultable typ_inner
    | EnumT _ -> true
    | SEnumT (_, typ_inner, _) -> is_defaultable typ_inner
    | ListT _ -> false
    | TupleT typs_inner -> List.for_all is_defaultable typs_inner
    | StackT (typ_inner, _) -> is_defaultable typ_inner
    | StructT (_, fields) | HeaderT (_, fields) | UnionT (_, fields) ->
        List.map snd fields |> List.for_all is_defaultable
    | ExternT _ | ParserT _ | ControlT _ | PackageT _ | AnyT | TableEnumT _
    | TableStructT _ | SeqT _ | SeqDefaultT _ | RecordT _ | RecordDefaultT _
    | DefaultT | InvalidT | SetT _ | StateT | TableT _ ->
        false

  let rec can_equals typ =
    let typ = saturate_typ typ in
    match typ with
    | VoidT -> false
    | ErrT | MatchKindT | StrT | BoolT | IntT | FIntT _ | FBitT _ | VBitT _ ->
        true
    | VarT _ -> false
    | SpecT _ -> assert false
    | NewT (_, typ_inner) -> can_equals typ_inner
    | EnumT _ -> true
    | SEnumT (_, typ_inner, _) | ListT typ_inner -> can_equals typ_inner
    | TupleT typs_inner -> List.for_all can_equals typs_inner
    | StackT (typ_inner, _) -> can_equals typ_inner
    | StructT (_, fields) | HeaderT (_, fields) | UnionT (_, fields) ->
        List.map snd fields |> List.for_all can_equals
    | ExternT _ | ParserT _ | ControlT _ | PackageT _ | AnyT | TableEnumT _
    | TableStructT _ ->
        false
    | SeqT typs_inner -> List.for_all can_equals typs_inner
    | SeqDefaultT _ -> false
    | RecordT fields -> List.map snd fields |> List.for_all can_equals
    | RecordDefaultT _ -> false
    | DefaultT -> false
    | InvalidT -> true
    | SetT _ | StateT | TableT _ -> false
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
    | ActionD _, ActionD _
    | ExternFunctionD _, ExternFunctionD _
    | FunctionD _, FunctionD _
    | ExternMethodD _, ExternMethodD _
    | ExternMethodD _, ExternAbstractMethodD _
    | ExternAbstractMethodD _, ExternMethodD _
    | ExternAbstractMethodD _, ExternAbstractMethodD _
    | ParserApplyMethodD _, ParserApplyMethodD _
    | ControlApplyMethodD _, ControlApplyMethodD _ ->
        true
    | _ -> false

  let free = free_funcdef
  let subst = subst_funcdef
  let specialize = specialize_funcdef

  let get_tparams = function
    | ActionD _ -> ([], [])
    | ExternFunctionD (tparams, tparams_hidden, _, _)
    | FunctionD (tparams, tparams_hidden, _, _)
    | ExternMethodD (tparams, tparams_hidden, _, _)
    | ExternAbstractMethodD (tparams, tparams_hidden, _, _) ->
        (tparams, tparams_hidden)
    | ParserApplyMethodD _ | ControlApplyMethodD _ -> ([], [])

  let get_params = function
    | ActionD params
    | ExternFunctionD (_, _, params, _)
    | FunctionD (_, _, params, _)
    | ExternMethodD (_, _, params, _)
    | ExternAbstractMethodD (_, _, params, _)
    | ParserApplyMethodD params
    | ControlApplyMethodD params ->
        params

  let get_typ_ret = function
    | ActionD _ -> VoidT
    | ExternFunctionD (_, _, _, typ_ret)
    | FunctionD (_, _, _, typ_ret)
    | ExternMethodD (_, _, _, typ_ret)
    | ExternAbstractMethodD (_, _, _, typ_ret) ->
        typ_ret
    | ParserApplyMethodD _ | ControlApplyMethodD _ -> VoidT
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
