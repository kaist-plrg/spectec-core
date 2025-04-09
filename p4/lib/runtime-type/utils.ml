open Il.Ast

(* Type utils *)

let is_numeric_typ (typ : typ') : bool =
  let typ = Subst.canon_typ typ in
  match typ with
  | SpecT _ | DefT _ -> assert false
  | IntT | FIntT _ | FBitT _ -> true
  | _ -> false

let is_nominal_typ (typ : typ') : bool =
  let typ = Subst.canon_typ typ in
  match typ with
  | SpecT _ | DefT _ -> assert false
  | NewT _ | EnumT _ | SEnumT _ | StructT _ | HeaderT _ | UnionT _ | ExternT _
  | TableT _ ->
      true
  | _ -> false

let rec is_ground_typ typ =
  let typ = Subst.canon_typ typ in
  match typ with
  | SpecT _ | DefT _ -> assert false
  | VoidT | ErrT | MatchKindT | StrT | BoolT | IntT | FIntT _ | FBitT _
  | VBitT _ ->
      true
  | VarT _ -> false
  | NewT (_, typ_inner) -> is_ground_typ typ_inner
  | EnumT _ -> true
  | SEnumT (_, typ_inner, _) -> is_ground_typ typ_inner
  | ListT typ_inner -> is_ground_typ typ_inner
  | TupleT typs_inner -> List.for_all is_ground_typ typs_inner
  | StackT (typ_inner, _) -> is_ground_typ typ_inner
  | StructT (_, fields) | HeaderT (_, fields) | UnionT (_, fields) ->
      List.map snd fields |> List.for_all is_ground_typ
  | ExternT _ | ParserT _ | ControlT _ | PackageT _ | TableT _ | AnyT -> true
  | TableEnumT _ | TableStructT _ -> true
  | SeqT typs_inner | SeqDefaultT typs_inner ->
      List.for_all is_ground_typ typs_inner
  | RecordT fields | RecordDefaultT fields ->
      List.map snd fields |> List.for_all is_ground_typ
  | DefaultT | InvalidT -> true
  | SetT typ_inner -> is_ground_typ typ_inner
  | StateT -> true

let is_assignable_typ typ =
  let typ = Subst.canon_typ typ in
  match typ with
  | SpecT _ | DefT _ -> assert false
  | VoidT -> false
  | ErrT | MatchKindT -> true
  | StrT -> false
  | BoolT -> true
  | IntT -> false
  | FIntT _ | FBitT _ | VBitT _ | VarT _ | NewT _ -> true
  | EnumT _ | SEnumT _ | ListT _ | TupleT _ | StackT _ | StructT _ | HeaderT _
  | UnionT _ ->
      true
  | ExternT _ | ParserT _ | ControlT _ | PackageT _ | TableT _ | AnyT
  | TableEnumT _ | TableStructT _ | SeqT _ | SeqDefaultT _ | RecordT _
  | RecordDefaultT _ | DefaultT | InvalidT | SetT _ | StateT ->
      false

let rec is_defaultable_typ typ =
  let typ = Subst.canon_typ typ in
  match typ with
  | SpecT _ | DefT _ -> assert false
  | VoidT -> false
  | ErrT -> true
  | MatchKindT -> false
  | StrT | BoolT | IntT | FIntT _ | FBitT _ | VBitT _ -> true
  | VarT _ -> false
  | NewT (_, typ_inner) -> is_defaultable_typ typ_inner
  | EnumT _ -> true
  | SEnumT (_, typ_inner, _) -> is_defaultable_typ typ_inner
  | ListT _ -> false
  | TupleT typs_inner -> List.for_all is_defaultable_typ typs_inner
  | StackT (typ_inner, _) -> is_defaultable_typ typ_inner
  | StructT (_, fields) | HeaderT (_, fields) | UnionT (_, fields) ->
      List.map snd fields |> List.for_all is_defaultable_typ
  | ExternT _ | ParserT _ | ControlT _ | PackageT _ | TableT _ | AnyT
  | TableEnumT _ | TableStructT _ | SeqT _ | SeqDefaultT _ | RecordT _
  | RecordDefaultT _ | DefaultT | InvalidT | SetT _ | StateT ->
      false

let is_equalable_typ typ =
  let typ = Subst.canon_typ typ in
  match typ with
  | SpecT _ | DefT _ -> assert false
  | VoidT -> false
  | ErrT | MatchKindT | StrT | BoolT | IntT | FIntT _ | FBitT _ | VBitT _ ->
      true
  | VarT _ -> false
  | NewT _ | EnumT _ | SEnumT _ | ListT _ | TupleT _ | StackT _ | StructT _
  | HeaderT _ | UnionT _ ->
      true
  | ExternT _ | ParserT _ | ControlT _ | PackageT _ | TableT _ | AnyT
  | TableEnumT _ | TableStructT _ ->
      false
  | SeqT _ -> true
  | SeqDefaultT _ -> false
  | RecordT _ -> true
  | RecordDefaultT _ -> false
  | DefaultT -> false
  | InvalidT -> true
  | SetT _ | StateT -> false

let rec get_width_typ typ =
  let typ = Subst.canon_typ typ in
  match typ with
  | SpecT _ | DefT _ -> assert false
  | FIntT width | FBitT width | VBitT width -> width |> Bigint.to_int_exn
  | NewT (_, typ_inner) -> get_width_typ typ_inner
  | _ -> assert false

(* Function type utils *)

let is_action_functyp = function ActionT _ -> true | _ -> false

let get_params_functyp = function
  | ActionT params
  | ExternFunctionT (params, _)
  | FunctionT (params, _)
  | BuiltinMethodT (params, _)
  | ExternMethodT (params, _)
  | ExternAbstractMethodT (params, _)
  | ParserApplyMethodT params
  | ControlApplyMethodT params ->
      params
  | TableApplyMethodT _ -> []

let get_typ_ret_functyp = function
  | ActionT _ -> VoidT
  | ExternFunctionT (_, typ_ret)
  | FunctionT (_, typ_ret)
  | BuiltinMethodT (_, typ_ret)
  | ExternMethodT (_, typ_ret)
  | ExternAbstractMethodT (_, typ_ret) ->
      typ_ret
  | ParserApplyMethodT _ | ControlApplyMethodT _ -> VoidT
  | TableApplyMethodT typ_ret -> typ_ret

(* Function definition utils *)

let get_tparams_funcdef = function
  | MonoFD _ -> ([], [])
  | PolyFD (tparams, tparams_hidden, _) -> (tparams, tparams_hidden)

let get_params_funcdef = function
  | MonoFD ft | PolyFD (_, _, ft) -> get_params_functyp ft

let get_typ_ret_funcdef = function
  | MonoFD ft | PolyFD (_, _, ft) -> get_typ_ret_functyp ft
