open Domain.Dom
open Tdom

(* Collecting free type variables *)

(* Parameters *)

let rec free_param (param : param) : TIdSet.t =
  let _, _, typ, _ = param in
  free_typ typ

and free_params (params : param list) : TIdSet.t =
  List.map free_param params |> List.fold_left TIdSet.union TIdSet.empty

(* Types *)

and free_typ (typ : typ) : TIdSet.t =
  match typ with
  | VoidT | ErrT | MatchKindT | StrT | BoolT | IntT | FIntT _ | FBitT _
  | VBitT _ ->
      TIdSet.empty
  | VarT id -> TIdSet.singleton id
  | SpecT (td, typs_inner) ->
      free_typdef td |> TIdSet.union (free_typs typs_inner)
  | DefT typ_inner | NewT (_, typ_inner) -> free_typ typ_inner
  | EnumT _ -> TIdSet.empty
  | SEnumT (_, typ_inner, _) -> free_typ typ_inner
  | ListT typ_inner -> free_typ typ_inner
  | TupleT typs_inner -> free_typs typs_inner
  | StackT (typ_inner, _) -> free_typ typ_inner
  | StructT (_, fields) | HeaderT (_, fields) | UnionT (_, fields) ->
      List.map snd fields |> free_typs
  | ExternT (_, fdenv) ->
      FIdMap.bindings fdenv |> List.map snd |> List.map free_funcdef
      |> List.fold_left TIdSet.union TIdSet.empty
  | ParserT params | ControlT params -> free_params params
  | PackageT typs_inner -> free_typs typs_inner
  | TableT typ_inner -> free_typ typ_inner
  | AnyT | TableEnumT _ -> TIdSet.empty
  | TableStructT (_, fields) -> List.map snd fields |> free_typs
  | SeqT typs_inner | SeqDefaultT typs_inner -> free_typs typs_inner
  | RecordT fields | RecordDefaultT fields -> List.map snd fields |> free_typs
  | DefaultT | InvalidT -> TIdSet.empty
  | SetT typ_inner -> free_typ typ_inner
  | StateT -> TIdSet.empty

and free_typs (typs : typ list) : TIdSet.t =
  List.map free_typ typs |> List.fold_left TIdSet.union TIdSet.empty

(* Type definitions *)

and free_typdef (td : typdef) : TIdSet.t =
  match td with
  | DefD typ_inner | NewD (_, typ_inner) -> free_typ typ_inner
  | EnumD _ -> TIdSet.empty
  | SEnumD (_, typ_inner, _) -> free_typ typ_inner
  | ListD (tparam, typ_inner) ->
      free_typ typ_inner |> TIdSet.diff (TIdSet.singleton tparam)
  | TupleD (tparams, typs_inner) ->
      free_typs typs_inner |> TIdSet.diff (TIdSet.of_list tparams)
  | StackD (tparam, typ_inner, _) ->
      free_typ typ_inner |> TIdSet.diff (TIdSet.singleton tparam)
  | StructD (_, tparams, tparams_hidden, fields)
  | HeaderD (_, tparams, tparams_hidden, fields)
  | UnionD (_, tparams, tparams_hidden, fields) ->
      List.map snd fields |> free_typs
      |> TIdSet.diff (TIdSet.of_list (tparams @ tparams_hidden))
  | ExternD (_, tparams, tparams_hidden, fdenv) ->
      FIdMap.bindings fdenv |> List.map snd |> List.map free_funcdef
      |> List.fold_left TIdSet.union TIdSet.empty
      |> TIdSet.diff (TIdSet.of_list (tparams @ tparams_hidden))
  | ParserD (tparams, tparams_hidden, params)
  | ControlD (tparams, tparams_hidden, params) ->
      free_params params
      |> TIdSet.diff (TIdSet.of_list (tparams @ tparams_hidden))
  | PackageD (tparams, tparams_hidden, typs_inner) ->
      free_typs typs_inner
      |> TIdSet.diff (TIdSet.of_list (tparams @ tparams_hidden))

(* Function definitions *)

and free_funcdef (fd : funcdef) : TIdSet.t =
  match fd with
  | ActionD params -> free_params params
  | ExternFunctionD (tparams, tparams_hidden, params, typ)
  | FunctionD (tparams, tparams_hidden, params, typ)
  | ExternMethodD (tparams, tparams_hidden, params, typ)
  | ExternAbstractMethodD (tparams, tparams_hidden, params, typ) ->
      free_params params
      |> TIdSet.union (free_typ typ)
      |> TIdSet.diff (TIdSet.of_list (tparams @ tparams_hidden))
  | ParserApplyMethodD params | ControlApplyMethodD params -> free_params params
