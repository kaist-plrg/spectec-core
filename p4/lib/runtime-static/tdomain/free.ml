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
      free_typdef_poly td |> TIdSet.union (free_typs typs_inner)
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
  | MonoD tdm -> free_typdef_mono tdm
  | PolyD tdp -> free_typdef_poly tdp

and free_typdef_mono (tdm : typdef_mono) : TIdSet.t = free_typ tdm

and free_typdef_poly (tdp : typdef_poly) : TIdSet.t =
  let tparams, tparams_hidden, typ_inner = tdp in
  free_typ typ_inner |> TIdSet.diff (TIdSet.of_list (tparams @ tparams_hidden))

(* Function types *)

and free_functyp (ft : functyp) : TIdSet.t =
  match ft with
  | ActionT params -> free_params params
  | ExternFunctionT (params, typ_ret)
  | FunctionT (params, typ_ret)
  | ExternMethodT (params, typ_ret)
  | ExternAbstractMethodT (params, typ_ret) ->
      free_params params |> TIdSet.union (free_typ typ_ret)
  | ParserApplyMethodT params | ControlApplyMethodT params -> free_params params
  | BuiltinMethodT (params, typ_ret) ->
      free_params params |> TIdSet.union (free_typ typ_ret)
  | TableApplyMethodT typ_ret -> free_typ typ_ret

(* Function definitions *)

and free_funcdef (fd : funcdef) : TIdSet.t =
  match fd with
  | MonoFD ft -> free_functyp ft
  | PolyFD (tparams, tparams_hidden, ft) ->
      free_functyp ft |> TIdSet.diff (TIdSet.of_list (tparams @ tparams_hidden))
