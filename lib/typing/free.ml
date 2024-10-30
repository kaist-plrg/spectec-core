open Runtime.Domain
module Types = Runtime.Types
module Type = Types.Type
module Envs = Runtime.Envs

(* Collecting free type variables *)

let rec free_typ (typ : Type.t) : TIdSet.t =
  match typ with
  | VoidT | ErrT | MatchKindT | StrT | BoolT | IntT | FIntT _ | FBitT _
  | VBitT _ ->
      TIdSet.empty
  | VarT id -> TIdSet.singleton id
  | NewT (_, typ_inner) -> free_typ typ_inner
  | EnumT _ -> TIdSet.empty
  | SEnumT (_, typ_inner) | ListT typ_inner -> free_typ typ_inner
  | TupleT typs_inner ->
      List.map free_typ typs_inner |> List.fold_left TIdSet.union TIdSet.empty
  | StackT (typ_inner, _) -> free_typ typ_inner
  | StructT (_, fields) | HeaderT (_, fields) | UnionT (_, fields) ->
      List.map snd fields |> List.map free_typ
      |> List.fold_left TIdSet.union TIdSet.empty
  | ExternT (_, fdenv) ->
      Envs.FDEnv.bindings fdenv |> List.map snd |> List.map free_fd
      |> List.fold_left TIdSet.union TIdSet.empty
  | ParserT params | ControlT params ->
      List.map free_param params |> List.fold_left TIdSet.union TIdSet.empty
  | PackageT | TopT -> TIdSet.empty
  | SeqT typs_inner ->
      List.map free_typ typs_inner |> List.fold_left TIdSet.union TIdSet.empty
  | RecordT fields ->
      List.map snd fields |> List.map free_typ
      |> List.fold_left TIdSet.union TIdSet.empty
  | SetT typ_inner -> free_typ typ_inner
  | StateT -> TIdSet.empty
  | TableT typ_inner -> free_typ typ_inner

and free_param (param : Types.param) : TIdSet.t =
  let _, _, typ, _ = param in
  free_typ typ

and free_fd (fd : Types.funcdef) : TIdSet.t =
  match fd with
  | ExternFunctionD (tparams, params, typ_ret)
  | FunctionD (tparams, params, typ_ret) ->
      let bounds = TIdSet.of_list tparams in
      let frees =
        List.map free_param params @ [ free_typ typ_ret ]
        |> List.fold_left TIdSet.union TIdSet.empty
      in
      TIdSet.diff frees bounds
  | ActionD params ->
      List.map free_param params |> List.fold_left TIdSet.union TIdSet.empty
  | ExternMethodD (tparams, params, typ_ret)
  | ExternAbstractMethodD (tparams, params, typ_ret) ->
      let bounds = TIdSet.of_list tparams in
      let frees =
        List.map free_param params @ [ free_typ typ_ret ]
        |> List.fold_left TIdSet.union TIdSet.empty
      in
      TIdSet.diff frees bounds
