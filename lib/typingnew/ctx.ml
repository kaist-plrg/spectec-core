open Runtime.Domain
open Types

(* Environments constitute a context *)

module CEnv = MakeEnv (FId) (ConsType)
module TDEnv = MakeEnv (TId) (TypeDef)
module FEnv = MakeEnv (FId) (FuncType)
module VEnv = MakeEnv (Id) (Runtime.Value)
module TEnv = MakeEnv (Id) (BaseType)

type envs = TDEnv.t * FEnv.t * VEnv.t * TEnv.t
type t = { cons : CEnv.t; glob : envs; obj : envs; loc : envs list }

let empty_envs = (TDEnv.empty, FEnv.empty, VEnv.empty, TEnv.empty)
let empty = { cons = CEnv.empty; glob = empty_envs; obj = empty_envs; loc = [] }

(* Adders *)

let add_td_glob name td ctx =
  let gtdenv, gfenv, gvenv, gtenv = ctx.glob in
  let gtdenv = TDEnv.add name td gtdenv in
  { ctx with glob = (gtdenv, gfenv, gvenv, gtenv) }

let add_value_glob name value ctx =
  let gtdenv, gfenv, gvenv, gtenv = ctx.glob in
  let gvenv = VEnv.add name value gvenv in
  { ctx with glob = (gtdenv, gfenv, gvenv, gtenv) }

let add_type_glob name typ ctx =
  let gtdenv, gfenv, gvenv, gtenv = ctx.glob in
  let gtenv = TEnv.add name typ gtenv in
  { ctx with glob = (gtdenv, gfenv, gvenv, gtenv) }

(* Finders *)

let find finder name ctx = function
  | Some value -> Some value
  | None -> finder name ctx

let find_td_glob_opt tid ctx =
  let gtdenv, _, _, _ = ctx.glob in
  TDEnv.find_opt tid gtdenv

let find_td_glob tid ctx = find_td_glob_opt tid ctx |> Option.get

let find_td_obj_opt tid ctx =
  let otdenv, _, _, _ = ctx.obj in
  TDEnv.find_opt tid otdenv

let find_td_obj tid ctx = find_td_obj_opt tid ctx |> Option.get

let find_td_loc_opt tid ctx =
  let loc = ctx.loc in
  let tdenvs = List.map (fun (tdenv, _, _, _) -> tdenv) loc in
  List.fold_left
    (fun value tdenv ->
      match value with Some _ -> value | None -> TDEnv.find_opt tid tdenv)
    None tdenvs

let find_td_loc tid ctx = find_td_loc_opt tid ctx |> Option.get

let find_td_opt tid ctx =
  find_td_loc_opt tid ctx
  |> find find_td_obj_opt tid ctx
  |> find find_td_glob_opt tid ctx

let find_td tid ctx = find_td_opt tid ctx |> Option.get

let find_value_glob_opt id ctx =
  let _, _, gvenv, _ = ctx.glob in
  VEnv.find_opt id gvenv

let find_const_glob id ctx = find_value_glob_opt id ctx |> Option.get

let find_value_obj_opt id ctx =
  let _, _, ovenv, _ = ctx.obj in
  VEnv.find_opt id ovenv

let find_value_obj id ctx = find_value_obj_opt id ctx |> Option.get

let find_value_loc_opt id ctx =
  let loc = ctx.loc in
  let venvs = List.map (fun (_, _, venv, _) -> venv) loc in
  List.fold_left
    (fun value venv ->
      match value with Some _ -> value | None -> VEnv.find_opt id venv)
    None venvs

let find_value_loc id ctx = find_value_loc_opt id ctx |> Option.get

let find_value_opt id ctx =
  find_value_loc_opt id ctx
  |> find find_value_obj_opt id ctx
  |> find find_value_glob_opt id ctx

let find_value id ctx = find_value_opt id ctx |> Option.get

(* Pretty-printer *)

let pp_envs fmt envs =
  let tdenv, fenv, venv, tenv = envs in
  Format.fprintf fmt
    "@[\n\
    \    @[<v 0>Typedefs:@ %a@]@\n\n\
    \    @[<v 0>Functions:@ %a@]@\n\n\
    \    @[<v 0>Constants:@ %a@]@\n\n\
    \    @[<v 0>Types:@ %a@]@\n\
     ]"
    TDEnv.pp tdenv FEnv.pp fenv VEnv.pp venv TEnv.pp tenv

let pp fmt ctx =
  Format.fprintf fmt
    "@[<v 0>Constructors:@ %a@]@\n\
     @[<v 0>Global:@ %a@]@\n\
     @[<v 0>Object:@ %a@]@\n\
     @[<v 0>Local:@ %a@\n\
     ]"
    CEnv.pp ctx.cons pp_envs ctx.glob pp_envs ctx.obj
    (Format.pp_print_list pp_envs)
    ctx.loc
