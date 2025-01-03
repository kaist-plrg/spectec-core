module L = Lang.Ast
module Envs = Runtime_dynamic.Envs
module F = Format
open Util.Pp
open Util.Source

(* Global counter for unique identifiers *)

let tick = ref 0
let refresh () = tick := 0

let fresh () =
  let id = !tick in
  tick := !tick + 1;
  id

(* Context is consisted of layers of environments *)

type cursor = Global | Block | Local

(* Defining each layer *)

type gt = { cenv : Envs.CEnv.t; fenv : Envs.FEnv.t; venv : Envs.VEnv.t }
type bt = { fenv : Envs.FEnv.t; venv : Envs.VEnv.t }
type lt = { venvs : Envs.VEnv.t list }
type t = { path : Domain.Dom.OId.t; global : gt; block : bt; local : lt }

let empty_gt =
  { cenv = Envs.CEnv.empty; fenv = Envs.FEnv.empty; venv = Envs.VEnv.empty }

let empty_bt = { fenv = Envs.FEnv.empty; venv = Envs.VEnv.empty }
let empty_lt = { venvs = [] }
let empty = { path = []; global = empty_gt; block = empty_bt; local = empty_lt }

(* Path management *)

let enter_path id ctx = { ctx with path = ctx.path @ [ id ] }

let exit_path ctx =
  match ctx.path with
  | [] -> assert false
  | path ->
      let path = List.rev path |> List.tl |> List.rev in
      { ctx with path }

(* Frame management *)

let enter_frame ctx =
  { ctx with local = { venvs = Envs.VEnv.empty :: ctx.local.venvs } }

let exit_frame ctx =
  match ctx.local.venvs with
  | [] -> assert false
  | _ :: venvs -> { ctx with local = { venvs } }

(* Adders *)

let add_cons cursor cid cons ctx =
  assert (cursor = Global);
  let cenv = ctx.global.cenv in
  let cenv = Envs.CEnv.add_nodup_overloaded cid cons cenv in
  { ctx with global = { ctx.global with cenv } }

let add_func_non_overload cursor fid func ctx =
  match cursor with
  | Global ->
      let fenv = ctx.global.fenv in
      let fenv = Envs.FEnv.add_nodup_non_overloaded fid func fenv in
      { ctx with global = { ctx.global with fenv } }
  | Block ->
      let fenv = ctx.block.fenv in
      let fenv = Envs.FEnv.add_nodup_non_overloaded fid func fenv in
      { ctx with block = { ctx.block with fenv } }
  | Local -> assert false

let add_func_overload cursor fid func ctx =
  match cursor with
  | Global ->
      let fenv = ctx.global.fenv in
      let fenv = Envs.FEnv.add_nodup_overloaded fid func fenv in
      { ctx with global = { ctx.global with fenv } }
  | Block ->
      let fenv = ctx.block.fenv in
      let fenv = Envs.FEnv.add_nodup_overloaded fid func fenv in
      { ctx with block = { ctx.block with fenv } }
  | Local -> assert false

let add_value cursor id value ctx =
  match cursor with
  | Global ->
      let venv = ctx.global.venv in
      let venv = Envs.VEnv.add_nodup id value venv in
      { ctx with global = { ctx.global with venv } }
  | Block ->
      let venv = ctx.block.venv in
      let venv = Envs.VEnv.add_nodup id value venv in
      { ctx with block = { ctx.block with venv } }
  | Local ->
      let venvs = ctx.local.venvs in
      let venv, venvs =
        if venvs = [] then (Envs.VEnv.empty, [])
        else (List.hd venvs, List.tl venvs)
      in
      let venv = Envs.VEnv.add_nodup id value venv in
      let venvs = venv :: venvs in
      { ctx with local = { venvs } }

(* Finders *)

let find_cont finder cursor id ctx = function
  | Some value -> Some value
  | None -> finder cursor id ctx

(* Finder for value *)

let rec find_value_opt cursor id ctx =
  match cursor with
  | Global -> Envs.VEnv.find_opt id ctx.global.venv
  | Block ->
      Envs.VEnv.find_opt id ctx.block.venv
      |> find_cont find_value_opt Global id ctx
  | Local ->
      let venvs = ctx.local.venvs in
      List.fold_left
        (fun value venv ->
          match value with
          | Some _ -> value
          | None -> Envs.VEnv.find_opt id venv)
        None venvs
      |> find_cont find_value_opt Block id ctx

(* Finder for constructor *)

let find_cons_opt _cursor (cname, args) ctx =
  Envs.CEnv.find_overloaded_opt (cname, args) ctx.global.cenv

let find_cons cursor (cname, args) ctx =
  find_cons_opt cursor (cname, args) ctx |> Option.get

(* Finder combinator *)

let find_opt finder_opt cursor var ctx =
  match var.it with
  | L.Top id -> finder_opt Global id.it ctx
  | L.Current id -> finder_opt cursor id.it ctx

let find finder_opt cursor var ctx =
  find_opt finder_opt cursor var ctx |> Option.get

let find_overloaded_opt finder_overloaded_opt cursor var args ctx =
  match var.it with
  | L.Top id -> finder_overloaded_opt Global (id.it, args) ctx
  | L.Current id -> finder_overloaded_opt cursor (id.it, args) ctx

let find_overloaded finder_overloaded_opt cursor var args ctx =
  find_overloaded_opt finder_overloaded_opt cursor var args ctx |> Option.get

(* Pretty-printer *)

let pp_gt fmt (gt : gt) =
  F.fprintf fmt
    "@[@[<v 0>[[Global]]@]@\n\
     @[@[<v 0>[Constructors]:@ %a@]@\n\
     @[<v 0>[Functions]:@ %a@]@\n\
     @[<v 0>[Values]:@ %a@]@]" Envs.CEnv.pp gt.cenv Envs.FEnv.pp gt.fenv
    (Envs.VEnv.pp ~level:0) gt.venv

let pp_bt fmt (bt : bt) =
  F.fprintf fmt
    "@[@[<v 0>[[Block]]@]@\n\
     @[@[<v 0>[Functions]:@ %a@]@\n\
     @[<v 0>[Values]:@ %a@]@]" Envs.FEnv.pp bt.fenv (Envs.VEnv.pp ~level:0)
    bt.venv

let pp_lt fmt (lt : lt) =
  F.fprintf fmt "@[@[<v 0>[[Local]]@]@\n@[%a@]@]"
    (pp_list (Envs.VEnv.pp ~level:0) ~sep:Nl)
    lt.venvs

let pp fmt ctx =
  F.fprintf fmt
    "@[@[<v 0>[[Context]]@]@\n\
     @[<v 0>[Global]:@ %a@]@\n\
     @[<v 0>[Block]:@ %a@]@\n\
     @[<v 0>[Local]:@ %a@]@]" pp_gt ctx.global pp_bt ctx.block pp_lt ctx.local
