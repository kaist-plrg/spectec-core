module F = Format
module L = Lang.Ast
module Types = Runtime_static.Tdomain.Types
module TypeDef = Types.TypeDef
module Envs_dynamic = Runtime_dynamic.Envs
module VEnv = Envs_dynamic.VEnv
module SEnv = Envs_dynamic.SEnv
module Theta = Envs_dynamic.Theta
module TDEnv = Envs_dynamic.TDEnv
module FEnv = Envs_dynamic.FEnv
module CEnv = Envs_dynamic.CEnv
open Util.Pp
open Util.Source
open Util.Error

let error_no_info = error_inst_no_info
let check = check_inst

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

type gt = { cenv : CEnv.t; tdenv : TDEnv.t; fenv : FEnv.t; venv : VEnv.t }
type bt = { theta : Theta.t; fenv : FEnv.t; senv : SEnv.t; venv : VEnv.t }
type lt = { theta : Theta.t; venvs : VEnv.t list }
type t = { path : Domain.Dom.OId.t; global : gt; block : bt; local : lt }

let empty_gt =
  {
    cenv = CEnv.empty;
    tdenv = TDEnv.empty;
    fenv = FEnv.empty;
    venv = VEnv.empty;
  }

let empty_bt =
  {
    theta = Theta.empty;
    fenv = FEnv.empty;
    senv = SEnv.empty;
    venv = VEnv.empty;
  }

let empty_lt = { theta = Theta.empty; venvs = [ VEnv.empty ] }
let empty = { path = []; global = empty_gt; block = empty_bt; local = empty_lt }

(* Inheritance *)

let copy cursor ctx =
  match cursor with
  | Global -> { ctx with block = empty_bt; local = empty_lt }
  | Block -> { ctx with local = empty_lt }
  | Local -> ctx

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
  let venvs = VEnv.empty :: ctx.local.venvs in
  { ctx with local = { ctx.local with venvs } }

let exit_frame ctx =
  match ctx.local.venvs with
  | [] -> assert false
  | _ :: venvs -> { ctx with local = { ctx.local with venvs } }

(* Adders *)

(* Adders for constructors *)

let add_cons cursor cid cons ctx =
  assert (cursor = Global);
  let cenv = ctx.global.cenv in
  let cenv = CEnv.add_nodup_overloaded cid cons cenv in
  { ctx with global = { ctx.global with cenv } }

(* Adders for type definitions *)

let add_tparam cursor tparam typ ctx =
  match cursor with
  | Global -> assert false
  | Block ->
      let theta = Theta.add tparam.it typ.it ctx.block.theta in
      { ctx with block = { ctx.block with theta } }
  | Local ->
      let theta = Theta.add tparam.it typ.it ctx.local.theta in
      { ctx with local = { ctx.local with theta } }

let add_tparams cursor tparams typs ctx =
  List.fold_left2
    (fun ctx tparam typ -> add_tparam cursor tparam typ ctx)
    ctx tparams typs

let add_typdef cursor tid td ctx =
  match cursor with
  | Global ->
      let tdenv = ctx.global.tdenv in
      let tdenv = TDEnv.add_nodup tid td tdenv in
      { ctx with global = { ctx.global with tdenv } }
  | Block | Local ->
      "(add_typdef) block and local layer cannot have type definitions"
      |> error_no_info

(* Adders for functions *)

let add_func_non_overload cursor fid func ctx =
  match cursor with
  | Global ->
      let fenv = ctx.global.fenv in
      let fenv = FEnv.add_nodup_non_overloaded fid func fenv in
      { ctx with global = { ctx.global with fenv } }
  | Block ->
      let fenv = ctx.block.fenv in
      let fenv = FEnv.add_nodup_non_overloaded fid func fenv in
      { ctx with block = { ctx.block with fenv } }
  | Local -> assert false

let add_func_overload cursor fid func ctx =
  match cursor with
  | Global ->
      let fenv = ctx.global.fenv in
      let fenv = FEnv.add_nodup_overloaded fid func fenv in
      { ctx with global = { ctx.global with fenv } }
  | Block ->
      let fenv = ctx.block.fenv in
      let fenv = FEnv.add_nodup_overloaded fid func fenv in
      { ctx with block = { ctx.block with fenv } }
  | Local -> assert false

let add_state cursor id state ctx =
  match cursor with
  | Block ->
      let senv = ctx.block.senv in
      let senv = SEnv.add_nodup id state senv in
      { ctx with block = { ctx.block with senv } }
  | _ -> assert false

(* Adders for values *)

let add_value cursor id value ctx =
  match cursor with
  | Global ->
      let venv = ctx.global.venv in
      let venv = VEnv.add_nodup id value venv in
      { ctx with global = { ctx.global with venv } }
  | Block ->
      let venv = ctx.block.venv in
      let venv = VEnv.add_nodup id value venv in
      { ctx with block = { ctx.block with venv } }
  | Local ->
      let venvs = ctx.local.venvs in
      let venv, venvs = (List.hd venvs, List.tl venvs) in
      let venv = VEnv.add_nodup id value venv in
      let venvs = venv :: venvs in
      { ctx with local = { ctx.local with venvs } }

let add_values cursor ids values ctx =
  List.fold_left2
    (fun ctx id value -> add_value cursor id value ctx)
    ctx ids values

(* Finders *)

let find_cont finder cursor id ctx = function
  | Some value -> Some value
  | None -> finder cursor id ctx

(* Finders for constructors *)

let find_cons_opt _cursor (cname, args) ctx =
  CEnv.find_func_opt (cname, args) ctx.global.cenv

let find_cons cursor (cname, args) ctx =
  find_cons_opt cursor (cname, args) ctx |> Option.get

(* Finders for type definitions *)

let find_typdef_opt cursor tid ctx =
  match cursor with
  | Global -> TDEnv.find_opt tid ctx.global.tdenv
  | Block | Local -> TDEnv.find_opt tid ctx.global.tdenv

let find_typdef cursor tid ctx = find_typdef_opt cursor tid ctx |> Option.get

(* Finders for values *)

let rec find_value_opt cursor id ctx =
  match cursor with
  | Global -> VEnv.find_opt id ctx.global.venv
  | Block ->
      VEnv.find_opt id ctx.block.venv |> find_cont find_value_opt Global id ctx
  | Local ->
      let venvs = ctx.local.venvs in
      List.fold_left
        (fun value venv ->
          match value with Some _ -> value | None -> VEnv.find_opt id venv)
        None venvs
      |> find_cont find_value_opt Block id ctx

let find_value cursor id ctx = find_value_opt cursor id ctx |> Option.get

(* Finder combinators *)

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

let pp_gt ?(level = 0) fmt (gt : gt) =
  F.fprintf fmt
    "%s[[Global Layer]]\n%s[Constructors]%a\n%s[Functions]%a\n%s[Values]%a\n"
    (indent level)
    (indent (level + 1))
    (CEnv.pp ~level:(level + 2))
    gt.cenv
    (indent (level + 1))
    (FEnv.pp ~level:(level + 2))
    gt.fenv
    (indent (level + 1))
    (VEnv.pp ~level:(level + 2))
    gt.venv

let pp_bt ?(level = 0) fmt (bt : bt) =
  F.fprintf fmt
    "%s[[Block Layer]]\n\
     %s[Theta]%a\n\
     %s[Functions]%a\n\
     %s[States]%a\n\
     %s[Values]%a\n"
    (indent level)
    (indent (level + 1))
    (Theta.pp ~level:(level + 2))
    bt.theta
    (indent (level + 1))
    (FEnv.pp ~level:(level + 2))
    bt.fenv
    (indent (level + 1))
    (SEnv.pp ~level:(level + 2))
    bt.senv
    (indent (level + 1))
    (VEnv.pp ~level:(level + 2))
    bt.venv

let pp_lt ?(level = 0) fmt (lt : lt) =
  F.fprintf fmt "%s[[Local Layer]]\n%s[Theta]%a\n%s[Values]\n%a\n"
    (indent level)
    (indent (level + 1))
    (Theta.pp ~level:(level + 2))
    lt.theta
    (indent (level + 1))
    (pp_list ~level:(level + 2) VEnv.pp ~sep:Nl)
    lt.venvs

let pp ?(level = 0) fmt ctx =
  F.fprintf fmt "===== Context =====\n%a%a%a"
    (pp_gt ~level:(level + 1))
    ctx.global
    (pp_bt ~level:(level + 1))
    ctx.block
    (pp_lt ~level:(level + 1))
    ctx.local
