module F = Format
module L = Lang.Ast
module Types = Il.Types
module Type = Types.Type
module Envs_static = Runtime_static.Envs
module Envs_dynamic = Runtime_dynamic.Envs
module VEnv = Envs_dynamic.VEnv
module SEnv = Envs_dynamic.SEnv
module Theta = Envs_dynamic.Theta
module TDEnv = Envs_dynamic.TDEnv
module FEnv = Envs_dynamic.FEnv
module CEnv = Envs_dynamic.CEnv
open Util.Pp
open Util.Source

(* Context is consisted of layers of environments *)

type cursor = Global | Block | Local

let pp_cursor fmt = function
  | Global -> F.pp_print_string fmt "Global"
  | Block -> F.pp_print_string fmt "Block"
  | Local -> F.pp_print_string fmt "Local"

(* Defining each layer *)

type gt = { cenv : CEnv.t; tdenv : TDEnv.t; fenv : FEnv.t; venv : VEnv.t }
type bt = { theta : Theta.t; fenv : FEnv.t; senv : SEnv.t; venv : VEnv.t }
type lt = { theta : Theta.t; venvs : VEnv.t list }
type t = { global : gt; block : bt; local : lt }

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
let empty = { global = empty_gt; block = empty_bt; local = empty_lt }

(* Inheritance *)

let copy cursor ctx =
  match cursor with
  | Global -> { ctx with block = empty_bt; local = empty_lt }
  | Block -> { ctx with local = empty_lt }
  | Local -> ctx

(* Frame management *)

let enter_frame ctx =
  let venvs = VEnv.empty :: ctx.local.venvs in
  { ctx with local = { ctx.local with venvs } }

let exit_frame ctx =
  match ctx.local.venvs with
  | [] -> assert false
  | _ :: venvs -> { ctx with local = { ctx.local with venvs } }

(* Type resolution *)

let resolve_typ cursor typ ctx =
  match cursor with
  | Global -> typ
  | Block -> Type.subst ctx.block.theta typ
  | Local ->
      let theta = Theta.extend ctx.block.theta ctx.local.theta in
      Type.subst theta typ

(* Adders *)

(* Adders for type definitions *)

let add_typ cursor id typ ctx =
  match cursor with
  | Global -> assert false
  | Block ->
      let theta = ctx.block.theta in
      let theta = Theta.add_nodup id typ theta in
      { ctx with block = { ctx.block with theta } }
  | Local ->
      let theta = ctx.local.theta in
      let theta = Theta.add_nodup id typ theta in
      { ctx with local = { ctx.local with theta } }

let add_typs cursor tparams targs ctx =
  List.fold_left
    (fun ctx (tparam, targ) -> add_typ cursor tparam.it targ.it ctx)
    ctx
    (List.combine tparams targs)

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

(* Updaters *)

(* Updaters for values *)

let rec update_value_opt cursor id value ctx =
  match cursor with
  | Global ->
      VEnv.find_opt id ctx.global.venv
      |> Option.map (fun _ ->
             let venv = VEnv.add id value ctx.global.venv in
             { ctx with global = { ctx.global with venv } })
  | Block ->
      let ctx' =
        VEnv.find_opt id ctx.block.venv
        |> Option.map (fun _ ->
               let venv = VEnv.add id value ctx.block.venv in
               { ctx with block = { ctx.block with venv } })
      in
      if Option.is_some ctx' then ctx' else update_value_opt Global id value ctx
  | Local ->
      let venvs = ctx.local.venvs in
      let venvs, updated =
        List.fold_left
          (fun (venvs, updated) venv ->
            if updated then (venvs @ [ venv ], updated)
            else
              match VEnv.find_opt id venv with
              | Some _ ->
                  let venv = VEnv.add id value venv in
                  (venvs @ [ venv ], true)
              | None -> (venvs @ [ venv ], false))
          ([], false) venvs
      in
      if updated then
        let ctx = { ctx with local = { ctx.local with venvs } } in
        Some ctx
      else update_value_opt Block id value ctx

let update_value cursor id value ctx =
  update_value_opt cursor id value ctx |> Option.get

(* Updater combinators *)

let update_opt updater_opt cursor var value ctx =
  match var.it with
  | L.Top id -> updater_opt Global id.it value ctx
  | L.Current id -> updater_opt cursor id.it value ctx

let update updater_opt cursor var value ctx =
  update_opt updater_opt cursor var value ctx |> Option.get

(* Finders *)

let find_cont finder cursor id ctx = function
  | Some value -> Some value
  | None -> finder cursor id ctx

(* Finders for states *)

let rec find_state_opt cursor id ctx =
  match cursor with
  | Global -> None
  | Block ->
      SEnv.find_opt id ctx.block.senv |> find_cont find_state_opt Global id ctx
  | Local -> find_state_opt Block id ctx

let find_state cursor id ctx = find_state_opt cursor id ctx |> Option.get

(* Finders for functions *)

let rec find_func_at_opt cursor (fname, args) ctx =
  match cursor with
  | Global ->
      FEnv.find_func_opt (fname, args) ctx.global.fenv
      |> Option.map (fun func -> (func, Global))
  | Block ->
      FEnv.find_func_opt (fname, args) ctx.block.fenv
      |> Option.map (fun func -> (func, Block))
      |> find_cont find_func_at_opt Global (fname, args) ctx
  | Local -> find_func_at_opt Block (fname, args) ctx

let find_func_at cursor (fname, args) ctx =
  find_func_at_opt cursor (fname, args) ctx |> Option.get

let find_func_opt cursor (fname, args) ctx =
  find_func_at_opt cursor (fname, args) ctx |> Option.map fst

let find_func cursor (fname, args) ctx =
  find_func_opt cursor (fname, args) ctx |> Option.get

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

(* Finders for type definitions *)

let find_typdef_opt cursor id ctx =
  match cursor with
  | Global -> TDEnv.find_opt id ctx.global.tdenv
  | Block | Local -> TDEnv.find_opt id ctx.global.tdenv

let find_typdef cursor id ctx = find_typdef_opt cursor id ctx |> Option.get

let rec find_typ_opt cursor id ctx =
  match cursor with
  | Global -> None
  | Block ->
      Theta.find_opt id ctx.block.theta |> find_cont find_typ_opt Global id ctx
  | Local ->
      let theta = ctx.local.theta in
      Theta.find_opt id theta |> find_cont find_typ_opt Block id ctx

let find_typ cursor id ctx = find_typ_opt cursor id ctx |> Option.get

(* Finder combinators *)

let find_opt finder_opt cursor var ctx =
  match var.it with
  | L.Top id -> finder_opt Global id.it ctx
  | L.Current id -> finder_opt cursor id.it ctx

let find finder_opt cursor var ctx =
  find_opt finder_opt cursor var ctx |> Option.get

let find_f_opt finder_f_opt cursor var args ctx =
  match var.it with
  | L.Top id -> finder_f_opt Global (id.it, args) ctx
  | L.Current id -> finder_f_opt cursor (id.it, args) ctx

let find_f finder_f_opt cursor var args ctx =
  find_f_opt finder_f_opt cursor var args ctx |> Option.get

let find_f_at_opt finder_f_at_opt cursor var args ctx =
  match var.it with
  | L.Top id -> finder_f_at_opt Global (id.it, args) ctx
  | L.Current id -> finder_f_at_opt cursor (id.it, args) ctx

let find_f_at finder_f_at_opt cursor var args ctx =
  find_f_at_opt finder_f_at_opt cursor var args ctx |> Option.get

(* Pretty-printer *)

let pp_gt ?(level = 0) fmt (gt : gt) =
  F.fprintf fmt
    "%s[[Global Layer]]\n\
     %s[Constructors]%a\n\
     %s[Typedefs]%a\n\
     %s[Functions]%a\n\
     %s[Values]%a\n"
    (indent level)
    (indent (level + 1))
    (CEnv.pp ~level:(level + 2))
    gt.cenv
    (indent (level + 1))
    (TDEnv.pp ~level:(level + 2))
    gt.tdenv
    (indent (level + 1))
    (FEnv.pp ~level:(level + 2))
    gt.fenv
    (indent (level + 1))
    (VEnv.pp ~level:(level + 2))
    gt.venv

let pp_bt ?(level = 0) fmt (bt : bt) =
  F.fprintf fmt
    "%s[[Block Layer]]\n%s[Theta]%a\n%s[Functions]%a\n%s[Values]%a\n"
    (indent level)
    (indent (level + 1))
    (Theta.pp ~level:(level + 2))
    bt.theta
    (indent (level + 1))
    (FEnv.pp ~level:(level + 2))
    bt.fenv
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
