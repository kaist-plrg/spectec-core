module Envs = Runtime_dynamic.Envs

(* Context is consisted of layers of environments *)

type cursor = Global | Block | Local

(* Defining each layer *)

type gbt = { fenv : Envs.FEnv.t; venv : Envs.VEnv.t }
type lt = { venvs : Envs.VEnv.t list }
type t = { global : gbt; block : gbt; local : lt }

let empty_gbt = { fenv = Envs.FEnv.empty; venv = Envs.VEnv.empty }
let empty_lt = { venvs = [] }
let empty = { global = empty_gbt; block = empty_gbt; local = empty_lt }

(* Adders *)

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
