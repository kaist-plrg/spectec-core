open Syntax.Ast
open Runtime.Domain
open Types
module Value = Runtime.Value

(* Environments *)

module CEnv = MakeEnv (FId) (ConsType)
module TDEnv = MakeEnv (TId) (TypeDef)
module FEnv = MakeEnv (FId) (FuncType)
module VEnv = MakeEnv (Id) (Value)
module TEnv = MakeEnv (Id) (Type)

(* Context is consisted of layers of environments *)

type layer = Global | Block | Local
type frame = TDEnv.t * FEnv.t * VEnv.t * TEnv.t
type t = { cons : CEnv.t; global : frame; block : frame; local : frame list }

let empty_frame = (TDEnv.empty, FEnv.empty, VEnv.empty, TEnv.empty)

let empty =
  { cons = CEnv.empty; global = empty_frame; block = empty_frame; local = [] }

(* Adders *)

let add_td layer tid td ctx =
  match layer with
  | Global ->
      let tdenv, fenv, venv, tenv = ctx.global in
      { ctx with global = (TDEnv.add tid td tdenv, fenv, venv, tenv) }
  | Block ->
      let tdenv, fenv, venv, tenv = ctx.block in
      { ctx with block = (TDEnv.add tid td tdenv, fenv, venv, tenv) }
  | Local ->
      let frame = List.hd ctx.local in
      let tdenv, fenv, venv, tenv = frame in
      let frame = (TDEnv.add tid td tdenv, fenv, venv, tenv) in
      { ctx with local = frame :: List.tl ctx.local }

let add_value layer id value ctx =
  match layer with
  | Global ->
      let tdenv, fenv, venv, tenv = ctx.global in
      { ctx with global = (tdenv, fenv, VEnv.add id value venv, tenv) }
  | Block ->
      let tdenv, fenv, venv, tenv = ctx.block in
      { ctx with block = (tdenv, fenv, VEnv.add id value venv, tenv) }
  | Local ->
      let frame = List.hd ctx.local in
      let tdenv, fenv, venv, tenv = frame in
      let frame = (tdenv, fenv, VEnv.add id value venv, tenv) in
      { ctx with local = frame :: List.tl ctx.local }

let add_type layer id typ ctx =
  match layer with
  | Global ->
      let tdenv, fenv, venv, tenv = ctx.global in
      { ctx with global = (tdenv, fenv, venv, TEnv.add id typ tenv) }
  | Block ->
      let tdenv, fenv, venv, tenv = ctx.block in
      { ctx with block = (tdenv, fenv, venv, TEnv.add id typ tenv) }
  | Local ->
      let frame = List.hd ctx.local in
      let tdenv, fenv, venv, tenv = frame in
      let frame = (tdenv, fenv, venv, TEnv.add id typ tenv) in
      { ctx with local = frame :: List.tl ctx.local }

(* Finders *)

let find_cont finder layer id ctx = function
  | Some value -> Some value
  | None -> finder layer id ctx

let rec find_td_opt layer tid ctx =
  match layer with
  | Global ->
      let tdenv, _, _, _ = ctx.global in
      TDEnv.find_opt tid tdenv
  | Block ->
      let tdenv, _, _, _ = ctx.block in
      TDEnv.find_opt tid tdenv |> find_cont find_td_opt Global tid ctx
  | Local ->
      List.fold_left
        (fun td frame ->
          match td with
          | Some td -> Some td
          | None ->
              let tdenv, _, _, _ = frame in
              TDEnv.find_opt tid tdenv)
        None ctx.local
      |> find_cont find_td_opt Block tid ctx

let find_td layer tid ctx = find_td_opt layer tid ctx |> Option.get

let rec find_value_opt layer id ctx =
  match layer with
  | Global ->
      let _, _, venv, _ = ctx.global in
      VEnv.find_opt id venv
  | Block ->
      let _, _, venv, _ = ctx.block in
      VEnv.find_opt id venv |> find_cont find_value_opt Global id ctx
  | Local ->
      List.fold_left
        (fun value frame ->
          match value with
          | Some value -> Some value
          | None ->
              let _, _, venv, _ = frame in
              VEnv.find_opt id venv)
        None ctx.local
      |> find_cont find_value_opt Block id ctx

let find_value layer id ctx = find_value_opt layer id ctx |> Option.get

let find finder var ctx =
  match var with
  | Top id -> finder Global id.it ctx
  | Bare id -> finder Local id.it ctx

let find_opt finder_opt var ctx =
  match var with
  | Top id -> finder_opt Global id.it ctx
  | Bare id -> finder_opt Local id.it ctx

(* let find_opt finder_opt var ctx = *)
(*   match var with *)
(*   | Top id -> finder_opt Global id ctx *)
(*   | Bare id -> finder_opt Local id ctx *)

(* Pretty-printer *)

let pp_frame fmt frame =
  let tdenv, fenv, venv, tenv = frame in
  Format.fprintf fmt
    "@[@[<v 0>Typedefs:@ %a@]@\n\
     @[<v 0>Functions:@ %a@]@\n\
     @[<v 0>Values:@ %a@]@\n\
     @[<v 0>Types:@ %a@]@]" TDEnv.pp tdenv FEnv.pp fenv VEnv.pp venv TEnv.pp
    tenv

let pp fmt ctx =
  Format.fprintf fmt
    "@[@[<v 0>Constructors:@ %a@]@\n\
     @[<v 0>Global:@ %a@]@\n\
     @[<v 0>Block:@ %a@]@\n\
     @[<v 0>Local:@ %a@]@]" CEnv.pp ctx.cons pp_frame ctx.global pp_frame
    ctx.block
    (Format.pp_print_list pp_frame)
    ctx.local
