open Domain.Dom
module Ctk = Runtime_static.Ctk
module Types = Runtime_static.Tdomain.Types
module Envs_static = Runtime_static.Envs
open Il.Ast
module Obj = Runtime_dynamic.Object
module Envs_dynamic = Runtime_dynamic.Envs
module VEnv = Envs_dynamic.VEnv
module TDEnv = Envs_dynamic.TDEnv
module FEnv = Envs_dynamic.FEnv
module CEnv = Envs_dynamic.CEnv
module Sto = Envs_dynamic.Sto
open Util.Source

(* (TODO) Inserts VoidT, shouldn't matter in dynamics but not a good practice either *)
let no_info_expr = (no_info, Il.Ast.{ typ = Types.VoidT; ctk = Ctk.DYN })

let make_expr_base (path : OId.t) =
  let base, members =
    match path with base :: members -> (base, members) | _ -> assert false
  in
  let var_base = Lang.Ast.Current (base $ no_info) $ no_info in
  let expr_base = Il.Ast.VarE { var = var_base } $$ no_info_expr in
  List.fold_left
    (fun expr_base member ->
      let member = member $ no_info in
      Il.Ast.ExprAccE { expr_base; member } $$ no_info_expr)
    expr_base members

let make_arg (arg : Id.t) =
  let var_arg = Lang.Ast.Current (arg $ no_info) $ no_info in
  let expr_arg = Il.Ast.VarE { var = var_arg } $$ no_info_expr in
  Lang.Ast.ExprA expr_arg $ no_info

let make_call (path : OId.t) (func : Id.t) (args : Id.t list) =
  let expr_base = make_expr_base path in
  let args = List.map make_arg args in
  (expr_base, func $ no_info, args)

module type ARCH = sig
  val eval_extern_func_call : Ctx.t -> FId.t -> Ctx.t * Sig.t
  val eval_extern_method_call : Ctx.t -> OId.t -> FId.t -> Ctx.t * Sig.t

  val drive :
    CEnv.t -> TDEnv.t -> FEnv.t -> VEnv.t -> Sto.t -> Stf.Ast.stmt list -> bool
end

module type INTERP = sig
  val sto : Sto.t ref
  val init : Sto.t -> unit
  val update : OId.t -> Obj.t -> unit

  val eval_method_call :
    Ctx.cursor ->
    Ctx.t ->
    expr ->
    member ->
    targ list ->
    arg list ->
    Ctx.t * Sig.t
end

module type DRIVER = sig
  val run :
    CEnv.t -> TDEnv.t -> FEnv.t -> VEnv.t -> Sto.t -> Stf.Ast.stmt list -> bool
end

module Make
    (MakeArch : functor (Interp : INTERP) -> ARCH)
    (MakeInterp : functor (Arch : ARCH) -> INTERP) : DRIVER = struct
  module rec Arch : ARCH = MakeArch (Interp)
  and Interp : INTERP = MakeInterp (Arch)

  let run = Arch.drive
end
