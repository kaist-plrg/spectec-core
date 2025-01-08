open Domain.Dom
open Il.Ast
module Envs_static = Runtime_static.Envs
module Envs_dynamic = Runtime_dynamic.Envs
module VEnv = Envs_dynamic.VEnv
module FEnv = Envs_dynamic.FEnv
module CEnv = Envs_dynamic.CEnv
module Sto = Envs_dynamic.Sto

module type ARCH = sig
  val eval_extern : Ctx.t -> OId.t -> FId.t -> Ctx.t * Sig.t
  val drive : CEnv.t -> FEnv.t -> VEnv.t -> Sto.t -> Stf.Ast.stmt list -> unit
end

module type INTERP = sig
  val init : Sto.t -> unit

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
  val run : CEnv.t -> FEnv.t -> VEnv.t -> Sto.t -> Stf.Ast.stmt list -> unit
end

module Make
    (MakeArch : functor (Interp : INTERP) -> ARCH)
    (MakeInterp : functor (Arch : ARCH) -> INTERP) : DRIVER = struct
  module rec Arch : ARCH = MakeArch (Interp)
  and Interp : INTERP = MakeInterp (Arch)

  let run = Arch.drive
end
