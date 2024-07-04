open Syntax.Ast
open Runtime.Object
open Runtime.Cclos
open Runtime.Context
open Runtime.Signal

module type ARCH = sig
  val configure : Config.t -> unit
  val interp_extern : Ctx.t -> Sig.t * Ctx.t
  val drive : CCEnv.t -> Sto.t -> Ctx.t -> Stf.Ast.stmt list -> unit
end

module type INTERP = sig
  val init : Sto.t -> unit
  val interp_call : Ctx.t -> expr -> typ list -> arg list -> Sig.t * Ctx.t
end

module type DRIVER = sig
  val configure : Config.t -> unit
  val run : CCEnv.t -> Sto.t -> Ctx.t -> Stf.Ast.stmt list -> unit
end

module Make
    (MakeArch : functor (Interp : INTERP) -> ARCH)
    (MakeInterp : functor (Arch : ARCH) -> INTERP) : DRIVER = struct
  module rec Arch : ARCH = MakeArch (Interp)
  and Interp : INTERP = MakeInterp (Arch)

  let configure = Arch.configure
  let run = Arch.drive
end
