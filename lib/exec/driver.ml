open Syntax.Ast
open Runtime.Object
open Runtime.Cclos
open Runtime.Context
open Runtime.Signal

module type ARCH = sig
  val drive : CCEnv.t -> Sto.t -> Ctx.t -> Stf.Ast.stmt list -> unit
  val interp_extern : Sig.t -> Ctx.t -> string -> Sig.t * Ctx.t
end

module type INTERP = sig
  val init : Sto.t -> unit
  val interp_call : Ctx.t -> expr -> typ list -> arg list -> Sig.t * Ctx.t
end

module type DRIVER = sig
  val run : CCEnv.t -> Sto.t -> Ctx.t -> Stf.Ast.stmt list -> unit
end

module Make
    (MakeArch : functor (Interp : INTERP) -> ARCH)
    (MakeInterp : functor (Arch : ARCH) -> INTERP) : DRIVER = struct
  module rec Arch : ARCH = MakeArch (Interp)
  and Interp : INTERP = MakeInterp (Arch)

  let run = Arch.drive
end
