open Il.Ast
module Envs = Runtime_dynamic.Envs
module Sto = Envs.Sto

module type ARCH = sig
  val interp_extern : Ctx.t -> Ctx.t * Sig.t
  val drive : Instance.Ctx.gt -> Sto.t -> Stf.Ast.stmt list -> unit
end

module type INTERP = sig
  val init : Sto.t -> unit
  val interp_call : Ctx.t -> expr -> typ list -> arg list -> Ctx.t * Sig.t
end

module type DRIVER = sig
  val run : Instance.Ctx.gt -> Sto.t -> Stf.Ast.stmt list -> unit
end

module Make
    (MakeArch : functor (Interp : INTERP) -> ARCH)
    (MakeInterp : functor (Arch : ARCH) -> INTERP) : DRIVER = struct
  module rec Arch : ARCH = MakeArch (Interp)
  and Interp : INTERP = MakeInterp (Arch)

  let run = Arch.drive
end
