open Il.Ast
open Driver
module Envs = Runtime_dynamic.Envs

module Make (Arch : ARCH) : INTERP = struct
  (* Global store *)

  let sto = ref Envs.Sto.empty
  let init (_sto : Sto.t) : unit = sto := _sto

  (* Entry point: call *)

  let interp_call (_ctx : Ctx.t) (_expr : expr) (_typs : typ list)
      (_args : arg list) : Ctx.t * Sig.t =
    failwith "TODO"
end
