open Base

(* Signal for control flow *)

module Sig = struct
  type t = Cont | Ret of Value.t option | Exit
end
