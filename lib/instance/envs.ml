open Utils
open Stackmap

(* Constructor closure environment *)

module CCEnv = StackMap (Var) (Cclos)

type ccenv = CCEnv.t
