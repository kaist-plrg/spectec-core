open Typ
open Utils
open Stackenv

(* Type-alias environment *)

module TDEnv = StackEnv(Var)(Typ)
type t = TDEnv.t
