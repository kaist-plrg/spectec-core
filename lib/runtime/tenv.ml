open Typ
open Utils 
open Stackenv

(* Type environment *)

module TEnv = StackEnv(Var)(Typ)
type t = TEnv.t
