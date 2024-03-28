open Value
open Utils
open Stackenv

(* Environment *)

module Env = StackEnv(Var)(Value)
type t = Env.t
