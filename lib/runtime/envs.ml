open Utils
open Stackenv

(* Environment *)

module Env = StackEnv(Var)(Value)
type env = Env.t

(* Type environment *)

module TEnv = StackEnv(Var)(Typ)
type tenv = TEnv.t

(* Type-alias environment *)

module TDEnv = StackEnv(Var)(Typ)
type tdenv = TDEnv.t

(* Constructor closure environment *)

module CCEnv = StackEnv(Var)(Cclosure)
type ccenv = CCEnv.t
