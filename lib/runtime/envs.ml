open Utils
open Stackmap

(* Environment *)

module Env = StackMap (Var) (Value)

type env = Env.t

(* Type environment *)

module TEnv = StackMap (Var) (Typ)

type tenv = TEnv.t

(* Type-alias environment *)

module TDEnv = StackMap (Var) (Typ)

type tdenv = TDEnv.t

(* Constructor closure environment *)

module CCEnv = StackMap (Var) (Cclosure)

type ccenv = CCEnv.t
