open Utils
open Stackmap
open Stackset

(* Environment *)

module Env = StackMap (Var) (Value)

type env = Env.t

(* Constant Environment *)

module CEnv = StackMap (Var) (Value)

type cenv = CEnv.t

(* Type environment *)

module TEnv = StackMap (Var) (Typ)

type tenv = TEnv.t

(* Type-alias environment *)

module TDEnv = Map.Make (Var)

type tdenv = Typ.t TDEnv.t

(* Local environment *)

module LEnv = StackSet (Var)

type lenv = LEnv.t

(* Constructor closure environment *)

module CCEnv = StackMap (Var) (Cclosure)

type ccenv = CCEnv.t
