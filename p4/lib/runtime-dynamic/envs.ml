open Domain.Dom
module Value = Runtime_static.Value

(* Environment for variables *)

module VEnv = MakeIdEnv (Value)

(* Environment for functions *)

module FEnv = MakeFIdEnv (Func)

(* Environment for constructors *)

module CEnv = MakeCIdEnv (Cons)

(* Environment for objects *)

module OEnv = MakeOIdEnv (Object)
