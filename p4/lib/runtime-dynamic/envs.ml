open Domain.Dom
module Value = Runtime_static.Value
module Types = Runtime_static.Tdomain.Types
module Type = Types.Type

(* Environment for variables *)

module VEnv = MakeIdEnv (Value)

(* Environment for types *)

module TEnv = MakeTIdEnv (Type)

(* Environment for parser states *)

module SEnv = MakeIdEnv (State)

(* Environment for functions *)

module FEnv = MakeFIdEnv (Func)

(* Environment for constructors *)

module CEnv = MakeCIdEnv (Cons)

(* Environment for objects *)

module Sto = MakeOIdEnv (Object)
