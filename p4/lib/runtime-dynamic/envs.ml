open Domain.Dom
module Value = Runtime_value.Value
module Types = Runtime_type.Types
module Type = Types.Type
module TypeDef = Types.TypeDef

(* Environment for variable identifiers *)

module VEnv = MakeIdEnv (Value)
module SEnv = MakeIdEnv (State)

(* Environment for type identifiers *)

module Theta = MakeTIdEnv (Type)
module TDEnv = MakeTIdEnv (TypeDef)

(* Environment for function identifiers *)

module FEnv = MakeFIdEnv (Func)

(* Environment for constructor identifiers *)

module CEnv = MakeCIdEnv (Cons)

(* Environment for object identifiers *)

module Sto = MakeOIdEnv (Object)
