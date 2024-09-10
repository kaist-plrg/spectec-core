open Domain
open Types

(* Environment for variable identifiers *)

module VEnv = MakeIdEnv (Value)
module RType = MakeTriple (Type) (Dir) (Ctk)
module TEnv = MakeIdEnv (RType)

(* Environment for type identifiers *)

module TDEnv = MakeTIdEnv (TypeDef)

(* Environment for function identifiers *)

module FDEnv = MakeFIdEnv (FuncDef)

(* Environment for constructor identifiers *)

module CDEnv = MakeCIdEnv (ConsDef)
