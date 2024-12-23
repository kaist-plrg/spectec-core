open Domain
open Tdomain.Types

(* Environment for variable identifiers *)

module VEnv = Dom.MakeIdEnv (Value)
module RType = Dom.MakeTriple (Type) (Dir) (Ctk)
module TEnv = Dom.MakeIdEnv (RType)

(* Environment for type identifiers *)

module TDEnv = Dom.MakeTIdEnv (TypeDef)

(* Environment for function identifiers *)

module FDEnv = Dom.MakeFIdEnv (FuncDef)

(* Environment for constructor identifiers *)

module CDEnv = Dom.MakeCIdEnv (ConsDef)
