open Domain.Dom
open Dom

(* Environments *)

(* Type environment *)

module TEnv = MakeIdEnv (Type)

(* Type definition environment *)

module TDEnv = MakeTIdEnv (TypeDef)

(* Relation environment *)

module REnv = MakeRIdEnv (Rel)

(* Definition environment *)

module FEnv = MakeFIdEnv (Func)
