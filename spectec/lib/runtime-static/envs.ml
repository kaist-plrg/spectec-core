open Domain.Lib

(* Environments *)

(* Identifier type and dimension environment *)

module VEnv = MakeIdEnv (Typ)

(* Plain type (EL type) environment *)

module PTEnv = MakeIdEnv (Plaintyp)

(* Type definition environment *)

module TDEnv = MakeTIdEnv (Typdef)

(* Relation environment *)

module HEnv = MakeIdEnv (Rel.Hint)
module REnv = MakeRIdEnv (Rel)

(* Definition environment *)

module FEnv = MakeFIdEnv (Func)
