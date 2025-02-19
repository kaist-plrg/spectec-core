open Domain.Dom
open Dom

(* Environments *)

(* Identifier dimension environment *)

module VEnv = struct
  include MakeIdEnv (Dim)

  let to_string env = to_string ~with_braces:false ~bind:"" env
end

(* Type environment *)

module TEnv = MakeIdEnv (Type)

(* Type definition environment *)

module TDEnv = MakeTIdEnv (TypeDef)

(* Relation environment *)

module REnv = MakeRIdEnv (Rel)

(* Definition environment *)

module FEnv = MakeFIdEnv (Func)
