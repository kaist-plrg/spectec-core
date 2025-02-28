open Domain.Lib

(* Environments *)

(* Identifier dimension environment *)

module VEnv = struct
  include MakeIdEnv (Dim)

  let to_string env = to_string ~with_braces:false ~bind:"" env
end

(* Type environment *)

module TEnv = MakeIdEnv (Typ)

(* Type definition environment *)

module TDEnv = MakeTIdEnv (Typdef)

(* Relation environment *)

module HEnv = MakeIdEnv (Rel.Hint)
module REnv = MakeRIdEnv (Rel)

(* Definition environment *)

module FEnv = MakeFIdEnv (Func)
