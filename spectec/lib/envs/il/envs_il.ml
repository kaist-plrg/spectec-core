open Envs_make
module Typ = Typ
module Typdef = Typdef

(* Type definition environment *)

module TDEnv = MakeTIdMap (Typdef)
