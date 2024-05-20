open Sem

(* Flat store of all objects in a program,
   distinguished with fully-qualified paths
   This is to avoid mutual recursion between Object and Store when
   modeling objects have their local objects *)

module Sto = struct
  module S = Map.Make(Path)

  type t = Object.t S.t
end
