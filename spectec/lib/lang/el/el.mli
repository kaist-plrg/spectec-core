(** Data types and pure data functions for EL. EL is the surface language of
    SpecTec. *)

include module type of Types
module Eq : module type of Eq
module Free : module type of Free
module Unparse : module type of Unparse
module Print : module type of Print
