(** Data types and pure data functions for SL. SL is IL with structured control
    flow *)

include module type of Types
module Eq : module type of Eq
module Free : module type of Free
module Print : module type of Print
