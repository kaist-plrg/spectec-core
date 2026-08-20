(** Data types and pure data functions for IL. IL is the internal representation
    after elaboration. *)

include module type of Types
module Mixfix : module type of Mixfix
module Mode : module type of Mode
module Eq : module type of Eq
module Free : module type of Free
module Fresh : module type of Fresh
module Var : module type of Var
module Print : module type of Print
module Print_debug : module type of Print_debug

(** Constructors and operations on IL Values. *)
module Value : sig
  include module type of Value

  val to_string : t -> string
end

(** Convenience constructors for CaseV values, used by target frontends. *)
module Case : module type of Case

(** Constructors and operations on IL Types. *)
module Typ : sig
  include module type of Typ

  val to_string : typ -> string
  val eq : typ -> typ -> bool
end
