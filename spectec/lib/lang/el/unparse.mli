(** Canonical EL printer.

    [pp_spec] emits a normalized EL text. Output parses back to the same AST
    modulo source positions, comments, and [SepD] paragraph separators (all
    three are stripped). Lines wrap at 80 columns via [PPrint] document
    combinators.

    This is not a user-facing formatter: comments are not preserved. Appropriate
    uses are canonical AST inspection, unit-test fixtures for elaboration and
    structuring, and the [./spectecx pp] CLI for debugging. A real formatter
    would require a CST representation; that work is out of scope here. *)

open Types

(** {1 Pretty-printers} *)

val pp_num : Format.formatter -> num -> unit
val pp_text : Format.formatter -> text -> unit
val pp_varid : Format.formatter -> id -> unit
val pp_typid : Format.formatter -> id -> unit
val pp_relid : Format.formatter -> id -> unit
val pp_ruleid : Format.formatter -> id -> unit
val pp_defid : Format.formatter -> id -> unit
val pp_atom : Format.formatter -> atom -> unit
val pp_iter : Format.formatter -> iter -> unit
val pp_typ : Format.formatter -> typ -> unit
val pp_plaintyp : Format.formatter -> plaintyp -> unit
val pp_nottyp : Format.formatter -> nottyp -> unit
val pp_deftyp : Format.formatter -> deftyp -> unit
val pp_exp : Format.formatter -> exp -> unit
val pp_path : Format.formatter -> path -> unit
val pp_param : Format.formatter -> param -> unit
val pp_tparam : Format.formatter -> tparam -> unit
val pp_arg : Format.formatter -> arg -> unit
val pp_prem : Format.formatter -> prem -> unit
val pp_hint : Format.formatter -> hint -> unit
val pp_def : Format.formatter -> def -> unit
val pp_spec : Format.formatter -> spec -> unit

(** {1 String wrappers} *)

val string_of_num : num -> string
val string_of_text : text -> string
val string_of_varid : id -> string
val string_of_typid : id -> string
val string_of_relid : id -> string
val string_of_ruleid : id -> string
val string_of_defid : id -> string
val string_of_atom : atom -> string
val string_of_iter : iter -> string
val string_of_typ : typ -> string
val string_of_plaintyp : plaintyp -> string
val string_of_nottyp : nottyp -> string
val string_of_deftyp : deftyp -> string
val string_of_exp : exp -> string
val string_of_path : path -> string
val string_of_param : param -> string
val string_of_tparam : tparam -> string
val string_of_arg : arg -> string
val string_of_prem : ?values:(id -> string option) -> prem -> string
val string_of_hint : hint -> string
val string_of_def : def -> string
val string_of_spec : spec -> string
