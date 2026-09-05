(** Index EL declarations independently of successful elaboration. *)

open Common.Source

type kind = Syntax | Var | Relation | Dec | Rule | Case | Field

(** Use-site notation: literal text or typed holes. *)
type part = Literal of string | Hole of string

type entry = {
  name : string;
      (** Use-site spelling, including function sigils and rule qualifiers. *)
  kind : kind;
  region : region;
      (** Identifier or case atom region, excluding surrounding declaration. *)
  signature : string;  (** One line, for hover. *)
  detail : string option;
      (** Sentence identifying the owning type of cases/fields. *)
  doc : string option;
      (** Source documentation, populated only after [with_docs] runs. *)
  shape : part list;  (** Use-site form; empty when no arguments are needed. *)
  fills : string option;
      (** Type represented by this name; [None] for non-values. *)
  notation : part list;
      (** Relation notation without its name; otherwise empty. *)
}

type t

val empty : t
val of_spec : Lang.El.spec -> t
val string_of_kind : kind -> string

(** Attach source comments to entries by declaration location. *)
val with_docs : sources:(string * string) list -> t -> t

(** Return every indexed entry in source order. *)
val to_list : t -> entry list

(** Find declarations newest-first; retry without subscripts or primes. *)
val find : t -> string -> entry list

(** Return the entries declared in this file. *)
val in_file : t -> string -> entry list

(** Whether a name declares a type or metavariable. *)
val declares : t -> string -> bool

(** Strip a trailing numeric subscript and primes. *)
val base_name : string -> string

(** Return word spellings; operators and punctuation have none. *)
val atom_name : Lang.El.atom -> string option
