(** Per-tag schema for user-authored hints.

    Each registered tag declares the kind of payload it carries and the set of
    subjects (relation, function, type case, ...) it may attach to. The
    elaborator validates every hint in the EL spec against this table. *)

type kind = Alter | Input | Fields | Precedence
type subject = Typcase | Typfield | Rel | Func | Var
type entry = { tag : string; kind : kind; subjects : subject list }

val lookup : string -> entry option
val string_of_subject : subject -> string
val string_of_subjects : subject list -> string
