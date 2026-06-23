(** Relative precedence hint for variant case constructors.

    A [hint(tighter_than '&&')] on a [typcase] declares that the case binds more
    tightly than the sibling case whose operator is the named atom, which must
    be an operator like ['&&'] or a keyword like ['AND']. *)

val parse : El.exp -> Xl.Atom.t option

(** The [tighter_than] atom carried by [hints], if present. *)
val find : Il.hint list -> Xl.Atom.t option
