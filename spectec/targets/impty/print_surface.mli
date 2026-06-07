(** Surface-syntax printer: an impty [prog] IL [value] back to [.imp] source.

    The output is intended to re-parse via {!Parse} to an equivalent program.
    Not all values are expressible (e.g. negative integer literals), so callers
    needing a guarantee should re-parse the result to confirm. *)

(** Raised when a value has no surface-syntax representation. *)
exception Unsupported of string

(** [string_of_prog v] renders the impty program value [v] as [.imp] source
    text.

    @raise Unsupported if [v] has a construct with no surface form. *)
val string_of_prog : Lang.Il.Value.t -> string
