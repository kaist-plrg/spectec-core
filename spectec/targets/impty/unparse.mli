(** Surface-syntax printer: an impty [prog] IL [value] back to [.imp] source.

    Intended to re-parse via {!Parse} to the same program, but not guaranteed
    (some constructs raise {!Unsupported}), so callers needing certainty should
    re-parse the result. *)

(** Raised when a value has no surface-syntax representation. *)
exception Unsupported of string

(** [string_of_prog v] renders the impty program value [v] as [.imp] source
    text.

    @raise Unsupported if [v] has a construct with no surface form. *)
val string_of_prog : Lang.Il.Value.t -> string

(** [Spectec.Task.S]-shaped entry point: renders each program value and joins
    with newlines. [spec] is unused (rendering is purely structural).

    @raise Unsupported if any value has a construct with no surface form. *)
val unparse : spec:Lang.Il.spec -> Lang.Il.Value.t list -> string
