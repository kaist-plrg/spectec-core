type error
type 'a result = ('a, error) Stdlib.result

(** Spec source [contents] paired with the [filename] its diagnostics are
    attributed to. [contents] may be a file's bytes on disk or an unsaved editor
    buffer; a synthetic input, such as a reparse check, uses an angle-bracketed
    name like [<roundtrip>]. *)
type spec_source = { filename : string; contents : string }

val parse_source : spec_source -> Lang.El.spec result

(** Parses each source in order into one concatenated spec. *)
val parse_sources : spec_source list -> Lang.El.spec result

(** Reads and parses each path in order into one concatenated spec; each path
    becomes the [filename] labeling its own diagnostics. The on-disk counterpart
    of {!parse_sources}. *)
val parse_files : string list -> Lang.El.spec result

val error_to_diagnostic : error -> Diag.t
