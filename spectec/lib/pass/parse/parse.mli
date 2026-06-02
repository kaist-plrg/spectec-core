type error
type 'a result = ('a, error) Stdlib.result

val parse_file : string -> Lang.El.spec result
val parse_files : string list -> Lang.El.spec result

(** [origin] is the label used in diagnostic messages. *)
val parse_string : origin:string -> string -> Lang.El.spec result

val error_to_diagnostic : error -> Diag.t
