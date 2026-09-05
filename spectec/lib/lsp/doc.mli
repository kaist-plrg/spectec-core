(** Recover documentation from source comments discarded during parsing. *)

type t

val empty : t

(** Index sources using filenames matching parsed regions. *)
val of_sources : (string * string) list -> t

(** Prefer trailing comments, then leading; [line] is 1-based. *)
val find : t -> file:string -> line:int -> string option
