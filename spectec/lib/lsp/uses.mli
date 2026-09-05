(** Index reference regions using names as written. *)

open Common.Source

type t

val empty : t
val of_spec : Lang.El.spec -> t

(** Find mentions in source order, including subscripted variants. *)
val find : t -> string -> region list
