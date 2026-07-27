(** Which interpreter runs a task. *)

type t = Il | Sl | Pl

let to_string = function Il -> "IL" | Sl -> "SL" | Pl -> "PL"
