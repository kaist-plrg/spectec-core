(** Map verified prose headings to AST source locations. *)

(** Return step mappings; mismatched layouts yield no steps. *)
val sl : text:string -> Lang.Sl.def -> (int * Common.Source.region) list

val pl : text:string -> Lang.Pl.def -> (int * Common.Source.region) list
