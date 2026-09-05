(** Types retained from the last successful spec elaboration. *)

type t

val empty : t

(** Build alias/relation tables; [None] yields empty tables. *)
val of_il : Lang.Il.spec option -> t

(** Resolve aliases; unknown and parameterised names remain unchanged. *)
val canonical : t -> string -> string

(** Return the relation slot's type in notation order. *)
val hole_type : t -> relation:string -> index:int -> string option
