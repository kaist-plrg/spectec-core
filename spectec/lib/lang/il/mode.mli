(** Mixfix with mode-tagged arg slots.

    A [Mode.t] is a [Mixfix.t] whose argument slots each carry either an input
    payload or an output payload. Two shapes appear in the codebase today:
    [('a, 'a) t] for IL relation types and [('a, unit) t] for SL relation
    interfaces. *)

type ('i, 'o) arg = In of 'i | Out of 'o
type ('i, 'o) t = ('i, 'o) arg Mixfix.t
type dir = Input | Output

(** {1 Construction} *)

(** Tag arg slots of a plain notation according to a parallel direction pattern.
    Raises [Mixfix.Arity_mismatch] on length mismatch. *)
val of_dirs : 'a Mixfix.t -> dir list -> ('a, 'a) t

(** Replace input payloads with [ins], erasing outputs to unit. Raises
    [Mixfix.Arity_mismatch] on length mismatch. *)
val with_inputs : ('a, 'a) t -> 'b list -> ('b, unit) t

(** {1 Projections} *)

val inputs : ('i, 'o) t -> 'i list
val outputs : ('i, 'o) t -> 'o list

(** Drop direction tags, recovering the underlying notation. *)
val notation : ('a, 'a) t -> 'a Mixfix.t

(** True when every arg slot is an input. *)
val is_predicate : ('i, 'o) t -> bool

(** {1 Partitioning} *)

(** Split a list parallel to the arg slots into ([inputs], [outputs]) by
    direction. Raises [Mixfix.Arity_mismatch] on length mismatch. *)
val partition : ('i, 'o) t -> 'a list -> 'a list * 'a list

(** Inverse of [partition]. Raises [Mixfix.Arity_mismatch] on per-direction
    length mismatch. *)
val interleave : ('i, 'o) t -> ins:'a list -> outs:'a list -> 'a list

(** Like [interleave] but keeps the direction tags. Inputs and outputs may carry
    distinct types. Raises [Mixfix.Arity_mismatch] on per-direction length
    mismatch. *)
val fill : ('i, 'o) t -> ins:'a list -> outs:'b list -> ('a, 'b) t

(** {1 Rendering} *)

(** Render via [Mixfix.render] with separate renderers for input and output
    payloads. *)
val render :
  ?pad_brackets:bool ->
  string_of_atom:(Mixfix.atom -> string) ->
  string_of_in:('i -> string) ->
  string_of_out:('o -> string) ->
  ('i, 'o) t ->
  string

(** Render input payloads only, joined by [sep]. *)
val render_inputs :
  sep:string -> string_of_arg:('i -> string) -> ('i, 'o) t -> string
