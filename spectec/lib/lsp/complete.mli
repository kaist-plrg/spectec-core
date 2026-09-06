(** Choose completion candidates from text before the cursor. *)

(** Insert names only, selected by syntactic context. *)
val candidates :
  index:Index.t -> line:string -> character:int -> Linol_eio.CompletionList.t

(** Rank candidates using preceding lines and elaborated types. *)
val in_context :
  index:Index.t ->
  typing:Typing.t ->
  preceding:string list ->
  line:string ->
  character:int ->
  Linol_eio.CompletionList.t
