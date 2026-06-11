(** Save generated artifacts (e.g. quickcheck counterexamples) to a directory,
    one numbered file per distinct content. *)

type outcome =
  | Saved of string  (** Written to this path. *)
  | Duplicate of string
      (** A file at this path already held the content; nothing written. *)

(** Write [content] to a fresh [<base>[_k]<ext>] slot under [out_dir] (created
    if needed; [base] is sanitized). [Duplicate] if some [ext] file there
    already holds [content]; [Error] on IO failure. *)
val save :
  out_dir:string ->
  base:string ->
  ext:string ->
  content:string ->
  (outcome, string) result
