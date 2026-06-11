open Lang.Il

(** Render a counterexample environment as comma-separated name-to-value
    bindings, with each name dimmed. *)
val show_env : ansi:Diag.Ansi.t -> (id' * value) list -> string

val generalize_env :
  ansi:Diag.Ansi.t ->
  spec ->
  (id' * value) list ->
  (string * (id' * value) list Gen.t) list
