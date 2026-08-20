type error = Diag.t list
type 'a result = ('a, error) Stdlib.result

val elab_spec : Lang.El.spec -> Lang.Il.spec result
val error_to_diagnostics : error -> Diag.Bag.t
