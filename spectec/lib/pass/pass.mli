(** The pipeline passes (parse, elaborate, structure, ...) behind one signature,
    with parse and elaborate failures unified under a single {!error}. *)

type error
type spec_source = Parse.spec_source = { filename : string; contents : string }

val error_to_diagnostics : error -> Diag.Bag.t
val parse_source : spec_source -> (Lang.El.spec, error) result
val parse_sources : spec_source list -> (Lang.El.spec, error) result
val parse_files : string list -> (Lang.El.spec, error) result
val elaborate : Lang.El.spec -> (Lang.Il.spec, error) result
val structure : Lang.Il.spec -> Lang.Sl.spec
val henv_of_el_spec : Lang.El.spec -> Hints.Henv.t
val henv_with_il_spec : Hints.Henv.t -> Lang.Il.spec -> Hints.Henv.t
val annotate : henv:Hints.Henv.t -> Lang.Sl.spec -> Pl.spec
val shorten : Pl.spec -> Pl.spec
