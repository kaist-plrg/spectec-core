type error

val error_to_diagnostics : error -> Diag.Bag.t
val parse_files : string list -> (Lang.El.spec, error) result
val parse_string : origin:string -> string -> (Lang.El.spec, error) result
val elaborate : Lang.El.spec -> (Lang.Il.spec, error) result
val structure : Lang.Il.spec -> Lang.Sl.spec
val henv_of_el_spec : Lang.El.spec -> Hints.Henv.t
val henv_with_il_spec : Hints.Henv.t -> Lang.Il.spec -> Hints.Henv.t
val annotate : henv:Hints.Henv.t -> Lang.Sl.spec -> Pl.spec
val shorten : Pl.spec -> Pl.spec
