(** Reconstructs a runnable [prog] from a Preservation counterexample, which is
    reported as a typing scenario rather than a program: a value environment
    [env], a typing context [tenv], and an [expr] that typechecks but evaluates
    to a value of the wrong type. The program declares one variable per
    [env]/[tenv] binding, then binds [expr], so it saves and replays like a
    program counterexample; it is well-typed and runs to completion, since
    surfacing the unsoundness is the property's job, not the program's. *)

open Lang.Il

(** [prog_of_env bindings] builds the [command] value reproducing the
    counterexample carried by [bindings] (keyed by free-variable name), or
    [None] when [bindings] is not an [env]/[tenv]/[expr] triple this can
    reconstruct. *)
val prog_of_env : (id' * Value.t) list -> Value.t option
