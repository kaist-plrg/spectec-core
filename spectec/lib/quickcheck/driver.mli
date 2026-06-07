(** Top-level driver: runs every property and generator declaration in a
    [Qc_il.spec] against the elaborated host spec, invoking the interpreter
    through a caller-supplied target. *)

open Lang.Il

(** Implementation of a [builtin generator $id : t] declaration, keyed by the
    declaration's EL identifier name. *)
type manual_gen = spec -> (id' * Value.t) list Gen.t

type error = NoManualGenerator of string
type 'a result = ('a, error) Stdlib.result

val error_to_string : error -> string
val error_to_diagnostic : error -> Diag.t

(** Drives every checkable declaration in [qc_spec], running each property and
    generator [num_tests] times through [target]. [max_size] caps the generator
    size parameter, which grows from 0 to [max_size] across the run.

    [on_counterexample name env recheck] is invoked once per falsified property
    with the property [name], the minimal failing assignment [env] (free
    variable name to value), and [recheck], which re-evaluates the property on
    any assignment and returns [true] iff it is still a counterexample (side
    premises hold, goal fails). [recheck] lets callers confirm that a
    rendered/round-tripped counterexample still reproduces. Omitting
    [on_counterexample] preserves the prior behaviour exactly: outcomes are
    printed and nothing else happens. *)
val check :
  target:(module Target.S) ->
  generalize:bool ->
  max_steps:int ->
  num_tests:int ->
  ?max_size:int ->
  ?on_counterexample:
    (string -> (id' * Value.t) list -> ((id' * Value.t) list -> bool) -> unit) ->
  manual_gens:(string * manual_gen) list ->
  spec ->
  Qc_il.spec ->
  unit result
