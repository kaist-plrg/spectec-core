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

(** A falsified property reported by {!check}: the property [name] and the
    minimal failing assignment [env] (free variable name to value). *)
type counterexample = { name : string; env : (id' * Value.t) list }

(** Drives every checkable declaration in [qc_spec], running each property and
    generator [num_tests] times through [target]. Outcomes are printed as they
    run; the result lists every falsified property as a {!counterexample}. *)
val check :
  target:(module Target.S) ->
  generalize:bool ->
  max_steps:int ->
  num_tests:int ->
  manual_gens:(string * manual_gen) list ->
  spec ->
  Qc_il.spec ->
  counterexample list result
