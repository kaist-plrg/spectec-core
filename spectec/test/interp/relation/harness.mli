type compiled_spec
type mode = Il | Sl | Pl
type expectation

(** [compile_file filename] compiles [filename] into the IL, SL, and PL forms
    used by [check]. Any diagnostic emitted during compilation fails the test.
*)
val compile_file : string -> compiled_spec

(** [returns expected] requires evaluation to return values equal to those
    constructed by [expected]. *)
val returns : (unit -> Lang.Il.Value.t list) -> expectation

(** [fails] accepts any evaluation error. *)
val fails : expectation

(** [fails_with substring] accepts an evaluation error when its rendered
    diagnostic contains [substring]. *)
val fails_with : string -> expectation

(** [check spec ~name ~relation ~args expectation] evaluates [relation] in every
    mode in [modes], which contains all three modes by default. [check] creates
    fresh target state for each mode and runs [args], evaluation, and any
    expected-value callback within that state. *)
val check :
  compiled_spec ->
  ?modes:mode list ->
  name:string ->
  relation:string ->
  args:(unit -> Lang.Il.Value.t list) ->
  expectation ->
  unit
