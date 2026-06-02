(** Spectec - Entrypoint API facade.

    Provides the core pipeline (parse, elaborate, structure), a unified
    interpreter entry point, and the core type modules (Error, Task, Target). *)

module Error = Error
module Task = Task
module Target = Target
module Diagnostic = Diag

type 'a result = ('a, Error.t) Stdlib.result

(** {1 Diagnostics}

    Warnings emitted during pipeline passes (parse, elaborate, interpret) are
    collected automatically. Use {!with_diagnostics} as the single entry point
    for running pipeline operations that may emit warnings — it handles the
    collection lifecycle so callers cannot forget to reset or drain. *)

(** [with_diagnostics f] runs [f] with a fresh diagnostic context and returns
    its result paired with all diagnostics emitted during the call. The sink is
    reset on entry, so sequential calls are independent. If [f] raises, the
    exception propagates and diagnostics emitted so far are discarded (the next
    call to [with_diagnostics] resets the sink regardless). *)
val with_diagnostics : (unit -> 'a) -> 'a * Diag.Bag.t

(** {1 Pipeline transformations} *)

val parse_spec_files : string list -> Lang.El.spec result

(** [origin] is the label used in diagnostic messages. *)
val parse_spec_string : origin:string -> string -> Lang.El.spec result

(** The elaborator's output: the language IL spec, plus per-backend IL slices.
*)
type il = Pass.il = { lang : Lang.Il.spec; qc : Qc_il.spec }

val elaborate : Lang.El.spec -> il result
val structure : Lang.Il.spec -> Lang.Sl.spec
val henv_of_el_spec : Lang.El.spec -> Hints.Henv.t
val henv_with_il_spec : Hints.Henv.t -> Lang.Il.spec -> Hints.Henv.t
val annotate : henv:Hints.Henv.t -> Lang.Sl.spec -> Pl.spec
val shorten : Pl.spec -> Pl.spec

(** Validate instrumentation config against the current mode. *)
val validate_config : Instrumentation.Config.t -> sl_mode:bool -> unit result

(** {1 Unified interpreter entry point}

    De-duplicates IL/SL dispatch: parses input via task, sets up the target
    handler, and runs the appropriate interpreter. *)

(** Evaluate without instrumentation session. Use when a session is managed
    externally (e.g., suite-level wrapping). *)
val eval_task :
  (module Task.S with type input = 'i) ->
  sl_mode:bool ->
  spec_il:Lang.Il.spec ->
  'i ->
  Lang.Il.Value.t list result

(** Evaluate with instrumentation wrapping the call. *)
val eval_task_with_instrumentation :
  (module Task.S with type input = 'i) ->
  ?config:Instrumentation.Config.t ->
  sl_mode:bool ->
  spec_il:Lang.Il.spec ->
  'i ->
  Lang.Il.Value.t list result
