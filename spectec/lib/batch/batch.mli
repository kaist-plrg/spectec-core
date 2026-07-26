(** Batch - Batch run infrastructure and checkpoint persistence. *)

(** {1 Checkpoint} *)

module Checkpoint : sig
  type config = {
    output_file : string option;
    resume_from : string option;
    save_interval : int;
  }

  val default_config : config

  type coverage = (string * bytes) list

  type t = {
    version : int;
    spec_hash : string;
    completed_inputs : string list;
    coverage : coverage;
    timestamp : float;
  }

  val load_from_file : file:string -> (t, Spectec.Error.t) result
  val save_to_file : file:string -> t -> unit

  val verify_and_load :
    file:string ->
    spec_files:string list ->
    verbose:bool ->
    (t, Spectec.Error.t) result

  val filter_remaining : t -> 'a list -> get_id:('a -> string) -> 'a list
  val restore_coverage : t -> unit

  val save :
    spec_files:string list ->
    completed_inputs:string list ->
    output_file:string option ->
    unit

  val display_report :
    spec:Lang.Il.spec -> config:Instrumentation.Config.t -> t -> unit

  val merge : t -> t -> (t, Spectec.Error.t) result
end

(** {1 Outcome-based runners} *)

type 'i test_result = {
  input : 'i;
  source : string;
  outcome : Spectec.Task.test_outcome;
}

(** Run a single input and compute outcome. Includes full instrumentation
    lifecycle. *)
val run_with_outcome_with_instrumentation :
  (module Spectec.Task.S with type input = 'i) ->
  ?config:Instrumentation.Config.t ->
  sl_mode:bool ->
  spec_il:Lang.Il.spec ->
  'i ->
  Spectec.Task.test_outcome

(** Run a batch of inputs and return individual outcomes. Instrumentation
    lifecycle wraps the entire batch. *)
val run_batch_with_outcomes :
  (module Spectec.Task.S with type input = 'i) ->
  ?config:Instrumentation.Config.t ->
  ansi:Spectec.Diagnostic.Ansi.t ->
  sl_mode:bool ->
  spec_il:Lang.Il.spec ->
  ?verbose:bool ->
  'i list ->
  'i test_result list

(** {1 Batch summary} *)

type batch_summary = {
  pass : int;
  expected_fail : int;
  fail : int;
  unexpected_pass : int;
  total : int;
}

val summarize_outcomes : 'i test_result list -> batch_summary
val summary_passed : batch_summary -> int
val summary_failed : batch_summary -> int

(** {1 Presentation} *)

val print_outcome :
  (module Spectec.Task.S with type input = 'i) ->
  ansi:Spectec.Diagnostic.Ansi.t ->
  string ->
  Spectec.Task.test_outcome ->
  unit

val print_summary : batch_summary -> unit

(** {1 Composed run + print} *)

val run_and_print_single :
  (module Spectec.Task.S with type input = 'i) ->
  ?config:Instrumentation.Config.t ->
  sl_mode:bool ->
  spec_il:Lang.Il.spec ->
  'i ->
  (unit, Spectec.Error.t) result

(** Single run through the PL interpreter. Instrumentation is not supported on
    PL, so unlike {!run_and_print_single} this takes no config. *)
val run_and_print_single_pl :
  (module Spectec.Task.S with type input = 'i) ->
  henv:Hints.Henv.t ->
  spec_il:Lang.Il.spec ->
  'i ->
  (unit, Spectec.Error.t) result

val run_and_print_batch :
  (module Spectec.Task.S with type input = 'i) ->
  ?config:Instrumentation.Config.t ->
  ansi:Spectec.Diagnostic.Ansi.t ->
  sl_mode:bool ->
  spec_il:Lang.Il.spec ->
  verbose:bool ->
  'i list ->
  (unit, Spectec.Error.t) result

(** {1 Per-target run} *)

type failure_kind = Unexpected_failure | Unexpected_pass
type failure = { source : string; kind : failure_kind }

val failure_label : failure_kind -> string

type task_result = {
  task_name : string;
  summary : batch_summary;
  failures : failure list;
}

(** The empty-input check runs before checkpoint filtering, so resuming a
    completed run is not mistaken for an empty collection. *)
val run_target :
  ?config:Instrumentation.Config.t ->
  ?test_dir:string ->
  ansi:Spectec.Diagnostic.Ansi.t ->
  checkpoint_config:Checkpoint.config ->
  verbose:bool ->
  sl_mode:bool ->
  spec_files:string list ->
  Lang.Il.spec ->
  Spectec.Task.packed_task list ->
  (task_result list, Spectec.Error.t) result
