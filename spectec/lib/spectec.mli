(** Spectec - Entrypoint API facade.

    Provides the core pipeline (parse, elaborate, structure), a unified
    interpreter entry point, and the core type modules (Error, Task, Target). *)

module Error = Error
module Task = Task
module Target = Target
module Diagnostic = Diag

type 'a result = ('a, Error.t) Stdlib.result

(** {1 Diagnostics}

    A diagnostic is a single message about the spec (an error, a warning, or a
    note) tied to a place in the source. The pipeline passes (parse, elaborate,
    interpret) report warnings as they run; {!with_warnings} and
    {!with_diagnostics} run a pass and hand those back as a [Diag.Bag.t], so
    callers gather them in one place instead of by hand. *)

(** [with_warnings f] runs [f] and returns its result together with every
    warning [f] reported. Warnings are cleared first, so the bag holds only the
    ones from this call. *)
val with_warnings : (unit -> 'a) -> 'a * Diag.Bag.t

(** [with_diagnostics f] is like {!with_warnings}, but when [f] returns
    [Error e] the error [e] is turned into a diagnostic and added to the bag
    too. The bag is then the complete set to show the user: the warnings plus
    the error the run failed with. *)
val with_diagnostics : (unit -> 'a result) -> 'a result * Diag.Bag.t

(** {1 Spec membership}

    A spec is elaborated from an ordered set of [.spectec] files; a [*.spec]
    marker file (e.g. [specs/p4/p4.spec]) marks its directory as the root of one
    spec. *)

(** [spec_root_of_file file] is the nearest ancestor directory of [file] that
    holds a [*.spec] marker, if any. *)
val spec_root_of_file : string -> string option

(** [collect_spec_files dir] is the [.spectec] files under [dir], gathered
    recursively; digit runs in names compare as numbers ([5.9-] before [5.11-]),
    so section numbers order without zero-padding. *)
val collect_spec_files : string -> string list

(** {1 Pipeline transformations} *)

val parse_spec_files : string list -> Lang.El.spec result

(** [origin] is the label used in diagnostic messages. *)
val parse_spec_string : origin:string -> string -> Lang.El.spec result

(** Parses [(origin, text)] pairs into one concatenated spec, the in-memory
    counterpart of {!parse_spec_files}. Order matters: parsing shares an atom
    and variable table, so each [origin] must follow those it takes names from.
*)
val parse_spec_sources : (string * string) list -> Lang.El.spec result

val elaborate : Lang.El.spec -> Lang.Il.spec result
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
