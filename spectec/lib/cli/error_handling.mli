(** Shared CLI error/diagnostic plumbing.

    {!guard} runs a pipeline thunk inside a fresh diagnostic context, renders
    any diagnostics (and errors) to stderr, and exits with status 1 on [Error].
    Call sites stay free of exit plumbing. *)

(** Resolve a [--color] choice into a concrete style, honoring [NO_COLOR] and a
    stderr TTY check under [Auto]. *)
val resolve_ansi : Cli_args.color -> Spectec.Diagnostic.Ansi.t

(** [suppress_trace] (default [false]) drops the diagnostic's backtrace section,
    for trace-related handlers that repeat the backtrace. *)
val guard :
  ?suppress_trace:bool ->
  color:Cli_args.color ->
  on_ok:('a -> unit) ->
  (unit -> ('a, Spectec.Error.t) result) ->
  unit

val guard_unit :
  ?suppress_trace:bool ->
  color:Cli_args.color ->
  (unit -> (unit, Spectec.Error.t) result) ->
  unit

val guard_errors_only :
  color:Cli_args.color -> (unit -> (unit, Spectec.Error.t) result) -> unit
