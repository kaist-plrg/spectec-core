(** Shared CLI error/diagnostic plumbing.

    {!guard} runs a pipeline thunk inside a fresh diagnostic context, renders
    any diagnostics (and errors) to stderr, and exits with status 1 on [Error].
    Call sites stay free of exit plumbing. *)

(** Resolve a [--color] choice into a concrete style, honoring [NO_COLOR] and a
    stderr TTY check under [Auto]. *)
val resolve_ansi : Cli_args.color -> Spectec.Diagnostic.Ansi.t

(** When [true], suppress warnings (keeping errors) for the rest of the
    process; backs the [-w] flag. *)
val apply_no_warnings : bool -> unit

val guard :
  color:Cli_args.color ->
  on_ok:('a -> unit) ->
  (unit -> ('a, Spectec.Error.t) result) ->
  unit

val guard_unit :
  color:Cli_args.color -> (unit -> (unit, Spectec.Error.t) result) -> unit

val guard_errors_only :
  color:Cli_args.color -> (unit -> (unit, Spectec.Error.t) result) -> unit
