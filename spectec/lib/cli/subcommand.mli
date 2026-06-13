(** Subcommand constructors for a target's CLI command group.

    {!make_task}, {!make_parse}, {!make_batch}, and {!make_checkpoint} each
    build one labeled subcommand the caller composes into a
    {!Core.Command.group}.

    Each constructor takes the target as a first-class module so spec loading
    happens inside the callback — [--spec] overrides and the instrumentation
    lifecycle then behave correctly. *)

val make_task :
  (module Spectec.Target.S) ->
  name:string ->
  summary:string ->
  (module Task_cli.S) ->
  string * Core.Command.t

(** Like {!make_task}, but runs a single input with the derivation-tree handler
    forced to premise level — a one-program debug view, no instrumentation flags
    to spell out. *)
val make_debug :
  (module Spectec.Target.S) ->
  name:string ->
  summary:string ->
  (module Task_cli.S) ->
  string * Core.Command.t

val make_parse :
  (module Spectec.Target.S) ->
  name:string ->
  summary:string ->
  (module Task_cli.S) ->
  string * Core.Command.t

val make_batch :
  (module Spectec.Target.S) ->
  name:string ->
  (module Task_cli.S) list ->
  string * Core.Command.t

val make_checkpoint :
  (module Spectec.Target.S) -> name:string -> string * Core.Command.t
