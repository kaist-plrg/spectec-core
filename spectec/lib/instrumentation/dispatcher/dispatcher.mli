(** Event runtime for instrumentation handlers. Drive instrumentation via
    {!Instrumentation.with_instrumentation}; interpreters call {!emit}. [init]
    fails if instrumentation is already active; [finish] is idempotent. *)

val init :
  spec:Instrumentation_api.Handler.spec ->
  handlers:(module Instrumentation_api.Handler.S) list ->
  unit

(** Dispatch an event to all active handlers. No-op when no session is active,
    so interpreters can be driven without instrumentation configured. *)
val emit : Instrumentation_api.Event.t -> unit

(** Like {!emit}, but builds the event only when a session is active. For events
    whose payload is expensive to construct and read only by handlers. *)
val emit_on_demand : (unit -> Instrumentation_api.Event.t) -> unit

val finish : unit -> unit

(** Extend the active instrumentation session with one extra [handler] for the
    dynamic extent of [f ()]; removed on exit even when [f] raises. Fails if no
    session is active.

    Use for controllers (handlers that raise from [handle] to interrupt
    evaluation). Wrap the call in a [try]/[with] for the controller's exception.
    A session-scope install via {!Instrumentation.Config.handlers} would let the
    exception escape outside any matching catcher. *)
val with_extra_handler :
  spec:Instrumentation_api.Handler.spec ->
  (module Instrumentation_api.Handler.S) ->
  (unit -> 'a) ->
  'a
