(** Instrumentation facade. Drive instrumentation via {!with_instrumentation}
    rather than poking {!Dispatcher} directly. *)

module Dispatcher = Instrumentation_dispatcher.Dispatcher
module Event = Instrumentation_api.Event
module Output = Instrumentation_api.Output
module Util = Instrumentation_handlers.Util
module Static = Instrumentation_static.Static
module Premise_uid = Instrumentation_static.Premise_uid
module Branch_coverage = Instrumentation_handlers.Branch_coverage
module Node_coverage_il = Instrumentation_handlers.Node_coverage_il
module Node_coverage_sl = Instrumentation_handlers.Node_coverage_sl
module Profile = Instrumentation_handlers.Profile
module Trace = Instrumentation_handlers.Trace

module Handler : sig
  include module type of Instrumentation_api.Handler
  module Spec = Instrumentation_spec.Spec
  module Config = Instrumentation_config.Handler_config
end

module Config : sig
  type t = Handler.Config.t list

  val default : t
  val register_static_dependencies : t -> unit
  val handlers : t -> (module Handler.S) list
  val has_handler : t -> name:string -> bool
  val validate_mode : t -> sl_mode:bool -> (unit, string) result
  val close_outputs : t -> unit
end

(** Every built-in handler spec. Add a new handler by appending here and nowhere
    else — the CLI discovers handlers through this list. *)
val builtin_handler_specs : Handler.Spec.t list

(** [with_instrumentation config spec f] initializes static analyses, starts the
    instrumentation dispatcher, runs [f ()], and always finishes handlers and
    closes outputs on both the success and exception paths. *)
val with_instrumentation : Config.t -> Static.spec -> (unit -> 'a) -> 'a
