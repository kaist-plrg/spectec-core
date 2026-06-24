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
module Tree = Instrumentation_handlers.Tree
module Run_config = Instrumentation_config.Config
module Exn = Instrumentation_common.Exn

module Handler = struct
  include Instrumentation_api.Handler
  module Spec = Instrumentation_spec.Spec
  module Config = Instrumentation_config.Handler_config
end

module Config = struct
  type t = Handler.Config.t list

  let default = Run_config.default
  let register_static_dependencies = Run_config.register_static_dependencies
  let handlers = Run_config.handlers
  let has_handler = Run_config.has_handler
  let validate_mode = Run_config.validate_mode
  let close_outputs = Run_config.close_outputs
end

(* *** Add one entry here when adding a new handler *** *)
let builtin_handler_specs : Handler.Spec.t list =
  [
    Trace.spec;
    Tree.spec;
    Profile.spec;
    Branch_coverage.spec;
    Node_coverage_il.spec;
    Node_coverage_sl.spec;
  ]

let with_instrumentation (config : Config.t) (spec : Static.spec)
    (f : unit -> 'a) : 'a =
  let handlers = Config.handlers config in
  let cleanup () =
    let first_error = Exn.try_record_first_error None Dispatcher.finish in
    let first_error =
      Exn.try_record_first_error first_error (fun () ->
          Config.close_outputs config)
    in
    Exn.raise_recorded_error first_error
  in
  Static.reset_all ();
  Config.register_static_dependencies config;
  Static.init_all spec;
  Exn.with_cleanup ~cleanup (fun () ->
      Dispatcher.init ~spec ~handlers;
      f ())
