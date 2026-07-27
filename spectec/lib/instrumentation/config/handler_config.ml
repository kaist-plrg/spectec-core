type t = {
  name : string;
  modes : [ `IL | `SL | `PL ] list;
  handler : (module Instrumentation_api.Handler.S);
  output : Instrumentation_api.Output.t;
}

let register_static_dependencies ({ handler; _ } : t) =
  let module H = (val handler : Instrumentation_api.Handler.S) in
  List.iter
    (fun (module M : Instrumentation_static.Static.S) ->
      Instrumentation_static.Static.register (module M))
    H.static_dependencies

let to_handler ({ handler; _ } : t) = handler
let close_output ({ output; _ } : t) = Instrumentation_api.Output.close output
