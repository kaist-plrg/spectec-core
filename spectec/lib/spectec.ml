(** Spectec - Entrypoint API facade.

    Provides the core pipeline (parse, elaborate, structure), a unified
    interpreter entry point, and the core type modules (Error, Task, Target). *)

module Error = Error
module Task = Task
module Target = Target
module Diagnostic = Diag

type 'a result = ('a, Error.t) Stdlib.result

let ( let* ) = Result.bind

(* --- Diagnostics --- *)

let with_warnings f =
  Diag.Sink.reset_global ();
  let result = f () in
  let bag = Diag.Sink.drain (Diag.Sink.global ()) in
  (result, bag)

let with_diagnostics f =
  let result, bag = with_warnings f in
  let bag =
    match result with
    | Ok _ -> bag
    | Error e -> Diag.Bag.merge bag (Error.to_diagnostics e)
  in
  (result, bag)

(* --- Pipeline transformations --- *)

let parse_spec_files filenames =
  Pass.parse_files filenames |> Result.map_error (fun e -> Error.PassError e)

let parse_spec_string ~origin source =
  Pass.parse_string ~origin source
  |> Result.map_error (fun e -> Error.PassError e)

let elaborate spec_el =
  Pass.elaborate spec_el |> Result.map_error (fun e -> Error.PassError e)

let structure spec_il = Pass.structure spec_il
let henv_of_el_spec spec_el = Pass.henv_of_el_spec spec_el
let henv_with_il_spec henv spec_il = Pass.henv_with_il_spec henv spec_il
let annotate ~henv spec_sl = Pass.annotate ~henv spec_sl
let shorten spec_pl = Pass.shorten spec_pl

let validate_config config ~sl_mode =
  Instrumentation.Config.validate_mode config ~sl_mode
  |> Result.map_error (fun msg ->
         Error.ConfigError (Common.Source.no_region, msg))

(* --- Unified interpreter entry point --- *)

let eval_task (type i) (module T : Task.S with type input = i) ~sl_mode ~spec_il
    (input : i) =
  let* relation, values = T.parse_input ~spec:spec_il input in
  T.Target.handler @@ fun () ->
  if sl_mode then
    let spec_sl = Pass.structure spec_il in
    Interp.eval_sl (module T.Target) spec_sl relation values (T.source input)
    |> Result.map snd
    |> Result.map_error (fun e -> Error.InterpError e)
  else
    Interp.eval_il (module T.Target) spec_il relation values (T.source input)
    |> Result.map snd
    |> Result.map_error (fun e -> Error.InterpError e)

let eval_task_with_instrumentation (type i)
    (module T : Task.S with type input = i)
    ?(config = Instrumentation.Config.default) ~sl_mode ~spec_il (input : i) =
  let* relation, values = T.parse_input ~spec:spec_il input in
  T.Target.handler @@ fun () ->
  if sl_mode then
    let spec_sl = Pass.structure spec_il in
    Instrumentation.with_instrumentation config
      (Instrumentation.Static.SlSpec spec_sl) (fun () ->
        Interp.eval_sl
          (module T.Target)
          spec_sl relation values (T.source input)
        |> Result.map snd
        |> Result.map_error (fun e -> Error.InterpError e))
  else
    Instrumentation.with_instrumentation config
      (Instrumentation.Static.IlSpec spec_il) (fun () ->
        Interp.eval_il
          (module T.Target)
          spec_il relation values (T.source input)
        |> Result.map snd
        |> Result.map_error (fun e -> Error.InterpError e))
