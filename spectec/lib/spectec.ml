(** Spectec - Entrypoint API facade.

    Provides the core pipeline (parse, elaborate, structure), a unified
    interpreter entry point, and the core type modules (Error, Task, Target). *)

module Error = Error
module Task = Task
module Target = Target
module Interp_mode = Interp_mode
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

(* --- Spec membership --- *)

let spec_root_of_file file = Spec_files.root_of_file file
let collect_spec_files dir = Spec_files.collect dir

(* --- Pipeline transformations --- *)

type spec_source = Pass.spec_source = { filename : string; contents : string }

let parse_spec_files filenames =
  Pass.parse_files filenames |> Result.map_error (fun e -> Error.PassError e)

let parse_spec_source source =
  Pass.parse_source source |> Result.map_error (fun e -> Error.PassError e)

let parse_spec_sources sources =
  Pass.parse_sources sources |> Result.map_error (fun e -> Error.PassError e)

let elaborate spec_el =
  Pass.elaborate spec_el |> Result.map_error (fun e -> Error.PassError e)

(* Structuring depends only on the spec, so caching on its identity is sound. *)
let structure =
  let cache = ref None in
  fun spec_il ->
    match !cache with
    | Some (spec_il', spec_sl) when spec_il' == spec_il -> spec_sl
    | _ ->
        let spec_sl = Pass.structure spec_il in
        cache := Some (spec_il, spec_sl);
        spec_sl

let henv_of_el_spec spec_el = Pass.henv_of_el_spec spec_el
let henv_with_il_spec henv spec_il = Pass.henv_with_il_spec henv spec_il
let annotate ~henv spec_sl = Pass.annotate ~henv spec_sl
let shorten spec_pl = Pass.shorten spec_pl

let validate_config config ~(mode : Interp_mode.t) =
  Instrumentation.Config.validate_mode config
    ~mode:(match mode with Il -> `IL | Sl -> `SL | Pl -> `PL)
  |> Result.map_error (fun msg ->
         Error.ConfigError (Common.Source.no_region, msg))

(* --- Unified interpreter entry point --- *)

let eval_task (type i) (module T : Task.S with type input = i)
    ~(mode : Interp_mode.t) ~henv ~spec_il (input : i) =
  let* relation, values = T.parse_input ~spec:spec_il input in
  let source = T.source input in
  T.Target.handler @@ fun () ->
  (match mode with
  | Il ->
      Interp.eval_il (module T.Target) spec_il relation values source
      |> Result.map snd
  | Sl ->
      let spec_sl = structure spec_il in
      Interp.eval_sl (module T.Target) spec_sl relation values source
      |> Result.map snd
  | Pl ->
      let spec_pl = structure spec_il |> Pass.annotate ~henv |> Pass.shorten in
      Interp.eval_pl (module T.Target) spec_pl relation values source
      |> Result.map snd)
  |> Result.map_error (fun e -> Error.InterpError e)

let eval_task_with_instrumentation (type i)
    (module T : Task.S with type input = i)
    ?(config = Instrumentation.Config.default) ~(mode : Interp_mode.t) ~henv
    ~spec_il (input : i) =
  let* relation, values = T.parse_input ~spec:spec_il input in
  let source = T.source input in
  T.Target.handler @@ fun () ->
  (match mode with
  | Il ->
      Instrumentation.with_instrumentation config
        (Instrumentation.Static.IlSpec spec_il) (fun () ->
          Interp.eval_il (module T.Target) spec_il relation values source
          |> Result.map snd)
  | Sl ->
      let spec_sl = structure spec_il in
      Instrumentation.with_instrumentation config
        (Instrumentation.Static.SlSpec spec_sl) (fun () ->
          Interp.eval_sl (module T.Target) spec_sl relation values source
          |> Result.map snd)
  | Pl ->
      let spec_sl = structure spec_il in
      let spec_pl = Pass.annotate ~henv spec_sl |> Pass.shorten in
      Instrumentation.with_instrumentation config
        (Instrumentation.Static.SlSpec spec_sl) (fun () ->
          Interp.eval_pl (module T.Target) spec_pl relation values source
          |> Result.map snd))
  |> Result.map_error (fun e -> Error.InterpError e)
