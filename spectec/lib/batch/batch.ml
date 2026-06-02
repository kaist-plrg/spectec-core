(** Batch - Batch run infrastructure and checkpoint persistence. *)

open Spectec
open Error
module Events = Instrumentation.Event

(* =========================================================================
   Checkpoint — resumable test run persistence
   ========================================================================= *)

module Checkpoint = struct
  type config = {
    output_file : string option;
    resume_from : string option;
    save_interval : int;
  }

  let default_config =
    { output_file = None; resume_from = None; save_interval = 100 }

  type coverage = (string * bytes) list

  type t = {
    version : int;
    spec_hash : string;
    completed_inputs : string list;
    coverage : coverage;
    timestamp : float;
  }

  let current_version = 2

  let compute_spec_hash spec_files =
    let contents =
      List.sort String.compare spec_files
      |> List.map (fun filename ->
             let input_channel = open_in filename in
             let length = in_channel_length input_channel in
             let content = really_input_string input_channel length in
             close_in input_channel;
             content)
      |> String.concat "\n"
    in
    Digest.string contents |> Digest.to_hex

  let create ~spec_files ~completed_inputs ~coverage =
    {
      version = current_version;
      spec_hash = compute_spec_hash spec_files;
      completed_inputs;
      coverage;
      timestamp = Unix.gettimeofday ();
    }

  let format_summary checkpoint =
    Printf.sprintf "Checkpoint: %d tests completed, saved at %s"
      (List.length checkpoint.completed_inputs)
      ( Unix.gmtime checkpoint.timestamp |> fun time_record ->
        Printf.sprintf "%04d-%02d-%02d %02d:%02d:%02d UTC"
          (time_record.tm_year + 1900)
          (time_record.tm_mon + 1) time_record.tm_mday time_record.tm_hour
          time_record.tm_min time_record.tm_sec )

  let load_from_file ~file =
    try
      let input_channel = open_in_bin file in
      let checkpoint : t = Marshal.from_channel input_channel in
      close_in input_channel;
      if checkpoint.version <> current_version then
        Error
          (Error.DirectoryError
             "Checkpoint format has changed; delete checkpoint file and re-run")
      else Ok checkpoint
    with
    | Sys_error msg ->
        Error
          (Error.DirectoryError
             (Printf.sprintf "Failed to load checkpoint file '%s': %s" file msg))
    | Failure _ | Invalid_argument _ ->
        Error
          (Error.DirectoryError
             "Checkpoint format has changed; delete checkpoint file and re-run")
    | e ->
        Error
          (Error.DirectoryError
             (Printf.sprintf "Failed to load checkpoint file '%s': %s" file
                (Printexc.to_string e)))

  let save_to_file ~file checkpoint =
    let output_channel = open_out_bin file in
    Marshal.to_channel output_channel checkpoint [];
    close_out output_channel

  let verify_spec checkpoint ~spec_files =
    let current_hash = compute_spec_hash spec_files in
    if checkpoint.spec_hash = current_hash then Ok ()
    else Error (Error.SpecMismatchError (checkpoint.spec_hash, current_hash))

  let snapshot_coverage () =
    List.filter_map
      (fun (module D : Instrumentation.Handler.Spec.S) ->
        D.checkpoint
        |> Option.map
             (fun (ops : Instrumentation.Handler.Spec.checkpoint_ops) ->
               (D.name, ops.snapshot ())))
      Instrumentation.builtin_handler_specs

  let restore_coverage checkpoint =
    List.iter
      (fun (name, data) ->
        let found =
          List.find_opt
            (fun (module D : Instrumentation.Handler.Spec.S) -> D.name = name)
            Instrumentation.builtin_handler_specs
        in
        match found with
        | Some (module D) -> (
            match D.checkpoint with
            | Some (ops : Instrumentation.Handler.Spec.checkpoint_ops) ->
                ops.restore data
            | None -> ())
        | None -> ())
      checkpoint.coverage

  let merge_coverage c1 c2 =
    let all_names =
      List.sort_uniq String.compare (List.map fst c1 @ List.map fst c2)
    in
    List.filter_map
      (fun name ->
        let ops_opt =
          match
            List.find_opt
              (fun (module D : Instrumentation.Handler.Spec.S) -> D.name = name)
              Instrumentation.builtin_handler_specs
          with
          | Some (module D : Instrumentation.Handler.Spec.S) -> D.checkpoint
          | None -> None
        in
        match (ops_opt, List.assoc_opt name c1, List.assoc_opt name c2) with
        | ( Some (ops : Instrumentation.Handler.Spec.checkpoint_ops),
            Some b1,
            Some b2 ) ->
            Some (name, ops.merge b1 b2)
        | _, Some b, None -> Some (name, b)
        | _, None, Some b -> Some (name, b)
        | _ -> None)
      all_names

  let merge checkpoint1 checkpoint2 =
    if checkpoint1.spec_hash <> checkpoint2.spec_hash then
      Error
        (Error.SpecMismatchError (checkpoint1.spec_hash, checkpoint2.spec_hash))
    else
      let completed_inputs =
        let seen = Hashtbl.create 256 in
        List.iter
          (fun id -> Hashtbl.replace seen id ())
          checkpoint1.completed_inputs;
        List.iter
          (fun id -> Hashtbl.replace seen id ())
          checkpoint2.completed_inputs;
        Hashtbl.fold (fun id () seen_list -> id :: seen_list) seen []
      in
      let coverage = merge_coverage checkpoint1.coverage checkpoint2.coverage in
      Ok
        {
          version = current_version;
          spec_hash = checkpoint1.spec_hash;
          completed_inputs;
          coverage;
          timestamp = Unix.gettimeofday ();
        }

  let verify_and_load ~file ~spec_files ~verbose =
    let ( let* ) = Result.bind in
    let* checkpoint = load_from_file ~file in
    match verify_spec checkpoint ~spec_files with
    | Ok () ->
        if verbose then
          Format.printf "Resuming from checkpoint: %s\n"
            (format_summary checkpoint);
        Ok checkpoint
    | Error e -> Error e

  let filter_remaining checkpoint inputs ~get_id =
    List.filter
      (fun input -> not (List.mem (get_id input) checkpoint.completed_inputs))
      inputs

  let save ~spec_files ~completed_inputs ~output_file =
    match output_file with
    | Some file ->
        let checkpoint =
          create ~spec_files ~completed_inputs ~coverage:(snapshot_coverage ())
        in
        save_to_file ~file checkpoint
    | None -> ()

  let display_report ~spec ~(config : Instrumentation.Config.t) checkpoint =
    Format.printf "=== Checkpoint Contents ===\n\n";
    Format.printf "%s\n\n" (format_summary checkpoint);
    Format.printf "Completed tests: %d\n\n"
      (List.length checkpoint.completed_inputs);
    let active =
      List.filter_map
        (fun (module D : Instrumentation.Handler.Spec.S) ->
          match D.checkpoint with
          | None -> None
          | Some _ -> (
              match
                List.find_opt
                  (fun a -> a.Instrumentation.Handler.Config.name = D.name)
                  config
              with
              | Some a -> Some a
              | None -> D.parse [ ("level", Some "full"); ("output", None) ]))
        Instrumentation.builtin_handler_specs
    in
    Instrumentation.with_instrumentation active
      (Instrumentation_static.Static.IlSpec spec) (fun () ->
        restore_coverage checkpoint)
end

(* =========================================================================
   Outcome-based runners
   ========================================================================= *)

type 'i test_result = {
  input : 'i;
  source : string;
  outcome : Task.test_outcome;
}

let run_with_outcome_with_instrumentation (type i)
    (module T : Task.S with type input = i)
    ?(config = Instrumentation.Config.default) ~sl_mode ~spec_il (input : i) =
  let result =
    Spectec.eval_task_with_instrumentation
      (module T)
      ~config ~sl_mode ~spec_il input
  in
  Task.compute_outcome (T.expectation input) result

let run_with_outcome (type i) (module T : Task.S with type input = i) ~sl_mode
    ~spec_il (input : i) =
  let test_case_id = T.source input in
  Instrumentation.Dispatcher.emit (Events.Test_start { test_case_id });
  let result =
    try Spectec.eval_task (module T) ~sl_mode ~spec_il input
    with e ->
      Instrumentation.Dispatcher.emit (Events.Test_end { test_case_id });
      raise e
  in
  Instrumentation.Dispatcher.emit (Events.Test_end { test_case_id });
  Task.compute_outcome (T.expectation input) result

let print_outcome_tag = function
  | Task.Pass _ -> Format.printf "PASS\n%!"
  | Task.ExpectedFail _ -> Format.printf "EXPECTED FAIL\n%!"
  | Task.Fail _ -> Format.printf "FAIL\n%!"
  | Task.UnexpectedPass _ -> Format.printf "UNEXPECTED PASS\n%!"

let run_one_input (type i) (module T : Task.S with type input = i) ~sl_mode
    ~spec_il ~verbose (input : i) =
  let source = T.source input in
  let outcome =
    try run_with_outcome (module T) ~sl_mode ~spec_il input
    with exn ->
      let error = UnhandledException (Printexc.to_string exn) in
      Task.compute_outcome (T.expectation input) (Error error)
  in
  if verbose then print_outcome_tag outcome;
  { input; source; outcome }

(* --- Batch summary --- *)

type batch_summary = {
  pass : int;
  expected_fail : int;
  fail : int;
  unexpected_pass : int;
  total : int;
}

let summary_passed s = s.pass + s.expected_fail
let summary_failed s = s.fail + s.unexpected_pass

let summarize_outcomes results =
  let pass, expected_fail, fail, unexpected_pass =
    List.fold_left
      (fun (p, ef, f, up) { outcome; _ } ->
        match outcome with
        | Task.Pass _ -> (p + 1, ef, f, up)
        | Task.ExpectedFail _ -> (p, ef + 1, f, up)
        | Task.Fail _ -> (p, ef, f + 1, up)
        | Task.UnexpectedPass _ -> (p, ef, f, up + 1))
      (0, 0, 0, 0) results
  in
  { pass; expected_fail; fail; unexpected_pass; total = List.length results }

(* --- Presentation --- *)

let print_outcome (type i) (module T : Task.S with type input = i) ~ansi source
    outcome =
  let render_error err =
    Diagnostic.Render.render_bag ~ansi (Error.to_diagnostics err)
  in
  match outcome with
  | Task.Pass values ->
      Format.printf "Passed: %s\n  %s\n\n" source (T.format_output values)
  | Task.ExpectedFail err ->
      Format.printf "Expected fail (passed): %s\n%s\n\n" source
        (render_error err)
  | Task.Fail err ->
      Format.printf "Failed: %s\n%s\n\n" source (render_error err)
  | Task.UnexpectedPass values ->
      Format.printf "Unexpected pass (failed): %s\n  %s\n\n" source
        (T.format_output values)

let print_summary summary =
  let passed = summary_passed summary in
  let failed = summary_failed summary in
  Format.printf "\nTest Results: %d/%d passed, %d failed\n" passed summary.total
    failed

(* --- Batch runner --- *)

let run_batch_with_outcomes (type i) (module T : Task.S with type input = i)
    ?(config = Instrumentation.Config.default) ~sl_mode ~spec_il
    ?(verbose = false) (inputs : i list) =
  let total = List.length inputs in
  Instrumentation.with_instrumentation config
    (Instrumentation.Static.IlSpec spec_il)
  @@ fun () ->
  List.mapi
    (fun idx input ->
      if verbose then
        Format.printf "[%d/%d] %s... %!" (idx + 1) total (T.source input);
      run_one_input (module T) ~sl_mode ~spec_il ~verbose input)
    inputs

(* --- Composed run + print --- *)

let run_and_print_single (type i) (module T : Task.S with type input = i)
    ?config ~sl_mode ~spec_il (input : i) =
  let outcome =
    run_with_outcome_with_instrumentation
      (module T)
      ?config ~sl_mode ~spec_il input
  in
  match outcome with
  | Task.Pass values | Task.UnexpectedPass values ->
      Format.printf "%s\n" (T.format_output values);
      Ok ()
  | Task.Fail err | Task.ExpectedFail err -> Error err

let run_and_print_batch (type i) (module T : Task.S with type input = i) ?config
    ~ansi ~sl_mode ~spec_il ~verbose (inputs : i list) =
  let results =
    run_batch_with_outcomes (module T) ?config ~sl_mode ~spec_il ~verbose inputs
  in
  if not verbose then
    List.iter
      (fun { source; outcome; _ } ->
        Format.printf ">>> Running %s on %s\n" T.name source;
        print_outcome (module T) ~ansi source outcome)
      results;
  print_summary (summarize_outcomes results)

(* --- Target batch runner --- *)

type task_result = { task_name : string; summary : batch_summary }

let run_target ?(config = Instrumentation.Config.default) ?test_dir
    ~(checkpoint_config : Checkpoint.config) ~verbose ~sl_mode ~spec_files
    spec_il tasks =
  Instrumentation.with_instrumentation config
    (Instrumentation.Static.IlSpec spec_il)
  @@ fun () ->
  let loaded_checkpoint =
    match checkpoint_config.resume_from with
    | Some file -> (
        match Checkpoint.verify_and_load ~file ~spec_files ~verbose with
        | Ok checkpoint -> Some checkpoint
        | Error e ->
            Format.printf "%s\n"
              (Diagnostic.Render.render_bag ~ansi:Diagnostic.Ansi.plain
                 (Error.to_diagnostics e));
            None)
    | None -> None
  in
  let all_completed_inputs = ref [] in
  (match loaded_checkpoint with
  | Some checkpoint ->
      all_completed_inputs := checkpoint.Checkpoint.completed_inputs;
      Checkpoint.restore_coverage checkpoint
  | None -> ());
  let save_current_checkpoint () =
    Checkpoint.save ~spec_files ~completed_inputs:!all_completed_inputs
      ~output_file:checkpoint_config.output_file
  in
  let results =
    List.map
      (fun (Task.Pack (module T)) ->
        let all_inputs =
          match test_dir with
          | Some dir -> T.collect ~dir ()
          | None -> T.collect ()
        in
        let total_all = List.length all_inputs in
        let inputs =
          match loaded_checkpoint with
          | Some checkpoint ->
              Checkpoint.filter_remaining checkpoint all_inputs ~get_id:T.source
          | None -> all_inputs
        in
        let completed_count = total_all - List.length inputs in
        if verbose then
          Format.printf "Running %s (%d tests, %d already completed)...\n%!"
            T.name (List.length inputs) completed_count;
        let task_results =
          List.mapi
            (fun index input ->
              if verbose then
                Format.printf "  [%d/%d] %s... %!"
                  (completed_count + index + 1)
                  total_all (T.source input);
              let result =
                run_one_input (module T) ~sl_mode ~spec_il ~verbose input
              in
              all_completed_inputs := result.source :: !all_completed_inputs;
              if (index + 1) mod checkpoint_config.save_interval = 0 then
                save_current_checkpoint ();
              result)
            inputs
        in
        { task_name = T.name; summary = summarize_outcomes task_results })
      tasks
  in
  save_current_checkpoint ();
  results
