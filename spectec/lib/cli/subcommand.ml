let ( let* ) = Result.bind

open Error_handling

let load_spec source =
  let* filenames = Spec_source.files source in
  let* spec = Spectec.parse_spec_files filenames in
  let* spec_il = Spectec.elaborate spec in
  Ok (filenames, spec_il)

let resolve_source ~cli ~config ~default_dir =
  match cli with
  | Some source -> source
  | None -> Option.value config ~default:(Spec_source.Dir default_dir)

let make_task (module Tgt : Spectec.Target.S) ~name ~summary
    (module TC : Task_cli.S) =
  let cmd =
    Core.Command.basic ~summary
    @@
    let open Core.Command.Let_syntax in
    let open Core.Command.Param in
    let%map cli_source = Cli_args.Spec.source_flag
    and sl_mode = Cli_args.Interpreter.sl_mode_flag
    and verbose = flag "-v" no_arg ~doc:" verbose output"
    and batch_mode = Cli_args.Batch.mode_flag
    and batch_dir = Cli_args.Batch.dir_flag
    and input = TC.flags
    and config = Cli_args.Interpreter.config_flags
    and color = Cli_args.Output.color_flag in
    fun () ->
      guard_unit ~color @@ fun () ->
      let ansi = resolve_ansi color in
      let open Spectec in
      let* () = validate_config config ~sl_mode in
      let* cfg = Config_file.load ~target:Tgt.name () in
      let source =
        resolve_source ~cli:cli_source ~config:cfg.Config_file.spec_source
          ~default_dir:Tgt.spec_dir
      in
      let* _files, spec_il = load_spec source in
      match (batch_mode, batch_dir) with
      | false, None ->
          Batch.run_and_print_single
            (module TC.Task)
            ~config ~sl_mode ~spec_il input
      | true, None ->
          Batch.run_and_print_batch
            (module TC.Task)
            ~config ~ansi ~sl_mode ~spec_il ~verbose (TC.Task.collect ())
      | _, Some dir ->
          Batch.run_and_print_batch
            (module TC.Task)
            ~config ~ansi ~sl_mode ~spec_il ~verbose (TC.Task.collect ~dir ())
  in
  (name, cmd)

let make_parse (module Tgt : Spectec.Target.S) ~name ~summary
    (module TC : Task_cli.S) =
  let cmd =
    Core.Command.basic ~summary
    @@
    let open Core.Command.Let_syntax in
    let open Core.Command.Param in
    let%map cli_source = Cli_args.Spec.source_flag
    and input = TC.flags
    and roundtrip = flag "-r" no_arg ~doc:" roundtrip parse/unparse"
    and color = Cli_args.Output.color_flag in
    fun () ->
      guard ~color ~on_ok:(Format.printf "%s\n") @@ fun () ->
      let open Spectec in
      let* cfg = Config_file.load ~target:Tgt.name () in
      let source =
        resolve_source ~cli:cli_source ~config:cfg.Config_file.spec_source
          ~default_dir:Tgt.spec_dir
      in
      let* _files, spec_il = load_spec source in
      let* _, values = TC.Task.parse_input ~spec:spec_il input in
      let unparsed = TC.Task.unparse ~spec:spec_il values in
      if roundtrip then
        let* values_rt =
          unparsed
          |> TC.Task.parse_string ~spec:spec_il ~filename:(TC.Task.source input)
        in
        let eq = Lang.Il.Eq.eq_values ~dbg:true values values_rt in
        if eq then Ok unparsed
        else
          Error
            (Error.RoundtripError (Common.Source.no_region, "Roundtrip failed"))
      else Ok unparsed
  in
  (name, cmd)

let make_batch (module Tgt : Spectec.Target.S) ~name
    (task_clis : (module Task_cli.S) list) =
  let packed_tasks =
    List.map
      (fun (module TC : Task_cli.S) -> Spectec.Task.Pack (module TC.Task))
      task_clis
  in
  let cmd =
    Core.Command.basic
      ~summary:("Run batch over all " ^ Tgt.name ^ " input specs")
    @@
    let open Core.Command.Let_syntax in
    let open Core.Command.Param in
    let%map sl_mode = Cli_args.Interpreter.sl_mode_flag
    and verbose = flag "-v" no_arg ~doc:" verbose: print progress for each test"
    and batch_dir = Cli_args.Batch.dir_flag
    and cli_source = Cli_args.Spec.source_flag
    and checkpoint = Cli_args.Checkpoint.flags
    and config = Cli_args.Interpreter.config_flags
    and color = Cli_args.Output.color_flag in
    fun () ->
      guard_unit ~color @@ fun () ->
      let open Spectec in
      let* () = validate_config config ~sl_mode in
      let* cfg = Config_file.load ~target:Tgt.name () in
      let source =
        resolve_source ~cli:cli_source ~config:cfg.Config_file.spec_source
          ~default_dir:Tgt.spec_dir
      in
      let* spec_files, spec_il = load_spec source in
      let batch_dir =
        match batch_dir with None -> cfg.Config_file.batch_dir | some -> some
      in
      let checkpoint_config : Batch.Checkpoint.config =
        {
          output_file = checkpoint.output;
          resume_from = checkpoint.resume;
          save_interval = checkpoint.save_interval;
        }
      in
      let* results =
        Batch.run_target ~config ?test_dir:batch_dir ~checkpoint_config ~verbose
          ~sl_mode ~spec_files spec_il packed_tasks
      in
      List.iter
        (fun Batch.{ task_name; summary } ->
          let passed = Batch.summary_passed summary in
          let failed = Batch.summary_failed summary in
          Format.printf "%s: %d/%d passed, %d failed\n" task_name passed
            summary.total failed)
        results;
      Ok ()
  in
  (name, cmd)

let make_checkpoint (module Tgt : Spectec.Target.S) ~name =
  let report_command =
    Core.Command.basic ~summary:"Decode and display checkpoint contents"
    @@
    let open Core.Command.Let_syntax in
    let open Core.Command.Param in
    let%map checkpoint_file = anon ("checkpoint-file" %: string)
    and config = Cli_args.Interpreter.config_flags
    and color = Cli_args.Output.color_flag in
    fun () ->
      guard_unit ~color @@ fun () ->
      let* cfg = Config_file.load ~target:Tgt.name () in
      let source =
        resolve_source ~cli:None ~config:cfg.Config_file.spec_source
          ~default_dir:Tgt.spec_dir
      in
      let* spec_files, spec_il = load_spec source in
      let* checkpoint =
        Batch.Checkpoint.verify_and_load ~file:checkpoint_file ~spec_files
          ~verbose:true
      in
      Batch.Checkpoint.display_report ~spec:spec_il ~config checkpoint;
      Ok ()
  in
  let merge_command =
    Core.Command.basic ~summary:"Merge two checkpoint files"
    @@
    let open Core.Command.Let_syntax in
    let open Core.Command.Param in
    let%map checkpoint_file1 = anon ("checkpoint-file-1" %: string)
    and checkpoint_file2 = anon ("checkpoint-file-2" %: string)
    and output_file =
      flag "--output" (required string)
        ~doc:"FILE output file for merged checkpoint"
    and color = Cli_args.Output.color_flag in
    fun () ->
      guard_unit ~color @@ fun () ->
      let* spec_files = Spec_source.files (Spec_source.Dir Tgt.spec_dir) in
      let* checkpoint1 =
        Batch.Checkpoint.verify_and_load ~file:checkpoint_file1 ~spec_files
          ~verbose:false
      in
      let* checkpoint2 =
        Batch.Checkpoint.verify_and_load ~file:checkpoint_file2 ~spec_files
          ~verbose:false
      in
      let* merged = Batch.Checkpoint.merge checkpoint1 checkpoint2 in
      Batch.Checkpoint.save_to_file ~file:output_file merged;
      Format.printf "Merged checkpoint saved to: %s\n" output_file;
      Format.printf "  Checkpoint 1: %d tests\n"
        (List.length checkpoint1.completed_inputs);
      Format.printf "  Checkpoint 2: %d tests\n"
        (List.length checkpoint2.completed_inputs);
      Format.printf "  Merged: %d tests\n" (List.length merged.completed_inputs);
      Ok ()
  in
  let cmd =
    Core.Command.group ~summary:"Checkpoint utilities"
      [ ("report", report_command); ("merge", merge_command) ]
  in
  (name, cmd)
