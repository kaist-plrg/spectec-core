open Util.Error
open Util.Source

let version = "0.1"

(* Statistics *)

type stat = { durations : float list; fail_run : int }

let empty_stat = { durations = []; fail_run = 0 }

let log_stat name stat total : unit =
  let fails = stat.fail_run in
  let passes = total - fails in
  let pass_rate = float_of_int passes /. float_of_int total *. 100.0 in
  let fail_rate = float_of_int fails /. float_of_int total *. 100.0 in
  let durations = List.sort compare stat.durations in
  let duration_total = List.fold_left ( +. ) 0.0 durations in
  let duration_avg = duration_total /. float_of_int total in
  let duration_max = durations |> List.rev |> List.hd in
  let duration_min = durations |> List.hd in
  Format.asprintf "%s: [PASS] %d/%d (%.2f%%) [FAIL] %d/%d (%.2f%%)" name passes
    total pass_rate fails total fail_rate
  |> print_endline;
  Format.eprintf "%s: [TOTAL] %.6f [AVG] %.6f [MAX] %.6f [MIN] %.6f\n" name
    duration_total duration_avg duration_max duration_min

(* Exceptions *)

exception TestCheckErr of string * region * float
exception TestCheckNegErr of float
exception TestUnknownErr of float

(* Timer *)

let start () = Unix.gettimeofday ()
let stop start = Unix.gettimeofday () -. start

(* File collector *)

let rec collect_files ~(suffix : string) dir =
  let files = Sys_unix.readdir dir in
  Array.sort String.compare files;
  Array.fold_left
    (fun files file ->
      let filename = dir ^ "/" ^ file in
      if Sys_unix.is_directory_exn filename && file <> "include" then
        files @ collect_files ~suffix filename
      else if String.ends_with ~suffix filename then files @ [ filename ]
      else files)
    [] files

(* Elaboration test *)

let elab specdir =
  specdir
  |> collect_files ~suffix:".watsup"
  |> List.concat_map Frontend.Parse.parse_file
  |> Elaborate.Elab.elab_spec

let elab_test specdir =
  let spec_il = elab specdir in
  Il.Print.string_of_spec spec_il |> print_endline

let elab_command =
  Core.Command.basic ~summary:"run elaboration test"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map specdir = flag "-s" (required string) ~doc:"p4 spec directory" in
     fun () ->
       try elab_test specdir
       with ParseError (at, msg) | ElabError (at, msg) ->
         Format.printf "Error on elaboration: %s\n" (string_of_error at msg))

(* Structuring test *)

let structure specdir = specdir |> elab |> Structure.Struct.struct_spec

let structure_test specdir =
  let spec_sl = structure specdir in
  Sl.Print.string_of_spec spec_sl |> print_endline

let structure_command =
  Core.Command.basic ~summary:"run structuring test"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map specdir = flag "-s" (required string) ~doc:"p4 spec directory" in
     fun () ->
       try structure_test specdir
       with ParseError (at, msg) | ElabError (at, msg) ->
         Format.printf "%s\n" (string_of_error at msg))

(* SL test *)

let run_sl negative spec_sl includes_p4 filename_p4 =
  let time_start = start () in
  try
    (* Run test *)
    (match Interp_sl.Typing.run_typing spec_sl includes_p4 filename_p4 with
    | WellTyped _ -> if negative then raise (TestCheckNegErr time_start)
    | IllTyped (at, msg, _) -> raise (TestCheckErr (msg, at, time_start))
    | IllFormed (msg, _) -> raise (TestCheckErr (msg, no_region, time_start)));
    time_start
  with
  | TestCheckErr _ as err -> raise err
  | TestCheckNegErr _ as err -> raise err
  | _ -> raise (TestUnknownErr time_start)

let run_sl_test negative stat spec_sl includes_p4 filename_p4 =
  try
    let time_start = run_sl negative spec_sl includes_p4 filename_p4 in
    let duration = stop time_start in
    let log = Format.asprintf "Typecheck success: %s" filename_p4 in
    log |> print_endline;
    Format.eprintf "%s\n" log;
    Format.eprintf ">>> took %.6f seconds\n" duration;
    { stat with durations = duration :: stat.durations }
  with
  | TestCheckErr (msg, at, time_start) ->
      let duration = stop time_start in
      let log =
        Format.asprintf "Error on typecheck: %s\n%s" filename_p4
          (string_of_error at msg)
      in
      log |> print_endline;
      Format.eprintf "%s\n" log;
      Format.eprintf ">>> took %.6f seconds\n" duration;
      { durations = duration :: stat.durations; fail_run = stat.fail_run + 1 }
  | TestCheckNegErr time_start ->
      let duration = stop time_start in
      let log = Format.asprintf "Error on typecheck: should fail" in
      log |> print_endline;
      Format.eprintf "%s\n" log;
      Format.eprintf ">>> took %.6f seconds\n" duration;
      { stat with durations = duration :: stat.durations }
  | TestUnknownErr time_start ->
      let duration = stop time_start in
      let log = Format.asprintf "Error on typecheck: unknown" in
      log |> print_endline;
      Format.eprintf "%s\n" log;
      Format.eprintf ">>> took %.6f seconds\n" duration;
      { durations = duration :: stat.durations; fail_run = stat.fail_run + 1 }

let run_sl_test_driver negative specdir includes_p4 testdir_p4 =
  let spec_sl = structure specdir in
  let filenames_p4 = collect_files ~suffix:".p4" testdir_p4 in
  let total = List.length filenames_p4 in
  let stat = empty_stat in
  Format.asprintf "Running typing test on %d files\n" total |> print_endline;
  let stat =
    List.fold_left
      (fun stat filename_p4 ->
        Format.asprintf "\n>>> Running typing test on %s" filename_p4
        |> print_endline;
        run_sl_test negative stat spec_sl includes_p4 filename_p4)
      stat filenames_p4
  in
  log_stat "\nRunning typing" stat total

let run_sl_command =
  Core.Command.basic ~summary:"run typing test on SL"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map specdir = flag "-s" (required string) ~doc:"p4 spec directory"
     and includes_p4 = flag "-i" (listed string) ~doc:"p4 include paths"
     and testdir_p4 = flag "-d" (required string) ~doc:"p4 test directory"
     and negative = flag "-neg" no_arg ~doc:"use negative typing rules" in
     fun () -> run_sl_test_driver negative specdir includes_p4 testdir_p4)

(* SL coverage test *)

let cover_sl_test specdir includes_p4 testdirs_p4 =
  let spec_sl = structure specdir in
  let filenames_p4 =
    List.concat_map (collect_files ~suffix:".p4") testdirs_p4
  in
  let cover = Interp_sl.Typing.cover_typings spec_sl includes_p4 filenames_p4 in
  Runtime_testgen.Cov.Multiple.log ~filename_cov_opt:None cover

let cover_sl_command =
  Core.Command.basic ~summary:"measure phantom coverage of SL"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map specdir = flag "-s" (required string) ~doc:"p4 spec directory"
     and includes_p4 = flag "-i" (listed string) ~doc:"p4 include paths"
     and testdirs_p4 = flag "-d" (listed string) ~doc:"p4 test directory" in
     fun () -> cover_sl_test specdir includes_p4 testdirs_p4)

let command =
  Core.Command.group ~summary:"p4spec-test"
    [
      ("elab", elab_command);
      ("struct", structure_command);
      ("run-sl", run_sl_command);
      ("cover-sl", cover_sl_command);
    ]

let () = Command_unix.run ~version command
