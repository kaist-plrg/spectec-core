open Util.Error
open Util.Source

let version = "0.1"

(* Statistics *)

type stat = { durations_typing : float list; fail_run_typing : int }

let empty_stat = { durations_typing = []; fail_run_typing = 0 }

let log_stat name stat total : unit =
  let fails = stat.fail_run_typing in
  let passes = total - fails in
  let pass_rate = float_of_int passes /. float_of_int total *. 100.0 in
  let fail_rate = float_of_int fails /. float_of_int total *. 100.0 in
  let durations_typing = List.sort compare stat.durations_typing in
  let duration_total = List.fold_left ( +. ) 0.0 durations_typing in
  let duration_avg = duration_total /. float_of_int total in
  let duration_max = durations_typing |> List.rev |> List.hd in
  let duration_min = durations_typing |> List.hd in
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

(* Typing test *)

let run_typing negative spec_il includes_p4 filename_p4 =
  let time_start = start () in
  try
    (* Set timer to 30 seconds *)
    let handler =
      Sys.Signal_handle (fun _ -> raise (Error (no_region, "timeout")))
    in
    let _ = Sys.set_signal Sys.sigalrm handler in
    let _ = Unix.alarm 30 in
    (* Run test *)
    let program_p4 =
      P4frontend.Parse.parse_file includes_p4 filename_p4
      |> Interpret.Program.In.in_program
    in
    Interpret.Interp.run_typing false false spec_il program_p4 |> ignore;
    if negative then raise (TestCheckNegErr time_start);
    (* Reset timer *)
    let _ = Unix.alarm 0 in
    time_start
  with
  | Error (at, msg) -> raise (TestCheckErr (msg, at, time_start))
  | TestCheckNegErr _ as err -> raise err
  | _ -> raise (TestUnknownErr time_start)

let run_typing_test negative stat spec_il includes_p4 filename_p4 =
  try
    let time_start = run_typing negative spec_il includes_p4 filename_p4 in
    let duration = stop time_start in
    let log = Format.asprintf "Typecheck success: %s" filename_p4 in
    log |> print_endline;
    Format.eprintf "%s\n" log;
    Format.eprintf ">>> took %.6f seconds\n" duration;
    { stat with durations_typing = duration :: stat.durations_typing }
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
      {
        durations_typing = duration :: stat.durations_typing;
        fail_run_typing = stat.fail_run_typing + 1;
      }
  | TestCheckNegErr time_start ->
      let duration = stop time_start in
      let log = Format.asprintf "Error on typecheck: should fail" in
      log |> print_endline;
      Format.eprintf "%s\n" log;
      Format.eprintf ">>> took %.6f seconds\n" duration;
      { stat with durations_typing = duration :: stat.durations_typing }
  | TestUnknownErr time_start ->
      let duration = stop time_start in
      let log = Format.asprintf "Error on typecheck: unknown" in
      log |> print_endline;
      Format.eprintf "%s\n" log;
      Format.eprintf ">>> took %.6f seconds\n" duration;
      {
        durations_typing = duration :: stat.durations_typing;
        fail_run_typing = stat.fail_run_typing + 1;
      }

let run_typing_test_driver negative specdir includes_p4 testdir_p4 =
  let spec =
    collect_files ~suffix:".watsup" specdir
    |> List.concat_map Frontend.Parse.parse_file
  in
  let spec_il = Elaborate.Elab.elab_spec spec in
  let filenames_p4 = collect_files ~suffix:".p4" testdir_p4 in
  let total = List.length filenames_p4 in
  let stat = empty_stat in
  Format.asprintf "Running typing test on %d files\n" total |> print_endline;
  let stat =
    List.fold_left
      (fun stat filename_p4 ->
        Format.asprintf "\n>>> Running typing test on %s" filename_p4
        |> print_endline;
        run_typing_test negative stat spec_il includes_p4 filename_p4)
      stat filenames_p4
  in
  log_stat "\nRunning typing" stat total

let run_typing_command =
  Core.Command.basic ~summary:"run typing test"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map specdir = flag "-s" (required string) ~doc:"p4 spec directory"
     and includes_p4 = flag "-i" (listed string) ~doc:"p4 include paths"
     and testdir_p4 = flag "-d" (required string) ~doc:"p4 test directory"
     and negative = flag "-neg" no_arg ~doc:"use negative typing rules" in
     fun () -> run_typing_test_driver negative specdir includes_p4 testdir_p4)

let command =
  Core.Command.group ~summary:"p4spec-test"
    [ ("run-typing", run_typing_command) ]

let () = Command_unix.run ~version command
