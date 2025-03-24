open Util.Error
open Util.Source

let version = "0.1"

(* Statistics *)

type stat = { fail_run_typing : int }

let empty_stat = { fail_run_typing = 0 }

let log_stat name fails total : unit =
  let passes = total - fails in
  let pass_rate = float_of_int passes /. float_of_int total *. 100.0 in
  let fail_rate = float_of_int fails /. float_of_int total *. 100.0 in
  Format.asprintf "%s: [PASS] %d/%d (%.2f%%) [FAIL] %d/%d (%.2f%%)" name passes
    total pass_rate fails total fail_rate
  |> print_endline

(* Timer *)

let timer filename_p4 test =
  (* Set timer to 30 seconds *)
  let handler =
    Sys.Signal_handle (fun _ -> raise (Error (no_region, "timeout")))
  in
  let _ = Sys.set_signal Sys.sigalrm handler in
  let _ = Unix.alarm 30 in
  (* Run test *)
  let time_start = Unix.gettimeofday () in
  let result = test () in
  let time_end = Unix.gettimeofday () in
  let duration = time_end -. time_start in
  Format.eprintf ">>> %s: %.6f\n" filename_p4 duration;
  (* Reset timer *)
  let _ = Unix.alarm 0 in
  result

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

let run_typing_test stat spec_il includes_p4 filename_p4 =
  try
    let program_p4 =
      P4frontend.Parse.parse_file includes_p4 filename_p4
      |> Interpret.Program.In.in_program
    in
    Interpret.Interp.run_typing false false spec_il program_p4 |> ignore;
    Format.asprintf "Typing success: %s" filename_p4 |> print_endline;
    stat
  with
  | Error (at, msg) ->
      Format.asprintf "Typing failed: %s\n%s" filename_p4
        (string_of_error at msg)
      |> print_endline;
      { fail_run_typing = stat.fail_run_typing + 1 }
  | _ ->
      Format.asprintf "Typing failed: %s" filename_p4 |> print_endline;
      { fail_run_typing = stat.fail_run_typing + 1 }

let run_typing_test_driver specdir includes_p4 testdir_p4 =
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
        timer filename_p4 (fun () ->
            run_typing_test stat spec_il includes_p4 filename_p4))
      stat filenames_p4
  in
  log_stat "\nRunning typing" stat.fail_run_typing total

let run_typing_command =
  Core.Command.basic ~summary:"run typing test"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map specdir = flag "-s" (required string) ~doc:"p4 spec directory"
     and includes_p4 = flag "-i" (listed string) ~doc:"p4 include paths"
     and testdir_p4 = flag "-d" (required string) ~doc:"p4 test directory" in
     fun () -> run_typing_test_driver specdir includes_p4 testdir_p4)

let command =
  Core.Command.group ~summary:"p4spec-test"
    [ ("run-typing", run_typing_command) ]

let () = Command_unix.run ~version command
