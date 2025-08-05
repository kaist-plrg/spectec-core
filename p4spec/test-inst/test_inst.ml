open Util.Error
open Util.Source

let version = "0.1"

(* Statistics *)

type stat = { durations : float list; exclude_run : int; fail_run : int }

let empty_stat = { durations = []; exclude_run = 0; fail_run = 0 }

let log_stat name stat total : unit =
  let excludes = stat.exclude_run in
  let fails = stat.fail_run in
  let passes = total - excludes - fails in
  let exclude_rate = float_of_int excludes /. float_of_int total *. 100.0 in
  let pass_rate = float_of_int passes /. float_of_int total *. 100.0 in
  let fail_rate = float_of_int fails /. float_of_int total *. 100.0 in
  let durations = List.sort compare stat.durations in
  let duration_total = List.fold_left ( +. ) 0.0 durations in
  let duration_avg = duration_total /. float_of_int total in
  let duration_max = durations |> List.rev |> List.hd in
  let duration_min = durations |> List.hd in
  Format.asprintf
    "%s: [EXCLUDE] %d/%d (%.2f%%) [PASS] %d/%d (%.2f%%) [FAIL] %d/%d (%.2f%%)"
    name excludes total exclude_rate passes total pass_rate fails total
    fail_rate
  |> print_endline;
  Format.eprintf "%s: [TOTAL] %.6f [AVG] %.6f [MAX] %.6f [MIN] %.6f\n" name
    duration_total duration_avg duration_max duration_min

(* Exceptions *)

exception TestInstErr of string * region * float
exception TestCheckErr of string * region * float
exception TestUnknownErr of float
exception TestParseErr of string * region * float

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

(* Exclude collector *)

let collect_exclude filename_exclude =
  let ic = open_in filename_exclude in
  let rec parse_lines excludes =
    try
      let exclude = "../../../../" ^ input_line ic in
      let excludes = exclude :: excludes in
      parse_lines excludes
    with End_of_file -> excludes
  in
  let excludes = parse_lines [] in
  close_in ic;
  excludes

let collect_excludes (paths_exclude : string list) =
  let filenames_exclude =
    List.concat_map (collect_files ~suffix:".exclude") paths_exclude
  in
  List.concat_map collect_exclude filenames_exclude

(* Elaboration function *)

let elab specdir =
  specdir
  |> collect_files ~suffix:".watsup"
  |> List.concat_map Frontend.Parse.parse_file
  |> Elaborate.Elab.elab_spec

(* IL test : instantiation *)

let inst_il spec_il includes_p4 filename_p4 =
  let time_start = start () in
  try
    (* Run test *)
    (match
       Interp_il.Instantiation.run_instantiation spec_il includes_p4 filename_p4
     with
    | Success -> ()
    | InstError (at, msg) -> raise (TestInstErr (msg, at, time_start))
    | IllTyped (at, msg) -> raise (TestCheckErr (msg, at, time_start))
    | IllFormed (_, msg) -> raise (TestParseErr (msg, no_region, time_start)));
    time_start
  with
  | TestCheckErr _ as err -> raise err
  | TestInstErr _ as err -> raise err
  | TestParseErr _ as err -> raise err
  | _ -> raise (TestUnknownErr time_start)

let inst_il_test stat spec_il includes_p4 excludes_p4 filename_p4 =
  if List.exists (String.equal filename_p4) excludes_p4 then (
    let log = Format.asprintf "Excluding file: %s" filename_p4 in
    log |> print_endline;
    {
      stat with
      durations = 0.0 :: stat.durations;
      exclude_run = stat.exclude_run + 1;
    })
  else
    try
      let time_start = inst_il spec_il includes_p4 filename_p4 in
      let duration = stop time_start in
      let log = Format.asprintf "Instantiation success: %s" filename_p4 in
      log |> print_endline;
      Format.eprintf "%s\n" log;
      Format.eprintf ">>> took %.6f seconds\n" duration;
      { stat with durations = duration :: stat.durations }
    with
    | TestInstErr (msg, at, time_start) ->
        let duration = stop time_start in
        let log =
          Format.asprintf "Error on instantiation: %s\n%s" filename_p4
            (string_of_error at msg)
        in
        log |> print_endline;
        Format.eprintf "%s\n" log;
        Format.eprintf ">>> took %.6f seconds\n" duration;
        {
          stat with
          durations = duration :: stat.durations;
          fail_run = stat.fail_run + 1;
        }
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
          stat with
          durations = duration :: stat.durations;
          fail_run = stat.fail_run + 1;
        }
    | TestParseErr (msg, at, time_start) ->
        let duration = stop time_start in
        let log =
          Format.asprintf "Error on parse: %s\n%s" filename_p4
            (string_of_error at msg)
        in
        log |> print_endline;
        Format.eprintf "%s\n" log;
        Format.eprintf ">>> took %.6f seconds\n" duration;
        {
          stat with
          durations = duration :: stat.durations;
          fail_run = stat.fail_run + 1;
        }
    | TestUnknownErr time_start ->
        let duration = stop time_start in
        let log = Format.asprintf "Error on instantiation: unknown" in
        log |> print_endline;
        Format.eprintf "%s\n" log;
        Format.eprintf ">>> took %.6f seconds\n" duration;
        {
          stat with
          durations = duration :: stat.durations;
          fail_run = stat.fail_run + 1;
        }

let inst_il_test_driver specdir includes_p4 excludes_p4 testdir_p4 =
  let spec_il = elab specdir in
  let excludes_p4 = collect_excludes excludes_p4 in
  let filenames_p4 = collect_files ~suffix:".p4" testdir_p4 in
  let total = List.length filenames_p4 in
  let stat = empty_stat in
  Format.asprintf "Running instantiation test on %d files\n" total
  |> print_endline;
  let stat =
    List.fold_left
      (fun stat filename_p4 ->
        Format.asprintf "\n>>> Running instantiation test on %s" filename_p4
        |> print_endline;
        inst_il_test stat spec_il includes_p4 excludes_p4 filename_p4)
      stat filenames_p4
  in
  log_stat "\nRunning instantiation" stat total

let inst_il_command =
  Core.Command.basic ~summary:"run instantiation test on IL"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map specdir = flag "-s" (required string) ~doc:"p4 spec directory"
     and includes_p4 = flag "-i" (listed string) ~doc:"p4 include paths"
     and excludes_p4 = flag "-e" (listed string) ~doc:"p4 test exclude paths"
     and testdir_p4 = flag "-d" (required string) ~doc:"p4 test directory" in
     fun () -> inst_il_test_driver specdir includes_p4 excludes_p4 testdir_p4)

let command =
  Core.Command.group ~summary:"p4spec-test-inst"
    [
      ("inst-il", inst_il_command);
    ]

let () = Command_unix.run ~version command 
