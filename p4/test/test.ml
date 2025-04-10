open Util.Error

let version = "0.1"

type stat = {
  fail_parse_file : int;
  fail_parse_string : int;
  fail_parse_roundtrip : int;
  fail_typecheck : int;
  fail_typecheck_roundtrip : int;
  fail_instantiate : int;
  fail_run : int;
}

let empty_stat =
  {
    fail_parse_file = 0;
    fail_parse_string = 0;
    fail_parse_roundtrip = 0;
    fail_typecheck = 0;
    fail_typecheck_roundtrip = 0;
    fail_instantiate = 0;
    fail_run = 0;
  }

let eq_stat stat_a stat_b =
  stat_a.fail_parse_file = stat_b.fail_parse_file
  && stat_a.fail_parse_string = stat_b.fail_parse_string
  && stat_a.fail_parse_roundtrip = stat_b.fail_parse_roundtrip
  && stat_a.fail_typecheck = stat_b.fail_typecheck
  && stat_a.fail_typecheck_roundtrip = stat_b.fail_typecheck_roundtrip
  && stat_a.fail_instantiate = stat_b.fail_instantiate
  && stat_a.fail_run = stat_b.fail_run

type mode = Pos | Neg

exception TestParseFileErr of string * Util.Source.info * stat
exception TestParseStringErr of string * Util.Source.info * stat
exception TestParseRoundtripErr of stat
exception TestCheckErr of string * Util.Source.info * stat
exception TestCheckNegErr of stat
exception TestCheckRoundtripCheckErr of string * Util.Source.info * stat
exception TestCheckRoundtripMismatchErr of stat
exception TestInstErr of string * Util.Source.info * stat
exception TestParseStfErr of string * stat
exception TestInterpErr of string * Util.Source.info * stat
exception TestDriverErr of string * stat
exception TestStfErr of stat

let log_stat name fails total : unit =
  let passes = total - fails in
  let pass_rate = float_of_int passes /. float_of_int total *. 100.0 in
  let fail_rate = float_of_int fails /. float_of_int total *. 100.0 in
  Format.asprintf "%s: [PASS] %d/%d (%.2f%%) [FAIL] %d/%d (%.2f%%)" name passes
    total pass_rate fails total fail_rate
  |> print_endline

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

(* Parser roundtrip test *)

let parse_file stat includes filename =
  try
    let program = Frontend.Parse.parse_file includes filename in
    (stat, program)
  with ParseErr (msg, info) -> raise (TestParseFileErr (msg, info, stat))

let parse_string stat filename file =
  try
    let program = Frontend.Parse.parse_string filename file in
    (stat, program)
  with ParseErr (msg, info) -> raise (TestParseStringErr (msg, info, stat))

let parse_roundtrip stat includes filename =
  let stat, program' = parse_file stat includes filename in
  let file' = Format.asprintf "%a\n" El.Pp.pp_program program' in
  let stat, program'' = parse_string stat filename file' in
  if not (El.Eq.eq_program program' program'') then
    raise (TestParseRoundtripErr stat);
  stat

let parse_test stat includes filename =
  Format.asprintf "\n>>> Running parser roundtrip test on %s" filename
  |> print_endline;
  try
    let stat = parse_roundtrip stat includes filename in
    Format.asprintf "Parser roundtrip success: %s" filename |> print_endline;
    stat
  with
  | TestParseFileErr (msg, info, stat) ->
      Format.asprintf "Error on file parser: %a\n%s" Util.Source.pp info msg
      |> print_endline;
      { stat with fail_parse_file = stat.fail_parse_file + 1 }
  | TestParseStringErr (msg, info, stat) ->
      Format.asprintf "Error on string parser: %a\n%s" Util.Source.pp info msg
      |> print_endline;
      { stat with fail_parse_string = stat.fail_parse_string + 1 }
  | TestParseRoundtripErr stat ->
      Format.asprintf "Error: parser roundtrip fail" |> print_endline;
      { stat with fail_parse_roundtrip = stat.fail_parse_roundtrip + 1 }

let parse_test_driver includes testdir =
  let files = collect_files ~suffix:".p4" testdir in
  let total = List.length files in
  let stat = empty_stat in
  Format.asprintf "Running parser roundtrip tests on %d files\n" total
  |> print_endline;
  let stat =
    List.fold_left
      (fun stat filename -> parse_test stat includes filename)
      stat files
  in
  log_stat "\nParser on file" stat.fail_parse_file total;
  let total = total - stat.fail_parse_file in
  log_stat "Parser on string" stat.fail_parse_string total;
  let total = total - stat.fail_parse_string in
  log_stat "Parser roundtrip" stat.fail_parse_roundtrip total

let parse_command =
  Core.Command.basic ~summary:"parser roundtrip test"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map includes = flag "-i" (listed string) ~doc:"include paths"
     and testdir = anon ("testdir" %: string) in
     fun () -> parse_test_driver includes testdir)

(* Typecheck test *)

let typecheck_file stat includes mode filename =
  try
    let stat, program = parse_file stat includes filename in
    let program = Typing.Typecheck.type_program program in
    if mode = Neg then raise (TestCheckNegErr stat);
    (stat, program)
  with CheckErr (msg, info) -> raise (TestCheckErr (msg, info, stat))

let typecheck_roundtrip stat includes filename =
  let stat, program' = typecheck_file stat includes Pos filename in
  let program' = Typing.Postprocess.postprocess_program program' in
  let file' = Format.asprintf "%a\n" Il.Pp_to_el.pp_program program' in
  let stat, program'' =
    try
      let program'' =
        Frontend.Parse.parse_string filename file'
        |> Typing.Typecheck.type_program
        |> Typing.Postprocess.postprocess_program
      in
      (stat, program'')
    with ParseErr (msg, info) | CheckErr (msg, info) ->
      raise (TestCheckRoundtripCheckErr (msg, info, stat))
  in
  if not (Il.Eq.eq_program program' program'') then
    raise (TestCheckRoundtripMismatchErr stat);
  stat

let typecheck_test stat includes mode filename =
  Format.asprintf "\n>>> Running typecheck test on %s" filename |> print_endline;
  try
    match mode with
    | Pos ->
        let stat = typecheck_roundtrip stat includes filename in
        Format.asprintf "Typecheck success" |> print_endline;
        stat
    | Neg ->
        let stat, _program = typecheck_file stat includes mode filename in
        Format.asprintf "Typecheck success: %s" filename |> print_endline;
        stat
  with
  | TestParseFileErr (msg, info, stat) ->
      Format.asprintf "Error on parser: %a\n%s" Util.Source.pp info msg
      |> print_endline;
      { stat with fail_parse_file = stat.fail_parse_file + 1 }
  | TestCheckErr (msg, info, stat) ->
      Format.asprintf "Error on typecheck: %a\n%s" Util.Source.pp info msg
      |> print_endline;
      { stat with fail_typecheck = stat.fail_typecheck + 1 }
  | TestCheckNegErr stat ->
      Format.asprintf "Error: typecheck success" |> print_endline;
      { stat with fail_typecheck = stat.fail_typecheck + 1 }
  | TestCheckRoundtripCheckErr (msg, info, stat) ->
      Format.asprintf "Error on roundtrip re-check: %a\n%s" Util.Source.pp info
        msg
      |> print_endline;
      { stat with fail_typecheck_roundtrip = stat.fail_typecheck_roundtrip + 1 }
  | TestCheckRoundtripMismatchErr stat ->
      Format.asprintf "Error: typecheck roundtrip fail" |> print_endline;
      { stat with fail_typecheck_roundtrip = stat.fail_typecheck_roundtrip + 1 }
  | _ ->
      Format.asprintf "Error: unknown error" |> print_endline;
      { stat with fail_typecheck = stat.fail_typecheck + 1 }

let typecheck_test_driver includes mode testdir =
  let files = collect_files ~suffix:".p4" testdir in
  let total = List.length files in
  let stat = empty_stat in
  Format.asprintf "Running typecheck tests on %d files\n" total |> print_endline;
  let stat =
    List.fold_left
      (fun stat filename -> typecheck_test stat includes mode filename)
      stat files
  in
  log_stat "\nParser on file" stat.fail_parse_file total;
  let total = total - stat.fail_parse_file in
  log_stat "Typecheck" stat.fail_typecheck total;
  let total = total - stat.fail_typecheck in
  if total = 0 then total |> ignore
  else log_stat "Typecheck roundtrip" stat.fail_typecheck_roundtrip total

let typecheck_command =
  Core.Command.basic ~summary:"typecheck test"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map includes = flag "-i" (listed string) ~doc:"include paths"
     and pos = flag "-p" no_arg ~doc:"positive test"
     and neg = flag "-n" no_arg ~doc:"negative test"
     and testdir = anon ("testdir" %: string) in
     let mode =
       match (pos, neg) with
       | false, false | true, false -> Pos
       | false, true -> Neg
       | _ -> failwith "Cannot specify both positive and negative tests"
     in
     fun () -> typecheck_test_driver includes mode testdir)

(* Instantiate test *)

let instantiate stat includes filename =
  try
    let stat, program = typecheck_file stat includes Pos filename in
    let cenv, tdenv, fenv, venv, sto =
      Instance.Instantiate.instantiate_program program
    in
    (stat, cenv, tdenv, fenv, venv, sto)
  with InstErr (msg, info) -> raise (TestInstErr (msg, info, stat))

let instantiate_test stat includes filename =
  try
    let stat, _, _, _, _, sto = instantiate stat includes filename in
    Format.asprintf "Instantiate success: %d objects"
      (Runtime_dynamic.Envs.Sto.cardinal sto)
    |> print_endline;
    stat
  with
  | TestParseFileErr (msg, info, stat) ->
      Format.asprintf "Error on parser: %a\n%s" Util.Source.pp info msg
      |> print_endline;
      { stat with fail_parse_file = stat.fail_parse_file + 1 }
  | TestCheckErr (msg, info, stat) ->
      Format.asprintf "Error on typecheck: %a\n%s" Util.Source.pp info msg
      |> print_endline;
      { stat with fail_typecheck = stat.fail_typecheck + 1 }
  | TestInstErr (msg, info, stat) ->
      Format.asprintf "Error on instantiate: %a\n%s" Util.Source.pp info msg
      |> print_endline;
      { stat with fail_instantiate = stat.fail_instantiate + 1 }
  | _ ->
      Format.asprintf "Error: unknown error" |> print_endline;
      { stat with fail_instantiate = stat.fail_instantiate + 1 }

let instantiate_test_driver includes testdir =
  let files = collect_files ~suffix:".p4" testdir in
  let total = List.length files in
  let stat = empty_stat in
  Format.asprintf "Running instantiate tests on %d files\n" total
  |> print_endline;
  let stat =
    List.fold_left
      (fun stat filename ->
        Format.asprintf "\n>>> Running instantiate test on %s" filename
        |> print_endline;
        instantiate_test stat includes filename)
      stat files
  in
  log_stat "\nParser on file" stat.fail_parse_file total;
  let total = total - stat.fail_parse_file in
  log_stat "Typecheck" stat.fail_typecheck total;
  let total = total - stat.fail_typecheck in
  log_stat "Instantiate" stat.fail_instantiate total

let instantiate_command =
  Core.Command.basic ~summary:"instantiate test"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map includes = flag "-i" (listed string) ~doc:"include paths"
     and testdir = anon ("testdir" %: string) in
     fun () -> instantiate_test_driver includes testdir)

(* Run test *)

let run stat (module Driver : Exec.Driver.DRIVER) includes filename stfname =
  try
    let stat, cenv, tdenv, fenv, venv, sto =
      instantiate stat includes filename
    in
    let stmts_stf = Stf.Parse.parse_file stfname in
    let pass = Driver.run cenv tdenv fenv venv sto stmts_stf in
    if not pass then raise (TestStfErr stat);
    stat
  with
  | StfErr msg -> raise (TestParseStfErr (msg, stat))
  | InterpErr (msg, info) -> raise (TestInterpErr (msg, info, stat))
  | DriverErr msg -> raise (TestDriverErr (msg, stat))

let run_test stat (module Driver : Exec.Driver.DRIVER) includes filename stfname
    =
  try
    let stat = run stat (module Driver) includes filename stfname in
    Format.asprintf "Run success" |> print_endline;
    stat
  with
  | TestParseFileErr (msg, info, stat) ->
      Format.asprintf "Error on parser: %a\n%s" Util.Source.pp info msg
      |> print_endline;
      { stat with fail_parse_file = stat.fail_parse_file + 1 }
  | TestCheckErr (msg, info, stat) ->
      Format.asprintf "Error on typecheck: %a\n%s" Util.Source.pp info msg
      |> print_endline;
      { stat with fail_typecheck = stat.fail_typecheck + 1 }
  | TestInstErr (msg, info, stat) ->
      Format.asprintf "Error on instantiate: %a\n%s" Util.Source.pp info msg
      |> print_endline;
      { stat with fail_instantiate = stat.fail_instantiate + 1 }
  | TestParseStfErr (msg, stat) ->
      Format.asprintf "Error on stf parser:\n%s" msg |> print_endline;
      { stat with fail_run = stat.fail_run + 1 }
  | TestInterpErr (msg, info, stat) ->
      Format.asprintf "Error on run: %a\n%s" Util.Source.pp info msg
      |> print_endline;
      { stat with fail_run = stat.fail_run + 1 }
  | TestDriverErr (msg, stat) ->
      Format.asprintf "Error on driver: %s" msg |> print_endline;
      { stat with fail_run = stat.fail_run + 1 }
  | TestStfErr stat ->
      Format.asprintf "Error on stf test" |> print_endline;
      { stat with fail_run = stat.fail_run + 1 }
  | _ ->
      Format.asprintf "Error: unknown error" |> print_endline;
      { stat with fail_run = stat.fail_run + 1 }

let run_test_with_patch stat (module Driver : Exec.Driver.DRIVER) includes
    filename patchname stfname =
  let stat_next = run_test stat (module Driver) includes filename stfname in
  if eq_stat stat stat_next then stat
  else (
    Format.asprintf ">>>>> Rerunning run test on %s with %s" filename patchname
    |> print_endline;
    run_test stat (module Driver) includes filename patchname)

let run_test_driver includes arch testdir patchdir stfdir =
  let (module Driver) = Exec.Gen.gen arch in
  let module FMap = Map.Make (String) in
  let files = collect_files ~suffix:".p4" testdir in
  let files_map =
    List.fold_left
      (fun files_map file ->
        let file_name = String.split_on_char '/' file |> List.rev |> List.hd in
        let file_name = String.split_on_char '.' file_name |> List.hd in
        FMap.add file_name file files_map)
      FMap.empty files
  in
  let patches =
    Option.map (collect_files ~suffix:".stf") patchdir
    |> Option.value ~default:[]
  in
  let patches_map =
    List.fold_left
      (fun patches_map patch ->
        let patch_name =
          String.split_on_char '/' patch |> List.rev |> List.hd
        in
        let patch_name = String.split_on_char '.' patch_name |> List.hd in
        FMap.add patch_name patch patches_map)
      FMap.empty patches
  in
  let stfs = collect_files ~suffix:".stf" stfdir in
  let tests =
    List.filter_map
      (fun stf ->
        let stf_name = String.split_on_char '/' stf |> List.rev |> List.hd in
        let stf_name = String.split_on_char '.' stf_name |> List.hd in
        if FMap.mem stf_name files_map then
          let file = FMap.find stf_name files_map in
          let patch = FMap.find_opt stf_name patches_map in
          Some (file, patch, stf)
        else (
          Format.asprintf "Cannot find file for %s in %s" stf testdir
          |> print_endline;
          None))
      stfs
  in
  let total = List.length tests in
  let stat = empty_stat in
  Format.asprintf "Running run tests on %d files\n" total |> print_endline;
  let stat =
    List.fold_left
      (fun stat (filename, patchname, stfname) ->
        Format.asprintf "\n>>> Running run test on %s with %s" filename stfname
        |> print_endline;
        match patchname with
        | Some patchname ->
            run_test_with_patch stat
              (module Driver)
              includes filename patchname stfname
        | None -> run_test stat (module Driver) includes filename stfname)
      stat tests
  in
  log_stat "\nParser on file" stat.fail_parse_file total;
  let total = total - stat.fail_parse_file in
  log_stat "Typecheck" stat.fail_typecheck total;
  let total = total - stat.fail_typecheck in
  log_stat "Instantiate" stat.fail_instantiate total;
  let total = total - stat.fail_instantiate in
  log_stat "Run" stat.fail_run total

let run_command =
  Core.Command.basic ~summary:"run test"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map includes = flag "-i" (listed string) ~doc:"include paths"
     and arch = flag "-a" (required string) ~doc:"target architecture"
     and patchdir = flag "-p" (optional string) ~doc:"patch directory"
     and testdir = anon ("testdir" %: string)
     and stfdir = anon ("stfdir" %: string) in
     fun () -> run_test_driver includes arch testdir patchdir stfdir)

let command =
  Core.Command.group ~summary:"p4cherry-test"
    [
      ("parse", parse_command);
      ("typecheck", typecheck_command);
      ("instantiate", instantiate_command);
      ("run", run_command);
    ]

let () = Command_unix.run ~version command
