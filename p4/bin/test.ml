open Util.Error

let version = "0.1"

type stat = {
  mutable fail_file : int;
  mutable fail_string : int;
  mutable fail_roundtrip : int;
  mutable fail_typecheck : int;
  mutable fail_instantiate : int;
}

let empty_stat =
  {
    fail_file = 0;
    fail_string = 0;
    fail_roundtrip = 0;
    fail_typecheck = 0;
    fail_instantiate = 0;
  }

type mode = Pos | Neg

exception TestParseFileErr of string * Util.Source.info * stat
exception TestParseStringErr of string * Util.Source.info * stat
exception TestParseRoundtripErr of stat
exception TestCheckErr of string * Util.Source.info * stat
exception TestCheckNegErr of stat

let log_stat name fails total : unit =
  let passes = total - fails in
  let pass_rate = float_of_int passes /. float_of_int total *. 100.0 in
  let fail_rate = float_of_int fails /. float_of_int total *. 100.0 in
  Format.asprintf "%s: [PASS] %d/%d (%.2f%%) [FAIL] %d/%d (%.2f%%)" name passes
    total pass_rate fails total fail_rate
  |> print_endline

let rec collect_files testdir =
  let files = Sys_unix.readdir testdir in
  Array.sort String.compare files;
  Array.fold_left
    (fun files file ->
      let filename = testdir ^ "/" ^ file in
      if Sys_unix.is_directory_exn filename && file <> "include" then
        files @ collect_files filename
      else if String.ends_with ~suffix:".p4" filename then files @ [ filename ]
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
      Format.asprintf "Error on file: %a\n%s" Util.Source.pp info msg
      |> print_endline;
      stat.fail_file <- stat.fail_file + 1;
      stat
  | TestParseStringErr (msg, info, stat) ->
      Format.asprintf "Error on string: %a\n%s" Util.Source.pp info msg
      |> print_endline;
      stat.fail_string <- stat.fail_string + 1;
      stat
  | TestParseRoundtripErr stat ->
      Format.asprintf "Error: parser roundtrip fail" |> print_endline;
      stat.fail_roundtrip <- stat.fail_roundtrip + 1;
      stat

let parse_test_driver includes testdir =
  let files = collect_files testdir in
  let total = List.length files in
  let stat = empty_stat in
  Format.asprintf "Running parser roundtrip tests on %d files\n" total
  |> print_endline;
  let stat =
    List.fold_left
      (fun stat filename -> parse_test stat includes filename)
      stat files
  in
  log_stat "\nParser on file" stat.fail_file total;
  let total = total - stat.fail_file in
  log_stat "Parser on string" stat.fail_string total;
  let total = total - stat.fail_string in
  log_stat "Parser roundtrip" stat.fail_roundtrip total

let parse_command =
  Core.Command.basic ~summary:"parser roundtrip test"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map includes = flag "-i" (listed string) ~doc:"include paths"
     and testdir = anon ("testdir" %: string) in
     fun () -> parse_test_driver includes testdir)

(* Typecheck test *)

let typecheck stat includes mode filename =
  try
    let stat, program = parse_file stat includes filename in
    let _program = Typing.Typecheck.type_program program in
    if mode = Neg then raise (TestCheckNegErr stat);
    stat
  with CheckErr (msg, info) -> raise (TestCheckErr (msg, info, stat))

let typecheck_test stat includes mode filename =
  try
    let stat = typecheck stat includes mode filename in
    Format.asprintf "Typecheck success" |> print_endline;
    stat
  with
  | TestParseFileErr (msg, info, stat) ->
      Format.asprintf "Error on file: %a\n%s" Util.Source.pp info msg
      |> print_endline;
      stat.fail_file <- stat.fail_file + 1;
      stat
  | TestCheckErr (msg, info, stat) ->
      Format.asprintf "Error on typecheck: %a\n%s" Util.Source.pp info msg
      |> print_endline;
      stat.fail_typecheck <- stat.fail_typecheck + 1;
      stat
  | TestCheckNegErr stat ->
      Format.asprintf "Error: typecheck success" |> print_endline;
      stat.fail_typecheck <- stat.fail_typecheck + 1;
      stat
  | _ ->
      Format.asprintf "Error: unknown error" |> print_endline;
      stat.fail_typecheck <- stat.fail_typecheck + 1;
      stat

let typecheck_test_driver includes mode testdir =
  let files = collect_files testdir in
  let total = List.length files in
  let stat = empty_stat in
  Format.asprintf "Running typecheck tests on %d files\n" total |> print_endline;
  let stat =
    List.fold_left
      (fun stat filename ->
        Format.asprintf "\n>>> Running typecheck test on %s" filename
        |> print_endline;
        typecheck_test stat includes mode filename)
      stat files
  in
  log_stat "\nParser on file" stat.fail_file total;
  let total = total - stat.fail_file in
  log_stat "Typecheck" stat.fail_typecheck total

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
  let stat, program = parse_file stat includes filename in
  let program = Typing.Typecheck.type_program program in
  let venv, fenv, sto = Instance.Instantiate.instantiate_program program in
  (stat, venv, fenv, sto)

let instantiate_test stat includes filename =
  try
    let stat, _venv, _fenv, sto = instantiate stat includes filename in
    Format.asprintf "Instantiate success: %d objects"
      (Runtime_dynamic.Envs.Sto.cardinal sto)
    |> print_endline;
    stat
  with
  | TestParseFileErr (msg, info, stat) ->
      Format.asprintf "Error on file: %a\n%s" Util.Source.pp info msg
      |> print_endline;
      stat.fail_file <- stat.fail_file + 1;
      stat
  | TestCheckErr (msg, info, stat) ->
      Format.asprintf "Error on typecheck: %a\n%s" Util.Source.pp info msg
      |> print_endline;
      stat.fail_typecheck <- stat.fail_typecheck + 1;
      stat
  | _ ->
      Format.asprintf "Error: unknown error" |> print_endline;
      stat.fail_instantiate <- stat.fail_instantiate + 1;
      stat

let instantiate_test_driver includes testdir =
  let files = collect_files testdir in
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
  log_stat "\nParser on file" stat.fail_file total;
  let total = total - stat.fail_file in
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

let command =
  Core.Command.group ~summary:"p4cherry-test"
    [
      ("parse", parse_command);
      ("typecheck", typecheck_command);
      ("instantiate", instantiate_command);
    ]

let () = Command_unix.run ~version command
