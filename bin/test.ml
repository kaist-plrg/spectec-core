let version = "0.1"

type stat = {
  mutable fail_file : int;
  mutable fail_string : int;
  mutable fail_roundtrip : int;
  mutable fail_typecheck : int;
}

type mode = Pos | Neg

let log_stat name fails total : unit =
  Printf.sprintf "%s: [PASS] %d [FAIL] %d [TOTAL] %d" name (total - fails) fails
    total
  |> print_endline

let collect_files testdir =
  let files = Sys_unix.readdir testdir in
  Array.sort String.compare files;
  let files = Array.to_list files in
  List.filter (String.ends_with ~suffix:".p4") files

(* Parser roundtrip test *)

let parse_file stat includes filename =
  let program =
    try Frontend.Parse.parse_file includes filename with _ -> None
  in
  if Option.is_none program then (
    stat.fail_file <- stat.fail_file + 1;
    Printf.sprintf "Error while parsing p4 file: %s" filename |> print_endline);
  (stat, program)

let parse_string stat filename file =
  let program =
    try Frontend.Parse.parse_string filename file with _ -> None
  in
  if Option.is_none program then (
    stat.fail_string <- stat.fail_string + 1;
    Printf.sprintf "Error while parsing p4 string: %s" filename |> print_endline);
  (stat, program)

let parse_roundtrip stat includes filename =
  let stat, program = parse_file stat includes filename in
  match program with
  | None -> stat
  | Some program -> (
      let file' = Format.asprintf "%a\n" El.Pp.pp_program program in
      let stat, program' = parse_string stat filename file' in
      match program' with
      | None -> stat
      | Some program' ->
          if El.Eq.eq_program program program' then
            Printf.sprintf "Parser roundtrip success: %s" filename
            |> print_endline
          else (
            stat.fail_roundtrip <- stat.fail_roundtrip + 1;
            Printf.sprintf "Parser roundtrip fail: %s" filename |> print_endline);
          stat)

let parse_test includes testdir =
  let files = collect_files testdir in
  let total = List.length files in
  let stat =
    { fail_file = 0; fail_string = 0; fail_roundtrip = 0; fail_typecheck = 0 }
  in
  Printf.sprintf "Running parser roundtrip tests on %d files\n" total
  |> print_endline;
  let stat =
    List.fold_left
      (fun stat file ->
        let filename = Printf.sprintf "%s/%s" testdir file in
        parse_roundtrip stat includes filename)
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
     fun () -> parse_test includes testdir)

(* Typecheck test *)

let typecheck stat includes mode filename =
  let stat, program = parse_file stat includes filename in
  match program with
  | None -> stat
  | Some program -> (
      try
        Typing.Typecheck.type_program program |> ignore;
        if mode = Neg then stat.fail_typecheck <- stat.fail_typecheck + 1;
        Printf.sprintf "Typecheck success: %s" filename |> print_endline;
        stat
      with _ ->
        if mode = Pos then stat.fail_typecheck <- stat.fail_typecheck + 1;
        Printf.sprintf "Error while typechecking p4 file: %s" filename
        |> print_endline;
        stat)

let typecheck_test includes mode testdir =
  let files = collect_files testdir in
  let total = List.length files in
  let stat =
    { fail_file = 0; fail_string = 0; fail_roundtrip = 0; fail_typecheck = 0 }
  in
  Printf.sprintf "Running typecheck tests on %d files\n" total |> print_endline;
  let stat =
    List.fold_left
      (fun stat file ->
        let filename = Printf.sprintf "%s/%s" testdir file in
        typecheck stat includes mode filename)
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
     fun () -> typecheck_test includes mode testdir)

let command =
  Core.Command.group ~summary:"p4cherry-test"
    [ ("parse", parse_command); ("typecheck", typecheck_command) ]

let () = Command_unix.run ~version command
