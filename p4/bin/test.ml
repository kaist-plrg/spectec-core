let version = "0.1"

type stat = {
  mutable fail_file : int;
  mutable fail_string : int;
  mutable fail_roundtrip : int;
  mutable fail_typecheck : int;
}

type mode = Pos | Neg

let log_stat name fails total : unit =
  let passes = total - fails in
  let pass_rate = float_of_int passes /. float_of_int total *. 100.0 in
  let fail_rate = float_of_int fails /. float_of_int total *. 100.0 in
  Printf.sprintf "%s: [PASS] %d/%d (%.2f%%) [FAIL] %d/%d (%.2f%%)" name passes
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
  let program = Frontend.Parse.parse_file includes filename in
  match program with
  | Ok program -> Ok (stat, program)
  | Error msg ->
      stat.fail_file <- stat.fail_file + 1;
      Error (stat, msg)

let parse_string stat filename file =
  let program = Frontend.Parse.parse_string filename file in
  match program with
  | Ok program -> Ok (stat, program)
  | Error msg ->
      stat.fail_string <- stat.fail_string + 1;
      Error (stat, msg)

let parse_roundtrip stat includes filename =
  match parse_file stat includes filename with
  | Error (stat, msg) ->
      Printf.sprintf "Error while parsing file: %s" msg |> print_endline;
      stat
  | Ok (stat, program) -> (
      let file' = Format.asprintf "%a\n" El.Pp.pp_program program in
      match parse_string stat filename file' with
      | Error (stat, msg) ->
          Printf.sprintf "Error while parsing string %s: %s" filename msg
          |> print_endline;
          stat
      | Ok (stat, program') ->
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
      (fun stat filename ->
        Printf.sprintf "\n>>> Running parser roundtrip test on %s" filename
        |> print_endline;
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
  match parse_file stat includes filename with
  | Error (stat, msg) ->
      Printf.sprintf "Error while parsing file: %s" msg |> print_endline;
      stat
  | Ok (stat, program) -> (
      let on_success stat filename =
        Printf.sprintf "Typecheck success: %s" filename |> print_endline;
        if mode = Neg then stat.fail_typecheck <- stat.fail_typecheck + 1;
        stat
      in
      let on_error stat msg =
        Printf.sprintf "Error while typechecking: %s" msg |> print_endline;
        if mode = Pos then stat.fail_typecheck <- stat.fail_typecheck + 1;
        stat
      in
      try
        let program = Typing.Typecheck.type_program program in
        match program with
        | Ok _ -> on_success stat filename
        | Error msg -> on_error stat msg
      with _ -> on_error stat "crash")

let typecheck_test includes mode testdir =
  let files = collect_files testdir in
  let total = List.length files in
  let stat =
    { fail_file = 0; fail_string = 0; fail_roundtrip = 0; fail_typecheck = 0 }
  in
  Printf.sprintf "Running typecheck tests on %d files\n" total |> print_endline;
  let stat =
    List.fold_left
      (fun stat filename ->
        Printf.sprintf "\n>>> Running typecheck test on %s" filename
        |> print_endline;
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
