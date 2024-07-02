let program_dir = "test/program"
let arch_dir = "test/arch"
let ( let* ) = Option.bind
let parse_file_fails = ref 0
let parse_string_fails = ref 0
let parse_roundtrip_fails = ref 0
let desugar_fails = ref 0
let desugar_roundtrip_fails = ref 0
let instantiate_fails = ref 0

let reset () =
  parse_file_fails := 0;
  parse_string_fails := 0;
  parse_roundtrip_fails := 0;
  desugar_fails := 0;
  desugar_roundtrip_fails := 0;
  instantiate_fails := 0

let parse_file filename =
  try
    let program = Frontend.Parse.parse_file arch_dir filename in
    program |> Option.get |> ignore;
    program
  with _ ->
    parse_file_fails := !parse_file_fails + 1;
    Printf.sprintf "Parser fail on file: %s" filename |> print_endline;
    None

let parse_string filename file =
  try
    let program = Frontend.Parse.parse_string filename file in
    program |> Option.get |> ignore;
    program
  with _ ->
    parse_string_fails := !parse_string_fails + 1;
    Printf.sprintf "Parser fail on string: %s" filename |> print_endline;
    None

let parse_roundtrip filename =
  let* program = parse_file filename in
  let file' = Surface.Print.print_program program in
  let* program' = parse_string filename file' in
  if not (Surface.Eq.eq_program program program') then (
    parse_roundtrip_fails := !parse_roundtrip_fails + 1;
    Printf.sprintf "parse_roundtrip fail: %s" filename |> print_endline);
  Some program'

let test_parser () =
  let files = Sys.readdir program_dir in
  let total = Array.length files in
  Printf.sprintf "Running parser roundtrip tests on %d files" total
  |> print_endline;
  Array.iter
    (fun filename ->
      let filename = Printf.sprintf "%s/%s" program_dir filename in
      parse_roundtrip filename |> ignore)
    files;
  Printf.sprintf "Parser fails on file: %d / %d" !parse_file_fails total
  |> print_endline;
  let total = total - !parse_file_fails in
  Printf.sprintf "Parser fails on string: %d / %d" !parse_string_fails total
  |> print_endline;
  let total = total - !parse_string_fails in
  Printf.sprintf "Roundtrip fails: %d / %d" !parse_roundtrip_fails total
  |> print_endline

let desugar_program filename program =
  try Some (Frontend.Desugar.desugar_program program)
  with _ ->
    desugar_fails := !desugar_fails + 1;
    Printf.sprintf "Desugar fail on file: %s" filename |> print_endline;
    None

let desugar_roundtrip filename =
  let* program = parse_file filename in
  let* program' = desugar_program filename program in
  let file' = Format.asprintf "%a" Syntax.Pp.pp_program program' in
  let* program'' = parse_string filename file' in
  if not (Surface.Eq.eq_program program program'') then (
    desugar_roundtrip_fails := !desugar_roundtrip_fails + 1;
    Printf.sprintf "Desugar roundtrip fail: %s" filename |> print_endline;
    None)
  else Some program'

let test_desugar () =
  let files = Sys.readdir program_dir in
  let total = Array.length files in
  Printf.sprintf "Running desugar roundtrip tests on %d files" total
  |> print_endline;
  Array.iter
    (fun filename ->
      let filename = Printf.sprintf "%s/%s" program_dir filename in
      desugar_roundtrip filename |> ignore)
    files;
  Printf.sprintf "Parser fails on file: %d / %d" !parse_file_fails total
  |> print_endline;
  let total = total - !parse_file_fails in
  Printf.sprintf "Desugar fails: %d / %d" !desugar_fails total |> print_endline;
  let total = total - !desugar_fails in
  Printf.sprintf "Parser fails on string: %d / %d" !parse_string_fails total
  |> print_endline;
  let total = total - !parse_string_fails in
  Printf.sprintf "Desugar roundtrip fails: %d / %d" !desugar_roundtrip_fails
    total
  |> print_endline

let instantiate filename =
  let* program = parse_file filename in
  let* program = desugar_program filename program in
  try Some (Instance.Instantiate.instantiate_program program)
  with e ->
    instantiate_fails := !instantiate_fails + 1;
    Printf.sprintf "Instantiation fail: %s due to %s" filename
      (Printexc.to_string e)
    |> print_endline;
    None

let test_instantiation () =
  let files = Sys.readdir program_dir in
  let total = Array.length files in
  Printf.sprintf "Running instantiation tests on %d files" total
  |> print_endline;
  Array.iter
    (fun filename ->
      let filename = Printf.sprintf "%s/%s" program_dir filename in
      instantiate filename |> ignore)
    files;
  Printf.sprintf "Parser fails on file: %d / %d" !parse_file_fails total
  |> print_endline;
  let total = total - !parse_file_fails in
  Printf.sprintf "Desugar fails: %d / %d" !desugar_fails total |> print_endline;
  let total = total - !desugar_fails in
  Printf.sprintf "Instantiation fails: %d / %d" !instantiate_fails total
  |> print_endline

type parsed_args = { parse : bool; desugar : bool; instantiate : bool }

let rec parse_arguments args (parsed_args : parsed_args) =
  match args with
  | [] -> parsed_args
  | "-parse" :: tl -> { parsed_args with parse = true } |> parse_arguments tl
  | "-desugar" :: tl ->
      { parsed_args with desugar = true } |> parse_arguments tl
  | "-instantiate" :: tl ->
      { parsed_args with instantiate = true } |> parse_arguments tl
  | _ :: tl -> parse_arguments tl parsed_args

let () =
  let args = Array.to_list Sys.argv in
  let parsed_args =
    { parse = false; desugar = false; instantiate = false }
    |> parse_arguments args
  in
  if parsed_args.parse then (
    test_parser ();
    reset ())
  else ();
  if parsed_args.desugar then (
    test_desugar ();
    reset ())
  else ();
  if parsed_args.instantiate then (
    test_instantiation ();
    reset ())
  else ()
