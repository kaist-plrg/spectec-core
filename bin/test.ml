let program_dir = "test/program"
let arch_dir = "test/arch"

let (let*) = Option.bind

let parse_file_fails = ref 0
let parse_string_fails = ref 0
let roundtrip_fails = ref 0

let parse_file filename =
  try Frontend.Parse.parse_file arch_dir filename
  with | _ ->
    parse_file_fails := !parse_file_fails + 1;
    Printf.sprintf "Parser fail on file: %s" filename
    |> print_endline;
    None

let parse_string filename file =
  try Frontend.Parse.parse_string filename file
  with | _ ->
    parse_string_fails := !parse_string_fails + 1;
    Printf.sprintf "Parser fail on string: %s" filename
    |> print_endline;
    None

let roundtrip filename =
  let* program = parse_file filename in
  let file' = Syntax.Print.print_program program in
  let* program' = parse_string filename file' in
  if not (Syntax.Eq.eq_program program program') then (
    roundtrip_fails := !roundtrip_fails + 1;
    Printf.sprintf "Roundtrip fail: %s" filename
    |> print_endline;
  );
  Some program'

let () =
  let files = Sys.readdir program_dir in
  let total = Array.length files in
  Printf.sprintf "Running parser roundtrip tests on %d files" total
  |> print_endline;
  Array.iter
    (fun filename ->
      let filename = Printf.sprintf "%s/%s" program_dir filename in
      roundtrip filename |> ignore)
    files;
  Printf.sprintf "Parser fails on file: %d / %d" !parse_file_fails total
  |> print_endline;
  let total = total - !parse_file_fails in
  Printf.sprintf "Parser fails on string: %d / %d" !parse_string_fails total
  |> print_endline;
  let total = total - !parse_string_fails in
  Printf.sprintf "Roundtrip fails: %d / %d" !roundtrip_fails total
  |> print_endline
