let program_dir = "test/program"
let arch_dir = "test/arch"

let (let*) = Option.bind

let parse_file filename =
  try Frontend.Parse.parse_file arch_dir filename
  with | _ ->
    Printf.sprintf "Parser fail on file: %s" filename
    |> print_endline;
    None

let parse_string filename file =
  try Frontend.Parse.parse_string filename file
  with | _ ->
    Printf.sprintf "Parser fail on string: %s" filename
    |> print_endline;
    None

let roundtrip filename =
  let* program = parse_file filename in
  let file' = Syntax.Print.print_program program in
  let* program' = parse_string filename file' in
  if not (Syntax.Eq.eq_program program program') then
    Printf.sprintf "Roundtrip fail: %s" filename
    |> print_endline;
    None

let () =
  let files = Sys.readdir program_dir in
  Printf.sprintf "Running parser roundtrip tests on %d files" (Array.length files)
  |> print_endline;
  Array.iter
    (fun filename ->
      let filename = Printf.sprintf "%s/%s" program_dir filename in
      roundtrip filename |> ignore)
    files
