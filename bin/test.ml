let program_dir = "test/program"
let arch_dir = "test/arch"

let parse_file filename =
  try Frontend.Parse.parse_file arch_dir filename
  with | _ -> None

let parse_string filename file =
  try Frontend.Parse.parse_string filename file
  with | _ -> None

let print_and_parse filename program =
  let file = program |> Syntax.Print.print_program in
  parse_string filename file

let () =
  let files = Sys.readdir program_dir in
  let total = Array.length files in
  Array.iteri
    (fun count filename ->
      let filename = program_dir ^ "/" ^ filename in
      match parse_file filename with
      | None ->
          Printf.sprintf "Parser fail: [%d/%d] %s" (count + 1) total filename
          |> print_endline
      | Some program ->
          match print_and_parse filename program with
          | None ->
              Printf.sprintf "Roundtrip fail: [%d/%d] %s" (count + 1) total filename
              |> print_endline
          | Some _ -> ())
    files;
