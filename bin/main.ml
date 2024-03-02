let () =
  if Array.length Sys.argv < 3 then (
    Printf.printf "Usage: %s <include_dir> <filename>\n" Sys.argv.(0);
    exit 1
  );
  
  let includes = Sys.argv.(1) in
  let filename = Sys.argv.(2) in
  let program = Frontend.Parse.parse_file includes filename in
  match program with
  | Some program -> print_endline (Syntax.Print.print_program program)
  | None -> print_endline "Error"
