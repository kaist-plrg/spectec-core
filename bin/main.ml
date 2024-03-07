let () =
  if Array.length Sys.argv < 3 then (
    Printf.printf "Usage: %s <include_dir> <filename>\n" Sys.argv.(0);
    exit 1
  );
  
  let includes = Sys.argv.(1) in
  let filename = Sys.argv.(2) in

  Printf.sprintf "Parsing %s with includes %s" filename includes
  |> print_endline;
  let program =
    match Frontend.Parse.parse_file includes filename with
    | Some program -> program
    | None -> failwith "Error while parsing."
  in

  Printf.sprintf "Instantiating %s" filename
  |> print_endline;
  let _ = Instance.Instantiate.instantiate_program program in
  ()
