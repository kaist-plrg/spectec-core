let () =
  if Array.length Sys.argv < 4 then (
    Printf.printf "Usage: %s <target> <include_dir> <filename>\n" Sys.argv.(0);
    exit 1);

  let target = Sys.argv.(1) in
  let includes = Sys.argv.(2) in
  let filename = Sys.argv.(3) in

  Printf.sprintf "Parsing %s with includes %s" filename includes
  |> print_endline;
  let program =
    match Frontend.Parse.parse_file includes filename with
    | Some program -> program
    | None -> failwith "Error while parsing."
  in

  Printf.sprintf "Instantiating %s" filename |> print_endline;
  let tdenv, ccenv, store = Instance.Instantiate.instantiate_program program in

  if target = "v1model" then (
    Printf.sprintf "Interpreting %s" filename |> print_endline;
    Exec.V1model.drive tdenv ccenv store |> ignore);
  ()
