let () =
  if Array.length Sys.argv < 4 then (
    Format.printf "Usage: %s <target> <include_dir> <filename>\n" Sys.argv.(0);
    exit 1);

  let target = Sys.argv.(1) in
  let includes = Sys.argv.(2) in
  let filename = Sys.argv.(3) in

  Format.printf "Parsing %s with includes %s\n" filename includes;
  let program =
    match Frontend.Parse.parse_file includes filename with
    | Some program -> program
    | None -> failwith "Error while parsing."
  in

  Format.printf "Desugaring %s\n" filename;
  let program = Syntax.Desugar.desugar_program program in

  Format.printf "Instantiating %s\n" filename;
  let ccenv, gctx = Instance_.Instantiate.instantiate_program program in

  let arch =
    match target with
    | "v1model" -> Exec_.V1model.drive
    | "custom" -> Exec_.Custom.drive
    | _ -> failwith "Unknown target"
  in

  Format.printf "Interpreting %s\n" filename;
  arch ccenv gctx |> ignore;
  ()
