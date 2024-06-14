let () =
  if Array.length Sys.argv < 4 then (
    Format.printf "Usage: %s <target> <include_dir> <filename>\n" Sys.argv.(0);
    exit 1);

  let arch = Sys.argv.(1) in
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
  let ccenv, sto, ctx = Instance.Instantiate.instantiate_program program in

  let (module Driver) =
    let open Exec in
    match arch with
    | "v1model" ->
        (module Driver.Make (V1model.Make) (Interp.Make) : Driver.DRIVER)
    | "custom" ->
        (module Driver.Make (Custom.Make) (Interp.Make) : Driver.DRIVER)
    | _ -> failwith "Unknown target: target = v1model | custom"
  in

  Format.printf "Interpreting %s\n" filename;
  Driver.run ccenv sto ctx |> ignore;
  ()
