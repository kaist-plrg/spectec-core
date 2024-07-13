open Core

let version = "0.1"

let parse includes filename =
  match Frontend.Parse.parse_file includes filename with
  | Some program -> program
  | None -> failwith "Error while parsing p4."

let desugar includes filename =
  let program = parse includes filename in
  Frontend.Desugar.desugar_program program

let typecheck includes filename =
  let program = desugar includes filename in
  Typing.Typecheck.type_program program

let instantiate includes filename =
  let program = typecheck includes filename in
  Instance.Instantiate.instantiate_program program

let interp arch includes filename stf debug =
  let ccenv, sto, ctx = instantiate includes filename in
  let (module Driver) =
    let open Exec in
    match arch with
    | "v1model" ->
        (module Driver.Make (V1model.Make) (Interp.Make) : Driver.DRIVER)
    | _ -> failwith "Unknown target: target = v1model | custom"
  in
  let stf =
    match Stf.Parse.parse_file stf with
    | Some stf -> stf
    | None -> failwith "Error while parsing stf."
  in
  let config = Exec.Config.{ debug } in
  Driver.configure config;
  Driver.run ccenv sto ctx stf |> ignore;
  ()

let parse_command =
  Command.basic ~summary:"parse a p4_16 program"
    (let open Command.Let_syntax in
     let open Command.Param in
     let%map includes = flag "-i" (listed string) ~doc:"include paths"
     and filename = anon ("file.p4" %: string) in
     fun () ->
       let program = parse includes filename in
       Format.printf "%s\n" (Surface.Print.print_program program))

let desugar_command =
  Command.basic ~summary:"desugar a p4_16 program (AST transformation)"
    (let open Command.Let_syntax in
     let open Command.Param in
     let%map includes = flag "-i" (listed string) ~doc:"include paths"
     and filename = anon ("file.p4" %: string) in
     fun () ->
       let program = desugar includes filename in
       Format.printf "%a\n" Syntax.Pp.pp_program program)

let instantiate_command =
  Command.basic ~summary:"instantiate a p4_16 program"
    (let open Command.Let_syntax in
     let open Command.Param in
     let%map includes = flag "-i" (listed string) ~doc:"include paths"
     and filename = anon ("file.p4" %: string) in
     fun () ->
       let _ccenv, sto, _ctx = instantiate includes filename in
       Format.printf "%a\n" Runtime.Object.Sto.pp sto)

let interp_command =
  Command.basic ~summary:"interpret a p4_16 program"
    (let open Command.Let_syntax in
     let open Command.Param in
     let%map arch = flag "-a" (required string) ~doc:"architecture"
     and includes = flag "-i" (listed string) ~doc:"include paths"
     and filename = anon ("file.p4" %: string)
     and stf = anon ("file.stf" %: string)
     and debug = flag "-d" no_arg ~doc:"debug" in
     fun () -> interp arch includes filename stf debug)

let command =
  Command.group ~summary:"p4cherry: an interpreter of the p4_16 language"
    [
      ("parse", parse_command);
      ("desugar", desugar_command);
      ("instantiate", instantiate_command);
      ("interp", interp_command);
    ]

let () = Command_unix.run ~version command
