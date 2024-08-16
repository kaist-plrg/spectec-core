open Core

let version = "0.1"

let parse includes filename =
  match Frontend.Parse.parse_file includes filename with
  | Some program -> program
  | None -> failwith "Error while parsing p4."

let typecheck includes filename =
  let program = parse includes filename in
  Typing.Typecheck.type_program program

let parse_command =
  Command.basic ~summary:"parse a p4_16 program"
    (let open Command.Let_syntax in
     let open Command.Param in
     let%map includes = flag "-i" (listed string) ~doc:"include paths"
     and filename = anon ("file.p4" %: string) in
     fun () ->
       let program = parse includes filename in
       Format.printf "%a\n" El.Pp.pp_program program)

let typecheck_command =
  Command.basic ~summary:"typecheck a p4_16 program"
    (let open Command.Let_syntax in
     let open Command.Param in
     let%map includes = flag "-i" (listed string) ~doc:"include paths"
     and filename = anon ("file.p4" %: string) in
     fun () ->
       let ctx = typecheck includes filename in
       Format.printf "%a\n" Typing.Ctx.pp ctx)

let command =
  Command.group ~summary:"p4cherry: an interpreter of the p4_16 language"
    [ ("parse", parse_command); ("typecheck", typecheck_command) ]

let () = Command_unix.run ~version command
