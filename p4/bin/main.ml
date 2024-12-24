open Core
open Util.Error

let version = "0.1"

let parse includes filename : El.Ast.program res =
  Frontend.Parse.parse_file includes filename

let roundtrip includes filename : El.Ast.program res =
  Frontend.Parse.roundtrip_file includes filename

let typecheck includes filename : Il.Ast.program res =
  let program = parse includes filename in
  Result.bind ~f:Typing.Typecheck.type_program program

let parse_command =
  Command.basic ~summary:"parse a p4_16 program"
    (let open Command.Let_syntax in
     let open Command.Param in
     let%map includes = flag "-i" (listed string) ~doc:"include paths"
     and roundtrip_flag =
       flag "-r" no_arg ~doc:"parse, stringify, and parse the program"
     and filename = anon ("file.p4" %: string) in
     fun () ->
       let program =
         let func = if roundtrip_flag then roundtrip else parse in
         func includes filename
       in
       match program with
       | Ok program -> Format.printf "%a\n" El.Pp.pp_program program
       | Error (msg, info) ->
           Format.printf "Error: %a\n%s\n" Util.Source.pp info msg)

let typecheck_command =
  Command.basic ~summary:"typecheck a p4_16 program"
    (let open Command.Let_syntax in
     let open Command.Param in
     let%map includes = flag "-i" (listed string) ~doc:"include paths"
     and filename = anon ("file.p4" %: string) in
     fun () ->
       let program = typecheck includes filename in
       match program with
       | Ok program -> Format.printf "%a\n" Il.Pp.pp_program program
       | Error (msg, info) ->
           Format.printf "Error: %a\n%s\n" Util.Source.pp info msg)

let command =
  Command.group ~summary:"p4cherry: an interpreter of the p4_16 language"
    [ ("parse", parse_command); ("typecheck", typecheck_command) ]

let () = Command_unix.run ~version command
