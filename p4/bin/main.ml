open Core
open Util.Error

let version = "0.1"

let parse includes filename : El.Ast.program =
  Frontend.Parse.parse_file includes filename

let roundtrip_el includes filename : El.Ast.program =
  Frontend.Parse.roundtrip_file includes filename

let typecheck includes filename : Il.Ast.program =
  parse includes filename |> Typing.Typecheck.type_program

let roundtrip_il includes filename : Il.Ast.program =
  let program = typecheck includes filename in
  let program_str = Format.asprintf "%a\n" Il.Pp_to_el.pp_program program in
  let program' =
    try
      Frontend.Parse.parse_string filename program_str
      |> Typing.Typecheck.type_program
    with ParseErr (msg, info) ->
      Format.sprintf "re-parse error: %s" msg |> error_parser_info info
  in
  if not (Il.Eq.eq_program program program') then
    "roundtrip error" |> error_parser_no_info;
  program'

let parse_command =
  Command.basic ~summary:"parse a p4_16 program"
    (let open Command.Let_syntax in
     let open Command.Param in
     let%map includes = flag "-i" (listed string) ~doc:"include paths"
     and roundtrip_flag =
       flag "-r" no_arg ~doc:"parse, stringify, and parse the program"
     and filename = anon ("file.p4" %: string) in
     fun () ->
       try
         let program =
           let func = if roundtrip_flag then roundtrip_el else parse in
           func includes filename
         in
         Format.printf "%a\n" El.Pp.pp_program program
       with ParseErr (msg, info) ->
         if Util.Source.is_no_info info then Format.printf "Error: %s\n" msg
         else Format.printf "Error: %a\n%s\n" Util.Source.pp info msg)

let parse_debug_command =
  Command.basic ~summary:"parse a p4_16 program into OCaml ASt"
    (let open Command.Let_syntax in
     let open Command.Param in
     let%map includes = flag "-i" (listed string) ~doc:"include paths"
     and filename = anon ("file.p4" %: string) in
     fun () ->
       try
         let program = parse includes filename in
         Format.printf "%s\n" (El.Mp.mp_program program)
       with ParseErr (msg, info) ->
         if Util.Source.is_no_info info then Format.printf "Error: %s\n" msg
         else Format.printf "Error: %a\n%s\n" Util.Source.pp info msg)

let typecheck_command =
  Command.basic ~summary:"typecheck a p4_16 program"
    (let open Command.Let_syntax in
     let open Command.Param in
     let%map includes = flag "-i" (listed string) ~doc:"include paths"
     and roundtrip_flag =
       flag "-r" no_arg ~doc:"typecheck, stringify, and typecheck the program"
     and filename = anon ("file.p4" %: string) in
     fun () ->
       try
         let program =
           let func = if roundtrip_flag then roundtrip_il else typecheck in
           func includes filename
         in
         Format.printf "%a\n" Il.Pp_to_el.pp_program program
       with ParseErr (msg, info) | CheckErr (msg, info) ->
         if Util.Source.is_no_info info then Format.printf "Error: %s\n" msg
         else Format.printf "Error: %a\n%s\n" Util.Source.pp info msg)

let instantiate_command =
  Command.basic ~summary:"instantiate a p4_16 program"
    (let open Command.Let_syntax in
     let open Command.Param in
     let%map includes = flag "-i" (listed string) ~doc:"include paths"
     and filename = anon ("file.p4" %: string) in
     fun () ->
       try
         let program = typecheck includes filename in
         Instance.Instantiate.instantiate_program program |> ignore
       with
       | ParseErr (msg, info) | CheckErr (msg, info) | InstErr (msg, info) ->
         if Util.Source.is_no_info info then Format.printf "Error: %s\n" msg
         else Format.printf "Error: %a\n%s\n" Util.Source.pp info msg)

let run_command =
  Command.basic ~summary:"run a p4_16 program"
    (let open Command.Let_syntax in
     let open Command.Param in
     let%map includes = flag "-i" (listed string) ~doc:"include paths"
     and arch = flag "-a" (required string) ~doc:"target architecture"
     and filename = anon ("file.p4" %: string)
     and stfname = anon ("file.stf" %: string) in
     fun () ->
       try
         let program = typecheck includes filename in
         let cenv, tdenv, fenv, venv, sto =
           Instance.Instantiate.instantiate_program program
         in
         let (module Driver) = Exec.Gen.gen arch in
         let stmts_stf = Stf.Parse.parse_file stfname in
         Driver.run cenv tdenv fenv venv sto stmts_stf |> ignore
       with
       | ParseErr (msg, info)
       | CheckErr (msg, info)
       | InstErr (msg, info)
       | InterpErr (msg, info) ->
           if Util.Source.is_no_info info then Format.printf "Error: %s\n" msg
           else Format.printf "Error: %a\n%s\n" Util.Source.pp info msg
       | DriverErr msg -> Format.printf "Error: %s\n" msg
       | StfErr msg -> Format.printf "Error: %s\n" msg)

let command =
  Command.group ~summary:"p4cherry: an interpreter of the p4_16 language"
    [
      ("parse", parse_command);
      ("parse-debug", parse_debug_command);
      ("typecheck", typecheck_command);
      ("instantiate", instantiate_command);
      ("run", run_command);
    ]

let () = Command_unix.run ~version command
