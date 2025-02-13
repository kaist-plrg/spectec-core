open Core
open Util.Error

let version = "0.1"

let parse_command =
  Command.basic ~summary:"parse a p4_16 spec"
    (let open Command.Let_syntax in
     let open Command.Param in
     let%map filenames = anon (sequence ("filename" %: string)) in
     fun () ->
       try
         let spec = List.concat_map ~f:Frontend.Parse.parse_file filenames in
         Format.printf "%s\n" (El.Print.string_of_spec spec);
         let spec_il = Elaboration.Elab.elab_spec spec in
         Format.printf "=== AFTER ELABORATION ===\n";
         Format.printf "%s\n" (Il.Print.string_of_spec spec_il);
         ()
       with Error (at, msg) -> Format.printf "%s\n" (string_of_error at msg))

let command =
  Command.group
    ~summary:"p4spec: a language design framework for the p4_16 language"
    [ ("parse", parse_command) ]

let () = Command_unix.run ~version command
