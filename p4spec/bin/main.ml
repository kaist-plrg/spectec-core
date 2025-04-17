open Core
open Util.Error

let version = "0.1"

let elab_command =
  Command.basic ~summary:"parse and elaborate a p4_16 spec"
    (let open Command.Let_syntax in
     let open Command.Param in
     let%map filenames = anon (sequence ("filename" %: string)) in
     fun () ->
       try
         let spec = List.concat_map ~f:Frontend.Parse.parse_file filenames in
         let spec_il = Elaborate.Elab.elab_spec spec in
         Format.printf "%s\n" (Il.Print.string_of_spec spec_il);
         ()
       with Error (at, msg) -> Format.printf "%s\n" (string_of_error at msg))

let run_il_command =
  Command.basic
    ~summary:"run static semantics of a p4_16 spec based on backtracking IL"
    (let open Command.Let_syntax in
     let open Command.Param in
     let%map filenames_spec = anon (sequence ("filename" %: string))
     and includes_p4 = flag "-i" (listed string) ~doc:"p4 include paths"
     and filename_p4 = flag "-p" (required string) ~doc:"p4 file to typecheck"
     and debug = flag "-dbg" no_arg ~doc:"print debug traces"
     and profile = flag "-profile" no_arg ~doc:"print execution profile" in
     fun () ->
       try
         let spec =
           List.concat_map ~f:Frontend.Parse.parse_file filenames_spec
         in
         let spec_il = Elaborate.Elab.elab_spec spec in
         let _ =
           Interp_il.Interp.run_typing debug profile spec_il includes_p4
             filename_p4
         in
         ()
       with Error (at, msg) -> Format.printf "%s\n" (string_of_error at msg))

let struct_command =
  Command.basic ~summary:"insert structured control flow to a p4_16 spec"
    (let open Command.Let_syntax in
     let open Command.Param in
     let%map filenames = anon (sequence ("filename" %: string)) in
     fun () ->
       try
         let spec = List.concat_map ~f:Frontend.Parse.parse_file filenames in
         let spec_il = Elaborate.Elab.elab_spec spec in
         let spec_sl = Structure.Struct.struct_spec spec_il in
         Format.printf "%s\n" (Sl.Print.string_of_spec spec_sl);
         ()
       with Error (at, msg) -> Format.printf "%s\n" (string_of_error at msg))

let run_sl_command =
  Command.basic
    ~summary:"run static semantics of a p4_16 spec based on non-backtracking SL"
    (let open Command.Let_syntax in
     let open Command.Param in
     let%map filenames_spec = anon (sequence ("filename" %: string))
     and includes_p4 = flag "-i" (listed string) ~doc:"p4 include paths"
     and filename_p4 =
       flag "-p" (required string) ~doc:"p4 file to typecheck"
     in
     fun () ->
       try
         let spec =
           List.concat_map ~f:Frontend.Parse.parse_file filenames_spec
         in
         let spec_il = Elaborate.Elab.elab_spec spec in
         let spec_sl = Structure.Struct.struct_spec spec_il in
         let _ = Interp_sl.Interp.run_typing spec_sl includes_p4 filename_p4 in
         ()
       with Error (at, msg) -> Format.printf "%s\n" (string_of_error at msg))

let command =
  Command.group
    ~summary:"p4spec: a language design framework for the p4_16 language"
    [
      ("elab", elab_command);
      ("run-il", run_il_command);
      ("struct", struct_command);
      ("run-sl", run_sl_command);
    ]

let () = Command_unix.run ~version command
