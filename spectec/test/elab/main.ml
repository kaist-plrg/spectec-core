(** Elaboration test - verifies spec elaboration from EL to IL *)

open Core
open Test_lib

let run specdir =
  let open Core.Result.Let_syntax in
  let spec_il =
    let spec_files = Files.collect ~suffix:".spectec" specdir in
    let%bind spec = Spectec.parse_spec_files spec_files in
    let%bind spec_il = Spectec.elaborate spec in
    Ok spec_il
  in
  match spec_il with
  | Ok spec_il -> Format.printf "%s\n" (Lang.Il.Print.string_of_spec spec_il)
  | Error err ->
      Format.printf "Elaboration failed:\n%s\n"
        (Spectec.Diagnostic.Render.render_bag
           ~ansi:Spectec.Diagnostic.Ansi.plain
           (Spectec.Error.to_diagnostics err));
      exit 1

let command =
  Command.basic ~summary:"run elaboration test"
  @@
  let open Command.Let_syntax in
  let open Command.Param in
  let%map specdir = flag "-s" (required string) ~doc:"DIR spec directory" in
  fun () -> run specdir

let () = Command_unix.run command
