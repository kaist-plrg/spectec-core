(** Annotate test - verifies prose rendering from SL through PL *)

open Core
open Test_lib

let run specdir =
  let open Core.Result.Let_syntax in
  let spec_pl =
    let spec_files = Files.collect ~suffix:".spectec" specdir in
    let%bind spec = Spectec.parse_spec_files spec_files in
    let%bind spec_il = Spectec.elaborate spec in
    let spec_sl = Spectec.structure spec_il in
    let henv = Spectec.henv_of_el_spec spec in
    let henv = Spectec.henv_with_il_spec henv spec_il in
    let spec_pl = Spectec.annotate ~henv spec_sl |> Spectec.shorten in
    Ok spec_pl
  in
  match spec_pl with
  | Error err ->
      Format.printf "Annotation failed:\n%s\n"
        (Spectec.Diagnostic.Render.render_bag
           ~ansi:Spectec.Diagnostic.Ansi.plain
           (Spectec.Error.to_diagnostics err))
  | Ok spec_pl -> Format.printf "%s\n" (Lang.Pl.Render.render_spec spec_pl)

let command =
  Command.basic ~summary:"run annotate test"
  @@
  let open Command.Let_syntax in
  let open Command.Param in
  let%map specdir = flag "-s" (required string) ~doc:"DIR spec directory" in
  fun () -> run specdir

let () = Command_unix.run command
