(** Splice test - verifies the AsciiDoc prose spliced from a fixed spec and
    skeleton, so changes to structuring or prose rendering surface as a diff. *)

open Core
open Test_lib

let run specdir skeleton output =
  let open Core.Result.Let_syntax in
  let result =
    let spec_files = Files.collect ~suffix:".spectec" specdir in
    let%bind spec = Spectec.parse_spec_files spec_files in
    let%bind { lang; _ } = Spectec.elaborate spec in
    let spec_sl = Spectec.structure lang in
    let henv = Spectec.henv_of_el_spec spec in
    let henv = Spectec.henv_with_il_spec henv lang in
    let spec_pl = Spectec.annotate ~henv spec_sl |> Spectec.shorten in
    let _report =
      Splice.Driver.run ~spec_el:spec ~spec_pl
        ~source_entries:Splice.Registry.source
        ~prose_entries:Splice.Registry.prose
        ~filenames:[ (skeleton, output) ]
    in
    Ok ()
  in
  match result with
  | Ok () -> ()
  | Error err ->
      Out_channel.write_all output
        ~data:
          (Format.asprintf "Splice failed:\n%s\n"
             (Spectec.Diagnostic.Render.render_bag
                ~ansi:Spectec.Diagnostic.Ansi.plain
                (Spectec.Error.to_diagnostics err)))

let command =
  Command.basic ~summary:"run splice test"
  @@
  let open Command.Let_syntax in
  let open Command.Param in
  let%map specdir = flag "-s" (required string) ~doc:"DIR spec directory"
  and skeleton = flag "-i" (required string) ~doc:"FILE skeleton .adoc"
  and output = flag "-o" (required string) ~doc:"FILE spliced output" in
  fun () -> run specdir skeleton output

let () = Command_unix.run command
