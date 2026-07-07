(** EL pretty-printer roundtrip test.

    For each [.spectec] file under the spec directory:
    - Print a header [;; === <path> ===].
    - Parse the file, pretty-print it via [Lang.El.Unparse], and dump the
      result.
    - Reparse the printed text and assert AST equality (modulo positions).

    The dumped unparser output is the test's golden artifact: any aesthetic
    change in [Lang.El.Unparse] shows up as a [.expected] diff. Roundtrip
    property failures (parse or AST mismatch) are emitted as [;; FAIL: ...]
    markers inline, and the test exits non-zero. *)

open Core
open Test_lib
module Unparse = Lang.El.Unparse
module Eq = Lang.El.Eq

let exit_code = ref 0

let fail msg =
  exit_code := 1;
  Printf.printf ";; FAIL: %s\n" msg

let render_error e =
  Spectec.Diagnostic.Render.render_bag ~ansi:Spectec.Diagnostic.Ansi.plain
    (Spectec.Error.to_diagnostics e)

let dump_one (file : string) ~label : unit =
  Printf.printf ";; === %s ===\n" label;
  match Spectec.parse_spec_files [ file ] with
  | Error e -> fail (render_error e)
  | Ok spec1 -> (
      let printed = Unparse.string_of_spec spec1 in
      print_string printed;
      match
        Spectec.parse_spec_source
          Spectec.{ filename = "<roundtrip>"; contents = printed }
      with
      | Error e -> fail (Printf.sprintf "reparse: %s" (render_error e))
      | Ok spec2 ->
          if not (Eq.eq_spec spec1 spec2) then
            fail "AST mismatch after roundtrip")

let relative_to ~root path =
  let prefix = root ^ "/" in
  if String.is_prefix path ~prefix then
    String.drop_prefix path (String.length prefix)
  else path

let run specdir =
  let files = Files.collect ~suffix:".spectec" specdir in
  let files = List.sort files ~compare:String.compare in
  List.iter files ~f:(fun file ->
      dump_one file ~label:(relative_to ~root:specdir file));
  if !exit_code <> 0 then exit !exit_code

let command =
  Command.basic ~summary:"EL pretty-printer roundtrip test"
  @@
  let open Command.Let_syntax in
  let open Command.Param in
  let%map specdir = flag "-s" (required string) ~doc:"DIR spec directory" in
  fun () -> run specdir

let () = Command_unix.run command
