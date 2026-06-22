(** Impty interpreter negative test harness.

    Invoked as [main.exe SPEC_DIR FILE]: parse + elaborate the spec, typecheck
    [FILE] through the IL interpreter, render any diagnostics to stderr in plain
    (non-ANSI) form, and exit 1 if any were emitted. [SPEC_DIR] is an argument
    rather than hardcoded so the [closure] variant stays reachable; the impty
    CLI only exposes [base]. *)

module D = Spectec.Diagnostic

let ( let* ) = Result.bind

let run spec_dir file =
  let spec_files = Test_lib.Files.collect ~suffix:".spectec" spec_dir in
  let* spec = Spectec.parse_spec_files spec_files in
  let* spec_il = Spectec.elaborate spec in
  let input =
    { Targets_impty.Impty.filename = file; expect = Spectec.Task.Negative }
  in
  Spectec.eval_task_with_instrumentation
    (module Targets_impty.Impty.Typecheck)
    ~sl_mode:false ~spec_il input
  |> Result.map ignore

let () =
  let spec_dir = Sys.argv.(1) in
  let file = Sys.argv.(2) in
  let _, bag = Spectec.with_diagnostics (fun () -> run spec_dir file) in
  prerr_string (D.Render.render_bag ~ansi:D.Ansi.plain bag);
  if D.Bag.is_empty bag then exit 0 else exit 1
