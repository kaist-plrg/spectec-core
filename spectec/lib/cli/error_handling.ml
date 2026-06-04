let resolve_ansi : Cli_args.color -> Spectec.Diagnostic.Ansi.t = function
  | Always -> Spectec.Diagnostic.Ansi.color
  | Never -> Spectec.Diagnostic.Ansi.plain
  | Auto -> Spectec.Diagnostic.Ansi.auto ~tty:(Unix.isatty Unix.stderr)

let guard ~color ~on_ok f =
  let ansi = resolve_ansi color in
  let result, bag = Spectec.with_diagnostics f in
  let combined =
    match result with
    | Ok _ -> bag
    | Error e ->
        Spectec.Diagnostic.Bag.merge bag (Spectec.Error.to_diagnostics e)
  in
  if not (Spectec.Diagnostic.Bag.is_empty combined) then
    Printf.eprintf "%s\n%!"
      (Spectec.Diagnostic.Render.render_bag ~ansi combined);
  match result with Ok v -> on_ok v | Error _ -> exit 1

let guard_unit ~color f = guard ~color ~on_ok:ignore f

let guard_errors_only ~color f =
  let ansi = resolve_ansi color in
  let result, _bag = Spectec.with_diagnostics f in
  match result with
  | Ok () -> ()
  | Error e ->
      Printf.eprintf "%s\n%!"
        (Spectec.Diagnostic.Render.render_bag ~ansi
           (Spectec.Error.to_diagnostics e));
      exit 1
