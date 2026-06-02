open Common.Source

type t =
  | PassError of Pass.error
  | InterpError of Interp.error
  | UnhandledException of string
  | TaskParseError of region * string
  | RoundtripError of region * string
  | SpecMismatchError of string * string
  | DirectoryError of string
  | ConfigError of region * string
  | QuickcheckError of string

let to_diagnostics = function
  | PassError e -> Pass.error_to_diagnostics e
  | InterpError e -> Diag.Bag.singleton (Interp.error_to_diagnostic e)
  | UnhandledException msg ->
      Diag.Bag.singleton
        (Diag.error ~source:"internal" Common.Source.no_region
           ("Unhandled exception: " ^ msg))
  | TaskParseError (at, msg) ->
      Diag.Bag.singleton (Diag.error ~source:"task-parse" at msg)
  | RoundtripError (at, msg) ->
      Diag.Bag.singleton (Diag.error ~source:"roundtrip" at msg)
  | SpecMismatchError (expected, actual) ->
      Diag.Bag.singleton
        (Diag.error ~source:"config" Common.Source.no_region
           (Printf.sprintf
              "Spec version mismatch: expected spec hash %s but got %s."
              expected actual))
  | DirectoryError msg ->
      Diag.Bag.singleton
        (Diag.error ~source:"config" Common.Source.no_region msg)
  | ConfigError (at, msg) ->
      Diag.Bag.singleton (Diag.error ~source:"config" at msg)
  | QuickcheckError msg ->
      Diag.Bag.singleton
        (Diag.error ~source:"quickcheck" Common.Source.no_region msg)
