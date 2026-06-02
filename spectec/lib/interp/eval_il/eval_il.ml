open Lang.Il
module F = Format
open Attempt
open Common.Source

let run_relation (filename : string) (builtins : Builtins.t) (cache : Cache.t)
    (spec : spec) (rid : id') (values : value list) : Ctx.t * value list =
  let ctx = Interp.load_spec filename builtins cache spec in
  let+ ctx, values = Interp.invoke_rel ctx (rid $ no_region) values in
  (ctx, values)

let run_relation_fresh (filename : string) (builtins : Builtins.t)
    (cache : Cache.t) (spec : spec) (rid : id') (values : value list) :
    Ctx.t * value list =
  Cache.clear cache;
  run_relation filename builtins cache spec rid values

type error = Plain of region * string | Backtrack of failtrace list

let run (module T : Target.S) (spec : spec) (rid : string) (values : value list)
    (filename : string) : (Ctx.t * value list, error) result =
  let builtins = Builtins.make T.builtins in
  let cache =
    Cache.make ~is_impure_func:T.is_impure_func ~is_impure_rel:T.is_impure_rel
      ~state_version:T.state_version
  in
  let inner () =
    run_relation_fresh filename builtins cache spec rid values |> Result.ok
  in
  try T.handler inner with
  | Error.InterpError (at, msg) -> Error (Plain (at, msg))
  | Error.BacktrackError failtraces -> Error (Backtrack failtraces)

let error_to_diagnostic = function
  | Plain (at, msg) -> Diag.error ~source:"il-interp" at msg
  | Backtrack failtraces ->
      Diag.of_failtraces ~source:"il-interp" ~fallback:"evaluation failed"
        (prune_failtraces failtraces)

module Ctx = Ctx
