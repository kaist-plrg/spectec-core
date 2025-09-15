open Sl.Ast
module Value = Runtime_dynamic.Value
module Cache = Runtime_dynamic.Cache
open Error
module F = Format
open Util.Source

let do_typing (ctx : Ctx.t) (spec : spec) (value_program : value) :
    Ctx.t * value list =
  let ctx = Interp.load_spec ctx spec in
  match Interp.invoke_rel ctx ("Prog_ok" $ no_region) [ value_program ] with
  | Some (ctx, values) -> (ctx, values)
  | None -> error no_region "relation was not matched"

(* Entry point : Run typing rule *)

type res = WellTyped | IllTyped of region * string | IllFormed of string

let run_typing_internal (spec : spec) (filename_p4 : string)
    (value_program : value) : res =
  Builtin.init ();
  Value.refresh ();
  Cache.Cache.clear !Interp.func_cache;
  Cache.Cache.clear !Interp.rule_cache;
  try
    let ctx = Ctx.empty filename_p4 in
    let _ctx, _values = do_typing ctx spec value_program in
    WellTyped
  with Util.Error.InterpError (at, msg) -> IllTyped (at, msg)

let run_typing' (spec : spec) (includes_p4 : string list) (filename_p4 : string)
    : res =
  Builtin.init ();
  Value.refresh ();
  Cache.Cache.clear !Interp.func_cache;
  Cache.Cache.clear !Interp.rule_cache;
  try
    let value_program = P4.Parse.parse_file includes_p4 filename_p4 in
    let ctx = Ctx.empty filename_p4 in
    let _ctx, _values = do_typing ctx spec value_program in
    WellTyped
  with
  | Util.Error.ConvertInError msg -> IllFormed msg
  | Util.Error.InterpError (at, msg) -> IllTyped (at, msg)

let run_typing (spec : spec) (includes_p4 : string list) (filename_p4 : string)
    : res =
  run_typing' spec includes_p4 filename_p4
