open Il.Ast
module Value = Runtime_dynamic.Value
module Cache = Runtime_dynamic.Cache
module F = Format
open Attempt
open Util.Source

let do_typing (ctx : Ctx.t) (spec : spec) (value_program : value) :
    Ctx.t * value list =
  let ctx = Interp.load_spec ctx spec in
  let+ ctx, values =
    Interp.invoke_rel ctx ("Program_ok" $ no_region) [ value_program ]
  in
  (ctx, values)

(* Entry point : Run typing rule *)

type res = WellTyped | IllTyped of region * string | IllFormed of string

let run_typing' ?(debug : bool = false) ?(profile : bool = false) (spec : spec)
    (includes_p4 : string list) (filename_p4 : string) : res =
  Builtin.init ();
  Value.refresh ();
  Cache.Cache.clear !Interp.func_cache;
  Cache.Cache.clear !Interp.rule_cache;
  try
    let value_program = Parsing.Parse.parse_file includes_p4 filename_p4 in
    let ctx = Ctx.empty ~debug ~profile filename_p4 in
    let ctx, _ = do_typing ctx spec value_program in
    Ctx.profile ctx;
    WellTyped
  with
  | Util.Error.ParseError (_, msg) -> IllFormed msg
  | Util.Error.InterpError (at, msg) -> IllTyped (at, msg)

let run_typing ?(debug : bool = false) ?(profile : bool = false) (spec : spec)
    (includes_p4 : string list) (filename_p4 : string) : res =
  run_typing' ~debug ~profile spec includes_p4 filename_p4
