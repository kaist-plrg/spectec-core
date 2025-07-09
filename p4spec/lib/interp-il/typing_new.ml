open Il.Ast
module Value = Runtime_dynamic.Value
module Cache = Runtime_dynamic.Cache.Cache
module F = Format
open Attempt
open Util.Source

let do_typing (ctx : Ctx.t) (spec : spec) (value_program : value) :
    Ctx.t * value list =
  let ctx = Interp.load_spec ctx spec in
  let+ ctx, values =
    Interp.invoke_rel ctx ("Prog_ok" $ no_region) [ value_program ]
  in
  (ctx, values)

(* Entry point : Run typing rule *)

type res = WellTyped | IllTyped of region * string | IllFormed of string

let run_typing' ?(debug : bool = false) (spec : spec)
    (includes_p4 : string list) (filename_p4 : string) : res =
  Builtin.init ();
  Value.refresh ();
  Cache.reset !Interp.func_cache;
  Cache.reset !Interp.rule_cache;
  try
    let value_program = Parsing.Parse.parse_file includes_p4 filename_p4 in
    let ctx = Ctx.empty ~debug filename_p4 in
    let _ = do_typing ctx spec value_program in
    WellTyped
  with
  | Util.Error.ConvertInError msg -> IllFormed msg
  | Util.Error.InterpError (at, msg) -> IllTyped (at, msg)

let run_typing ?(debug : bool = false) (spec : spec) (includes_p4 : string list)
    (filename_p4 : string) : res =
  run_typing' ~debug spec includes_p4 filename_p4
