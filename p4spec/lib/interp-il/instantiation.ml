open Il.Ast
module Value = Runtime_dynamic.Value
module Cache = Runtime_dynamic.Cache
module F = Format
open Attempt
open Util.Source

let do_instantiation (ctx : Ctx.t) (_spec : spec) (value_program_ir : value) :
    Ctx.t * value list =
  let+ ctx, values =
    Interp.invoke_rel ctx ("Program_inst" $ no_region) [ value_program_ir ]
  in
  (ctx, values)

(* Entry point : Run instantiation rule *)

type res =
  | Success
  | InstError of region * string
  | IllTyped of region * string
  | IllFormed of region * string

type typing_result =
  | WellTyped of Ctx.t * value list
  | IllTyped of region * string
  | IllFormed of region * string

let run_instantiation' ?(debug : bool = false) ?(profile : bool = false)
    (spec : spec) (includes_p4 : string list) (filename_p4 : string) : res =
  Builtin.init ();
  Value.refresh ();
  Cache.Cache.clear !Interp.func_cache;
  Cache.Cache.clear !Interp.rule_cache;
  let typing_result =
    try
      let value_program = Parsing.Parse.parse_file includes_p4 filename_p4 in
      let ctx = Ctx.empty ~debug ~profile filename_p4 in
      let ctx, values = Typing_concrete.do_typing ctx spec value_program in
      WellTyped (ctx, values)
    with
    | Util.Error.ParseError (at, msg) -> IllFormed (at, msg)
    | Util.Error.InterpError (at, msg) -> IllTyped (at, msg)
  in
  match typing_result with
  | WellTyped (ctx, values) -> (
      try
        let _ = do_instantiation ctx spec (List.nth values 1) in
        Success
      with Util.Error.InterpError (at, msg) -> InstError (at, msg))
  | IllTyped (at, msg) -> IllTyped (at, msg)
  | IllFormed (at, msg) -> IllFormed (at, msg)

let run_instantiation ?(debug : bool = false) ?(profile : bool = false)
    (spec : spec) (includes_p4 : string list) (filename_p4 : string) : res =
  run_instantiation' ~debug ~profile spec includes_p4 filename_p4
