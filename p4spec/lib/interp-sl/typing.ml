open Domain.Lib
open Sl.Ast
module Hint = Runtime_static.Rel.Hint
module Typ = Runtime_dynamic_sl.Typ
module Value = Runtime_dynamic_sl.Value
module Rel = Runtime_dynamic_sl.Rel
module Dep = Runtime_testgen.Dep
module Ignore = Runtime_testgen.Cov.Ignore
module SCov = Runtime_testgen.Cov.Single
module MCov = Runtime_testgen.Cov.Multiple
module Cache = Cache.Cache
open Error
module F = Format
open Util.Source

(* Entry point *)

let do_typing (ctx : Ctx.t) (spec : spec) (value_program : value) :
    Ctx.t * value list =
  let ctx = Interp.load_spec ctx spec in
  match Interp.invoke_rel ctx ("Prog_ok" $ no_region) [ value_program ] with
  | Some (ctx, values) -> (ctx, values)
  | None -> error no_region "relation was not matched"

(* Entry point : Run typing rule *)

type res =
  | WellTyped of Dep.Graph.t * vid * SCov.Cover.t
  | IllTyped of region * string * SCov.Cover.t
  | IllFormed of string * SCov.Cover.t

let run_typing_internal (spec : spec) (filename_p4 : string)
    (value_program : value) (ignores : IdSet.t) : res =
  Builtin.init ();
  Dep.Graph.refresh ();
  Cache.reset !Interp.func_cache;
  Cache.reset !Interp.rule_cache;
  let graph = Dep.Graph.empty () in
  let cover = ref (SCov.init ignores spec) in
  try
    let ctx =
      Ctx.empty ~derive:false filename_p4 graph value_program.note.vid cover
    in
    let ctx, _values = do_typing ctx spec value_program in
    WellTyped (ctx.graph, ctx.vid_program, !(ctx.cover))
  with Util.Error.InterpError (at, msg) -> IllTyped (at, msg, !cover)

let run_typing' ?(mini : bool = false) ?(derive : bool = false) (spec : spec)
    (includes_p4 : string list) (filename_p4 : string) (ignores : IdSet.t) : res
    =
  Builtin.init ();
  Dep.Graph.refresh ();
  Cache.reset !Interp.func_cache;
  Cache.reset !Interp.rule_cache;
  let graph = Dep.Graph.init () in
  let cover = ref (SCov.init ignores spec) in
  try
    let value_program =
      if mini then Convert.In_mini.in_program graph includes_p4 filename_p4
      else Convert.In.in_program graph includes_p4 filename_p4
    in
    let ctx =
      Ctx.empty ~derive filename_p4 graph value_program.note.vid cover
    in
    let ctx, _values = do_typing ctx spec value_program in
    WellTyped (ctx.graph, ctx.vid_program, !(ctx.cover))
  with
  | Util.Error.ConvertInError msg -> IllFormed (msg, !cover)
  | Util.Error.InterpError (at, msg) -> IllTyped (at, msg, !cover)

let run_typing ?(mini : bool = false) ?(derive : bool = false) (spec : spec)
    (includes_p4 : string list) (filename_p4 : string)
    (filenames_ignore : string list) : res =
  let ignores = Ignore.init filenames_ignore in
  run_typing' ~mini ~derive spec includes_p4 filename_p4 ignores

(* Entry point : Measure spec coverage of phantom nodes *)

let cover_typings ?(mini : bool = false) (spec : spec)
    (includes_p4 : string list) (filenames_p4 : string list)
    (filenames_ignore : string list) : MCov.Cover.t =
  let ignores = Ignore.init filenames_ignore in
  let cover_multi = MCov.init ignores spec in
  List.fold_left
    (fun cover_multi filename_p4 ->
      let wellformed, welltyped, cover_single =
        match run_typing' ~mini spec includes_p4 filename_p4 ignores with
        | WellTyped (_, _, cover_single) -> (true, true, cover_single)
        | IllTyped (_, _, cover_single) -> (true, false, cover_single)
        | IllFormed (_, cover_single) -> (false, false, cover_single)
      in
      MCov.extend cover_multi filename_p4 wellformed welltyped cover_single)
    cover_multi filenames_p4
