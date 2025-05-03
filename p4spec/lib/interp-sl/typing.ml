open Sl.Ast
module Hint = Runtime_static.Rel.Hint
module Typ = Runtime_dynamic_sl.Typ
module Value = Runtime_dynamic_sl.Value
module Rel = Runtime_dynamic_sl.Rel
module Dep = Runtime_testgen.Dep
module SCov = Runtime_testgen.Cov.Single
module MCov = Runtime_testgen.Cov.Multiple
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
  | WellTyped of Dep.Graph.t option * vid option * SCov.Cover.t
  | IllTyped of region * string * SCov.Cover.t
  | IllFormed of string * SCov.Cover.t

let run_typing_internal (spec : spec) (filename_p4 : string)
    (value_program : value) : res =
  Builtin.init ();
  Dep.Graph.refresh ();
  let ctx = Ctx.empty spec filename_p4 false in
  try
    let ctx, _values = do_typing ctx spec value_program in
    WellTyped (ctx.graph, ctx.vid_program, !(ctx.cover))
  with
  | Util.Error.InterpError (at, msg) -> IllTyped (at, msg, !(ctx.cover))
  | _ -> IllTyped (no_region, "unknown error", !(ctx.cover))

let run_typing ?(derive : bool = false) (spec : spec)
    (includes_p4 : string list) (filename_p4 : string) : res =
  Builtin.init ();
  Dep.Graph.refresh ();
  let ctx = Ctx.empty spec filename_p4 derive in
  try
    let value_program = Convert.In.in_program ctx includes_p4 filename_p4 in
    let ctx = Ctx.set_vid_program ctx value_program.note.vid in
    let ctx, _values = do_typing ctx spec value_program in
    WellTyped (ctx.graph, ctx.vid_program, !(ctx.cover))
  with
  | Util.Error.ConvertInError msg -> IllFormed (msg, !(ctx.cover))
  | Util.Error.InterpError (at, msg) -> IllTyped (at, msg, !(ctx.cover))

(* Entry point : Measure spec coverage of phantom nodes *)

let cover_typings (spec : spec) (includes_p4 : string list)
    (filenames_p4 : string list) : MCov.Cover.t =
  let cover_multi = MCov.init spec in
  List.fold_left
    (fun cover_multi filename_p4 ->
      let cover_single =
        match run_typing spec includes_p4 filename_p4 with
        | WellTyped (_, _, cover_single) -> cover_single
        | IllTyped (_, _, cover_single) -> cover_single
        | IllFormed (_, cover_single) -> cover_single
      in
      MCov.extend cover_multi filename_p4 cover_single)
    cover_multi filenames_p4
