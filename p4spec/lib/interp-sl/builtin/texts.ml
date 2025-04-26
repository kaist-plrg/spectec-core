open Xl
open Sl.Ast
module Value = Runtime_dynamic_sl.Value
module Dep = Runtime_testgen.Dep
open Util.Source

(* dec $int_to_text(int) : text *)

let int_to_text (ctx : Ctx.t) (at : region) (targs : targ list)
    (values_input : value list) : value =
  Extract.zero at targs;
  let num = Extract.one at values_input |> Value.get_num in
  let value =
    let vid = Dep.Graph.fresh () in
    let typ = Il.Ast.TextT in
    TextV (Num.string_of_num num) $$$ { vid; typ }
  in
  Ctx.add_node ctx value;
  value

(* dec $strip_prefix(text, text) : text *)

let strip_prefix (ctx : Ctx.t) (at : region) (targs : targ list)
    (values_input : value list) : value =
  Extract.zero at targs;
  let value_text, value_prefix = Extract.two at values_input in
  let text = Value.get_text value_text in
  let prefix = Value.get_text value_prefix in
  assert (String.starts_with ~prefix text);
  let text =
    String.sub text (String.length prefix)
      (String.length text - String.length prefix)
  in
  let value =
    let vid = Dep.Graph.fresh () in
    let typ = Il.Ast.TextT in
    TextV text $$$ { vid; typ }
  in
  Ctx.add_node ctx value;
  value

(* dec $strip_suffix(text, text) : text *)

let strip_suffix (ctx : Ctx.t) (at : region) (targs : targ list)
    (values_input : value list) : value =
  Extract.zero at targs;
  let value_text, value_suffix = Extract.two at values_input in
  let text = Value.get_text value_text in
  let suffix = Value.get_text value_suffix in
  assert (String.ends_with ~suffix text);
  let text = String.sub text 0 (String.length text - String.length suffix) in
  let value =
    let vid = Dep.Graph.fresh () in
    let typ = Il.Ast.TextT in
    TextV text $$$ { vid; typ }
  in
  Ctx.add_node ctx value;
  value
