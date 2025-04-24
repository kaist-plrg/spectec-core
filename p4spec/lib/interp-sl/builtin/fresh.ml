open Sl.Ast
module Dep = Runtime_testgen.Dep
open Util.Source

let ctr = ref 0

(* dec $fresh_tid() : tid *)

let fresh_tid (ctx : Ctx.t) (at : region) (targs : targ list)
    (values_input : value list) : value =
  Extract.zero at targs;
  Extract.zero at values_input;
  let tid = "FRESH__" ^ string_of_int !ctr in
  ctr := !ctr + 1;
  let value = TextV tid $$$ Dep.Graph.fresh () in
  Ctx.add_node ctx value;
  value
