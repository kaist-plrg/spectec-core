open Il.Ast
module Value = Runtime_dynamic.Value
open Util.Source

let ctr = ref 0

(* dec $fresh_tid() : tid *)

let fresh_tid (at : region) (targs : targ list) (values_input : value list) :
    value =
  Extract.zero at targs;
  Extract.zero at values_input;
  let tid = "FRESH__" ^ string_of_int !ctr in
  ctr := !ctr + 1;
  let value =
    let vid = Value.fresh () in
    let typ = Il.Ast.VarT ("tid" $ no_region, []) in
    TextV tid $$$ { vid; typ }
  in
  value
