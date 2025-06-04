open Il.Ast
open Util.Source

let ctr = ref 0

(* dec $fresh_tid() : tid *)

let fresh_tid (at : region) (targs : targ list) (values_input : value list) :
    value =
  Extract.zero at targs;
  Extract.zero at values_input;
  let tid = "FRESH__" ^ string_of_int !ctr in
  let value_tid = TextV tid in
  ctr := !ctr + 1;
  value_tid
