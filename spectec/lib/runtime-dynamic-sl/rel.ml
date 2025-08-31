open Sl.Ast
open Sl.Print

(* Relation *)

type t = Runtime_static.Rel.Hint.t * exp list * instr list

let to_string (inputs, exps, instrs) =
  Runtime_static.Rel.Hint.to_string inputs
  ^ string_of_exps ", " exps ^ "\n\n" ^ string_of_instrs instrs
