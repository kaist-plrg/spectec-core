open Sl.Ast
open Sl.Print

(* Function *)

type t = tparam list * arg list * instr list

let to_string (tparams, args, instrs) =
  "def " ^ string_of_tparams tparams ^ string_of_args args ^ " :\n\n"
  ^ string_of_instrs instrs
