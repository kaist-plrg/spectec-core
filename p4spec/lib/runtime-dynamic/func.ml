open Il.Ast
open Il.Print

(* Function *)

type t = tparam list * param list * typ * clause list

let to_string (tparams, params, typ, clauses) =
  "def " ^ string_of_tparams tparams ^ string_of_params params ^ " : "
  ^ string_of_typ typ ^ " =\n"
  ^ String.concat "\n" (List.map string_of_clause clauses)
