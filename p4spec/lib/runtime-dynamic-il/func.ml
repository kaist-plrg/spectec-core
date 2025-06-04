open Il.Ast
open Il.Print

(* Function *)

type t = tparam list * clause list

let to_string (tparams, clauses) =
  "def" ^ string_of_tparams tparams ^ "\n"
  ^ String.concat "\n"
      (List.mapi (fun idx clause -> string_of_clause idx clause) clauses)
