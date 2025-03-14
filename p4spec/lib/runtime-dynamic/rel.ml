open Il.Ast
open Il.Print

(* Relation *)

type t = nottyp * Runtime_static.Rel.Hint.t * rule list

let to_string (nottyp, inputs, rules) =
  string_of_nottyp nottyp ^ " "
  ^ Runtime_static.Rel.Hint.to_string inputs
  ^ " =\n"
  ^ String.concat "\n   " (List.map string_of_rule rules)
