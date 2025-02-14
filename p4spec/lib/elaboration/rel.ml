open El.Ast

type t = nottyp * Il.Ast.rule list

let to_string (nottyp, rules) =
  El.Print.string_of_nottyp nottyp
  ^ " =\n"
  ^ String.concat "\n   " (List.map Il.Print.string_of_rule rules)
