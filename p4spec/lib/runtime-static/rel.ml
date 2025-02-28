open Il.Ast

(* Input hints for rules *)

module Hint = struct
  type t = int list

  let to_string t =
    Format.asprintf "hint(input %s)"
      (String.concat " " (List.map (fun idx -> "%" ^ string_of_int idx) t))

  let split_exps (hint : t) (exps : exp list) :
      (int * exp) list * (int * exp) list =
    exps
    |> List.mapi (fun idx exp -> (idx, exp))
    |> List.partition (fun (idx, _) -> List.mem idx hint)

  let combine_exps (exps_input : (int * exp) list)
      (exps_output : (int * exp) list) : exp list =
    exps_input @ exps_output
    |> List.sort (fun (idx_i, _) (idx_o, _) -> compare idx_i idx_o)
    |> List.map snd
end

(* Relation *)

type t = El.Ast.nottyp * Hint.t * Il.Ast.rule list

let to_string (nottyp, inputs, rules) =
  El.Print.string_of_nottyp nottyp
  ^ " hint(input "
  ^ String.concat ", " (List.map string_of_int inputs)
  ^ ") =\n"
  ^ String.concat "\n   " (List.map Il.Print.string_of_rule rules)
