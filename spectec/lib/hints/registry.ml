(* Hint tag registry *)

type kind = Alter | Input | Fields | Precedence
type subject = Typcase | Typfield | Rel | Func | Var
type entry = { tag : string; kind : kind; subjects : subject list }

let table : entry list =
  [
    { tag = "input"; kind = Input; subjects = [ Rel ] };
    { tag = "print"; kind = Alter; subjects = [ Typcase ] };
    { tag = "tighter_than"; kind = Precedence; subjects = [ Typcase ] };
    { tag = "fields"; kind = Fields; subjects = [ Typcase ] };
    { tag = "prose"; kind = Alter; subjects = [ Rel; Func; Typcase ] };
    { tag = "prose_in"; kind = Alter; subjects = [ Rel; Func ] };
    { tag = "prose_out"; kind = Alter; subjects = [ Rel ] };
    { tag = "prose_true"; kind = Alter; subjects = [ Rel; Func ] };
    { tag = "prose_false"; kind = Alter; subjects = [ Rel; Func ] };
  ]

let lookup (tag : string) : entry option =
  List.find_opt (fun entry -> entry.tag = tag) table

let string_of_subject (s : subject) : string =
  match s with
  | Typcase -> "type case"
  | Typfield -> "type field"
  | Rel -> "relation"
  | Func -> "function"
  | Var -> "variable"

let string_of_subjects (subjects : subject list) : string =
  String.concat ", " (List.map string_of_subject subjects)
