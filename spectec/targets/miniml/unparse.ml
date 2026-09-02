open Format
open Lang.Il

let flatten = Value.flatten_case_v

let rec print_expr fmt e =
  match flatten e with
  | _, [ "Eapp" ], [ e1; e2 ] ->
      fprintf fmt "@[%a@ %a@]" print_atom e1 print_atom e2
  | _, [ "Efun" ], [ x; e ] ->
      fprintf fmt "@[fun %s ->@ %a@]" (Value.get_text x) print_expr e
  | _, [ "Elet" ], [ x; e1; e2 ] ->
      fprintf fmt "@[let %s = %a in@ %a@]" (Value.get_text x) print_expr e1
        print_expr e2
  | _ -> print_atom fmt e

and print_atom fmt e =
  match flatten e with
  | _, [ "Ecst" ], [ n ] ->
      let n = Xl.Num.to_int (Value.get_num n) in
      fprintf fmt "%s" (Bigint.to_string n)
  | _, [ "Eop" ], [ o ] -> (
      match flatten o with
      | _, [ "ADD" ], _ -> fprintf fmt "+"
      | _, [ "SUB" ], _ -> fprintf fmt "-"
      | _, [ "IFZ" ], _ -> fprintf fmt "ifz"
      | _, [ "FST" ], _ -> fprintf fmt "fst"
      | _, [ "SND" ], _ -> fprintf fmt "snd"
      | _ -> failwith "unexpected operator")
  | _, [ "Evar" ], [ x ] -> fprintf fmt "%s" (Value.get_text x)
  | _, [ "Epair" ], [ e1; e2 ] ->
      fprintf fmt "(@[%a,@ %a@])" print_expr e1 print_expr e2
  | _, [ ("Eapp" | "Efun" | "Elet") ], _ -> fprintf fmt "(@[%a@])" print_expr e
  | _ -> fprintf fmt "<print_expr: unexpected term>"

let print_expr (e : Value.t) : string = kasprintf (fun s -> s) "%a" print_expr e

let unparse ~spec:_ (values : Value.t list) : string =
  match values with
  | [ e ] -> print_expr e
  | _ -> failwith "unparse expects a single expression"
