open Syntax
open Ast

let extract_base (value: Value.t): Value.base =
  match value with
  | Value.Base bvalue -> bvalue
  | _ ->
      Printf.sprintf
        "Not a base value: %s"
        (Value.print_value value)
      |> failwith

let eval_unop' (op: Op.un) (bvalue: Value.base): Value.base =
  match op, bvalue with
  | UMinus _, AInt i ->
      AInt (-i)
  | UMinus _, Int { value; width } ->
      Int { value = -value; width }
  | _ ->
      Printf.sprintf
        "(TODO: eval_unop) op: %s, value: %s"
        (Print.print_unop op) (Value.print_base_value bvalue)
      |> failwith

let eval_unop (op: Op.un) (value: Value.t): Value.t =
  let bvalue = eval_unop' op (extract_base value) in 
  Value.Base bvalue
