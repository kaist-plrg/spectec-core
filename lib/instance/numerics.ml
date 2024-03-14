open Syntax
open Ast
open Runtime

(* Utils *)

let extract_base (value: Value.t): Value.base =
  match value with
  | Value.Base bvalue -> bvalue
  | _ ->
      Printf.sprintf
        "Not a base value: %s"
        (Value.print_value value)
      |> failwith


(* Unop evaluation *)

let eval_unop_not (bvalue: Value.base): Value.base =
  match bvalue with
  | Bool b -> Bool (not b)
  | _ -> 
      Printf.sprintf "Not a boolean value: %s"
        (Value.print_base_value bvalue)
      |> failwith

let eval_unop_bitnot (bvalue: Value.base): Value.base =
  match bvalue with
  | _ ->
      Printf.sprintf
        "Not a bit value: %s"
        (Value.print_base_value bvalue)
      |> failwith

let eval_unop_uminus (bvalue: Value.base): Value.base =
  match bvalue with
  | _ ->
      Printf.sprintf
        "Not an integer value: %s"
        (Value.print_base_value bvalue)
      |> failwith

let eval_unop' (op: Op.un) (bvalue: Value.base): Value.base =
  match op with
  | Not _ -> eval_unop_not bvalue
  | BitNot _ -> eval_unop_bitnot bvalue
  | UMinus _ -> eval_unop_uminus bvalue

let eval_unop (op: Op.un) (value: Value.t): Value.t =
  let bvalue = eval_unop' op (extract_base value) in 
  Value.Base bvalue
