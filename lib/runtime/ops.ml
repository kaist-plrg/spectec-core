(* Copyright 2018-present Cornell University
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not
 * use this file except in compliance with the License. You may obtain a copy
 * of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
 * WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
 * License for the specific language governing permissions and limitations
 * under the License.
 *)

open Syntax.Ast
open Domain

(* Bit manipulation *)

let rec shift_bitstring_left (v : Bigint.t) (o : Bigint.t) : Bigint.t =
  if Bigint.(o > zero) then
    shift_bitstring_left Bigint.(v * (one + one)) Bigint.(o - one)
  else v

let power_of_two (w : Bigint.t) : Bigint.t = shift_bitstring_left Bigint.one w

let rec to_two_complement (n : Bigint.t) (w : Bigint.t) : Bigint.t =
  let two = Bigint.(one + one) in
  let w' = power_of_two w in
  if Bigint.(n >= w' / two) then to_two_complement Bigint.(n - w') w
  else if Bigint.(n < -(w' / two)) then to_two_complement Bigint.(n + w') w
  else n

let rec of_two_complement (n : Bigint.t) (w : Bigint.t) : Bigint.t =
  let w' = power_of_two w in
  if Bigint.(n >= w') then Bigint.(n % w')
  else if Bigint.(n < zero) then of_two_complement Bigint.(n + w') w
  else n

let slice_bitstring (n : Bigint.t) (m : Bigint.t) (l : Bigint.t) : Bigint.t =
  let slice_width = Bigint.(m + one - l) in
  if Bigint.(l < zero) then
    raise (Invalid_argument "bitslice x[y:z] must have y > z > 0");
  let shifted = Bigint.(n asr to_int_exn l) in
  let mask = power_of_two Bigint.(slice_width - one) in
  Bigint.bit_and shifted mask

let rec bitwise_neg (n : Bigint.t) (w : Bigint.t) : Bigint.t =
  if Bigint.(w > zero) then
    let w' = power_of_two Bigint.(w - one) in
    let g = slice_bitstring n Bigint.(w - one) Bigint.(w - one) in
    if Bigint.(g = zero) then bitwise_neg Bigint.(n + w') Bigint.(w - one)
    else bitwise_neg Bigint.(n - w') Bigint.(w - one)
  else n

let bit_of_raw_int (n : Bigint.t) (w : Bigint.t) : value =
  let value = of_two_complement n w in
  let width = w in
  VBit { value; width }

let int_of_raw_int (n : Bigint.t) (w : Bigint.t) : value =
  let value = of_two_complement n w in
  let width = w in
  VInt { value; width }

(* Value extraction *)

let extract_bigint (value : value) : Bigint.t =
  match value with
  | VAInt value -> value
  | VInt { value; _ } -> value
  | VBit { value; _ } -> value
  | _ ->
      Printf.sprintf "Not a int/bit value: %s" (Pretty.print_value value)
      |> failwith

(* Unop evaluation *)

let eval_unop_not (value : value) : value =
  match value with
  | VBool b -> VBool (not b)
  | _ ->
      Printf.sprintf "Not a boolean value: %s" (Pretty.print_value value)
      |> failwith

let eval_unop_bitnot (value : value) : value =
  match value with
  | VBit { value; width } ->
      let value = bitwise_neg value width in
      VBit { value; width }
  | _ ->
      Printf.sprintf "Not a bit value: %s" (Pretty.print_value value)
      |> failwith

let eval_unop_uminus (value : value) : value =
  match value with
  | VAInt value -> VAInt (Bigint.neg value)
  | VInt { value; width } ->
      let value = to_two_complement (Bigint.neg value) width in
      VInt { value; width }
  | VBit { value; width } ->
      let value = Bigint.(power_of_two width - value) in
      VBit { value; width }
  | _ ->
      Printf.sprintf "Not an integer value: %s" (Pretty.print_value value)
      |> failwith

let eval_unop (op : Op.un) (value : value) : value =
  match op with
  | Not _ -> eval_unop_not value
  | BitNot _ -> eval_unop_bitnot value
  | UMinus _ -> eval_unop_uminus value

(* Binop evaluation *)

let unsigned_op_sat (l : Bigint.t) (r : Bigint.t) (w : Bigint.t)
    (op : Bigint.t -> Bigint.t -> Bigint.t) : value =
  let x = power_of_two w in
  let n = op l r in
  let n' =
    if Bigint.(n > zero) then Bigint.min n Bigint.(x - one)
    else Bigint.max n Bigint.zero
  in
  VBit { value = n'; width = w }

let signed_op_sat (l : Bigint.t) (r : Bigint.t) (w : Bigint.t)
    (op : Bigint.t -> Bigint.t -> Bigint.t) : value =
  let x = power_of_two Bigint.(w - one) in
  let n = op l r in
  let n' =
    if Bigint.(n > zero) then Bigint.min n Bigint.(x - one)
    else Bigint.max n Bigint.(-x)
  in
  VInt { value = n'; width = w }

let rec shift_bitstring_left (v : Bigint.t) (o : Bigint.t) : Bigint.t =
  if Bigint.(o > zero) then
    shift_bitstring_left Bigint.(v * (one + one)) Bigint.(o - one)
  else v

let rec shift_bitstring_right (v : Bigint.t) (o : Bigint.t) (arith : bool)
    (mx : Bigint.t) : Bigint.t =
  if not arith then
    if Bigint.(o > zero) then
      shift_bitstring_right Bigint.(v / (one + one)) Bigint.(o - one) arith mx
    else v
  else if Bigint.(o > zero) then
    shift_bitstring_right
      Bigint.((v / (one + one)) + mx)
      Bigint.(o - one)
      arith mx
  else v

let rec eval_binop_plus (lvalue : value) (rvalue : value) : value =
  match (lvalue, rvalue) with
  | VBit { value = lvalue; width }, VBit { value = rvalue; _ } ->
      let value = of_two_complement Bigint.(lvalue + rvalue) width in
      VBit { value; width }
  | VInt { value = lvalue; width }, VInt { value = rvalue; _ } ->
      let value = of_two_complement Bigint.(lvalue + rvalue) width in
      VInt { value; width }
  | VBit { width; _ }, VAInt rvalue ->
      eval_binop_plus lvalue (bit_of_raw_int rvalue width)
  | VAInt lvalue, VBit { width; _ } ->
      eval_binop_plus (bit_of_raw_int lvalue width) rvalue
  | VInt { width; _ }, VAInt rvalue ->
      eval_binop_plus lvalue (int_of_raw_int rvalue width)
  | VAInt lvalue, VInt { width; _ } ->
      eval_binop_plus (int_of_raw_int lvalue width) rvalue
  | VAInt lvalue, VAInt rvalue -> VAInt Bigint.(lvalue + rvalue)
  | _ ->
      Printf.sprintf "Invalid addition: %s + %s"
        (Pretty.print_value lvalue)
        (Pretty.print_value rvalue)
      |> failwith

let rec eval_binop_plussat (lvalue : value) (rvalue : value) : value =
  match (lvalue, rvalue) with
  | VBit { value = lvalue; width }, VBit { value = rvalue; _ } ->
      unsigned_op_sat lvalue rvalue width Bigint.( + )
  | VInt { value = lvalue; width }, VInt { value = rvalue; _ } ->
      signed_op_sat lvalue rvalue width Bigint.( + )
  | VBit { width; _ }, VAInt rvalue ->
      eval_binop_plussat lvalue (bit_of_raw_int rvalue width)
  | VAInt lvalue, VBit { width; _ } ->
      eval_binop_plussat (bit_of_raw_int lvalue width) rvalue
  | VInt { width; _ }, VAInt rvalue ->
      eval_binop_plussat lvalue (int_of_raw_int rvalue width)
  | VAInt lvalue, VInt { width; _ } ->
      eval_binop_plussat (int_of_raw_int lvalue width) rvalue
  | _ ->
      Printf.sprintf "Invalid addition with saturation: %s (+) %s"
        (Pretty.print_value lvalue)
        (Pretty.print_value rvalue)
      |> failwith

let rec eval_binop_minus (lvalue : value) (rvalue : value) : value =
  match (lvalue, rvalue) with
  | VBit { value = lvalue; width }, VBit { value = rvalue; _ } ->
      let value = of_two_complement Bigint.(lvalue - rvalue) width in
      VBit { value; width }
  | VInt { value = lvalue; width }, VInt { value = rvalue; _ } ->
      let value = of_two_complement Bigint.(lvalue - rvalue) width in
      VInt { value; width }
  | VBit { width; _ }, VAInt rvalue ->
      eval_binop_minus lvalue (bit_of_raw_int rvalue width)
  | VAInt lvalue, VBit { width; _ } ->
      eval_binop_minus (bit_of_raw_int lvalue width) rvalue
  | VInt { width; _ }, VAInt rvalue ->
      eval_binop_minus lvalue (int_of_raw_int rvalue width)
  | VAInt lvalue, VInt { width; _ } ->
      eval_binop_minus (int_of_raw_int lvalue width) rvalue
  | VAInt lvalue, VAInt rvalue -> VAInt Bigint.(lvalue - rvalue)
  | _ ->
      Printf.sprintf "Invalid subtraction: %s - %s"
        (Pretty.print_value lvalue)
        (Pretty.print_value rvalue)
      |> failwith

let rec eval_binop_minussat (lvalue : value) (rvalue : value) : value =
  match (lvalue, rvalue) with
  | VBit { value = lvalue; width }, VBit { value = rvalue; _ } ->
      unsigned_op_sat lvalue rvalue width Bigint.( - )
  | VInt { value = lvalue; width }, VInt { value = rvalue; _ } ->
      signed_op_sat lvalue rvalue width Bigint.( - )
  | VBit { width; _ }, VAInt rvalue ->
      eval_binop_minussat lvalue (bit_of_raw_int rvalue width)
  | VAInt lvalue, VBit { width; _ } ->
      eval_binop_minussat (bit_of_raw_int lvalue width) rvalue
  | VInt { width; _ }, VAInt rvalue ->
      eval_binop_minussat lvalue (int_of_raw_int rvalue width)
  | VAInt lvalue, VInt { width; _ } ->
      eval_binop_minussat (int_of_raw_int lvalue width) rvalue
  | _ ->
      Printf.sprintf "Invalid subtraction with saturation: %s (-) %s"
        (Pretty.print_value lvalue)
        (Pretty.print_value rvalue)
      |> failwith

let rec eval_binop_mul (lvalue : value) (rvalue : value) : value =
  match (lvalue, rvalue) with
  | VBit { value = lvalue; width }, VBit { value = rvalue; _ } ->
      let value = of_two_complement Bigint.(lvalue * rvalue) width in
      VBit { value; width }
  | VInt { value = lvalue; width }, VInt { value = rvalue; _ } ->
      let value = to_two_complement Bigint.(lvalue * rvalue) width in
      VInt { value; width }
  | VBit { width; _ }, VAInt rvalue ->
      eval_binop_mul lvalue (bit_of_raw_int rvalue width)
  | VAInt lvalue, VBit { width; _ } ->
      eval_binop_mul (bit_of_raw_int lvalue width) rvalue
  | VInt { width; _ }, VAInt rvalue ->
      eval_binop_mul lvalue (int_of_raw_int rvalue width)
  | VAInt lvalue, VInt { width; _ } ->
      eval_binop_mul (int_of_raw_int lvalue width) rvalue
  | VAInt lvalue, VAInt rvalue -> VAInt Bigint.(lvalue * rvalue)
  | _ ->
      Printf.sprintf "Invalid multiplication: %s * %s"
        (Pretty.print_value lvalue)
        (Pretty.print_value rvalue)
      |> failwith

let eval_binop_div (lvalue : value) (rvalue : value) : value =
  match (lvalue, rvalue) with
  | VAInt lvalue, VAInt rvalue -> VAInt Bigint.(lvalue / rvalue)
  | VBit { value = lvalue; width }, VBit { value = rvalue; _ } ->
      let value = Bigint.(lvalue / rvalue) in
      VBit { value; width }
  | _ ->
      Printf.sprintf "Invalid division: %s / %s"
        (Pretty.print_value lvalue)
        (Pretty.print_value rvalue)
      |> failwith

let eval_binop_mod (lvalue : value) (rvalue : value) : value =
  match (lvalue, rvalue) with
  | VAInt lvalue, VAInt rvalue -> VAInt Bigint.(lvalue % rvalue)
  | VBit { value = lvalue; width }, VBit { value = rvalue; _ } ->
      let value = Bigint.(lvalue % rvalue) in
      VBit { value; width }
  | _ ->
      Printf.sprintf "Invalid modulo: %s %% %s"
        (Pretty.print_value lvalue)
        (Pretty.print_value rvalue)
      |> failwith

let eval_binop_shl (lvalue : value) (rvalue : value) : value =
  match (lvalue, rvalue) with
  | VBit { value = lvalue; width }, VBit { value = rvalue; _ }
  | VBit { value = lvalue; width }, VAInt rvalue ->
      let value =
        of_two_complement (shift_bitstring_left lvalue rvalue) width
      in
      VBit { value; width }
  | VInt { value = lvalue; width }, VBit { value = rvalue; _ }
  | VInt { value = lvalue; width }, VAInt rvalue ->
      let value =
        to_two_complement (shift_bitstring_left lvalue rvalue) width
      in
      VInt { value; width }
  | VAInt lvalue, VAInt rvalue ->
      let value = shift_bitstring_left lvalue rvalue in
      VAInt value
  | VBit { value = lvalue; width }, VInt { value = rvalue; _ } ->
      let value =
        of_two_complement (shift_bitstring_left lvalue rvalue) width
      in
      VBit { value; width }
  | VInt { value = lvalue; width }, VInt { value = rvalue; _ } ->
      let value =
        to_two_complement (shift_bitstring_left lvalue rvalue) width
      in
      VInt { value; width }
  | _ ->
      Printf.sprintf "Invalid shift left: %s << %s"
        (Pretty.print_value lvalue)
        (Pretty.print_value rvalue)
      |> failwith

let eval_binop_shr (lvalue : value) (rvalue : value) : value =
  match (lvalue, rvalue) with
  | VBit { value = lvalue; width }, VBit { value = rvalue; _ }
  | VBit { value = lvalue; width }, VAInt rvalue ->
      let value =
        of_two_complement
          (shift_bitstring_right lvalue rvalue false Bigint.zero)
          width
      in
      VBit { value; width }
  | VInt { value = lvalue; width }, VBit { value = rvalue; _ }
  | VInt { value = lvalue; width }, VInt { value = rvalue; _ }
  | VInt { value = lvalue; width }, VAInt rvalue ->
      let exp = power_of_two Bigint.(width - one) in
      let arith = Bigint.(of_two_complement lvalue width > exp) in
      let value =
        to_two_complement (shift_bitstring_right lvalue rvalue arith exp) width
      in
      VInt { value; width }
  | VAInt lvalue, VAInt rvalue ->
      let value = shift_bitstring_right lvalue rvalue false Bigint.zero in
      VAInt value
  | VBit { value = lvalue; width }, VInt { value = rvalue; _ } ->
      let value =
        of_two_complement
          (shift_bitstring_right lvalue rvalue false Bigint.zero)
          width
      in
      VBit { value; width }
  | _ ->
      Printf.sprintf "Invalid shift right: %s >> %s"
        (Pretty.print_value lvalue)
        (Pretty.print_value rvalue)
      |> failwith

let rec eval_binop_le (lvalue : value) (rvalue : value) : value =
  match (lvalue, rvalue) with
  | VBit { value = lvalue; _ }, VBit { value = rvalue; _ }
  | VAInt lvalue, VAInt rvalue ->
      VBool Bigint.(lvalue <= rvalue)
  | VAInt lvalue, VBit { width; _ } ->
      eval_binop_le (bit_of_raw_int lvalue width) rvalue
  | VBit { width; _ }, VAInt rvalue ->
      eval_binop_le lvalue (bit_of_raw_int rvalue width)
  | VAInt lvalue, VInt { width; _ } ->
      eval_binop_le (int_of_raw_int lvalue width) rvalue
  | VInt { width; _ }, VAInt rvalue ->
      eval_binop_le lvalue (int_of_raw_int rvalue width)
  | _ ->
      Printf.sprintf "Invalid less than or equal: %s <= %s"
        (Pretty.print_value lvalue)
        (Pretty.print_value rvalue)
      |> failwith

let rec eval_binop_ge (lvalue : value) (rvalue : value) : value =
  match (lvalue, rvalue) with
  | VBit { value = lvalue; _ }, VBit { value = rvalue; _ }
  | VAInt lvalue, VAInt rvalue ->
      VBool Bigint.(lvalue >= rvalue)
  | VAInt lvalue, VBit { width; _ } ->
      eval_binop_ge (bit_of_raw_int lvalue width) rvalue
  | VBit { width; _ }, VAInt rvalue ->
      eval_binop_ge lvalue (bit_of_raw_int rvalue width)
  | VAInt lvalue, VInt { width; _ } ->
      eval_binop_ge (int_of_raw_int lvalue width) rvalue
  | VInt { width; _ }, VAInt rvalue ->
      eval_binop_ge lvalue (int_of_raw_int rvalue width)
  | _ ->
      Printf.sprintf "Invalid greater than or equal: %s >= %s"
        (Pretty.print_value lvalue)
        (Pretty.print_value rvalue)
      |> failwith

let rec eval_binop_lt (lvalue : value) (rvalue : value) : value =
  match (lvalue, rvalue) with
  | VBit { value = lvalue; _ }, VBit { value = rvalue; _ }
  | VAInt lvalue, VAInt rvalue ->
      VBool Bigint.(lvalue < rvalue)
  | VAInt lvalue, VBit { width; _ } ->
      eval_binop_lt (bit_of_raw_int lvalue width) rvalue
  | VBit { width; _ }, VAInt rvalue ->
      eval_binop_lt lvalue (bit_of_raw_int rvalue width)
  | VAInt lvalue, VInt { width; _ } ->
      eval_binop_lt (int_of_raw_int lvalue width) rvalue
  | VInt { width; _ }, VAInt rvalue ->
      eval_binop_lt lvalue (int_of_raw_int rvalue width)
  | _ ->
      Printf.sprintf "Invalid less than: %s < %s"
        (Pretty.print_value lvalue)
        (Pretty.print_value rvalue)
      |> failwith

let rec eval_binop_gt (lvalue : value) (rvalue : value) : value =
  match (lvalue, rvalue) with
  | VBit { value = lvalue; _ }, VBit { value = rvalue; _ }
  | VAInt lvalue, VAInt rvalue ->
      VBool Bigint.(lvalue > rvalue)
  | VAInt lvalue, VBit { width; _ } ->
      eval_binop_gt (bit_of_raw_int lvalue width) rvalue
  | VBit { width; _ }, VAInt rvalue ->
      eval_binop_gt lvalue (bit_of_raw_int rvalue width)
  | VAInt lvalue, VInt { width; _ } ->
      eval_binop_gt (int_of_raw_int lvalue width) rvalue
  | VInt { width; _ }, VAInt rvalue ->
      eval_binop_gt lvalue (int_of_raw_int rvalue width)
  | _ ->
      Printf.sprintf "Invalid greater than: %s > %s"
        (Pretty.print_value lvalue)
        (Pretty.print_value rvalue)
      |> failwith

let rec eval_binop_eq_entries (lentries : (string * value) list)
    (rentries : (string * value) list) : bool =
  List.length lentries = List.length rentries
  && List.for_all
       (fun lentry ->
         let lfield, lvalue = lentry in
         try
           let rvalue = List.assoc lfield rentries in
           eval_binop_eq lvalue rvalue
         with _ -> false)
       lentries

and eval_binop_eq (lvalue : value) (rvalue : value) : bool =
  match (lvalue, rvalue) with
  | VBool b1, VBool b2 -> b1 = b2
  | VBit { value = lvalue; _ }, VBit { value = rvalue; _ }
  | VAInt lvalue, VAInt rvalue
  | VInt { value = lvalue; _ }, VInt { value = rvalue; _ } ->
      Bigint.(lvalue = rvalue)
  | VBit { width; _ }, VAInt rvalue ->
      eval_binop_eq lvalue (bit_of_raw_int rvalue width)
  | VAInt lvalue, VBit { width; _ } ->
      eval_binop_eq (bit_of_raw_int lvalue width) rvalue
  | VInt { width; _ }, VAInt rvalue ->
      eval_binop_eq lvalue (int_of_raw_int rvalue width)
  | VAInt lvalue, VInt { width; _ } ->
      eval_binop_eq (int_of_raw_int lvalue width) rvalue
  | ( VHeader { valid = lvalid; entries = lentries },
      VHeader { valid = rvalid; entries = rentries } ) ->
      lvalid = rvalid && eval_binop_eq_entries lentries rentries
  | VStruct { entries = lentries }, VStruct { entries = rentries } ->
      List.length lentries = List.length rentries
      && eval_binop_eq_entries lentries rentries
  | VTuple lvalues, VTuple rvalues ->
      List.length lvalues = List.length rvalues
      && List.for_all2
           (fun lvalue rvalue -> eval_binop_eq lvalue rvalue)
           lvalues rvalues
  | _ ->
      Printf.sprintf "Invalid equality: %s == %s"
        (Pretty.print_value lvalue)
        (Pretty.print_value rvalue)
      |> failwith

let eval_binop_ne (lvalue : value) (rvalue : value) : bool =
  not (eval_binop_eq lvalue rvalue)

let rec eval_binop_bitand (lvalue : value) (rvalue : value) : value =
  match (lvalue, rvalue) with
  | VBit { value = lvalue; width }, VBit { value = rvalue; _ } ->
      let value = Bigint.bit_and lvalue rvalue in
      VBit { value; width }
  | VBit { width; _ }, VAInt rvalue ->
      eval_binop_bitand lvalue (bit_of_raw_int rvalue width)
  | VAInt lvalue, VBit { width; _ } ->
      eval_binop_bitand (bit_of_raw_int lvalue width) rvalue
  | _ ->
      Printf.sprintf "Invalid bitwise and: %s & %s"
        (Pretty.print_value lvalue)
        (Pretty.print_value rvalue)
      |> failwith

let rec eval_binop_bitxor (lvalue : value) (rvalue : value) : value =
  match (lvalue, rvalue) with
  | VBit { value = lvalue; width }, VBit { value = rvalue; _ } ->
      let value = Bigint.bit_xor lvalue rvalue in
      VBit { value; width }
  | VBit { width; _ }, VAInt rvalue ->
      eval_binop_bitxor lvalue (bit_of_raw_int rvalue width)
  | VAInt lvalue, VBit { width; _ } ->
      eval_binop_bitxor (bit_of_raw_int lvalue width) rvalue
  | _ ->
      Printf.sprintf "Invalid bitwise xor: %s ^ %s"
        (Pretty.print_value lvalue)
        (Pretty.print_value rvalue)
      |> failwith

let rec eval_binop_bitor (lvalue : value) (rvalue : value) : value =
  match (lvalue, rvalue) with
  | VBit { value = lvalue; width }, VBit { value = rvalue; _ } ->
      let value = Bigint.bit_or lvalue rvalue in
      VBit { value; width }
  | VBit { width; _ }, VAInt rvalue ->
      eval_binop_bitor lvalue (bit_of_raw_int rvalue width)
  | VAInt lvalue, VBit { width; _ } ->
      eval_binop_bitor (bit_of_raw_int lvalue width) rvalue
  | _ ->
      Printf.sprintf "Invalid bitwise or: %s | %s"
        (Pretty.print_value lvalue)
        (Pretty.print_value rvalue)
      |> failwith

let rec eval_binop_plusplus (lvalue : value) (rvalue : value) : value =
  match (lvalue, rvalue) with
  | ( VBit { value = lvalue; width = lwidth },
      VBit { value = rvalue; width = rwidth } ) ->
      let value = Bigint.(shift_bitstring_left lvalue rwidth + rvalue) in
      let width = Bigint.(lwidth + rwidth) in
      VBit { value; width }
  | VBit { width; _ }, VAInt rvalue ->
      eval_binop_plusplus lvalue (bit_of_raw_int rvalue width)
  | VAInt lvalue, VBit { width; _ } ->
      eval_binop_plusplus (bit_of_raw_int lvalue width) rvalue
  | _ ->
      Printf.sprintf "Invalid concatenation: %s ++ %s"
        (Pretty.print_value lvalue)
        (Pretty.print_value rvalue)
      |> failwith

let eval_binop_and (lvalue : value) (rvalue : value) : value =
  match (lvalue, rvalue) with
  | VBool b1, VBool b2 -> VBool (b1 && b2)
  | _ ->
      Printf.sprintf "Invalid and operator: %s && %s"
        (Pretty.print_value lvalue)
        (Pretty.print_value rvalue)
      |> failwith

let eval_binop_or (lvalue : value) (rvalue : value) : value =
  match (lvalue, rvalue) with
  | VBool b1, VBool b2 -> VBool (b1 || b2)
  | _ ->
      Printf.sprintf "Invalid or operator: %s || %s"
        (Pretty.print_value lvalue)
        (Pretty.print_value rvalue)
      |> failwith

let eval_binop (op : Op.bin) (lvalue : value) (rvalue : value) : value =
  match op with
  | Plus _ -> eval_binop_plus lvalue rvalue
  | PlusSat _ -> eval_binop_plussat lvalue rvalue
  | Minus _ -> eval_binop_minus lvalue rvalue
  | MinusSat _ -> eval_binop_minussat lvalue rvalue
  | Mul _ -> eval_binop_mul lvalue rvalue
  | Div _ -> eval_binop_div lvalue rvalue
  | Mod _ -> eval_binop_mod lvalue rvalue
  | Shl _ -> eval_binop_shl lvalue rvalue
  | Shr _ -> eval_binop_shr lvalue rvalue
  | Le _ -> eval_binop_le lvalue rvalue
  | Ge _ -> eval_binop_ge lvalue rvalue
  | Lt _ -> eval_binop_lt lvalue rvalue
  | Gt _ -> eval_binop_gt lvalue rvalue
  | Eq _ -> VBool (eval_binop_eq lvalue rvalue)
  | NotEq _ -> VBool (eval_binop_ne lvalue rvalue)
  | BitAnd _ -> eval_binop_bitand lvalue rvalue
  | BitXor _ -> eval_binop_bitxor lvalue rvalue
  | BitOr _ -> eval_binop_bitor lvalue rvalue
  | PlusPlus _ -> eval_binop_plusplus lvalue rvalue
  | And _ -> eval_binop_and lvalue rvalue
  | Or _ -> eval_binop_or lvalue rvalue

(* Bitslice evaluation *)

let eval_bitstring_access' (value : Bigint.t) (lvalue : Bigint.t)
    (hvalue : Bigint.t) : value =
  let width = Bigint.(hvalue - lvalue + one) in
  let value = slice_bitstring value hvalue lvalue in
  VBit { value; width }

let eval_bitstring_access (value : value) (lvalue : value) (hvalue : value) :
    value =
  let extract value = extract_bigint value in
  eval_bitstring_access' (extract value) (extract lvalue) (extract hvalue)

(* Type cast evaluation *)

let eval_cast_to_bool (value : value) : value =
  match value with
  | VBool b -> VBool b
  | VBit { width; value } when width = Bigint.one -> VBool Bigint.(value = one)
  | VAInt value -> VBool Bigint.(value = one)
  | _ -> failwith "cast to bool undefined"

let eval_cast_to_bit (width : Bigint.t) (value : value) : value =
  match value with
  | VBool b ->
      let value = if b then Bigint.one else Bigint.zero in
      VBit { value; width }
  | VInt { value; _ } | VBit { value; _ } | VAInt value ->
      bit_of_raw_int value width
  | _ ->
      Printf.sprintf "(TODO) Cast to bitstring undefined: %s"
        (Pretty.print_value value)
      |> failwith

let eval_cast_to_int (width : Bigint.t) (value : value) : value =
  match value with
  | VBit { value; _ } | VInt { value; _ } | VAInt value ->
      int_of_raw_int value width
  | _ ->
      Printf.sprintf "(TODO) Cast to integer undefined: %s"
        (Pretty.print_value value)
      |> failwith

let rec eval_cast_entries (entries : (string * typ) list) (value : value) :
    (string * value) list =
  match value with
  | VTuple values ->
      assert (List.length entries = List.length values);
      List.map2
        (fun (field, typ) value ->
          let value = eval_cast typ value in
          (field, value))
        entries values
  | VHeader { entries; _ } | VStruct { entries } -> entries
  | _ ->
      Printf.sprintf "(TODO) Cast to entries undefined: %s"
        (Pretty.print_value value)
      |> failwith

and eval_cast_tuple (typs : typ list) (value : value) : value =
  match value with
  | VTuple values ->
      assert (List.length typs = List.length values);
      let values = List.map2 eval_cast typs values in
      VTuple values
  | _ ->
      Printf.sprintf "(TODO) Cast to tuple undefined: %s"
        (Pretty.print_value value)
      |> failwith

and eval_cast (typ : typ) (value : value) : value =
  match typ with
  | TBool -> eval_cast_to_bool value
  | TAInt ->
      let value = extract_bigint value in
      VAInt value
  | TBit { width } -> eval_cast_to_bit width value
  | TInt { width } -> eval_cast_to_int width value
  | TTuple typs -> eval_cast_tuple typs value
  | THeader { entries } ->
      let entries = eval_cast_entries entries value in
      VHeader { valid = true; entries }
  | TStruct { entries } ->
      let entries = eval_cast_entries entries value in
      VStruct { entries }
  | _ ->
      Printf.sprintf "(TODO) Cast to type %s undefined" (Pretty.print_typ typ)
      |> failwith
