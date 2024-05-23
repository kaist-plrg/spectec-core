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
open Base

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

let bit_of_raw_int (n : Bigint.t) (w : Bigint.t) : Value.t =
  let value = of_two_complement n w in
  let width = w in
  BitV (width, value)

let int_of_raw_int (n : Bigint.t) (w : Bigint.t) : Value.t =
  let value = of_two_complement n w in
  let width = w in
  IntV (width, value)

(* Value extraction *)

let extract_bigint (value : Value.t) : Bigint.t =
  match value with
  | AIntV value -> value
  | IntV (_, value) -> value
  | BitV (_, value) -> value
  | _ -> Format.asprintf "Not a int/bit value: %a" Value.pp value |> failwith

(* Unop evaluation *)

let eval_unop_not (value : Value.t) : Value.t =
  match value with
  | BoolV b -> BoolV (not b)
  | _ -> Format.asprintf "Not a boolean value: %a" Value.pp value |> failwith

let eval_unop_bitnot (value : Value.t) : Value.t =
  match value with
  | BitV (width, value) ->
      let value = bitwise_neg value width in
      BitV (width, value)
  | _ -> Format.asprintf "Not a bit value: %a" Value.pp value |> failwith

let eval_unop_uminus (value : Value.t) : Value.t =
  match value with
  | AIntV value -> AIntV (Bigint.neg value)
  | IntV (width, value) ->
      let value = to_two_complement (Bigint.neg value) width in
      IntV (width, value)
  | BitV (width, value) ->
      let value = Bigint.(power_of_two width - value) in
      BitV (width, value)
  | _ -> Format.asprintf "Not an integer value: %a" Value.pp value |> failwith

let eval_unop (op : unop) (value : Value.t) : Value.t =
  match op with
  | BNotOp -> eval_unop_bitnot value
  | LNotOp -> eval_unop_not value
  | UMinusOp -> eval_unop_uminus value

(* Binop evaluation *)

let unsigned_op_sat (l : Bigint.t) (r : Bigint.t) (w : Bigint.t)
    (op : Bigint.t -> Bigint.t -> Bigint.t) : Value.t =
  let x = power_of_two w in
  let n = op l r in
  let n' =
    if Bigint.(n > zero) then Bigint.min n Bigint.(x - one)
    else Bigint.max n Bigint.zero
  in
  let width = w in
  let value = n' in
  BitV (width, value)

let signed_op_sat (l : Bigint.t) (r : Bigint.t) (w : Bigint.t)
    (op : Bigint.t -> Bigint.t -> Bigint.t) : Value.t =
  let x = power_of_two Bigint.(w - one) in
  let n = op l r in
  let n' =
    if Bigint.(n > zero) then Bigint.min n Bigint.(x - one)
    else Bigint.max n Bigint.(-x)
  in
  let width = w in
  let value = n' in
  IntV (width, value)

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

let rec eval_binop_plus (lvalue : Value.t) (rvalue : Value.t) : Value.t =
  match (lvalue, rvalue) with
  | BitV (width, lvalue), BitV (_, rvalue) ->
      let value = of_two_complement Bigint.(lvalue + rvalue) width in
      BitV (width, value)
  | IntV (width, lvalue), IntV (_, rvalue) ->
      let value = of_two_complement Bigint.(lvalue + rvalue) width in
      IntV (width, value)
  | BitV (width, _), AIntV rvalue ->
      eval_binop_plus lvalue (bit_of_raw_int rvalue width)
  | AIntV lvalue, BitV (width, _) ->
      eval_binop_plus (bit_of_raw_int lvalue width) rvalue
  | IntV (width, _), AIntV rvalue ->
      eval_binop_plus lvalue (int_of_raw_int rvalue width)
  | AIntV lvalue, IntV (width, _) ->
      eval_binop_plus (int_of_raw_int lvalue width) rvalue
  | AIntV lvalue, AIntV rvalue -> AIntV Bigint.(lvalue + rvalue)
  | _ ->
      Format.asprintf "Invalid addition: %a + %a" Value.pp lvalue Value.pp
        rvalue
      |> failwith

let rec eval_binop_plussat (lvalue : Value.t) (rvalue : Value.t) : Value.t =
  match (lvalue, rvalue) with
  | BitV (width, lvalue), BitV (_, rvalue) ->
      unsigned_op_sat lvalue rvalue width Bigint.( + )
  | IntV (width, lvalue), IntV (_, rvalue) ->
      signed_op_sat lvalue rvalue width Bigint.( + )
  | BitV (width, _), AIntV rvalue ->
      eval_binop_plussat lvalue (bit_of_raw_int rvalue width)
  | AIntV lvalue, BitV (width, _) ->
      eval_binop_plussat (bit_of_raw_int lvalue width) rvalue
  | IntV (width, _), AIntV rvalue ->
      eval_binop_plussat lvalue (int_of_raw_int rvalue width)
  | AIntV lvalue, IntV (width, _) ->
      eval_binop_plussat (int_of_raw_int lvalue width) rvalue
  | _ ->
      Format.asprintf "Invalid addition with saturation: %a (+) %a" Value.pp
        lvalue Value.pp rvalue
      |> failwith

let rec eval_binop_minus (lvalue : Value.t) (rvalue : Value.t) : Value.t =
  match (lvalue, rvalue) with
  | BitV (width, lvalue), BitV (_, rvalue) ->
      let value = of_two_complement Bigint.(lvalue - rvalue) width in
      BitV (width, value)
  | IntV (width, lvalue), IntV (_, rvalue) ->
      let value = of_two_complement Bigint.(lvalue - rvalue) width in
      IntV (width, value)
  | BitV (width, _), AIntV rvalue ->
      eval_binop_minus lvalue (bit_of_raw_int rvalue width)
  | AIntV lvalue, BitV (width, _) ->
      eval_binop_minus (bit_of_raw_int lvalue width) rvalue
  | IntV (width, _), AIntV rvalue ->
      eval_binop_minus lvalue (int_of_raw_int rvalue width)
  | AIntV lvalue, IntV (width, _) ->
      eval_binop_minus (int_of_raw_int lvalue width) rvalue
  | AIntV lvalue, AIntV rvalue -> AIntV Bigint.(lvalue - rvalue)
  | _ ->
      Format.asprintf "Invalid subtraction: %a - %a" Value.pp lvalue Value.pp
        rvalue
      |> failwith

let rec eval_binop_minussat (lvalue : Value.t) (rvalue : Value.t) : Value.t =
  match (lvalue, rvalue) with
  | BitV (width, lvalue), BitV (_, rvalue) ->
      unsigned_op_sat lvalue rvalue width Bigint.( - )
  | IntV (width, lvalue), IntV (_, rvalue) ->
      signed_op_sat lvalue rvalue width Bigint.( - )
  | BitV (width, _), AIntV rvalue ->
      eval_binop_minussat lvalue (bit_of_raw_int rvalue width)
  | AIntV lvalue, BitV (width, _) ->
      eval_binop_minussat (bit_of_raw_int lvalue width) rvalue
  | IntV (width, _), AIntV rvalue ->
      eval_binop_minussat lvalue (int_of_raw_int rvalue width)
  | AIntV lvalue, IntV (width, _) ->
      eval_binop_minussat (int_of_raw_int lvalue width) rvalue
  | _ ->
      Format.asprintf "Invalid subtraction with saturation: %a (-) %a" Value.pp
        lvalue Value.pp rvalue
      |> failwith

let rec eval_binop_mul (lvalue : Value.t) (rvalue : Value.t) : Value.t =
  match (lvalue, rvalue) with
  | BitV (width, lvalue), BitV (_, rvalue) ->
      let value = of_two_complement Bigint.(lvalue * rvalue) width in
      BitV (width, value)
  | IntV (width, lvalue), IntV (_, rvalue) ->
      let value = to_two_complement Bigint.(lvalue * rvalue) width in
      IntV (width, value)
  | BitV (width, _), AIntV rvalue ->
      eval_binop_mul lvalue (bit_of_raw_int rvalue width)
  | AIntV lvalue, BitV (width, _) ->
      eval_binop_mul (bit_of_raw_int lvalue width) rvalue
  | IntV (width, _), AIntV rvalue ->
      eval_binop_mul lvalue (int_of_raw_int rvalue width)
  | AIntV lvalue, IntV (width, _) ->
      eval_binop_mul (int_of_raw_int lvalue width) rvalue
  | AIntV lvalue, AIntV rvalue -> AIntV Bigint.(lvalue * rvalue)
  | _ ->
      Format.asprintf "Invalid multiplication: %a * %a" Value.pp lvalue Value.pp
        rvalue
      |> failwith

let eval_binop_div (lvalue : Value.t) (rvalue : Value.t) : Value.t =
  match (lvalue, rvalue) with
  | AIntV lvalue, AIntV rvalue -> AIntV Bigint.(lvalue / rvalue)
  | BitV (width, lvalue), BitV (_, rvalue) ->
      let value = Bigint.(lvalue / rvalue) in
      BitV (width, value)
  | _ ->
      Format.asprintf "Invalid division: %a / %a" Value.pp lvalue Value.pp
        rvalue
      |> failwith

let eval_binop_mod (lvalue : Value.t) (rvalue : Value.t) : Value.t =
  match (lvalue, rvalue) with
  | AIntV lvalue, AIntV rvalue -> AIntV Bigint.(lvalue % rvalue)
  | BitV (width, lvalue), BitV (_, rvalue) ->
      let value = Bigint.(lvalue % rvalue) in
      BitV (width, value)
  | _ ->
      Format.asprintf "Invalid modulo: %a %% %a" Value.pp lvalue Value.pp rvalue
      |> failwith

let eval_binop_shl (lvalue : Value.t) (rvalue : Value.t) : Value.t =
  match (lvalue, rvalue) with
  | BitV (width, lvalue), BitV (_, rvalue) | BitV (width, lvalue), AIntV rvalue
    ->
      let value =
        of_two_complement (shift_bitstring_left lvalue rvalue) width
      in
      BitV (width, value)
  | IntV (width, lvalue), BitV (_, rvalue) | IntV (width, lvalue), AIntV rvalue
    ->
      let value =
        to_two_complement (shift_bitstring_left lvalue rvalue) width
      in
      IntV (width, value)
  | AIntV lvalue, AIntV rvalue ->
      let value = shift_bitstring_left lvalue rvalue in
      AIntV value
  | BitV (width, lvalue), IntV (_, rvalue) ->
      let value =
        of_two_complement (shift_bitstring_left lvalue rvalue) width
      in
      BitV (width, value)
  | IntV (width, lvalue), IntV (_, rvalue) ->
      let value =
        to_two_complement (shift_bitstring_left lvalue rvalue) width
      in
      IntV (width, value)
  | _ ->
      Format.asprintf "Invalid shift left: %a << %a" Value.pp lvalue Value.pp
        rvalue
      |> failwith

let eval_binop_shr (lvalue : Value.t) (rvalue : Value.t) : Value.t =
  match (lvalue, rvalue) with
  | BitV (width, lvalue), BitV (_, rvalue) | BitV (width, lvalue), AIntV rvalue
    ->
      let value =
        of_two_complement
          (shift_bitstring_right lvalue rvalue false Bigint.zero)
          width
      in
      BitV (width, value)
  | IntV (width, lvalue), BitV (_, rvalue)
  | IntV (width, lvalue), IntV (_, rvalue)
  | IntV (width, lvalue), AIntV rvalue ->
      let exp = power_of_two Bigint.(width - one) in
      let arith = Bigint.(of_two_complement lvalue width > exp) in
      let value =
        to_two_complement (shift_bitstring_right lvalue rvalue arith exp) width
      in
      IntV (width, value)
  | AIntV lvalue, AIntV rvalue ->
      let value = shift_bitstring_right lvalue rvalue false Bigint.zero in
      AIntV value
  | BitV (width, lvalue), IntV (_, rvalue) ->
      let value =
        of_two_complement
          (shift_bitstring_right lvalue rvalue false Bigint.zero)
          width
      in
      BitV (width, value)
  | _ ->
      Format.asprintf "Invalid shift right: %a >> %a" Value.pp lvalue Value.pp
        rvalue
      |> failwith

let rec eval_binop_le (lvalue : Value.t) (rvalue : Value.t) : Value.t =
  match (lvalue, rvalue) with
  | BitV (_, lvalue), BitV (_, rvalue) | AIntV lvalue, AIntV rvalue ->
      BoolV Bigint.(lvalue <= rvalue)
  | AIntV lvalue, BitV (width, _) ->
      eval_binop_le (bit_of_raw_int lvalue width) rvalue
  | BitV (width, _), AIntV rvalue ->
      eval_binop_le lvalue (bit_of_raw_int rvalue width)
  | AIntV lvalue, IntV (width, _) ->
      eval_binop_le (int_of_raw_int lvalue width) rvalue
  | IntV (width, _), AIntV rvalue ->
      eval_binop_le lvalue (int_of_raw_int rvalue width)
  | _ ->
      Format.asprintf "Invalid less than or equal: %a <= %a" Value.pp lvalue
        Value.pp rvalue
      |> failwith

let rec eval_binop_ge (lvalue : Value.t) (rvalue : Value.t) : Value.t =
  match (lvalue, rvalue) with
  | BitV (_, lvalue), BitV (_, rvalue) | AIntV lvalue, AIntV rvalue ->
      BoolV Bigint.(lvalue >= rvalue)
  | AIntV lvalue, BitV (width, _) ->
      eval_binop_ge (bit_of_raw_int lvalue width) rvalue
  | BitV (width, _), AIntV rvalue ->
      eval_binop_ge lvalue (bit_of_raw_int rvalue width)
  | AIntV lvalue, IntV (width, _) ->
      eval_binop_ge (int_of_raw_int lvalue width) rvalue
  | IntV (width, _), AIntV rvalue ->
      eval_binop_ge lvalue (int_of_raw_int rvalue width)
  | _ ->
      Format.asprintf "Invalid greater than or equal: %a >= %a" Value.pp lvalue
        Value.pp rvalue
      |> failwith

let rec eval_binop_lt (lvalue : Value.t) (rvalue : Value.t) : Value.t =
  match (lvalue, rvalue) with
  | BitV (_, lvalue), BitV (_, rvalue) | AIntV lvalue, AIntV rvalue ->
      BoolV Bigint.(lvalue < rvalue)
  | AIntV lvalue, BitV (width, _) ->
      eval_binop_lt (bit_of_raw_int lvalue width) rvalue
  | BitV (width, _), AIntV rvalue ->
      eval_binop_lt lvalue (bit_of_raw_int rvalue width)
  | AIntV lvalue, IntV (width, _) ->
      eval_binop_lt (int_of_raw_int lvalue width) rvalue
  | IntV (width, _), AIntV rvalue ->
      eval_binop_lt lvalue (int_of_raw_int rvalue width)
  | _ ->
      Format.asprintf "Invalid less than: %a < %a" Value.pp lvalue Value.pp
        rvalue
      |> failwith

let rec eval_binop_gt (lvalue : Value.t) (rvalue : Value.t) : Value.t =
  match (lvalue, rvalue) with
  | BitV (_, lvalue), BitV (_, rvalue) | AIntV lvalue, AIntV rvalue ->
      BoolV Bigint.(lvalue > rvalue)
  | AIntV lvalue, BitV (width, _) ->
      eval_binop_gt (bit_of_raw_int lvalue width) rvalue
  | BitV (width, _), AIntV rvalue ->
      eval_binop_gt lvalue (bit_of_raw_int rvalue width)
  | AIntV lvalue, IntV (width, _) ->
      eval_binop_gt (int_of_raw_int lvalue width) rvalue
  | IntV (width, _), AIntV rvalue ->
      eval_binop_gt lvalue (int_of_raw_int rvalue width)
  | _ ->
      Format.asprintf "Invalid greater than: %a > %a" Value.pp lvalue Value.pp
        rvalue
      |> failwith

let rec eval_binop_eq_entries (lentries : (string * Value.t) list)
    (rentries : (string * Value.t) list) : bool =
  List.length lentries = List.length rentries
  && List.for_all
       (fun lentry ->
         let lfield, lvalue = lentry in
         try
           let rvalue = List.assoc lfield rentries in
           eval_binop_eq lvalue rvalue
         with _ -> false)
       lentries

and eval_binop_eq (lvalue : Value.t) (rvalue : Value.t) : bool =
  match (lvalue, rvalue) with
  | BoolV b1, BoolV b2 -> b1 = b2
  | BitV (_, lvalue), BitV (_, rvalue)
  | AIntV lvalue, AIntV rvalue
  | IntV (_, lvalue), IntV (_, rvalue) ->
      Bigint.(lvalue = rvalue)
  | BitV (width, _), AIntV rvalue ->
      eval_binop_eq lvalue (bit_of_raw_int rvalue width)
  | AIntV lvalue, BitV (width, _) ->
      eval_binop_eq (bit_of_raw_int lvalue width) rvalue
  | IntV (width, _), AIntV rvalue ->
      eval_binop_eq lvalue (int_of_raw_int rvalue width)
  | AIntV lvalue, IntV (width, _) ->
      eval_binop_eq (int_of_raw_int lvalue width) rvalue
  | HeaderV (lvalid, lentries), HeaderV (rvalid, rentries) ->
      lvalid = rvalid && eval_binop_eq_entries lentries rentries
  | StructV lentries, StructV rentries ->
      List.length lentries = List.length rentries
      && eval_binop_eq_entries lentries rentries
  | TupleV lvalues, TupleV rvalues ->
      List.length lvalues = List.length rvalues
      && List.for_all2
           (fun lvalue rvalue -> eval_binop_eq lvalue rvalue)
           lvalues rvalues
  | _ ->
      Format.asprintf "Invalid equality: %a == %a" Value.pp lvalue Value.pp
        rvalue
      |> failwith

let eval_binop_ne (lvalue : Value.t) (rvalue : Value.t) : bool =
  not (eval_binop_eq lvalue rvalue)

let rec eval_binop_bitand (lvalue : Value.t) (rvalue : Value.t) : Value.t =
  match (lvalue, rvalue) with
  | BitV (width, lvalue), BitV (_, rvalue) ->
      let value = Bigint.bit_and lvalue rvalue in
      BitV (width, value)
  | BitV (width, _), AIntV rvalue ->
      eval_binop_bitand lvalue (bit_of_raw_int rvalue width)
  | AIntV lvalue, BitV (width, _) ->
      eval_binop_bitand (bit_of_raw_int lvalue width) rvalue
  | _ ->
      Format.asprintf "Invalid bitwise and: %a & %a" Value.pp lvalue Value.pp
        rvalue
      |> failwith

let rec eval_binop_bitxor (lvalue : Value.t) (rvalue : Value.t) : Value.t =
  match (lvalue, rvalue) with
  | BitV (width, lvalue), BitV (_, rvalue) ->
      let value = Bigint.bit_xor lvalue rvalue in
      BitV (width, value)
  | BitV (width, _), AIntV rvalue ->
      eval_binop_bitxor lvalue (bit_of_raw_int rvalue width)
  | AIntV lvalue, BitV (width, _) ->
      eval_binop_bitxor (bit_of_raw_int lvalue width) rvalue
  | _ ->
      Format.asprintf "Invalid bitwise xor: %a ^ %a" Value.pp lvalue Value.pp
        rvalue
      |> failwith

let rec eval_binop_bitor (lvalue : Value.t) (rvalue : Value.t) : Value.t =
  match (lvalue, rvalue) with
  | BitV (width, lvalue), BitV (_, rvalue) ->
      let value = Bigint.bit_or lvalue rvalue in
      BitV (width, value)
  | BitV (width, _), AIntV rvalue ->
      eval_binop_bitor lvalue (bit_of_raw_int rvalue width)
  | AIntV lvalue, BitV (width, _) ->
      eval_binop_bitor (bit_of_raw_int lvalue width) rvalue
  | _ ->
      Format.asprintf "Invalid bitwise or: %a | %a" Value.pp lvalue Value.pp
        rvalue
      |> failwith

let rec eval_binop_plusplus (lvalue : Value.t) (rvalue : Value.t) : Value.t =
  match (lvalue, rvalue) with
  | BitV (lwidth, lvalue), BitV (rwidth, rvalue) ->
      let value = Bigint.(shift_bitstring_left lvalue rwidth + rvalue) in
      let width = Bigint.(lwidth + rwidth) in
      BitV (width, value)
  | BitV (width, _), AIntV rvalue ->
      eval_binop_plusplus lvalue (bit_of_raw_int rvalue width)
  | AIntV lvalue, BitV (width, _) ->
      eval_binop_plusplus (bit_of_raw_int lvalue width) rvalue
  | _ ->
      Format.asprintf "Invalid concatenation: %a ++ %a" Value.pp lvalue Value.pp
        rvalue
      |> failwith

let eval_binop_and (lvalue : Value.t) (rvalue : Value.t) : Value.t =
  match (lvalue, rvalue) with
  | BoolV b1, BoolV b2 -> BoolV (b1 && b2)
  | _ ->
      Format.asprintf "Invalid and operator: %a && %a" Value.pp lvalue Value.pp
        rvalue
      |> failwith

let eval_binop_or (lvalue : Value.t) (rvalue : Value.t) : Value.t =
  match (lvalue, rvalue) with
  | BoolV b1, BoolV b2 -> BoolV (b1 || b2)
  | _ ->
      Format.asprintf "Invalid or operator: %a || %a" Value.pp lvalue Value.pp
        rvalue
      |> failwith

let eval_binop (op : binop) (lvalue : Value.t) (rvalue : Value.t) : Value.t =
  match op with
  | PlusOp -> eval_binop_plus lvalue rvalue
  | SPlusOp -> eval_binop_plussat lvalue rvalue
  | MinusOp -> eval_binop_minus lvalue rvalue
  | SMinusOp -> eval_binop_minussat lvalue rvalue
  | MulOp -> eval_binop_mul lvalue rvalue
  | DivOp -> eval_binop_div lvalue rvalue
  | ModOp -> eval_binop_mod lvalue rvalue
  | ShlOp -> eval_binop_shl lvalue rvalue
  | ShrOp -> eval_binop_shr lvalue rvalue
  | LeOp -> eval_binop_le lvalue rvalue
  | GeOp -> eval_binop_ge lvalue rvalue
  | LtOp -> eval_binop_lt lvalue rvalue
  | GtOp -> eval_binop_gt lvalue rvalue
  | EqOp -> BoolV (eval_binop_eq lvalue rvalue)
  | NeOp -> BoolV (eval_binop_ne lvalue rvalue)
  | BAndOp -> eval_binop_bitand lvalue rvalue
  | BXorOp -> eval_binop_bitxor lvalue rvalue
  | BOrOp -> eval_binop_bitor lvalue rvalue
  | ConcatOp -> eval_binop_plusplus lvalue rvalue
  | LAndOp -> eval_binop_and lvalue rvalue
  | LOrOp -> eval_binop_or lvalue rvalue

(* Bitslice evaluation *)

let eval_bitstring_access' (value : Bigint.t) (lvalue : Bigint.t)
    (hvalue : Bigint.t) : Value.t =
  let width = Bigint.(hvalue - lvalue + one) in
  let value = slice_bitstring value hvalue lvalue in
  BitV (width, value)

let eval_bitstring_access (value : Value.t) (lvalue : Value.t)
    (hvalue : Value.t) : Value.t =
  let extract value = extract_bigint value in
  eval_bitstring_access' (extract value) (extract lvalue) (extract hvalue)

(* Type cast evaluation *)

let eval_cast_to_bool (value : Value.t) : Value.t =
  match value with
  | BoolV b -> BoolV b
  | BitV (width, value) when width = Bigint.one -> BoolV Bigint.(value = one)
  | AIntV value -> BoolV Bigint.(value = one)
  | _ -> failwith "cast to bool undefined"

let eval_cast_to_bit (width : Bigint.t) (value : Value.t) : Value.t =
  match value with
  | BoolV b ->
      let value = if b then Bigint.one else Bigint.zero in
      BitV (width, value)
  | IntV (_, value) | BitV (_, value) | AIntV value ->
      bit_of_raw_int value width
  | _ ->
      Format.asprintf "(TODO) Cast to bitstring undefined: %a" Value.pp value
      |> failwith

let eval_cast_to_int (width : Bigint.t) (value : Value.t) : Value.t =
  match value with
  | BitV (_, value) | IntV (_, value) | AIntV value ->
      int_of_raw_int value width
  | _ ->
      Format.asprintf "(TODO) Cast to integer undefined: %a" Value.pp value
      |> failwith

let rec eval_cast_entries (entries : (string * Type.t) list) (value : Value.t) :
    (string * Value.t) list =
  match value with
  | TupleV values ->
      assert (List.length entries = List.length values);
      List.map2
        (fun (field, typ) value ->
          let value = eval_cast typ value in
          (field, value))
        entries values
  | HeaderV (_, entries) | StructV entries -> entries
  | _ ->
      Format.asprintf "(TODO) Cast to entries undefined: %a" Value.pp value
      |> failwith

and eval_cast_tuple (typs : Type.t list) (value : Value.t) : Value.t =
  match value with
  | TupleV values ->
      assert (List.length typs = List.length values);
      let values = List.map2 eval_cast typs values in
      TupleV values
  | _ ->
      Format.asprintf "(TODO) Cast to tuple undefined: %a" Value.pp value
      |> failwith

and eval_cast (typ : Type.t) (value : Value.t) : Value.t =
  match typ with
  | BoolT -> eval_cast_to_bool value
  | AIntT ->
      let value = extract_bigint value in
      AIntV value
  | BitT width -> eval_cast_to_bit width value
  | IntT width -> eval_cast_to_int width value
  | TupleT typs -> eval_cast_tuple typs value
  | HeaderT entries ->
      let entries = eval_cast_entries entries value in
      HeaderV (true, entries)
  | StructT entries ->
      let entries = eval_cast_entries entries value in
      StructV entries
  | _ ->
      Format.asprintf "(TODO) Cast to type %a undefined" Type.pp typ |> failwith

let rec eval_default_value (typ : Type.t) : Value.t =
  match typ with
  | BoolT -> BoolV false
  | ErrT -> ErrV ""
  | StrT -> StrV ""
  | AIntT -> AIntV Bigint.zero
  | IntT width -> IntV (width, Bigint.zero)
  | BitT width -> BitV (width, Bigint.zero)
  | VBitT width -> VBitV (width, Bigint.zero)
  | TupleT types -> TupleV (List.map eval_default_value types)
  | StructT entries ->
      let entries =
        List.map (fun (name, typ) -> (name, eval_default_value typ)) entries
      in
      StructV entries
  | HeaderT entries ->
      let entries =
        List.map (fun (name, typ) -> (name, eval_default_value typ)) entries
      in
      HeaderV (false, entries)
  | _ ->
      Format.asprintf "(TODO) default_value: not implemented for %a" Type.pp typ
      |> failwith
