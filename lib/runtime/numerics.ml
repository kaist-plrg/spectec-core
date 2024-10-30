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

module Type = Types.Type
open Util.Source

(* Bit manipulation *)

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

let power_of_two (w : Bigint.t) : Bigint.t = shift_bitstring_left Bigint.one w

let slice_bitstring (n : Bigint.t) (m : Bigint.t) (l : Bigint.t) : Bigint.t =
  let slice_width = Bigint.(m + one - l) in
  if Bigint.(l < zero) then
    raise (Invalid_argument "bitslice x[y:z] must have y > z > 0");
  let shifted = Bigint.(n asr to_int_exn l) in
  let mask = Bigint.(power_of_two slice_width - one) in
  Bigint.bit_and shifted mask

let rec bitwise_neg (n : Bigint.t) (w : Bigint.t) : Bigint.t =
  if Bigint.(w > zero) then
    let w' = power_of_two Bigint.(w - one) in
    let g = slice_bitstring n Bigint.(w - one) Bigint.(w - one) in
    if Bigint.(g = zero) then bitwise_neg Bigint.(n + w') Bigint.(w - one)
    else bitwise_neg Bigint.(n - w') Bigint.(w - one)
  else n

let add_one_complement (v : Bigint.t) (w : Bigint.t) : Bigint.t =
  let tmp = Bigint.(v + w) in
  let thres = power_of_two (Bigint.of_int 16) in
  if Bigint.(tmp >= thres) then Bigint.((tmp % thres) + one)
  else Bigint.(tmp % thres)

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

let bit_of_raw_int (n : Bigint.t) (w : Bigint.t) : Value.t =
  FBitV (w, of_two_complement n w)

let int_of_raw_int (n : Bigint.t) (w : Bigint.t) : Value.t =
  FIntV (w, of_two_complement n w)

(* Unop evaluation *)

let eval_unop_not (value : Value.t) : Value.t =
  match value with
  | BoolV b -> BoolV (not b)
  | _ -> Format.asprintf "Not a boolean value: %a" Value.pp value |> failwith

let eval_unop_bitnot (value : Value.t) : Value.t =
  match value with
  | FBitV (width, value) ->
      let value = bitwise_neg value width in
      FBitV (width, value)
  | _ -> Format.asprintf "Not a bit value: %a" Value.pp value |> failwith

let eval_unop_uminus (value : Value.t) : Value.t =
  match value with
  | IntV value -> IntV (Bigint.neg value)
  | FIntV (width, value) ->
      let value = to_two_complement (Bigint.neg value) width in
      FIntV (width, value)
  | FBitV (width, value) ->
      let value = Bigint.(power_of_two width - value) in
      FBitV (width, value)
  | _ -> Format.asprintf "Not an integer value: %a" Value.pp value |> failwith

let eval_unop (op : Lang.Ast.unop) (value : Value.t) : Value.t =
  match op.it with
  | BNotOp -> eval_unop_bitnot value
  | LNotOp -> eval_unop_not value
  | UMinusOp -> eval_unop_uminus value

(* Binop evaluation *)

let unsigned_op_sat (l : Bigint.t) (r : Bigint.t) (w : Bigint.t)
    (op : Bigint.t -> Bigint.t -> Bigint.t) : Value.t =
  let x = power_of_two w in
  let n =
    let n = op l r in
    if Bigint.(n > zero) then Bigint.min n Bigint.(x - one)
    else Bigint.max n Bigint.zero
  in
  FBitV (w, n)

let signed_op_sat (l : Bigint.t) (r : Bigint.t) (w : Bigint.t)
    (op : Bigint.t -> Bigint.t -> Bigint.t) : Value.t =
  let x = power_of_two Bigint.(w - one) in
  let n =
    let n = op l r in
    if Bigint.(n > zero) then Bigint.min n Bigint.(x - one)
    else Bigint.max n Bigint.(-x)
  in
  FIntV (w, n)

let rec eval_binop_plus (lvalue : Value.t) (rvalue : Value.t) : Value.t =
  match (lvalue, rvalue) with
  | FBitV (width, lvalue), FBitV (_, rvalue) ->
      let value = of_two_complement Bigint.(lvalue + rvalue) width in
      FBitV (width, value)
  | FIntV (width, lvalue), FIntV (_, rvalue) ->
      let value = of_two_complement Bigint.(lvalue + rvalue) width in
      FIntV (width, value)
  | FBitV (width, _), IntV rvalue ->
      eval_binop_plus lvalue (bit_of_raw_int rvalue width)
  | IntV lvalue, FBitV (width, _) ->
      eval_binop_plus (bit_of_raw_int lvalue width) rvalue
  | FIntV (width, _), IntV rvalue ->
      eval_binop_plus lvalue (int_of_raw_int rvalue width)
  | IntV lvalue, FIntV (width, _) ->
      eval_binop_plus (int_of_raw_int lvalue width) rvalue
  | IntV lvalue, IntV rvalue -> IntV Bigint.(lvalue + rvalue)
  | _ ->
      Format.asprintf "Invalid addition: %a + %a" Value.pp lvalue Value.pp
        rvalue
      |> failwith

let rec eval_binop_plussat (lvalue : Value.t) (rvalue : Value.t) : Value.t =
  match (lvalue, rvalue) with
  | FBitV (width, lvalue), FBitV (_, rvalue) ->
      unsigned_op_sat lvalue rvalue width Bigint.( + )
  | FIntV (width, lvalue), FIntV (_, rvalue) ->
      signed_op_sat lvalue rvalue width Bigint.( + )
  | FBitV (width, _), IntV rvalue ->
      eval_binop_plussat lvalue (bit_of_raw_int rvalue width)
  | IntV lvalue, FBitV (width, _) ->
      eval_binop_plussat (bit_of_raw_int lvalue width) rvalue
  | FIntV (width, _), IntV rvalue ->
      eval_binop_plussat lvalue (int_of_raw_int rvalue width)
  | IntV lvalue, FIntV (width, _) ->
      eval_binop_plussat (int_of_raw_int lvalue width) rvalue
  | _ ->
      Format.asprintf "Invalid addition with saturation: %a (+) %a" Value.pp
        lvalue Value.pp rvalue
      |> failwith

let rec eval_binop_minus (lvalue : Value.t) (rvalue : Value.t) : Value.t =
  match (lvalue, rvalue) with
  | FBitV (width, lvalue), FBitV (_, rvalue) ->
      let value = of_two_complement Bigint.(lvalue - rvalue) width in
      FBitV (width, value)
  | FIntV (width, lvalue), FIntV (_, rvalue) ->
      let value = of_two_complement Bigint.(lvalue - rvalue) width in
      FIntV (width, value)
  | FBitV (width, _), IntV rvalue ->
      eval_binop_minus lvalue (bit_of_raw_int rvalue width)
  | IntV lvalue, FBitV (width, _) ->
      eval_binop_minus (bit_of_raw_int lvalue width) rvalue
  | FIntV (width, _), IntV rvalue ->
      eval_binop_minus lvalue (int_of_raw_int rvalue width)
  | IntV lvalue, FIntV (width, _) ->
      eval_binop_minus (int_of_raw_int lvalue width) rvalue
  | IntV lvalue, IntV rvalue -> IntV Bigint.(lvalue - rvalue)
  | _ ->
      Format.asprintf "Invalid subtraction: %a - %a" Value.pp lvalue Value.pp
        rvalue
      |> failwith

let rec eval_binop_minussat (lvalue : Value.t) (rvalue : Value.t) : Value.t =
  match (lvalue, rvalue) with
  | FBitV (width, lvalue), FBitV (_, rvalue) ->
      unsigned_op_sat lvalue rvalue width Bigint.( - )
  | FIntV (width, lvalue), FIntV (_, rvalue) ->
      signed_op_sat lvalue rvalue width Bigint.( - )
  | FBitV (width, _), IntV rvalue ->
      eval_binop_minussat lvalue (bit_of_raw_int rvalue width)
  | IntV lvalue, FBitV (width, _) ->
      eval_binop_minussat (bit_of_raw_int lvalue width) rvalue
  | FIntV (width, _), IntV rvalue ->
      eval_binop_minussat lvalue (int_of_raw_int rvalue width)
  | IntV lvalue, FIntV (width, _) ->
      eval_binop_minussat (int_of_raw_int lvalue width) rvalue
  | _ ->
      Format.asprintf "Invalid subtraction with saturation: %a (-) %a" Value.pp
        lvalue Value.pp rvalue
      |> failwith

let rec eval_binop_mul (lvalue : Value.t) (rvalue : Value.t) : Value.t =
  match (lvalue, rvalue) with
  | FBitV (width, lvalue), FBitV (_, rvalue) ->
      let value = of_two_complement Bigint.(lvalue * rvalue) width in
      FBitV (width, value)
  | FIntV (width, lvalue), FIntV (_, rvalue) ->
      let value = to_two_complement Bigint.(lvalue * rvalue) width in
      FIntV (width, value)
  | FBitV (width, _), IntV rvalue ->
      eval_binop_mul lvalue (bit_of_raw_int rvalue width)
  | IntV lvalue, FBitV (width, _) ->
      eval_binop_mul (bit_of_raw_int lvalue width) rvalue
  | FIntV (width, _), IntV rvalue ->
      eval_binop_mul lvalue (int_of_raw_int rvalue width)
  | IntV lvalue, FIntV (width, _) ->
      eval_binop_mul (int_of_raw_int lvalue width) rvalue
  | IntV lvalue, IntV rvalue -> IntV Bigint.(lvalue * rvalue)
  | _ ->
      Format.asprintf "Invalid multiplication: %a * %a" Value.pp lvalue Value.pp
        rvalue
      |> failwith

let eval_binop_div (lvalue : Value.t) (rvalue : Value.t) : Value.t =
  match (lvalue, rvalue) with
  | IntV lvalue, IntV rvalue -> IntV Bigint.(lvalue / rvalue)
  | FBitV (width, lvalue), FBitV (_, rvalue) ->
      let value = Bigint.(lvalue / rvalue) in
      FBitV (width, value)
  | _ ->
      Format.asprintf "Invalid division: %a / %a" Value.pp lvalue Value.pp
        rvalue
      |> failwith

let eval_binop_mod (lvalue : Value.t) (rvalue : Value.t) : Value.t =
  match (lvalue, rvalue) with
  | IntV lvalue, IntV rvalue -> IntV Bigint.(lvalue % rvalue)
  | FBitV (width, lvalue), FBitV (_, rvalue) ->
      let value = Bigint.(lvalue % rvalue) in
      FBitV (width, value)
  | _ ->
      Format.asprintf "Invalid modulo: %a %% %a" Value.pp lvalue Value.pp rvalue
      |> failwith

let eval_binop_shl (lvalue : Value.t) (rvalue : Value.t) : Value.t =
  match (lvalue, rvalue) with
  | FBitV (width, lvalue), FBitV (_, rvalue)
  | FBitV (width, lvalue), IntV rvalue ->
      let value =
        of_two_complement (shift_bitstring_left lvalue rvalue) width
      in
      FBitV (width, value)
  | FIntV (width, lvalue), FBitV (_, rvalue)
  | FIntV (width, lvalue), IntV rvalue ->
      let value =
        to_two_complement (shift_bitstring_left lvalue rvalue) width
      in
      FIntV (width, value)
  | IntV lvalue, IntV rvalue ->
      let value = shift_bitstring_left lvalue rvalue in
      IntV value
  | FBitV (width, lvalue), FIntV (_, rvalue) ->
      let value =
        of_two_complement (shift_bitstring_left lvalue rvalue) width
      in
      FBitV (width, value)
  | FIntV (width, lvalue), FIntV (_, rvalue) ->
      let value =
        to_two_complement (shift_bitstring_left lvalue rvalue) width
      in
      FIntV (width, value)
  | _ ->
      Format.asprintf "Invalid shift left: %a << %a" Value.pp lvalue Value.pp
        rvalue
      |> failwith

let eval_binop_shr (lvalue : Value.t) (rvalue : Value.t) : Value.t =
  match (lvalue, rvalue) with
  | FBitV (width, lvalue), FBitV (_, rvalue)
  | FBitV (width, lvalue), IntV rvalue ->
      let value =
        of_two_complement
          (shift_bitstring_right lvalue rvalue false Bigint.zero)
          width
      in
      FBitV (width, value)
  | FIntV (width, lvalue), FBitV (_, rvalue)
  | FIntV (width, lvalue), FIntV (_, rvalue)
  | FIntV (width, lvalue), IntV rvalue ->
      let exp = power_of_two Bigint.(width - one) in
      let arith = Bigint.(of_two_complement lvalue width > exp) in
      let value =
        to_two_complement (shift_bitstring_right lvalue rvalue arith exp) width
      in
      FIntV (width, value)
  | IntV lvalue, IntV rvalue ->
      let value = shift_bitstring_right lvalue rvalue false Bigint.zero in
      IntV value
  | FBitV (width, lvalue), FIntV (_, rvalue) ->
      let value =
        of_two_complement
          (shift_bitstring_right lvalue rvalue false Bigint.zero)
          width
      in
      FBitV (width, value)
  | _ ->
      Format.asprintf "Invalid shift right: %a >> %a" Value.pp lvalue Value.pp
        rvalue
      |> failwith

let rec eval_binop_le (lvalue : Value.t) (rvalue : Value.t) : Value.t =
  match (lvalue, rvalue) with
  | FBitV (_, lvalue), FBitV (_, rvalue) | IntV lvalue, IntV rvalue ->
      BoolV Bigint.(lvalue <= rvalue)
  | IntV lvalue, FBitV (width, _) ->
      eval_binop_le (bit_of_raw_int lvalue width) rvalue
  | FBitV (width, _), IntV rvalue ->
      eval_binop_le lvalue (bit_of_raw_int rvalue width)
  | IntV lvalue, FIntV (width, _) ->
      eval_binop_le (int_of_raw_int lvalue width) rvalue
  | FIntV (width, _), IntV rvalue ->
      eval_binop_le lvalue (int_of_raw_int rvalue width)
  | _ ->
      Format.asprintf "Invalid less than or equal: %a <= %a" Value.pp lvalue
        Value.pp rvalue
      |> failwith

let rec eval_binop_ge (lvalue : Value.t) (rvalue : Value.t) : Value.t =
  match (lvalue, rvalue) with
  | FBitV (_, lvalue), FBitV (_, rvalue) | IntV lvalue, IntV rvalue ->
      BoolV Bigint.(lvalue >= rvalue)
  | IntV lvalue, FBitV (width, _) ->
      eval_binop_ge (bit_of_raw_int lvalue width) rvalue
  | FBitV (width, _), IntV rvalue ->
      eval_binop_ge lvalue (bit_of_raw_int rvalue width)
  | IntV lvalue, FIntV (width, _) ->
      eval_binop_ge (int_of_raw_int lvalue width) rvalue
  | FIntV (width, _), IntV rvalue ->
      eval_binop_ge lvalue (int_of_raw_int rvalue width)
  | _ ->
      Format.asprintf "Invalid greater than or equal: %a >= %a" Value.pp lvalue
        Value.pp rvalue
      |> failwith

let rec eval_binop_lt (lvalue : Value.t) (rvalue : Value.t) : Value.t =
  match (lvalue, rvalue) with
  | FBitV (_, lvalue), FBitV (_, rvalue) | IntV lvalue, IntV rvalue ->
      BoolV Bigint.(lvalue < rvalue)
  | IntV lvalue, FBitV (width, _) ->
      eval_binop_lt (bit_of_raw_int lvalue width) rvalue
  | FBitV (width, _), IntV rvalue ->
      eval_binop_lt lvalue (bit_of_raw_int rvalue width)
  | IntV lvalue, FIntV (width, _) ->
      eval_binop_lt (int_of_raw_int lvalue width) rvalue
  | FIntV (width, _), IntV rvalue ->
      eval_binop_lt lvalue (int_of_raw_int rvalue width)
  | _ ->
      Format.asprintf "Invalid less than: %a < %a" Value.pp lvalue Value.pp
        rvalue
      |> failwith

let rec eval_binop_gt (lvalue : Value.t) (rvalue : Value.t) : Value.t =
  match (lvalue, rvalue) with
  | FBitV (_, lvalue), FBitV (_, rvalue) | IntV lvalue, IntV rvalue ->
      BoolV Bigint.(lvalue > rvalue)
  | IntV lvalue, FBitV (width, _) ->
      eval_binop_gt (bit_of_raw_int lvalue width) rvalue
  | FBitV (width, _), IntV rvalue ->
      eval_binop_gt lvalue (bit_of_raw_int rvalue width)
  | IntV lvalue, FIntV (width, _) ->
      eval_binop_gt (int_of_raw_int lvalue width) rvalue
  | FIntV (width, _), IntV rvalue ->
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
  | IntV lvalue, IntV rvalue
  | FBitV (_, lvalue), FBitV (_, rvalue)
  | FIntV (_, lvalue), FIntV (_, rvalue)
  | VBitV (_, _, lvalue), VBitV (_, _, rvalue) ->
      Bigint.(lvalue = rvalue)
  | FBitV (width, _), IntV rvalue ->
      eval_binop_eq lvalue (bit_of_raw_int rvalue width)
  | IntV lvalue, FBitV (width, _) ->
      eval_binop_eq (bit_of_raw_int lvalue width) rvalue
  | FIntV (width, _), IntV rvalue ->
      eval_binop_eq lvalue (int_of_raw_int rvalue width)
  | IntV lvalue, FIntV (width, _) ->
      eval_binop_eq (int_of_raw_int lvalue width) rvalue
  | StackV (lvalues, _, _), StackV (rvalues, _, _)
  | TupleV lvalues, TupleV rvalues ->
      List.length lvalues = List.length rvalues
      && List.for_all2 eval_binop_eq lvalues rvalues
  | StructV lentries, StructV rentries | UnionV lentries, UnionV rentries ->
      List.length lentries = List.length rentries
      && eval_binop_eq_entries lentries rentries
  | HeaderV (lvalid, lentries), HeaderV (rvalid, rentries) ->
      lvalid = rvalid && eval_binop_eq_entries lentries rentries
  | EnumFieldV (lid, lmember), EnumFieldV (rid, rmember) ->
      lid = rid && lmember = rmember
  | SEnumFieldV (lid, lmember, lvalue), SEnumFieldV (rid, rmember, rvalue) ->
      lid = rid && lmember = rmember && eval_binop_eq lvalue rvalue
  | _ ->
      Format.asprintf "Invalid equality: %a == %a" Value.pp lvalue Value.pp
        rvalue
      |> failwith

let eval_binop_ne (lvalue : Value.t) (rvalue : Value.t) : bool =
  not (eval_binop_eq lvalue rvalue)

let rec eval_binop_bitand (lvalue : Value.t) (rvalue : Value.t) : Value.t =
  match (lvalue, rvalue) with
  | FBitV (width, lvalue), FBitV (_, rvalue) ->
      let value = Bigint.bit_and lvalue rvalue in
      FBitV (width, value)
  | FBitV (width, _), IntV rvalue ->
      eval_binop_bitand lvalue (bit_of_raw_int rvalue width)
  | IntV lvalue, FBitV (width, _) ->
      eval_binop_bitand (bit_of_raw_int lvalue width) rvalue
  | _ ->
      Format.asprintf "Invalid bitwise and: %a & %a" Value.pp lvalue Value.pp
        rvalue
      |> failwith

let rec eval_binop_bitxor (lvalue : Value.t) (rvalue : Value.t) : Value.t =
  match (lvalue, rvalue) with
  | FBitV (width, lvalue), FBitV (_, rvalue) ->
      let value = Bigint.bit_xor lvalue rvalue in
      FBitV (width, value)
  | FBitV (width, _), IntV rvalue ->
      eval_binop_bitxor lvalue (bit_of_raw_int rvalue width)
  | IntV lvalue, FBitV (width, _) ->
      eval_binop_bitxor (bit_of_raw_int lvalue width) rvalue
  | _ ->
      Format.asprintf "Invalid bitwise xor: %a ^ %a" Value.pp lvalue Value.pp
        rvalue
      |> failwith

let rec eval_binop_bitor (lvalue : Value.t) (rvalue : Value.t) : Value.t =
  match (lvalue, rvalue) with
  | FBitV (width, lvalue), FBitV (_, rvalue) ->
      let value = Bigint.bit_or lvalue rvalue in
      FBitV (width, value)
  | FBitV (width, _), IntV rvalue ->
      eval_binop_bitor lvalue (bit_of_raw_int rvalue width)
  | IntV lvalue, FBitV (width, _) ->
      eval_binop_bitor (bit_of_raw_int lvalue width) rvalue
  | _ ->
      Format.asprintf "Invalid bitwise or: %a | %a" Value.pp lvalue Value.pp
        rvalue
      |> failwith

let rec eval_binop_plusplus (lvalue : Value.t) (rvalue : Value.t) : Value.t =
  match (lvalue, rvalue) with
  | FBitV (lwidth, lvalue), FBitV (rwidth, rvalue) ->
      let value = Bigint.(shift_bitstring_left lvalue rwidth + rvalue) in
      let width = Bigint.(lwidth + rwidth) in
      FBitV (width, value)
  | FBitV (width, _), IntV rvalue ->
      eval_binop_plusplus lvalue (bit_of_raw_int rvalue width)
  | IntV lvalue, FBitV (width, _) ->
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

let eval_binop (op : Lang.Ast.binop) (lvalue : Value.t) (rvalue : Value.t) :
    Value.t =
  match op.it with
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

let eval_bitstring_access' (value : Bigint.t) (hvalue : Bigint.t)
    (lvalue : Bigint.t) : Value.t =
  let width = Bigint.(hvalue - lvalue + one) in
  let value = slice_bitstring value hvalue lvalue in
  FBitV (width, value)

let eval_bitstring_access (value : Value.t) (hvalue : Value.t)
    (lvalue : Value.t) : Value.t =
  let extract value = Value.get_num value in
  eval_bitstring_access' (extract value) (extract hvalue) (extract lvalue)

(* Type cast evaluation *)

let eval_cast_to_bool (value : Value.t) : Value.t =
  match value with
  | BoolV b -> BoolV b
  | FBitV (width, value) when width = Bigint.one -> BoolV Bigint.(value = one)
  | IntV value -> BoolV Bigint.(value = one)
  | _ -> Format.asprintf "Cast to bool undefined" |> failwith

let rec eval_cast_to_bit (width : Bigint.t) (value : Value.t) : Value.t =
  match value with
  | BoolV b ->
      let value = if b then Bigint.one else Bigint.zero in
      FBitV (width, value)
  | FIntV (_, value) | FBitV (_, value) | IntV value ->
      bit_of_raw_int value width
  | SEnumFieldV (_, _, value) -> eval_cast_to_bit width value
  | _ ->
      Format.asprintf "(TODO) Cast to bitstring undefined: %a" Value.pp value
      |> failwith

let rec eval_cast_to_int (width : Bigint.t) (value : Value.t) : Value.t =
  match value with
  | FBitV (_, value) | FIntV (_, value) | IntV value ->
      int_of_raw_int value width
  | SEnumFieldV (_, _, value) -> eval_cast_to_int width value
  | _ ->
      Format.asprintf "(TODO) Cast to integer undefined: %a" Value.pp value
      |> failwith

let rec eval_cast_fields (fields_typ : (string * Type.t) list) (value : Value.t)
    : (string * Value.t) list =
  match value with
  | StructV fields_value | RecordV fields_value ->
      List.fold_left
        (fun fields (member_typ, typ) ->
          let value = List.assoc member_typ fields_value in
          let value = eval_cast typ value in
          fields @ [ (member_typ, value) ])
        [] fields_typ
  | SeqV values ->
      assert (List.length fields_typ = List.length values);
      List.map2
        (fun (member, typ) value ->
          let value = eval_cast typ value in
          (member, value))
        fields_typ values
  | _ ->
      Format.asprintf "(TODO) Cast to entries undefined: %a" Value.pp value
      |> failwith

and eval_cast_tuple (typs : Type.t list) (value : Value.t) : Value.t =
  match value with
  | SeqV values ->
      assert (List.length typs = List.length values);
      let values = List.map2 eval_cast typs values in
      TupleV values
  | _ ->
      Format.asprintf "(TODO) Cast to tuple undefined: %a" Value.pp value
      |> failwith

and eval_cast (typ : Type.t) (value : Value.t) : Value.t =
  match typ with
  | BoolT -> eval_cast_to_bool value
  | IntT ->
      let value = Value.get_num value in
      IntV value
  | FBitT width -> eval_cast_to_bit width value
  | FIntT width -> eval_cast_to_int width value
  | NewT (_id, typ_inner) -> eval_cast typ_inner value
  | TupleT typs_inner -> eval_cast_tuple typs_inner value
  | StructT (_id, fields_typ) ->
      let fields = eval_cast_fields fields_typ value in
      StructV fields
  | HeaderT (_id, fields_typ) ->
      let fields = eval_cast_fields fields_typ value in
      HeaderV (true, fields)
  | _ ->
      Format.asprintf "(TODO) Cast from %a to type %a undefined" Value.pp value
        Type.pp typ
      |> failwith

(* Size evaluation *)

let rec eval_min_size_in_bits' (typ : Type.t) : Bigint.t =
  match typ with
  | BoolT -> Bigint.one
  | FBitT width | FIntT width -> width
  | VBitT _ -> Bigint.zero
  | SEnumT (_, typ_inner) -> eval_min_size_in_bits' typ_inner
  | TupleT typs_inner ->
      List.fold_left
        (fun size typ_inner -> Bigint.(size + eval_min_size_in_bits' typ_inner))
        Bigint.zero typs_inner
  | StackT (typ_inner, size) -> Bigint.(size * eval_min_size_in_bits' typ_inner)
  | StructT (_id, fields) | HeaderT (_id, fields) ->
      List.fold_left
        (fun size (_, typ_inner) ->
          Bigint.(size + eval_min_size_in_bits' typ_inner))
        Bigint.zero fields
  | UnionT (_id, fields) ->
      let sizes =
        List.map (fun (_, typ_inner) -> eval_min_size_in_bits' typ_inner) fields
      in
      List.fold_left Bigint.min Bigint.zero sizes
  | _ ->
      Format.asprintf "(TODO) Size of type %a undefined" Type.pp typ |> failwith

let eval_min_size_in_bits (typ : Type.t) : Value.t =
  let size = eval_min_size_in_bits' typ in
  IntV size

let eval_min_size_in_bytes (typ : Type.t) : Value.t =
  let size = eval_min_size_in_bits' typ in
  let size = Bigint.((size + of_int 7) asr 3) in
  IntV size

let rec eval_max_size_in_bits' (typ : Type.t) : Bigint.t =
  match typ with
  | BoolT -> Bigint.one
  | FBitT width | FIntT width | VBitT width -> width
  | SEnumT (_, typ_inner) -> eval_max_size_in_bits' typ_inner
  | TupleT typs_inner ->
      List.fold_left
        (fun size typ_inner -> Bigint.(size + eval_max_size_in_bits' typ_inner))
        Bigint.zero typs_inner
  | StackT (typ_inner, size) -> Bigint.(size * eval_max_size_in_bits' typ_inner)
  | StructT (_id, fields) | HeaderT (_id, fields) ->
      List.fold_left
        (fun size (_, typ_inner) ->
          Bigint.(size + eval_max_size_in_bits' typ_inner))
        Bigint.zero fields
  | UnionT (_id, fields) ->
      let sizes =
        List.map (fun (_, typ_inner) -> eval_max_size_in_bits' typ_inner) fields
      in
      List.fold_left Bigint.max Bigint.zero sizes
  | _ ->
      Format.asprintf "(TODO) Size of type %a undefined" Type.pp typ |> failwith

let eval_max_size_in_bits (typ : Type.t) : Value.t =
  let size = eval_max_size_in_bits' typ in
  IntV size

let eval_max_size_in_bytes (typ : Type.t) : Value.t =
  let size = eval_max_size_in_bits' typ in
  let size = Bigint.((size + of_int 7) asr 3) in
  IntV size

let eval_size (typ : Type.t) (member : string) : Value.t =
  match member with
  | "minSizeInBits" -> eval_min_size_in_bits typ
  | "minSizeInBytes" -> eval_min_size_in_bytes typ
  | "maxSizeInBits" -> eval_max_size_in_bits typ
  | "maxSizeInBytes" -> eval_max_size_in_bytes typ
  | _ -> assert false
