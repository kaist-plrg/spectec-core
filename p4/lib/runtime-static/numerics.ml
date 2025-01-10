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

module Type = Tdomain.Types.Type
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
  | _ ->
      Format.asprintf "Not a boolean value: %a" (Value.pp ~level:0) value
      |> failwith

let eval_unop_bitnot (value : Value.t) : Value.t =
  match value with
  | FBitV (width, value) ->
      let value = bitwise_neg value width in
      FBitV (width, value)
  | _ ->
      Format.asprintf "Not a bit value: %a" (Value.pp ~level:0) value
      |> failwith

let eval_unop_uminus (value : Value.t) : Value.t =
  match value with
  | IntV value -> IntV (Bigint.neg value)
  | FIntV (width, value) ->
      let value = to_two_complement (Bigint.neg value) width in
      FIntV (width, value)
  | FBitV (width, value) ->
      let value = Bigint.(power_of_two width - value) in
      FBitV (width, value)
  | _ ->
      Format.asprintf "Not an integer value: %a" (Value.pp ~level:0) value
      |> failwith

let eval_unop (op : Lang.Ast.unop) (value : Value.t) : Value.t =
  match op.it with
  | BNotOp -> eval_unop_bitnot value
  | LNotOp -> eval_unop_not value
  | UPlusOp -> value
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
      Format.asprintf "Invalid addition: %a + %a" (Value.pp ~level:0) lvalue
        (Value.pp ~level:0) rvalue
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
      Format.asprintf "Invalid addition with saturation: %a (+) %a"
        (Value.pp ~level:0) lvalue (Value.pp ~level:0) rvalue
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
      Format.asprintf "Invalid subtraction: %a - %a" (Value.pp ~level:0) lvalue
        (Value.pp ~level:0) rvalue
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
      Format.asprintf "Invalid subtraction with saturation: %a (-) %a"
        (Value.pp ~level:0) lvalue (Value.pp ~level:0) rvalue
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
      Format.asprintf "Invalid multiplication: %a * %a" (Value.pp ~level:0)
        lvalue (Value.pp ~level:0) rvalue
      |> failwith

let eval_binop_div (lvalue : Value.t) (rvalue : Value.t) : Value.t =
  match (lvalue, rvalue) with
  | IntV lvalue, IntV rvalue -> IntV Bigint.(lvalue / rvalue)
  | FBitV (width, lvalue), FBitV (_, rvalue) ->
      let value = Bigint.(lvalue / rvalue) in
      FBitV (width, value)
  | _ ->
      Format.asprintf "Invalid division: %a / %a" (Value.pp ~level:0) lvalue
        (Value.pp ~level:0) rvalue
      |> failwith

let eval_binop_mod (lvalue : Value.t) (rvalue : Value.t) : Value.t =
  match (lvalue, rvalue) with
  | IntV lvalue, IntV rvalue -> IntV Bigint.(lvalue % rvalue)
  | FBitV (width, lvalue), FBitV (_, rvalue) ->
      let value = Bigint.(lvalue % rvalue) in
      FBitV (width, value)
  | _ ->
      Format.asprintf "Invalid modulo: %a %% %a" (Value.pp ~level:0) lvalue
        (Value.pp ~level:0) rvalue
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
      Format.asprintf "Invalid shift left: %a << %a" (Value.pp ~level:0) lvalue
        (Value.pp ~level:0) rvalue
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
      let lvalue = of_two_complement lvalue width in
      let exp = power_of_two Bigint.(width - one) in
      let arith = Bigint.(lvalue > exp) in
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
      Format.asprintf "Invalid shift right: %a >> %a" (Value.pp ~level:0) lvalue
        (Value.pp ~level:0) rvalue
      |> failwith

let rec eval_binop_le (lvalue : Value.t) (rvalue : Value.t) : Value.t =
  match (lvalue, rvalue) with
  | FIntV (_, lvalue), FIntV (_, rvalue)
  | FBitV (_, lvalue), FBitV (_, rvalue)
  | IntV lvalue, IntV rvalue ->
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
      Format.asprintf "Invalid less than or equal: %a <= %a" (Value.pp ~level:0)
        lvalue (Value.pp ~level:0) rvalue
      |> failwith

let rec eval_binop_ge (lvalue : Value.t) (rvalue : Value.t) : Value.t =
  match (lvalue, rvalue) with
  | FIntV (_, lvalue), FIntV (_, rvalue)
  | FBitV (_, lvalue), FBitV (_, rvalue)
  | IntV lvalue, IntV rvalue ->
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
      Format.asprintf "Invalid greater than or equal: %a >= %a"
        (Value.pp ~level:0) lvalue (Value.pp ~level:0) rvalue
      |> failwith

let rec eval_binop_lt (lvalue : Value.t) (rvalue : Value.t) : Value.t =
  match (lvalue, rvalue) with
  | FIntV (_, lvalue), FIntV (_, rvalue)
  | FBitV (_, lvalue), FBitV (_, rvalue)
  | IntV lvalue, IntV rvalue ->
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
      Format.asprintf "Invalid less than: %a < %a" (Value.pp ~level:0) lvalue
        (Value.pp ~level:0) rvalue
      |> failwith

let rec eval_binop_gt (lvalue : Value.t) (rvalue : Value.t) : Value.t =
  match (lvalue, rvalue) with
  | FIntV (_, lvalue), FIntV (_, rvalue)
  | FBitV (_, lvalue), FBitV (_, rvalue)
  | IntV lvalue, IntV rvalue ->
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
      Format.asprintf "Invalid greater than: %a > %a" (Value.pp ~level:0) lvalue
        (Value.pp ~level:0) rvalue
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
  | StructV (lid, lentries), StructV (rid, rentries)
  | UnionV (lid, lentries), UnionV (rid, rentries) ->
      lid = rid
      && List.length lentries = List.length rentries
      && eval_binop_eq_entries lentries rentries
  | HeaderV (lid, lvalid, lentries), HeaderV (rid, rvalid, rentries) ->
      lid = rid && lvalid = rvalid && eval_binop_eq_entries lentries rentries
  | EnumFieldV (lid, lmember), EnumFieldV (rid, rmember) ->
      lid = rid && lmember = rmember
  | SEnumFieldV (lid, lmember, lvalue), SEnumFieldV (rid, rmember, rvalue) ->
      lid = rid && lmember = rmember && eval_binop_eq lvalue rvalue
  | _ ->
      Format.asprintf "Invalid equality: %a == %a" (Value.pp ~level:0) lvalue
        (Value.pp ~level:0) rvalue
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
      Format.asprintf "Invalid bitwise and: %a & %a" (Value.pp ~level:0) lvalue
        (Value.pp ~level:0) rvalue
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
      Format.asprintf "Invalid bitwise xor: %a ^ %a" (Value.pp ~level:0) lvalue
        (Value.pp ~level:0) rvalue
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
      Format.asprintf "Invalid bitwise or: %a | %a" (Value.pp ~level:0) lvalue
        (Value.pp ~level:0) rvalue
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
      Format.asprintf "Invalid concatenation: %a ++ %a" (Value.pp ~level:0)
        lvalue (Value.pp ~level:0) rvalue
      |> failwith

let eval_binop_and (lvalue : Value.t) (rvalue : Value.t) : Value.t =
  match (lvalue, rvalue) with
  | BoolV b1, BoolV b2 -> BoolV (b1 && b2)
  | _ ->
      Format.asprintf "Invalid and operator: %a && %a" (Value.pp ~level:0)
        lvalue (Value.pp ~level:0) rvalue
      |> failwith

let eval_binop_or (lvalue : Value.t) (rvalue : Value.t) : Value.t =
  match (lvalue, rvalue) with
  | BoolV b1, BoolV b2 -> BoolV (b1 || b2)
  | _ ->
      Format.asprintf "Invalid or operator: %a || %a" (Value.pp ~level:0) lvalue
        (Value.pp ~level:0) rvalue
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

let rec eval_cast_bool (typ : Type.t) (b : bool) : Value.t =
  let typ = Type.canon typ in
  match typ with
  | SpecT _ | DefT _ -> assert false
  | BoolT -> BoolV b
  | FBitT width_to -> FBitV (width_to, if b then Bigint.one else Bigint.zero)
  | NewT (_, typ_inner) -> eval_cast_bool typ_inner b
  | _ ->
      Format.asprintf "(TODO) Cast from bool to type %a undefined"
        (Type.pp ~level:0) typ
      |> failwith

and eval_cast_int (typ : Type.t) (i : Bigint.t) : Value.t =
  let typ = Type.canon typ in
  match typ with
  | SpecT _ | DefT _ -> assert false
  | BoolT -> BoolV Bigint.(i = one)
  | IntT -> IntV i
  | FIntT width -> int_of_raw_int i width
  | FBitT width -> bit_of_raw_int i width
  | NewT (_, typ_inner) -> eval_cast_int typ_inner i
  | _ ->
      Format.asprintf
        "(TODO) Cast from arbitrary-precision int to type %a undefined"
        (Type.pp ~level:0) typ
      |> failwith

and eval_cast_fint (typ : Type.t) (width : Bigint.t) (i : Bigint.t) : Value.t =
  let typ = Type.canon typ in
  match typ with
  | SpecT _ | DefT _ -> assert false
  | IntT -> IntV i
  | FIntT width_to -> int_of_raw_int i width_to
  | FBitT width_to -> bit_of_raw_int i width_to
  | NewT (_, typ_inner) -> eval_cast_fint typ_inner width i
  | _ ->
      Format.asprintf
        "(TODO) Cast from fixed-precision int to type %a undefined"
        (Type.pp ~level:0) typ
      |> failwith

and eval_cast_fbit (typ : Type.t) (width : Bigint.t) (i : Bigint.t) : Value.t =
  let typ = Type.canon typ in
  match typ with
  | SpecT _ | DefT _ -> assert false
  | BoolT -> BoolV Bigint.(i = one)
  | IntT -> IntV i
  | FIntT width_to -> int_of_raw_int i width_to
  | FBitT width_to -> bit_of_raw_int i width_to
  | NewT (_, typ_inner) -> eval_cast_fbit typ_inner width i
  | _ ->
      Format.asprintf
        "(TODO) Cast from fixed-precision bit to type %a undefined"
        (Type.pp ~level:0) typ
      |> failwith

and eval_cast_senum_field (typ : Type.t) (_id : string) (_member : string)
    (value : Value.t) : Value.t =
  eval_cast typ value

and eval_cast_struct (typ : Type.t) (id_value : string)
    (fields_value : (string * Value.t) list) : Value.t =
  let typ = Type.canon typ in
  match typ with
  | SpecT _ | DefT _ -> assert false
  | StructT (id, _) when id = id_value -> StructV (id, fields_value)
  | _ ->
      Format.asprintf "(TODO) Cast from struct to type %a undefined"
        (Type.pp ~level:0) typ
      |> failwith

and eval_cast_header (typ : Type.t) (id_value : string) (valid : bool)
    (fields_value : (string * Value.t) list) : Value.t =
  let typ = Type.canon typ in
  match typ with
  | SpecT _ | DefT _ -> assert false
  | HeaderT (id, _) when id = id_value -> HeaderV (id, valid, fields_value)
  | _ ->
      Format.asprintf "(TODO) Cast from struct to type %a undefined"
        (Type.pp ~level:0) typ
      |> failwith

and eval_cast_seq (typ : Type.t) (values : Value.t list) : Value.t =
  let typ = Type.canon typ in
  match typ with
  | SpecT _ | DefT _ -> assert false
  | ListT typ_inner ->
      let values = List.map (eval_cast typ_inner) values in
      SeqV values
  | TupleT typs_inner ->
      let values = List.map2 eval_cast typs_inner values in
      TupleV values
  | StructT (id, fields) ->
      let members, typs_inner = List.split fields in
      let values = List.map2 eval_cast typs_inner values in
      let fields = List.combine members values in
      StructV (id, fields)
  | HeaderT (id, fields) ->
      let members, typs_inner = List.split fields in
      let values = List.map2 eval_cast typs_inner values in
      let fields = List.combine members values in
      HeaderV (id, true, fields)
  | _ ->
      Format.asprintf "(TODO) Cast from sequence to type %a undefined"
        (Type.pp ~level:0) typ
      |> failwith

and eval_cast_seq_default (typ : Type.t) (values : Value.t list) : Value.t =
  let typ = Type.canon typ in
  match typ with
  | SpecT _ | DefT _ -> assert false
  | TupleT typs_inner ->
      let before i = i < List.length values in
      let typs_inner_default =
        List.filteri (fun i _ -> not (before i)) typs_inner
      in
      let typs_inner = List.filteri (fun i _ -> before i) typs_inner in
      let values =
        List.map2 eval_cast typs_inner values
        @ List.map eval_default typs_inner_default
      in
      TupleV values
  | StructT (id, fields_typ) ->
      let before i = i < List.length values in
      let members, typs_inner = List.split fields_typ in
      let typs_inner_default =
        List.filteri (fun i _ -> not (before i)) typs_inner
      in
      let typs_inner = List.filteri (fun i _ -> before i) typs_inner in
      let values =
        List.map2 eval_cast typs_inner values
        @ List.map eval_default typs_inner_default
      in
      let fields = List.combine members values in
      StructV (id, fields)
  | HeaderT (id, fields_typ) ->
      let before i = i < List.length values in
      let members, typs_inner = List.split fields_typ in
      let typs_inner_default =
        List.filteri (fun i _ -> not (before i)) typs_inner
      in
      let typs_inner = List.filteri (fun i _ -> before i) typs_inner in
      let values =
        List.map2 eval_cast typs_inner values
        @ List.map eval_default typs_inner_default
      in
      let fields = List.combine members values in
      HeaderV (id, true, fields)
  | _ ->
      Format.asprintf "(TODO) Cast from default sequence to type %a undefined"
        (Type.pp ~level:0) typ
      |> failwith

and eval_cast_record (typ : Type.t) (fields_value : (string * Value.t) list) :
    Value.t =
  let typ = Type.canon typ in
  match typ with
  | SpecT _ | DefT _ -> assert false
  | StructT (id, fields_typ) ->
      let fields =
        List.fold_left
          (fun fields (member, typ) ->
            let value = List.assoc member fields_value in
            let value = eval_cast typ value in
            fields @ [ (member, value) ])
          [] fields_typ
      in
      StructV (id, fields)
  | HeaderT (id, fields_typ) ->
      let fields =
        List.fold_left
          (fun fields (member, typ) ->
            let value = List.assoc member fields_value in
            let value = eval_cast typ value in
            fields @ [ (member, value) ])
          [] fields_typ
      in
      HeaderV (id, true, fields)
  | _ ->
      Format.asprintf "(TODO) Cast from record to type %a undefined"
        (Type.pp ~level:0) typ
      |> failwith

and eval_cast_record_default (typ : Type.t)
    (fields_value : (string * Value.t) list) : Value.t =
  let typ = Type.canon typ in
  match typ with
  | SpecT _ | DefT _ -> assert false
  | StructT (id, fields_typ) ->
      let fields =
        List.fold_left
          (fun fields (member, typ) ->
            let value =
              match List.assoc_opt member fields_value with
              | Some value -> eval_cast typ value
              | None -> eval_default typ
            in
            fields @ [ (member, value) ])
          [] fields_typ
      in
      StructV (id, fields)
  | HeaderT (id, fields_typ) ->
      let fields =
        List.fold_left
          (fun fields (member, typ) ->
            let value =
              match List.assoc_opt member fields_value with
              | Some value -> eval_cast typ value
              | None -> eval_default typ
            in
            fields @ [ (member, value) ])
          [] fields_typ
      in
      HeaderV (id, true, fields)
  | _ ->
      Format.asprintf "(TODO) Cast from default record to type %a undefined"
        (Type.pp ~level:0) typ
      |> failwith

and eval_cast_default (typ : Type.t) : Value.t = eval_default typ

and eval_cast_invalid (typ : Type.t) : Value.t =
  let typ = Type.canon typ in
  match typ with
  | SpecT _ | DefT _ -> assert false
  | HeaderT (id, fields) ->
      let members, typs = List.split fields in
      let values = List.map eval_default typs in
      let fields = List.combine members values in
      HeaderV (id, false, fields)
  | UnionT (id, fields) ->
      let members, typs = List.split fields in
      let values = List.map eval_default typs in
      let fields = List.combine members values in
      UnionV (id, fields)
  | _ ->
      Format.asprintf "(TODO) Cast from invalid to type %a undefined"
        (Type.pp ~level:0) typ
      |> failwith

and eval_cast (typ : Type.t) (value : Value.t) : Value.t =
  match value with
  | BoolV b -> eval_cast_bool typ b
  | IntV i -> eval_cast_int typ i
  | FIntV (width, i) -> eval_cast_fint typ width i
  | FBitV (width, i) -> eval_cast_fbit typ width i
  | SEnumFieldV (id, member, value) -> eval_cast_senum_field typ id member value
  | StructV (id, fields) -> eval_cast_struct typ id fields
  | HeaderV (id, valid, fields) -> eval_cast_header typ id valid fields
  | SeqV values -> eval_cast_seq typ values
  | SeqDefaultV values -> eval_cast_seq_default typ values
  | RecordV fields -> eval_cast_record typ fields
  | RecordDefaultV fields -> eval_cast_record_default typ fields
  | DefaultV -> eval_cast_default typ
  | InvalidV -> eval_cast_invalid typ
  | _ ->
      Format.asprintf "(TODO) Cast from %a to type %a undefined"
        (Value.pp ~level:0) value (Type.pp ~level:0) typ
      |> failwith

(* Default evaluation *)

(* (7.3) Default values

   Some P4 types define a “default value,” which can be used to automatically initialize values of that type.
   The default values are as follows:

    - For int, bit<N> and int<N> types the default value is 0.
    - For bool the default value is false.
    - For error the default value is error.NoError (defined in core.p4)
    - For string the default value is the empty string ""
    - For varbit<N> the default value is a string of zero bits
      (there is currently no P4 literal to represent such a value).
    - For enum values with an underlying type the default value is 0,
      even if 0 is actually not one of the named values in the enum.
    - For enum values without an underlying type the default value is
      the first value that appears in the enum type declaration.
    - For header types the default value is invalid.
    - For header stacks the default value is that all elements are invalid and the nextIndex is 0.
    - For header_union values the default value is that all union elements are invalid.
    - For struct types the default value is a struct where each field has
      the default value of the suitable field type – if all such default values are defined.
    - For a tuple type the default value is a tuple where each field has
      the default value of the suitable type – if all such default values are defined.

   Note that some types do not have default values, e.g., match_kind, set types, function types,
   extern types, parser types, control types, package types. *)

and eval_default (typ : Type.t) : Value.t =
  let typ = Type.canon typ in
  match typ with
  | SpecT _ | DefT _ -> assert false
  | ErrT -> ErrV "NoError"
  | StrT -> StrV ""
  | BoolT -> BoolV false
  | IntT -> IntV Bigint.zero
  | FIntT width -> FIntV (width, Bigint.zero)
  | FBitT width -> FBitV (width, Bigint.zero)
  | VBitT width -> VBitV (width, Bigint.zero, Bigint.zero)
  | EnumT (id, members) -> EnumFieldV (id, List.hd members)
  | SEnumT (id, typ_inner, fields) ->
      let zero = eval_cast typ_inner (IntV Bigint.zero) in
      let fields_swapped =
        List.map (fun (member, value) -> (value, member)) fields
      in
      let member =
        match List.assoc_opt zero fields_swapped with
        | Some member -> member
        | None -> "__UNSPECIFIED"
      in
      SEnumFieldV (id, member, zero)
  | TupleT typs_inner ->
      let values = List.map eval_default typs_inner in
      TupleV values
  | StackT (typ_inner, size) ->
      let values =
        List.init (Bigint.to_int_exn size) (fun _ -> eval_default typ_inner)
      in
      StackV (values, Bigint.zero, size)
  | StructT (id, fields) ->
      let members, typs_inner = List.split fields in
      let values = List.map eval_default typs_inner in
      let fields = List.combine members values in
      StructV (id, fields)
  | HeaderT (id, fields) ->
      let members, typs_inner = List.split fields in
      let values = List.map eval_default typs_inner in
      let fields = List.combine members values in
      HeaderV (id, false, fields)
  | UnionT (id, fields) ->
      let members, typs_inner = List.split fields in
      let values = List.map eval_default typs_inner in
      let fields = List.combine members values in
      UnionV (id, fields)
  | _ ->
      Format.asprintf "(default) Default value not defined for type %a"
        (Type.pp ~level:0) typ
      |> failwith
