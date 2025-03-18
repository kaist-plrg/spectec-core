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

open Vdomain.Num
module Value = Vdomain.Value
module Type = Il.Types.Type
open Util.Source

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
  | IntV value -> IntV Bigint.(-value)
  | FIntV (width, value) ->
      let value = to_two_complement Bigint.(-value) width in
      int_of_raw_int value width
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

let eval_binop_plus (value_a : Value.t) (value_b : Value.t) : Value.t =
  match (value_a, value_b) with
  | IntV value_a, IntV value_b -> IntV Bigint.(value_a + value_b)
  | FIntV (width_a, value_a), FIntV (width_b, value_b)
    when Bigint.(width_a = width_b) ->
      let value_a = raw_int_of_int value_a width_a in
      let value_b = raw_int_of_int value_b width_b in
      int_of_raw_int Bigint.(value_a + value_b) width_a
  | FBitV (width_a, value_a), FBitV (width_b, value_b)
    when Bigint.(width_a = width_b) ->
      let value_a = raw_int_of_bit value_a width_a in
      let value_b = raw_int_of_bit value_b width_b in
      bit_of_raw_int Bigint.(value_a + value_b) width_a
  | _ ->
      Format.asprintf "Invalid addition: %a + %a" (Value.pp ~level:0) value_a
        (Value.pp ~level:0) value_b
      |> failwith

let eval_binop_plussat (value_a : Value.t) (value_b : Value.t) : Value.t =
  match (value_a, value_b) with
  | FIntV (width_a, value_a), FIntV (width_b, value_b)
    when Bigint.(width_a = width_b) ->
      let value_a = raw_int_of_int value_a width_a in
      let value_b = raw_int_of_int value_b width_b in
      let value = Bigint.(value_a + value_b) in
      let mx = power_of_two Bigint.(width_a - one) in
      let value =
        if Bigint.(value > zero) then Bigint.min value Bigint.(mx - one)
        else Bigint.max value Bigint.(-mx)
      in
      int_of_raw_int value width_a
  | FBitV (width_a, value_a), FBitV (width_b, value_b)
    when Bigint.(width_a = width_b) ->
      let value_a = raw_int_of_bit value_a width_a in
      let value_b = raw_int_of_bit value_b width_b in
      let value = Bigint.(value_a + value_b) in
      let mx = power_of_two width_a in
      let value =
        if Bigint.(value > zero) then Bigint.min value Bigint.(mx - one)
        else Bigint.max value Bigint.zero
      in
      bit_of_raw_int value width_a
  | _ ->
      Format.asprintf "Invalid addition with saturation: %a |+| %a"
        (Value.pp ~level:0) value_a (Value.pp ~level:0) value_b
      |> failwith

let eval_binop_minus (value_a : Value.t) (value_b : Value.t) : Value.t =
  match (value_a, value_b) with
  | IntV value_a, IntV value_b -> IntV Bigint.(value_a - value_b)
  | FIntV (width_a, value_a), FIntV (width_b, value_b)
    when Bigint.(width_a = width_b) ->
      let value_a = raw_int_of_int value_a width_a in
      let value_b = raw_int_of_int value_b width_b in
      int_of_raw_int Bigint.(value_a - value_b) width_a
  | FBitV (width_a, value_a), FBitV (width_b, value_b)
    when Bigint.(width_a = width_b) ->
      let value_a = raw_int_of_bit value_a width_a in
      let value_b = raw_int_of_bit value_b width_b in
      bit_of_raw_int Bigint.(value_a - value_b) width_a
  | _ ->
      Format.asprintf "Invalid subtraction: %a - %a" (Value.pp ~level:0) value_a
        (Value.pp ~level:0) value_b
      |> failwith

let eval_binop_minussat (value_a : Value.t) (value_b : Value.t) : Value.t =
  match (value_a, value_b) with
  | FIntV (width_a, value_a), FIntV (width_b, value_b)
    when Bigint.(width_a = width_b) ->
      let value_a = raw_int_of_int value_a width_a in
      let value_b = raw_int_of_int value_b width_b in
      let value = Bigint.(value_a - value_b) in
      let mx = power_of_two Bigint.(width_a - one) in
      let value =
        if Bigint.(value > zero) then Bigint.min value Bigint.(mx - one)
        else Bigint.max value Bigint.(-mx)
      in
      int_of_raw_int value width_a
  | FBitV (width_a, value_a), FBitV (width_b, value_b)
    when Bigint.(width_a = width_b) ->
      let value_a = raw_int_of_bit value_a width_a in
      let value_b = raw_int_of_bit value_b width_b in
      let value = Bigint.(value_a - value_b) in
      let mx = power_of_two width_a in
      let value =
        if Bigint.(value > zero) then Bigint.min value Bigint.(mx - one)
        else Bigint.max value Bigint.zero
      in
      bit_of_raw_int value width_a
  | _ ->
      Format.asprintf "Invalid subtraction with saturation: %a |-| %a"
        (Value.pp ~level:0) value_a (Value.pp ~level:0) value_b
      |> failwith

let eval_binop_mul (value_a : Value.t) (value_b : Value.t) : Value.t =
  match (value_a, value_b) with
  | IntV value_a, IntV value_b -> IntV Bigint.(value_a * value_b)
  | FIntV (width_a, value_a), FIntV (width_b, value_b)
    when Bigint.(width_a = width_b) ->
      let value_a = raw_int_of_int value_a width_a in
      let value_b = raw_int_of_int value_b width_b in
      int_of_raw_int Bigint.(value_a * value_b) width_a
  | FBitV (width_a, value_a), FBitV (width_b, value_b)
    when Bigint.(width_a = width_b) ->
      let value_a = raw_int_of_bit value_a width_a in
      let value_b = raw_int_of_bit value_b width_b in
      bit_of_raw_int Bigint.(value_a * value_b) width_a
  | _ ->
      Format.asprintf "Invalid multiplication: %a * %a" (Value.pp ~level:0)
        value_a (Value.pp ~level:0) value_b
      |> failwith

let eval_binop_div (value_a : Value.t) (value_b : Value.t) : Value.t =
  match (value_a, value_b) with
  | IntV value_a, IntV value_b -> IntV Bigint.(value_a / value_b)
  | _ ->
      Format.asprintf "Invalid division: %a / %a" (Value.pp ~level:0) value_a
        (Value.pp ~level:0) value_b
      |> failwith

let eval_binop_mod (value_a : Value.t) (value_b : Value.t) : Value.t =
  match (value_a, value_b) with
  | IntV value_a, IntV value_b -> IntV Bigint.(value_a % value_b)
  | _ ->
      Format.asprintf "Invalid modulo: %a %% %a" (Value.pp ~level:0) value_a
        (Value.pp ~level:0) value_b
      |> failwith

let rec eval_binop_shl (value_a : Value.t) (value_b : Value.t) : Value.t =
  match (value_a, value_b) with
  | IntV value_a, IntV value_b ->
      let value = shift_bitstring_left value_a value_b in
      IntV value
  | IntV value_a, FIntV (width_b, value_b) ->
      let value_b = raw_int_of_int value_b width_b in
      let value = shift_bitstring_left value_a value_b in
      IntV value
  | IntV value_a, FBitV (width_b, value_b) ->
      let value_b = raw_int_of_bit value_b width_b in
      let value = shift_bitstring_left value_a value_b in
      IntV value
  | FIntV (width_a, _), IntV value_b ->
      eval_binop_shl value_a (int_of_raw_int value_b width_a)
  | FIntV (width_a, value_a), FIntV (width_b, value_b) ->
      let value_a = raw_int_of_int value_a width_a in
      let value_b = raw_int_of_int value_b width_b in
      let value = shift_bitstring_left value_a value_b in
      int_of_raw_int value width_a
  | FIntV (width_a, value_a), FBitV (width_b, value_b) ->
      let value_a = raw_int_of_int value_a width_a in
      let value_b = raw_int_of_bit value_b width_b in
      let value = shift_bitstring_left value_a value_b in
      int_of_raw_int value width_a
  | FBitV (width_a, _), IntV value_b ->
      eval_binop_shl value_a (bit_of_raw_int value_b width_a)
  | FBitV (width_a, value_a), FIntV (width_b, value_b) ->
      let value_a = raw_int_of_bit value_a width_a in
      let value_b = raw_int_of_int value_b width_b in
      let value = shift_bitstring_left value_a value_b in
      bit_of_raw_int value width_a
  | FBitV (width_a, value_a), FBitV (width_b, value_b) ->
      let value_a = raw_int_of_bit value_a width_a in
      let value_b = raw_int_of_bit value_b width_b in
      let value = shift_bitstring_left value_a value_b in
      bit_of_raw_int value width_a
  | _ ->
      Format.asprintf "Invalid shift left: %a << %a" (Value.pp ~level:0) value_a
        (Value.pp ~level:0) value_b
      |> failwith

let eval_binop_shr (value_a : Value.t) (value_b : Value.t) : Value.t =
  match (value_a, value_b) with
  | IntV value_a, IntV value_b ->
      let value = shift_bitstring_right value_a value_b in
      IntV value
  | IntV value_a, FIntV (width_b, value_b) ->
      let value_b = raw_int_of_int value_b width_b in
      let value = shift_bitstring_right value_a value_b in
      IntV value
  | IntV value_a, FBitV (width_b, value_b) ->
      let value_b = raw_int_of_bit value_b width_b in
      let value = shift_bitstring_right value_a value_b in
      IntV value
  | FIntV (width_a, value_a), IntV value_b ->
      let value_a_signed = raw_int_of_int value_a width_a in
      let value_a_unsigned = raw_int_of_bit value_a width_a in
      let arith = Bigint.(value_a_signed < zero) in
      let value =
        if arith then
          let mx = power_of_two Bigint.(width_a - one) in
          shift_bitstring_right_arith value_a_unsigned value_b mx
        else shift_bitstring_right value_a_unsigned value_b
      in
      int_of_raw_int value width_a
  | FIntV (width_a, value_a), FIntV (width_b, value_b) ->
      let value_a_signed = raw_int_of_int value_a width_a in
      let value_a_unsigned = raw_int_of_bit value_a width_a in
      let value_b = raw_int_of_int value_b width_b in
      let arith = Bigint.(value_a_signed < zero) in
      let value =
        if arith then
          let mx = power_of_two Bigint.(width_a - one) in
          shift_bitstring_right_arith value_a_unsigned value_b mx
        else shift_bitstring_right value_a_unsigned value_b
      in
      int_of_raw_int value width_a
  | FIntV (width_a, value_a), FBitV (width_b, value_b) ->
      let value_a_signed = raw_int_of_int value_a width_a in
      let value_a_unsigned = raw_int_of_bit value_a width_a in
      let value_b = raw_int_of_bit value_b width_b in
      let arith = Bigint.(value_a_signed < zero) in
      let value =
        if arith then
          let mx = power_of_two Bigint.(width_a - one) in
          shift_bitstring_right_arith value_a_unsigned value_b mx
        else shift_bitstring_right value_a_unsigned value_b
      in
      int_of_raw_int value width_a
  | FBitV (width_a, value_a), IntV value_b ->
      let value_a = raw_int_of_bit value_a width_a in
      let value = shift_bitstring_right value_a value_b in
      bit_of_raw_int value width_a
  | FBitV (width_a, value_a), FIntV (width_b, value_b) ->
      let value_a = raw_int_of_bit value_a width_a in
      let value_b = raw_int_of_int value_b width_b in
      let value = shift_bitstring_right value_a value_b in
      bit_of_raw_int value width_a
  | FBitV (width_a, value_a), FBitV (width_b, value_b) ->
      let value_a = raw_int_of_bit value_a width_a in
      let value_b = raw_int_of_bit value_b width_b in
      let value = shift_bitstring_right value_a value_b in
      bit_of_raw_int value width_a
  | _ ->
      Format.asprintf "Invalid shift right: %a >> %a" (Value.pp ~level:0)
        value_a (Value.pp ~level:0) value_b
      |> failwith

let eval_binop_le (value_a : Value.t) (value_b : Value.t) : bool =
  match (value_a, value_b) with
  | IntV value_a, IntV value_b -> Bigint.(value_a <= value_b)
  | FIntV (width_a, value_a), FIntV (width_b, value_b)
    when Bigint.(width_a = width_b) ->
      let value_a = raw_int_of_int value_a width_a in
      let value_b = raw_int_of_int value_b width_b in
      Bigint.(value_a <= value_b)
  | FBitV (width_a, value_a), FBitV (width_b, value_b)
    when Bigint.(width_a = width_b) ->
      let value_a = raw_int_of_bit value_a width_a in
      let value_b = raw_int_of_bit value_b width_b in
      Bigint.(value_a <= value_b)
  | _ ->
      Format.asprintf "Invalid less than or equal: %a <= %a" (Value.pp ~level:0)
        value_a (Value.pp ~level:0) value_b
      |> failwith

let eval_binop_ge (value_a : Value.t) (value_b : Value.t) : bool =
  match (value_a, value_b) with
  | IntV value_a, IntV value_b -> Bigint.(value_a >= value_b)
  | FIntV (width_a, value_a), FIntV (width_b, value_b)
    when Bigint.(width_a = width_b) ->
      let value_a = raw_int_of_int value_a width_a in
      let value_b = raw_int_of_int value_b width_b in
      Bigint.(value_a >= value_b)
  | FBitV (width_a, value_a), FBitV (width_b, value_b)
    when Bigint.(width_a = width_b) ->
      let value_a = raw_int_of_bit value_a width_a in
      let value_b = raw_int_of_bit value_b width_b in
      Bigint.(value_a >= value_b)
  | _ ->
      Format.asprintf "Invalid greater than or equal: %a >= %a"
        (Value.pp ~level:0) value_a (Value.pp ~level:0) value_b
      |> failwith

let eval_binop_lt (value_a : Value.t) (value_b : Value.t) : bool =
  match (value_a, value_b) with
  | IntV value_a, IntV value_b -> Bigint.(value_a < value_b)
  | FIntV (width_a, value_a), FIntV (width_b, value_b)
    when Bigint.(width_a = width_b) ->
      let value_a = raw_int_of_int value_a width_a in
      let value_b = raw_int_of_int value_b width_b in
      Bigint.(value_a < value_b)
  | FBitV (width_a, value_a), FBitV (width_b, value_b)
    when Bigint.(width_a = width_b) ->
      let value_a = raw_int_of_bit value_a width_a in
      let value_b = raw_int_of_bit value_b width_b in
      Bigint.(value_a < value_b)
  | _ ->
      Format.asprintf "Invalid less than: %a < %a" (Value.pp ~level:0) value_a
        (Value.pp ~level:0) value_b
      |> failwith

let eval_binop_gt (value_a : Value.t) (value_b : Value.t) : bool =
  match (value_a, value_b) with
  | IntV value_a, IntV value_b -> Bigint.(value_a > value_b)
  | FIntV (width_a, value_a), FIntV (width_b, value_b)
    when Bigint.(width_a = width_b) ->
      let value_a = raw_int_of_int value_a width_a in
      let value_b = raw_int_of_int value_b width_b in
      Bigint.(value_a > value_b)
  | FBitV (width_a, value_a), FBitV (width_b, value_b)
    when Bigint.(width_a = width_b) ->
      let value_a = raw_int_of_bit value_a width_a in
      let value_b = raw_int_of_bit value_b width_b in
      Bigint.(value_a > value_b)
  | _ ->
      Format.asprintf "Invalid greater than: %a > %a" (Value.pp ~level:0)
        value_a (Value.pp ~level:0) value_b
      |> failwith

let rec eval_binop_eq (value_a : Value.t) (value_b : Value.t) : bool =
  match (value_a, value_b) with
  | ErrV member_a, ErrV member_b | MatchKindV member_a, MatchKindV member_b ->
      member_a = member_b
  | StrV s_a, StrV s_b -> s_a = s_b
  | BoolV b_a, BoolV b_b -> b_a = b_b
  | IntV value_a, IntV value_b -> Bigint.(value_a = value_b)
  | FBitV (width_a, value_a), FBitV (width_b, value_b)
  | FIntV (width_a, value_a), FIntV (width_b, value_b)
  | VBitV (width_a, _, value_a), VBitV (width_b, _, value_b)
    when Bigint.(width_a = width_b) ->
      Bigint.(value_a = value_b)
  | EnumFieldV (id_a, member_a), EnumFieldV (id_b, member_b) ->
      id_a = id_b && member_a = member_b
  | SEnumFieldV (id_a, member_a, value_a), SEnumFieldV (id_b, member_b, value_b)
    ->
      id_a = id_b && member_a = member_b && eval_binop_eq value_a value_b
  | ListV values_a, ListV values_b | TupleV values_a, TupleV values_b ->
      List.length values_a = List.length values_b
      && List.for_all2 eval_binop_eq values_a values_b
  | StackV (values_a, _, size_a), StackV (values_b, _, size_b) ->
      List.length values_a = List.length values_b
      && List.for_all2 eval_binop_eq values_a values_b
      && Bigint.(size_a = size_b)
  | StructV (id_a, fields_a), StructV (id_b, fields_b) ->
      id_a = id_b
      && List.length fields_a = List.length fields_b
      && List.for_all2
           (fun (member_a, value_a) (member_b, value_b) ->
             member_a = member_b && eval_binop_eq value_a value_b)
           fields_a fields_b
  | HeaderV (id_a, valid_a, fields_a), HeaderV (id_b, valid_b, fields_b) ->
      id_a = id_b
      && ((valid_a = false && valid_b = false)
         || (valid_a = true && valid_b = true)
            && List.length fields_a = List.length fields_b
            && List.for_all2
                 (fun (member_a, value_a) (member_b, value_b) ->
                   member_a = member_b && eval_binop_eq value_a value_b)
                 fields_a fields_b)
  | UnionV (id_a, fields_a), UnionV (id_b, fields_b) ->
      id_a = id_b
      && List.length fields_a = List.length fields_b
      && List.for_all2
           (fun (member_a, value_a) (member_b, value_b) ->
             member_a = member_b && eval_binop_eq value_a value_b)
           fields_a fields_b
  | InvalidV, InvalidV -> true
  | _ ->
      Format.asprintf "Invalid equality: %a == %a" (Value.pp ~level:0) value_a
        (Value.pp ~level:0) value_b
      |> failwith

let eval_binop_ne (value_a : Value.t) (value_b : Value.t) : bool =
  not (eval_binop_eq value_a value_b)

let eval_binop_bitand (value_a : Value.t) (value_b : Value.t) : Value.t =
  match (value_a, value_b) with
  | FIntV (width_a, value_a), FIntV (width_b, value_b)
    when Bigint.(width_a = width_b) ->
      let value_a = raw_int_of_int value_a width_a in
      let value_b = raw_int_of_int value_b width_b in
      let value = Bigint.bit_and value_a value_b in
      int_of_raw_int value width_a
  | FBitV (width_a, value_a), FBitV (width_b, value_b)
    when Bigint.(width_a = width_b) ->
      let value_a = raw_int_of_bit value_a width_a in
      let value_b = raw_int_of_bit value_b width_b in
      let value = Bigint.bit_and value_a value_b in
      bit_of_raw_int value width_a
  | _ ->
      Format.asprintf "Invalid bitwise and: %a & %a" (Value.pp ~level:0) value_a
        (Value.pp ~level:0) value_b
      |> failwith

let eval_binop_bitxor (value_a : Value.t) (value_b : Value.t) : Value.t =
  match (value_a, value_b) with
  | FIntV (width_a, value_a), FIntV (width_b, value_b)
    when Bigint.(width_a = width_b) ->
      let value_a = raw_int_of_int value_a width_a in
      let value_b = raw_int_of_int value_b width_b in
      let value = Bigint.bit_xor value_a value_b in
      int_of_raw_int value width_a
  | FBitV (width_a, value_a), FBitV (width_b, value_b)
    when Bigint.(width_a = width_b) ->
      let value_a = raw_int_of_bit value_a width_a in
      let value_b = raw_int_of_bit value_b width_b in
      let value = Bigint.bit_xor value_a value_b in
      bit_of_raw_int value width_a
  | _ ->
      Format.asprintf "Invalid bitwise xor: %a ^ %a" (Value.pp ~level:0) value_a
        (Value.pp ~level:0) value_b
      |> failwith

let eval_binop_bitor (value_a : Value.t) (value_b : Value.t) : Value.t =
  match (value_a, value_b) with
  | FIntV (width_a, value_a), FIntV (width_b, value_b)
    when Bigint.(width_a = width_b) ->
      let value_a = raw_int_of_int value_a width_a in
      let value_b = raw_int_of_int value_b width_b in
      let value = Bigint.bit_or value_a value_b in
      int_of_raw_int value width_a
  | FBitV (width_a, value_a), FBitV (width_b, value_b)
    when Bigint.(width_a = width_b) ->
      let value_a = raw_int_of_bit value_a width_a in
      let value_b = raw_int_of_bit value_b width_b in
      let value = Bigint.bit_or value_a value_b in
      bit_of_raw_int value width_a
  | _ ->
      Format.asprintf "Invalid bitwise or: %a | %a" (Value.pp ~level:0) value_a
        (Value.pp ~level:0) value_b
      |> failwith

let eval_binop_concat (value_a : Value.t) (value_b : Value.t) : Value.t =
  match (value_a, value_b) with
  | FIntV (width_a, value_a), FIntV (width_b, value_b) ->
      let value_a = raw_int_of_int value_a width_a in
      let value_a = shift_bitstring_left value_a width_b in
      let value = Bigint.(value_a + value_b) in
      let width = Bigint.(width_a + width_b) in
      int_of_raw_int value width
  | FIntV (width_a, value_a), FBitV (width_b, value_b) ->
      let value_a = raw_int_of_int value_a width_a in
      let value_a = shift_bitstring_left value_a width_b in
      let value = Bigint.(value_a + value_b) in
      let width = Bigint.(width_a + width_b) in
      int_of_raw_int value width
  | FBitV (width_a, value_a), FIntV (width_b, value_b) ->
      let value_a = raw_int_of_bit value_a width_a in
      let value_a = shift_bitstring_left value_a width_b in
      let value = Bigint.(value_a + value_b) in
      let width = Bigint.(width_a + width_b) in
      int_of_raw_int value width
  | FBitV (width_a, value_a), FBitV (width_b, value_b) ->
      let value_a = raw_int_of_bit value_a width_a in
      let value_a = shift_bitstring_left value_a width_b in
      let value = Bigint.(value_a + value_b) in
      let width = Bigint.(width_a + width_b) in
      int_of_raw_int value width
  | _ ->
      Format.asprintf "Invalid concatenation: %a ++ %a" (Value.pp ~level:0)
        value_a (Value.pp ~level:0) value_b
      |> failwith

let eval_binop_and (value_a : Value.t) (value_b : Value.t) : Value.t =
  match (value_a, value_b) with
  | BoolV b1, BoolV b2 -> BoolV (b1 && b2)
  | _ ->
      Format.asprintf "Invalid and operator: %a && %a" (Value.pp ~level:0)
        value_a (Value.pp ~level:0) value_b
      |> failwith

let eval_binop_or (value_a : Value.t) (value_b : Value.t) : Value.t =
  match (value_a, value_b) with
  | BoolV b1, BoolV b2 -> BoolV (b1 || b2)
  | _ ->
      Format.asprintf "Invalid or operator: %a || %a" (Value.pp ~level:0)
        value_a (Value.pp ~level:0) value_b
      |> failwith

let eval_binop (op : Lang.Ast.binop) (value_a : Value.t) (value_b : Value.t) :
    Value.t =
  match op.it with
  | PlusOp -> eval_binop_plus value_a value_b
  | SPlusOp -> eval_binop_plussat value_a value_b
  | MinusOp -> eval_binop_minus value_a value_b
  | SMinusOp -> eval_binop_minussat value_a value_b
  | MulOp -> eval_binop_mul value_a value_b
  | DivOp -> eval_binop_div value_a value_b
  | ModOp -> eval_binop_mod value_a value_b
  | ShlOp -> eval_binop_shl value_a value_b
  | ShrOp -> eval_binop_shr value_a value_b
  | LeOp -> BoolV (eval_binop_le value_a value_b)
  | GeOp -> BoolV (eval_binop_ge value_a value_b)
  | LtOp -> BoolV (eval_binop_lt value_a value_b)
  | GtOp -> BoolV (eval_binop_gt value_a value_b)
  | EqOp -> BoolV (eval_binop_eq value_a value_b)
  | NeOp -> BoolV (eval_binop_ne value_a value_b)
  | BAndOp -> eval_binop_bitand value_a value_b
  | BXorOp -> eval_binop_bitxor value_a value_b
  | BOrOp -> eval_binop_bitor value_a value_b
  | ConcatOp -> eval_binop_concat value_a value_b
  | LAndOp -> eval_binop_and value_a value_b
  | LOrOp -> eval_binop_or value_a value_b

(* Bitslice evaluation *)

let eval_bitstring_access' (value : Bigint.t) (hvalue : Bigint.t)
    (lvalue : Bigint.t) : Value.t =
  let width = Bigint.(hvalue - lvalue + one) in
  let value = slice_bitstring value hvalue lvalue in
  FBitV (width, value)

let eval_bitstring_access (value : Value.t) (hvalue : Value.t)
    (lvalue : Value.t) : Value.t =
  let extract value = raw_int_of_value value in
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
  | SetT typ_inner ->
      let value = eval_cast_int typ_inner i in
      SetV (`Singleton value)
  | _ ->
      Format.asprintf
        "(TODO) Cast from arbitrary-precision int to type %a undefined"
        (Type.pp ~level:0) typ
      |> failwith

and eval_cast_fint (typ : Type.t) (width : Bigint.t) (i : Bigint.t) : Value.t =
  let typ = Type.canon typ in
  match typ with
  | SpecT _ | DefT _ -> assert false
  | IntT ->
      let i = raw_int_of_int i width in
      IntV i
  | FIntT width_to ->
      let i = raw_int_of_int i width in
      int_of_raw_int i width_to
  | FBitT width_to ->
      let i = raw_int_of_int i width in
      bit_of_raw_int i width_to
  | NewT (_, typ_inner) -> eval_cast_fint typ_inner width i
  | SetT typ_inner ->
      let value = eval_cast_fint typ_inner width i in
      SetV (`Singleton value)
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
  | IntT ->
      let i = raw_int_of_bit i width in
      IntV i
  | FIntT width_to ->
      let i = raw_int_of_bit i width in
      int_of_raw_int i width_to
  | FBitT width_to ->
      let i = raw_int_of_bit i width in
      bit_of_raw_int i width_to
  | NewT (_, typ_inner) -> eval_cast_fbit typ_inner width i
  | SetT typ_inner ->
      let value = eval_cast_fbit typ_inner width i in
      SetV (`Singleton value)
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
  | StackT (typ_inner, size) ->
      let values = List.map (eval_cast typ_inner) values in
      let idx = List.length values |> Bigint.of_int in
      StackV (values, idx, size)
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

and eval_cast_set_singleton (typ : Type.t) (value : Value.t) : Value.t =
  let typ = Type.canon typ in
  match typ with
  | SpecT _ | DefT _ -> assert false
  | SetT typ_inner -> SetV (`Singleton (eval_cast typ_inner value))
  | _ ->
      Format.asprintf "(TODO) Cast from set singleton to type %a undefined"
        (Type.pp ~level:0) typ
      |> failwith

and eval_cast_set_mask (typ : Type.t) (value : Value.t) (mask : Value.t) :
    Value.t =
  let typ = Type.canon typ in
  match typ with
  | SpecT _ | DefT _ -> assert false
  | SetT typ_inner ->
      let value = eval_cast typ_inner value in
      let mask = eval_cast typ_inner mask in
      SetV (`Mask (value, mask))
  | _ ->
      Format.asprintf "(TODO) Cast from set mask to type %a undefined"
        (Type.pp ~level:0) typ
      |> failwith

and eval_cast_set_range (typ : Type.t) (value : Value.t) (range : Value.t) :
    Value.t =
  let typ = Type.canon typ in
  match typ with
  | SpecT _ | DefT _ -> assert false
  | SetT typ_inner ->
      let value = eval_cast typ_inner value in
      let range = eval_cast typ_inner range in
      SetV (`Range (value, range))
  | _ ->
      Format.asprintf "(TODO) Cast from set range to type %a undefined"
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
  | SetV (`Singleton value) -> eval_cast_set_singleton typ value
  | SetV (`Mask (value, mask)) -> eval_cast_set_mask typ value mask
  | SetV (`Range (value, range)) -> eval_cast_set_range typ value range
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
      Format.asprintf "(eval_default) Default value not defined for type %a"
        (Type.pp ~level:0) typ
      |> failwith
