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

open Syntax
open Ast
open Value

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
  Bit { value; width }

let int_of_raw_int (n : Bigint.t) (w : Bigint.t) : Value.t =
  let value = of_two_complement n w in
  let width = w in
  Int { value; width }

(* Unop evaluation *)

let eval_unop_not (value : Value.t) : Value.t =
  match value with
  | Bool b -> Bool (not b)
  | _ ->
      Printf.sprintf "Not a boolean value: %s" (Value.print value)
      |> failwith

let eval_unop_bitnot (value : Value.t) : Value.t =
  match value with
  | Bit { value; width } ->
      let value = bitwise_neg value width in
      Bit { value; width }
  | _ ->
      Printf.sprintf "Not a bit value: %s" (Value.print value) |> failwith

let eval_unop_uminus (value : Value.t) : Value.t =
  match value with
  | AInt value -> AInt (Bigint.neg value)
  | Int { value; width } ->
      let value = to_two_complement (Bigint.neg value) width in
      Int { value; width }
  | Bit { value; width } ->
      let value = Bigint.(power_of_two width - value) in
      Bit { value; width }
  | _ ->
      Printf.sprintf "Not an integer value: %s" (Value.print value)
      |> failwith

let eval_unop (op : Op.un) (value : Value.t) : Value.t =
  match op with
  | Not _ -> eval_unop_not value
  | BitNot _ -> eval_unop_bitnot value
  | UMinus _ -> eval_unop_uminus value

(* Binop evaluation *)

let unsigned_op_sat (l : Bigint.t) (r : Bigint.t) (w : Bigint.t)
    (op : Bigint.t -> Bigint.t -> Bigint.t) : Value.t =
  let x = power_of_two w in
  let n = op l r in
  let n' =
    if Bigint.(n > zero) then Bigint.min n Bigint.(x - one)
    else Bigint.max n Bigint.zero
  in
  Bit { value = n'; width = w }

let signed_op_sat (l : Bigint.t) (r : Bigint.t) (w : Bigint.t)
    (op : Bigint.t -> Bigint.t -> Bigint.t) : Value.t =
  let x = power_of_two Bigint.(w - one) in
  let n = op l r in
  let n' =
    if Bigint.(n > zero) then Bigint.min n Bigint.(x - one)
    else Bigint.max n Bigint.(-x)
  in
  Int { value = n'; width = w }

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

let rec eval_binop_plus (lvalue : Value.t) (rvalue : Value.t) : Value.t
    =
  match (lvalue, rvalue) with
  | Bit { value = lvalue; width }, Bit { value = rvalue; _ } ->
      let value = of_two_complement Bigint.(lvalue + rvalue) width in
      Bit { value; width }
  | Int { value = lvalue; width }, Int { value = rvalue; _ } ->
      let value = of_two_complement Bigint.(lvalue + rvalue) width in
      Int { value; width }
  | Bit { width; _ }, AInt rvalue ->
      eval_binop_plus lvalue (bit_of_raw_int rvalue width)
  | AInt lvalue, Bit { width; _ } ->
      eval_binop_plus (bit_of_raw_int lvalue width) rvalue
  | Int { width; _ }, AInt rvalue ->
      eval_binop_plus lvalue (int_of_raw_int rvalue width)
  | AInt lvalue, Int { width; _ } ->
      eval_binop_plus (int_of_raw_int lvalue width) rvalue
  | AInt lvalue, AInt rvalue -> AInt Bigint.(lvalue + rvalue)
  | _ ->
      Printf.sprintf "Invalid addition: %s + %s" (Value.print lvalue)
        (Value.print rvalue)
      |> failwith

let rec eval_binop_plussat (lvalue : Value.t) (rvalue : Value.t) :
    Value.t =
  match (lvalue, rvalue) with
  | Bit { value = lvalue; width }, Bit { value = rvalue; _ } ->
      unsigned_op_sat lvalue rvalue width Bigint.( + )
  | Int { value = lvalue; width }, Int { value = rvalue; _ } ->
      signed_op_sat lvalue rvalue width Bigint.( + )
  | Bit { width; _ }, AInt rvalue ->
      eval_binop_plussat lvalue (bit_of_raw_int rvalue width)
  | AInt lvalue, Bit { width; _ } ->
      eval_binop_plussat (bit_of_raw_int lvalue width) rvalue
  | Int { width; _ }, AInt rvalue ->
      eval_binop_plussat lvalue (int_of_raw_int rvalue width)
  | AInt lvalue, Int { width; _ } ->
      eval_binop_plussat (int_of_raw_int lvalue width) rvalue
  | _ ->
      Printf.sprintf "Invalid addition with saturation: %s (+) %s"
        (Value.print lvalue) (Value.print rvalue)
      |> failwith

let rec eval_binop_minus (lvalue : Value.t) (rvalue : Value.t) :
    Value.t =
  match (lvalue, rvalue) with
  | Bit { value = lvalue; width }, Bit { value = rvalue; _ } ->
      let value = of_two_complement Bigint.(lvalue - rvalue) width in
      Bit { value; width }
  | Int { value = lvalue; width }, Int { value = rvalue; _ } ->
      let value = of_two_complement Bigint.(lvalue - rvalue) width in
      Int { value; width }
  | Bit { width; _ }, AInt rvalue ->
      eval_binop_minus lvalue (bit_of_raw_int rvalue width)
  | AInt lvalue, Bit { width; _ } ->
      eval_binop_minus (bit_of_raw_int lvalue width) rvalue
  | Int { width; _ }, AInt rvalue ->
      eval_binop_minus lvalue (int_of_raw_int rvalue width)
  | AInt lvalue, Int { width; _ } ->
      eval_binop_minus (int_of_raw_int lvalue width) rvalue
  | AInt lvalue, AInt rvalue -> AInt Bigint.(lvalue - rvalue)
  | _ ->
      Printf.sprintf "Invalid subtraction: %s - %s" (Value.print lvalue)
        (Value.print rvalue)
      |> failwith

let rec eval_binop_minussat (lvalue : Value.t) (rvalue : Value.t) :
    Value.t =
  match (lvalue, rvalue) with
  | Bit { value = lvalue; width }, Bit { value = rvalue; _ } ->
      unsigned_op_sat lvalue rvalue width Bigint.( - )
  | Int { value = lvalue; width }, Int { value = rvalue; _ } ->
      signed_op_sat lvalue rvalue width Bigint.( - )
  | Bit { width; _ }, AInt rvalue ->
      eval_binop_minussat lvalue (bit_of_raw_int rvalue width)
  | AInt lvalue, Bit { width; _ } ->
      eval_binop_minussat (bit_of_raw_int lvalue width) rvalue
  | Int { width; _ }, AInt rvalue ->
      eval_binop_minussat lvalue (int_of_raw_int rvalue width)
  | AInt lvalue, Int { width; _ } ->
      eval_binop_minussat (int_of_raw_int lvalue width) rvalue
  | _ ->
      Printf.sprintf "Invalid subtraction with saturation: %s (-) %s"
        (Value.print lvalue) (Value.print rvalue)
      |> failwith

let rec eval_binop_mul (lvalue : Value.t) (rvalue : Value.t) : Value.t
    =
  match (lvalue, rvalue) with
  | Bit { value = lvalue; width }, Bit { value = rvalue; _ } ->
      let value = of_two_complement Bigint.(lvalue * rvalue) width in
      Bit { value; width }
  | Int { value = lvalue; width }, Int { value = rvalue; _ } ->
      let value = to_two_complement Bigint.(lvalue * rvalue) width in
      Int { value; width }
  | Bit { width; _ }, AInt rvalue ->
      eval_binop_mul lvalue (bit_of_raw_int rvalue width)
  | AInt lvalue, Bit { width; _ } ->
      eval_binop_mul (bit_of_raw_int lvalue width) rvalue
  | Int { width; _ }, AInt rvalue ->
      eval_binop_mul lvalue (int_of_raw_int rvalue width)
  | AInt lvalue, Int { width; _ } ->
      eval_binop_mul (int_of_raw_int lvalue width) rvalue
  | AInt lvalue, AInt rvalue -> AInt Bigint.(lvalue * rvalue)
  | _ ->
      Printf.sprintf "Invalid multiplication: %s * %s" (Value.print lvalue)
        (Value.print rvalue)
      |> failwith

let eval_binop_div (lvalue : Value.t) (rvalue : Value.t) : Value.t =
  match (lvalue, rvalue) with
  | AInt lvalue, AInt rvalue -> AInt Bigint.(lvalue / rvalue)
  | Bit { value = lvalue; width }, Bit { value = rvalue; _ } ->
      let value = Bigint.(lvalue / rvalue) in
      Bit { value; width }
  | _ ->
      Printf.sprintf "Invalid division: %s / %s" (Value.print lvalue)
        (Value.print rvalue)
      |> failwith

let eval_binop_mod (lvalue : Value.t) (rvalue : Value.t) : Value.t =
  match (lvalue, rvalue) with
  | AInt lvalue, AInt rvalue -> AInt Bigint.(lvalue % rvalue)
  | Bit { value = lvalue; width }, Bit { value = rvalue; _ } ->
      let value = Bigint.(lvalue % rvalue) in
      Bit { value; width }
  | _ ->
      Printf.sprintf "Invalid modulo: %s %% %s" (Value.print lvalue)
        (Value.print rvalue)
      |> failwith

let eval_binop_shl (lvalue : Value.t) (rvalue : Value.t) : Value.t =
  match (lvalue, rvalue) with
  | Bit { value = lvalue; width }, Bit { value = rvalue; _ }
  | Bit { value = lvalue; width }, AInt rvalue ->
      let value =
        of_two_complement (shift_bitstring_left lvalue rvalue) width
      in
      Bit { value; width }
  | Int { value = lvalue; width }, Bit { value = rvalue; _ }
  | Int { value = lvalue; width }, AInt rvalue ->
      let value =
        to_two_complement (shift_bitstring_left lvalue rvalue) width
      in
      Int { value; width }
  | AInt lvalue, AInt rvalue ->
      let value = shift_bitstring_left lvalue rvalue in
      AInt value
  | Bit { value = lvalue; width }, Int { value = rvalue; _ } ->
      let value =
        of_two_complement (shift_bitstring_left lvalue rvalue) width
      in
      Bit { value; width }
  | Int { value = lvalue; width }, Int { value = rvalue; _ } ->
      let value =
        to_two_complement (shift_bitstring_left lvalue rvalue) width
      in
      Int { value; width }
  | _ ->
      Printf.sprintf "Invalid shift left: %s << %s" (Value.print lvalue)
        (Value.print rvalue)
      |> failwith

let eval_binop_shr (lvalue : Value.t) (rvalue : Value.t) : Value.t =
  match (lvalue, rvalue) with
  | Bit { value = lvalue; width }, Bit { value = rvalue; _ }
  | Bit { value = lvalue; width }, AInt rvalue ->
      let value =
        of_two_complement
          (shift_bitstring_right lvalue rvalue false Bigint.zero)
          width
      in
      Bit { value; width }
  | Int { value = lvalue; width }, Bit { value = rvalue; _ }
  | Int { value = lvalue; width }, Int { value = rvalue; _ }
  | Int { value = lvalue; width }, AInt rvalue ->
      let exp = power_of_two Bigint.(width - one) in
      let arith = Bigint.(of_two_complement lvalue width > exp) in
      let value =
        to_two_complement (shift_bitstring_right lvalue rvalue arith exp) width
      in
      Int { value; width }
  | AInt lvalue, AInt rvalue ->
      let value = shift_bitstring_right lvalue rvalue false Bigint.zero in
      AInt value
  | Bit { value = lvalue; width }, Int { value = rvalue; _ } ->
      let value =
        of_two_complement
          (shift_bitstring_right lvalue rvalue false Bigint.zero)
          width
      in
      Bit { value; width }
  | _ ->
      Printf.sprintf "Invalid shift right: %s >> %s" (Value.print lvalue)
        (Value.print rvalue)
      |> failwith

let rec eval_binop_le (lvalue : Value.t) (rvalue : Value.t) : Value.t =
  match (lvalue, rvalue) with
  | Bit { value = lvalue; _ }, Bit { value = rvalue; _ }
  | AInt lvalue, AInt rvalue ->
      Bool Bigint.(lvalue <= rvalue)
  | AInt lvalue, Bit { width; _ } ->
      eval_binop_le (bit_of_raw_int lvalue width) rvalue
  | Bit { width; _ }, AInt rvalue ->
      eval_binop_le lvalue (bit_of_raw_int rvalue width)
  | AInt lvalue, Int { width; _ } ->
      eval_binop_le (int_of_raw_int lvalue width) rvalue
  | Int { width; _ }, AInt rvalue ->
      eval_binop_le lvalue (int_of_raw_int rvalue width)
  | _ ->
      Printf.sprintf "Invalid less than or equal: %s <= %s"
        (Value.print lvalue) (Value.print rvalue)
      |> failwith

let rec eval_binop_ge (lvalue : Value.t) (rvalue : Value.t) : Value.t =
  match (lvalue, rvalue) with
  | Bit { value = lvalue; _ }, Bit { value = rvalue; _ }
  | AInt lvalue, AInt rvalue ->
      Bool Bigint.(lvalue >= rvalue)
  | AInt lvalue, Bit { width; _ } ->
      eval_binop_ge (bit_of_raw_int lvalue width) rvalue
  | Bit { width; _ }, AInt rvalue ->
      eval_binop_ge lvalue (bit_of_raw_int rvalue width)
  | AInt lvalue, Int { width; _ } ->
      eval_binop_ge (int_of_raw_int lvalue width) rvalue
  | Int { width; _ }, AInt rvalue ->
      eval_binop_ge lvalue (int_of_raw_int rvalue width)
  | _ ->
      Printf.sprintf "Invalid greater than or equal: %s >= %s"
        (Value.print lvalue) (Value.print rvalue)
      |> failwith

let rec eval_binop_lt (lvalue : Value.t) (rvalue : Value.t) : Value.t =
  match (lvalue, rvalue) with
  | Bit { value = lvalue; _ }, Bit { value = rvalue; _ }
  | AInt lvalue, AInt rvalue ->
      Bool Bigint.(lvalue < rvalue)
  | AInt lvalue, Bit { width; _ } ->
      eval_binop_lt (bit_of_raw_int lvalue width) rvalue
  | Bit { width; _ }, AInt rvalue ->
      eval_binop_lt lvalue (bit_of_raw_int rvalue width)
  | AInt lvalue, Int { width; _ } ->
      eval_binop_lt (int_of_raw_int lvalue width) rvalue
  | Int { width; _ }, AInt rvalue ->
      eval_binop_lt lvalue (int_of_raw_int rvalue width)
  | _ ->
      Printf.sprintf "Invalid less than: %s < %s" (Value.print lvalue)
        (Value.print rvalue)
      |> failwith

let rec eval_binop_gt (lvalue : Value.t) (rvalue : Value.t) : Value.t =
  match (lvalue, rvalue) with
  | Bit { value = lvalue; _ }, Bit { value = rvalue; _ }
  | AInt lvalue, AInt rvalue ->
      Bool Bigint.(lvalue > rvalue)
  | AInt lvalue, Bit { width; _ } ->
      eval_binop_gt (bit_of_raw_int lvalue width) rvalue
  | Bit { width; _ }, AInt rvalue ->
      eval_binop_gt lvalue (bit_of_raw_int rvalue width)
  | AInt lvalue, Int { width; _ } ->
      eval_binop_gt (int_of_raw_int lvalue width) rvalue
  | Int { width; _ }, AInt rvalue ->
      eval_binop_gt lvalue (int_of_raw_int rvalue width)
  | _ ->
      Printf.sprintf "Invalid greater than: %s > %s" (Value.print lvalue)
        (Value.print rvalue)
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
  | Bool b1, Bool b2 -> b1 = b2
  | Bit { value = lvalue; _ }, Bit { value = rvalue; _ }
  | AInt lvalue, AInt rvalue
  | Int { value = lvalue; _ }, Int { value = rvalue; _ } ->
      Bigint.(lvalue = rvalue)
  | Bit { width; _ }, AInt rvalue ->
      eval_binop_eq lvalue (bit_of_raw_int rvalue width)
  | AInt lvalue, Bit { width; _ } ->
      eval_binop_eq (bit_of_raw_int lvalue width) rvalue
  | Int { width; _ }, AInt rvalue ->
      eval_binop_eq lvalue (int_of_raw_int rvalue width)
  | AInt lvalue, Int { width; _ } ->
      eval_binop_eq (int_of_raw_int lvalue width) rvalue
  | ( Header { valid = lvalid; entries = lentries },
      Header { valid = rvalid; entries = rentries } ) ->
      lvalid = rvalid && eval_binop_eq_entries lentries rentries
  | Struct { entries = lentries }, Struct { entries = rentries } ->
      List.length lentries = List.length rentries
      && eval_binop_eq_entries lentries rentries
  | Tuple lvalues, Tuple rvalues ->
      List.length lvalues = List.length rvalues
      && List.for_all2
           (fun lvalue rvalue -> eval_binop_eq lvalue rvalue)
           lvalues rvalues
  | _ ->
      Printf.sprintf "Invalid equality: %s == %s" (Value.print lvalue)
        (Value.print rvalue)
      |> failwith

let eval_binop_ne (lvalue : Value.t) (rvalue : Value.t) : bool =
  not (eval_binop_eq lvalue rvalue)

let rec eval_binop_bitand (lvalue : Value.t) (rvalue : Value.t) :
    Value.t =
  match (lvalue, rvalue) with
  | Bit { value = lvalue; width }, Bit { value = rvalue; _ } ->
      let value = Bigint.bit_and lvalue rvalue in
      Bit { value; width }
  | Bit { width; _ }, AInt rvalue ->
      eval_binop_bitand lvalue (bit_of_raw_int rvalue width)
  | AInt lvalue, Bit { width; _ } ->
      eval_binop_bitand (bit_of_raw_int lvalue width) rvalue
  | _ ->
      Printf.sprintf "Invalid bitwise and: %s & %s" (Value.print lvalue)
        (Value.print rvalue)
      |> failwith

let rec eval_binop_bitxor (lvalue : Value.t) (rvalue : Value.t) :
    Value.t =
  match (lvalue, rvalue) with
  | Bit { value = lvalue; width }, Bit { value = rvalue; _ } ->
      let value = Bigint.bit_xor lvalue rvalue in
      Bit { value; width }
  | Bit { width; _ }, AInt rvalue ->
      eval_binop_bitxor lvalue (bit_of_raw_int rvalue width)
  | AInt lvalue, Bit { width; _ } ->
      eval_binop_bitxor (bit_of_raw_int lvalue width) rvalue
  | _ ->
      Printf.sprintf "Invalid bitwise xor: %s ^ %s" (Value.print lvalue)
        (Value.print rvalue)
      |> failwith

let rec eval_binop_bitor (lvalue : Value.t) (rvalue : Value.t) :
    Value.t =
  match (lvalue, rvalue) with
  | Bit { value = lvalue; width }, Bit { value = rvalue; _ } ->
      let value = Bigint.bit_or lvalue rvalue in
      Bit { value; width }
  | Bit { width; _ }, AInt rvalue ->
      eval_binop_bitor lvalue (bit_of_raw_int rvalue width)
  | AInt lvalue, Bit { width; _ } ->
      eval_binop_bitor (bit_of_raw_int lvalue width) rvalue
  | _ ->
      Printf.sprintf "Invalid bitwise or: %s | %s" (Value.print lvalue)
        (Value.print rvalue)
      |> failwith

let rec eval_binop_plusplus (lvalue : Value.t) (rvalue : Value.t) :
    Value.t =
  match (lvalue, rvalue) with
  | ( Bit { value = lvalue; width = lwidth },
      Bit { value = rvalue; width = rwidth } ) ->
      let value = Bigint.(shift_bitstring_left lvalue rwidth + rvalue) in
      let width = Bigint.(lwidth + rwidth) in
      Bit { value; width }
  | Bit { width; _ }, AInt rvalue ->
      eval_binop_plusplus lvalue (bit_of_raw_int rvalue width)
  | AInt lvalue, Bit { width; _ } ->
      eval_binop_plusplus (bit_of_raw_int lvalue width) rvalue
  | _ ->
      Printf.sprintf "Invalid concatenation: %s ++ %s" (Value.print lvalue)
        (Value.print rvalue)
      |> failwith

let eval_binop_and (lvalue : Value.t) (rvalue : Value.t) : Value.t =
  match (lvalue, rvalue) with
  | Bool b1, Bool b2 -> Bool (b1 && b2)
  | _ ->
      Printf.sprintf "Invalid and operator: %s && %s" (Value.print lvalue)
        (Value.print rvalue)
      |> failwith

let eval_binop_or (lvalue : Value.t) (rvalue : Value.t) : Value.t =
  match (lvalue, rvalue) with
  | Bool b1, Bool b2 -> Bool (b1 || b2)
  | _ ->
      Printf.sprintf "Invalid or operator: %s || %s" (Value.print lvalue)
        (Value.print rvalue)
      |> failwith

let eval_binop (op : Op.bin) (lvalue : Value.t) (rvalue : Value.t) : Value.t =
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
  | Eq _ -> Bool (eval_binop_eq lvalue rvalue)
  | NotEq _ -> Bool (eval_binop_ne lvalue rvalue)
  | BitAnd _ -> eval_binop_bitand lvalue rvalue
  | BitXor _ -> eval_binop_bitxor lvalue rvalue
  | BitOr _ -> eval_binop_bitor lvalue rvalue
  | PlusPlus _ -> eval_binop_plusplus lvalue rvalue
  | And _ -> eval_binop_and lvalue rvalue
  | Or _ -> eval_binop_or lvalue rvalue

(* Bitslice evaluation *)

let eval_bitstring_access' (value : Bigint.t) (lvalue : Bigint.t)
    (hvalue : Bigint.t) : Value.t =
  let width = Bigint.(hvalue - lvalue + one) in
  let value = slice_bitstring value hvalue lvalue in
  Bit { value; width }

let eval_bitstring_access (value : Value.t) (lvalue : Value.t)
    (hvalue : Value.t) : Value.t =
  let extract value = extract_bigint value in
  eval_bitstring_access' (extract value) (extract lvalue) (extract hvalue)

(* Type cast evaluation *)

let eval_cast_to_bool (value : Value.t) : Value.t =
  match value with
  | Bool b -> Bool b
  | Bit { width; value } when width = Bigint.one -> Bool Bigint.(value = one)
  | AInt value -> Bool Bigint.(value = one)
  | _ -> failwith "cast to bool undefined"

let eval_cast_to_bit (width : Bigint.t) (value : Value.t) : Value.t =
  match value with
  | Bool b ->
      let value = if b then Bigint.one else Bigint.zero in
      Bit { value; width }
  | Int { value; _ } | Bit { value; _ } | AInt value ->
      bit_of_raw_int value width
  | _ ->
      Printf.sprintf "(TODO) Cast to bitstring undefined: %s"
        (Value.print value)
      |> failwith

let eval_cast_to_int (width : Bigint.t) (value : Value.t) : Value.t =
  match value with
  | Bit { value; _ } | Int { value; _ } | AInt value ->
      int_of_raw_int value width
  | _ ->
      Printf.sprintf "(TODO) Cast to integer undefined: %s"
        (Value.print value)
      |> failwith

let rec eval_cast_entries (entries : (string * Typ.t) list)
    (value : Value.t) : (string * Value.t) list =
  match value with
  | Tuple values ->
      assert (List.length entries = List.length values);
      List.map2
        (fun (field, typ) value ->
          let value = eval_cast typ value in
          (field, value))
        entries values
  | Header { entries; _ } | Struct { entries } -> entries
  | _ ->
      Printf.sprintf "(TODO) Cast to entries undefined: %s"
        (Value.print value)
      |> failwith

and eval_cast_tuple (typs : Typ.t list) (value : Value.t) : Value.t =
  match value with
  | Value.Tuple values ->
      assert (List.length typs = List.length values);
      let values =
        List.map2 eval_cast typs values
      in
      Value.Tuple values
  | _ ->
      Printf.sprintf "(TODO) Cast to tuple undefined: %s"
        (Value.print value)
      |> failwith

and eval_cast (typ : Typ.t) (value : Value.t) : Value.t =
  match typ with
  | Bool -> eval_cast_to_bool value
  | AInt ->
      let value = Value.extract_bigint value in
      Value.AInt value
  | Bit { width } ->
      eval_cast_to_bit width value
  | Int { width } ->
      eval_cast_to_int width value
  | Tuple typs ->
      eval_cast_tuple typs value
  | Header { entries } ->
      let entries = eval_cast_entries entries value in
      Value.Header { valid = true; entries }
  | Struct { entries } ->
      let entries = eval_cast_entries entries value in
      Value.Struct { entries }
  | _ ->
      Printf.sprintf "(TODO) Cast to type %s undefined"
        (Typ.print typ)
      |> failwith
