open Xl
open Il.Ast
module Value = Runtime_dynamic_il.Value
open Util.Source

(* Conversion between meta-numerics and OCaml numerics *)

let bigint_of_value (value : value) : Bigint.t =
  value |> Value.get_num |> Num.to_int

let value_of_bigint (i : Bigint.t) : value =
  let value =
    let vid = Runtime_dynamic.Vid.fresh () in
    let typ = Il.Ast.NumT `IntT in
    NumV (`Int i) $$$ { vid; typ }
  in
  value

(* Built-in implementations *)

(* dec $shl(int, int) : int *)

let rec shl' (v : Bigint.t) (o : Bigint.t) : Bigint.t =
  if Bigint.(o > zero) then shl' Bigint.(v * (one + one)) Bigint.(o - one)
  else v

let shl (at : region) (targs : targ list) (values_input : value list) : value =
  Extract.zero at targs;
  let value_base, value_offset = Extract.two at values_input in
  let base = bigint_of_value value_base in
  let offset = bigint_of_value value_offset in
  shl' base offset |> value_of_bigint

(* dec $shr(int, int) : int *)

let rec shr' (v : Bigint.t) (o : Bigint.t) : Bigint.t =
  if Bigint.(o > zero) then
    let v_shifted = Bigint.(v / (one + one)) in
    shr' v_shifted Bigint.(o - one)
  else v

let shr (at : region) (targs : targ list) (values_input : value list) : value =
  Extract.zero at targs;
  let value_base, value_offset = Extract.two at values_input in
  let base = bigint_of_value value_base in
  let offset = bigint_of_value value_offset in
  shr' base offset |> value_of_bigint

(* dec $shr_arith(int, int, int) : int *)

let shr_arith' (v : Bigint.t) (o : Bigint.t) (m : Bigint.t) : Bigint.t =
  let rec shr_arith'' (v : Bigint.t) (o : Bigint.t) : Bigint.t =
    if Bigint.(o > zero) then
      let v_shifted = Bigint.((v / (one + one)) + m) in
      shr_arith'' v_shifted Bigint.(o - one)
    else v
  in
  shr_arith'' v o

let shr_arith (at : region) (targs : targ list) (values_input : value list) :
    value =
  Extract.zero at targs;
  let value_base, value_offset, value_modulus = Extract.three at values_input in
  let base = bigint_of_value value_base in
  let offset = bigint_of_value value_offset in
  let modulus = bigint_of_value value_modulus in
  shr_arith' base offset modulus |> value_of_bigint

(* dec $pow2(nat) : int *)

let pow2' (w : Bigint.t) : Bigint.t = shl' Bigint.one w

let pow2 (at : region) (targs : targ list) (values_input : value list) : value =
  Extract.zero at targs;
  let value_width = Extract.one at values_input in
  let width = bigint_of_value value_width in
  pow2' width |> value_of_bigint

(* dec $to_int(int, bitstr) : int *)

let rec to_int' (w : Bigint.t) (n : Bigint.t) : Bigint.t =
  let two = Bigint.(one + one) in
  let w' = pow2' w in
  if Bigint.(n >= w' / two) then to_int' w Bigint.(n - w')
  else if Bigint.(n < -(w' / two)) then to_int' w Bigint.(n + w')
  else n

let to_int (at : region) (targs : targ list) (values_input : value list) : value
    =
  Extract.zero at targs;
  let value_width, value_bitstr = Extract.two at values_input in
  let width = bigint_of_value value_width in
  let bitstr = bigint_of_value value_bitstr in
  to_int' width bitstr |> value_of_bigint

(* dec $to_bitstr(int, int) : bitstr *)

let rec to_bitstr' (w : Bigint.t) (n : Bigint.t) : Bigint.t =
  let w' = pow2' w in
  if Bigint.(n >= w') then Bigint.(n % w')
  else if Bigint.(n < zero) then to_bitstr' w Bigint.(n + w')
  else n

let to_bitstr (at : region) (targs : targ list) (values_input : value list) :
    value =
  Extract.zero at targs;
  let value_width, value_int = Extract.two at values_input in
  let width = bigint_of_value value_width in
  let rawint = bigint_of_value value_int in
  to_bitstr' width rawint |> value_of_bigint

(* dec $bneg(int) : int *)

let bneg (at : region) (targs : targ list) (values_input : value list) : value =
  Extract.zero at targs;
  let value = Extract.one at values_input in
  let rawint = bigint_of_value value in
  Bigint.bit_not rawint |> value_of_bigint

(* dec $band(int, int) : int *)

let band (at : region) (targs : targ list) (values_input : value list) : value =
  Extract.zero at targs;
  let value_l, value_r = Extract.two at values_input in
  let rawint_l = bigint_of_value value_l in
  let rawint_r = bigint_of_value value_r in
  Bigint.bit_and rawint_l rawint_r |> value_of_bigint

(* dec $bxor(int, int) : int *)

let bxor (at : region) (targs : targ list) (values_input : value list) : value =
  Extract.zero at targs;
  let value_l, value_r = Extract.two at values_input in
  let rawint_l = bigint_of_value value_l in
  let rawint_r = bigint_of_value value_r in
  Bigint.bit_xor rawint_l rawint_r |> value_of_bigint

(* dec $bor(int, int) : int *)

let bor (at : region) (targs : targ list) (values_input : value list) : value =
  Extract.zero at targs;
  let value_l, value_r = Extract.two at values_input in
  let rawint_l = bigint_of_value value_l in
  let rawint_r = bigint_of_value value_r in
  Bigint.bit_or rawint_l rawint_r |> value_of_bigint

(* dec $bitacc(int, int, int) : int *)

let bitacc' (n : Bigint.t) (m : Bigint.t) (l : Bigint.t) : Bigint.t =
  let slice_width = Bigint.(m + one - l) in
  if Bigint.(l < zero) then
    raise (Invalid_argument "bitslice x[y:z] must have y > z > 0");
  let shifted = Bigint.(n asr to_int_exn l) in
  let mask = Bigint.(pow2' slice_width - one) in
  Bigint.bit_and shifted mask

let bitacc (at : region) (targs : targ list) (values_input : value list) : value
    =
  Extract.zero at targs;
  let value_b, value_h, value_l = Extract.three at values_input in
  let rawint_b = bigint_of_value value_b in
  let rawint_h = bigint_of_value value_h in
  let rawint_l = bigint_of_value value_l in
  bitacc' rawint_b rawint_h rawint_l |> value_of_bigint
