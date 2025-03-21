open Xl
open Il.Ast
module Value = Runtime_dynamic.Value
open Error
open Util.Source

(* Fresh type identifiers *)

let ctr = ref 0

let fresh_tid (at : region) (targs : targ list) (values_input : value list) :
    value =
  check (targs = []) at "arity mismatch in type arguments";
  check (values_input = []) at "arity mismatch in arguments";
  let tid = "FRESH__" ^ string_of_int !ctr in
  let value_tid = TextV tid $$ (no_region, VarT ("tid" $ no_region, [])) in
  ctr := !ctr + 1;
  value_tid

(* Numerics *)

let bigint_of_value (value : value) : Bigint.t =
  value |> Value.get_num |> Num.to_int |> Z.to_int |> Bigint.of_int

let value_of_bigint (i : Bigint.t) : value =
  let i = i |> Bigint.to_int_exn |> Z.of_int in
  let n = `Int i in
  NumV n $$ (no_region, NumT (Num.to_typ n))

(* Shift bitstring left *)

let rec shl' (v : Bigint.t) (o : Bigint.t) : Bigint.t =
  if Bigint.(o > zero) then shl' Bigint.(v * (one + one)) Bigint.(o - one)
  else v

let shl (at : region) (targs : targ list) (values_input : value list) : value =
  check (targs = []) at "arity mismatch in type arguments";
  let value_base, value_offset =
    match values_input with
    | [ value_base; value_offset ] -> (value_base, value_offset)
    | _ -> error at "arity mismatch in arguments"
  in
  let base = bigint_of_value value_base in
  let offset = bigint_of_value value_offset in
  shl' base offset |> value_of_bigint

(* Shift bitstring right *)

let rec shr' (v : Bigint.t) (o : Bigint.t) : Bigint.t =
  if Bigint.(o > zero) then
    let v_shifted = Bigint.(v / (one + one)) in
    shr' v_shifted Bigint.(o - one)
  else v

let shr (at : region) (targs : targ list) (values_input : value list) : value =
  check (targs = []) at "arity mismatch in type arguments";
  let value_base, value_offset =
    match values_input with
    | [ value_base; value_offset ] -> (value_base, value_offset)
    | _ -> error at "arity mismatch in arguments"
  in
  let base = bigint_of_value value_base in
  let offset = bigint_of_value value_offset in
  shr' base offset |> value_of_bigint

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
  check (targs = []) at "arity mismatch in type arguments";
  let value_base, value_offset, value_modulus =
    match values_input with
    | [ value_base; value_offset; value_modulus ] ->
        (value_base, value_offset, value_modulus)
    | _ -> error at "arity mismatch in arguments"
  in
  let base = bigint_of_value value_base in
  let offset = bigint_of_value value_offset in
  let modulus = bigint_of_value value_modulus in
  shr_arith' base offset modulus |> value_of_bigint

(* Power of two *)

let pow2' (w : Bigint.t) : Bigint.t = shl' Bigint.one w

let pow2 (at : region) (targs : targ list) (values_input : value list) : value =
  check (targs = []) at "arity mismatch in type arguments";
  let value_width =
    match values_input with
    | [ value_width ] -> value_width
    | _ -> error at "arity mismatch in arguments"
  in
  let width = bigint_of_value value_width in
  pow2' width |> value_of_bigint

(* Conversion from bitstring to integer *)

let rec to_int' (w : Bigint.t) (n : Bigint.t) : Bigint.t =
  let two = Bigint.(one + one) in
  let w' = pow2' w in
  if Bigint.(n >= w' / two) then to_int' w Bigint.(n - w')
  else if Bigint.(n < -(w' / two)) then to_int' w Bigint.(n + w')
  else n

let to_int (at : region) (targs : targ list) (values_input : value list) : value
    =
  check (targs = []) at "arity mismatch in type arguments";
  let value_width, value_bitstr =
    match values_input with
    | [ value_width; value_bitstr ] -> (value_width, value_bitstr)
    | _ -> error at "arity mismatch in arguments"
  in
  let width = bigint_of_value value_width in
  let bitstr = bigint_of_value value_bitstr in
  to_int' width bitstr |> value_of_bigint

(* Conversion from integer to bitstring *)

let rec to_bitstr' (w : Bigint.t) (n : Bigint.t) : Bigint.t =
  let w' = pow2' w in
  if Bigint.(n >= w') then Bigint.(n % w')
  else if Bigint.(n < zero) then to_bitstr' Bigint.(n + w') w
  else n

let to_bitstr (at : region) (targs : targ list) (values_input : value list) :
    value =
  check (targs = []) at "arity mismatch in type arguments";
  let value_width, value_int =
    match values_input with
    | [ value_width; value_int ] -> (value_width, value_int)
    | _ -> error at "arity mismatch in arguments"
  in
  let width = bigint_of_value value_width in
  let rawint = bigint_of_value value_int in
  to_bitstr' width rawint |> value_of_bigint

(* Bitwise operations *)

let bneg (at : region) (targs : targ list) (values_input : value list) : value =
  check (targs = []) at "arity mismatch in type arguments";
  let value =
    match values_input with
    | [ value ] -> value
    | _ -> error at "arity mismatch in arguments"
  in
  let rawint = bigint_of_value value in
  Bigint.bit_not rawint |> value_of_bigint

let band (at : region) (targs : targ list) (values_input : value list) : value =
  check (targs = []) at "arity mismatch in type arguments";
  let value_l, value_r =
    match values_input with
    | [ value_l; value_r ] -> (value_l, value_r)
    | _ -> error at "arity mismatch in arguments"
  in
  let rawint_l = bigint_of_value value_l in
  let rawint_r = bigint_of_value value_r in
  Bigint.bit_and rawint_l rawint_r |> value_of_bigint

let bxor (at : region) (targs : targ list) (values_input : value list) : value =
  check (targs = []) at "arity mismatch in type arguments";
  let value_l, value_r =
    match values_input with
    | [ value_l; value_r ] -> (value_l, value_r)
    | _ -> error at "arity mismatch in arguments"
  in
  let rawint_l = bigint_of_value value_l in
  let rawint_r = bigint_of_value value_r in
  Bigint.bit_xor rawint_l rawint_r |> value_of_bigint

let bor (at : region) (targs : targ list) (values_input : value list) : value =
  check (targs = []) at "arity mismatch in type arguments";
  let value_l, value_r =
    match values_input with
    | [ value_l; value_r ] -> (value_l, value_r)
    | _ -> error at "arity mismatch in arguments"
  in
  let rawint_l = bigint_of_value value_l in
  let rawint_r = bigint_of_value value_r in
  Bigint.bit_or rawint_l rawint_r |> value_of_bigint

(* Initializer *)

let init () : unit = ctr := 0

(* Builtin calls *)

module Funcs = Map.Make (String)

let funcs =
  Funcs.empty
  |> Funcs.add "fresh_tid" (fun at targs values_input ->
         fresh_tid at targs values_input)
  |> Funcs.add "shl" (fun at targs values_input -> shl at targs values_input)
  |> Funcs.add "shr" (fun at targs values_input -> shr at targs values_input)
  |> Funcs.add "shr_arith" (fun at targs values_input ->
         shr_arith at targs values_input)
  |> Funcs.add "pow2" (fun at targs values_input -> pow2 at targs values_input)
  |> Funcs.add "to_int" (fun at targs values_input ->
         to_int at targs values_input)
  |> Funcs.add "to_bitstr" (fun at targs values_input ->
         to_bitstr at targs values_input)
  |> Funcs.add "bneg" (fun at targs values_input -> bneg at targs values_input)
  |> Funcs.add "band" (fun at targs values_input -> band at targs values_input)
  |> Funcs.add "bxor" (fun at targs values_input -> bxor at targs values_input)
  |> Funcs.add "bor" (fun at targs values_input -> bor at targs values_input)

let is_builtin (id : id) : bool = Funcs.mem id.it funcs

let invoke (id : id) (targs : targ list) (args : value list) : value =
  let func = Funcs.find_opt id.it funcs in
  check (Option.is_some func) id.at
    (Format.asprintf "implementation for builtin %s is missing" id.it);
  let func = Option.get func in
  func id.at targs args
