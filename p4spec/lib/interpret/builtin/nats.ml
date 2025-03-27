open Il.Ast
module Value = Runtime_dynamic.Value
open Util.Source

(* dec $sum(nat* ) : nat *)

let sum (at : region) (targs : targ list) (values_input : value list) : value =
  Extract.zero at targs;
  let values =
    Extract.one at values_input
    |> Value.get_list
    |> List.map Numerics.bigint_of_value
  in
  let sum = List.fold_left Bigint.( + ) Bigint.zero values in
  Numerics.value_of_bigint sum

(* dec $max(nat* ) : nat *)

let max (at : region) (targs : targ list) (values_input : value list) : value =
  Extract.zero at targs;
  let values =
    Extract.one at values_input
    |> Value.get_list
    |> List.map Numerics.bigint_of_value
  in
  let max = List.fold_left Bigint.max Bigint.zero values in
  Numerics.value_of_bigint max

(* dec $min(nat* ) : nat *)

let min (at : region) (targs : targ list) (values_input : value list) : value =
  Extract.zero at targs;
  let values =
    Extract.one at values_input
    |> Value.get_list
    |> List.map Numerics.bigint_of_value
  in
  let min = List.fold_left Bigint.min Bigint.zero values in
  Numerics.value_of_bigint min
