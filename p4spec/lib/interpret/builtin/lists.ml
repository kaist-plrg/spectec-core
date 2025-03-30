open Il.Ast
module Value = Runtime_dynamic.Value
open Util.Source

(* dec $rev_<X>(X* ) : X* *)

let rev_ (at : region) (targs : targ list) (values_input : value list) : value =
  let _typ = Extract.one at targs in
  let values = Extract.one at values_input |> Value.get_list in
  ListV (List.rev values)

(* dec $concat_<X>((X* )* ) : X* *)

let concat_ (at : region) (targs : targ list) (values_input : value list) :
    value =
  let _typ = Extract.one at targs in
  let values =
    Extract.one at values_input
    |> Value.get_list
    |> List.concat_map Value.get_list
  in
  ListV values

(* dec $distinct_<K>(K* ) : bool *)

let distinct_ (at : region) (targs : targ list) (values_input : value list) :
    value =
  let _typ = Extract.one at targs in
  let values = Extract.one at values_input |> Value.get_list in
  let set = Sets.VSet.of_list values in
  BoolV (Sets.VSet.cardinal set = List.length values)
