open Xl
open Sl.Ast
module Value = Runtime_dynamic_il.Value
open Error
open Util.Source

(* Value set *)

module VSet = Set.Make (Value)

type set = VSet.t

(* Conversion between meta-sets and OCaml lists *)

let set_of_value (value : value) : set =
  match value with
  | CaseV ([ [ { it = Atom "SET"; _ } ]; [] ], [ value_elements ]) ->
      let values_element = Value.get_list value_elements in
      VSet.of_list values_element
  | _ ->
      error no_region
        (Format.asprintf "expected a set, but got %s" (Value.to_string value))

let value_of_set (set : set) : value =
  let values_element = VSet.elements set in
  let value_elements = Il.Ast.ListV values_element in
  CaseV ([ [ Atom.Atom "SET" $ no_region ]; [] ], [ value_elements ])

(* dec $intersect_set<K>(set<K>, set<K>) : set<K> *)

let intersect_set (at : region) (targs : targ list) (values_input : value list)
    : value =
  let _typ_key = Extract.one at targs in
  let value_set_a, value_set_b = Extract.two at values_input in
  let set_a = set_of_value value_set_a in
  let set_b = set_of_value value_set_b in
  VSet.inter set_a set_b |> value_of_set

(* dec $union_set<K>(set<K>, set<K>) : set<K> *)

let union_set (at : region) (targs : targ list) (values_input : value list) :
    value =
  let _typ_key = Extract.one at targs in
  let value_set_a, value_set_b = Extract.two at values_input in
  let set_a = set_of_value value_set_a in
  let set_b = set_of_value value_set_b in
  VSet.union set_a set_b |> value_of_set

(* dec $unions_set<K>(set<K>* ) : set<K> *)

let unions_set (at : region) (targs : targ list) (values_input : value list) :
    value =
  let _typ_key = Extract.one at targs in
  let value_sets = Extract.one at values_input in
  let sets = Value.get_list value_sets |> List.map set_of_value in
  List.fold_left VSet.union VSet.empty sets |> value_of_set

(* dec $diff_set<K>(set<K>, set<K>) : set<K> *)

let diff_set (at : region) (targs : targ list) (values_input : value list) :
    value =
  let _typ_key = Extract.one at targs in
  let value_set_a, value_set_b = Extract.two at values_input in
  let set_a = set_of_value value_set_a in
  let set_b = set_of_value value_set_b in
  VSet.diff set_a set_b |> value_of_set

(* dec $sub_set<K>(set<K>, set<K>) : bool *)

let sub_set (at : region) (targs : targ list) (values_input : value list) :
    value =
  let _typ_key = Extract.one at targs in
  let value_set_a, value_set_b = Extract.two at values_input in
  let set_a = set_of_value value_set_a in
  let set_b = set_of_value value_set_b in
  BoolV (VSet.subset set_a set_b)

(* dec $eq_set<K>(set<K>, set<K>) : bool *)

let eq_set (at : region) (targs : targ list) (values_input : value list) : value
    =
  let _typ_key = Extract.one at targs in
  let value_set_a, value_set_b = Extract.two at values_input in
  let set_a = set_of_value value_set_a in
  let set_b = set_of_value value_set_b in
  BoolV (VSet.equal set_a set_b)
