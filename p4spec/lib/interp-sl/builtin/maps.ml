open Xl
open Sl.Ast
module Value = Runtime_dynamic_sl.Value
module Dep = Runtime_testgen.Dep
open Error
open Util.Source

(* Value map *)

module VMap = Map.Make (Value)

type map = Value.t VMap.t

(* Conversion between meta-maps and OCaml assoc lists *)

let map_of_value (value : value) : map =
  let tuple_of_value (value : value) : value * value =
    match value.it with
    | CaseV ([ []; [ { it = Atom.Arrow; _ } ]; [] ], [ value_key; value_value ])
      ->
        (value_key, value_value)
    | _ ->
        error no_region
          (Format.asprintf "expected a pair, but got %s" (Value.to_string value))
  in
  match value.it with
  | CaseV
      ( [ [ { it = Atom.LBrace; _ } ]; [ { it = Atom.RBrace; _ } ] ],
        [ value_pairs ] ) ->
      Value.get_list value_pairs |> List.map tuple_of_value
      |> List.fold_left
           (fun map (value_key, value_value) ->
             VMap.add value_key value_value map)
           VMap.empty
  | _ ->
      error no_region
        (Format.asprintf "expected a map, but got %s" (Value.to_string value))

let value_of_map (ctx : Ctx.t) (map : map) : value =
  let value_of_tuple ((value_key, value_value) : value * value) : value =
    let value =
      CaseV ([ []; [ Atom.Arrow $ no_region ]; [] ], [ value_key; value_value ])
      $$$ Dep.Graph.fresh ()
    in
    Ctx.add_node ctx value;
    value
  in
  let value_pairs =
    ListV (VMap.bindings map |> List.map value_of_tuple) $$$ Dep.Graph.fresh ()
  in
  Ctx.add_node ctx value_pairs;
  let value =
    CaseV
      ( [ [ Atom.LBrace $ no_region ]; [ Atom.RBrace $ no_region ] ],
        [ value_pairs ] )
    $$$ Dep.Graph.fresh ()
  in
  Ctx.add_node ctx value;
  value

(* Built-in implementations *)

(* dec $find_map<K, V>(map<K, V>, K) : V? *)

let find_map (ctx : Ctx.t) (at : region) (targs : targ list)
    (values_input : value list) : value =
  let _typ_key, _typ_value = Extract.two at targs in
  let value_map, value_key = Extract.two at values_input in
  let map = map_of_value value_map in
  let value_opt = VMap.find_opt value_key map in
  let value = OptV value_opt $$$ Dep.Graph.fresh () in
  Ctx.add_node ctx value;
  value

(* dec $find_maps<K, V>(map<K, V>*, K) : V? *)

let find_maps (ctx : Ctx.t) (at : region) (targs : targ list)
    (values_input : value list) : value =
  let _typ_key, _typ_value = Extract.two at targs in
  let value_maps, value_key = Extract.two at values_input in
  let maps = value_maps |> Value.get_list |> List.map map_of_value in
  let value_opt =
    List.fold_left
      (fun value_opt map ->
        match value_opt with
        | Some _ -> value_opt
        | None -> VMap.find_opt value_key map)
      None maps
  in
  let value = OptV value_opt $$$ Dep.Graph.fresh () in
  Ctx.add_node ctx value;
  value

(* dec $add_map<K, V>(map<K, V>, K, V) : map<K, V> *)

let add_map (ctx : Ctx.t) (at : region) (targs : targ list)
    (values_input : value list) : value =
  let _typ_key, _typ_value = Extract.two at targs in
  let value_map, value_key, value_value = Extract.three at values_input in
  map_of_value value_map |> VMap.add value_key value_value |> value_of_map ctx

(* dec $adds_map<K, V>(map<K, V>, K*, V* ) : map<K, V> *)

let adds_map (ctx : Ctx.t) (at : region) (targs : targ list)
    (values_input : value list) : value =
  let _typ_key, _typ_value = Extract.two at targs in
  let value_map, value_keys, value_values = Extract.three at values_input in
  let map = map_of_value value_map in
  let values_key = value_keys |> Value.get_list in
  let values_value = value_values |> Value.get_list in
  List.fold_left2
    (fun map value_key value_value -> VMap.add value_key value_value map)
    map values_key values_value
  |> value_of_map ctx

(* dec $update_map<K, V>(map<K, V>, K, V) : map<K, V> *)

let update_map (ctx : Ctx.t) (at : region) (targs : targ list)
    (values_input : value list) : value =
  let _typ_key, _typ_value = Extract.two at targs in
  let value_map, value_key, value_value = Extract.three at values_input in
  map_of_value value_map |> VMap.add value_key value_value |> value_of_map ctx
