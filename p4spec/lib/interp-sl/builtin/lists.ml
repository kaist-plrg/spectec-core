open Xl
open Sl.Ast
module Value = Runtime_dynamic_sl.Value
module Dep = Runtime_testgen.Dep
open Util.Source

(* dec $rev_<X>(X* ) : X* *)

let rev_ (ctx : Ctx.t) (at : region) (targs : targ list)
    (values_input : value list) : value =
  let _typ = Extract.one at targs in
  let values = Extract.one at values_input |> Value.get_list in
  let value = ListV (List.rev values) $$$ Dep.Graph.fresh () in
  Ctx.add_node ctx value;
  value

(* dec $concat_<X>((X* )* ) : X* *)

let concat_ (ctx : Ctx.t) (at : region) (targs : targ list)
    (values_input : value list) : value =
  let _typ = Extract.one at targs in
  let values =
    Extract.one at values_input
    |> Value.get_list
    |> List.concat_map Value.get_list
  in
  let value = ListV values $$$ Dep.Graph.fresh () in
  Ctx.add_node ctx value;
  value

(* dec $distinct_<K>(K* ) : bool *)

let distinct_ (ctx : Ctx.t) (at : region) (targs : targ list)
    (values_input : value list) : value =
  let _typ = Extract.one at targs in
  let values = Extract.one at values_input |> Value.get_list in
  let set = Sets.VSet.of_list values in
  let value =
    BoolV (Sets.VSet.cardinal set = List.length values) $$$ Dep.Graph.fresh ()
  in
  Ctx.add_node ctx value;
  value

(* dec $partition_<X>(X*, nat) : (X*, X* ) *)

let partition_ (ctx : Ctx.t) (at : region) (targs : targ list)
    (values_input : value list) : value =
  let _typ = Extract.one at targs in
  let value_list, value_len = Extract.two at values_input in
  let values = Value.get_list value_list in
  let len = value_len |> Value.get_num |> Num.to_int |> Bigint.to_int_exn in
  let values_left, values_right =
    values
    |> List.mapi (fun idx value -> (idx, value))
    |> List.partition (fun (idx, _) -> idx < len)
  in
  let value_left = ListV (List.map snd values_left) $$$ Dep.Graph.fresh () in
  Ctx.add_node ctx value_left;
  let value_right = ListV (List.map snd values_right) $$$ Dep.Graph.fresh () in
  Ctx.add_node ctx value_right;
  let value = TupleV [ value_left; value_right ] $$$ Dep.Graph.fresh () in
  Ctx.add_node ctx value;
  value

(* dec $assoc_<X, Y>(X, (X, Y)* ) : Y? *)

let assoc_ (ctx : Ctx.t) (at : region) (targs : targ list)
    (values_input : value list) : value =
  let _typ_key, _typ_value = Extract.two at targs in
  let value, value_list = Extract.two at values_input in
  let values =
    Value.get_list value_list
    |> List.map (fun value ->
           match value.it with
           | TupleV [ value_key; value_value ] -> (value_key, value_value)
           | _ -> assert false)
  in
  let value =
    List.fold_left
      (fun value_found (value_key, value_value) ->
        match value_found with
        | Some _ -> value_found
        | None when Value.compare value value_key = 0 -> Some value_value
        | None -> None)
      None values
  in
  let value =
    match value with
    | Some value -> OptV (Some value) $$$ Dep.Graph.fresh ()
    | None -> OptV None $$$ Dep.Graph.fresh ()
  in
  Ctx.add_node ctx value;
  value
