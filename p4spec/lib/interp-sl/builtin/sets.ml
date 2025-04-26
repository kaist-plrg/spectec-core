open Xl
open Sl.Ast
module Value = Runtime_dynamic_sl.Value
module Dep = Runtime_testgen.Dep
open Error
open Util.Source

(* Value set *)

module VSet = Set.Make (Value)

type set = VSet.t

(* Conversion between meta-sets and OCaml lists *)

let set_of_value (value : value) : set =
  match value.it with
  | CaseV
      ( [ [ { it = Atom.LBrace; _ } ]; [ { it = Atom.RBrace; _ } ] ],
        [ value_elements ] ) ->
      let values_element = Value.get_list value_elements in
      VSet.of_list values_element
  | _ ->
      error no_region
        (Format.asprintf "expected a set, but got %s" (Value.to_string value))

let value_of_set (ctx : Ctx.t) (typ_key : typ) (set : set) : value =
  let values_element = VSet.elements set in
  let value_elements =
    let vid = Dep.Graph.fresh () in
    let typ = Il.Ast.IterT (typ_key, Il.Ast.List) in
    ListV values_element $$$ { vid; typ }
  in
  Ctx.add_node ctx value_elements;
  let value =
    let vid = Dep.Graph.fresh () in
    let typ = Il.Ast.VarT ("set" $ no_region, [ typ_key ]) in
    CaseV
      ( [ [ Atom.LBrace $ no_region ]; [ Atom.RBrace $ no_region ] ],
        [ value_elements ] )
    $$$ { vid; typ }
  in
  Ctx.add_node ctx value;
  value

(* dec $intersect_set<K>(set<K>, set<K>) : set<K> *)

let intersect_set (ctx : Ctx.t) (at : region) (targs : targ list)
    (values_input : value list) : value =
  let typ_key = Extract.one at targs in
  let value_set_a, value_set_b = Extract.two at values_input in
  let set_a = set_of_value value_set_a in
  let set_b = set_of_value value_set_b in
  VSet.inter set_a set_b |> value_of_set ctx typ_key

(* dec $union_set<K>(set<K>, set<K>) : set<K> *)

let union_set (ctx : Ctx.t) (at : region) (targs : targ list)
    (values_input : value list) : value =
  let typ_key = Extract.one at targs in
  let value_set_a, value_set_b = Extract.two at values_input in
  let set_a = set_of_value value_set_a in
  let set_b = set_of_value value_set_b in
  VSet.union set_a set_b |> value_of_set ctx typ_key

(* dec $unions_set<K>(set<K>* ) : set<K> *)

let unions_set (ctx : Ctx.t) (at : region) (targs : targ list)
    (values_input : value list) : value =
  let typ_key = Extract.one at targs in
  let value_sets = Extract.one at values_input in
  let sets = Value.get_list value_sets |> List.map set_of_value in
  List.fold_left VSet.union VSet.empty sets |> value_of_set ctx typ_key

(* dec $diff_set<K>(set<K>, set<K>) : set<K> *)

let diff_set (ctx : Ctx.t) (at : region) (targs : targ list)
    (values_input : value list) : value =
  let typ_key = Extract.one at targs in
  let value_set_a, value_set_b = Extract.two at values_input in
  let set_a = set_of_value value_set_a in
  let set_b = set_of_value value_set_b in
  VSet.diff set_a set_b |> value_of_set ctx typ_key

(* dec $sub_set<K>(set<K>, set<K>) : bool *)

let sub_set (ctx : Ctx.t) (at : region) (targs : targ list)
    (values_input : value list) : value =
  let _typ_key = Extract.one at targs in
  let value_set_a, value_set_b = Extract.two at values_input in
  let set_a = set_of_value value_set_a in
  let set_b = set_of_value value_set_b in
  let value =
    let vid = Dep.Graph.fresh () in
    let typ = Il.Ast.BoolT in
    BoolV (VSet.subset set_a set_b) $$$ { vid; typ }
  in
  Ctx.add_node ctx value;
  value

(* dec $eq_set<K>(set<K>, set<K>) : bool *)

let eq_set (ctx : Ctx.t) (at : region) (targs : targ list)
    (values_input : value list) : value =
  let _typ_key = Extract.one at targs in
  let value_set_a, value_set_b = Extract.two at values_input in
  let set_a = set_of_value value_set_a in
  let set_b = set_of_value value_set_b in
  let value =
    let vid = Dep.Graph.fresh () in
    let typ = Il.Ast.BoolT in
    BoolV (VSet.equal set_a set_b) $$$ { vid; typ }
  in
  Ctx.add_node ctx value;
  value
