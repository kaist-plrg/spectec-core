open Lang.Xl
open Lang.Il
open Common.Source
open Builtins
open Atom
open Mixfix

(* Value set *)

module VSet = Set.Make (Value)

type set = VSet.t

(* Conversion between meta-sets and OCaml lists *)

let value_of_set (typ_key : typ) (set : set) : value =
  let value_elements = VSet.elements set |> Value.list typ_key in
  let value =
    let typ = Typ.var "set" [ typ_key ] in
    [ Atom (LBrace $ no_region); Arg value_elements; Atom (RBrace $ no_region) ]
    |> Value.Make.case typ
  in
  value

(* dec $intersect_set<K>(set<K>, set<K>) : set<K> *)

let intersect_set ~at (key_typ : targ) (set_a : VSet.t) (set_b : VSet.t) =
  at |> ignore;
  let result_set = VSet.inter set_a set_b in
  Ok (value_of_set key_typ result_set)

(* dec $union_set<K>(set<K>, set<K>) : set<K> *)

let union_set ~at (key_typ : targ) (set_a : VSet.t) (set_b : VSet.t) =
  at |> ignore;
  let result_set = VSet.union set_a set_b in
  Ok (value_of_set key_typ result_set)

(* dec $unions_set<K>(set<K>* ) : set<K> *)

let unions_set ~at (key_typ : targ) (sets : VSet.t list) =
  at |> ignore;
  let result_set = List.fold_left VSet.union VSet.empty sets in
  Ok (value_of_set key_typ result_set)

(* dec $diff_set<K>(set<K>, set<K>) : set<K> *)

let diff_set ~at (key_typ : targ) (set_a : VSet.t) (set_b : VSet.t) =
  at |> ignore;
  let result_set = VSet.diff set_a set_b in
  Ok (value_of_set key_typ result_set)

(* dec $sub_set<K>(set<K>, set<K>) : bool *)

let sub_set ~at (_key_typ : targ) (set_a : VSet.t) (set_b : VSet.t) =
  at |> ignore;
  Ok (Value.bool (VSet.subset set_a set_b))

(* dec $eq_set<K>(set<K>, set<K>) : bool *)

let eq_set ~at (_key_typ : targ) (set_a : VSet.t) (set_b : VSet.t) =
  at |> ignore;
  Ok (Value.bool (VSet.equal set_a set_b))

let builtins : (string * Define.t) list =
  [
    ("intersect_set", Define.T1.a2 Arg.set Arg.set intersect_set);
    ("union_set", Define.T1.a2 Arg.set Arg.set union_set);
    ("unions_set", Define.T1.a1 (Arg.list_of Arg.set) unions_set);
    ("diff_set", Define.T1.a2 Arg.set Arg.set diff_set);
    ("sub_set", Define.T1.a2 Arg.set Arg.set sub_set);
    ("eq_set", Define.T1.a2 Arg.set Arg.set eq_set);
  ]
