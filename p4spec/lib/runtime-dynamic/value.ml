open Xl
open Il.Ast
open Il.Print

(* Value *)

type t = value

(* Stringifier *)

let to_string t = string_of_value t

(* Comparison *)

let rec compare (value_l : t) (value_r : t) =
  let tag (value : t) =
    match value with
    | BoolV _ -> 0
    | NumV _ -> 1
    | TextV _ -> 2
    | StructV _ -> 3
    | CaseV _ -> 4
    | TupleV _ -> 5
    | OptV _ -> 6
    | ListV _ -> 7
    | FuncV _ -> 8
  in
  match (value_l, value_r) with
  | BoolV b_l, BoolV b_r -> Stdlib.compare b_l b_r
  | NumV n_l, NumV n_r -> Num.compare n_l n_r
  | TextV s_l, TextV s_r -> String.compare s_l s_r
  | StructV fields_l, StructV fields_r ->
      let atoms_l, values_l = List.split fields_l in
      let atoms_r, values_r = List.split fields_r in
      let cmp_atoms = List.compare Atom.compare atoms_l atoms_r in
      if cmp_atoms <> 0 then cmp_atoms else compares values_l values_r
  | CaseV (mixop_l, values_l), CaseV (mixop_r, values_r) ->
      let cmp_mixop = Mixop.compare mixop_l mixop_r in
      if cmp_mixop <> 0 then cmp_mixop else compares values_l values_r
  | TupleV values_l, TupleV values_r -> compares values_l values_r
  | OptV value_opt_l, OptV value_opt_r -> (
      match (value_opt_l, value_opt_r) with
      | Some value_l, Some value_r -> compare value_l value_r
      | Some _, None -> 1
      | None, Some _ -> -1
      | None, None -> 0)
  | ListV values_l, ListV values_r -> compares values_l values_r
  | _ -> Int.compare (tag value_l) (tag value_r)

and compares (values_l : t list) (values_r : t list) : int =
  match (values_l, values_r) with
  | [], [] -> 0
  | [], _ :: _ -> -1
  | _ :: _, [] -> 1
  | value_l :: values_l, value_r :: values_r ->
      let cmp = compare value_l value_r in
      if cmp <> 0 then cmp else compares values_l values_r

(* Equality *)

let rec eq (value_l : t) (value_r : t) : bool =
  match (value_l, value_r) with
  | BoolV b_l, BoolV b_r -> b_l = b_r
  | NumV n_l, NumV n_r -> Num.eq n_l n_r
  | TextV s_l, TextV s_r -> s_l = s_r
  | StructV fields_l, StructV fields_r ->
      let atoms_l, values_l = List.split fields_l in
      let atoms_r, values_r = List.split fields_r in
      List.length atoms_l = List.length atoms_r
      && List.for_all2 Atom.eq atoms_l atoms_r
      && eqs values_l values_r
  | CaseV (mixop_l, values_l), CaseV (mixop_r, values_r) ->
      Mixop.eq mixop_l mixop_r && eqs values_l values_r
  | TupleV values_l, TupleV values_r -> eqs values_l values_r
  | OptV value_opt_l, OptV value_opt_r -> (
      match (value_opt_l, value_opt_r) with
      | Some value_l, Some value_r -> eq value_l value_r
      | None, None -> true
      | _ -> false)
  | ListV values_l, ListV values_r -> eqs values_l values_r
  | _ ->
      failwith
        (Format.asprintf "TODO %s vs %s" (to_string value_l) (to_string value_r))

and eqs (values_l : t list) (values_r : t list) : bool =
  List.length values_l = List.length values_r
  && List.for_all2 eq values_l values_r

(* Boolean *)

let get_bool (value : t) =
  match value with BoolV b -> b | _ -> failwith "get_bool"

(* Number *)

let get_num (value : t) =
  match value with NumV n -> n | _ -> failwith "get_num"

(* Text *)

let get_text (value : t) =
  match value with TextV s -> s | _ -> failwith "get_text"

(* List *)

let to_list (values : t list) = ListV values

let get_list (value : t) =
  match value with ListV values -> values | _ -> failwith "unseq"

(* Option *)

let to_opt (value : t option) = OptV value

let get_opt (value : t) =
  match value with OptV value -> value | _ -> failwith "get_opt"

(* Struct *)

let get_struct (value : t) =
  match value with StructV fields -> fields | _ -> failwith "get_struct"
