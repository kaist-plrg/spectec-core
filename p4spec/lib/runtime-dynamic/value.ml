open Xl
open Il.Ast
open Il.Print
open Util.Source

(* Value *)

type t = value

(* Stringifier *)

let to_string t = string_of_value t

(* Equality *)

let rec eq (value_l : t) (value_r : t) : bool =
  match (value_l.it, value_r.it) with
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
  match value.it with BoolV b -> b | _ -> failwith "get_bool"

(* Number *)

let get_num (value : t) =
  match value.it with NumV n -> n | _ -> failwith "get_num"

(* List *)

let seq (values : t list) = ListV values

let unseq (value : t) =
  match value.it with ListV values -> values | _ -> failwith "unseq"

(* Option *)

let opt (value : t option) = OptV value

let unopt (value : t) =
  match value.it with OptV value -> value | _ -> failwith "unopt"

(* Struct *)

let get_str (value : t) =
  match value.it with StructV fields -> fields | _ -> failwith "get_str"
