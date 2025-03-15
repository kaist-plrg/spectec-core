open Xl
open Util.Print

(* Value *)

type t =
  | BoolV of bool
  | NumV of Num.t
  | TextV of string
  | StrV of (Atom.t * t) list
  | CaseV of Atom.t list list * t list
  | TupleV of t list
  | OptV of t option
  | ListV of t list

(* (TODO) I should surely do something with this mess *)

let rec to_string_with_indent ?(level = 0) (value : t) =
  match value with
  | BoolV b -> string_of_bool b
  | NumV n -> Num.string_of_num n
  | TextV s -> "\"" ^ s ^ "\""
  | StrV [] -> "{}"
  | StrV fields ->
      "{\n"
      ^ String.concat ";\n"
          (List.map
             (fun (atom, value) ->
               indent (level + 1)
               ^ Atom.string_of_atom atom ^ " "
               ^ to_string_with_indent ~level:(level + 2) value)
             fields)
      ^ " }"
  | CaseV (mixop, values) ->
      let atoms_h, mixop_t = (List.hd mixop, List.tl mixop) in
      let to_string_atoms atoms =
        match atoms with
        | [] -> ""
        | _ -> "`" ^ String.concat "" (List.map Atom.string_of_atom atoms) ^ "`"
      in
      "(" ^ to_string_atoms atoms_h
      ^ String.concat ""
          (List.map2
             (fun value atoms ->
               to_string_with_indent ~level:(level + 1) value
               ^ to_string_atoms atoms)
             values mixop_t)
      ^ ")"
  | TupleV values ->
      "("
      ^ String.concat ", " (to_strings_with_indent ~level:(level + 1) values)
      ^ ")"
  | OptV (Some value) ->
      "Some(" ^ to_string_with_indent ~level:(level + 1) value ^ ")"
  | OptV None -> "None"
  | ListV [] -> "[]"
  | ListV values ->
      "[ "
      ^ String.concat ",\n"
          (List.mapi
             (fun idx value ->
               let indent = if idx = 0 then "" else indent (level + 1) in
               indent ^ to_string_with_indent ~level:(level + 2) value)
             values)
      ^ " ]"

and to_strings_with_indent ?(level = 0) (values : t list) =
  List.map (to_string_with_indent ~level) values

let to_string (value : t) = to_string_with_indent value

(* Equality *)

let rec eq (value_l : t) (value_r : t) : bool =
  match (value_l, value_r) with
  | BoolV b_l, BoolV b_r -> b_l = b_r
  | NumV n_l, NumV n_r -> Num.eq n_l n_r
  | TextV s_l, TextV s_r -> s_l = s_r
  | CaseV (mixop_l, values_l), CaseV (mixop_r, values_r) ->
      List.compare (List.compare Atom.compare) mixop_l mixop_r = 0
      && eqs values_l values_r
  | ListV values_l, ListV values_r -> eqs values_l values_r
  | OptV value_opt_l, OptV value_opt_r -> (
      match (value_opt_l, value_opt_r) with
      | Some value_l, Some value_r -> eq value_l value_r
      | None, None -> true
      | _ -> false)
  | _ -> failwith "TODO"

and eqs (values_l : t list) (values_r : t list) : bool =
  List.length values_l = List.length values_r
  && List.for_all2 eq values_l values_r

(* Boolean *)

let get_bool (value : t) =
  match value with BoolV b -> b | _ -> failwith "get_bool"

(* List *)

let seq (values : t list) = ListV values

let unseq (value : t) =
  match value with ListV values -> values | _ -> failwith "unseq"

(* Option *)

let opt (value : t option) = OptV value

let unopt (value : t) =
  match value with OptV value -> value | _ -> failwith "unopt"

(* Struct *)

let get_str (value : t) =
  match value with StrV fields -> fields | _ -> failwith "get_str"
