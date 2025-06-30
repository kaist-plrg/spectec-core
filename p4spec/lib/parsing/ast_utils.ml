open Il.Ast
open Xl.Atom
open Util.Source

let wrap_atom (s : string) : atom = Atom s $ no_region
let wrap_var_t (s : string) : typ' = VarT (s $ no_region, [])
let wrap_iter_t (i : iter) (t : typ') : typ' = IterT (t $ no_region, i)

let with_fresh_val (typ : typ') : vnote =
  let vid = Value.fresh () in
  { vid; typ }

let with_typ (typ : typ') (v : value') : value = v $$$ with_fresh_val typ

type symbol = NT of value | Term of string

let wrap_case_v (vs : symbol list) : value' =
  let rec build_mixop acc_mixop acc_terms = function
    | [] ->
        (* Always add the final group, even if empty *)
        acc_mixop @ [ acc_terms ]
    | Term s :: rest ->
        (* Accumulate terms *)
        build_mixop acc_mixop (acc_terms @ [ wrap_atom s ]) rest
    | NT _ :: rest ->
        (* When we hit a non-terminal, add accumulated terms to mixop and start new group *)
        let new_mixop = acc_mixop @ [ acc_terms ] in
        build_mixop new_mixop [] rest
  in
  let mixop = build_mixop [] [] vs in
  let values =
    vs
    |> List.filter (fun v -> match v with NT _ -> true | _ -> false)
    |> List.map (function NT v -> v | Term _ -> assert false)
  in
  CaseV (mixop, values)

let wrap_opt_v (v : value option) (s : string) : value =
  OptV v |> with_typ (wrap_iter_t Opt (wrap_var_t s))

let wrap_list_v (vs : value list) (s : string) : value =
  ListV vs |> with_typ (wrap_iter_t List (wrap_var_t s))

let id_of_case_v (v : value) : string =
  match (v.it, v.note.typ) with
  | CaseV _, VarT (id, _) -> id.it
  | _ -> failwith "not a case value"


type syntax' = string list list * value' list
type syntax = string list list * value list

let flatten_case_v' (value : value) : string * string list list * value' list =
  match value.it with
  | CaseV (mixop, values) ->
      let mixop = List.map (List.map (fun p -> string_of_atom p.it)) mixop in
      let values = List.map (fun v -> v.it) values in
      let id = id_of_case_v value in
      (id, mixop, values)
  | _ -> failwith "Expected a CaseV value"

let flatten_case_v (value : value) : string * string list list * value list =
  match value.it with
  | CaseV (mixop, values) ->
      let mixop = List.map (List.map (fun p -> string_of_atom p.it)) mixop in
      let id = id_of_case_v value in
      (id, mixop, values)
  | _ -> failwith "Expected a CaseV value"
