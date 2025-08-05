open Il.Ast
open Il.Eq
open Xl
open Util.Source
open Ast_utils
open Hint
module Num = Num
module F = Format

(**** SpecTec IL ****)

(* Numbers *)

let pp_num fmt (num : num) : unit =
  match num with
  | `Nat n -> F.fprintf fmt "%s" (Bigint.to_string n)
  | `Int i ->
      F.fprintf fmt "%s"
        ((if i >= Bigint.zero then "" else "-")
        ^ Bigint.to_string (Bigint.abs i))

(* Atoms *)

let pp_atom fmt (atom : atom) : unit =
  match atom.it with
  | Atom.SilentAtom _ -> F.fprintf fmt ""
  | _ ->
      F.fprintf fmt "%s" (Atom.string_of_atom atom.it |> String.lowercase_ascii)

let pp_atoms fmt (atoms : atom list) : unit =
  match atoms with
  | [] -> F.fprintf fmt ""
  | _ ->
      let atoms =
        atoms
        |> List.map (fun atom -> F.asprintf "%a" pp_atom atom)
        |> List.filter (fun str -> str <> String.empty)
      in
      F.fprintf fmt "%s" (String.concat " " atoms)

(* Values *)

let rec pp_value (hmap : hmap) fmt (value : value) : unit =
  match value.it with
  | BoolV b -> F.fprintf fmt "%b" b
  | NumV n -> F.fprintf fmt "%a" pp_num n
  | TextV _ -> pp_text_v fmt value
  | StructV _ -> failwith "@pp_value: StructV not implemented"
  | CaseV _ -> pp_case_v hmap fmt value
  | TupleV values ->
      F.fprintf fmt "(%s)"
        (String.concat ", "
           (List.map (fun v -> F.asprintf "%a" (pp_value hmap) v) values))
  | OptV _ -> pp_opt_v hmap fmt value
  | ListV _ -> pp_list_v hmap fmt value
  | _ -> failwith "@pp_value: TODO"

(* TextV *)

and pp_text_v fmt (value : value) : unit =
  match value.it with
  | TextV text -> F.fprintf fmt "%s" (String.escaped text)
  | _ -> failwith "@pp_text_v: expected TextV value"

(* CaseV *)

and pp_case_v (hmap : hmap) fmt (value : value) : unit =
  let id, _, values = flatten_case_v value in
  let matches_hint nottyp value =
    match value.it with
    | CaseV (mixop, _) -> eq_mixop (fst nottyp.it) mixop
    | _ -> false
  in
  let find_hint id value =
    match SMap.find_opt id hmap with
    | None -> None
    | Some typs ->
        List.find_opt (fun (nottyp, _) -> matches_hint nottyp value) typs
        |> Option.map snd
  in
  match find_hint id value with
  | Some hintexp -> pp_hint_case_v hmap hintexp fmt values
  | None -> pp_default_case_v hmap fmt value

and pp_hint_case_v (hmap : hmap) (exp : El.Ast.exp) fmt (values : value list) :
    unit =
  let _, str = pp_hint_case_v' hmap 0 exp values in
  F.fprintf fmt "%s" str

and pp_hint_case_v' (hmap : hmap) (cur : int) (exp : El.Ast.exp)
    (values : value list) : int * string =
  match exp.it with
  | TextE text -> (cur, text)
  | AtomE atom -> (cur, F.asprintf "%a" pp_atom atom)
  | SeqE exps ->
      let cur, strs =
        List.fold_left
          (fun (cur, l) exp ->
            let cur, str = pp_hint_case_v' hmap cur exp values in
            (cur, l @ [ str ]))
          (cur, []) exps
      in
      (cur, String.concat " " strs)
  | BrackE (atom_l, exp, atom_r) ->
      let cur, exp_str = pp_hint_case_v' hmap cur exp values in
      let strs =
        [
          F.asprintf "%a" pp_atom atom_l;
          exp_str;
          F.asprintf "%a" pp_atom atom_r;
        ]
      in
      let strs = List.filter (fun s -> s <> String.empty) strs in
      (cur, String.concat " " strs)
  | HoleE (`Num i) -> (i, F.asprintf "%a" (pp_value hmap) (List.nth values i))
  | HoleE `Next ->
      (cur + 1, F.asprintf "%a" (pp_value hmap) (List.nth values cur))
  | FuseE (exp_l, exp_r) ->
      let cur_l, str_l = pp_hint_case_v' hmap cur exp_l values in
      let cur_r, str_r = pp_hint_case_v' hmap cur_l exp_r values in
      (cur_r, str_l ^ str_r)
  | _ -> (cur, El.Print.string_of_exp exp)

and pp_default_case_v (hmap : hmap) fmt (value : value) : unit =
  match value.it with
  | CaseV (mixop, values) ->
      let len = List.length mixop + List.length values in
      List.init len (fun idx ->
          if idx mod 2 = 0 then
            idx / 2 |> List.nth mixop |> F.asprintf "%a" pp_atoms
          else idx / 2 |> List.nth values |> F.asprintf "%a" (pp_value hmap))
      |> List.filter (fun str -> str <> "")
      |> String.concat " " |> F.fprintf fmt "%s"
  | _ -> failwith "@pp_default_case_v: Expected CaseV value"

(* OptV *)

and pp_opt_v (hmap : hmap) fmt (value : value) : unit =
  match value.it with
  | OptV (Some v) -> F.fprintf fmt "%a" (pp_value hmap) v
  | OptV None -> ()
  | _ -> failwith "@pp_opt_v: expected OptV value"

(* ListV *)

and pp_list_v (hmap : hmap) fmt (value : value) : unit =
  let values =
    match value.it with
    | ListV values -> values
    | _ ->
        failwith
          (F.asprintf "@pp_list_v: expected ListV, got %a" (pp_value hmap) value)
  in
  let ss = List.map (F.asprintf "%a" (pp_value hmap)) values in
  F.fprintf fmt "%s" (String.concat " " ss)

(* P4 program *)

let pp_program (spec : spec) fmt (value : value) : unit =
  let hmap = hints_of_spec spec in
  pp_value hmap fmt value
