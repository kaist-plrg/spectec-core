open Il.Ast
open Xl
open Util.Source
open El.Ast
open Ast_utils
module Num = Num
module F = Format

let verbose = ref false

let pp_atom fmt (atom : atom) : unit =
  F.fprintf fmt "%s" (Atom.string_of_atom atom.it)

let pp_atoms fmt (atoms : atom list) : unit =
  match atoms with
  | [] -> F.fprintf fmt ""
  | _ ->
      let atoms =
        atoms
        |> List.map (fun atom -> F.asprintf "%a" pp_atom atom)
        |> List.map String.lowercase_ascii
      in
      F.fprintf fmt "%s" (String.concat "" atoms)

let rec pp_default_case_v fmt value : unit =
  match value.it with
  | CaseV (mixop, values) ->
      let len = List.length mixop + List.length values in
      List.init len (fun idx ->
          if idx mod 2 = 0 then
            idx / 2 |> List.nth mixop |> F.asprintf "%a" pp_atoms
          else idx / 2 |> List.nth values |> F.asprintf "%a" pp_value)
      |> List.filter (fun str -> str <> "")
      |> String.concat " "
      |>
      if !verbose then F.fprintf fmt "@%s_< %s>_@" (id_of_case_v value)
      else F.fprintf fmt "%s"
  | _ -> failwith "@pp_default_case_v: Expected CaseV value"

and pp_num fmt (num : num) : unit =
  match num with
  | `Nat n -> F.fprintf fmt "%s" (Bigint.to_string n)
  | `Int i ->
      F.fprintf fmt "%s"
        ((if i >= Bigint.zero then "" else "-")
        ^ Bigint.to_string (Bigint.abs i))

and pp_value fmt (value : value) : unit =
  match value.it with
  | BoolV b -> F.fprintf fmt "%b" b
  | NumV n -> F.fprintf fmt "%a" pp_num n
  | TextV t -> F.fprintf fmt "%s" t
  | StructV _ -> failwith "not implemented"
  | CaseV _ -> pp_case_v fmt value
  | TupleV values ->
      F.fprintf fmt "(%s)"
        (String.concat ", "
           (List.map (fun v -> F.asprintf "%a" pp_value v) values))
  | OptV (Some v) -> F.fprintf fmt "%a" pp_value v
  | OptV None -> F.fprintf fmt ""
  | ListV [] -> F.fprintf fmt ""
  | ListV values ->
      F.fprintf fmt "%s"
        (String.concat "; "
           (List.map (fun v -> F.asprintf "%a" pp_value v) values))
  | _ -> failwith "@pp_value: TODO"

and pp_case_v fmt (value : value) : unit =
  match flatten_case_v value with
  | "number", [ []; [ "PHTM_1" ] ], [ value_int ] ->
      F.fprintf fmt "%a" pp_value value_int
  | "stringLiteral", [ []; [ "PHTM_2" ] ], [ value_text ] ->
      pp_value fmt value_text
  | "identifier", [ [ "$" ]; [] ], [ value_text ] -> pp_value fmt value_text
  | "typeIdentifier", [ [ "@" ]; [] ], [ value_text ] -> pp_value fmt value_text
  | "direction", [ [ "NONE" ] ], [] -> F.fprintf fmt ""
  | "baseType", [ [ "INT"; "<" ]; [ ">" ] ], [ value_int ] ->
      F.fprintf fmt "%a" pp_value value_int
  | _ -> pp_default_case_v fmt value
