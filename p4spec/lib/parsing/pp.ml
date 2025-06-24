open Il.Ast
open Xl.Atom
open Util.Source
open El.Ast
open Traverse
module F = Format

let flatten_case_v (value: value) = 
  match value.it with
  | CaseV (mixop, values) ->
      let mixop = List.map 
          (List.map 
            (fun (p: Atom.t phrase) -> 
              let Atom v = p.it in
              v
            )
          )
          mixop
      in
      let values = List.map (fun v -> v.it) values in
      (id_of_case_v value) mixop, values
  | _ -> failwith "Expected a CaseV value"

let rec pp_value fmt value =
  match value.it with
  | BoolV b -> F.fprintf fmt "%b" b
  | NumV n -> F.fprintf fmt "%s" (Num.string_of_num n)
  | TextV t -> F.fprintf fmt "%S" t
  | StructV _ -> failwith "not implemented"
  | CaseV _ -> pp_case_v fmt value
  | TupleV values ->
      F.fprintf fmt "(%s)"
        (String.concat ", " (List.map (fun v -> Num.string_of_num v.it) values))

and pp_case_v fmt value =
  match flatten_case_v value with
  | "const", [ ["CONST"] ], [] ->
    F.fprintf fmt "const"
  | "number", [ [], ["PHTM_1"] ], [ value_int ] ->
    pp_value fmt value_int
  | "number", [ [], ["W"], [] ], [ value_nat, value_int ] ->
    F.fprintf fmt "%sw%s"
      (pp_value fmt value_nat)
      (pp_value fmt value_int)
  | "number", [ [], ["S"], [] ], [ value_nat, value_int ] ->
    F.fprintf fmt "%ss%s"
      (pp_value fmt value_nat)
      (pp_value fmt value_int)
  | "stringLiteral", [ [], ["PHTM_2"] ], [ value_text ] ->
    pp_value fmt value_text
  | "dotPrefix", [ ["."] ], [] ->
    F.fprintf fmt "."
  | "identifier", [ ["$"], [] ], [ value_text ] ->
    pp_value fmt value_text
  | "typeIdentifier", [ ["@"], [] ], [ value_text ] ->
    pp_value fmt value_text
  | "nonTypeName", [ ["$"], [] ], [ value_text ] ->
    pp_value fmt value_text
  | _ -> failwith "TODO"
