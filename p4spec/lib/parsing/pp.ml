open Il.Ast
open Xl.Atom
open Util.Source
open El.Ast
open Ast_utils
module F = Format

let pp_atoms fmt atoms =
  match atoms with
  | [] -> F.fprintf fmt ""
  | _ ->
    let atoms = atoms |> List.map string_of_atom |> List.map String.lowercase_ascii
    in
    F.fprintf fmt "%s" (String.concat " " atoms);

let pp_default_case_v fmt value =
  match value.it with
  | CaseV (_mixop, _values) ->
    failwith "wtf"
    (* let len = List.length mixop + List.length values in *)
    (* List.init len (fun idx -> *)
    (*   if idx mod 2 = 0 then idx / 2 |> List.nth mixop |> pp_atoms fmt *)
    (*   else idx / 2 |> List.nth values |> pp_value fmt) *)
    (* |> List.filter_map (fun str -> if str = "" then None else Some str) *)
    (* |> String.concat " " *)
    (* |> F.fprintf fmt "%s" *)
  | _ -> failwith "Expected CaseV value"

(* let rec pp_value fmt value = *)
(*   match value.it with *)
(*   | BoolV b -> F.fprintf fmt "%b" b *)
(*   | NumV n -> F.fprintf fmt "%s" (Num.string_of_num n) *)
(*   | TextV t -> F.fprintf fmt "%S" t *)
(*   | StructV _ -> failwith "not implemented" *)
(*   | CaseV _ -> pp_case_v fmt value *)
(*   | TupleV values -> *)
(*       F.fprintf fmt "(%s)" *)
(*         (String.concat ", " (List.map (fun v -> Num.string_of_num v.it) values)) *)
(**)
(* and pp_trailing_comma fmt value = *)
(*   match flatten_case_v value with *)
(*   | "trailingComma", [ [","; "PHTM_0"] ], [] -> *)
(*     F.fprintf fmt "," *)

(* and pp_case_v fmt value = *)
(*   match flatten_case_v value with *)
(*   | "const", [ ["CONST"] ], [] -> *)
(*     F.fprintf fmt "const" *)
(*   | "number", [ []; ["PHTM_1"] ], [ value_int ] -> *)
(*     pp_value fmt value_int *)
(*   | "number", [ []; ["W"]; [] ], [ value_nat, value_int ] -> *)
(*     F.fprintf fmt "%sw%s" *)
(*       (pp_value fmt value_nat) *)
(*       (pp_value fmt value_int) *)
(*   | "number", [ []; ["S"]; [] ], [ value_nat, value_int ] -> *)
(*     F.fprintf fmt "%ss%s" *)
(*       (pp_value fmt value_nat) *)
(*       (pp_value fmt value_int) *)
(*   | "stringLiteral", [ []; ["PHTM_2"] ], [ value_text ] -> *)
(*     pp_value fmt value_text *)
(*   | "dotPrefix", [ ["."] ], [] -> *)
(*     F.fprintf fmt "." *)
(*   | "identifier", [ ["$"]; [] ], [ value_text ] -> *)
(*     pp_value fmt value_text *)
(*   | "typeIdentifier", [ ["@"]; [] ], [ value_text ] -> *)
(*     pp_value fmt value_text *)
(*   | "nonTypeName", [ ["APPLY"] ], [ ] -> *)
(*     F.fprintf fmt "apply" *)
(*   | "nonTypeName", [ ["KEY"] ], [ ] -> *)
(*     F.fprintf fmt "key" *)
(*   | "nonTypeName", [ ["ACTIONS"] ], [ ] -> *)
(*     F.fprintf fmt "actions" *)
(*   | "nonTypeName", [ ["STATE"] ], [ ] -> *)
(*     F.fprintf fmt "state" *)
(*   | "nonTypeName", [ ["ENTRIES"] ], [ ] -> *)
(*     F.fprintf fmt "entries" *)
(*   | "nonTypeName", [ ["TYPE"] ], [ ] -> *)
(*     F.fprintf fmt "type" *)
(*   | "nonTypeName", [ ["PRIORITY"] ], [ ] -> *)
(*     F.fprintf fmt "priority" *)
(*   | "prefixedNonTypeName", [ []; []; [] ], [ dotPrefix; nonTypeName ] -> *)
(*     F.fprintf fmt "%s%s" (pp_value fmt dotPrefix) (pp_value fmt nonTypeName) *)
(*   | "prefixedType", [ []; []; [] ], [ dotPrefix; typeIdentifier ] -> *)
(*     F.fprintf fmt "%s%s" (pp_value fmt dotPrefix) (pp_value fmt typeIdentifier) *)
(*   | "name", ["LIST"], [] -> *)
(*     F.fprintf fmt "list" *)
(*   | "direction", ["IN"], [] -> *)
(*     F.fprintf fmt "in" *)
(*   | "direction", ["OUT"], [] -> *)
(*     F.fprintf fmt "out" *)
(*   | "direction", ["INOUT"], [] -> *)
(*     F.fprintf fmt "inout" *)
(*   | "direction", ["NONE"], [] -> *)
(*     F.fprintf fmt "" *)
(*   | "baseType", ["BOOL"], [] -> *)
(*     F.fprintf fmt "bool" *)
(*   | "baseType", ["MATCH_KIND"], [] -> *)
(*     F.fprintf fmt "match_kind" *)
(*   | "baseType", ["ERROR"], [] -> *)
(*     F.fprintf fmt "error" *)
(*   | "baseType", ["BIT"], [] -> *)
(*     F.fprintf fmt "bit" *)
(*   | "baseType", ["STRING"], [] -> *)
(*     F.fprintf fmt "string" *)
(*   | _ -> failwith "TODO" *)
