open Il.Ast
open Xl
open Util.Source
open El.Ast
open Ast_utils
open Pp_utils
module Num = Num
module F = Format

let verbose = ref false

let pp_atom fmt (atom : atom) : unit =
  match atom.it with
  | Atom.SilentAtom _ -> F.fprintf fmt ""
  | _ -> F.fprintf fmt "%s" (Atom.string_of_atom atom.it)

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
  | TextV _ -> pp_text_v fmt value
  | StructV _ -> failwith "not implemented"
  | CaseV _ -> pp_default_case_v fmt value
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

and pp_text_v fmt (value : value) : unit =
  match value.it with
  | TextV text -> F.fprintf fmt "%s" text
  | _ -> failwith "@pp_text_v: expected TextV value"

and pp_syntax_id fmt (value : value) : unit =
  match flatten_case_v value with
  | "identifier", [ [ "`ID" ]; [] ], [ value_text ] -> pp_text_v fmt value_text
  | "identifier", _, _ ->
      failwith
        (F.asprintf "@pp_syntax_id: ill-formed identifier:\n%a"
           pp_default_case_v value)
  | _ ->
      failwith
        (Printf.sprintf "@pp_syntax_id: expected identifier, got %s"
           (id_of_case_v value))

and pp_syntax_tid fmt (value : value) : unit =
  match flatten_case_v value with
  | "typeIdentifier", [ [ "`TID" ]; [] ], [ value_text ] ->
      pp_text_v fmt value_text
  | "typeIdentifier", _, _ ->
      failwith
        (F.asprintf "@pp_syntax_tid: ill-formed typeIdentifier:\n%a"
           pp_default_case_v value)
  | _ ->
      failwith
        (Printf.sprintf "@pp_syntax_tid: expected typeIdentifier, got %s"
           (id_of_case_v value))

and pp_syntax_name fmt (value : value) : unit =
  match flatten_case_v value with
  | "nonTypeName", [ [ "APPLY" ] ], [] -> F.fprintf fmt "apply"
  | "nonTypeName", [ [ "KEY" ] ], [] -> F.fprintf fmt "key"
  | "nonTypeName", [ [ "ACTIONS" ] ], [] -> F.fprintf fmt "actions"
  | "nonTypeName", [ [ "STATE" ] ], [] -> F.fprintf fmt "state"
  | "nonTypeName", [ [ "ENTRIES" ] ], [] -> F.fprintf fmt "entries"
  | "nonTypeName", [ [ "TYPE" ] ], [] -> F.fprintf fmt "type"
  | "nonTypeName", [ [ "PRIORITY" ] ], [] -> F.fprintf fmt "priority"
  | "name", [ [ "LIST" ] ], [] -> F.fprintf fmt "list"
  | _ ->
      failwith
        (Printf.sprintf "@pp_syntax_name: expected name, got %s"
           (id_of_case_v value))

and pp_syntax_mthd fmt (value : value) : unit =
  match flatten_case_v value with
  | "methodPrototype", [ []; []; [ ";" ] ], [ _anno; _func ]
  | "methodPrototype", [ []; [ "ABSTRACT" ]; [ ";" ] ], [ _anno; _func ]
  | "methodPrototype", [ []; []; [ "(" ]; [ ")"; ";" ] ], [ _anno; _; _func ] ->
      pp_default_case_v fmt value
  | "methodPrototype", _, _ ->
      failwith
        (F.asprintf "@pp_syntax_method: ill-formed methodPrototype:\n%a"
           pp_default_case_v value)
  | _ ->
      failwith
        (Printf.sprintf "@pp_syntax_method: expected methodPrototype, got %s"
           (id_of_case_v value))

and pp_syntax_block fmt (value : value) : unit =
  match flatten_case_v value with
  | "blockStatement", [ []; [ "{" ]; [ "}" ] ], [ _; _ ] ->
      F.fprintf fmt "%a" pp_default_case_v value
  | "blockStatement", _, _ ->
      failwith
        (F.asprintf "@pp_syntax_block: ill-formed blockStatement:\n%a"
           pp_default_case_v value)
  | _ ->
      failwith
        (Printf.sprintf "@pp_syntax_block: expected block, got %s"
           (id_of_case_v value))

and pp_syntax_decls ~level fmt (value : value) : unit =
  match value.it with
  | ListV values ->
      pp_list ~level (pp_syntax_decl ~level) ~sep:SemicolonNl fmt values
  | _ ->
      failwith
        (F.asprintf "@pp_syntax_decls: expected ListV, got %a" pp_value value)

and pp_syntax_decl ~level fmt (value : value) : unit =
  match flatten_case_v value with
  | "constantDeclaration", _, _
  | "errorDeclaration", _, _
  | "matchKindDeclaration", _, _
  | ( "externDeclaration",
      [ []; [ "EXTERN" ]; []; [ "{" ]; [ "}" ] ],
      [ _; _; _; _ ] )
  | "externDeclaration", _, _
  | "instantiation", _, _
  | "functionDeclaration", [ []; []; []; [] ], [ _; _; _ ]
  | "actionDeclaration", _, _
  | "parserDeclaration", _, _ ->
      pp_default_case_v fmt value
  | ( "controlDeclaration",
      [ []; []; [ "{" ]; [ "APPLY" ]; [ "}" ] ],
      [ control_type_decl; opt_constructor_params; control_local_decls; body ] )
    ->
      F.fprintf fmt "%a%a {\n%a\n%sapply %a\n%s}" pp_default_case_v
        control_type_decl pp_value opt_constructor_params
        (pp_syntax_decls ~level:(level + 1))
        control_local_decls
        (indent (level + 1))
        pp_syntax_block body (indent level)
  | ( "headerTypeDeclaration",
      [ []; [ "HEADER" ]; []; [ "{" ]; [ "}" ] ],
      [ _; _; _; _ ] )
  | ( "headerUnionDeclaration",
      [ []; [ "HEADER_UNION" ]; []; [ "{" ]; [ "}" ] ],
      [ _; _; _; _ ] )
  | ( "structTypeDeclaration",
      [ []; [ "STRUCT" ]; []; [ "{" ]; [ "}" ] ],
      [ _; _; _; _ ] )
  | "enumDeclaration", _, _
  | "typeDeclaration", [ []; [ ";" ] ], [ _ ]
  | "typeDeclaration", [ []; [ ";"; "PHTM_13" ] ], [ _ ]
  | "typeDeclaration", [ []; [ ";"; "PHTM_14" ] ], [ _ ]
  | "typeDeclaration", [ []; [ ";"; "PHTM_15" ] ], [ _ ]
  | "tableDeclaration", _, _ ->
      pp_default_case_v fmt value
  | _ ->
      failwith
        (Printf.sprintf "@pp_syntax_decl: expected declaration, got %s"
           (id_of_case_v value))

and pp_case_v' fmt (value : value) : unit =
  match flatten_case_v value with
  | "number", [ []; [ "PHTM_1" ] ], [ value_int ] ->
      F.fprintf fmt "%a" pp_value value_int
  | "stringLiteral", [ []; [ "PHTM_2" ] ], [ value_text ] ->
      pp_value fmt value_text
  | "direction", [ [ "NONE" ] ], [] -> F.fprintf fmt ""
  | "baseType", [ [ "INT"; "<" ]; [ ">" ] ], [ value_int ] ->
      F.fprintf fmt "%a" pp_value value_int
  | _ -> pp_default_case_v fmt value

and pp_case_v fmt (value : value) : unit =
  match id_of_case_v value with
  | "constantDeclaration" | "errorDeclaration" | "matchKindDeclaration"
  | "externDeclaration" | "instantiation" | "functionDeclaration"
  | "actionDeclaration" | "parserDeclaration" | "controlDeclaration"
  | "headerTypeDeclaration" | "headerUnionDeclaration" | "structTypeDeclaration"
  | "enumDeclaration" | "typeDeclaration" ->
      pp_syntax_decl ~level:0 fmt value
  | "nonTypeName" | "name" -> pp_syntax_name fmt value
  | "typeIdentifier" -> pp_syntax_tid fmt value
  | "identifier" -> pp_syntax_id fmt value
  | _ -> pp_case_v' fmt value
