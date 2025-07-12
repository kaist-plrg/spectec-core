(* 
 * Helper functions for context management
 *
 * - id_of : extracts identifiers from CaseV values
 * - has_type_params : checks for type parameters in CaseV values
 *)

open Il.Ast
open Ast_utils
open Util.Source
module F = Format

(* Identifier extraction *)

let id_of_name (value : value) : string =
  match flatten_case_v' value with
  | "identifier", [ [ "$" ]; [] ], [ TextV s ] -> s
  | "nonTypeName", [ [ "APPLY" ] ], [] -> "apply"
  | "nonTypeName", [ [ "KEY" ] ], [] -> "key"
  | "nonTypeName", [ [ "ACTIONS" ] ], [] -> "actions"
  | "nonTypeName", [ [ "STATE" ] ], [] -> "state"
  | "nonTypeName", [ [ "ENTRIES" ] ], [] -> "entries"
  | "nonTypeName", [ [ "TYPE" ] ], [] -> "type"
  | "nonTypeName", [ [ "PRIORITY" ] ], [] -> "priority"
  | "name", [ [ "LIST" ] ], [] -> "list"
  | "typeIdentifier", [ [ "@" ]; [] ], [ TextV s ] -> s
  | _ ->
      failwith
        (Printf.sprintf "Invalid name structure %s: %s "
           (Il.Print_debug.string_of_value value)
           (id_of_case_v value))

let id_of_function_prototype (v : value) : string =
  match flatten_case_v v with
  | "functionPrototype", [ []; []; []; [ "(" ]; [ ")" ] ], [ _; name; _; _ ] ->
      id_of_name name
  | _ ->
      failwith
        (Printf.sprintf "Invalid functionPrototype: %s"
           (Il.Print_debug.string_of_value v))

let id_of_typedef_declaration (v : value) : string =
  match flatten_case_v v with
  | "typedefDeclaration", [ []; [ "TYPEDEF" ]; []; [] ], [ _; _; name ]
  | "typedefDeclaration", [ []; [ "TYPEDEF" ]; [] ], [ _; _; name ]
  | "typedefDeclaration", [ []; [ "TYPE" ]; []; [] ], [ _; _; name ] ->
      id_of_name name
  | _ ->
      failwith
        (Printf.sprintf "Invalid typedefDeclaration: %s"
           (Il.Print_debug.string_of_value v))

let id_of_control_type_declaration (v : value) : string =
  match flatten_case_v v with
  | ( "controlTypeDeclaration",
      [ []; [ "CONTROL" ]; []; [ "(" ]; [ ")" ] ],
      [ _; name; _; _ ] ) ->
      id_of_name name
  | _ ->
      failwith
        (Printf.sprintf "Invalid controlTypeDeclaration: %s"
           (Il.Print_debug.string_of_value v))

let id_of_parser_type_declaration (v : value) : string =
  match flatten_case_v v with
  | ( "parserTypeDeclaration",
      [ []; [ "PARSER" ]; []; [ "(" ]; [ ")" ] ],
      [ _; name; _; _ ] ) ->
      id_of_name name
  | _ ->
      failwith
        (Printf.sprintf "Invalid parserTypeDeclaration: %s"
           (Il.Print_debug.string_of_value v))

let id_of_package_type_declaration (v : value) : string =
  match flatten_case_v v with
  | ( "packageTypeDeclaration",
      [ []; [ "PACKAGE" ]; []; [ "(" ]; [ ")" ] ],
      [ _; name; _; _ ] ) ->
      id_of_name name
  | _ ->
      failwith
        (Printf.sprintf "Invalid packageTypeDeclaration: %s"
           (Il.Print_debug.string_of_value v))

let id_of_declaration (decl : value) : string =
  match flatten_case_v decl with
  | ( "constantDeclaration",
      [ []; [ "CONST" ]; []; []; [ ";" ] ],
      [ _; _; name; _ ] ) ->
      id_of_name name
  | "errorDeclaration", _, _ -> failwith "errorDeclaration: no name"
  | "matchKindDeclaration", _, _ -> failwith "matchKindDeclaration: no name"
  | ( "externDeclaration",
      [ []; [ "EXTERN" ]; []; [ "{" ]; [ "}" ] ],
      [ _; nonTypeName; _; _ ] ) ->
      id_of_name nonTypeName
  | "externDeclaration", [ []; [ "EXTERN" ]; [ ";" ] ], [ _; functionPrototype ]
    ->
      id_of_function_prototype functionPrototype
  | "instantiation", [ []; []; [ "(" ]; [ ")" ]; [ ";" ] ], [ _; _; _; name ] ->
      id_of_name name
  | ( "instantiation",
      [ []; []; [ "(" ]; [ ")" ]; []; [ ";" ] ],
      [ _; _; _; name; _ ] ) ->
      id_of_name name
  | "functionDeclaration", [ []; []; []; [] ], [ _; functionPrototype; _ ] ->
      id_of_function_prototype functionPrototype
  | ( "actionDeclaration",
      [ []; [ "ACTION" ]; [ "(" ]; [ ")" ]; [] ],
      [ _; name; _; _ ] ) ->
      id_of_name name
  | ( "parserDeclaration",
      [ []; []; [ "{" ]; []; [ "}" ] ],
      [ parserTypeDeclaration; _; _; _ ] ) ->
      id_of_parser_type_declaration parserTypeDeclaration
  | ( "controlDeclaration",
      [ []; []; [ "{" ]; [ "APPLY" ]; [ "}" ] ],
      [ controlTypeDeclaration; _; _; _ ] ) ->
      id_of_control_type_declaration controlTypeDeclaration
  | ( "headerTypeDeclaration",
      [ []; [ "HEADER" ]; []; [ "{" ]; [ "}" ] ],
      [ _; name; _; _ ] ) ->
      id_of_name name
  | ( "headerUnionDeclaration",
      [ []; [ "HEADER_UNION" ]; []; [ "{" ]; [ "}" ] ],
      [ _; name; _; _ ] ) ->
      id_of_name name
  | ( "structTypeDeclaration",
      [ []; [ "STRUCT" ]; []; [ "{" ]; [ "}" ] ],
      [ _; name; _; _ ] ) ->
      id_of_name name
  | ( "enumDeclaration",
      [ []; [ "ENUM" ]; [ "{" ]; []; [ "}" ] ],
      [ _; name; _; _ ] ) ->
      id_of_name name
  | ( "enumDeclaration",
      [ []; [ "ENUM" ]; []; [ "{" ]; []; [ "}" ] ],
      [ _; _; name; _; _ ] ) ->
      id_of_name name
  | "typeDeclaration", [ []; [ ";" ] ], [ nonterminal ] -> (
      match flatten_case_v nonterminal with
      | "typedefDeclaration", _, _ -> id_of_typedef_declaration nonterminal
      | "parserTypeDeclaration", _, _ ->
          id_of_parser_type_declaration nonterminal
      | "controlTypeDeclaration", _, _ ->
          id_of_control_type_declaration nonterminal
      | "packageTypeDeclaration", _, _ ->
          id_of_package_type_declaration nonterminal
      | _ -> assert false)
  (* not a variant of declaration *)
  | "tableDeclaration", [ []; [ "TABLE" ]; [ "{" ]; [ "}" ] ], [ _; name; _ ] ->
      id_of_name name
  | _ ->
      failwith
        (Printf.sprintf "Invalid declaration structure: %s"
           (F.asprintf "%a" Pp.pp_default_case_v decl))

let id_of_parameter (v : value) : string =
  match flatten_case_v v with
  | "parameter", [ []; []; []; []; [] ], [ _; _; _; name ] -> id_of_name name
  | "parameter", [ []; []; []; []; []; [] ], [ _; _; _; name; _ ] ->
      id_of_name name
  | _ -> failwith "@id_of_parameter: invalid CaseV"

(* Type parameter extraction *)

let has_type_params_opt_type_parameters (v : value) : bool =
  match v.it with
  | OptV (Some typeParameters) -> (
      match flatten_case_v typeParameters with
      | "typeParameters", [ [ "<" ]; [ ">" ] ], [ typeParameterList ] -> (
          match typeParameterList.it with
          | ListV [] -> false
          | ListV _ -> true
          | _ -> failwith "@has_type_params_opt_type_parameters: not ListV")
      | _ -> failwith "@has_type_params_opt_type_parameters: invalid CaseV")
  | OptV None -> false
  | _ -> failwith "@has_type_params_opt_type_parameters: not OptV"

let has_type_params_function_prototype (v : value) : bool =
  match flatten_case_v v with
  | ( "functionPrototype",
      [ []; []; []; [ "(" ]; [ ")" ] ],
      [ _; _; optTypeParameters; _ ] ) ->
      has_type_params_opt_type_parameters optTypeParameters
  | _ ->
      failwith
        (Printf.sprintf "Invalid functionPrototype: %s"
           (Il.Print_debug.string_of_value v))

let has_type_params_parser_type_declaration (v : value) : bool =
  match flatten_case_v v with
  | ( "parserTypeDeclaration",
      [ []; [ "PARSER" ]; []; [ "(" ]; [ ")" ] ],
      [ _; _; optTypeParameters; _ ] ) ->
      has_type_params_opt_type_parameters optTypeParameters
  | _ ->
      failwith
        (Printf.sprintf "Invalid parserTypeDeclaration: %s"
           (Il.Print_debug.string_of_value v))

let has_type_params_control_type_declaration (v : value) : bool =
  match flatten_case_v v with
  | ( "controlTypeDeclaration",
      [ []; [ "CONTROL" ]; []; [ "(" ]; [ ")" ] ],
      [ _; _; optTypeParameters; _ ] ) ->
      has_type_params_opt_type_parameters optTypeParameters
  | _ ->
      failwith
        (Printf.sprintf "Invalid controlTypeDeclaration: %s"
           (Il.Print_debug.string_of_value v))

let has_type_params_package_type_declaration (v : value) : bool =
  match flatten_case_v v with
  | ( "packageTypeDeclaration",
      [ []; [ "PACKAGE" ]; []; [ "(" ]; [ ")" ] ],
      [ _; _; optTypeParameters; _ ] ) ->
      has_type_params_opt_type_parameters optTypeParameters
  | _ ->
      failwith
        (Printf.sprintf "Invalid packageTypeDeclaration: %s"
           (Il.Print_debug.string_of_value v))

let has_type_params_declaration (decl : value) : bool =
  match flatten_case_v decl with
  | "constantDeclaration", _, _
  | "errorDeclaration", _, _
  | "matchKindDeclaration", _, _ ->
      false
  | ( "externDeclaration",
      [ []; [ "EXTERN" ]; []; [ "{" ]; [ "}" ] ],
      [ _; _; optTypeParameters; _ ] ) ->
      has_type_params_opt_type_parameters optTypeParameters
  | "externDeclaration", _, _ -> false
  | "instantiation", _, _ -> false
  | "functionDeclaration", [ []; []; []; [] ], [ _; functionPrototype; _ ] ->
      has_type_params_function_prototype functionPrototype
  | "actionDeclaration", _, _ -> false
  | "parserDeclaration", _, _ -> false
  | "controlDeclaration", _, _ -> false
  | ( "headerTypeDeclaration",
      [ []; [ "HEADER" ]; []; [ "{" ]; [ "}" ] ],
      [ _; _; optTypeParameters; _ ] )
  | ( "headerUnionDeclaration",
      [ []; [ "HEADER_UNION" ]; []; [ "{" ]; [ "}" ] ],
      [ _; _; optTypeParameters; _ ] )
  | ( "structTypeDeclaration",
      [ []; [ "STRUCT" ]; []; [ "{" ]; [ "}" ] ],
      [ _; _; optTypeParameters; _ ] ) ->
      has_type_params_opt_type_parameters optTypeParameters
  | "enumDeclaration", _, _ -> false
  | "typeDeclaration", [ []; [ ";" ] ], [ _typedefDeclaration ] -> false
  | "typeDeclaration", [ []; [ ";"; "PHTM_13" ] ], [ parserTypeDeclaration ] ->
      has_type_params_parser_type_declaration parserTypeDeclaration
  | "typeDeclaration", [ []; [ ";"; "PHTM_14" ] ], [ controlTypeDeclaration ] ->
      has_type_params_control_type_declaration controlTypeDeclaration
  | "typeDeclaration", [ []; [ ";"; "PHTM_15" ] ], [ packageTypeDeclaration ] ->
      has_type_params_package_type_declaration packageTypeDeclaration
  | _ ->
      failwith
        (Printf.sprintf "@has_typ_params: Unknown declaration %s"
           (id_of_case_v decl))
