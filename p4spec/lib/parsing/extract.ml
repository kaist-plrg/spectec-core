open Il.Ast
open Ast_utils
module F = Format

(* name = nonTypeName | LIST | typeIdentifier *)
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

(******** Name of declarations ********)

let id_of_function_prototype (v : value) : string =
  match flatten_case_v v with
  | "functionPrototype",
    [ []; []; []; ["("]; [")"] ],
    [ _; name; _; _ ] ->
      id_of_name name
  | _ ->
      failwith
        (Printf.sprintf "Invalid functionPrototype: %s"
           (Il.Print_debug.string_of_value v))

let id_of_typedef_declaration (v : value) : string =
  match flatten_case_v v with
  | "typedefDeclaration",
    [ []; ["TYPEDEF"]; []; [] ],
    [ _; _; name]
  | "typedefDeclaration",
    [ []; ["TYPEDEF"]; []; ["PHTM_12"] ],
    [ _; _; name]
  | "typedefDeclaration",
    [ []; ["TYPE"]; []; [] ],
    [ _; _; name] ->
      id_of_name name
  | _ ->
      failwith
        (Printf.sprintf "Invalid typedefDeclaration: %s"
           (Il.Print_debug.string_of_value v))

let id_of_control_type_declaration (v : value) : string =
  match flatten_case_v v with
  | "controlTypeDeclaration",
    [ []; ["CONTROL"]; []; ["("]; [")"] ],
    [ _; name; _; _ ] ->
      id_of_name name
  | _ -> failwith
        (Printf.sprintf "Invalid controlTypeDeclaration: %s"
           (Il.Print_debug.string_of_value v))

let id_of_parser_type_declaration (v : value) : string =
  match flatten_case_v v with
  | "parserTypeDeclaration",
    [ []; ["PARSER"]; []; ["("]; [")"] ],
    [ _; name; _; _ ] ->
      id_of_name name
  | _ ->
      failwith
        (Printf.sprintf "Invalid parserTypeDeclaration: %s"
           (Il.Print_debug.string_of_value v))

let id_of_package_type_declaration (v : value) : string =
  match flatten_case_v v with
  | "packageTypeDeclaration",
    [ []; ["PACKAGE"]; []; ["("]; [")"] ],
    [ _; name; _; _ ] ->
      id_of_name name
  | _ ->
      failwith
        (Printf.sprintf "Invalid packageTypeDeclaration: %s"
           (Il.Print_debug.string_of_value v))

let id_of_declaration (decl : value) : string =
  match flatten_case_v decl with
  | "constantDeclaration", [ []; ["CONST"]; []; []; [";"] ], [ _; _; name; _] ->
    id_of_name name
  | "errorDeclaration", _, _ -> failwith "errorDeclaration: no name"
  | "matchKindDeclaration", _, _ -> failwith "matchKindDeclaration: no name"
  | "externDeclaration", [ []; ["EXTERN"]; []; ["{"]; ["}"] ], [ _; nonTypeName; _; _ ] ->
    id_of_name nonTypeName
  | "externDeclaration", [ []; ["EXTERN"]; [";"] ], [ _; functionPrototype ] ->
    id_of_function_prototype functionPrototype
  | "instantiation", [ []; []; ["("]; [")"]; [";"] ], [ _; _; _; name] ->
    id_of_name name
  | "instantiation", [ []; []; ["("]; [")"]; []; [";"] ], [ _; _; _; name; _ ] ->
    id_of_name name
  | "functionDeclaration", [ []; []; []; [] ], [ _; functionPrototype; _ ] ->
    id_of_function_prototype functionPrototype
  | "actionDeclaration", [ []; ["ACTION"]; ["("]; [")"]; [] ], [ _; name; _; _ ] ->
    id_of_name name
  | "parserDeclaration", [ []; []; ["{"]; []; ["}"] ], [ parserTypeDeclaration; _; _; _ ] ->
    id_of_parser_type_declaration parserTypeDeclaration
  | "controlDeclaration", [ []; []; ["{"]; ["APPLY"]; ["}"] ], [ controlTypeDeclaration; _; _; _ ] ->
    id_of_control_type_declaration controlTypeDeclaration
  | "headerTypeDeclaration", [ []; ["HEADER"]; []; ["{"]; ["}"] ], [ _; name; _; _ ] ->
    id_of_name name
  | "headerUnionDeclaration", [ []; ["HEADER_UNION"]; []; ["{"]; ["}"] ], [ _; name; _; _ ] ->
    id_of_name name
  | "structTypeDeclaration", [ []; ["STRUCT"]; []; ["{"]; ["}"] ], [ _; name; _; _ ] ->
    id_of_name name
  | "enumDeclaration", [ []; ["ENUM"]; ["{"]; []; ["}"] ], [ _; name; _; _ ] ->
    id_of_name name
  | "enumDeclaration", [ []; ["ENUM"]; []; ["{"]; []; ["}"] ], [ _; _; name; _; _ ] ->
    id_of_name name
  | "typeDeclaration", [ []; [";"] ], [ typedefDeclaration ] ->
    id_of_typedef_declaration typedefDeclaration
  | "typeDeclaration", [ []; [";"; "PHTM_13"] ], [ parserTypeDeclaration ] ->
    id_of_parser_type_declaration parserTypeDeclaration
  | "typeDeclaration", [ []; [";"; "PHTM_14"] ], [ controlTypeDeclaration ] ->
    id_of_control_type_declaration controlTypeDeclaration
  | "typeDeclaration", [ []; [";"; "PHTM_15"] ], [ packageTypeDeclaration ] ->
    id_of_package_type_declaration packageTypeDeclaration
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
  | _ -> failwith "invalid parameter structure"

let has_typ_params_parser_type_declaration (_v : value') : bool = false
(* match flatten_case_v' parserTypeDeclaration with *)
(* | "parserTypeDeclaration", [ []; ["PHTM_13"] ], _ -> true *)
(* | _ -> false *)

let has_typ_params_control_type_declaration (_v : value') : bool = false
let has_typ_params_package_type_declaration (_v : value') : bool = false
let has_typ_params_declaration (_value_decl : value) : bool = false
(* match flatten_case_v' value_decl with *)
(* | "constantDeclaration", _, _ *)
(* | "errorDeclaration", _, _ *)
(* | "matchKindDeclaration", _, _ -> false *)
(* | "externDeclaration", [ []; ["EXTERN"]; []; ["{"]; ["}"] ], [ _; _; _optTypeParameters; _] -> failwith "TODO" *)
(* | "externDeclaration", _, _ -> false *)
(* | "instantiation", _, _ -> false *)
(* | "functionDeclaration", _, _ -> failwith "TODO" *)
(* | "actionDeclaration", _, _ -> false *)
(* | "parserDeclaration", _, _ -> false *)
(* | "controlDeclaration", _, _ -> false *)
(* | "headerTypeDeclaration", _, _ *)
(* | "headerUnionDeclaration", _, _ *)
(* | "structTypeDeclaration", _, _ *)
(*   ->  failwith "TODO" *)
(* | "enumDeclaration", _, _ -> false *)
(* | "typeDeclaration", [ []; [";"] ], [ _typedefDeclaration ] -> false *)
(* | "typeDeclaration", [ []; [";"; "PHTM_13"] ], [ parserTypeDeclaration ] -> *)
(*   has_typ_params_parser_type_declaration parserTypeDeclaration *)
(* | "typeDeclaration", [ []; [";"; "PHTM_14"] ], [ controlTypeDeclaration ] -> *)
(*   has_typ_params_control_type_declaration controlTypeDeclaration *)
(* | "typeDeclaration", [ []; [";"; "PHTM_15"] ], [ packageTypeDeclaration ] -> *)
(*   has_typ_params_package_type_declaration packageTypeDeclaration *)
(* | _ -> failwith (Printf.sprintf "@has_typ_params: Unknown declaration %s" (id_of_case_v value_decl)) *)
