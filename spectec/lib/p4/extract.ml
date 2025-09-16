(*
 * Helper functions for context management
 *
 * - id_of : extracts identifiers from CaseV values
 * - has_type_params : checks for type parameters in CaseV values
 *)

open Il.Ast
open Concrete.Ast_utils
module F = Format

(* Identifier extraction *)

let id_of_name (value : value) : string =
  match flatten_case_v' value with
  | "identifier", [ [ "`ID" ]; [] ], [ TextV s ] -> s
  | "nonTypeName", [ [ "APPLY" ] ], [] -> "apply"
  | "nonTypeName", [ [ "KEY" ] ], [] -> "key"
  | "nonTypeName", [ [ "ACTIONS" ] ], [] -> "actions"
  | "nonTypeName", [ [ "STATE" ] ], [] -> "state"
  | "nonTypeName", [ [ "ENTRIES" ] ], [] -> "entries"
  | "nonTypeName", [ [ "TYPE" ] ], [] -> "type"
  | "nonTypeName", [ [ "PRIORITY" ] ], [] -> "priority"
  | "name", [ [ "LIST" ] ], [] -> "list"
  | "typeIdentifier", [ [ "`TID" ]; [] ], [ TextV s ] -> s
  | _ ->
      failwith
        (Printf.sprintf "Invalid name structure %s: %s "
           (Il.Print.string_of_value value)
           (id_of_case_v value))

let id_of_function_prototype (v : value) : string =
  match flatten_case_v v with
  | "functionPrototype", [ []; []; []; [ "(" ]; [ ")" ] ], [ _; name; _; _ ] ->
      id_of_name name
  | _ ->
      failwith
        (Printf.sprintf "Invalid functionPrototype: %s"
           (Il.Print_debug.string_of_value v))

let id_of_declaration (decl : value) : string =
  match flatten_case_v decl with
  | ( "constantDeclaration",
      [ []; [ "CONST" ]; []; []; [ ";" ] ],
      [ _; _; name; _ ] ) ->
      id_of_name name
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
  | "errorDeclaration", _, _ -> failwith "errorDeclaration: no name"
  | "matchKindDeclaration", _, _ -> failwith "matchKindDeclaration: no name"
  | ( "externFunctionDeclaration",
      [ []; [ "EXTERN" ]; [ ";" ] ],
      [ _; functionPrototype ] ) ->
      id_of_function_prototype functionPrototype
  | ( "externObjectDeclaration",
      [ []; [ "EXTERN" ]; []; [ "{" ]; [ "}" ] ],
      [ _; nonTypeName; _; _ ] ) ->
      id_of_name nonTypeName
  | ( "parserDeclaration",
      [ []; [ "PARSER" ]; []; [ "(" ]; [ ")" ]; [ "{" ]; []; [ "}" ] ],
      [ _; name; _; _; _; _; _ ] )
  | ( "controlDeclaration",
      [ []; [ "CONTROL" ]; []; [ "(" ]; [ ")" ]; [ "{" ]; [ "APPLY" ]; [ "}" ] ],
      [ _; name; _; _; _; _; _ ] )
  | ( "enumTypeDeclaration",
      [ []; [ "ENUM" ]; [ "{" ]; []; [ "}" ] ],
      [ _; name; _; _ ] )
  | ( "enumTypeDeclaration",
      [ []; [ "ENUM" ]; []; [ "{" ]; []; [ "}" ] ],
      [ _; _; name; _; _ ] )
  | ( "structTypeDeclaration",
      [ []; [ "STRUCT" ]; []; [ "{" ]; [ "}" ] ],
      [ _; name; _; _ ] )
  | ( "headerTypeDeclaration",
      [ []; [ "HEADER" ]; []; [ "{" ]; [ "}" ] ],
      [ _; name; _; _ ] )
  | ( "headerUnionTypeDeclaration",
      [ []; [ "HEADER_UNION" ]; []; [ "{" ]; [ "}" ] ],
      [ _; name; _; _ ] )
  | "typedefDeclaration", [ []; [ "TYPEDEF" ]; []; [ ";" ] ], [ _; _; name ]
  | "typedefDeclaration", [ []; [ "TYPE" ]; []; [ ";" ] ], [ _; _; name ]
  | ( "parserTypeDeclaration",
      [ []; [ "PARSER" ]; []; [ "(" ]; [ ")"; ";" ] ],
      [ _; name; _; _ ] )
  | ( "controlTypeDeclaration",
      [ []; [ "CONTROL" ]; []; [ "(" ]; [ ")"; ";" ] ],
      [ _; name; _; _ ] )
  | ( "packageTypeDeclaration",
      [ []; [ "PACKAGE" ]; []; [ "(" ]; [ ")"; ";" ] ],
      [ _; name; _; _ ] ) ->
      id_of_name name
  (* not a variant of declaration *)
  | "tableDeclaration", [ []; [ "TABLE" ]; [ "{" ]; [ "}" ] ], [ _; name; _ ] ->
      id_of_name name
  | _ ->
      failwith
        (Printf.sprintf "Invalid declaration structure: %s"
           (F.asprintf "%a"
              (Concrete.Pp.pp_value Concrete.Hint.SMap.empty)
              decl))

let id_of_parameter (v : value) : string =
  match flatten_case_v v with
  | "parameter", [ []; []; []; []; [] ], [ _; _; _; name ] -> id_of_name name
  | "parameter", [ []; []; []; []; []; [] ], [ _; _; _; name; _ ] ->
      id_of_name name
  | _ -> failwith "@id_of_parameter: invalid CaseV"

(* Type parameter extraction *)

let has_type_params (v : value) : bool =
  match flatten_case_v v with
  | "typeParameterListOpt", [ [ "`EMPTY" ] ], [] -> false
  | "typeParameterListOpt", [ [ "<" ]; [ ">" ] ], [ _ ] -> true
  | "typeParameterListOpt", _, _ ->
      failwith
        (F.asprintf "@has_type_params: ill-formed typeParameterListOpt:\n%a"
           (Concrete.Pp.pp_value Concrete.Hint.SMap.empty)
           v)
  | _ ->
      failwith
        (Printf.sprintf
           "@has_type_params: expected typeParameterListOpt, got %s"
           (id_of_case_v v))

let has_type_params_function_prototype (v : value) : bool =
  match flatten_case_v v with
  | ( "functionPrototype",
      [ []; []; []; [ "(" ]; [ ")" ] ],
      [ _; _; typeParameterListOpt; _ ] ) ->
      has_type_params typeParameterListOpt
  | _ ->
      failwith
        (Printf.sprintf "Invalid functionPrototype: %s"
           (Il.Print_debug.string_of_value v))

let has_type_params_declaration (decl : value) : bool =
  match flatten_case_v decl with
  | "constantDeclaration", _, _ | "instantiation", _, _ -> false
  | "functionDeclaration", [ []; []; []; [] ], [ _; functionPrototype; _ ] ->
      has_type_params_function_prototype functionPrototype
  | "actionDeclaration", _, _
  | "errorDeclaration", _, _
  | "matchKindDeclaration", _, _ ->
      false
  | ( "externFunctionDeclaration",
      [ []; [ "EXTERN" ]; [ ";" ] ],
      [ _; functionPrototype ] ) ->
      has_type_params_function_prototype functionPrototype
  | ( "externObjectDeclaration",
      [ []; [ "EXTERN" ]; []; [ "{" ]; [ "}" ] ],
      [ _; _; typeParameterListOpt; _ ] ) ->
      has_type_params typeParameterListOpt
  | "parserDeclaration", _, _
  | "controlDeclaration", _, _
  | "enumTypeDeclaration", _, _ ->
      false
  | ( "structTypeDeclaration",
      [ []; [ "STRUCT" ]; []; [ "{" ]; [ "}" ] ],
      [ _; _; typeParameterListOpt; _ ] )
  | ( "headerTypeDeclaration",
      [ []; [ "HEADER" ]; []; [ "{" ]; [ "}" ] ],
      [ _; _; typeParameterListOpt; _ ] )
  | ( "headerUnionTypeDeclaration",
      [ []; [ "HEADER_UNION" ]; []; [ "{" ]; [ "}" ] ],
      [ _; _; typeParameterListOpt; _ ] ) ->
      has_type_params typeParameterListOpt
  | "typedefDeclaration", _, _ -> false
  | ( "parserTypeDeclaration",
      [ []; [ "PARSER" ]; []; [ "(" ]; [ ")"; ";" ] ],
      [ _; _; typeParameterListOpt; _ ] )
  | ( "controlTypeDeclaration",
      [ []; [ "CONTROL" ]; []; [ "(" ]; [ ")"; ";" ] ],
      [ _; _; typeParameterListOpt; _ ] )
  | ( "packageTypeDeclaration",
      [ []; [ "PACKAGE" ]; []; [ "(" ]; [ ")"; ";" ] ],
      [ _; _; typeParameterListOpt; _ ] ) ->
      has_type_params typeParameterListOpt
  | _ ->
      failwith
        (Printf.sprintf "@has_typ_params: Unknown declaration %s"
           (id_of_case_v decl))
