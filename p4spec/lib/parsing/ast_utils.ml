  open Il.Ast
  open Xl.Atom
  open Util.Source

  let wrap_atom (s : string) : atom = Atom s $ no_region
  let wrap_var_t (s : string) : typ' = VarT (s $ no_region, [])
  let wrap_iter_t (i : iter) (t: typ') : typ' = IterT (t $ no_region, i)
  
  let with_fresh_val (typ: typ') : vnote = 
    let vid = Value.fresh () in
    { vid; typ }
  let with_typ (typ: typ') (v: value') : value =
    v $$$ with_fresh_val typ

  type value_or_atom = NT of value | Term of string

  let wrap_case_v (vs: value_or_atom list) : value' =
    let rec build_mixop acc_mixop acc_terms = function
      | [] -> 
          (* Always add the final group, even if empty *)
          acc_mixop @ [acc_terms]
      | Term s :: rest ->
          (* Accumulate terms *)
          build_mixop acc_mixop (acc_terms @ [wrap_atom s]) rest
      | NT _ :: rest ->
          (* When we hit a non-terminal, add accumulated terms to mixop and start new group *)
          let new_mixop = acc_mixop @ [acc_terms] in
          build_mixop new_mixop [] rest
    in
    let mixop = build_mixop [] [] vs in
    let values = 
      vs 
      |> List.filter (fun v -> match v with NT _ -> true | _ -> false) 
      |> List.map (function
        | NT v -> v
        | Term _ -> assert false)
    in
    CaseV (mixop, values)

  let wrap_opt_v (v: value option) (s: string) : value = OptV v |> with_typ (wrap_iter_t Opt (wrap_var_t s))
  let wrap_list_v (vs: value list) (s: string) : value = ListV vs |> with_typ (wrap_iter_t List (wrap_var_t s))

  let id_of_case_v (v: value) : string =
    match v.it, v.note.typ with
    | CaseV _, VarT (id, _) -> id.it
    | _ -> failwith "not a case value"

  let flatten_case_v (value: value) = 
    match value.it with
    | CaseV (mixop, values) ->
        let mixop = List.map 
            (List.map 
              (fun p -> 
                string_of_atom p.it
              )
            )
            mixop
        in
        let values = List.map (fun v -> v.it) values in
        let id = id_of_case_v value in
        id, mixop, values
    | _ -> failwith "Expected a CaseV value"

  (* name = nonTypeName | LIST | typeIdentifier *)
  let id_of_name (value: value) : string =
    match flatten_case_v value with
    | "identifier", [ ["$"]; [] ], [ TextV s ]
      -> s
    | "nonTypeName", [ ["APPLY"] ], []
      -> "apply"
    | "nonTypeName", [ ["KEY"] ], []
      -> "key"
    | "nonTypeName", [ ["ACTIONS"] ], []
      -> "actions"
    | "nonTypeName", [ ["STATE"] ], []
      -> "state"
    | "nonTypeName", [ ["ENTRIES"] ], []
      -> "entries"
    | "nonTypeName", [ ["TYPE"] ], []
      -> "type"
    | "nonTypeName", [ ["PRIORITY"] ], []
      -> "priority"
    | "name", [ ["LIST"] ], []
      -> "list"
    | "typeIdentifier", [ ["@"]; [] ], [ TextV s ] -> s
| _ -> failwith (Printf.sprintf "Invalid name structure %s: %s " (Il.Print.string_of_value value) (id_of_case_v value))

  (******** Name of declarations ********)

  (* constantDeclaration =  optAnnotations CONST typeRef name initializer `; *)
  let name_of_constant_declaration (v: value) : string =
    assert (id_of_case_v v = "constantDeclaration");
    match v.it with
    | CaseV (_, [ _; _; name; _ ]) -> id_of_name name
    | _ -> failwith "invalid constantDeclaration structure"

  (* functionPrototype = typeOrVoid name optTypeParameters `( parameterList ) *)
  let name_of_function_prototype (v: value) : string =
    assert (id_of_case_v v = "functionPrototype");
    match v.it with
    | CaseV (_, [ _; name; _; _ ]) -> id_of_name name
    | _ -> failwith "invalid functionPrototype structure"

  (* externDeclaration *)
  let name_of_extern_declaration (v: value) : string =
    assert (id_of_case_v v = "externDeclaration");
    match v.it with
    (* optAnnotations EXTERN nonTypeName optTypeParameters `{ methodPrototypes } *)
    | CaseV (_, [ _; nonTypeName; _; _ ]) -> id_of_name nonTypeName
    (* optAnnotations EXTERN functionPrototype `; *)
    | CaseV (_, [ _; functionPrototype; _ ]) -> name_of_function_prototype functionPrototype
    | _ -> failwith "invalid externDeclaration structure"

  let name_of_instantiation (v: value) : string =
    assert (id_of_case_v v = "instantiation");
    match v.it with
    (* optAnnotations typeRef `( argumentList ) name `; *)
    | CaseV (_, [ _; _; _; name ]) -> id_of_name name
    (* optAnnotations typeRef `( argumentList ) name objInitializer `; *)
    | CaseV (_, [ _; _; _; name; _ ]) -> id_of_name name
    | _ -> failwith "invalid instantiation structure"

  (* functionDeclaration = optAnnotations functionPrototype blockStatement *)
  let name_of_function_declaration (v: value) : string =
    assert (id_of_case_v v = "functionDeclaration");
    match v.it with
    | CaseV (_, [ _; functionPrototype; _ ]) -> name_of_function_prototype functionPrototype
    | _ -> failwith "invalid functionDeclaration structure"

  (* actionDeclaration = optAnnotations ACTION name `( parameterList ) blockStatement *)
  let name_of_action_declaration (v: value) : string =
    assert (id_of_case_v v = "actionDeclaration");
    match v.it with
    (* optAnnotations ACTION name `( parameterList ) blockStatement *)
    | CaseV (_, [ _; name; _; _ ]) -> id_of_name name
    | _ -> failwith "invalid actionDeclaration structure"

  (* parserTypeDeclaration = optAnnotations PARSER name optTypeParameters `( parameterList ) *)
  let name_of_parser_type_declaration (v: value) : string =
    assert (id_of_case_v v = "parserTypeDeclaration");
        match v.it with
        | CaseV (_, [ _; name; _; _ ]) -> id_of_name name
        | _ -> failwith "invalid parserTypeDeclaration structure"

  (* parserDeclaration = parserTypeDeclaration optConstructorParameters `{ parserLocalElements parserStates } *)
  let name_of_parser_declaration (v: value) : string =
    assert (id_of_case_v v = "parserDeclaration");
    match v.it with
    | CaseV (_, [ parserTypeDeclaration; _; _ ]) -> name_of_parser_type_declaration parserTypeDeclaration
    | _ -> failwith "invalid parserDeclaration structure"

  (* controlDeclaration = controlTypeDeclaration optConstructorParameters `{ controlLocalDeclarations APPLY controlBody } *)
  let name_of_control_declaration (v: value) : string =
    assert (id_of_case_v v = "controlDeclaration");
    match v.it with
    | CaseV (_, [ controlTypeDeclaration; _; _ ]) -> (
        assert (id_of_case_v controlTypeDeclaration = "controlTypeDeclaration");
        match controlTypeDeclaration.it with
        | CaseV (_, [ _; name; _; _ ]) -> id_of_name name
        | _ -> failwith "invalid controlTypeDeclaration structure"
    )
    | _ -> failwith "invalid controlDeclaration structure"

  (* headerTypeDeclaration = optAnnotations HEADER name optTypeParameters `{ structFieldList } *)
  let name_of_header_type_declaration (v: value) : string =
    assert (id_of_case_v v = "headerTypeDeclaration");
    match v.it with
    (* optAnnotations HEADER name optTypeParameters `{ structFieldList } *)
    | CaseV (_, [ _; name; _; _ ]) -> id_of_name name
    | _ -> failwith "invalid headerTypeDeclaration structure"

  (* headerUnionDeclaration = optAnnotations HEADER_UNION name optTypeParameters `{ structFieldList } *)
  let name_of_header_union_declaration (v: value) : string =
    assert (id_of_case_v v = "headerUnionDeclaration");
    match v.it with
    (* optAnnotations HEADER_UNION name optTypeParameters `{ structFieldList } *)
    | CaseV (_, [ _; name; _; _ ]) -> id_of_name name
    | _ -> failwith "invalid headerUnionDeclaration structure"

  (* structTypeDeclaration = optAnnotations STRUCT name optTypeParameters `{ structFieldList } *)
  let name_of_struct_type_declaration (v: value) : string =
    assert (id_of_case_v v = "structTypeDeclaration");
    match v.it with
    (* optAnnotations STRUCT name optTypeParameters `{ structFieldList } *)
    | CaseV (_, [ _; name; _; _ ]) -> id_of_name name
    | _ -> failwith "invalid structTypeDeclaration structure"

  (* enumDeclaration = optAnnotations ENUM name `{ identifierList optTrailingComma }
   * | optAnnotations ENUM typeRef name `{ specifiedIdentifierList optTrailingComma } *)
  let name_of_enum_declaration (v: value) : string =
    assert (id_of_case_v v = "enumDeclaration");
    match v.it with
    | CaseV (_, [ _; name; _; _ ]) -> id_of_name name
    | CaseV (_, [ _; _; name; _; _ ]) -> id_of_name name
    | _ -> failwith "invalid enumDeclaration structure"

  (* derivedTypeDeclaration = headerTypeDeclaration | headerUnionDeclaration | structTypeDeclaration | enumDeclaration *)
  let name_of_derived_type_declaration (v: value) : string =
    assert (id_of_case_v v = "derivedTypeDeclaration");
    match v.it with
    | CaseV (_, [ value ]) when id_of_case_v value = "headerTypeDeclaration" -> name_of_header_type_declaration value
    | CaseV (_, [ value ]) when id_of_case_v value = "headerUnionDeclaration" -> name_of_header_union_declaration value
    | CaseV (_, [ value ]) when id_of_case_v value = "structTypeDeclaration" -> name_of_struct_type_declaration value
    | CaseV (_, [ value ]) when id_of_case_v value = "enumDeclaration" -> name_of_enum_declaration value
    | _ -> failwith (Printf.sprintf "Unknown derived type declaration: %s" (Il.Print.string_of_value v))
  
  (* TODO: disambiguate mixops *)
  let name_of_type_def_declaration (v: value) : string =
    assert (id_of_case_v v = "typedefDeclaration");
    let name =
        match v.it with
        | CaseV (_, [ _; _; name ]) -> name
        | _ -> failwith "invalid typedefDeclaration structure"
    in
    id_of_name name

  (* controlTypeDeclaration = optAnnotations CONTROL name optTypeParameters `( parameterList ) *)
  let name_of_control_type_declaration (v: value) : string =
    assert (id_of_case_v v = "controlTypeDeclaration");
    let name =
        match v.it with
        | CaseV (_, [ _; name; _; _ ]) -> name
        | _ -> failwith "invalid controlTypeDeclaration structure"
    in
    id_of_name name

  (* packageTypeDeclaration = optAnnotations PACKAGE name optTypeParameters `( parameterList ) *)
  let name_of_package_type_declaration (v: value) : string =
    assert (id_of_case_v v = "packageTypeDeclaration");
    let name =
        match v.it with
        | CaseV (_, [ _; name; _; _ ]) -> name
        | _ -> failwith "invalid packageTypeDeclaration structure"
    in
    id_of_name name

  (* typeDeclaration = derivedTypeDeclaration | typedefDeclaration | parserTypeDeclaration | controlTypeDeclaration | packageTypeDeclaration *)
  let name_of_type_declaration (v: value) : string =
    assert (id_of_case_v v = "typeDeclaration");
    match v.it with
    | CaseV (_, [ value ]) when id_of_case_v value = "derivedTypeDeclaration" -> name_of_derived_type_declaration value
    | CaseV (_, [ value ]) when id_of_case_v value = "typedefDeclaration" -> name_of_type_def_declaration value
    | CaseV (_, [ value ]) when id_of_case_v value = "parserTypeDeclaration" -> name_of_parser_type_declaration value
    | CaseV (_, [ value ]) when id_of_case_v value = "controlTypeDeclaration" -> name_of_control_type_declaration value
    | CaseV (_, [ value ]) when id_of_case_v value = "packageTypeDeclaration" -> name_of_package_type_declaration value
    | _ -> failwith (Printf.sprintf "Unknown type declaration: %s" (id_of_case_v v))

  let name_of_declaration (decl: value) : string =
    assert (id_of_case_v decl = "declaration");
    match decl.it with
    | CaseV (_, [ value ]) when id_of_case_v value = "constantDeclaration" -> name_of_constant_declaration value
    | CaseV (_, [ value ]) when id_of_case_v value = "errorDeclaration" -> failwith "no name"
    | CaseV (_, [ value ]) when id_of_case_v value = "matchKindDeclaration" -> failwith "no name"
    | CaseV (_, [ value ]) when id_of_case_v value = "externDeclaration" -> name_of_extern_declaration value
    | CaseV (_, [ value ]) when id_of_case_v value = "instantiation" -> name_of_instantiation value
    | CaseV (_, [ value ]) when id_of_case_v value = "functionDeclaration" -> name_of_function_declaration value
    | CaseV (_, [ value ]) when id_of_case_v value = "actionDeclaration" -> name_of_action_declaration value
    | CaseV (_, [ value ]) when id_of_case_v value = "parserDeclaration" -> name_of_parser_declaration value
    | CaseV (_, [ value ]) when id_of_case_v value = "controlDeclaration" -> name_of_control_declaration value
    | CaseV (_, [ value ]) when id_of_case_v value = "typeDeclaration" -> name_of_type_declaration value
    | _ -> failwith (Printf.sprintf "Unknown declaration type: %s" (id_of_case_v decl))

  let name_of_any_declaration (v: value) : string =
    match id_of_case_v v with
    | "declaration" -> name_of_declaration v
    | "constantDeclaration" -> name_of_constant_declaration v
    | "errorDeclaration" -> failwith "no name"
    | "matchKindDeclaration" -> failwith "no name"
    | "externDeclaration" -> name_of_extern_declaration v
    | "instantiation" -> name_of_instantiation v
    | "functionDeclaration" -> name_of_function_declaration v
    | "actionDeclaration" -> name_of_action_declaration v
    | "parserDeclaration" -> name_of_parser_declaration v
    | "controlDeclaration" -> name_of_control_declaration v
    | "typeDeclaration" -> name_of_type_declaration v
    | "derivedTypeDeclaration" -> name_of_derived_type_declaration v
    | "headerTypeDeclaration" -> name_of_header_type_declaration v
    | "headerUnionDeclaration" -> name_of_header_union_declaration v
    | "structTypeDeclaration" -> name_of_struct_type_declaration v
    | "enumDeclaration" -> name_of_enum_declaration v
    | "typedefDeclaration" -> name_of_type_def_declaration v
    | "parserTypeDeclaration" -> name_of_parser_type_declaration v
    | "controlTypeDeclaration" -> name_of_control_type_declaration v
    | "packageTypeDeclaration" -> name_of_package_type_declaration v
    | _ -> failwith (Printf.sprintf "Unknown declaration type: %s" (id_of_case_v v))

  let has_typ_params_parser_type_declaration (_v: value') : bool =
    false
    (* match flatten_case_v parserTypeDeclaration with *)
    (* | "parserTypeDeclaration", [ []; ["PHTM_13"] ], _ -> true *)
    (* | _ -> false *)

  let has_typ_params_control_type_declaration (_v: value') : bool =
    false

  let has_typ_params_package_type_declaration (_v: value') : bool =
    false

  let has_typ_params_declaration (value_decl: value) : bool =
    match flatten_case_v value_decl with
    | "constantDeclaration", _, _
    | "errorDeclaration", _, _
    | "matchKindDeclaration", _, _ -> false
    | "externDeclaration", [ []; ["EXTERN"]; []; ["{"]; ["}"] ], [ _; _; _optTypeParameters; _] -> failwith "TODO"
    | "externDeclaration", _, _ -> false
    | "instantiation", _, _ -> false
    | "functionDeclaration", _, _ -> failwith "TODO"
    | "actionDeclaration", _, _ -> false
    | "parserDeclaration", _, _ -> false
    | "controlDeclaration", _, _ -> false
    | "headerTypeDeclaration", _, _
    | "headerUnionDeclaration", _, _
    | "structTypeDeclaration", _, _
      ->  failwith "TODO"
    | "enumDeclaration", _, _ -> false
    | "typeDeclaration", [ []; [";"] ], [ _typedefDeclaration ] -> false
    | "typeDeclaration", [ []; [";"; "PHTM_13"] ], [ parserTypeDeclaration ] ->
      has_typ_params_parser_type_declaration parserTypeDeclaration
    | "typeDeclaration", [ []; [";"; "PHTM_14"] ], [ controlTypeDeclaration ] ->
      has_typ_params_control_type_declaration controlTypeDeclaration
    | "typeDeclaration", [ []; [";"; "PHTM_15"] ], [ packageTypeDeclaration ] ->
      has_typ_params_package_type_declaration packageTypeDeclaration
    | _ -> failwith (Printf.sprintf "Unknown type declaration: %s" (id_of_case_v value_decl))


