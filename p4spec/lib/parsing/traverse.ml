  open Il.Ast
  open Xl.Atom
  open Util.Source

  let id_of_case_v (v: value) : string =
    match v.it, v.note.typ with
    | CaseV _, VarT (id, _) -> id.it
    | _ -> failwith "not a case value"

  let check_id (v: value) (expected_id: string) : unit =
    let actual_id = id_of_case_v v in
    if actual_id <> expected_id then
      failwith (Printf.sprintf "Expected id '%s', but got '%s'" expected_id actual_id)

  (* name = nonTypeName | LIST | typeIdentifier *)
  let id_of_name (v: value) : string =
    assert (id_of_case_v v = "name");
    match v.it with
    | CaseV (_, [ value ]) when id_of_case_v value = "nonTypeName" -> (
        (* nonTypeName = identifier | APPLY | KEY | ACTIONS | STATE | ENTRIES | TYPE | PRIORITY *)
        match value.it with
        | CaseV (_, [ value ]) when id_of_case_v value = "identifier" -> (
            match value.it with
            | TextV s -> s
            | _ -> failwith "unknown identifier text"
        )
        | CaseV ( [ [ atom ] ], []) when atom.it = Atom "apply" -> "apply"
        | CaseV ( [ [ atom ] ], []) when atom.it = Atom "key" -> "key"
        | CaseV ( [ [ atom ] ], []) when atom.it = Atom "actions" -> "actions"
        | CaseV ( [ [ atom ] ], []) when atom.it = Atom "state" -> "state"
        | CaseV ( [ [ atom ] ], []) when atom.it = Atom "entries" -> "entries"
        | CaseV ( [ [ atom ] ], []) when atom.it = Atom "type" -> "type"
        | CaseV ( [ [ atom ] ], []) when atom.it = Atom "priority" -> "priority"
        | _ -> failwith "invalid nonTypeName structure"
    )
    | CaseV ( [ [ atom ] ], []) when atom.it = Atom "list" -> "list"
    | CaseV (_, [ value ]) when id_of_case_v value = "typeIdentifier" -> (
        (* typeIdentifier = `@ text *)
        match value.it with
        | CaseV (_, [_; text_value ]) -> (
            match text_value.it with
            | TextV s -> s
            | _ -> failwith "invalid typeIdentifier text"
        )
        | _ -> failwith "invalid typeIdentifier structure"
    )
    | _ -> failwith "invalid name structure"

  (* constantDeclaration =  optAnnotations CONST typeRef name initializer `; *)
  let name_of_constant_declaration (v: value) : value =
    assert (id_of_case_v v = "constantDeclaration");
    let name =
        match v.it with
        | CaseV (_, [ _; _; name; _ ]) -> name
        | _ -> failwith "invalid constantDeclaration structure"
    in
    assert (id_of_case_v name = "name");
    name

  (* functionPrototype = typeOrVoid name optTypeParameters `( parameterList ) *)
  let name_of_function_prototype (v: value) : value =
    assert (id_of_case_v v = "functionPrototype");
    let name =
        match v.it with
        | CaseV (_, [ _; name; _; _ ]) -> name
        | _ -> failwith "invalid functionPrototype structure"
    in
    assert (id_of_case_v name = "name");
    name

  (* externDeclaration *)
  let name_of_extern_declaration (v: value) : value =
    assert (id_of_case_v v = "externDeclaration");
    let name =
        match v.it with
        (* optAnnotations EXTERN nonTypeName optTypeParameters `{ methodPrototypes } *)
        | CaseV (_, [ _; nonTypeName; _; _ ]) -> nonTypeName
        (* optAnnotations EXTERN functionPrototype `; *)
        | CaseV (_, [ _; functionPrototype; _ ]) -> name_of_function_prototype functionPrototype
        | _ -> failwith "invalid externDeclaration structure"
    in
    assert (id_of_case_v name = "name");
    name

  let name_of_instantiation (v: value) : value =
    assert (id_of_case_v v = "instantiation");
    let name =
        match v.it with
        (* optAnnotations typeRef `( argumentList ) name `; *)
        | CaseV (_, [ _; _; _; name ]) -> name
        (* optAnnotations typeRef `( argumentList ) name objInitializer `; *)
        | CaseV (_, [ _; _; _; name; _ ]) -> name
        | _ -> failwith "invalid instantiation structure"
    in
    assert (id_of_case_v name = "name");
    name

  (* functionDeclaration = optAnnotations functionPrototype blockStatement *)
  let name_of_function_declaration (v: value) : value =
    assert (id_of_case_v v = "functionDeclaration");
    let name =
        match v.it with
      | CaseV (_, [ _; functionPrototype; _ ]) -> name_of_function_prototype functionPrototype
        | _ -> failwith "invalid functionDeclaration structure"
    in
    assert (id_of_case_v name = "name");
    name

  (* actionDeclaration = optAnnotations ACTION name `( parameterList ) blockStatement *)
  let name_of_action_declaration (v: value) : value =
    assert (id_of_case_v v = "actionDeclaration");
    let name =
        match v.it with
        (* optAnnotations ACTION name `( parameterList ) blockStatement *)
        | CaseV (_, [ _; name; _; _ ]) -> name
        | _ -> failwith "invalid actionDeclaration structure"
    in
    assert (id_of_case_v name = "name");
    name

  (* parserTypeDeclaration = optAnnotations PARSER name optTypeParameters `( parameterList ) *)
  let name_of_parser_type_declaration (v: value) : value =
    assert (id_of_case_v v = "parserTypeDeclaration");
    let name =
        match v.it with
        | CaseV (_, [ _; name; _; _ ]) -> name
        | _ -> failwith "invalid parserTypeDeclaration structure"
    in
    assert (id_of_case_v name = "name");
    name

  (* parserDeclaration = parserTypeDeclaration optConstructorParameters `{ parserLocalElements parserStates } *)
  let name_of_parser_declaration (v: value) : value =
    assert (id_of_case_v v = "parserDeclaration");
    let name =
        match v.it with
        | CaseV (_, [ parserTypeDeclaration; _; _ ]) -> name_of_parser_type_declaration parserTypeDeclaration
        | _ -> failwith "invalid parserDeclaration structure"
    in
    assert (id_of_case_v name = "name");
    name

  (* controlDeclaration = controlTypeDeclaration optConstructorParameters `{ controlLocalDeclarations APPLY controlBody } *)
  let name_of_control_declaration (v: value) : value =
    assert (id_of_case_v v = "controlDeclaration");
    let name =
        match v.it with
        | CaseV (_, [ controlTypeDeclaration; _; _ ]) -> (
            assert (id_of_case_v controlTypeDeclaration = "controlTypeDeclaration");
            match controlTypeDeclaration.it with
            | CaseV (_, [ _; name; _; _ ]) -> name
            | _ -> failwith "invalid controlTypeDeclaration structure"
        )
        | _ -> failwith "invalid controlDeclaration structure"
    in
    assert (id_of_case_v name = "name");
    name

  (* headerTypeDeclaration = optAnnotations HEADER name optTypeParameters `{ structFieldList } *)
  let name_of_header_type_declaration (v: value) : value =
    assert (id_of_case_v v = "headerTypeDeclaration");
    let name =
        match v.it with
        (* optAnnotations HEADER name optTypeParameters `{ structFieldList } *)
        | CaseV (_, [ _; name; _; _ ]) -> name
        | _ -> failwith "invalid headerTypeDeclaration structure"
    in
    assert (id_of_case_v name = "name");
    name

  (* headerUnionDeclaration = optAnnotations HEADER_UNION name optTypeParameters `{ structFieldList } *)
  let name_of_header_union_declaration (v: value) : value =
    assert (id_of_case_v v = "headerUnionDeclaration");
    let name =
        match v.it with
        (* optAnnotations HEADER_UNION name optTypeParameters `{ structFieldList } *)
        | CaseV (_, [ _; name; _; _ ]) -> name
        | _ -> failwith "invalid headerUnionDeclaration structure"
    in
    assert (id_of_case_v name = "name");
    name

  (* structTypeDeclaration = optAnnotations STRUCT name optTypeParameters `{ structFieldList } *)
  let name_of_struct_type_declaration (v: value) : value =
    assert (id_of_case_v v = "structTypeDeclaration");
    let name =
        match v.it with
        (* optAnnotations STRUCT name optTypeParameters `{ structFieldList } *)
        | CaseV (_, [ _; name; _; _ ]) -> name
        | _ -> failwith "invalid structTypeDeclaration structure"
    in
    assert (id_of_case_v name = "name");
    name

  (* enumDeclaration = optAnnotations ENUM name `{ identifierList optTrailingComma }
   * | optAnnotations ENUM typeRef name `{ specifiedIdentifierList optTrailingComma } *)
  let name_of_enum_declaration (v: value) : value =
    assert (id_of_case_v v = "enumDeclaration");
    let name =
        match v.it with
        | CaseV (_, [ _; name; _; _ ]) -> name
        | CaseV (_, [ _; _; name; _; _ ]) -> name
        | _ -> failwith "invalid enumDeclaration structure"
    in
    assert (id_of_case_v name = "name");
    name

  (* derivedTypeDeclaration = headerTypeDeclaration | headerUnionDeclaration | structTypeDeclaration | enumDeclaration *)
  let name_of_derived_type_declaration (v: value) : value =
    assert (id_of_case_v v = "derivedTypeDeclaration");
    let name =
      match v.it with
      | CaseV (_, [ value ]) when id_of_case_v value = "headerTypeDeclaration" -> name_of_header_type_declaration value
      | CaseV (_, [ value ]) when id_of_case_v value = "headerUnionDeclaration" -> name_of_header_union_declaration value
      | CaseV (_, [ value ]) when id_of_case_v value = "structTypeDeclaration" -> name_of_struct_type_declaration value
      | CaseV (_, [ value ]) when id_of_case_v value = "enumDeclaration" -> name_of_enum_declaration value
      | _ -> failwith (Printf.sprintf "Unknown derived type declaration: %s" (Il.Print.string_of_value v))
    in
    assert (id_of_case_v name = "name");
    name
  
  (* TODO: disambiguate mixops *)
  let name_of_type_def_declaration (v: value) : value =
    assert (id_of_case_v v = "typedefDeclaration");
    let name =
        match v.it with
        | CaseV (_, [ _; _; name ]) -> name
        | _ -> failwith "invalid typedefDeclaration structure"
    in
    assert (id_of_case_v name = "name");
    name

  (* controlTypeDeclaration = optAnnotations CONTROL name optTypeParameters `( parameterList ) *)
  let name_of_control_type_declaration (v: value) : value =
    assert (id_of_case_v v = "controlTypeDeclaration");
    let name =
        match v.it with
        | CaseV (_, [ _; name; _; _ ]) -> name
        | _ -> failwith "invalid controlTypeDeclaration structure"
    in
    assert (id_of_case_v name = "name");
    name

  (* packageTypeDeclaration = optAnnotations PACKAGE name optTypeParameters `( parameterList ) *)
  let name_of_package_type_declaration (v: value) : value =
    assert (id_of_case_v v = "packageTypeDeclaration");
    let name =
        match v.it with
        | CaseV (_, [ _; name; _; _ ]) -> name
        | _ -> failwith "invalid packageTypeDeclaration structure"
    in
    assert (id_of_case_v name = "name");
    name

  (* typeDeclaration = derivedTypeDeclaration | typedefDeclaration | parserTypeDeclaration | controlTypeDeclaration | packageTypeDeclaration *)
  let name_of_type_declaration (v: value) : value =
    assert (id_of_case_v v = "typeDeclaration");
    let name =
        match v.it with
        | CaseV (_, [ value ]) when id_of_case_v value = "derivedTypeDeclaration" -> name_of_derived_type_declaration value
        | CaseV (_, [ value ]) when id_of_case_v value = "typedefDeclaration" -> name_of_type_def_declaration value
        | CaseV (_, [ value ]) when id_of_case_v value = "parserTypeDeclaration" -> name_of_parser_type_declaration value
        | CaseV (_, [ value ]) when id_of_case_v value = "controlTypeDeclaration" -> name_of_control_type_declaration value
        | CaseV (_, [ value ]) when id_of_case_v value = "packageTypeDeclaration" -> name_of_package_type_declaration value
        | _ -> failwith (Printf.sprintf "Unknown type declaration: %s" (id_of_case_v v))
    in
    assert (id_of_case_v name = "name");
    name

  let name_of_declaration (decl: value) : value =
    assert (id_of_case_v decl = "declaration");
    let name =
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
    in
    assert (id_of_case_v name = "name");
    name

  let name_of_any_declaration (v: value) : value =
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

  let has_typ_params_declaration (_d: value) : bool =
    (* TODO: check for type parameters in CaseV *)
    false
