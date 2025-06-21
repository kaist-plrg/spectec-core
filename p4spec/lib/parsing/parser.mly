%{
  open Il.Ast
  open Xl.Atom
  open Util.Source
  open Context

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
    let mixop : atom list list =
      List.mapi
        (fun i v ->
          if i = 0 then [[]]
          (* TODO: handle case where last value is an NT *)
          else 
            (let v_prev = List.nth vs i in
              (match (v_prev, v) with
              | NT _, NT _ -> [[]]
              | _, Term s -> [[wrap_atom s]] 
              | _, _ -> [])
            )
          ) vs |> List.concat
    in
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


  let name_of_declaration (_declaration: value) : value =
    (* match declaration.it, declaration.note.typ with *)
    (* | CaseV (_, [ const_decl ]), VarT ({ it = "constantDeclaration"; _ }, _) -> ( *)
    (*     (* constantDeclaration = optAnnotations CONST typeRef name initializer `; *) *)
    (*     (* Extract the name field (4th position: optAnnotations, CONST, typeRef, name, initializer, `;`) *) *)
    (*     match const_decl.it with *)
    (*     | CaseV (_, [ _; _; _; name; _; _ ]) -> name *)
    (*     | _ -> failwith "invalid constantDeclaration structure") *)
    (* | CaseV (_, [ func_decl ]), VarT ({ it = "functionDeclaration"; _ }, _) -> ( *)
    (*     (* functionDeclaration = optAnnotations functionPrototype blockStatement *) *)
    (*     (* Extract name from functionPrototype = typeOrVoid name optTypeParameters `( parameterList ) *) *)
    (*     match func_decl.it with *)
    (*     | CaseV (_, [ _; func_proto; _ ]) -> ( *)
    (*         match func_proto.it with *)
    (*         | CaseV (_, [ _; name; _; _ ]) -> name *)
    (*         | _ -> failwith "invalid functionPrototype structure") *)
    (*     | _ -> failwith "invalid functionDeclaration structure") *)
    (* | CaseV (_, [ action_decl ]), VarT ({ it = "actionDeclaration"; _ }, _) -> ( *)
    (*     (* actionDeclaration = optAnnotations ACTION name `( parameterList ) blockStatement *) *)
    (*     (* Extract the name field (3rd position: optAnnotations, ACTION, name, `( parameterList )`, blockStatement) *) *)
    (*     match action_decl.it with *)
    (*     | CaseV (_, [ _; _; name; _; _ ]) -> name *)
    (*     | _ -> failwith "invalid actionDeclaration structure") *)
    (* | CaseV (_, [ inst_decl ]), VarT ({ it = "instantiation"; _ }, _) -> ( *)
    (*     (* instantiation = optAnnotations typeRef `( argumentList ) name `; *) *)
    (*     (* Extract the name field (5th position: optAnnotations, typeRef, `( argumentList )`, name, `;`) *) *)
    (*     match inst_decl.it with *)
    (*     | CaseV (_, [ _; _; _; name; _ ]) -> name *)
    (*     | _ -> failwith "invalid instantiation structure") *)
    (* | CaseV (_, [ table_decl ]), VarT ({ it = "tableDeclaration"; _ }, _) -> ( *)
    (*     (* tableDeclaration = optAnnotations TABLE name `{ tablePropertyList } *) *)
    (*     (* Extract the name field (3rd position: optAnnotations, TABLE, name, `{ tablePropertyList }`) *) *)
    (*     match table_decl.it with *)
    (*     | CaseV (_, [ _; _; name; _ ]) -> name *)
    (*     | _ -> failwith "invalid tableDeclaration structure") *)
    (* | CaseV (_, [ parser_decl ]), VarT ({ it = "parserDeclaration"; _ }, _) -> ( *)
    (*     (* parserDeclaration = parserTypeDeclaration optConstructorParameters `{ parserLocalElements parserStates } *) *)
    (*     (* Extract name from parserTypeDeclaration = optAnnotations PARSER name optTypeParameters `( parameterList ) *) *)
    (*     match parser_decl.it with *)
    (*     | CaseV (_, [ parser_type; _; _ ]) -> ( *)
    (*         match parser_type.it with *)
    (*         | CaseV (_, [ _; _; name; _; _ ]) -> name *)
    (*         | _ -> failwith "invalid parserTypeDeclaration structure") *)
    (*     | _ -> failwith "invalid parserDeclaration structure") *)
    (* | CaseV (_, [ control_decl ]), VarT ({ it = "controlDeclaration"; _ }, _) -> ( *)
    (*     (* controlDeclaration = controlTypeDeclaration optConstructorParameters `{ controlLocalDeclarations APPLY controlBody } *) *)
    (*     (* Extract name from controlTypeDeclaration = optAnnotations CONTROL name optTypeParameters `( parameterList ) *) *)
    (*     match control_decl.it with *)
    (*     | CaseV (_, [ control_type; _; _ ]) -> ( *)
    (*         match control_type.it with *)
    (*         | CaseV (_, [ _; _; name; _; _ ]) -> name *)
    (*         | _ -> failwith "invalid controlTypeDeclaration structure") *)
    (*     | _ -> failwith "invalid controlDeclaration structure") *)
    (* | CaseV (_, [ extern_decl ]), VarT ({ it = "externDeclaration"; _ }, _) -> ( *)
    (*     (* externDeclaration = optAnnotations EXTERN nonTypeName optTypeParameters `{ methodPrototypes } *) *)
    (*     (* Extract the nonTypeName field (3rd position: optAnnotations, EXTERN, nonTypeName, optTypeParameters, `{ methodPrototypes }`) *) *)
    (*     match extern_decl.it with *)
    (*     | CaseV (_, [ _; _; name; _; _ ]) -> name *)
    (*     | _ -> failwith "invalid externDeclaration structure") *)
    (* | CaseV (_, [ error_decl ]), VarT ({ it = "errorDeclaration"; _ }, _) -> ( *)
    (*     (* errorDeclaration = ERROR `{ identifierList } *) *)
    (*     (* This doesn't have a single name, but we can extract the first identifier from identifierList *) *)
    (*     match error_decl.it with *)
    (*     | CaseV (_, [ _; id_list ]) -> ( *)
    (*         match id_list.it with *)
    (*         | ListV (first_id :: _) -> first_id *)
    (*         | _ -> failwith "empty identifierList in errorDeclaration") *)
    (*     | _ -> failwith "invalid errorDeclaration structure") *)
    (* | CaseV (_, [ match_kind_decl ]), VarT ({ it = "matchKindDeclaration"; _ }, _) -> ( *)
    (*     (* matchKindDeclaration = MATCH_KIND `{ identifierList optTrailingComma } *) *)
    (*     (* This doesn't have a single name, but we can extract the first identifier from identifierList *) *)
    (*     match match_kind_decl.it with *)
    (*     | CaseV (_, [ _; id_list; _ ]) -> ( *)
    (*         match id_list.it with *)
    (*         | ListV (first_id :: _) -> first_id *)
    (*         | _ -> failwith "empty identifierList in matchKindDeclaration") *)
    (*     | _ -> failwith "invalid matchKindDeclaration structure") *)
    (* | CaseV (_, [ type_decl ]), VarT ({ it = "typeDeclaration"; _ }, _) -> ( *)
    (*     (* typeDeclaration = derivedTypeDeclaration | typedefDeclaration `; | parserTypeDeclaration `; | controlTypeDeclaration `; | packageTypeDeclaration `; *) *)
    (*     (* We need to handle each subtype *) *)
    (*     match type_decl.it with *)
    (*     | CaseV (_, [ derived_type ]) -> ( *)
    (*         (* derivedTypeDeclaration = headerTypeDeclaration | headerUnionDeclaration | structTypeDeclaration | enumDeclaration *) *)
    (*         match derived_type.it, derived_type.note.typ with *)
    (*         | CaseV (_, [ header_type ]), VarT ({ it = "headerTypeDeclaration"; _ }, _) -> ( *)
    (*             (* headerTypeDeclaration = optAnnotations HEADER name optTypeParameters `{ structFieldList } *) *)
    (*             match header_type.it with *)
    (*             | CaseV (_, [ _; _; name; _; _ ]) -> name *)
    (*             | _ -> failwith "invalid headerTypeDeclaration structure") *)
    (*         | CaseV (_, [ header_union ]), VarT ({ it = "headerUnionDeclaration"; _ }, _) -> ( *)
    (*             (* headerUnionDeclaration = optAnnotations HEADER_UNION name optTypeParameters `{ structFieldList } *) *)
    (*             match header_union.it with *)
    (*             | CaseV (_, [ _; _; name; _; _ ]) -> name *)
    (*             | _ -> failwith "invalid headerUnionDeclaration structure") *)
    (*         | CaseV (_, [ struct_type ]), VarT ({ it = "structTypeDeclaration"; _ }, _) -> ( *)
    (*             (* structTypeDeclaration = optAnnotations STRUCT name optTypeParameters `{ structFieldList } *) *)
    (*             match struct_type.it with *)
    (*             | CaseV (_, [ _; _; name; _; _ ]) -> name *)
    (*             | _ -> failwith "invalid structTypeDeclaration structure") *)
    (*         | CaseV (_, [ enum_type ]), VarT ({ it = "enumDeclaration"; _ }, _) -> ( *)
    (*             (* enumDeclaration = optAnnotations ENUM name `{ identifierList optTrailingComma } *) *)
    (*             match enum_type.it with *)
    (*             | CaseV (_, [ _; _; name; _; _ ]) -> name *)
    (*             | _ -> failwith "invalid enumDeclaration structure") *)
    (*         | _ -> failwith "unknown derivedTypeDeclaration" *)
    (*     ) *)
    (*     | CaseV (_, [ typedef_decl; _ ]) -> ( *)
    (*         (* typedefDeclaration = optAnnotations TYPEDEF typeRef name *) *)
    (*         match typedef_decl.it with *)
    (*         | CaseV (_, [ _; _; name ]) -> name *)
    (*         | _ -> failwith "invalid typedefDeclaration structure" *)
    (*     ) *)
    (*     | CaseV (_, [ parser_type; _ ]) -> ( *)
    (*         (* parserTypeDeclaration = optAnnotations PARSER name optTypeParameters `( parameterList ) *) *)
    (*         match parser_type.it with *)
    (*         | CaseV (_, [ _; _; name; _; _ ]) -> name *)
    (*         | _ -> failwith "invalid parserTypeDeclaration structure" *)
    (*     ) *)
    (*     | CaseV (_, [ control_type; _ ]) -> ( *)
    (*         (* controlTypeDeclaration = optAnnotations CONTROL name optTypeParameters `( parameterList ) *) *)
    (*         match control_type.it with *)
    (*         | CaseV (_, [ _; _; name; _; _ ]) -> name *)
    (*         | _ -> failwith "invalid controlTypeDeclaration structure" *)
    (*     ) *)
    (*     | CaseV (_, [ package_type; _ ]) -> ( *)
    (*         (* packageTypeDeclaration = optAnnotations PACKAGE name optTypeParameters `( parameterList ) *) *)
    (*         match package_type.it with *)
    (*         | CaseV (_, [ _; _; name; _; _ ]) -> name *)
    (*         | _ -> failwith "invalid packageTypeDeclaration structure" *)
    (*     ) *)
    (*     | _ -> failwith "unknown typeDeclaration" *)
    (* ) *)
    (* | _ -> failwith "unknown declaration type" *)
    failwith "not implemented yet"

  let id_of_case_v (v: value) : string =
    match v.it, v.note.typ with
    | CaseV _, VarT (id, _) -> id.it
    | _ -> failwith "not a case value"

  let extract_id_from_name (v: value) : string =
    (* name = nonTypeName | LIST | typeIdentifier *)
    if id_of_case_v v = "name" then (
        match v.it with
        | CaseV (_, [ value ]) when id_of_case_v value = "nonTypeName" -> (
            (* nonTypeName = identifier | APPLY | KEY | ACTIONS | STATE | ENTRIES | TYPE | PRIORITY *)
            match value.it with
            | CaseV (_, [ text_value ]) when id_of_case_v text_value = "identifier" -> (
                match text_value.it with
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
        | _ -> failwith "invalid name structure")
    else failwith "not a name value"

  let has_typ_params_declaration (_d: value) : bool =
    (* TODO: check for type parameters in CaseV *)
    false

  let declare_var_of_il (v: value) (b: bool) : unit =
    let id = extract_id_from_name v in
    declare_var id b
  
  let declare_vars_of_il (vs: value list) : unit =
    List.iter (fun s -> declare_var_of_il s false) vs

  let declare_type_of_il (v: value) (b: bool) : unit =
    let id = extract_id_from_name v in
    declare_type id b

  let declare_types_of_il (vs: value list) : unit =
    List.iter (fun s -> declare_type_of_il s false) vs
%}

(**************************** TOKENS ******************************)

%token<Source.info> END
%token TYPENAME IDENTIFIER
%token<Il.Ast.value> NAME STRING_LITERAL
%token<Il.Ast.value * string> NUMBER
%token<Source.info> LE GE SHL AND OR NE EQ
%token<Source.info> PLUS MINUS PLUS_SAT MINUS_SAT MUL INVALID DIV MOD
%token<Source.info> BIT_OR BIT_AND BIT_XOR COMPLEMENT
%token<Source.info> L_BRACKET R_BRACKET L_BRACE R_BRACE L_ANGLE L_ANGLE_ARGS R_ANGLE R_ANGLE_SHIFT L_PAREN R_PAREN
%token<Source.info> ASSIGN COLON COMMA QUESTION DOT NOT SEMICOLON
%token<Source.info> AT PLUSPLUS
%token<Source.info> DONTCARE
%token<Source.info> MASK DOTS RANGE
%token<Source.info> TRUE FALSE
%token<Source.info> ABSTRACT ACTION ACTIONS APPLY BOOL BIT BREAK CONST CONTINUE CONTROL DEFAULT DEFAULT_ACTION
%token<Source.info> ELSE ENTRIES ENUM ERROR EXIT EXTERN HEADER HEADER_UNION IF IN INOUT
%token<Source.info> INT KEY LIST SELECT MATCH_KIND OUT PACKAGE PARSER PRIORITY RETURN STATE STRING STRUCT
%token<Source.info> SWITCH TABLE THIS TRANSITION TUPLE TYPEDEF TYPE VALUESET VARBIT VOID
%token<Source.info> PRAGMA PRAGMA_END
%token<Il.Ast.value> UNEXPECTED_TOKEN

(**************************** PRIORITY AND ASSOCIATIVITY ******************************)

%right THEN ELSE
%nonassoc QUESTION
%nonassoc COLON
%left OR
%left AND
%left EQ NE
%left L_ANGLE R_ANGLE LE GE
%left BIT_OR
%left BIT_XOR
%left BIT_AND
%left SHL R_ANGLE_SHIFT
%left PLUSPLUS PLUS MINUS PLUS_SAT MINUS_SAT
%left MUL DIV MOD
%right PREFIX
%nonassoc L_PAREN L_BRACKET L_ANGLE_ARGS
%left DOT

%start <Il.Ast.value> p4program

%%

(**************************** CONTEXTS ******************************)

push_scope:
| (* empty *)
    { push_scope() }
;

push_name:
| n = name
   { push_scope();
     declare_type_of_il n false;
     n }

push_externName:
| n = externName
    { push_scope();
      declare_type_of_il n false;
      n }

pop_scope:
| (* empty *)
    { pop_scope() }
;

go_toplevel:
| (* empty *)
    { go_toplevel () }

go_local:
| (* empty *)
    { go_local () }

%inline toplevel(X):
| go_toplevel x = X go_local
    { x }

(**************************** LISTS ******************************)

(* We re-implement right-recursive versions of these standard functions to
   avoid some shift/reduce conflicts *)

separated_nonempty_list_aux(sep, X):
| x = X
    { [x] }
| xs = separated_nonempty_list_aux(sep, X) sep x = X
    { x :: xs }
;

separated_nonempty_list(sep, X):
| rev_list = separated_nonempty_list_aux(sep, X)
    { List.rev rev_list }
;

separated_nonempty_trailing_list(sep, X):
| rev_list = separated_nonempty_list_aux(sep, X) sep
    { List.rev rev_list }

separated_nonempty_opt_trailing_list(sep, X):
| rev_list = separated_nonempty_list_aux(sep, X) option(sep)
    { List.rev rev_list }
;

separated_atLeastTwo_list_aux(sep, X):
| xs = separated_nonempty_list_aux(sep, X) sep x = X
    { x :: xs }
;

separated_atLeastTwo_list(sep, X):
| rev_list = separated_atLeastTwo_list_aux(sep, X)
    { List.rev rev_list }
;

separated_list_aux(sep, X):
| (* empty *)
    { [] }
| x = X
    { [x] }
| xs = separated_list_aux(sep, X) sep x = X
    { x :: xs }
;

separated_list(sep, X):
| rev_list = separated_list_aux(sep, X)
    { List.rev rev_list }
;

separated_opt_trailing_list(sep, X):
| rev_list = separated_list_aux(sep, X) option(sep)
    { List.rev rev_list }
;

nonempty_list_aux(X):
| x = X
    { [x] }
| xs = nonempty_list_aux(X) x=X
    { x :: xs }
;

nonempty_list(X):
| rev_list = nonempty_list_aux(X)
    { List.rev rev_list }
;

list_aux(X):
| (* empty *)
    { [] }
| xs = list_aux(X) x=X
    { x :: xs }
;

list(X):
| rev_list = list_aux(X)
    { List.rev rev_list }
;

%inline option(X):
| (* empty *)
    { None }
| x = X
    { Some x }
;

(* Missing helper functions *)
(* TODO: convert *)
expressionOptTrailingList:
| exprs = separated_opt_trailing_list(COMMA, expression)
    { wrap_list_v exprs "expression" }
;

(* TODO: convert *)
optTrailingComma:
| (* empty *)
    { [ Term "empty" ] |> wrap_case_v |> with_typ (wrap_var_t "optTrailingComma") }
| COMMA
    { [ Term "," ] |> wrap_case_v |> with_typ (wrap_var_t "optTrailingComma") }
;
(* TODO: convert *)
kvList:
| kvs = separated_list(COMMA, kvPair)
    { let typ = wrap_var_t "kvPair" |> wrap_iter_t List in
      ListV kvs |> with_typ typ }
;
(* TODO: convert *)
comma_separated_list(X):
| xs = separated_list(COMMA, X)
    { xs }
;
(**************************** P4-16 GRAMMAR ******************************)

(******** Misc ********)
(** TODO: are these necessary? **)
(* trailingComma: *)
(* optTrailingComma: *)
const:
| CONST
    { [ Term "const" ] |> wrap_case_v |> with_typ (wrap_var_t "const") }
;

optCONST:
| c = option(const)
    { wrap_opt_v c "const" }

(******** Numbers ********)
(* Processed by lexer *)
number:
| number = NUMBER
    { fst number }

(******** Strings ********)
(* Petr4 X / Spec O *)
stringLiteral:
| text = STRING_LITERAL
    { [ NT text; Term "PHTM_2" ] |> wrap_case_v |> with_typ (wrap_var_t "stringLiteral")}

(******** Names ********)

dotPrefix:
| info = DOT
    { info |> ignore;
      [ Term "." ] |> wrap_case_v |> with_typ (wrap_var_t "dotPrefix") }
;

(* Petr4 X / Spec O *)
identifier:
| text = NAME IDENTIFIER
    { [ Term "$"; NT text ] |> wrap_case_v |> with_typ (wrap_var_t "identifier") }
;

(* Petr4 X / Spec O *)
typeIdentifier:
| text = NAME TYPENAME
    { [ Term "@"; NT text ] |> wrap_case_v |> with_typ (wrap_var_t "typeIdentifier") }
;

(* Petr4: nonTypeName + varName + tableKwName *)
nonTypeName:
| identifier = identifier
    { [ NT identifier ] |> wrap_case_v |> with_typ (wrap_var_t "nonTypeName") }
| info = APPLY
    { info |> ignore;
      [ Term "apply" ] |> wrap_case_v |> with_typ (wrap_var_t "nonTypeName") }
| info = KEY
    { info |> ignore;
      [ Term "key" ] |> wrap_case_v |> with_typ (wrap_var_t "nonTypeName") }
| info = ACTIONS
    { info |> ignore;
      [ Term "actions" ] |> wrap_case_v |> with_typ (wrap_var_t "nonTypeName") }
| info = STATE
    { info |> ignore;
      [ Term "state" ] |> wrap_case_v |> with_typ (wrap_var_t "nonTypeName") }
| info = ENTRIES
    { info |> ignore;
      [ Term "entries" ] |> wrap_case_v |> with_typ (wrap_var_t "nonTypeName") }
| info = TYPE
    { info |> ignore;
      [ Term "type" ] |> wrap_case_v |> with_typ (wrap_var_t "nonTypeName") }
| info = PRIORITY
    { info |> ignore;
      [ Term "priority" ] |> wrap_case_v |> with_typ (wrap_var_t "nonTypeName") }
;

prefixedNonTypeName:
| nonTypeName = nonTypeName
    { [ NT nonTypeName ] |> wrap_case_v |> with_typ (wrap_var_t "prefixedNonTypeName") }
| dotPrefix = dotPrefix go_toplevel nonTypeName = nonTypeName go_local
    { [ NT dotPrefix; NT nonTypeName ] |> wrap_case_v |> with_typ (wrap_var_t "prefixedNonTypeName") }
;

(* Petr4: nonTableKwName + tableKwName *)
nonTableKwName:
| nonTypeName = nonTypeName
    { [ NT nonTypeName ] |> wrap_case_v |> with_typ (wrap_var_t "nonTableKwName") }
;

(* Petr4: prefixedType + prefixedTypeName *)
prefixedType:
| typeIdentifier = typeIdentifier
    { [ NT typeIdentifier ] |> wrap_case_v |> with_typ (wrap_var_t "prefixedType") }
| dotPrefix = dotPrefix go_toplevel typeIdentifier = typeIdentifier go_local
    { 
      [ NT dotPrefix; NT typeIdentifier ] |> wrap_case_v |> with_typ (wrap_var_t "prefixedType")
    }
;

typeName:
| prefixedType = prefixedType
    { [ NT prefixedType ] |> wrap_case_v |> with_typ (wrap_var_t "typeName") }
;

name:
| nonTypeName = nonTypeName 
    { [ NT nonTypeName ] |> wrap_case_v |> with_typ (wrap_var_t "name") }
| info = LIST
    { info |> ignore;
      [ Term "list" ] |> wrap_case_v |> with_typ (wrap_var_t "name") }
| typeIdentifier = typeIdentifier
    { [ NT typeIdentifier ] |> wrap_case_v |> with_typ (wrap_var_t "name") }
;

(* TODO: ListV *)
identifierList:
| ids = separated_nonempty_list(COMMA, id = name { id })
    { wrap_list_v ids "identifier" }
;
member:
| name = name
    { [ NT name ] |> wrap_case_v |> with_typ (wrap_var_t "member") }
;
(******** Directions ********)
direction:
| IN
    { [ Term "in" ] |> wrap_case_v |> with_typ (wrap_var_t "direction") }
| OUT
    { [ Term "out" ] |> wrap_case_v |> with_typ (wrap_var_t "direction") }
| INOUT
    { [ Term "inout" ] |> wrap_case_v |> with_typ (wrap_var_t "direction") }
| (* empty *)
    { [ Term "none" ] |> wrap_case_v |> with_typ (wrap_var_t "direction") }


(******** Types ********)
(** TODO **)

baseType:
| info = BOOL
    { info |> ignore;
      [ Term "bool" ] |> wrap_case_v |> with_typ (wrap_var_t "baseType") }
| info = MATCH_KIND
    { info |> ignore;
      [ Term "match_kind" ] |> wrap_case_v |> with_typ (wrap_var_t "baseType") }
| info = ERROR
    { info |> ignore;
      [ Term "error" ] |> wrap_case_v |> with_typ (wrap_var_t "baseType") }
| info = BIT
    { info |> ignore;
      [ Term "bit" ] |> wrap_case_v |> with_typ (wrap_var_t "baseType") }
| info = STRING
    { info |> ignore;
      [ Term "string" ] |> wrap_case_v |> with_typ (wrap_var_t "baseType") }
| info = INT
    { info |> ignore;
      [ Term "int" ] |> wrap_case_v |> with_typ (wrap_var_t "baseType") }
(* TODO: int handling *)
| info1 = BIT l_angle value = number info_r = r_angle
    { let tags = Source.merge info1 info_r in
      tags |> ignore;
      [ Term "bit"; Term "<"; NT value; Term ">" ]
      |> wrap_case_v 
      |> with_typ (wrap_var_t "baseType") }
;

specializedType:
| typeName = typeName l_angle typeArgumentList = typeArgumentList info_r = r_angle
    { info_r |> ignore;
      [ NT typeName; Term "<"; NT typeArgumentList; Term ">" ]
      |> wrap_case_v 
      |> with_typ (wrap_var_t "specializedType") }
;

namedType:
| t = typeName
| t = specializedType
    { [ NT t ] |> wrap_case_v |> with_typ (wrap_var_t "namedType") }
;

headerStackType:
| typeName = typeName L_BRACKET expression = expression info2 = R_BRACKET
    { info2 |> ignore;
      [ NT typeName; Term "["; NT expression; Term "]" ]
      |> wrap_case_v 
      |> with_typ (wrap_var_t "headerStackType") }
| specializedType = specializedType L_BRACKET expression = expression info2 = R_BRACKET
    { info2 |> ignore;
      [ NT specializedType; Term "["; NT expression; Term "]"; Term "PHTM_16" ]
      |> wrap_case_v 
      |> with_typ (wrap_var_t "headerStackType") }
;

listType:
| info1 = LIST l_angle typeArg = typeArg info_r = r_angle
    { let tags = Source.merge info1 info_r in
      tags |> ignore;
      [ Term "list"; Term "<"; NT typeArg; Term ">" ]
      |> wrap_case_v 
      |> with_typ (wrap_var_t "listType") }
;
tupleType:
| info1 = TUPLE l_angle typeArgumentList = typeArgumentList info_r = r_angle
    { let tags = Source.merge info1 info_r in
      tags |> ignore;
      [ Term "tuple"; Term "<"; NT typeArgumentList; Term ">" ]
      |> wrap_case_v 
      |> with_typ (wrap_var_t "tupleType") }
;

(******** Type references ********)

typeRef:
| t = baseType
| t = typeName
(* | t = specializedType *) (* Petr4 O / Spec X *)
| t = headerStackType
| t = listType
| t = tupleType
    { [ NT t ] |> wrap_case_v |> with_typ (wrap_var_t "typeRef") }
;

typeOrVoid:
| typeRef = typeRef
    { [ NT typeRef ] |> wrap_case_v |> with_typ (wrap_var_t "typeOrVoid") }
| info = VOID
    { info |> ignore;
      [ Term "void" ] |> wrap_case_v |> with_typ (wrap_var_t "typeOrVoid") }
;
(* Petr4 O / Spec X *)
(* | name = varName *)
(*   { let tags: P4util.Source.info = Text.tags name in *)
(*     Type.TypeName { tags; name = BareName name } } *)

(******** Type parameters ********)

(* TODO: listify *)
typeParameterList:
| names = separated_nonempty_list(COMMA, name) 
    { declare_types_of_il names;
      wrap_list_v names "typeParameterList" }
;
typeParameters:
| info_l = l_angle type_params = typeParameterList info_r = r_angle
    { let tags = Source.merge info_l info_r in
      tags |> ignore;
      [ Term "<"; NT type_params; Term ">" ]
      |> wrap_case_v 
      |> with_typ (wrap_var_t "typeParameters") }
;
  
optTypeParameters:
| typeParameters = option(typeParameters)
    { wrap_opt_v typeParameters "typeParameters" }
;
(******** Parameters ********)

parameter:
| optAnnotations = optAnnotations direction = direction
    typeRef = typeRef name = name
    { 
      [ NT optAnnotations; NT direction; NT typeRef; NT name ]
      |> wrap_case_v 
      |> with_typ (wrap_var_t "parameter")
    }
  (* TODO: ASSIGN terminal in grammar? *)
| optAnnotations = optAnnotations direction = direction
    typeRef = typeRef name = name init = initialValue
    { 
      [ NT optAnnotations; NT direction; NT typeRef; NT name; NT init ]
      |> wrap_case_v 
      |> with_typ (wrap_var_t "parameter")
    }
;

parameterList:
| params = separated_list(COMMA, parameter)
    { declare_vars_of_il (List.map name_of_declaration params);
      wrap_list_v params "parameter"}
;

constructorParameters:
| L_PAREN parameterList = parameterList R_PAREN
    { [ Term "("; NT parameterList; Term ")" ] |> wrap_case_v |> with_typ (wrap_var_t "constructorParameters") }
;

optConstructorParameters:
| params = option(constructorParameters)
    { wrap_opt_v params "constructorParameters" }
;

(******** Expressions ********)
nonBraceExpression:
| number = number
    { [ NT number ] |> wrap_case_v |> with_typ (wrap_var_t "nonBraceExpression") }
| stringLiteral = stringLiteral
    { [ NT stringLiteral ] |> wrap_case_v |> with_typ (wrap_var_t "nonBraceExpression") }
(* TODO: TRUE / FALSE / THIS *)
(* | info1 = TRUE *)
(*     { Expression.True { tags = info1 } } *)
(* | info1 = FALSE *)
(*     { Expression.False { tags = info1 } } *)
(* | info1 = THIS *)
(*     { let name = Text.{ tags = info1; str = "this" } in *)
(*       let tags = Text.tags name in *)
(*       Expression.Name { tags; name = BareName name } } *)
(* TODO: Petr4 O / Spec X *)
(* | expr = prefixedNonTypeName
    { expr } *)
| array = nonBraceExpression L_BRACKET index = expression info2 = R_BRACKET
    { info2 |> ignore;
      [ NT array; Term "["; NT index; Term "]" ] 
      |> wrap_case_v |> with_typ (wrap_var_t "nonBraceExpression") }
| bits = nonBraceExpression L_BRACKET hi = expression COLON lo = expression info2 = R_BRACKET
    { info2 |> ignore;
      [ NT bits; Term "["; NT hi; Term ":"; NT lo; Term "]" ]
    |> wrap_case_v |> with_typ (wrap_var_t "nonBraceExpression") }
| L_PAREN exp = expression R_PAREN
    { exp }
| info1 = NOT arg = expression %prec PREFIX
    { info1 |> ignore;
  [ Term "!"; NT arg ] |> wrap_case_v |> with_typ (wrap_var_t "nonBraceExpression") }
| info1 = COMPLEMENT arg = expression %prec PREFIX
    { info1 |> ignore;
  [ Term "~"; NT arg ] |> wrap_case_v |> with_typ (wrap_var_t "nonBraceExpression") }
| info1 = MINUS arg = expression %prec PREFIX
    { info1 |> ignore;
  [ Term "-"; NT arg ] |> wrap_case_v |> with_typ (wrap_var_t "nonBraceExpression") }
| info1 = PLUS arg = expression %prec PREFIX
    { info1 |> ignore;
      [ Term "+"; NT arg ] |> wrap_case_v |> with_typ (wrap_var_t "nonBraceExpression") }
(* Petr4: prefixedTypeName *)
| typ = typeName DOT member = member
    { [ NT typ; Term "."; NT member ]
      |> wrap_case_v |> with_typ (wrap_var_t "nonBraceExpression") }
| info1 = ERROR DOT member = member
    { info1 |> ignore;
      [ Term "error"; Term "."; NT member ]
      |> wrap_case_v |> with_typ (wrap_var_t "nonBraceExpression") }
| expr = nonBraceExpression DOT member = member
    { [ NT expr; Term "."; NT member; Term "PHTM_5" ]
      |> wrap_case_v |> with_typ (wrap_var_t "nonBraceExpression") }
(* TODO: binop *)
| arg1 = nonBraceExpression op_str = binop arg2 = expression
    { [ NT arg1; Term op_str; NT arg2 ]
      |> wrap_case_v |> with_typ (wrap_var_t "nonBraceExpression") }
| cond = nonBraceExpression QUESTION true_expr = expression COLON false_expr = expression
    { [ NT cond; Term "?"; NT true_expr; Term ":"; NT false_expr ]
      |> wrap_case_v |> with_typ (wrap_var_t "nonBraceExpression") }
| func = nonBraceExpression l_angle type_args = realTypeArgumentList r_angle
  L_PAREN args = argumentList info2 = R_PAREN
    { info2 |> ignore;
      [ NT func; Term "<"; NT type_args; Term ">"; Term "("; NT args; Term ")" ]
      |> wrap_case_v |> with_typ (wrap_var_t "nonBraceExpression") }
| func = nonBraceExpression L_PAREN args = argumentList info2 = R_PAREN
    { info2 |> ignore;
      [ NT func; Term "("; NT args; Term ")" ]
      |> wrap_case_v |> with_typ (wrap_var_t "nonBraceExpression") }
| typ = namedType L_PAREN args = argumentList info2 = R_PAREN
    { info2 |> ignore;
      [ NT typ; Term "("; NT args; Term ")"; Term "PHTM_6" ]
      |> wrap_case_v |> with_typ (wrap_var_t "nonBraceExpression") }
| info1 = L_PAREN typ = typeRef R_PAREN expr = expression %prec PREFIX
    { info1 |> ignore;
      [ Term "("; NT typ; Term ")"; NT expr ]
      |> wrap_case_v |> with_typ (wrap_var_t "nonBraceExpression") }
;

%inline binop:
| info = MUL { info |> ignore; "*" }
| info = DIV { info |> ignore; "/" }
| info = MOD { info |> ignore; "%" }
| info = PLUS { info |> ignore; "+" }
| info = PLUS_SAT { info |> ignore; "|+|" }
| info = MINUS { info |> ignore; "-" }
| info = MINUS_SAT { info |> ignore; "|-|" }
| info = SHL { info |> ignore; "<<" }
| info_r = r_angle info_2 = R_ANGLE_SHIFT
    { let tags = Source.merge info_r info_2 in
      tags |> ignore; ">>" }
| info = LE { info |> ignore; "<=" }
| info = GE { info |> ignore; ">=" }
| info = l_angle { info |> ignore; "<" }
| info_r = r_angle { info_r |> ignore; ">" }
| info = NE { info |> ignore; "!=" }
| info = EQ { info |> ignore; "==" }
| info = BIT_AND { info |> ignore; "&" }
| info = BIT_XOR { info |> ignore; "^" }
| info = BIT_OR { info |> ignore; "|" }
| info = PLUSPLUS { info |> ignore; "++" }
| info = AND { info |> ignore; "&&" }
| info = OR { info |> ignore; "||" }
;

(* TODO: convert *)
expression:
| num = number
    { [ NT num ] |> wrap_case_v |> with_typ (wrap_var_t "expression") }
| info1 = DOTS
    { info1 |> ignore;
      [ Term "..." ] |> wrap_case_v |> with_typ (wrap_var_t "expression") }
| info1 = TRUE
    { info1 |> ignore;
      [ Term "true" ] |> wrap_case_v |> with_typ (wrap_var_t "expression") }
| info1 = FALSE
    { info1 |> ignore;
      [ Term "false" ] |> wrap_case_v |> with_typ (wrap_var_t "expression") }
| value = STRING_LITERAL
    { [ NT value ] |> wrap_case_v |> with_typ (wrap_var_t "expression") }
| info1 = THIS
    { info1 |> ignore;
      [ Term "this" ] |> wrap_case_v |> with_typ (wrap_var_t "expression") }
| name = nonTypeName
    { [ NT name ] |> wrap_case_v |> with_typ (wrap_var_t "expression") }
| info1 = dotPrefix go_toplevel name = nonTypeName go_local
    { [ NT info1; NT name ] |> wrap_case_v |> with_typ (wrap_var_t "expression") }
| array = expression L_BRACKET index = expression info2 = R_BRACKET
    { info2 |> ignore;
      [ NT array; Term "["; NT index; Term "]" ] |> wrap_case_v |> with_typ (wrap_var_t "expression") }
| bits = expression L_BRACKET hi = expression COLON lo = expression info2 = R_BRACKET
    { info2 |> ignore;
      [ NT bits; Term "["; NT hi; Term ":"; NT lo; Term "]" ] |> wrap_case_v |> with_typ (wrap_var_t "expression") }
(* List expressions *)
| info1 = L_BRACE values = expressionOptTrailingList info2 = R_BRACE
    { let tags = Source.merge info1 info2 in
      tags |> ignore;
      [ Term "{"; NT values; Term "}" ] |> wrap_case_v |> with_typ (wrap_var_t "expression") }
| info = INVALID
    { info |> ignore;
      [ Term "invalid" ] |> wrap_case_v |> with_typ (wrap_var_t "expression") }
| info1 = L_BRACE entries = kvOptTrailingList info2 = R_BRACE 
    { let tags = Source.merge info1 info2 in
      tags |> ignore;
      [ Term "{"; NT entries; Term "}" ] |> wrap_case_v |> with_typ (wrap_var_t "expression") }
| info1 = L_BRACE entries = kvTrailingList DOTS option(COMMA) info2 = R_BRACE
    { let tags = Source.merge info1 info2 in
      tags |> ignore;
      [ Term "{"; NT entries; Term "..."; Term "}" ] |> wrap_case_v |> with_typ (wrap_var_t "expression") }
| L_PAREN exp = expression R_PAREN
    { exp }
| info1 = NOT arg = expression %prec PREFIX
    { info1 |> ignore;
      [ Term "!"; NT arg ] |> wrap_case_v |> with_typ (wrap_var_t "expression") }
| info1 = COMPLEMENT arg = expression %prec PREFIX
    { info1 |> ignore;
      [ Term "~"; NT arg ] |> wrap_case_v |> with_typ (wrap_var_t "expression") }
| info1 = MINUS arg = expression %prec PREFIX
    { info1 |> ignore;
      [ Term "-"; NT arg ] |> wrap_case_v |> with_typ (wrap_var_t "expression") }
| info1 = PLUS arg = expression %prec PREFIX
    { info1 |> ignore;
      [ Term "+"; NT arg ] |> wrap_case_v |> with_typ (wrap_var_t "expression") }
| info1 = L_PAREN typ = typeRef R_PAREN expr = expression %prec PREFIX
    { info1 |> ignore;
      [ Term "("; NT typ; Term ")"; NT expr ] |> wrap_case_v |> with_typ (wrap_var_t "expression") }
(* Petr4: prefixedTypeName *)
| typ = typeName DOT name = member
    { [ NT typ; Term "."; NT name ] |> wrap_case_v |> with_typ (wrap_var_t "expression") }
| info1 = ERROR DOT name = member
    { info1 |> ignore;
      [ Term "error"; Term "."; NT name ] |> wrap_case_v |> with_typ (wrap_var_t "expression") }
| expr = expression DOT name = member
    { [ NT expr; Term "."; NT name ] |> wrap_case_v |> with_typ (wrap_var_t "expression") }
| arg1 = expression op = binop arg2 = expression
    { [ NT arg1; Term op; NT arg2 ] |> wrap_case_v |> with_typ (wrap_var_t "expression") }
| cond = expression QUESTION tru = expression COLON fls = expression
    { [ NT cond; Term "?"; NT tru; Term ":"; NT fls ] |> wrap_case_v |> with_typ (wrap_var_t "expression") }
| func = expression l_angle type_args = realTypeArgumentList r_angle
  L_PAREN args = argumentList info2 = R_PAREN
    { info2 |> ignore;
      [ NT func; Term "<"; NT type_args; Term ">"; Term "("; NT args; Term ")" ] |> wrap_case_v |> with_typ (wrap_var_t "expression") }
| func = expression L_PAREN args = argumentList info2 = R_PAREN
    { info2 |> ignore;
      [ NT func; Term "("; NT args; Term ")" ] |> wrap_case_v |> with_typ (wrap_var_t "expression") }
| typ = namedType L_PAREN args = argumentList info2 = R_PAREN
    { info2 |> ignore;
      [ NT typ; Term "("; NT args; Term ")" ] |> wrap_case_v |> with_typ (wrap_var_t "expression") }
;

(* TODO: convert *)
expressionList:
| exprs = separated_list(COMMA, expression)
    { let typ = wrap_var_t "expression" |> wrap_iter_t List in
      ListV exprs |> with_typ typ }
;

simpleKeysetExpression:
| expr = expression
    { [ NT expr ] |> wrap_case_v |> with_typ (wrap_var_t "simpleKeysetExpression") }
| expr = expression MASK mask = expression
    { [ NT expr; Term "&&&"; NT mask ] |> wrap_case_v |> with_typ (wrap_var_t "simpleKeysetExpression") }
| lo = expression RANGE hi = expression
    { [ NT lo; Term ".."; NT hi ] |> wrap_case_v |> with_typ (wrap_var_t "simpleKeysetExpression") }
| info = DONTCARE
    { info |> ignore;
      [ Term "_" ] |> wrap_case_v |> with_typ (wrap_var_t "simpleKeysetExpression") }
| info = DEFAULT
    { info |> ignore;
      [ Term "default" ] |> wrap_case_v |> with_typ (wrap_var_t "simpleKeysetExpression") }
;

reducedSimpleKeysetExpression:
| expr = expression MASK mask = expression
    { [ NT expr; Term "&&&"; NT mask ] |> wrap_case_v |> with_typ (wrap_var_t "reducedSimpleKeysetExpression") }
| lo = expression RANGE hi = expression
    { [ NT lo; Term ".."; NT hi ] |> wrap_case_v |> with_typ (wrap_var_t "reducedSimpleKeysetExpression") }
| info = DONTCARE
    { info |> ignore;
      [ Term "_" ] |> wrap_case_v |> with_typ (wrap_var_t "reducedSimpleKeysetExpression") }
| info = DEFAULT
    { info |> ignore;
      [ Term "default" ] |> wrap_case_v |> with_typ (wrap_var_t "reducedSimpleKeysetExpression") }
;

(* TODO: convert *)
(* simpleExressionList: *)

(* TODO: convert *)
tupleKeysetExpression:
| L_PAREN exprs = separated_atLeastTwo_list(COMMA, simpleKeysetExpression) R_PAREN
    { let typ = wrap_var_t "simpleKeysetExpression" |> wrap_iter_t List in
      ListV exprs |> with_typ typ }
| L_PAREN expr = reducedSimpleKeysetExpression R_PAREN
    { let typ = wrap_var_t "reducedSimpleKeysetExpression" |> wrap_iter_t List in
      ListV [expr] |> with_typ typ }
;

(* TODO: convert *)
keysetExpression:
| exprs = tupleKeysetExpression
    { exprs }
| expr = simpleKeysetExpression
    { let typ = wrap_var_t "simpleKeysetExpression" |> wrap_iter_t List in
      ListV [expr] |> with_typ typ }
;

(* TODO: convert *)
%inline kvPair:
| key = name ASSIGN value = expression 
    { [ NT key; Term "="; NT value ] |> wrap_case_v |> with_typ (wrap_var_t "kvPair") }
;

kvTrailingList:
| kvs = separated_nonempty_trailing_list(COMMA, kvPair)
    { let typ = wrap_var_t "kvPair" |> wrap_iter_t List in
      ListV kvs |> with_typ typ }
;

kvOptTrailingList:
| kvs = separated_nonempty_opt_trailing_list(COMMA, kvPair)
    { let typ = wrap_var_t "kvPair" |> wrap_iter_t List in
      ListV kvs |> with_typ typ }
;

(******** Type arguments ********)

realTypeArg:
| typeRef = typeRef
    { [ NT typeRef ] |> wrap_case_v |> with_typ (wrap_var_t "realTypeArg") }
| info = VOID
    { info |> ignore;
      [ Term "void" ] |> wrap_case_v |> with_typ (wrap_var_t "realTypeArg") }
| info = DONTCARE
    { info |> ignore;
      [ Term "_" ] |> wrap_case_v |> with_typ (wrap_var_t "realTypeArg") }
;

(* TODO: is this special case necessary? *)
realTypeArgumentList:
| ts = separated_list(COMMA, realTypeArg)
    { wrap_list_v ts "realTypeArg" }
(* | t = realTypeArg *)
(*     { [ NT t ] |> wrap_case_v |> with_typ (wrap_var_t "realTypeArgumentList") } *)
(* | t = realTypeArg COMMA ts = separated_list(COMMA, typeArg) *)
(*     { [ NT t; Term ","; NT ts ] |> wrap_case_v |> with_typ (wrap_var_t "realTypeArgumentList") } *)
;

typeArg:
| typeRef = typeRef
    { [ NT typeRef ] |> wrap_case_v |> with_typ (wrap_var_t "typeArg") }
| nonTypeName = nonTypeName
    { [ NT nonTypeName ] |> wrap_case_v |> with_typ (wrap_var_t "typeArg") }
| info = VOID
    { info |> ignore;
      [ Term "void" ] |> wrap_case_v |> with_typ (wrap_var_t "typeArg") }
| info = DONTCARE
    { info |> ignore;
      [ Term "_" ] |> wrap_case_v |> with_typ (wrap_var_t "typeArg") }
;

typeArgumentList:
| ts = separated_list(COMMA, typeArg)
    { let typ = wrap_var_t "typeArg" |> wrap_iter_t List in
      ListV ts |> with_typ typ }
;

(******** Arguments ********)

argument:
| expression = expression
    { [ NT expression ] |> wrap_case_v |> with_typ (wrap_var_t "argument") }
| name = name ASSIGN expression = expression
    { [ NT name; Term "="; NT expression ] |> wrap_case_v |> with_typ (wrap_var_t "argument") }
| info = DONTCARE
    { info |> ignore;
      [ Term "_" ] |> wrap_case_v |> with_typ (wrap_var_t "argument") }
| name = name ASSIGN info = DONTCARE
    { info |> ignore;
      [ NT name; Term "="; Term "_" ] |> wrap_case_v |> with_typ (wrap_var_t "argument") }
;
argumentList: 
| args = separated_list(COMMA, argument)
    { let typ = wrap_var_t "argument" |> wrap_iter_t List in
      ListV args |> with_typ typ }
;
(******** L-values ********)
(** TODO **)

lvalue:
| prefixedNonTypeName = prefixedNonTypeName
    { [ NT prefixedNonTypeName ] |> wrap_case_v |> with_typ (wrap_var_t "lvalue") }
| info = THIS
    { info |> ignore;
      [ Term "this" ] |> wrap_case_v |> with_typ (wrap_var_t "lvalue") }
| expr = lvalue DOT name = member
    { [ NT expr; Term "."; NT name ] |> wrap_case_v |> with_typ (wrap_var_t "lvalue") }
| array = lvalue L_BRACKET index = expression info2 = R_BRACKET
    { info2 |> ignore;
      [ NT array; Term "["; NT index; Term "]" ] |> wrap_case_v |> with_typ (wrap_var_t "lvalue") }
| bits = lvalue L_BRACKET hi = expression COLON lo = expression info2 = R_BRACKET
    { info2 |> ignore;
      [ NT bits; Term "["; NT hi; Term ":"; NT lo; Term "]" ] |> wrap_case_v |> with_typ (wrap_var_t "lvalue") }
| L_PAREN expr = lvalue R_PAREN
    { [ Term "("; NT expr; Term ")" ] |> wrap_case_v |> with_typ (wrap_var_t "lvalue") }
;

(******** Variable and constant declarations ********)

(* initializer -> initialValue due to reserved word in OCaml *)
initialValue:
| ASSIGN expression = expression
    { [ Term "="; NT expression ] |> wrap_case_v |> with_typ (wrap_var_t "initializer") }
;

optInitializer: 
| i = option(initialValue) 
    { wrap_opt_v i "initializer" }
;
(* Pert4 X / Spec O *)
variableDeclarationWithoutSemicolon:
| optAnnotations = optAnnotations
  typeRef = typeRef
  name = name
  optInitializer = optInitializer
    { declare_var_of_il name false;
      [ NT optAnnotations; NT typeRef; NT name; NT optInitializer ]
      |> wrap_case_v |> with_typ (wrap_var_t "variableDeclarationWithoutSemicolon") }
;

variableDeclaration:
| variableDeclarationWithoutSemicolon = variableDeclarationWithoutSemicolon info2 = SEMICOLON
    { info2 |> ignore;
      [ NT variableDeclarationWithoutSemicolon; Term ";" ]
      |> wrap_case_v |> with_typ (wrap_var_t "variableDeclaration") }
;

constantDeclaration:
| optAnnotations = optAnnotations
  info1 = CONST
  typeRef = typeRef
  name = name
  init = initialValue
  info2 = SEMICOLON
    { let tags = Source.merge info1 info2 in
      tags |> ignore;
      [ NT optAnnotations; Term "const"; NT typeRef; NT name; NT init; Term ";" ] 
      |> wrap_case_v |> with_typ (wrap_var_t "constantDeclaration") }
;

(******** Statements ********)

(* Petr4 X / Spec O *)
assignmentOrMethodCallStatementWithoutSemicolon:
| func = lvalue L_PAREN args = argumentList R_PAREN
    { [ NT func; Term "("; NT args; Term ")" ] |> wrap_case_v |> with_typ (wrap_var_t "assignmentOrMethodCallStatementWithoutSemicolon") }
| func = lvalue l_angle type_args = typeArgumentList r_angle
    L_PAREN args = argumentList R_PAREN
    { [ NT func; Term "<"; NT type_args; Term ">"; Term "("; NT args; Term ")" ] 
      |> wrap_case_v |> with_typ (wrap_var_t "assignmentOrMethodCallStatementWithoutSemicolon") }
(* TODO: disambiguate different assignment operators *)
| lhs = lvalue ASSIGN rhs = expression
    { [ NT lhs; Term "="; NT rhs ] |> wrap_case_v |> with_typ (wrap_var_t "assignmentOrMethodCallStatementWithoutSemicolon") }
;

assignmentOrMethodCallStatement:
| assignmentOrMethodCallStatementWithoutSemicolon = assignmentOrMethodCallStatementWithoutSemicolon info2 = SEMICOLON
    { info2 |> ignore;
      [ NT assignmentOrMethodCallStatementWithoutSemicolon; Term ";" ]
      |> wrap_case_v |> with_typ (wrap_var_t "assignmentOrMethodCallStatement") }
;

directApplication:
| namedType = namedType DOT APPLY (* Differs from Petr4 *)
  L_PAREN args = argumentList R_PAREN info2 = SEMICOLON
    { info2 |> ignore;
      [ NT namedType; Term "."; Term "APPLY"; Term "("; NT args; Term ")"; Term ";" ]
      |> wrap_case_v |> with_typ (wrap_var_t "directApplication") }
;

conditionalStatement:
| info1 = IF L_PAREN cond = expression R_PAREN tru = statement %prec THEN
    { info1 |> ignore;
      [ Term "if"; Term "("; NT cond; Term ")"; NT tru ]
      |> wrap_case_v |> with_typ (wrap_var_t "conditionalStatement") }
| info1 = IF L_PAREN cond = expression R_PAREN tru = statement ELSE fls = statement
    { info1 |> ignore;
      [ Term "if"; Term "("; NT cond; Term ")"; NT tru; Term "else"; NT fls ]
      |> wrap_case_v |> with_typ (wrap_var_t "conditionalStatement") }
;

(* TODO: convert *)
emptyStatement:
| info = SEMICOLON
    { info |> ignore;
      [ Term ";" ] |> wrap_case_v |> with_typ (wrap_var_t "emptyStatement") }
;

blockStatement:
| annotations = optAnnotations
  info1 = L_BRACE
  push_scope
  statements = statOrDeclList info2 = R_BRACE
  pop_scope
    { let tags = Source.merge info1 info2 in 
      tags |> ignore;
      [ NT annotations; Term "{"; NT statements; Term "}" ]
      |> wrap_case_v |> with_typ (wrap_var_t "blockStatement") }
;

returnStatement:
| info1 = RETURN info2 = SEMICOLON
    { let tags = Source.merge info1 info2 in 
      tags |> ignore;
      [ Term "return"; Term ";" ] |> wrap_case_v |> with_typ (wrap_var_t "returnStatement") }
| info1 = RETURN expr = expression info2 = SEMICOLON
    { let tags = Source.merge info1 info2 in
      tags |> ignore;
      [ Term "return"; NT expr; Term ";" ] |> wrap_case_v |> with_typ (wrap_var_t "returnStatement") }
;

(* Petr4 X / Spec O *)
breakStatement:
| info1 = BREAK info2 = SEMICOLON
    { let tags = Source.merge info1 info2 in
      tags |> ignore;
      [ Term "break"; Term ";" ] |> wrap_case_v |> with_typ (wrap_var_t "breakStatement") }
;

(* Petr4 X / Spec O *)
continueStatement:
| info1 = CONTINUE info2 = SEMICOLON
    { let tags = Source.merge info1 info2 in
      tags |> ignore;
      [ Term "continue"; Term ";" ] |> wrap_case_v |> with_typ (wrap_var_t "continueStatement") }
;

exitStatement:
| info1 = EXIT info2 = SEMICOLON
    { let tags = Source.merge info1 info2 in
      tags |> ignore;
      [ Term "exit"; Term ";" ] |> wrap_case_v |> with_typ (wrap_var_t "exitStatement") }
;


switchLabel:
| info = DEFAULT
    { info |> ignore;
      [ Term "default" ] |> wrap_case_v |> with_typ (wrap_var_t "switchLabel") }
| expr = nonBraceExpression
    { [ NT expr ] |> wrap_case_v |> with_typ (wrap_var_t "switchLabel") }
;

switchCase:
| label = switchLabel COLON code = blockStatement
    { [ NT label; Term ":"; NT code ] |> wrap_case_v |> with_typ (wrap_var_t "switchCase") }
| label = switchLabel info2 = COLON
    { info2 |> ignore;
      [ NT label; Term ":" ] |> wrap_case_v |> with_typ (wrap_var_t "switchCase") }
;

switchCases: 
| cases = list(switchCase) 
    { let typ = wrap_var_t "switchCase" |> wrap_iter_t List in
      ListV cases |> with_typ typ }
;

(* TODO: convert *)
switchStatement:
| info1 = SWITCH
  L_PAREN expr = expression R_PAREN
  L_BRACE cases = switchCases info2 = R_BRACE
    { let tags = Source.merge info1 info2 in
      tags |> ignore;
      [ Term "switch"; Term "("; NT expr; Term ")"; Term "{"; NT cases; Term "}" ]
      |> wrap_case_v |> with_typ (wrap_var_t "switchStatement") }
;

(* (* Petr4 X / Spec O whole for statement missing *) *)
(* declOrAssignmentOrMethodCallStatement: *)
(* | variableDeclarationWithoutSemicolon = variableDeclarationWithoutSemicolon *)
(*     { [ NT variableDeclarationWithoutSemicolon ] *)
(*       |> wrap_case_v |> with_typ (wrap_var_t "declOrAssignmentOrMethodCallStatement") } *)
(* | assignmentOrMethodCallStatementWithoutSemicolon = assignmentOrMethodCallStatementWithoutSemicolon *)
(*     { [ NT assignmentOrMethodCallStatementWithoutSemicolon ] *)
(*       |> wrap_case_v |> with_typ (wrap_var_t "declOrAssignmentOrMethodCallStatement") } *)
(* ; *)
(**)
(* forInitStatementNonEmpty: *)
(* | ls = separated_nonempty_list(COMMA, declOrAssignmentOrMethodCallStatement) *)
(*     { let typ = wrap_var_t "declOrAssignmentOrMethodCallStatement" |> wrap_iter_t List in *)
(*       ListV ls |> with_typ typ } *)
(*   ; *)
(* forInitStatements: *)
(* | ls = separated_list(COMMA, declOrAssignmentOrMethodCallStatement) *)
(*     { let typ = wrap_var_t "declOrAssignmentOrMethodCallStatement" |> wrap_iter_t List in *)
(*       ListV ls |> with_typ typ } *)
(*   ; *)
(* forUpdateStatementsNonEmpty: *)
(* | assignments = separated_nonempty_list(COMMA, assignmentOrMethodCallStatement) *)
(*     { let typ = wrap_var_t "assignmentOrMethodCallStatement" |> wrap_iter_t List in *)
(*       ListV assignments |> with_typ typ } *)
(* ; *)
(**)
(* forUpdateStatements: *)
(* | assignments = separated_list(COMMA, assignmentOrMethodCallStatement) *)
(*     { let typ = wrap_var_t "assignmentOrMethodCallStatement" |> wrap_iter_t List in *)
(*       ListV assignments |> with_typ typ } *)
(**)
(* forCollectionExpr: *)
(* | expr = expression *)
(*     { [ NT expr ] |> wrap_case_v |> with_typ (wrap_var_t "forCollectionExpr") } *)
(* | expr_l = expression DOTDOT expr_r = expression *)
(*     { [ NT expr_l; Term ".."; NT expr_r ] *)
(*       |> wrap_case_v |> with_typ (wrap_var_t "forCollectionExpr") } *)
(* ; *)
(**)
(* forStatement: *)
(* | anno = optAnnotations *)
(*   info1 = FOR *)
(*   L_PAREN init = forInitStatements SEMICOLON cond = expression SEMICOLON update = forUpdateStatements R_PAREN *)
(*   body = statement *)
(*     { [ NT anno; Term "for"; Term "("; NT init; Term ";"; NT cond; Term ";"; NT update; Term ")"; NT body ] *)
(*       |> wrap_case_v |> with_typ (wrap_var_t "forStatement") } *)
(* | anno = optAnnotations info1 = FOR L_PAREN *)
(*     typ = typeRef name = name IN collection = forCollectionExpr R_PAREN body = statement *)
(*     { [ NT anno; Term "for"; Term "("; NT typ; NT name; Term "in"; NT collection; Term ")"; NT body ] *)
(*       |> wrap_case_v |> with_typ (wrap_var_t "forStatement") } *)
(* | anno = optAnnotations info1 = FOR L_PAREN *)
(*     anno_in = optAnnotations typ = typeRef name = name IN  *)
(*     collection = forCollectionExpr R_PAREN body = statement *)
(*     { [ NT anno; Term "for"; Term "("; NT anno_in; NT typ; NT name; Term "in"; NT collection; Term ")"; NT body ] *)
(*       |> wrap_case_v |> with_typ (wrap_var_t "forStatement") } *)
(* ; *)

statement:
| stmt = assignmentOrMethodCallStatement
| stmt = directApplication
| stmt = conditionalStatement
| stmt = emptyStatement
| stmt = blockStatement
| stmt = returnStatement
| stmt = breakStatement
| stmt = continueStatement
| stmt = exitStatement
| stmt = switchStatement
(* | stmt = forStatement *)
    { [ NT stmt ] |> wrap_case_v |> with_typ (wrap_var_t "statement") }
;

statementOrDeclaration:
| variableDeclaration = variableDeclaration
    { [ NT variableDeclaration ] |> wrap_case_v |> with_typ (wrap_var_t "statementOrDeclaration") }
| constantDeclaration = constantDeclaration
    { [ NT constantDeclaration ] |> wrap_case_v |> with_typ (wrap_var_t "statementOrDeclaration") }
| statement = statement
    { [ NT statement ] |> wrap_case_v |> with_typ (wrap_var_t "statementOrDeclaration") }
;

statOrDeclList:
| s = list(statementOrDeclaration)
    { let typ = wrap_var_t "statementOrDeclaration" |> wrap_iter_t List in
      ListV (List.map (fun x -> x) s) |> with_typ typ }


(******** Error and match kind declarations ********)

matchKindDeclaration:
| info1 = MATCH_KIND L_BRACE ids = identifierList comma = optTrailingComma info2 = R_BRACE
    { let tags = Source.merge info1 info2 in
      tags |> ignore;
      [ Term "match_kind"; Term "{"; NT ids; NT comma; Term "}" ] |> wrap_case_v |> with_typ (wrap_var_t "matchKindDeclaration") }
;

errorDeclaration:
| info1 = ERROR L_BRACE ids = identifierList info2 = R_BRACE
    { let tags = Source.merge info1 info2 in
      tags |> ignore;
      [ Term "error"; Term "{"; NT ids; Term "}" ] |> wrap_case_v |> with_typ (wrap_var_t "errorDeclaration") }
;

(******** Extern declarations ********)

functionPrototype:
| typeOrVoid = typeOrVoid name = name
    push_scope
    optTypeParameters = optTypeParameters
    L_PAREN params = parameterList info2 = R_PAREN
    { info2 |> ignore;
      [ NT typeOrVoid; NT name; NT optTypeParameters; Term "("; NT params; Term ")" ]
      |> wrap_case_v |> with_typ (wrap_var_t "functionPrototype") }
;

methodPrototype:
| anno = optAnnotations proto = functionPrototype pop_scope info2 = SEMICOLON
    { info2 |> ignore;
      [ NT anno; NT proto; Term ";" ] |> wrap_case_v |> with_typ (wrap_var_t "methodPrototype") }
| anno = optAnnotations info1 = ABSTRACT proto = functionPrototype
    pop_scope info2 = SEMICOLON
    { let tags = Source.merge info1 info2 in
      tags |> ignore;
      [ NT anno; Term "abstract"; NT proto; Term ";" ] |> wrap_case_v |> with_typ (wrap_var_t "methodPrototype") }
(* Petr4: alias methodName in place of typeIdentifier *)
| anno = optAnnotations tid = typeIdentifier L_PAREN params = parameterList info2 = R_PAREN
    { info2 |> ignore;
      [ NT anno; NT tid; Term "("; NT params; Term ")"; Term ";" ] |> wrap_case_v |> with_typ (wrap_var_t "methodPrototype") }
;

methodPrototypes:
| protos = list(methodPrototype)
    { let typ = wrap_var_t "methodPrototype" |> wrap_iter_t List in
      ListV protos |> with_typ typ }
;

externDeclaration:
(* TODO: nonTypeName -> invoke push_externName *)
| anno = optAnnotations info1 = EXTERN name = nonTypeName type_params = optTypeParameters
    L_BRACE protos = methodPrototypes info2 = R_BRACE pop_scope
    { let tags = Source.merge info1 info2 in
      tags |> ignore;
      [ NT anno; Term "extern"; NT name; NT type_params; Term "{"; NT protos; Term "}" ]
      |> wrap_case_v |> with_typ (wrap_var_t "externDeclaration") }
| anno = optAnnotations info1 = EXTERN proto = functionPrototype pop_scope info2 = SEMICOLON
    { let tags = Source.merge info1 info2 in
      tags |> ignore;
      [ NT anno; Term "extern"; NT proto; Term ";" ] |> wrap_case_v |> with_typ (wrap_var_t "externDeclaration") }
;

(* Auxiliary from Petr4 for push_externName *)
externName:
| n = nonTypeName
    { declare_type_of_il n false; n}

(******** Function declarations ********)

functionDeclaration:
| optAnnotations = optAnnotations functionPrototype = functionPrototype body = blockStatement pop_scope
    { [ NT optAnnotations; NT functionPrototype; NT body ] |> wrap_case_v |> with_typ (wrap_var_t "functionDeclaration") }
;

(******** Instantiations ********)

(* Petr4: ASSIGN kept outside *)
objInitializer:
| ASSIGN L_BRACE decls = objDeclarations info2 = R_BRACE
    { info2 |> ignore;
      [ Term "="; Term "{"; NT decls; Term "}" ] |> wrap_case_v |> with_typ (wrap_var_t "objInitializer") }
;

instantiation:
| anno = optAnnotations typ = typeRef L_PAREN args = argumentList R_PAREN name = name info2 = SEMICOLON
    { info2 |> ignore;
      [ NT anno; NT typ; Term "("; NT args; Term ")"; NT name; Term ";" ]
      |> wrap_case_v |> with_typ (wrap_var_t "instantiation") }
| anno = optAnnotations typ = typeRef L_PAREN args = argumentList R_PAREN name = name init = objInitializer info2 = SEMICOLON
    { info2 |> ignore;
      [ NT anno; NT typ; Term "("; NT args; Term ")"; NT name; NT init; Term ";" ]
      |> wrap_case_v |> with_typ (wrap_var_t "instantiation") }
;

objDeclaration:
| functionDeclaration = functionDeclaration
    { [ NT functionDeclaration ] |> wrap_case_v |> with_typ (wrap_var_t "objDeclaration") }
| instantiation = instantiation
    { [ NT instantiation ] |> wrap_case_v |> with_typ (wrap_var_t "objDeclaration") }
;

objDeclarations:
| decls = list(objDeclaration)
    { let typ = wrap_var_t "objDeclaration" |> wrap_iter_t List in
      ListV decls |> with_typ typ }
;

(******** Action declarations ********)

actionDeclaration:
| anno = optAnnotations info1 = ACTION name = name L_PAREN params = parameterList R_PAREN body = blockStatement
    { info1 |> ignore;
      [ NT anno; Term "action"; NT name; Term "("; NT params; Term ")"; NT body ]
      |> wrap_case_v |> with_typ (wrap_var_t "actionDeclaration") }
;

(******** Table declarations ********)

keyElement:
| key = expression COLON match_kind = name anno = optAnnotations info2 = SEMICOLON
    { info2 |> ignore;
      [ NT key; Term ":"; NT match_kind; NT anno; Term ";" ] |> wrap_case_v |> with_typ (wrap_var_t "keyElement") }
;

keyElementList:
| elements = list(keyElement)
    { let typ = wrap_var_t "keyElement" |> wrap_iter_t List in
      ListV elements |> with_typ typ }
;

(* Petr4: contains optAnnotations, name = name *)
actionRef:
| name = prefixedNonTypeName
    { [ NT name ] |> wrap_case_v |> with_typ (wrap_var_t "actionRef") }
| name = prefixedNonTypeName L_PAREN args = argumentList R_PAREN
    { [ NT name; Term "("; NT args; Term ")" ] |> wrap_case_v |> with_typ (wrap_var_t "actionRef") }
;

(* Petr4 X / Spec O *)
action:
| optAnnotations = optAnnotations actionRef = actionRef info2 = SEMICOLON
    { info2 |> ignore;
      [ NT optAnnotations; NT actionRef; Term ";" ] |> wrap_case_v |> with_typ (wrap_var_t "action") }
;

actionList:
| actions = separated_list_aux(SEMICOLON, action)
    { wrap_list_v actions "action" }
;

entryPriority:
| PRIORITY ASSIGN num = number COLON
    { [ Term "priority"; Term "="; NT num; Term ":" ] |> wrap_case_v |> with_typ (wrap_var_t "entryPriority") }
| PRIORITY ASSIGN L_PAREN expr = expression R_PAREN COLON
    { [ Term "priority"; Term "="; Term "("; NT expr; Term ")"; Term ":" ] |> wrap_case_v |> with_typ (wrap_var_t "entryPriority") }
;

entry:
| optConst = optCONST prio = entryPriority keyset = keysetExpression COLON actionRef = actionRef anno = optAnnotations info2 = SEMICOLON
    { info2 |> ignore;
      [ NT optConst; NT prio; NT keyset; Term ":"; NT actionRef; NT anno; Term ";" ] |> wrap_case_v |> with_typ (wrap_var_t "entry") }
| optConst = optCONST keyset = keysetExpression COLON actions = actionList anno = optAnnotations info2 = SEMICOLON
    { info2 |> ignore;
      [ NT optConst; NT keyset; Term ":"; NT actions; NT anno; Term ";" ] |> wrap_case_v |> with_typ (wrap_var_t "entry") }
;

entriesList:
| entries = list(entry)
    { let typ = wrap_var_t "entry" |> wrap_iter_t List in
      ListV entries |> with_typ typ }
;

tableProperty:
| info1 = KEY ASSIGN L_BRACE keys = keyElementList info2 = R_BRACE
    { let tags = Source.merge info1 info2 in
      tags |> ignore;
      [ Term "key"; Term "="; Term "{"; NT keys; Term "}" ] |> wrap_case_v |> with_typ (wrap_var_t "tableProperty") }
| info1 = ACTIONS ASSIGN L_BRACE actions = actionList info2 = R_BRACE
    { let tags = Source.merge info1 info2 in
      tags |> ignore;
      [ Term "actions"; Term "="; Term "{"; NT actions; Term "}" ] |> wrap_case_v |> with_typ (wrap_var_t "tableProperty") }
| anno = optAnnotations optConst = optCONST info1 = ENTRIES ASSIGN L_BRACE entries = entriesList info2 = R_BRACE
    { let tags = Source.merge info1 info2 in
      tags |> ignore;
      [ NT anno; NT optConst; Term "entries"; Term "="; Term "{"; NT entries; Term "}" ] |> wrap_case_v |> with_typ (wrap_var_t "tableProperty") }
| anno = optAnnotations optConst = optCONST name = nonTableKwName init = initialValue info2 = SEMICOLON
    { info2 |> ignore;
      [ NT anno; NT optConst; NT name; NT init; Term ";" ] |> wrap_case_v |> with_typ (wrap_var_t "tableProperty") }
;

tablePropertyList:
| properties = list(tableProperty)
    { let typ = wrap_var_t "tableProperty" |> wrap_iter_t List in
      ListV properties |> with_typ typ }
;

tableDeclaration:
| anno = optAnnotations info1 = TABLE name = name L_BRACE props = tablePropertyList info2 = R_BRACE
    { let tags = Source.merge info1 info2 in
      tags |> ignore;
      [ NT anno; Term "table"; NT name; Term "{"; NT props; Term "}" ] |> wrap_case_v |> with_typ (wrap_var_t "tableDeclaration") }
;

(******** Control and control type declarations ********)

controlBody:
| body = blockStatement
    { [ NT body ] |> wrap_case_v |> with_typ (wrap_var_t "controlBody") }
;

controlLocalDeclaration:
| const = constantDeclaration
    { [ NT const ] |> wrap_case_v |> with_typ (wrap_var_t "controlLocalDeclaration") }
| action = actionDeclaration
    { declare_var_of_il (name_of_declaration action) false;
      [ NT action ] |> wrap_case_v |> with_typ (wrap_var_t "controlLocalDeclaration") }
| table = tableDeclaration
    { declare_var_of_il (name_of_declaration table) false;
        [ NT table ] |> wrap_case_v |> with_typ (wrap_var_t "controlLocalDeclaration") }
| inst = instantiation
    { [ NT inst ] |> wrap_case_v |> with_typ (wrap_var_t "controlLocalDeclaration") }
| var = variableDeclaration
    { [ NT var ] |> wrap_case_v |> with_typ (wrap_var_t "controlLocalDeclaration") }
;

controlLocalDeclarations:
| decls = list(controlLocalDeclaration)
    { let typ = wrap_var_t "controlLocalDeclaration" |> wrap_iter_t List in
      ListV decls |> with_typ typ }
;

controlTypeDeclaration:
(* TODO: name to push_name *)
| anno = optAnnotations info1 = CONTROL name = name type_params = optTypeParameters
    L_PAREN params = parameterList info2 = R_PAREN
    { let tags = Source.merge info1 info2 in
      tags |> ignore;
      [ NT anno; Term "control"; NT name; NT type_params; Term "("; NT params; Term ")" ]
      |> wrap_case_v |> with_typ (wrap_var_t "controlTypeDeclaration") }
;

controlDeclaration:
| controlTypeDeclaration = controlTypeDeclaration optConstructorParameters = optConstructorParameters
    L_BRACE locals = controlLocalDeclarations APPLY apply = controlBody info2 = R_BRACE pop_scope
    { info2 |> ignore; 
      [ NT controlTypeDeclaration; NT optConstructorParameters; Term "{"; NT locals; Term "apply"; NT apply; Term "}" ]
      |> wrap_case_v |> with_typ (wrap_var_t "controlDeclaration") }
;

(******** Value set declarations ********)

valueSetDeclaration:
| anno = optAnnotations info1 = VALUESET l_angle base = baseType r_angle
    L_PAREN size = expression R_PAREN name = name info2 = SEMICOLON
    { let tags = Source.merge info1 info2 in
      tags |> ignore;
      [ NT anno; Term "value_set"; Term "<"; NT base; Term ">"; Term "("; NT size; Term ")"; NT name; Term ";" ]
      |> wrap_case_v |> with_typ (wrap_var_t "valueSetDeclaration") }
| anno = optAnnotations info1 = VALUESET l_angle tuple = tupleType r_angle
    L_PAREN size = expression R_PAREN name = name info2 = SEMICOLON
    { let tags = Source.merge info1 info2 in
      tags |> ignore;
      [ NT anno; Term "value_set"; Term "<"; NT tuple; Term ">"; Term "("; NT size; Term ")"; NT name; Term ";"; Term "PHTM_17"]
      |> wrap_case_v |> with_typ (wrap_var_t "valueSetDeclaration") }
| anno = optAnnotations info1 = VALUESET l_angle typeName = typeName r_angle
    L_PAREN size = expression R_PAREN name = name info2 = SEMICOLON
    { let tags = Source.merge info1 info2 in
      tags |> ignore;
      [ NT anno; Term "value_set"; Term "<"; NT typeName; Term ">"; Term "("; NT size; Term ")"; NT name; Term ";"; Term "PHTM_18"]
      |> wrap_case_v |> with_typ (wrap_var_t "valueSetDeclaration") }
;

(******** Select expressions ********)

selectCase:
| keysetExpression = keysetExpression COLON name = name info2 = SEMICOLON
    { info2 |> ignore;
      [ NT keysetExpression; Term ":"; NT name; Term ";" ] |> wrap_case_v |> with_typ (wrap_var_t "selectCase") }
;

selectCaseList:
| cases = list(selectCase)
    { let typ = wrap_var_t "selectCase" |> wrap_iter_t List in
      ListV cases |> with_typ typ }
;

selectExpression:
| info1 = SELECT L_PAREN exprs = expressionList R_PAREN L_BRACE cases = selectCaseList info2 = R_BRACE
    { let tags = Source.merge info1 info2 in
      tags |> ignore;
      [ Term "select"; Term "("; NT exprs; Term ")"; Term "{"; NT cases; Term "}" ]
      |> wrap_case_v |> with_typ (wrap_var_t "selectExpression") }
;

(******** Transition statements ********)

stateExpression:
| name = name info2 = SEMICOLON
    { info2 |> ignore;
      [ NT name; Term ";" ] |> wrap_case_v |> with_typ (wrap_var_t "stateExpression") }
| selectExpression = selectExpression
    { [ NT selectExpression ] |> wrap_case_v |> with_typ (wrap_var_t "stateExpression") }
;

transitionStatement:
| info1 = TRANSITION stateExpression = stateExpression
    { info1 |> ignore;
      [ Term "transition"; NT stateExpression ] |> wrap_case_v |> with_typ (wrap_var_t "transitionStatement") }
;

(******** Parser and parser type declarations ********)

parserBlockStatement:
| optAnnotations = optAnnotations info1 = L_BRACE parserStatements = parserStatements info2 = R_BRACE
    { let tags = Source.merge info1 info2 in
      tags |> ignore;
      [ NT optAnnotations; Term "{"; NT parserStatements; Term "}" ] |> wrap_case_v |> with_typ (wrap_var_t "parserBlockStatement") }
;

parserStatement:
| stmt = assignmentOrMethodCallStatement
    { [ NT stmt ] |> wrap_case_v |> with_typ (wrap_var_t "parserStatement") }
| app = directApplication
    { [ NT app ] |> wrap_case_v |> with_typ (wrap_var_t "parserStatement") }
| stmt = emptyStatement
    { [ NT stmt ] |> wrap_case_v |> with_typ (wrap_var_t "parserStatement") }
| var = variableDeclaration
    { [ NT var ] |> wrap_case_v |> with_typ (wrap_var_t "parserStatement") }
| const = constantDeclaration
    { [ NT const ] |> wrap_case_v |> with_typ (wrap_var_t "parserStatement") }
| block = parserBlockStatement
    { [ NT block ] |> wrap_case_v |> with_typ (wrap_var_t "parserStatement") }
| cond = conditionalStatement
    { [ NT cond ] |> wrap_case_v |> with_typ (wrap_var_t "parserStatement") }
;

parserStatements:
| stmts = list(parserStatement)
    { let typ = wrap_var_t "parserStatement" |> wrap_iter_t List in
      ListV stmts |> with_typ typ }
;

(* TODO: name to push_name *)
parserState:
| anno = optAnnotations info1 = STATE name = name L_BRACE stmts = parserStatements trans = transitionStatement info2 = R_BRACE
    { let tags = Source.merge info1 info2 in
      tags |> ignore;
      [ NT anno; Term "state"; NT name; Term "{"; NT stmts; NT trans; Term "}" ]
      |> wrap_case_v |> with_typ (wrap_var_t "parserState") }
;


parserStates:
| states = list(parserState)
    { let typ = wrap_var_t "parserState" |> wrap_iter_t List in
      ListV states |> with_typ typ }
;

(* TODO: name to push_name *)
parserTypeDeclaration:
| anno = optAnnotations info1 = PARSER name = name optTypeParams = optTypeParameters
    L_PAREN params = parameterList R_PAREN
    { info1 |> ignore;
      [ NT anno; Term "parser"; NT name; NT optTypeParams; Term "("; NT params; Term ")" ]
      |> wrap_case_v |> with_typ (wrap_var_t "parserTypeDeclaration") }
;

(* AUX *)
declarationList:
| (* empty *) { [] }
| SEMICOLON ds = declarationList
    { ds }
| d = declaration ds = declarationList
    { d :: ds }
;

(******** P4 program ********)

p4program: 
  ds = declarationList END 
    { let value =
        let typ = wrap_var_t "declaration" |> wrap_iter_t List in
        ListV ds |> with_typ typ
      in
      value }
;

(********)

%inline r_angle:
| info_r = R_ANGLE
    { info_r } 
| info_r = R_ANGLE_SHIFT
    { info_r }
;
%inline l_angle:
| info_r = L_ANGLE
    { info_r } 
| info_r = L_ANGLE_ARGS
    { info_r }
;

parserLocalElement:
| const = constantDeclaration
    { [ NT const ] |> wrap_case_v |> with_typ (wrap_var_t "parserLocalElement") }
| var = variableDeclaration
    { [ NT var ] |> wrap_case_v |> with_typ (wrap_var_t "parserLocalElement") }
| inst = instantiation
    { [ NT inst ] |> wrap_case_v |> with_typ (wrap_var_t "parserLocalElement") }
| valueSet = valueSetDeclaration
    { [ NT valueSet ] |> wrap_case_v |> with_typ (wrap_var_t "parserLocalElement") }
;

(* Petr4 X / Spec O *)
parserLocalElements:
| elements = list(parserLocalElement)
    { let typ = wrap_var_t "parserLocalElement" |> wrap_iter_t List in
      ListV elements |> with_typ typ }
;

parserDeclaration:
| decl = parserTypeDeclaration params = optConstructorParameters
    L_BRACE locals = parserLocalElements states = parserStates info2 = R_BRACE pop_scope
    { info2 |> ignore;
      [ NT decl; NT params; Term "{"; NT locals; NT states; Term "}" ]
      |> wrap_case_v |> with_typ (wrap_var_t "parserDeclaration") }
;

packageTypeDeclaration:
| anno = optAnnotations info1 = PACKAGE name = name type_params = optTypeParameters
    L_PAREN params = parameterList info2 = R_PAREN
    { let tags = Source.merge info1 info2 in
      tags |> ignore;
      [ NT anno; Term "package"; NT name; NT type_params; Term "("; NT params; Term ")" ]
      |> wrap_case_v |> with_typ (wrap_var_t "packageTypeDeclaration") }
;

specifiedIdentifier:
| name = name init = initialValue
    { [ NT name; NT init ] |> wrap_case_v |> with_typ (wrap_var_t "specifiedIdentifier") }
;

specifiedIdentifierList:
| ids = comma_separated_list(specifiedIdentifier)
    { let typ = wrap_var_t "specifiedIdentifier" |> wrap_iter_t List in
      ListV ids |> with_typ typ }
;

(* TODO: compare with Petr4 *)
enumDeclaration:
| anno = optAnnotations info1 = ENUM name = name L_BRACE ids = identifierList comma = optTrailingComma info2 = R_BRACE
    { let tags = Source.merge info1 info2 in
      tags |> ignore;
      [ NT anno; Term "enum"; NT name; Term "{"; NT ids; NT comma; Term "}" ]
      |> wrap_case_v |> with_typ (wrap_var_t "enumDeclaration") }
| anno = optAnnotations info1 = ENUM typeRef = typeRef name = name L_BRACE ids = specifiedIdentifierList comma = optTrailingComma info2 = R_BRACE
    { let tags = Source.merge info1 info2 in
      tags |> ignore;
      [ NT anno; Term "enum"; NT typeRef; NT name; Term "{"; NT ids; NT comma; Term "}" ]
      |> wrap_case_v |> with_typ (wrap_var_t "enumDeclaration") }
;

structField:
| anno = optAnnotations typeRef = typeRef name = name info2 = SEMICOLON
    { info2 |> ignore;
      [ NT anno; NT typeRef; NT name; Term ";" ] |> wrap_case_v |> with_typ (wrap_var_t "structField") }
;

structFieldList:
| fields = list(structField)
    { let typ = wrap_var_t "structField" |> wrap_iter_t List in
      ListV fields |> with_typ typ }
;

headerUnionDeclaration:
| anno = optAnnotations info1 = HEADER_UNION name = name type_params = optTypeParameters
    L_BRACE fields = structFieldList info2 = R_BRACE
    { let tags = Source.merge info1 info2 in
      tags |> ignore;
      [ NT anno; Term "header_union"; NT name; NT type_params; Term "{"; NT fields; Term "}" ]
      |> wrap_case_v |> with_typ (wrap_var_t "headerUnionDeclaration") }
;

structTypeDeclaration:
| anno = optAnnotations info1 = STRUCT name = name type_params = optTypeParameters
    L_BRACE fields = structFieldList info2 = R_BRACE
    { let tags = Source.merge info1 info2 in
      tags |> ignore;
      [ NT anno; Term "struct"; NT name; NT type_params; Term "{"; NT fields; Term "}" ]
      |> wrap_case_v |> with_typ (wrap_var_t "structTypeDeclaration") }
;

headerTypeDeclaration:
| anno = optAnnotations info1 = HEADER name = name type_params = optTypeParameters
    L_BRACE fields = structFieldList info2 = R_BRACE
    { let tags = Source.merge info1 info2 in
      tags |> ignore;
      [ NT anno; Term "header"; NT name; NT type_params; Term "{"; NT fields; Term "}" ]
      |> wrap_case_v |> with_typ (wrap_var_t "headerTypeDeclaration") }
;

derivedTypeDeclaration:
| decl = headerTypeDeclaration
| decl = headerUnionDeclaration
| decl = structTypeDeclaration
| decl = enumDeclaration
    { [ NT decl ] |> wrap_case_v |> with_typ (wrap_var_t "derivedTypeDeclaration") }
;

(* TODO: Petr4 has trailing Semicolon here instead of typeDeclaration *)
typedefDeclaration:
| anno = optAnnotations info1 = TYPEDEF typeRef = typeRef name = name
    { info1 |> ignore;
      [ NT anno; Term "typedef"; NT typeRef; NT name ] |> wrap_case_v |> with_typ (wrap_var_t "typedefDeclaration") }
| anno = optAnnotations info1 = TYPEDEF derived = derivedTypeDeclaration name = name
    { info1 |> ignore;
      [ NT anno; Term "typedef"; NT derived; NT name; Term "PHTM_12" ] |> wrap_case_v |> with_typ (wrap_var_t "typedefDeclaration") }
| anno = optAnnotations info1 = TYPE typeRef = typeRef name = name
    { info1 |> ignore;
      [ NT anno; Term "type"; NT typeRef; NT name ] |> wrap_case_v |> with_typ (wrap_var_t "typedefDeclaration") }
;

typeDeclaration:
| derived = derivedTypeDeclaration
    { [ NT derived ] |> wrap_case_v |> with_typ (wrap_var_t "typeDeclaration") }
| typedef = typedefDeclaration info2 = SEMICOLON
    { info2 |> ignore;
      [ NT typedef; Term ";" ] |> wrap_case_v |> with_typ (wrap_var_t "typeDeclaration") }
| parserTypeDeclaration = parserTypeDeclaration pop_scope info2 = SEMICOLON
    { info2 |> ignore;
      [ NT parserTypeDeclaration; Term ";"; Term "PHTM_13" ] |> wrap_case_v |> with_typ (wrap_var_t "typeDeclaration") }
| controlTypeDeclaration = controlTypeDeclaration pop_scope info2 = SEMICOLON
    { info2 |> ignore;
      [ NT controlTypeDeclaration; Term ";"; Term "PHTM_14" ] |> wrap_case_v |> with_typ (wrap_var_t "typeDeclaration") }
| packageTypeDeclaration = packageTypeDeclaration pop_scope info2 = SEMICOLON
    { info2 |> ignore;
      [ NT packageTypeDeclaration; Term ";"; Term "PHTM_15" ] |> wrap_case_v |> with_typ (wrap_var_t "typeDeclaration") }
;

declaration:
| const = constantDeclaration
    { declare_var_of_il (name_of_declaration const) (has_typ_params_declaration const);
      [ NT const ] |> wrap_case_v |> with_typ (wrap_var_t "declaration") }
| error = errorDeclaration
    { [ NT error ] |> wrap_case_v |> with_typ (wrap_var_t "declaration") }
| matchKind = matchKindDeclaration
    { [ NT matchKind ] |> wrap_case_v |> with_typ (wrap_var_t "declaration") }
| extern = externDeclaration
    { [ NT extern ] |> wrap_case_v |> with_typ (wrap_var_t "declaration") }
| inst = instantiation
    { declare_var_of_il (name_of_declaration inst) false;
      [ NT inst ] |> wrap_case_v |> with_typ (wrap_var_t "declaration") }
| func = functionDeclaration
    { declare_var_of_il (name_of_declaration func) (has_typ_params_declaration func);
      [ NT func ] |> wrap_case_v |> with_typ (wrap_var_t "declaration") }
| action = actionDeclaration
    { declare_var_of_il (name_of_declaration action) false;
      [ NT action ] |> wrap_case_v |> with_typ (wrap_var_t "declaration") }
| parserDeclaration = parserDeclaration
    { declare_type_of_il (name_of_declaration parserDeclaration) (has_typ_params_declaration parserDeclaration);
      [ NT parserDeclaration ] |> wrap_case_v |> with_typ (wrap_var_t "declaration") }
| controlDeclaration = controlDeclaration
    { declare_type_of_il (name_of_declaration controlDeclaration) (has_typ_params_declaration controlDeclaration);
      [ NT controlDeclaration ] |> wrap_case_v |> with_typ (wrap_var_t "declaration") }
| typeDeclaration = typeDeclaration
    { declare_type_of_il (name_of_declaration typeDeclaration) (has_typ_params_declaration typeDeclaration);
      [ NT typeDeclaration ] |> wrap_case_v |> with_typ (wrap_var_t "declaration") }
;

annotationToken:
| value = UNEXPECTED_TOKEN
    { [ NT value ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| ABSTRACT
    { [ Term "abstract" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| ACTION
    { [ Term "action" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| ACTIONS
    { [ Term "actions" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| APPLY
    { [ Term "apply" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| BOOL
    { [ Term "bool" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| BIT
    { [ Term "bit" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| BREAK
    { [ Term "break" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| CONST
    { [ Term "const" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| CONTINUE
    { [ Term "continue" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| CONTROL
    { [ Term "control" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| DEFAULT
    { [ Term "default" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| ELSE
    { [ Term "else" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| ENTRIES
    { [ Term "entries" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| ENUM
    { [ Term "enum" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| ERROR
    { [ Term "error" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| EXIT
    { [ Term "exit" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| EXTERN
    { [ Term "extern" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| FALSE
    { [ Term "false" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
(* | FOR *)
(*     { [ Term "for" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") } *)
| HEADER
    { [ Term "header" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| HEADER_UNION
    { [ Term "header_union" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| IF
    { [ Term "if" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| IN
    { [ Term "in" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| INOUT
    { [ Term "inout" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| INT
    { [ Term "int" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| KEY
    { [ Term "key" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| MATCH_KIND
    { [ Term "match_kind" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| TYPE
    { [ Term "type" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| OUT
    { [ Term "out" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| PARSER
    { [ Term "parser" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| PACKAGE
    { [ Term "package" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| PRAGMA
    { [ Term "pragma" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| RETURN
    { [ Term "return" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| SELECT
    { [ Term "select" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| STATE
    { [ Term "state" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| STRING
    { [ Term "string" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| STRUCT
    { [ Term "struct" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| SWITCH
    { [ Term "switch" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| TABLE
    { [ Term "table" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| THIS
    { [ Term "this" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| TRANSITION
    { [ Term "transition" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| TRUE
    { [ Term "true" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| TUPLE
    { [ Term "tuple" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| TYPEDEF
    { [ Term "typedef" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| VARBIT
    { [ Term "varbit" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| VALUESET
    { [ Term "valueset" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| LIST
    { [ Term "list" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| VOID
    { [ Term "void" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| DONTCARE
    { [ Term "_" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| id = identifier
    { [ NT id ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| tid = typeIdentifier
    { [ NT tid ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| str = stringLiteral
    { [ NT str ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| num = number
    { [ NT num ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| MASK
    { [ Term "&&&" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
  (* TODO: missing DOTS "..." in spec *)
| RANGE
    { [ Term ".." ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| SHL
    { [ Term "<<" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| AND
    { [ Term "&&" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| OR
    { [ Term "||" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| EQ
    { [ Term "==" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| NE
    { [ Term "!=" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| GE
    { [ Term ">=" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| LE    
    { [ Term "<=" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| PLUSPLUS
    { [ Term "++" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| PLUS
    { [ Term "+" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| PLUS_SAT
    { [ Term "|+|" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| MINUS
    { [ Term "-" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| MINUS_SAT
    { [ Term "|-|" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| MUL
    { [ Term "*" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| DIV
    { [ Term "/" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| MOD
    { [ Term "%" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| BIT_OR
    { [ Term "|" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| BIT_AND
    { [ Term "&" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| BIT_XOR
    { [ Term "^" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| COMPLEMENT
    { [ Term "~" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| L_BRACKET
    { [ Term "[" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| R_BRACKET
    { [ Term "]" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| L_BRACE
    { [ Term "{" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| R_BRACE
    { [ Term "}" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| L_ANGLE
    { [ Term "<" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| R_ANGLE
    { [ Term ">" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| NOT
    { [ Term "!" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| COLON
    { [ Term ":" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| COMMA
    { [ Term ","; Term "PHTM_21" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| QUESTION
    { [ Term "?" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| DOT
    { [ Term "." ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| ASSIGN
    { [ Term "=" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| SEMICOLON
    { [ Term ";" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| AT
    { [ Term "@" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
;

simpleAnnotation:
| L_PAREN body = simpleAnnotationBody R_PAREN
    { [ Term "("; NT body; Term ")" ] |> wrap_case_v |> with_typ (wrap_var_t "simpleAnnotation") }
| token = annotationToken
    { [ NT token ] |> wrap_case_v |> with_typ (wrap_var_t "simpleAnnotation") }
;

simpleAnnotationBody:
| body = list(simpleAnnotation)
    { let typ = wrap_var_t "simpleAnnotation" |> wrap_iter_t List in
      ListV body |> with_typ typ }
;

structuredAnnotationBody:
| exprs = expressionList comma = optTrailingComma
    { [ NT exprs; NT comma ] |> wrap_case_v |> with_typ (wrap_var_t "structuredAnnotationBody") }
| kvs = kvList comma = optTrailingComma
    { [ NT kvs; NT comma; Term "PHTM_20" ] |> wrap_case_v |> with_typ (wrap_var_t "structuredAnnotationBody") }
;

(* Petr4 also has "pragma name body" *)
annotation:
| info1 = AT name = name
    { info1 |> ignore;
      [ Term "@"; NT name ] |> wrap_case_v |> with_typ (wrap_var_t "annotation") }
| info1 = AT name = name L_PAREN body = simpleAnnotationBody R_PAREN
    { info1 |> ignore;
      [ Term "@"; NT name; Term "("; NT body; Term ")" ] |> wrap_case_v |> with_typ (wrap_var_t "annotation") }
| info1 = AT name = name L_BRACKET body = structuredAnnotationBody R_BRACKET
    { info1 |> ignore;
      [ Term "@"; NT name; Term "["; NT body; Term "]" ] |> wrap_case_v |> with_typ (wrap_var_t "annotation") }
;

(* TODO: nonempty? *)
annotations:
| annos = list(annotation)
  { let typ = wrap_var_t "annotation" |> wrap_iter_t List in
    ListV annos |> with_typ typ }
;

%inline optAnnotations:
| (* empty *)
  { [ Term "empty" ] |> wrap_case_v |> with_typ (wrap_var_t "optAnnotations") }
| annos = nonempty_list(annotation)
  { let typ = wrap_var_t "annotation" |> wrap_iter_t List in
    ListV annos |> with_typ typ }
;
