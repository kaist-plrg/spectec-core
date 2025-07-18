%{
  open Il.Ast
  open Context
  open Ast_utils
  open Extract
  let declare_var_of_il (v: value) (b: bool) : unit =
    let id = id_of_name v in
    declare_var id b
  let declare_vars_of_il (v: value) : unit =
    match v.it with
    | ListV vs ->
        List.iter (fun s -> declare_var_of_il s false) vs
    | _ -> failwith "@declare_vars_of_il: expected a ListV value"
  let declare_type_of_il (v: value) (b: bool) : unit =
    let id = id_of_name v in
    declare_type id b
  let declare_types_of_il (v: value) : unit =
    match v.it with
    | ListV vs ->
        List.iter (fun s -> declare_type_of_il s false) vs
    | _ -> failwith "@declare_types_of_il: expected a ListV value"
%}
(**************************** TOKENS ******************************)
%token<Source.info> END
%token TYPENAME IDENTIFIER
%token<Il.Ast.value> NAME STRING_LITERAL
%token<Il.Ast.value * string> NUMBER_INT NUMBER
%token<Source.info> LE GE SHL AND OR NE EQ
%token<Source.info> PLUS MINUS PLUS_SAT MINUS_SAT MUL INVALID DIV MOD
%token<Source.info> BIT_OR BIT_AND BIT_XOR COMPLEMENT
%token<Source.info> L_BRACKET R_BRACKET L_BRACE R_BRACE L_ANGLE L_ANGLE_ARGS R_ANGLE R_ANGLE_SHIFT L_PAREN R_PAREN
%token<Source.info> ASSIGN COLON COMMA QUESTION DOT NOT SEMICOLON
%token<Source.info> AT PLUSPLUS
%token<Source.info> DONTCARE
%token<Source.info> MASK DOTS RANGE
%token<Source.info> TRUE FALSE
%token<Source.info> ABSTRACT ACTION ACTIONS APPLY BOOL BIT BREAK CONST CONTINUE CONTROL DEFAULT
%token<Source.info> ELSE ENTRIES ENUM ERROR EXIT EXTERN HEADER HEADER_UNION IF IN INOUT FOR
%token<Source.info> INT KEY LIST SELECT MATCH_KIND OUT PACKAGE PARSER PRIORITY RETURN STATE STRING STRUCT
%token<Source.info> SWITCH TABLE THIS TRANSITION TUPLE TYPEDEF TYPE VALUESET VARBIT VOID
%token<Source.info> PRAGMA PRAGMA_END
%token<Source.info> PLUS_ASSIGN PLUS_SAT_ASSIGN MINUS_ASSIGN MINUS_SAT_ASSIGN MUL_ASSIGN DIV_ASSIGN MOD_ASSIGN  SHL_ASSIGN SHR_ASSIGN BIT_AND_ASSIGN BIT_XOR_ASSIGN BIT_OR_ASSIGN
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
%start <Il.Ast.value> variableDeclaration
%start <Il.Ast.value> typeDeclaration
(**************************** TYPES ******************************)
(* %type <Il.Ast.value> declaration *)
(* %type <Il.Ast.value> trailingComma kvPair optTrailingComma const optCONST number stringLiteral dotPrefix identifier typeIdentifier nonTypeName prefixedNonTypeName nonTableKwName prefixedTypeName typeName name identifierList member direction baseType specializedType namedType headerStackType listType tupleType typeRef typeOrVoid typeParameterList typeParameters optTypeParameters parameter parameterList constructorParameters optConstructorParameters nonBraceExpression expression simpleKeysetExpression reducedSimpleKeysetExpression simpleExpressionList tupleKeysetExpression keysetExpression realTypeArg realTypeArgumentList typeArg typeArgList argument argumentList lvalue initialValue optInitializer variableDeclarationWithoutSemicolon constantDeclaration assignmentOrMethodCallStatementWithoutSemicolon assignmentOrMethodCallStatement directApplication conditionalStatement emptyStatement blockStatement returnStatement breakStatement continueStatement exitStatement switchLabel switchCase switchCases switchStatement statement statementOrDeclaration statOrDeclList matchKindDeclaration errorDeclaration functionPrototype methodPrototype methodPrototypes externDeclaration externName functionDeclaration objInitializer instantiation objDeclaration objDeclarations actionDeclaration keyElement keyElementList actionRef action actionList entryPriority entry entriesList *)
(* %type <Il.Ast.value> annotation annotationToken annotations controlBody controlDeclaration controlLocalDeclaration controlLocalDeclarations controlTypeDeclaration derivedTypeDeclaration enumDeclaration headerTypeDeclaration headerUnionDeclaration packageTypeDeclaration parserBlockStatement parserDeclaration parserLocalElement parserLocalElements parserState parserStatement parserStatements parserStates parserTypeDeclaration selectCase selectCaseList selectExpression simpleAnnotation simpleAnnotationBody specifiedIdentifier specifiedIdentifierList stateExpression structField structFieldList structTypeDeclaration structuredAnnotationBody tableDeclaration tableProperty tablePropertyList transitionStatement typedefDeclaration valueSetDeclaration *)
(* %type <Il.Ast.value list> lib_parsing_parser_list(action) lib_parsing_parser_list(annotation) lib_parsing_parser_list(controlLocalDeclaration) lib_parsing_parser_list(declaration) lib_parsing_parser_list(entry) lib_parsing_parser_list(keyElement) lib_parsing_parser_list(methodPrototype) lib_parsing_parser_list(objDeclaration) lib_parsing_parser_list(parserLocalElement) lib_parsing_parser_list(parserState) lib_parsing_parser_list(parserStatement) lib_parsing_parser_list(selectCase) lib_parsing_parser_list(simpleAnnotation) lib_parsing_parser_list(statementOrDeclaration) lib_parsing_parser_list(structField) lib_parsing_parser_list(switchCase) lib_parsing_parser_list(tableProperty) *)
(* %type <Il.Ast.value list> lib_parsing_parser_separated_list(COMMA,argument) lib_parsing_parser_separated_list(COMMA,expression) lib_parsing_parser_separated_list(COMMA,parameter) lib_parsing_parser_separated_list(COMMA,realTypeArg) lib_parsing_parser_separated_list(COMMA,simpleKeysetExpression) lib_parsing_parser_separated_list(COMMA,specifiedIdentifier) lib_parsing_parser_separated_list(COMMA,typeArg) *)
(* %type <Il.Ast.value list> lib_parsing_parser_separated_nonempty_list(COMMA,__anonymous_0) lib_parsing_parser_separated_nonempty_list(COMMA,kvPair) lib_parsing_parser_separated_nonempty_list(COMMA,name) *)
(* %type <Il.Ast.value list> list_aux(action) list_aux(annotation) list_aux(controlLocalDeclaration) list_aux(declaration) list_aux(entry) list_aux(keyElement) list_aux(methodPrototype) list_aux(objDeclaration) list_aux(parserLocalElement) list_aux(parserState) list_aux(parserStatement) list_aux(selectCase) list_aux(simpleAnnotation) list_aux(statementOrDeclaration) list_aux(structField) list_aux(switchCase) list_aux(tableProperty) *)
(* %type <Il.Ast.value list> separated_list_aux(COMMA,argument) separated_list_aux(COMMA,expression) separated_list_aux(COMMA,parameter) separated_list_aux(COMMA,realTypeArg) separated_list_aux(COMMA,simpleKeysetExpression) separated_list_aux(COMMA,specifiedIdentifier) separated_list_aux(COMMA,typeArg) *)
(* %type <Il.Ast.value list> separated_nonempty_list_aux(COMMA,__anonymous_0) separated_nonempty_list_aux(COMMA,kvPair) separated_nonempty_list_aux(COMMA,name) separated_nonempty_list_aux(COMMA,specifiedIdentifier) *)
(* %type <Il.Ast.value list> separated_nonempty_opt_trailing_list(COMMA,__anonymous_0) separated_nonempty_opt_trailing_list(COMMA,kvPair) separated_nonempty_trailing_list(COMMA,kvPair) separated_opt_trailing_list(COMMA,expression) separated_nonempty_opt_trailing_list(COMMA,specifiedIdentifier) *)
(* %type <Il.Ast.value list> declarationList kvList expressionList *)
(* %type <Il.Ast.value> push_name push_externName *)
(* %type <unit> push_scope pop_scope go_toplevel go_local *)
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
(**************************** P4-16 GRAMMAR ******************************)
(******** Built-in ********)
int:
| int = NUMBER_INT
    { fst int }
;
(******** Misc ********)
trailingComma:
| COMMA
        { [ Term "," ] #@ "trailingComma" }
;
optTrailingComma:
| comma = option(trailingComma)
    { wrap_opt_v "trailingComma" comma }
;
const:
| CONST
    { [ Term "CONST" ] #@ "const" }
;
optCONST:
| c = option(const)
    { wrap_opt_v "const" c }
;
(******** Numbers ********)
(* Processed by lexer *)
number:
| int = int
    { [ Term "D"; NT int ] #@ "number" }
| number = NUMBER
    { fst number }
;
(******** Strings ********)
(* Petr4 X / Spec O *)
stringLiteral:
| text = STRING_LITERAL
    { [ Term (Char.escaped '"'); NT text; Term (Char.escaped '"') ] #@ "stringLiteral"}
;
(******** Names ********)
identifier:
| text = NAME IDENTIFIER
    { [ Term "`ID"; NT text ] #@ "identifier" }
;
typeIdentifier:
| text = NAME TYPENAME
    { [ Term "`TID"; NT text ] #@ "typeIdentifier" }
;
nonTypeName:
| identifier = identifier
    { identifier }
| info = APPLY
    { info |> ignore;
      [ Term "APPLY" ] #@ "nonTypeName" }
| info = KEY
    { info |> ignore;
      [ Term "KEY" ] #@ "nonTypeName" }
| info = ACTIONS
    { info |> ignore;
      [ Term "ACTIONS" ] #@ "nonTypeName" }
| info = STATE
    { info |> ignore;
      [ Term "STATE" ] #@ "nonTypeName" }
| info = ENTRIES
    { info |> ignore;
      [ Term "ENTRIES" ] #@ "nonTypeName" }
| info = TYPE
    { info |> ignore;
      [ Term "TYPE" ] #@ "nonTypeName" }
| info = PRIORITY
    { info |> ignore;
      [ Term "PRIORITY" ] #@ "nonTypeName" }
;
prefixedNonTypeName:
| n = nonTypeName { n }
| go_toplevel nonTypeName = nonTypeName go_local
    { [ Term "."; NT nonTypeName ] #@ "prefixedNonTypeName" }
;
typeName:
| n = typeIdentifier { n }
;
prefixedTypeName:
| n = typeName { n }
| go_toplevel typeIdentifier = typeIdentifier go_local
    {
      [ Term ".."; NT typeIdentifier ] #@ "prefixedType"
    }
;
name:
| nonTypeName = nonTypeName
    { nonTypeName }
| typeName = typeName
    { typeName }
| info = LIST
    { info |> ignore;
      [ Term "LIST" ]  #@ "name" }
;
nameList:
| n = name { n }
| nl = nameList COMMA n = name
    { [ NT nl; Term ","; NT n ]
       #@ "nameList" }
;
nonTableKwName:
| identifier = identifier
    { identifier }
| typeIdentifier = typeIdentifier
    { typeIdentifier }
| info = APPLY
    { info |> ignore;
      [ Term "APPLY" ]  #@ "nonTypeName" }
| info = STATE
    { info |> ignore;
      [ Term "STATE" ]  #@ "nonTypeName" }
| info = TYPE
    { info |> ignore;
      [ Term "TYPE" ]  #@ "nonTypeName" }
| info = PRIORITY
    { info |> ignore;
      [ Term "PRIORITY" ]  #@ "nonTypeName" }
;
member:
| name = name
    { name }
;
(******** Directions ********)
direction:
| (* empty *)
    { [ Term "`EMPTY" ]  #@ "direction" }
| IN
    { [ Term "IN" ]  #@ "direction" }
| OUT
    { [ Term "OUT" ]  #@ "direction" }
| INOUT
    { [ Term "INOUT" ]  #@ "direction" }
;
(******** Types ********)
baseType:
| info = BOOL
    { info |> ignore;
      [ Term "BOOL" ]  #@ "baseType" }
| info = MATCH_KIND
    { info |> ignore;
      [ Term "MATCH_KIND" ]  #@ "baseType" }
| info = ERROR
    { info |> ignore;
      [ Term "ERROR" ]  #@ "baseType" }
| info = BIT
    { info |> ignore;
      [ Term "BIT" ]  #@ "baseType" }
| info = STRING
    { info |> ignore;
      [ Term "STRING" ]  #@ "baseType" }
| info = INT
    { info |> ignore;
      [ Term "INT" ]  #@ "baseType" }
| info1 = BIT l_angle value = int info_r = r_angle
    { let tags = Source.merge info1 info_r in
      tags |> ignore;
      [ Term "BIT"; Term "<"; NT value; Term ">" ]
       #@ "baseType" }
| info1 = INT l_angle value = int info_r = r_angle
    { let tags = Source.merge info1 info_r in
      tags |> ignore;
      [ Term "INT"; Term "<"; NT value; Term ">" ]
       #@ "baseType" }
| info1 = VARBIT l_angle value = int info_r = r_angle
    { let tags = Source.merge info1 info_r in
      tags |> ignore;
      [ Term "VARBIT"; Term "<"; NT value; Term ">" ]
       #@ "baseType" }
| info1 = BIT l_angle L_PAREN expr = expression R_PAREN info_r = r_angle
    { let tags = Source.merge info1 info_r in
      tags |> ignore;
      [ Term "BIT"; Term "<"; Term "("; NT expr; Term ")"; Term ">" ]
       #@ "baseType" }
| info1 = INT l_angle L_PAREN expr = expression R_PAREN info_r = r_angle
    { let tags = Source.merge info1 info_r in
      tags |> ignore;
      [ Term "INT"; Term "<"; Term "("; NT expr; Term ")"; Term ">" ]
       #@ "baseType" }
| info1 = VARBIT l_angle L_PAREN expr = expression R_PAREN info_r = r_angle
    { let tags = Source.merge info1 info_r in
      tags |> ignore;
      [ Term "VARBIT"; Term "<"; Term "("; NT expr; Term ")"; Term ">" ]
       #@ "baseType" }
;
specializedType:
| prefixedTypeName = prefixedTypeName l_angle typeArgList = typeArgList info_r = r_angle
    { info_r |> ignore;
      [ NT prefixedTypeName; Term "<"; NT typeArgList; Term ">" ]
       #@ "specializedType" }
;
namedType:
| t = prefixedTypeName
| t = specializedType
    { t }
;
headerStackType:
| typeName = typeName L_BRACKET expression = expression info2 = R_BRACKET
    { info2 |> ignore;
      [ NT typeName; Term "["; NT expression; Term "]" ]
       #@ "headerStackType" }
| specializedType = specializedType L_BRACKET expression = expression info2 = R_BRACKET
    { info2 |> ignore;
      [ NT specializedType; Term "["; NT expression; Term "]" ]
       #@ "headerStackType" }
;
listType:
| info1 = LIST l_angle typeArg = typeArg info_r = r_angle
    { let tags = Source.merge info1 info_r in
      tags |> ignore;
      [ Term "LIST"; Term "<"; NT typeArg; Term ">" ]
       #@ "listType" }
;
tupleType:
| info1 = TUPLE l_angle typeArgList = typeArgList info_r = r_angle
    { let tags = Source.merge info1 info_r in
      tags |> ignore;
      [ Term "TUPLE"; Term "<"; NT typeArgList; Term ">" ]
       #@ "tupleType" }
;
(******** Type references ********)
typeRef:
| t = baseType
| t = namedType
| t = headerStackType
| t = listType
| t = tupleType
    { t }
;
typeOrVoid:
| typeRef = typeRef
    { typeRef }
| info = VOID
    { info |> ignore;
      [ Term "VOID" ]  #@ "typeOrVoid" }
(* From Petr4: HACK for generic return type *)
| name = name
    { match flatten_case_v name with
        | "identifier", [ ["$"]; [] ], [ value_text ]  ->
          [ Term "@"; NT value_text ]
           #@ "typeIdentifier"
        | _ -> failwith "@typeOrVoid: expected identifier" }
;
(******** Type parameters ********)
typeParameterList:
| names = separated_nonempty_list(COMMA, name)
    { wrap_list_v "name" names }
;
typeParameters:
| l_angle type_params = typeParameterList r_angle
    { declare_types_of_il type_params;
      [ Term "<"; NT type_params; Term ">" ]
       #@ "typeParameters" }
;
optTypeParameters:
| typeParameters = option(typeParameters)
    { wrap_opt_v "typeParameters" typeParameters }
;
(******** Parameters ********)
parameter:
| optAnnotations = optAnnotations direction = direction
    typeRef = typeRef name = name
    {
      [ NT optAnnotations; NT direction; NT typeRef; NT name ]
       #@ "parameter"
    }
  (* TODO: ASSIGN terminal in grammar? *)
| optAnnotations = optAnnotations direction = direction
    typeRef = typeRef name = name init = initialValue
    {
      [ NT optAnnotations; NT direction; NT typeRef; NT name; NT init ]
       #@ "parameter"
    }
;
parameterList:
| params = separated_list(COMMA, parameter)
    { declare_vars (List.map id_of_parameter params);
      wrap_list_v "parameter" params }
;
constructorParameters:
| L_PAREN parameterList = parameterList R_PAREN
    { [ Term "("; NT parameterList; Term ")" ]  #@ "constructorParameters" }
;
optConstructorParameters:
| params = option(constructorParameters)
    { wrap_opt_v "constructorParameters" params }
;
(******** Expressions ********)
nonBraceExpression:
| number = number
    { number }
| stringLiteral = stringLiteral
    { stringLiteral }
| info1 = TRUE
    { info1 |> ignore;
      [ Term "TRUE" ]  #@ "nonBraceExpression" }
| info1 = FALSE
    { info1 |> ignore;
      [ Term "FALSE" ]  #@ "nonBraceExpression" }
| info1 = THIS
    { info1 |> ignore;
      [ Term "THIS" ]  #@ "nonBraceExpression" }
(* Petr4 O / Spec X : SPEC BUG - no identifier variant in switchLabel *)
| expr = prefixedNonTypeName
    { expr }
| array = nonBraceExpression L_BRACKET index = expression info2 = R_BRACKET
    { info2 |> ignore;
      [ NT array; Term "["; NT index; Term "]" ]
       #@ "nonBraceExpression" }
| bits = nonBraceExpression L_BRACKET hi = expression COLON lo = expression info2 = R_BRACKET
    { info2 |> ignore;
      [ NT bits; Term "["; NT hi; Term ":"; NT lo; Term "]" ]
     #@ "nonBraceExpression" }
| L_PAREN exp = expression R_PAREN
    { exp }
| info1 = NOT arg = expression %prec PREFIX
    { info1 |> ignore;
  [ Term "!"; NT arg ]  #@ "nonBraceExpression" }
| info1 = COMPLEMENT arg = expression %prec PREFIX
    { info1 |> ignore;
  [ Term "~"; NT arg ]  #@ "nonBraceExpression" }
| info1 = MINUS arg = expression %prec PREFIX
    { info1 |> ignore;
  [ Term "-"; NT arg ]  #@ "nonBraceExpression" }
| info1 = PLUS arg = expression %prec PREFIX
    { info1 |> ignore;
      [ Term "+"; NT arg ]  #@ "nonBraceExpression" }
(* Petr4: prefixedTypeName *)
| typ = typeName DOT member = member
    { [ NT typ; Term "."; NT member ]
       #@ "nonBraceExpression" }
| info1 = ERROR DOT member = member
    { info1 |> ignore;
      [ Term "ERROR"; Term "."; NT member ]
       #@ "nonBraceExpression" }
| expr = nonBraceExpression DOT member = member
    { [ NT expr; Term "."; NT member ]
       #@ "nonBraceExpression" }
(* TODO: binop *)
| arg1 = nonBraceExpression op_str = binop arg2 = expression
    { [ NT arg1; Term op_str; NT arg2 ]
       #@ "nonBraceExpression" }
| cond = nonBraceExpression QUESTION true_expr = expression COLON false_expr = expression
    { [ NT cond; Term "?"; NT true_expr; Term ":"; NT false_expr ]
       #@ "nonBraceExpression" }
| func = nonBraceExpression l_angle type_args = realTypeArgumentList r_angle
  L_PAREN args = argumentList info2 = R_PAREN
    { info2 |> ignore;
      [ NT func; Term "<"; NT type_args; Term ">"; Term "("; NT args; Term ")" ]
       #@ "nonBraceExpression" }
| func = nonBraceExpression L_PAREN args = argumentList info2 = R_PAREN
    { info2 |> ignore;
      [ NT func; Term "("; NT args; Term ")" ]
       #@ "nonBraceExpression" }
| typ = namedType L_PAREN args = argumentList info2 = R_PAREN
    { info2 |> ignore;
      [ NT typ; Term "("; NT args; Term ")" ]
       #@ "nonBraceExpression" }
| info1 = L_PAREN typ = typeRef R_PAREN expr = expression %prec PREFIX
    { info1 |> ignore;
      [ Term "("; NT typ; Term ")"; NT expr ]
       #@ "nonBraceExpression" }
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
expression:
| num = number
    { num }
| info1 = DOTS
    { info1 |> ignore;
      [ Term "..." ]  #@ "expression" }
| info1 = TRUE
    { info1 |> ignore;
      [ Term "TRUE" ]  #@ "expression" }
| info1 = FALSE
    { info1 |> ignore;
      [ Term "FALSE" ]  #@ "expression" }
| value = stringLiteral
    { value }
| info1 = THIS
    { info1 |> ignore;
      [ Term "THIS" ]  #@ "expression" }
| n = prefixedNonTypeName { n }
| array = expression L_BRACKET index = expression info2 = R_BRACKET
    { info2 |> ignore;
      [ NT array; Term "["; NT index; Term "]" ]  #@ "expression" }
| bits = expression L_BRACKET hi = expression COLON lo = expression info2 = R_BRACKET
    { info2 |> ignore;
      [ NT bits; Term "["; NT hi; Term ":"; NT lo; Term "]" ]  #@ "expression" }
(* List expressions *)
| info1 = L_BRACE exprs = expressionList comma = optTrailingComma info2 = R_BRACE
    { let tags = Source.merge info1 info2 in
      tags |> ignore;
      let exprs = List.rev exprs |> wrap_list_v "expression" in
    [ Term "{"; NT exprs; NT comma; Term "}" ]  #@ "expression" }
| info = INVALID
    { info |> ignore;
      [ Term "INVALID" ]  #@ "expression" }
(* HACK: shift-reduce conflict with COMMA *)
| info1 = L_BRACE kvs = kvList comma = optTrailingComma info2 = R_BRACE
    { let tags = Source.merge info1 info2 in
    tags |> ignore;
      let kvs = List.rev kvs |> wrap_list_v "kvPair" in
    [ Term "{"; NT kvs; NT comma; Term "}" ]  #@ "expression" }
| info1 = L_BRACE kvs = kvList COMMA DOTS comma = optTrailingComma info2 = R_BRACE
    { let tags = Source.merge info1 info2 in
      tags |> ignore;
      let kvs = List.rev kvs |> wrap_list_v "kvPair" in
    [ Term "{"; NT kvs; Term ","; Term "..."; NT comma; Term "}" ]  #@ "expression" }
| L_PAREN exp = expression R_PAREN
    { exp }
| info1 = NOT arg = expression %prec PREFIX
    { info1 |> ignore;
      [ Term "!"; NT arg ]  #@ "expression" }
| info1 = COMPLEMENT arg = expression %prec PREFIX
    { info1 |> ignore;
      [ Term "~"; NT arg ]  #@ "expression" }
| info1 = MINUS arg = expression %prec PREFIX
    { info1 |> ignore;
      [ Term "-"; NT arg ]  #@ "expression" }
| info1 = PLUS arg = expression %prec PREFIX
    { info1 |> ignore;
      [ Term "+"; NT arg ]  #@ "expression" }
| info1 = L_PAREN typ = typeRef R_PAREN expr = expression %prec PREFIX
    { info1 |> ignore;
      [ Term "("; NT typ; Term ")"; NT expr ]  #@ "expression" }
(* Petr4: prefixedTypeName *)
| typ = typeName DOT name = member
    { [ NT typ; Term "."; NT name ]  #@ "expression" }
| info1 = ERROR DOT name = member
    { info1 |> ignore;
      [ Term "ERROR"; Term "."; NT name ]  #@ "expression" }
| expr = expression DOT name = member
    { [ NT expr; Term "."; NT name ]  #@ "expression" }
| arg1 = expression op = binop arg2 = expression
    { [ NT arg1; Term op; NT arg2 ]  #@ "expression" }
| cond = expression QUESTION tru = expression COLON fls = expression
    { [ NT cond; Term "?"; NT tru; Term ":"; NT fls ]  #@ "expression" }
| func = expression l_angle type_args = realTypeArgumentList r_angle
  L_PAREN args = argumentList info2 = R_PAREN
    { info2 |> ignore;
      [ NT func; Term "<"; NT type_args; Term ">"; Term "("; NT args; Term ")" ]  #@ "expression" }
| func = expression L_PAREN args = argumentList info2 = R_PAREN
    { info2 |> ignore;
      [ NT func; Term "("; NT args; Term ")" ]  #@ "expression" }
| typ = namedType L_PAREN args = argumentList info2 = R_PAREN
    { info2 |> ignore;
      [ NT typ; Term "("; NT args; Term ")" ]  #@ "expression" }
;
%inline expressionList:
| exprs = separated_list_aux(COMMA, expression)
    { exprs }
;
simpleKeysetExpression:
| expr = expression
    { expr }
| expr = expression MASK mask = expression
    { [ NT expr; Term "&&&"; NT mask ]  #@ "simpleKeysetExpression" }
| lo = expression RANGE hi = expression
    { [ NT lo; Term ".."; NT hi ]  #@ "simpleKeysetExpression" }
| info = DONTCARE
    { info |> ignore;
      [ Term "_" ]  #@ "simpleKeysetExpression" }
| info = DEFAULT
    { info |> ignore;
      [ Term "DEFAULT" ]  #@ "simpleKeysetExpression" }
;
reducedSimpleKeysetExpression:
| expr = expression MASK mask = expression
    { [ NT expr; Term "&&&"; NT mask ]  #@ "reducedSimpleKeysetExpression" }
| lo = expression RANGE hi = expression
    { [ NT lo; Term ".."; NT hi ]  #@ "reducedSimpleKeysetExpression" }
| info = DONTCARE
    { info |> ignore;
      [ Term "_" ]  #@ "reducedSimpleKeysetExpression" }
| info = DEFAULT
    { info |> ignore;
      [ Term "DEFAULT" ]  #@ "reducedSimpleKeysetExpression" }
;
simpleExpressionList:
| exprs = separated_list(COMMA, simpleKeysetExpression)
    { wrap_list_v "simpleKeysetExpression" exprs }
tupleKeysetExpression:
| L_PAREN expr = simpleKeysetExpression COMMA exprs = simpleExpressionList R_PAREN
      { [ Term "("; NT expr; Term ","; NT exprs; Term ")" ]
       #@ "tupleKeysetExpression" }
| L_PAREN expr = reducedSimpleKeysetExpression R_PAREN
      { [ Term "("; NT expr; Term ")" ]
           #@ "tupleKeysetExpression" }
;
keysetExpression:
| expr = tupleKeysetExpression
| expr = simpleKeysetExpression
    { expr }
;
kvPair:
| key = name ASSIGN value = expression
    { [ NT key; Term "="; NT value ]  #@ "kvPair" }
;
%inline kvList:
| kvs = separated_nonempty_list_aux(COMMA, kvPair)
    { kvs }
;
(******** Type arguments ********)
realTypeArg:
| typeRef = typeRef
    { typeRef }
| info = VOID
    { info |> ignore;
      [ Term "VOID" ]  #@ "realTypeArg" }
| info = DONTCARE
    { info |> ignore;
      [ Term "_" ]  #@ "realTypeArg" }
;
(* TODO: is this special case necessary? *)
realTypeArgumentList:
| ts = separated_list(COMMA, realTypeArg)
    { wrap_list_v "realTypeArg" ts }
(* | t = realTypeArg *)
(*     { [ NT t ]  #@ "realTypeArgumentList" } *)
(* | t = realTypeArg COMMA ts = separated_list(COMMA, typeArg) *)
(*     { [ NT t; Term ","; NT ts ]  #@ "realTypeArgumentList" } *)
;
typeArg:
| typeRef = typeRef
    { typeRef }
| nonTypeName = nonTypeName
    { nonTypeName }
| info = VOID
    { info |> ignore;
      [ Term "VOID" ]  #@ "typeArg" }
| info = DONTCARE
    { info |> ignore;
      [ Term "_" ]  #@ "typeArg" }
;
typeArgList:
| ts = separated_list(COMMA, typeArg)
    { wrap_list_v "typeArg" ts }
;
(******** Arguments ********)
argument:
| expression = expression
    { expression }
| name = name ASSIGN expression = expression
    { [ NT name; Term "="; NT expression ]  #@ "argument" }
| info = DONTCARE
    { info |> ignore;
      [ Term "_" ]  #@ "argument" }
| name = name ASSIGN info = DONTCARE
    { info |> ignore;
      [ NT name; Term "="; Term "_" ]  #@ "argument" }
;
argumentList:
| args = separated_list(COMMA, argument)
    { wrap_list_v "argument" args }
;
(******** L-values ********)
lvalue:
| prefixedNonTypeName = prefixedNonTypeName
    { prefixedNonTypeName }
| info = THIS
    { info |> ignore;
      [ Term "THIS" ]  #@ "lvalue" }
| expr = lvalue DOT name = member
    { [ NT expr; Term "."; NT name ]  #@ "lvalue" }
| array = lvalue L_BRACKET index = expression info2 = R_BRACKET
    { info2 |> ignore;
      [ NT array; Term "["; NT index; Term "]" ]  #@ "lvalue" }
| bits = lvalue L_BRACKET hi = expression COLON lo = expression info2 = R_BRACKET
    { info2 |> ignore;
      [ NT bits; Term "["; NT hi; Term ":"; NT lo; Term "]" ]  #@ "lvalue" }
| L_PAREN expr = lvalue R_PAREN
    { [ Term "("; NT expr; Term ")" ]  #@ "lvalue" }
;
(******** Variable and constant declarations ********)
(* initializer -> initialValue due to reserved word in OCaml *)
initialValue:
| ASSIGN expression = expression
    { [ Term "="; NT expression ]  #@ "initializer" }
;
optInitializer:
| i = option(initialValue)
    { wrap_opt_v "initializer" i }
;
(* Pert4 X / Spec O *)
variableDeclarationWithoutSemicolon:
| optAnnotations = optAnnotations
  typeRef = typeRef
  name = name
  optInitializer = optInitializer
    { declare_var_of_il name false;
      [ NT optAnnotations; NT typeRef; NT name; NT optInitializer ]
       #@ "variableDeclarationWithoutSemicolon" }
;
variableDeclaration:
| variableDeclarationWithoutSemicolon = variableDeclarationWithoutSemicolon info2 = SEMICOLON
    { info2 |> ignore;
      [ NT variableDeclarationWithoutSemicolon; Term ";" ]
       #@ "variableDeclaration" }
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
      [ NT optAnnotations; Term "CONST"; NT typeRef; NT name; NT init; Term ";" ]
       #@ "constantDeclaration" }
;
(******** Statements ********)
(* Petr4 X / Spec O *)
assignmentOrMethodCallStatementWithoutSemicolon:
| func = lvalue L_PAREN args = argumentList R_PAREN
    { [ NT func; Term "("; NT args; Term ")" ]  #@ "assignmentOrMethodCallStatementWithoutSemicolon" }
| func = lvalue l_angle type_args = typeArgList r_angle
    L_PAREN args = argumentList R_PAREN
    { [ NT func; Term "<"; NT type_args; Term ">"; Term "("; NT args; Term ")" ]
       #@ "assignmentOrMethodCallStatementWithoutSemicolon" }
| lhs = lvalue ASSIGN rhs = expression
    { [ NT lhs; Term "="; NT rhs ]  #@ "assignmentOrMethodCallStatementWithoutSemicolon" }
| lhs = lvalue PLUS_ASSIGN rhs = expression
    { [ NT lhs; Term "+="; NT rhs ]  #@ "assignmentOrMethodCallStatementWithoutSemicolon" }
| lhs = lvalue PLUS_SAT_ASSIGN rhs = expression
    { [ NT lhs; Term "|+|="; NT rhs ]  #@ "assignmentOrMethodCallStatementWithoutSemicolon" }
| lhs = lvalue MINUS_ASSIGN rhs = expression
    { [ NT lhs; Term "-="; NT rhs ]  #@ "assignmentOrMethodCallStatementWithoutSemicolon" }
| lhs = lvalue MINUS_SAT_ASSIGN rhs = expression
    { [ NT lhs; Term "|-|="; NT rhs ]  #@ "assignmentOrMethodCallStatementWithoutSemicolon" }
| lhs = lvalue MUL_ASSIGN rhs = expression
    { [ NT lhs; Term "*="; NT rhs ]  #@ "assignmentOrMethodCallStatementWithoutSemicolon" }
| lhs = lvalue DIV_ASSIGN rhs = expression
    { [ NT lhs; Term "/="; NT rhs ]  #@ "assignmentOrMethodCallStatementWithoutSemicolon" }
| lhs = lvalue MOD_ASSIGN rhs = expression
    { [ NT lhs; Term "%="; NT rhs ]  #@ "assignmentOrMethodCallStatementWithoutSemicolon" }
| lhs = lvalue SHL_ASSIGN rhs = expression
    { [ NT lhs; Term "<<="; NT rhs ]  #@ "assignmentOrMethodCallStatementWithoutSemicolon" }
| lhs = lvalue SHR_ASSIGN rhs = expression
    { [ NT lhs; Term ">>="; NT rhs ]  #@ "assignmentOrMethodCallStatementWithoutSemicolon" }
| lhs = lvalue BIT_AND_ASSIGN rhs = expression
    { [ NT lhs; Term "&="; NT rhs ]  #@ "assignmentOrMethodCallStatementWithoutSemicolon" }
| lhs = lvalue BIT_XOR_ASSIGN rhs = expression
    { [ NT lhs; Term "^="; NT rhs ]  #@ "assignmentOrMethodCallStatementWithoutSemicolon" }
| lhs = lvalue BIT_OR_ASSIGN rhs = expression
    { [ NT lhs; Term "|="; NT rhs ]  #@ "assignmentOrMethodCallStatementWithoutSemicolon" }
;
assignmentOrMethodCallStatement:
| assignmentOrMethodCallStatementWithoutSemicolon = assignmentOrMethodCallStatementWithoutSemicolon info2 = SEMICOLON
    { info2 |> ignore;
      [ NT assignmentOrMethodCallStatementWithoutSemicolon; Term ";" ]
       #@ "assignmentOrMethodCallStatement" }
;
directApplication:
| namedType = namedType DOT APPLY (* Differs from Petr4, should mean same thing *)
  L_PAREN args = argumentList R_PAREN info2 = SEMICOLON
    { info2 |> ignore;
      [ NT namedType; Term "."; Term "APPLY"; Term "("; NT args; Term ")"; Term ";" ]
       #@ "directApplication" }
;
conditionalStatement:
| info1 = IF L_PAREN cond = expression R_PAREN tru = statement %prec THEN
    { info1 |> ignore;
      [ Term "IF"; Term "("; NT cond; Term ")"; NT tru ]
       #@ "conditionalStatement" }
| info1 = IF L_PAREN cond = expression R_PAREN tru = statement ELSE fls = statement
    { info1 |> ignore;
      [ Term "IF"; Term "("; NT cond; Term ")"; NT tru; Term "ELSE"; NT fls ]
       #@ "conditionalStatement" }
;
emptyStatement:
| info = SEMICOLON
    { info |> ignore;
      [ Term ";" ]  #@ "emptyStatement" }
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
       #@ "blockStatement" }
;
returnStatement:
| info1 = RETURN info2 = SEMICOLON
    { let tags = Source.merge info1 info2 in
      tags |> ignore;
      [ Term "RETURN"; Term ";" ]  #@ "returnStatement" }
| info1 = RETURN expr = expression info2 = SEMICOLON
    { let tags = Source.merge info1 info2 in
      tags |> ignore;
      [ Term "RETURN"; NT expr; Term ";" ]  #@ "returnStatement" }
;
(* Petr4 X / Spec O *)
breakStatement:
| info1 = BREAK info2 = SEMICOLON
    { let tags = Source.merge info1 info2 in
      tags |> ignore;
      [ Term "BREAK"; Term ";" ]  #@ "breakStatement" }
;
(* Petr4 X / Spec O *)
continueStatement:
| info1 = CONTINUE info2 = SEMICOLON
    { let tags = Source.merge info1 info2 in
      tags |> ignore;
      [ Term "CONTINUE"; Term ";" ]  #@ "continueStatement" }
;
exitStatement:
| info1 = EXIT info2 = SEMICOLON
    { let tags = Source.merge info1 info2 in
      tags |> ignore;
      [ Term "EXIT"; Term ";" ]  #@ "exitStatement" }
;
switchLabel:
| info = DEFAULT
    { info |> ignore;
      [ Term "DEFAULT" ]  #@ "switchLabel" }
| expr = nonBraceExpression
    { expr }
;
switchCase:
| label = switchLabel COLON code = blockStatement
    { [ NT label; Term ":"; NT code ]  #@ "switchCase" }
| label = switchLabel info2 = COLON
    { info2 |> ignore;
      [ NT label; Term ":" ]  #@ "switchCase" }
;
switchCases:
| cases = list(switchCase)
    { wrap_list_v "switchCase" cases }
;
switchStatement:
| info1 = SWITCH
  L_PAREN expr = expression R_PAREN
  L_BRACE cases = switchCases info2 = R_BRACE
    { let tags = Source.merge info1 info2 in
      tags |> ignore;
      [ Term "SWITCH"; Term "("; NT expr; Term ")"; Term "{"; NT cases; Term "}" ]
       #@ "switchStatement" }
;
(* Petr4 X / Spec O whole for statement missing *)
declOrAssignmentOrMethodCallStatement:
| variableDeclarationWithoutSemicolon = variableDeclarationWithoutSemicolon
    { variableDeclarationWithoutSemicolon }
| assignmentOrMethodCallStatementWithoutSemicolon = assignmentOrMethodCallStatementWithoutSemicolon
    { assignmentOrMethodCallStatementWithoutSemicolon }
;
forInitStatementNonEmpty:
| ls = separated_nonempty_list(COMMA, declOrAssignmentOrMethodCallStatement)
    { wrap_list_v "declOrAssignmentOrMethodCallStatement" ls }
;
forInitStatements:
| ls = separated_list(COMMA, declOrAssignmentOrMethodCallStatement)
    { wrap_list_v "declOrAssignmentOrMethodCallStatement" ls }
;
forUpdateStatementsNonEmpty:
| assignments = separated_nonempty_list(COMMA, assignmentOrMethodCallStatementWithoutSemicolon)
    { wrap_list_v "assignmentOrMethodCallStatementWithoutSemicolon" assignments }
;
forUpdateStatements:
| assignments = separated_list(COMMA, assignmentOrMethodCallStatementWithoutSemicolon)
    { wrap_list_v "assignmentOrMethodCallStatementWithoutSemicolon" assignments }
;
forCollectionExpr:
| expr = expression
    { expr }
| expr_l = expression RANGE expr_r = expression
    { [ NT expr_l; Term ".."; NT expr_r ]
       #@ "forCollectionExpr" }
;
forStatement:
| anno = optAnnotations
  FOR
  L_PAREN init = forInitStatements SEMICOLON cond = expression SEMICOLON update = forUpdateStatements R_PAREN
  body = statement
    { [ NT anno; Term "FOR"; Term "("; NT init; Term ";"; NT cond; Term ";"; NT update; Term ")"; NT body ]
       #@ "forStatement" }
| anno = optAnnotations FOR L_PAREN
    typ = typeRef name = name IN collection = forCollectionExpr R_PAREN body = statement
    { [ NT anno; Term "FOR"; Term "("; NT typ; NT name; Term "IN"; NT collection; Term ")"; NT body ]
       #@ "forStatement" }
| anno = optAnnotations FOR L_PAREN
    anno_in = optAnnotations typ = typeRef name = name IN
    collection = forCollectionExpr R_PAREN body = statement
    { [ NT anno; Term "FOR"; Term "("; NT anno_in; NT typ; NT name; Term "IN"; NT collection; Term ")"; NT body ]
       #@ "forStatement" }
;
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
| stmt = forStatement
    { stmt }
;
statementOrDeclaration:
| variableDeclaration = variableDeclaration
    { variableDeclaration }
| constantDeclaration = constantDeclaration
    { constantDeclaration }
| statement = statement
    { statement }
;
statOrDeclList:
| s = list(statementOrDeclaration)
    { wrap_list_v "statementOrDeclaration" s }
(******** Error and match kind declarations ********)
matchKindDeclaration:
| info1 = MATCH_KIND L_BRACE ids = nameList comma = optTrailingComma info2 = R_BRACE
    { let tags = Source.merge info1 info2 in
      tags |> ignore;
      declare_vars_of_il ids;
      [ Term "MATCH_KIND"; Term "{"; NT ids; NT comma; Term "}" ]  #@ "matchKindDeclaration" }
;
errorDeclaration:
| info1 = ERROR L_BRACE ids = nameList info2 = R_BRACE
    { let tags = Source.merge info1 info2 in
      tags |> ignore;
      declare_vars_of_il ids;
      [ Term "ERROR"; Term "{"; NT ids; Term "}" ]  #@ "errorDeclaration" }
;
(******** Extern declarations ********)
functionPrototype:
| typeOrVoid = typeOrVoid name = name
    push_scope
    optTypeParameters = optTypeParameters
    L_PAREN params = parameterList info2 = R_PAREN
    { info2 |> ignore;
      [ NT typeOrVoid; NT name; NT optTypeParameters; Term "("; NT params; Term ")" ]
       #@ "functionPrototype" }
;
methodPrototype:
| anno = optAnnotations proto = functionPrototype pop_scope info2 = SEMICOLON
    { info2 |> ignore;
      [ NT anno; NT proto; Term ";" ]  #@ "methodPrototype" }
| anno = optAnnotations info1 = ABSTRACT proto = functionPrototype
    pop_scope info2 = SEMICOLON
    { let tags = Source.merge info1 info2 in
      tags |> ignore;
      [ NT anno; Term "ABSTRACT"; NT proto; Term ";" ]  #@ "methodPrototype" }
(* Petr4: alias methodName in place of typeIdentifier *)
| anno = optAnnotations tid = typeIdentifier L_PAREN params = parameterList R_PAREN info2 = SEMICOLON
    { info2 |> ignore;
      [ NT anno; NT tid; Term "("; NT params; Term ")"; Term ";" ]  #@ "methodPrototype" }
;
methodPrototypes:
| protos = list(methodPrototype)
    { wrap_list_v "methodPrototype" protos }
;
externDeclaration:
| anno = optAnnotations info1 = EXTERN name = push_externName type_params = optTypeParameters
    L_BRACE protos = methodPrototypes info2 = R_BRACE pop_scope
    { let tags = Source.merge info1 info2 in
      tags |> ignore;
      let decl =
        [ NT anno; Term "EXTERN"; NT name; NT type_params; Term "{"; NT protos; Term "}" ]
         #@ "externDeclaration"
      in
      declare_type_of_il name (has_type_params_declaration decl);
      decl }
| anno = optAnnotations info1 = EXTERN proto = functionPrototype pop_scope info2 = SEMICOLON
    { let tags = Source.merge info1 info2 in
      tags |> ignore;
      let decl =
        [ NT anno; Term "EXTERN"; NT proto; Term ";" ]  #@ "externDeclaration"
      in
      declare_var (id_of_declaration decl) (has_type_params_declaration decl);
      decl }
;
(* Auxiliary from Petr4 for push_externName, changed from name to nonTypeName *)
externName:
| n = nonTypeName
    { declare_type_of_il n false;
      n }
(******** Function declarations ********)
functionDeclaration:
| optAnnotations = optAnnotations functionPrototype = functionPrototype body = blockStatement pop_scope
    { [ NT optAnnotations; NT functionPrototype; NT body ]  #@ "functionDeclaration" }
;
(******** Instantiations ********)
(* Petr4: ASSIGN kept outside *)
objInitializer:
| ASSIGN L_BRACE decls = objDeclarations info2 = R_BRACE
    { info2 |> ignore;
      [ Term "="; Term "{"; NT decls; Term "}" ]  #@ "objInitializer" }
;
instantiation:
| anno = optAnnotations typ = typeRef L_PAREN args = argumentList R_PAREN name = name info2 = SEMICOLON
    { info2 |> ignore;
      [ NT anno; NT typ; Term "("; NT args; Term ")"; NT name; Term ";" ]
       #@ "instantiation" }
| anno = optAnnotations typ = typeRef L_PAREN args = argumentList R_PAREN name = name init = objInitializer info2 = SEMICOLON
    { info2 |> ignore;
      [ NT anno; NT typ; Term "("; NT args; Term ")"; NT name; NT init; Term ";" ]
       #@ "instantiation" }
;
objDeclaration:
| functionDeclaration = functionDeclaration
    { functionDeclaration }
| instantiation = instantiation
    { instantiation }
;
objDeclarations:
| decls = list(objDeclaration)
    { wrap_list_v "objDeclaration" decls }
;
(******** Action declarations ********)
actionDeclaration:
| anno = optAnnotations info1 = ACTION name = name L_PAREN params = parameterList R_PAREN body = blockStatement
    { info1 |> ignore;
      [ NT anno; Term "ACTION"; NT name; Term "("; NT params; Term ")"; NT body ]
       #@ "actionDeclaration" }
;
(******** Table declarations ********)
keyElement:
| key = expression COLON match_kind = name anno = optAnnotations info2 = SEMICOLON
    { info2 |> ignore;
      [ NT key; Term ":"; NT match_kind; NT anno; Term ";" ]  #@ "keyElement" }
;
keyElementList:
| elements = list(keyElement)
    { wrap_list_v "keyElement" elements }
;
(* Petr4: contains optAnnotations, name = name *)
actionRef:
| name = prefixedNonTypeName
    { name }
| name = prefixedNonTypeName L_PAREN args = argumentList R_PAREN
    { [ NT name; Term "("; NT args; Term ")" ]  #@ "actionRef" }
;
(* Petr4 X / Spec O *)
action:
| optAnnotations = optAnnotations actionRef = actionRef info2 = SEMICOLON
    { info2 |> ignore;
      [ NT optAnnotations; NT actionRef; Term ";" ]  #@ "action" }
;
actionList:
(* Diff: Petr4 uses actionRef with ';' separators *)
| actions = list(action)
    { wrap_list_v "action" actions }
;
entryPriority:
| PRIORITY ASSIGN num = number COLON
    { [ Term "PRIORITY"; Term "="; NT num; Term ":" ]  #@ "entryPriority" }
| PRIORITY ASSIGN L_PAREN expr = expression R_PAREN COLON
    { [ Term "PRIORITY"; Term "="; Term "("; NT expr; Term ")"; Term ":" ]  #@ "entryPriority" }
;
entry:
| optConst = optCONST prio = entryPriority keyset = keysetExpression COLON action = actionRef anno = optAnnotations info2 = SEMICOLON
    { info2 |> ignore;
      [ NT optConst; NT prio; NT keyset; Term ":"; NT action; NT anno; Term ";" ]  #@ "entry" }
(* SPEC BUG?: actionList in Spec, actionRef in Petr4/p4c *)
| optConst = optCONST keyset = keysetExpression COLON action = actionRef anno = optAnnotations info2 = SEMICOLON
    { info2 |> ignore;
      [ NT optConst; NT keyset; Term ":"; NT action; NT anno; Term ";" ]  #@ "entry" }
;
entriesList:
| entries = list(entry)
    { wrap_list_v "entry" entries }
;
tableProperty:
| info1 = KEY ASSIGN L_BRACE keys = keyElementList info2 = R_BRACE
    { let tags = Source.merge info1 info2 in
      tags |> ignore;
      [ Term "KEY"; Term "="; Term "{"; NT keys; Term "}" ]  #@ "tableProperty" }
| info1 = ACTIONS ASSIGN L_BRACE actions = actionList info2 = R_BRACE
    { let tags = Source.merge info1 info2 in
      tags |> ignore;
      [ Term "ACTIONS"; Term "="; Term "{"; NT actions; Term "}" ]  #@ "tableProperty" }
| anno = optAnnotations optConst = optCONST info1 = ENTRIES ASSIGN L_BRACE entries = entriesList info2 = R_BRACE
    { let tags = Source.merge info1 info2 in
      tags |> ignore;
      [ NT anno; NT optConst; Term "ENTRIES"; Term "="; Term "{"; NT entries; Term "}" ]  #@ "tableProperty" }
| anno = optAnnotations optConst = optCONST name = nonTableKwName init = initialValue info2 = SEMICOLON
    { info2 |> ignore;
      [ NT anno; NT optConst; NT name; NT init; Term ";" ]  #@ "tableProperty" }
;
tablePropertyList:
| properties = list(tableProperty)
    { wrap_list_v "tableProperty" properties }
;
tableDeclaration:
| anno = optAnnotations info1 = TABLE name = name L_BRACE props = tablePropertyList info2 = R_BRACE
    { let tags = Source.merge info1 info2 in
      tags |> ignore;
      [ NT anno; Term "TABLE"; NT name; Term "{"; NT props; Term "}" ]  #@ "tableDeclaration" }
;
(******** Control and control type declarations ********)
controlBody:
| body = blockStatement
    { body }
;
controlLocalDeclaration:
| const = constantDeclaration
    { const }
| action = actionDeclaration
    { declare_var (id_of_declaration action) false;
      action }
| table = tableDeclaration
    { declare_var (id_of_declaration table) false;
      table }
| inst = instantiation
    { inst }
| var = variableDeclaration
    { var }
;
controlLocalDeclarations:
| decls = list(controlLocalDeclaration)
    { wrap_list_v "controlLocalDeclaration" decls }
;
controlTypeDeclaration:
| anno = optAnnotations info1 = CONTROL name = push_name type_params = optTypeParameters
    L_PAREN params = parameterList info2 = R_PAREN
    { let tags = Source.merge info1 info2 in
      tags |> ignore;
      [ NT anno; Term "CONTROL"; NT name; NT type_params; Term "("; NT params; Term ")" ]
       #@ "controlTypeDeclaration" }
;
controlDeclaration:
| controlTypeDeclaration = controlTypeDeclaration optConstructorParameters = optConstructorParameters
    L_BRACE locals = controlLocalDeclarations APPLY apply = controlBody info2 = R_BRACE pop_scope
    { info2 |> ignore;
      [ NT controlTypeDeclaration; NT optConstructorParameters; Term "{"; NT locals; Term "APPLY"; NT apply; Term "}" ]
       #@ "controlDeclaration" }
;
(******** Value set declarations ********)
valueSetDeclaration:
| anno = optAnnotations info1 = VALUESET l_angle base = baseType r_angle
    L_PAREN size = expression R_PAREN name = name info2 = SEMICOLON
    { let tags = Source.merge info1 info2 in
      tags |> ignore;
      [ NT anno; Term "VALUESET"; Term "<"; NT base; Term ">"; Term "("; NT size; Term ")"; NT name; Term ";" ]
       #@ "valueSetDeclaration" }
| anno = optAnnotations info1 = VALUESET l_angle tuple = tupleType r_angle
    L_PAREN size = expression R_PAREN name = name info2 = SEMICOLON
    { let tags = Source.merge info1 info2 in
      tags |> ignore;
      [ NT anno; Term "VALUESET"; Term "<"; NT tuple; Term ">"; Term "("; NT size; Term ")"; NT name; Term ";" ]
       #@ "valueSetDeclaration" }
| anno = optAnnotations info1 = VALUESET l_angle typeName = typeName r_angle
    L_PAREN size = expression R_PAREN name = name info2 = SEMICOLON
    { let tags = Source.merge info1 info2 in
      tags |> ignore;
      [ NT anno; Term "VALUESET"; Term "<"; NT typeName; Term ">"; Term "("; NT size; Term ")"; NT name; Term ";" ]
       #@ "valueSetDeclaration" }
;
(******** Select expressions ********)
selectCase:
| keysetExpression = keysetExpression COLON name = name info2 = SEMICOLON
    { info2 |> ignore;
      [ NT keysetExpression; Term ":"; NT name; Term ";" ]  #@ "selectCase" }
;
selectCaseList:
| cases = list(selectCase)
    { wrap_list_v "selectCase" cases }
;
selectExpression:
| info1 = SELECT L_PAREN exprs = expressionList R_PAREN L_BRACE cases = selectCaseList info2 = R_BRACE
    { let tags = Source.merge info1 info2 in
      tags |> ignore;
      let exprs = List.rev exprs |> wrap_list_v "expression" in
      [ Term "SELECT"; Term "("; NT exprs; Term ")"; Term "{"; NT cases; Term "}" ]
       #@ "selectExpression" }
;
(******** Transition statements ********)
stateExpression:
| name = name info2 = SEMICOLON
    { info2 |> ignore;
      [ NT name; Term ";" ]  #@ "stateExpression" }
| selectExpression = selectExpression
    { selectExpression }
;
transitionStatement:
(* SPEC-BUG? transitionStatement can be empty in Petr4/p4c for REJECT, but not in Spec*)
(* TODO: choose CaseV *)
| (* empty *) { [ Term "SPEC_BUG" ]  #@ "transitionStatement" }
| info1 = TRANSITION stateExpression = stateExpression
    { info1 |> ignore;
      [ Term "TRANSITION"; NT stateExpression ]  #@ "transitionStatement" }
;
(******** Parser and parser type declarations ********)
parserBlockStatement:
| optAnnotations = optAnnotations info1 = L_BRACE parserStatements = parserStatements info2 = R_BRACE
    { let tags = Source.merge info1 info2 in
      tags |> ignore;
      [ NT optAnnotations; Term "{"; NT parserStatements; Term "}" ]  #@ "parserBlockStatement" }
;
parserStatement:
| stmt = assignmentOrMethodCallStatement
    { stmt }
| app = directApplication
    { app }
| stmt = emptyStatement
    { stmt }
| var = variableDeclaration
    { var }
| const = constantDeclaration
    { const }
| block = parserBlockStatement
    { block }
| cond = conditionalStatement
    { cond }
;
parserStatements:
| stmts = list(parserStatement)
    { wrap_list_v "parserStatement" stmts }
;
parserState:
| anno = optAnnotations info1 = STATE name = push_name L_BRACE stmts = parserStatements trans = transitionStatement info2 = R_BRACE
    { let tags = Source.merge info1 info2 in
      tags |> ignore;
      [ NT anno; Term "STATE"; NT name; Term "{"; NT stmts; NT trans; Term "}" ]
       #@ "parserState" }
;
parserStates:
| states = list(parserState)
    { wrap_list_v "parserState" states }
;
parserTypeDeclaration:
| anno = optAnnotations info1 = PARSER name = push_name optTypeParams = optTypeParameters
    L_PAREN params = parameterList R_PAREN
    { info1 |> ignore;
      [ NT anno; Term "PARSER"; NT name; NT optTypeParams; Term "("; NT params; Term ")" ]
       #@ "parserTypeDeclaration" }
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
    { const }
| var = variableDeclaration
    { var }
| inst = instantiation
    { inst }
| valueSet = valueSetDeclaration
    { valueSet }
;
(* Petr4 X / Spec O *)
parserLocalElements:
| elements = list(parserLocalElement)
    { wrap_list_v "parserLocalElement" elements }
;
parserDeclaration:
| decl = parserTypeDeclaration params = optConstructorParameters
    L_BRACE locals = parserLocalElements states = parserStates info2 = R_BRACE pop_scope
    { info2 |> ignore;
      [ NT decl; NT params; Term "{"; NT locals; NT states; Term "}" ]
       #@ "parserDeclaration" }
;
packageTypeDeclaration:
| anno = optAnnotations info1 = PACKAGE name = push_name type_params = optTypeParameters
    L_PAREN params = parameterList info2 = R_PAREN
    { let tags = Source.merge info1 info2 in
      tags |> ignore;
      [ NT anno; Term "PACKAGE"; NT name; NT type_params; Term "("; NT params; Term ")" ]
       #@ "packageTypeDeclaration" }
;
specifiedIdentifier:
| name = name init = initialValue
    { [ NT name; NT init ]  #@ "specifiedIdentifier" }
;
(* From Petr4: opt trailing comma *)
specifiedIdentifierList:
| ids = separated_nonempty_opt_trailing_list(COMMA, specifiedIdentifier)
    { wrap_list_v "specifiedIdentifier" ids }
;
enumDeclaration:
| anno = optAnnotations info1 = ENUM name = name L_BRACE ids = nameList comma = optTrailingComma info2 = R_BRACE
    { let tags = Source.merge info1 info2 in
      tags |> ignore;
      [ NT anno; Term "ENUM"; NT name; Term "{"; NT ids; NT comma; Term "}" ]
       #@ "enumDeclaration" }
| anno = optAnnotations info1 = ENUM typeRef = typeRef name = name L_BRACE ids = specifiedIdentifierList comma = optTrailingComma info2 = R_BRACE
    { let tags = Source.merge info1 info2 in
      tags |> ignore;
      [ NT anno; Term "ENUM"; NT typeRef; NT name; Term "{"; NT ids; NT comma; Term "}" ]
       #@ "enumDeclaration" }
;
structField:
| anno = optAnnotations typeRef = typeRef name = name info2 = SEMICOLON
    { info2 |> ignore;
      [ NT anno; NT typeRef; NT name; Term ";" ]  #@ "structField" }
;
structFieldList:
| fields = list(structField)
    { wrap_list_v "structField" fields }
;
headerUnionDeclaration:
| anno = optAnnotations info1 = HEADER_UNION name = name type_params = optTypeParameters
    L_BRACE fields = structFieldList info2 = R_BRACE
    { let tags = Source.merge info1 info2 in
      tags |> ignore;
      [ NT anno; Term "HEADER_UNION"; NT name; NT type_params; Term "{"; NT fields; Term "}" ]
       #@ "headerUnionDeclaration" }
;
structTypeDeclaration:
| anno = optAnnotations info1 = STRUCT name = name type_params = optTypeParameters
    L_BRACE fields = structFieldList info2 = R_BRACE
    { let tags = Source.merge info1 info2 in
      tags |> ignore;
      [ NT anno; Term "STRUCT"; NT name; NT type_params; Term "{"; NT fields; Term "}" ]
       #@ "structTypeDeclaration" }
;
headerTypeDeclaration:
| anno = optAnnotations info1 = HEADER name = name type_params = optTypeParameters
    L_BRACE fields = structFieldList info2 = R_BRACE
    { let tags = Source.merge info1 info2 in
      tags |> ignore;
      [ NT anno; Term "HEADER"; NT name; NT type_params; Term "{"; NT fields; Term "}" ]
       #@ "headerTypeDeclaration" }
;
derivedTypeDeclaration:
| d = headerTypeDeclaration
| d = headerUnionDeclaration
| d = structTypeDeclaration
| d = enumDeclaration
    { d }
;
(* Petr4 has trailing Semicolon here instead of typeDeclaration *)
typedefDeclaration:
| anno = optAnnotations info1 = TYPEDEF typeRef = typeRef name = name
    { info1 |> ignore;
      [ NT anno; Term "TYPEDEF"; NT typeRef; NT name ]  #@ "typedefDeclaration" }
| anno = optAnnotations info1 = TYPEDEF derived = derivedTypeDeclaration name = name
    { info1 |> ignore;
      [ NT anno; Term "TYPEDEF"; NT derived; NT name ]  #@ "typedefDeclaration" }
| anno = optAnnotations info1 = TYPE typeRef = typeRef name = name
    { info1 |> ignore;
      [ NT anno; Term "TYPE"; NT typeRef; NT name ]  #@ "typedefDeclaration" }
;
typeDeclaration:
| derived = derivedTypeDeclaration
    { derived }
| typedef = typedefDeclaration info2 = SEMICOLON
    { info2 |> ignore;
      [ NT typedef; Term ";" ]  #@ "typeDeclaration" }
| parserTypeDeclaration = parserTypeDeclaration pop_scope info2 = SEMICOLON
    { info2 |> ignore;
      [ NT parserTypeDeclaration; Term ";" ]  #@ "typeDeclaration" }
| controlTypeDeclaration = controlTypeDeclaration pop_scope info2 = SEMICOLON
    { info2 |> ignore;
      [ NT controlTypeDeclaration; Term ";" ]  #@ "typeDeclaration" }
| packageTypeDeclaration = packageTypeDeclaration pop_scope info2 = SEMICOLON
    { info2 |> ignore;
      [ NT packageTypeDeclaration; Term ";" ]  #@ "typeDeclaration" }
;
declaration:
| const = constantDeclaration
    { declare_var (id_of_declaration const) (has_type_params_declaration const);
      const }
| error = errorDeclaration
    { error }
| matchKind = matchKindDeclaration
    { matchKind }
| extern = externDeclaration
    { extern }
| inst = instantiation
    { declare_var (id_of_declaration inst) false;
      inst }
| func = functionDeclaration
    { declare_var (id_of_declaration func) (has_type_params_declaration func);
      func }
| action = actionDeclaration
    { declare_var (id_of_declaration action) false;
      action }
| parserDeclaration = parserDeclaration
    { declare_type (id_of_declaration parserDeclaration) (has_type_params_declaration parserDeclaration);
      parserDeclaration }
| controlDeclaration = controlDeclaration
    { declare_type (id_of_declaration controlDeclaration) (has_type_params_declaration controlDeclaration);
      controlDeclaration }
| typeDeclaration = typeDeclaration
    { declare_type (id_of_declaration typeDeclaration) (has_type_params_declaration typeDeclaration);
      typeDeclaration }
;
annotationToken:
| UNEXPECTED_TOKEN
    { [ Term "UNEXPECTED_TOKEN" ]  #@ "annotationToken" }
| ABSTRACT
    { [ Term "ABSTRACT" ]  #@ "annotationToken" }
| ACTION
    { [ Term "ACTION" ]  #@ "annotationToken" }
| ACTIONS
    { [ Term "ACTIONS" ]  #@ "annotationToken" }
| APPLY
    { [ Term "APPLY" ]  #@ "annotationToken" }
| BOOL
    { [ Term "BOOL" ]  #@ "annotationToken" }
| BIT
    { [ Term "BIT" ]  #@ "annotationToken" }
| BREAK
    { [ Term "BREAK" ]  #@ "annotationToken" }
| CONST
    { [ Term "CONST" ]  #@ "annotationToken" }
| CONTINUE
    { [ Term "CONTINUE" ]  #@ "annotationToken" }
| CONTROL
    { [ Term "CONTROL" ]  #@ "annotationToken" }
| DEFAULT
    { [ Term "DEFAULT" ]  #@ "annotationToken" }
| ELSE
    { [ Term "ELSE" ]  #@ "annotationToken" }
| ENTRIES
    { [ Term "ENTRIES" ]  #@ "annotationToken" }
| ENUM
    { [ Term "ENUM" ]  #@ "annotationToken" }
| ERROR
    { [ Term "ERROR" ]  #@ "annotationToken" }
| EXIT
    { [ Term "EXIT" ]  #@ "annotationToken" }
| EXTERN
    { [ Term "EXTERN" ]  #@ "annotationToken" }
| FALSE
    { [ Term "FALSE" ]  #@ "annotationToken" }
| FOR
    { [ Term "FOR" ]  #@ "annotationToken" }
| HEADER
    { [ Term "HEADER" ]  #@ "annotationToken" }
| HEADER_UNION
    { [ Term "HEADER_UNION" ]  #@ "annotationToken" }
| IF
    { [ Term "IF" ]  #@ "annotationToken" }
| IN
    { [ Term "IN" ]  #@ "annotationToken" }
| INOUT
    { [ Term "INOUT" ]  #@ "annotationToken" }
| INT
    { [ Term "INT" ]  #@ "annotationToken" }
| KEY
    { [ Term "KEY" ]  #@ "annotationToken" }
| MATCH_KIND
    { [ Term "MATCH_KIND" ]  #@ "annotationToken" }
| TYPE
    { [ Term "TYPE" ]  #@ "annotationToken" }
| OUT
    { [ Term "OUT" ]  #@ "annotationToken" }
| PARSER
    { [ Term "PARSER" ]  #@ "annotationToken" }
| PACKAGE
    { [ Term "PACKAGE" ]  #@ "annotationToken" }
| PRAGMA
    { [ Term "PRAGMA" ]  #@ "annotationToken" }
| RETURN
    { [ Term "RETURN" ]  #@ "annotationToken" }
| SELECT
    { [ Term "SELECT" ]  #@ "annotationToken" }
| STATE
    { [ Term "STATE" ]  #@ "annotationToken" }
| STRING
    { [ Term "STRING" ]  #@ "annotationToken" }
| STRUCT
    { [ Term "STRUCT" ]  #@ "annotationToken" }
| SWITCH
    { [ Term "SWITCH" ]  #@ "annotationToken" }
| TABLE
    { [ Term "TABLE" ]  #@ "annotationToken" }
| THIS
    { [ Term "THIS" ]  #@ "annotationToken" }
| TRANSITION
    { [ Term "TRANSITION" ]  #@ "annotationToken" }
| TRUE
    { [ Term "TRUE" ]  #@ "annotationToken" }
| TUPLE
    { [ Term "TUPLE" ]  #@ "annotationToken" }
| TYPEDEF
    { [ Term "TYPEDEF" ]  #@ "annotationToken" }
| VARBIT
    { [ Term "VARBIT" ]  #@ "annotationToken" }
| VALUESET
    { [ Term "VALUESET" ]  #@ "annotationToken" }
| LIST
    { [ Term "LIST" ]  #@ "annotationToken" }
| VOID
    { [ Term "VOID" ]  #@ "annotationToken" }
| DONTCARE
    { [ Term "_" ]  #@ "annotationToken" }
| id = identifier
    { id }
| tid = typeIdentifier
    { tid }
| str = stringLiteral
    { str }
| num = number
    { num }
| MASK
    { [ Term "&&&" ]  #@ "annotationToken" }
  (* TODO: missing DOTS "..." in spec *)
| RANGE
    { [ Term ".." ]  #@ "annotationToken" }
| SHL
    { [ Term "<<" ]  #@ "annotationToken" }
| AND
    { [ Term "&&" ]  #@ "annotationToken" }
| OR
    { [ Term "||" ]  #@ "annotationToken" }
| EQ
    { [ Term "==" ]  #@ "annotationToken" }
| NE
    { [ Term "!=" ]  #@ "annotationToken" }
| GE
    { [ Term ">=" ]  #@ "annotationToken" }
| LE
    { [ Term "<=" ]  #@ "annotationToken" }
| PLUSPLUS
    { [ Term "++" ]  #@ "annotationToken" }
| PLUS
    { [ Term "+" ]  #@ "annotationToken" }
| PLUS_SAT
    { [ Term "|+|" ]  #@ "annotationToken" }
| MINUS
    { [ Term "-" ]  #@ "annotationToken" }
| MINUS_SAT
    { [ Term "|-|" ]  #@ "annotationToken" }
| MUL
    { [ Term "*" ]  #@ "annotationToken" }
| DIV
    { [ Term "/" ]  #@ "annotationToken" }
| MOD
    { [ Term "%" ]  #@ "annotationToken" }
| BIT_OR
    { [ Term "|" ]  #@ "annotationToken" }
| BIT_AND
    { [ Term "&" ]  #@ "annotationToken" }
| BIT_XOR
    { [ Term "^" ]  #@ "annotationToken" }
| COMPLEMENT
    { [ Term "~" ]  #@ "annotationToken" }
| L_BRACKET
    { [ Term "[" ]  #@ "annotationToken" }
| R_BRACKET
    { [ Term "]" ]  #@ "annotationToken" }
| L_BRACE
    { [ Term "{" ]  #@ "annotationToken" }
| R_BRACE
    { [ Term "}" ]  #@ "annotationToken" }
| L_ANGLE
    { [ Term "<" ]  #@ "annotationToken" }
| R_ANGLE
    { [ Term ">" ]  #@ "annotationToken" }
| NOT
    { [ Term "!" ]  #@ "annotationToken" }
| COLON
    { [ Term ":" ]  #@ "annotationToken" }
| COMMA
    { [ Term "," ]  #@ "annotationToken" }
| QUESTION
    { [ Term "?" ]  #@ "annotationToken" }
| DOT
    { [ Term "." ]  #@ "annotationToken" }
| ASSIGN
    { [ Term "=" ]  #@ "annotationToken" }
| SEMICOLON
    { [ Term ";" ]  #@ "annotationToken" }
| AT
    { [ Term "@" ]  #@ "annotationToken" }
;
simpleAnnotation:
| L_PAREN body = simpleAnnotationBody R_PAREN
    { [ Term "("; NT body; Term ")" ]  #@ "simpleAnnotation" }
| token = annotationToken
    { token }
;
simpleAnnotationBody:
| body = list(simpleAnnotation)
    { wrap_list_v "simpleAnnotation" body }
;
structuredAnnotationBody:
| exprs = expressionList comma = optTrailingComma
    { let exprs = List.rev exprs |> wrap_list_v "expression" in
      [ NT exprs; NT comma ]  #@ "structuredAnnotationBody" }
| kvs = kvList comma = optTrailingComma
    { let kvs = List.rev kvs |> wrap_list_v "kvPair" in
      [ NT kvs; NT comma ]  #@ "structuredAnnotationBody" }
;
annotation:
| info1 = AT name = name
    { info1 |> ignore;
      [ Term "@"; NT name ]  #@ "annotation" }
| info1 = AT name = name L_PAREN body = simpleAnnotationBody R_PAREN
    { info1 |> ignore;
      [ Term "@"; NT name; Term "("; NT body; Term ")" ]  #@ "annotation" }
| info1 = AT name = name L_BRACKET body = structuredAnnotationBody R_BRACKET
    { info1 |> ignore;
      [ Term "@"; NT name; Term "["; NT body; Term "]" ]  #@ "annotation" }
(* From Petr4: PRAGMA not in Spec, but in Petr4/p4c *)
| info1 = PRAGMA name = name body = simpleAnnotationBody info2 = PRAGMA_END
    { let tags = Source.merge info1 info2 in
      tags |> ignore;
      [ Term "PRAGMA"; NT name; NT body; Term "PRAGMA_END" ]  #@ "annotation" }
;
(* HACK to solve reduce issues with built-in list *)
annotations:
| (* empty *)
    { wrap_list_v "annotation" [] }
| annotations = annotations annotation = annotation
    {
      let rest = match annotations.it with ListV xs -> xs | _ -> failwith "Expected ListV" in
      wrap_list_v "annotation" (rest @ [annotation])
    }
;
%inline optAnnotations:
| annos = option(annotations)
    { wrap_opt_v "annotation" annos }
  ;
(******** P4 program ********)
(* From Petr4: separator can be both semicolon or whitespace*)
declarationList:
| (* empty *) { [] }
| SEMICOLON ds = declarationList
    { ds }
| d = declaration ds = declarationList
    { d :: ds }
;
p4program:
| ds = declarationList END
    { wrap_list_v "declaration" ds }
;
