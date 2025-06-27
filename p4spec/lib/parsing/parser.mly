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
%token<Source.info> ABSTRACT ACTION ACTIONS APPLY BOOL BIT BREAK CONST CONTINUE CONTROL DEFAULT
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
%start <Il.Ast.value> variableDeclaration
%start <Il.Ast.value> typeDeclaration

(**************************** TYPES ******************************)
%type <Il.Ast.value> declaration
%type <Il.Ast.value> expressionOptTrailingList optTrailingComma kvList const optCONST number stringLiteral dotPrefix identifier typeIdentifier nonTypeName prefixedNonTypeName nonTableKwName prefixedType typeName name identifierList member direction baseType specializedType namedType headerStackType listType tupleType typeRef typeOrVoid typeParameterList typeParameters optTypeParameters parameter parameterList constructorParameters optConstructorParameters nonBraceExpression expression expressionList simpleKeysetExpression reducedSimpleKeysetExpression simpleExpressionList tupleKeysetExpression keysetExpression kvTrailingList kvOptTrailingList realTypeArg realTypeArgumentList typeArg typeArgumentList argument argumentList lvalue initialValue optInitializer variableDeclarationWithoutSemicolon constantDeclaration assignmentOrMethodCallStatementWithoutSemicolon assignmentOrMethodCallStatement directApplication conditionalStatement emptyStatement blockStatement returnStatement breakStatement continueStatement exitStatement switchLabel switchCase switchCases switchStatement statement statementOrDeclaration statOrDeclList matchKindDeclaration errorDeclaration functionPrototype methodPrototype methodPrototypes externDeclaration externName functionDeclaration objInitializer instantiation objDeclaration objDeclarations actionDeclaration keyElement keyElementList actionRef action actionList entryPriority entry entriesList
%type <Il.Ast.value> annotation annotationToken annotations controlBody controlDeclaration controlLocalDeclaration controlLocalDeclarations controlTypeDeclaration derivedTypeDeclaration enumDeclaration headerTypeDeclaration headerUnionDeclaration packageTypeDeclaration parserBlockStatement parserDeclaration parserLocalElement parserLocalElements parserState parserStatement parserStatements parserStates parserTypeDeclaration selectCase selectCaseList selectExpression simpleAnnotation simpleAnnotationBody specifiedIdentifier specifiedIdentifierList stateExpression structField structFieldList structTypeDeclaration structuredAnnotationBody tableDeclaration tableProperty tablePropertyList transitionStatement typedefDeclaration valueSetDeclaration
%type <Il.Ast.value list> lib_parsing_parser_list(action) lib_parsing_parser_list(annotation) lib_parsing_parser_list(controlLocalDeclaration) lib_parsing_parser_list(declaration) lib_parsing_parser_list(entry) lib_parsing_parser_list(keyElement) lib_parsing_parser_list(methodPrototype) lib_parsing_parser_list(objDeclaration) lib_parsing_parser_list(parserLocalElement) lib_parsing_parser_list(parserState) lib_parsing_parser_list(parserStatement) lib_parsing_parser_list(selectCase) lib_parsing_parser_list(simpleAnnotation) lib_parsing_parser_list(statementOrDeclaration) lib_parsing_parser_list(structField) lib_parsing_parser_list(switchCase) lib_parsing_parser_list(tableProperty)
%type <Il.Ast.value list> lib_parsing_parser_separated_list(COMMA,argument) lib_parsing_parser_separated_list(COMMA,expression) lib_parsing_parser_separated_list(COMMA,parameter) lib_parsing_parser_separated_list(COMMA,realTypeArg) lib_parsing_parser_separated_list(COMMA,simpleKeysetExpression) lib_parsing_parser_separated_list(COMMA,specifiedIdentifier) lib_parsing_parser_separated_list(COMMA,typeArg)
%type <Il.Ast.value list> lib_parsing_parser_separated_nonempty_list(COMMA,__anonymous_0) lib_parsing_parser_separated_nonempty_list(COMMA,kvPair) lib_parsing_parser_separated_nonempty_list(COMMA,name)
%type <Il.Ast.value list> list_aux(action) list_aux(annotation) list_aux(controlLocalDeclaration) list_aux(declaration) list_aux(entry) list_aux(keyElement) list_aux(methodPrototype) list_aux(objDeclaration) list_aux(parserLocalElement) list_aux(parserState) list_aux(parserStatement) list_aux(selectCase) list_aux(simpleAnnotation) list_aux(statementOrDeclaration) list_aux(structField) list_aux(switchCase) list_aux(tableProperty)
%type <Il.Ast.value list> separated_list_aux(COMMA,argument) separated_list_aux(COMMA,expression) separated_list_aux(COMMA,parameter) separated_list_aux(COMMA,realTypeArg) separated_list_aux(COMMA,simpleKeysetExpression) separated_list_aux(COMMA,specifiedIdentifier) separated_list_aux(COMMA,typeArg)
%type <Il.Ast.value list> separated_nonempty_list_aux(COMMA,__anonymous_0) separated_nonempty_list_aux(COMMA,kvPair) separated_nonempty_list_aux(COMMA,name)
%type <Il.Ast.value list> separated_nonempty_opt_trailing_list(COMMA,kvPair) separated_nonempty_trailing_list(COMMA,kvPair) separated_opt_trailing_list(COMMA,expression)
%type <Il.Ast.value> push_name push_externName
%type <unit> push_scope pop_scope go_toplevel go_local
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
    { [ Term "EMPTY" ] |> wrap_case_v |> with_typ (wrap_var_t "optTrailingComma") }
| COMMA
    { [ Term "," ] |> wrap_case_v |> with_typ (wrap_var_t "optTrailingComma") }
;
(* TODO: convert *)
kvList:
| kvs = separated_nonempty_list(COMMA, kvPair)
    { wrap_list_v kvs "kvPair" }
;

(**************************** P4-16 GRAMMAR ******************************)

(******** Misc ********)
(** TODO: are these necessary? **)
(* trailingComma: *)
(* optTrailingComma: *)
const:
| CONST
    { [ Term "CONST" ] |> wrap_case_v |> with_typ (wrap_var_t "const") }
;

optCONST:
| c = option(const)
    { wrap_opt_v c "const" }
            ;
(******** Numbers ********)
(* Processed by lexer *)
number:
| number = NUMBER
    { fst number }
          ;
(******** Strings ********)
(* Petr4 X / Spec O *)
stringLiteral:
| text = STRING_LITERAL
    { [ NT text; Term "PHTM_2" ] |> wrap_case_v |> with_typ (wrap_var_t "stringLiteral")}
;
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
    { identifier }
| info = APPLY
    { info |> ignore;
      [ Term "APPLY" ] |> wrap_case_v |> with_typ (wrap_var_t "nonTypeName") }
| info = KEY
    { info |> ignore;
      [ Term "KEY" ] |> wrap_case_v |> with_typ (wrap_var_t "nonTypeName") }
| info = ACTIONS
    { info |> ignore;
      [ Term "ACTIONS" ] |> wrap_case_v |> with_typ (wrap_var_t "nonTypeName") }
| info = STATE
    { info |> ignore;
      [ Term "STATE" ] |> wrap_case_v |> with_typ (wrap_var_t "nonTypeName") }
| info = ENTRIES
    { info |> ignore;
      [ Term "ENTRIES" ] |> wrap_case_v |> with_typ (wrap_var_t "nonTypeName") }
| info = TYPE
    { info |> ignore;
      [ Term "TYPE" ] |> wrap_case_v |> with_typ (wrap_var_t "nonTypeName") }
| info = PRIORITY
    { info |> ignore;
      [ Term "PRIORITY" ] |> wrap_case_v |> with_typ (wrap_var_t "nonTypeName") }
;

prefixedNonTypeName:
| nonTypeName = nonTypeName
    { nonTypeName }
| dotPrefix = dotPrefix go_toplevel nonTypeName = nonTypeName go_local
    { [ NT dotPrefix; NT nonTypeName ] |> wrap_case_v |> with_typ (wrap_var_t "prefixedNonTypeName") }
;

(* Petr4: nonTableKwName + tableKwName *)
nonTableKwName:
| nonTypeName = nonTypeName
    { nonTypeName }
;

(* Petr4: prefixedType + prefixedTypeName *)
prefixedType:
| typeIdentifier = typeIdentifier
    { typeIdentifier }
| dotPrefix = dotPrefix go_toplevel typeIdentifier = typeIdentifier go_local
    { 
      [ NT dotPrefix; NT typeIdentifier ] |> wrap_case_v |> with_typ (wrap_var_t "prefixedType")
    }
;

typeName:
| prefixedType = prefixedType
    { prefixedType }
;

name:
| nonTypeName = nonTypeName 
    { nonTypeName }
| info = LIST
    { info |> ignore;
      [ Term "LIST" ] |> wrap_case_v |> with_typ (wrap_var_t "name") }
| typeIdentifier = typeIdentifier
    { typeIdentifier }
;

identifierList:
| ids = separated_nonempty_list(COMMA, id = name { id })
    { wrap_list_v ids "identifier" }
;
member:
| name = name
    { name }
;
(******** Directions ********)
direction:
| IN
    { [ Term "IN" ] |> wrap_case_v |> with_typ (wrap_var_t "direction") }
| OUT
    { [ Term "OUT" ] |> wrap_case_v |> with_typ (wrap_var_t "direction") }
| INOUT
    { [ Term "INOUT" ] |> wrap_case_v |> with_typ (wrap_var_t "direction") }
| (* empty *)
    { [ Term "NONE" ] |> wrap_case_v |> with_typ (wrap_var_t "direction") }


(******** Types ********)
(** TODO **)

baseType:
| info = BOOL
    { info |> ignore;
      [ Term "BOOL" ] |> wrap_case_v |> with_typ (wrap_var_t "baseType") }
| info = MATCH_KIND
    { info |> ignore;
      [ Term "MATCH_KIND" ] |> wrap_case_v |> with_typ (wrap_var_t "baseType") }
| info = ERROR
    { info |> ignore;
      [ Term "ERROR" ] |> wrap_case_v |> with_typ (wrap_var_t "baseType") }
| info = BIT
    { info |> ignore;
      [ Term "BIT" ] |> wrap_case_v |> with_typ (wrap_var_t "baseType") }
| info = STRING
    { info |> ignore;
      [ Term "STRING" ] |> wrap_case_v |> with_typ (wrap_var_t "baseType") }
| info = INT
    { info |> ignore;
      [ Term "INT" ] |> wrap_case_v |> with_typ (wrap_var_t "baseType") }
(* TODO: int handling *)
| info1 = BIT l_angle value = number info_r = r_angle
    { let tags = Source.merge info1 info_r in
      tags |> ignore;
      [ Term "BIT"; Term "<"; NT value; Term ">" ]
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
    { t }
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
      [ Term "LIST"; Term "<"; NT typeArg; Term ">" ]
      |> wrap_case_v 
      |> with_typ (wrap_var_t "listType") }
;
tupleType:
| info1 = TUPLE l_angle typeArgumentList = typeArgumentList info_r = r_angle
    { let tags = Source.merge info1 info_r in
      tags |> ignore;
      [ Term "TUPLE"; Term "<"; NT typeArgumentList; Term ">" ]
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
    { t }
;

typeOrVoid:
| typeRef = typeRef
    { typeRef }
| info = VOID
    { info |> ignore;
      [ Term "VOID" ] |> wrap_case_v |> with_typ (wrap_var_t "typeOrVoid") }
(* Petr4 O / Spec X: HACK for generic return type *)
| name = name
    { match flatten_case_v name with
        | "identifier", [ ["$"]; [] ], [ value_text ]  ->
          [ Term "@"; NT value_text ]
          |> wrap_case_v 
          |> with_typ (wrap_var_t "typeIdentifier")
        | _ -> failwith "@typeOrVoid: expected identifier" }
;

(******** Type parameters ********)

typeParameterList:
| names = separated_nonempty_list(COMMA, name) 
    { wrap_list_v names "typeParameterList" }
;
typeParameters:
| l_angle type_params = typeParameterList r_angle
    { declare_types_of_il type_params;
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
    { declare_vars (List.map id_of_parameter params);
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
    { number }
| stringLiteral = stringLiteral
    { stringLiteral }
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
      [ Term "ERROR"; Term "."; NT member ]
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

expression:
| num = number
    { num }
| info1 = DOTS
    { info1 |> ignore;
      [ Term "..." ] |> wrap_case_v |> with_typ (wrap_var_t "expression") }
| info1 = TRUE
    { info1 |> ignore;
      [ Term "TRUE" ] |> wrap_case_v |> with_typ (wrap_var_t "expression") }
| info1 = FALSE
    { info1 |> ignore;
      [ Term "FALSE" ] |> wrap_case_v |> with_typ (wrap_var_t "expression") }
| value = STRING_LITERAL
    { value }
| info1 = THIS
    { info1 |> ignore;
      [ Term "THIS" ] |> wrap_case_v |> with_typ (wrap_var_t "expression") }
| name = nonTypeName
    { name }
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
      [ Term "INVALID" ] |> wrap_case_v |> with_typ (wrap_var_t "expression") }
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
      [ Term "ERROR"; Term "."; NT name ] |> wrap_case_v |> with_typ (wrap_var_t "expression") }
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
    { wrap_list_v exprs "expression" }
;

simpleKeysetExpression:
| expr = expression
    { expr }
| expr = expression MASK mask = expression
    { [ NT expr; Term "&&&"; NT mask ] |> wrap_case_v |> with_typ (wrap_var_t "simpleKeysetExpression") }
| lo = expression RANGE hi = expression
    { [ NT lo; Term ".."; NT hi ] |> wrap_case_v |> with_typ (wrap_var_t "simpleKeysetExpression") }
| info = DONTCARE
    { info |> ignore;
      [ Term "_" ] |> wrap_case_v |> with_typ (wrap_var_t "simpleKeysetExpression") }
| info = DEFAULT
    { info |> ignore;
      [ Term "DEFAULT" ] |> wrap_case_v |> with_typ (wrap_var_t "simpleKeysetExpression") }
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
      [ Term "DEFAULT" ] |> wrap_case_v |> with_typ (wrap_var_t "reducedSimpleKeysetExpression") }
;

simpleExpressionList:
| exprs = separated_list(COMMA, simpleKeysetExpression)
    { wrap_list_v exprs "simpleKeysetExpression" }

tupleKeysetExpression:
| L_PAREN expr = simpleKeysetExpression COMMA exprs = simpleExpressionList R_PAREN
              { [ Term "("; NT expr; Term ","; NT exprs; Term ")" ]
      |> wrap_case_v |> with_typ (wrap_var_t "tupleKeysetExpression") }
| L_PAREN expr = reducedSimpleKeysetExpression R_PAREN
      { [ Term "("; NT expr; Term ")"; Term "PHTM_19" ]
          |> wrap_case_v |> with_typ (wrap_var_t "tupleKeysetExpression") }
;

keysetExpression:
| expr = tupleKeysetExpression
| expr = simpleKeysetExpression
    { expr }
;

%inline kvPair:
| key = name ASSIGN value = expression 
    { [ NT key; Term "="; NT value ] |> wrap_case_v |> with_typ (wrap_var_t "kvPair") }
;

kvTrailingList:
| kvs = separated_nonempty_trailing_list(COMMA, kvPair)
    { wrap_list_v kvs "kvPair" }
;

kvOptTrailingList:
| kvs = separated_nonempty_opt_trailing_list(COMMA, kvPair)
    { wrap_list_v kvs "kvPair" }
;

(******** Type arguments ********)

realTypeArg:
| typeRef = typeRef
    { typeRef }
| info = VOID
    { info |> ignore;
      [ Term "VOID" ] |> wrap_case_v |> with_typ (wrap_var_t "realTypeArg") }
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
    { typeRef }
| nonTypeName = nonTypeName
    { nonTypeName }
| info = VOID
    { info |> ignore;
      [ Term "VOID" ] |> wrap_case_v |> with_typ (wrap_var_t "typeArg") }
| info = DONTCARE
    { info |> ignore;
      [ Term "_" ] |> wrap_case_v |> with_typ (wrap_var_t "typeArg") }
;

typeArgumentList:
| ts = separated_list(COMMA, typeArg)
    { wrap_list_v ts "typeArg" }
;

(******** Arguments ********)

argument:
| expression = expression
    { expression }
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
    { wrap_list_v args "argument" }
;
(******** L-values ********)
(** TODO **)

lvalue:
| prefixedNonTypeName = prefixedNonTypeName
    { prefixedNonTypeName }
| info = THIS
    { info |> ignore;
      [ Term "THIS" ] |> wrap_case_v |> with_typ (wrap_var_t "lvalue") }
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
      [ NT optAnnotations; Term "CONST"; NT typeRef; NT name; NT init; Term ";" ] 
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
| namedType = namedType DOT APPLY (* Differs from Petr4, should mean same thing *)
  L_PAREN args = argumentList R_PAREN info2 = SEMICOLON
    { info2 |> ignore;
      [ NT namedType; Term "."; Term "APPLY"; Term "("; NT args; Term ")"; Term ";" ]
      |> wrap_case_v |> with_typ (wrap_var_t "directApplication") }
;

conditionalStatement:
| info1 = IF L_PAREN cond = expression R_PAREN tru = statement %prec THEN
    { info1 |> ignore;
      [ Term "IF"; Term "("; NT cond; Term ")"; NT tru ]
      |> wrap_case_v |> with_typ (wrap_var_t "conditionalStatement") }
| info1 = IF L_PAREN cond = expression R_PAREN tru = statement ELSE fls = statement
    { info1 |> ignore;
      [ Term "IF"; Term "("; NT cond; Term ")"; NT tru; Term "ELSE"; NT fls ]
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
      [ Term "RETURN"; Term ";" ] |> wrap_case_v |> with_typ (wrap_var_t "returnStatement") }
| info1 = RETURN expr = expression info2 = SEMICOLON
    { let tags = Source.merge info1 info2 in
      tags |> ignore;
      [ Term "RETURN"; NT expr; Term ";" ] |> wrap_case_v |> with_typ (wrap_var_t "returnStatement") }
;

(* Petr4 X / Spec O *)
breakStatement:
| info1 = BREAK info2 = SEMICOLON
    { let tags = Source.merge info1 info2 in
      tags |> ignore;
      [ Term "BREAK"; Term ";" ] |> wrap_case_v |> with_typ (wrap_var_t "breakStatement") }
;

(* Petr4 X / Spec O *)
continueStatement:
| info1 = CONTINUE info2 = SEMICOLON
    { let tags = Source.merge info1 info2 in
      tags |> ignore;
      [ Term "CONTINUE"; Term ";" ] |> wrap_case_v |> with_typ (wrap_var_t "continueStatement") }
;

exitStatement:
| info1 = EXIT info2 = SEMICOLON
    { let tags = Source.merge info1 info2 in
      tags |> ignore;
      [ Term "EXIT"; Term ";" ] |> wrap_case_v |> with_typ (wrap_var_t "exitStatement") }
;


switchLabel:
| info = DEFAULT
    { info |> ignore;
      [ Term "DEFAULT" ] |> wrap_case_v |> with_typ (wrap_var_t "switchLabel") }
| expr = nonBraceExpression
    { expr }
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
    { wrap_list_v cases "switchCase" }
;

(* TODO: convert *)
switchStatement:
| info1 = SWITCH
  L_PAREN expr = expression R_PAREN
  L_BRACE cases = switchCases info2 = R_BRACE
    { let tags = Source.merge info1 info2 in
      tags |> ignore;
      [ Term "SWITCH"; Term "("; NT expr; Term ")"; Term "{"; NT cases; Term "}" ]
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
    { wrap_list_v s "statementOrDeclaration" }


(******** Error and match kind declarations ********)

matchKindDeclaration:
| info1 = MATCH_KIND L_BRACE ids = identifierList comma = optTrailingComma info2 = R_BRACE
    { let tags = Source.merge info1 info2 in
      tags |> ignore;
      declare_vars_of_il ids;
      [ Term "MATCH_KIND"; Term "{"; NT ids; NT comma; Term "}" ] |> wrap_case_v |> with_typ (wrap_var_t "matchKindDeclaration") }
;

errorDeclaration:
| info1 = ERROR L_BRACE ids = identifierList info2 = R_BRACE
    { let tags = Source.merge info1 info2 in
      tags |> ignore;
      declare_vars_of_il ids;
      [ Term "ERROR"; Term "{"; NT ids; Term "}" ] |> wrap_case_v |> with_typ (wrap_var_t "errorDeclaration") }
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
      [ NT anno; Term "ABSTRACT"; NT proto; Term ";" ] |> wrap_case_v |> with_typ (wrap_var_t "methodPrototype") }
(* Petr4: alias methodName in place of typeIdentifier *)
| anno = optAnnotations tid = typeIdentifier L_PAREN params = parameterList info2 = R_PAREN
    { info2 |> ignore;
      [ NT anno; NT tid; Term "("; NT params; Term ")"; Term ";" ] |> wrap_case_v |> with_typ (wrap_var_t "methodPrototype") }
;

methodPrototypes:
| protos = list(methodPrototype)
    { wrap_list_v protos "methodPrototype" }
;

externDeclaration:
| anno = optAnnotations info1 = EXTERN name = push_externName type_params = optTypeParameters
    L_BRACE protos = methodPrototypes info2 = R_BRACE pop_scope
    { let tags = Source.merge info1 info2 in
      tags |> ignore;
      let decl = 
        [ NT anno; Term "EXTERN"; NT name; NT type_params; Term "{"; NT protos; Term "}" ]
        |> wrap_case_v |> with_typ (wrap_var_t "externDeclaration")
      in
      declare_type_of_il name (has_typ_params_declaration decl);
      decl }
| anno = optAnnotations info1 = EXTERN proto = functionPrototype pop_scope info2 = SEMICOLON
    { let tags = Source.merge info1 info2 in
      tags |> ignore;
      let decl = 
        [ NT anno; Term "EXTERN"; NT proto; Term ";" ] |> wrap_case_v |> with_typ (wrap_var_t "externDeclaration") 
      in
      declare_var (name_of_any_declaration decl) (has_typ_params_declaration decl);
      decl }
;

(* Auxiliary from Petr4 for push_externName, changed from name to nonTypeName *)
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
    { functionDeclaration }
| instantiation = instantiation
    { instantiation }
;

objDeclarations:
| decls = list(objDeclaration)
    { wrap_list_v decls "objDeclaration" }
;

(******** Action declarations ********)

actionDeclaration:
| anno = optAnnotations info1 = ACTION name = name L_PAREN params = parameterList R_PAREN body = blockStatement
    { info1 |> ignore;
      [ NT anno; Term "ACTION"; NT name; Term "("; NT params; Term ")"; NT body ]
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
    { wrap_list_v elements "keyElement" }
;

(* Petr4: contains optAnnotations, name = name *)
actionRef:
| name = prefixedNonTypeName
    { name }
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
(* Diff: Petr4 uses actionRef with ';' separators *)
| actions = list(action)
    { wrap_list_v actions "action" }
;

entryPriority:
| PRIORITY ASSIGN num = number COLON
    { [ Term "PRIORITY"; Term "="; NT num; Term ":" ] |> wrap_case_v |> with_typ (wrap_var_t "entryPriority") }
| PRIORITY ASSIGN L_PAREN expr = expression R_PAREN COLON
    { [ Term "PRIORITY"; Term "="; Term "("; NT expr; Term ")"; Term ":" ] |> wrap_case_v |> with_typ (wrap_var_t "entryPriority") }
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
    { wrap_list_v entries "entry" }
;

tableProperty:
| info1 = KEY ASSIGN L_BRACE keys = keyElementList info2 = R_BRACE
    { let tags = Source.merge info1 info2 in
      tags |> ignore;
      [ Term "KEY"; Term "="; Term "{"; NT keys; Term "}" ] |> wrap_case_v |> with_typ (wrap_var_t "tableProperty") }
| info1 = ACTIONS ASSIGN L_BRACE actions = actionList info2 = R_BRACE
    { let tags = Source.merge info1 info2 in
      tags |> ignore;
      [ Term "ACTIONS"; Term "="; Term "{"; NT actions; Term "}" ] |> wrap_case_v |> with_typ (wrap_var_t "tableProperty") }
| anno = optAnnotations optConst = optCONST info1 = ENTRIES ASSIGN L_BRACE entries = entriesList info2 = R_BRACE
    { let tags = Source.merge info1 info2 in
      tags |> ignore;
      [ NT anno; NT optConst; Term "ENTRIES"; Term "="; Term "{"; NT entries; Term "}" ] |> wrap_case_v |> with_typ (wrap_var_t "tableProperty") }
(* Petr4 O / Spec X : Spec bug *)
(* | optConst = optCONST DEFAULT_ACTION ASSIGN actionRef = actionRef info2 = SEMICOLON *)
(*     { info2 |> ignore; *)
(*       [ NT optConst; Term "DEFAULT_ACTION"; Term "="; NT actionRef; Term ";" ] |> wrap_case_v |> with_typ (wrap_var_t "tableProperty") } *)
| anno = optAnnotations optConst = optCONST name = nonTableKwName init = initialValue info2 = SEMICOLON
    { info2 |> ignore;
      [ NT anno; NT optConst; NT name; NT init; Term ";" ] |> wrap_case_v |> with_typ (wrap_var_t "tableProperty") }
;

tablePropertyList:
| properties = list(tableProperty)
    { wrap_list_v properties "tableProperty" }
;

tableDeclaration:
| anno = optAnnotations info1 = TABLE name = name L_BRACE props = tablePropertyList info2 = R_BRACE
    { let tags = Source.merge info1 info2 in
      tags |> ignore;
      [ NT anno; Term "TABLE"; NT name; Term "{"; NT props; Term "}" ] |> wrap_case_v |> with_typ (wrap_var_t "tableDeclaration") }
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
    { declare_var (name_of_any_declaration action) false;
      action }
| table = tableDeclaration
    { declare_var (name_of_any_declaration table) false;
        table }
| inst = instantiation
    { inst }
| var = variableDeclaration
    { var }
;

controlLocalDeclarations:
| decls = list(controlLocalDeclaration)
    { wrap_list_v decls "controlLocalDeclaration" }
;

controlTypeDeclaration:
| anno = optAnnotations info1 = CONTROL name = push_name type_params = optTypeParameters
    L_PAREN params = parameterList info2 = R_PAREN
    { let tags = Source.merge info1 info2 in
      tags |> ignore;
      [ NT anno; Term "CONTROL"; NT name; NT type_params; Term "("; NT params; Term ")" ]
      |> wrap_case_v |> with_typ (wrap_var_t "controlTypeDeclaration") }
;

controlDeclaration:
| controlTypeDeclaration = controlTypeDeclaration optConstructorParameters = optConstructorParameters
    L_BRACE locals = controlLocalDeclarations APPLY apply = controlBody info2 = R_BRACE pop_scope
    { info2 |> ignore; 
      [ NT controlTypeDeclaration; NT optConstructorParameters; Term "{"; NT locals; Term "APPLY"; NT apply; Term "}" ]
      |> wrap_case_v |> with_typ (wrap_var_t "controlDeclaration") }
;

(******** Value set declarations ********)

valueSetDeclaration:
| anno = optAnnotations info1 = VALUESET l_angle base = baseType r_angle
    L_PAREN size = expression R_PAREN name = name info2 = SEMICOLON
    { let tags = Source.merge info1 info2 in
      tags |> ignore;
      [ NT anno; Term "VALUESET"; Term "<"; NT base; Term ">"; Term "("; NT size; Term ")"; NT name; Term ";" ]
      |> wrap_case_v |> with_typ (wrap_var_t "valueSetDeclaration") }
| anno = optAnnotations info1 = VALUESET l_angle tuple = tupleType r_angle
    L_PAREN size = expression R_PAREN name = name info2 = SEMICOLON
    { let tags = Source.merge info1 info2 in
      tags |> ignore;
      [ NT anno; Term "VALUESET"; Term "<"; NT tuple; Term ">"; Term "("; NT size; Term ")"; NT name; Term ";"; Term "PHTM_17"]
      |> wrap_case_v |> with_typ (wrap_var_t "valueSetDeclaration") }
| anno = optAnnotations info1 = VALUESET l_angle typeName = typeName r_angle
    L_PAREN size = expression R_PAREN name = name info2 = SEMICOLON
    { let tags = Source.merge info1 info2 in
      tags |> ignore;
      [ NT anno; Term "VALUESET"; Term "<"; NT typeName; Term ">"; Term "("; NT size; Term ")"; NT name; Term ";"; Term "PHTM_18"]
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
    { wrap_list_v cases "selectCase" }
;

selectExpression:
| info1 = SELECT L_PAREN exprs = expressionList R_PAREN L_BRACE cases = selectCaseList info2 = R_BRACE
    { let tags = Source.merge info1 info2 in
      tags |> ignore;
      [ Term "SELECT"; Term "("; NT exprs; Term ")"; Term "{"; NT cases; Term "}" ]
      |> wrap_case_v |> with_typ (wrap_var_t "selectExpression") }
;

(******** Transition statements ********)

stateExpression:
| name = name info2 = SEMICOLON
    { info2 |> ignore;
      [ NT name; Term ";" ] |> wrap_case_v |> with_typ (wrap_var_t "stateExpression") }
| selectExpression = selectExpression
    { selectExpression }
;

transitionStatement:
| info1 = TRANSITION stateExpression = stateExpression
    { info1 |> ignore;
      [ Term "TRANSITION"; NT stateExpression ] |> wrap_case_v |> with_typ (wrap_var_t "transitionStatement") }
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
    { wrap_list_v stmts "parserStatement" }
;

parserState:
| anno = optAnnotations info1 = STATE name = push_name L_BRACE stmts = parserStatements trans = transitionStatement info2 = R_BRACE
    { let tags = Source.merge info1 info2 in
      tags |> ignore;
      [ NT anno; Term "STATE"; NT name; Term "{"; NT stmts; NT trans; Term "}" ]
      |> wrap_case_v |> with_typ (wrap_var_t "parserState") }
;


parserStates:
| states = list(parserState)
    { wrap_list_v states "parserState" }
;

parserTypeDeclaration:
| anno = optAnnotations info1 = PARSER name = push_name optTypeParams = optTypeParameters
    L_PAREN params = parameterList R_PAREN
    { info1 |> ignore;
      [ NT anno; Term "PARSER"; NT name; NT optTypeParams; Term "("; NT params; Term ")" ]
      |> wrap_case_v |> with_typ (wrap_var_t "parserTypeDeclaration") }
;

(* AUX *)
(* declarationList: *)
(* | (* empty *) { [] } *)
(* | SEMICOLON ds = declarationList *)
(*     { ds } *)
(* | d = declaration ds = declarationList *)
(*     { d :: ds } *)
(* ; *)

(******** P4 program ********)

p4program: 
  ds = list(declaration) END 
    { wrap_list_v ds "declaration" }
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
    { wrap_list_v elements "parserLocalElement" }
;

parserDeclaration:
| decl = parserTypeDeclaration params = optConstructorParameters
    L_BRACE locals = parserLocalElements states = parserStates info2 = R_BRACE pop_scope
    { info2 |> ignore;
      [ NT decl; NT params; Term "{"; NT locals; NT states; Term "}" ]
      |> wrap_case_v |> with_typ (wrap_var_t "parserDeclaration") }
;

packageTypeDeclaration:
| anno = optAnnotations info1 = PACKAGE name = push_name type_params = optTypeParameters
    L_PAREN params = parameterList info2 = R_PAREN
    { let tags = Source.merge info1 info2 in
      tags |> ignore;
      [ NT anno; Term "PACKAGE"; NT name; NT type_params; Term "("; NT params; Term ")" ]
      |> wrap_case_v |> with_typ (wrap_var_t "packageTypeDeclaration") }
;

specifiedIdentifier:
| name = name init = initialValue
    { [ NT name; NT init ] |> wrap_case_v |> with_typ (wrap_var_t "specifiedIdentifier") }
;

specifiedIdentifierList:
| ids = separated_list(COMMA, specifiedIdentifier)
    { wrap_list_v ids "specifiedIdentifier" }
;

(* TODO: compare with Petr4 *)
enumDeclaration:
| anno = optAnnotations info1 = ENUM name = name L_BRACE ids = identifierList comma = optTrailingComma info2 = R_BRACE
    { let tags = Source.merge info1 info2 in
      tags |> ignore;
      [ NT anno; Term "ENUM"; NT name; Term "{"; NT ids; NT comma; Term "}" ]
      |> wrap_case_v |> with_typ (wrap_var_t "enumDeclaration") }
| anno = optAnnotations info1 = ENUM typeRef = typeRef name = name L_BRACE ids = specifiedIdentifierList comma = optTrailingComma info2 = R_BRACE
    { let tags = Source.merge info1 info2 in
      tags |> ignore;
      [ NT anno; Term "ENUM"; NT typeRef; NT name; Term "{"; NT ids; NT comma; Term "}" ]
      |> wrap_case_v |> with_typ (wrap_var_t "enumDeclaration") }
;

structField:
| anno = optAnnotations typeRef = typeRef name = name info2 = SEMICOLON
    { info2 |> ignore;
      [ NT anno; NT typeRef; NT name; Term ";" ] |> wrap_case_v |> with_typ (wrap_var_t "structField") }
;

structFieldList:
| fields = list(structField)
    { wrap_list_v fields "structField" }
;

headerUnionDeclaration:
| anno = optAnnotations info1 = HEADER_UNION name = name type_params = optTypeParameters
    L_BRACE fields = structFieldList info2 = R_BRACE
    { let tags = Source.merge info1 info2 in
      tags |> ignore;
      [ NT anno; Term "HEADER_UNION"; NT name; NT type_params; Term "{"; NT fields; Term "}" ]
      |> wrap_case_v |> with_typ (wrap_var_t "headerUnionDeclaration") }
;

structTypeDeclaration:
| anno = optAnnotations info1 = STRUCT name = name type_params = optTypeParameters
    L_BRACE fields = structFieldList info2 = R_BRACE
    { let tags = Source.merge info1 info2 in
      tags |> ignore;
      [ NT anno; Term "STRUCT"; NT name; NT type_params; Term "{"; NT fields; Term "}" ]
      |> wrap_case_v |> with_typ (wrap_var_t "structTypeDeclaration") }
;

headerTypeDeclaration:
| anno = optAnnotations info1 = HEADER name = name type_params = optTypeParameters
    L_BRACE fields = structFieldList info2 = R_BRACE
    { let tags = Source.merge info1 info2 in
      tags |> ignore;
      [ NT anno; Term "HEADER"; NT name; NT type_params; Term "{"; NT fields; Term "}" ]
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
      [ NT anno; Term "TYPEDEF"; NT typeRef; NT name ] |> wrap_case_v |> with_typ (wrap_var_t "typedefDeclaration") }
| anno = optAnnotations info1 = TYPEDEF derived = derivedTypeDeclaration name = name
    { info1 |> ignore;
      [ NT anno; Term "TYPEDEF"; NT derived; NT name; Term "PHTM_12" ] |> wrap_case_v |> with_typ (wrap_var_t "typedefDeclaration") }
| anno = optAnnotations info1 = TYPE typeRef = typeRef name = name
    { info1 |> ignore;
      [ NT anno; Term "TYPE"; NT typeRef; NT name ] |> wrap_case_v |> with_typ (wrap_var_t "typedefDeclaration") }
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
    { declare_var (name_of_any_declaration const) (has_typ_params_declaration const);
      [ NT const ] |> wrap_case_v |> with_typ (wrap_var_t "declaration") }
| error = errorDeclaration
    { [ NT error ] |> wrap_case_v |> with_typ (wrap_var_t "declaration") }
| matchKind = matchKindDeclaration
    { [ NT matchKind ] |> wrap_case_v |> with_typ (wrap_var_t "declaration") }
| extern = externDeclaration
    { [ NT extern ] |> wrap_case_v |> with_typ (wrap_var_t "declaration") }
| inst = instantiation
    { declare_var (name_of_any_declaration inst) false;
      [ NT inst ] |> wrap_case_v |> with_typ (wrap_var_t "declaration") }
| func = functionDeclaration
    { declare_var (name_of_any_declaration func) (has_typ_params_declaration func);
      [ NT func ] |> wrap_case_v |> with_typ (wrap_var_t "declaration") }
| action = actionDeclaration
    { declare_var (name_of_any_declaration action) false;
      [ NT action ] |> wrap_case_v |> with_typ (wrap_var_t "declaration") }
| parserDeclaration = parserDeclaration
    { declare_type (name_of_any_declaration parserDeclaration) (has_typ_params_declaration parserDeclaration);
      [ NT parserDeclaration ] |> wrap_case_v |> with_typ (wrap_var_t "declaration") }
| controlDeclaration = controlDeclaration
    { declare_type (name_of_any_declaration controlDeclaration) (has_typ_params_declaration controlDeclaration);
      [ NT controlDeclaration ] |> wrap_case_v |> with_typ (wrap_var_t "declaration") }
| typeDeclaration = typeDeclaration
    { declare_type (name_of_any_declaration typeDeclaration) (has_typ_params_declaration typeDeclaration);
      [ NT typeDeclaration ] |> wrap_case_v |> with_typ (wrap_var_t "declaration") }
;

annotationToken:
| value = UNEXPECTED_TOKEN
    { [ NT value ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| ABSTRACT
    { [ Term "ABSTRACT" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| ACTION
    { [ Term "ACTION" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| ACTIONS
    { [ Term "ACTIONS" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| APPLY
    { [ Term "APPLY" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| BOOL
    { [ Term "BOOL" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| BIT
    { [ Term "BIT" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| BREAK
    { [ Term "BREAK" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| CONST
    { [ Term "CONST" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| CONTINUE
    { [ Term "CONTINUE" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| CONTROL
    { [ Term "CONTROL" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| DEFAULT
    { [ Term "DEFAULT" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| ELSE
    { [ Term "ELSE" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| ENTRIES
    { [ Term "ENTRIES" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| ENUM
    { [ Term "ENUM" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| ERROR
    { [ Term "ERROR" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| EXIT
    { [ Term "EXIT" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| EXTERN
    { [ Term "EXTERN" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| FALSE
    { [ Term "FALSE" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
(* | FOR *)
(*     { [ Term "FOR" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") } *)
| HEADER
    { [ Term "HEADER" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| HEADER_UNION
    { [ Term "HEADER_UNION" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| IF
    { [ Term "IF" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| IN
    { [ Term "IN" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| INOUT
    { [ Term "INOUT" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| INT
    { [ Term "INT" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| KEY
    { [ Term "KEY" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| MATCH_KIND
    { [ Term "MATCH_KIND" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| TYPE
    { [ Term "TYPE" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| OUT
    { [ Term "OUT" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| PARSER
    { [ Term "PARSER" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| PACKAGE
    { [ Term "PACKAGE" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| PRAGMA
    { [ Term "PRAGMA" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| RETURN
    { [ Term "RETURN" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| SELECT
    { [ Term "SELECT" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| STATE
    { [ Term "STATE" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| STRING
    { [ Term "STRING" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| STRUCT
    { [ Term "STRUCT" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| SWITCH
    { [ Term "SWITCH" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| TABLE
    { [ Term "TABLE" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| THIS
    { [ Term "THIS" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| TRANSITION
    { [ Term "TRANSITION" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| TRUE
    { [ Term "TRUE" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| TUPLE
    { [ Term "TUPLE" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| TYPEDEF
    { [ Term "TYPEDEF" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| VARBIT
    { [ Term "VARBIT" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| VALUESET
    { [ Term "VALUESET" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| LIST
    { [ Term "LIST" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| VOID
    { [ Term "VOID" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
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
    { wrap_list_v body "simpleAnnotation" }
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
  { wrap_list_v annos "annotation" }
;

%inline optAnnotations:
| annos = option(annotations)
    { wrap_opt_v annos "annotation" }
  ;
