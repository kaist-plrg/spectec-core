%{
  open Il.Ast
  open Context
  open Ast_utils
  open Extract

  let declare_var_of_il (v: value) (b: bool) : unit =
    let id = id_of_name v in
    declare_var id b

  let rec declare_vars_of_il (v: value) : unit =
    match flatten_case_v v with
  | "nameList", [ []; [","]; [] ], [ v_nameList; v_name ] ->
      declare_vars_of_il v_nameList;
      declare_var_of_il v_name false
    | "identifier", _, _ 
    | "nonTypeName", _, _
    | "name", _, _
    | "typeIdentifier", _, _ -> declare_var_of_il v false
    | _ -> failwith
        (Printf.sprintf "@declare_vars_of_il: expected name, got %s"
           (id_of_case_v v))

  let declare_type_of_il (v: value) (b: bool) : unit =
    let id = id_of_name v in
    declare_type id b

  let rec declare_types_of_il (v: value) : unit =
    match flatten_case_v v with
    | "typeParameterList", [ []; [","]; [] ], [ v_tpList; v_name ] ->
      declare_types_of_il v_tpList;
      declare_type_of_il v_name false
    | "identifier", _, _ 
    | "nonTypeName", _, _
    | "name", _, _
    | "typeIdentifier", _, _ -> declare_type_of_il v false
    | _ -> failwith
        (Printf.sprintf "@declare_types_of_il: expected name, got %s"
           (id_of_case_v v))
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
%type <Il.Ast.value>
  int
  optTrailingComma optCONST
  number stringLiteral
  identifier typeIdentifier nonTypeName prefixedNonTypeName typeName prefixedTypeName name nameList nonTableKwName member
  direction
  baseType specializedType namedType headerStackBaseType headerStackType listType tupleType
  typeRef typeOrVoid typeParameterList typeParameters optTypeParameters
  parameter nonEmptyParameterList parameterList constructorParameters optConstructorParameters
  kvPair kvList
  nonBraceExpression accessBaseNonBrace callBaseNonBrace 
  expression expressionList accessBase callBase kvPairRecordExpression recordExpression
  simpleKeysetExpression simpleExpressionList tupleKeysetExpression reducedSimpleKeysetExpression keysetExpression
  realTypeArg realTypeArgList typeArg typeArgList argument nonEmptyArgumentList argumentList
  lvalue initialValue optInitializer variableDeclarationWithoutSemicolon (*variableDeclaration*) constantDeclaration
  assignmentOrMethodCallStatementWithoutSemicolon assignmentOrMethodCallStatement directApplication conditionalStatement 
    emptyStatement blockStatement returnStatement breakStatement continueStatement exitStatement switchLabel switchCase switchCases switchStatement 
  declOrAssignmentOrMethodCallStatement forInitStatementsNonEmpty forInitStatements forUpdateStatementsNonEmpty forUpdateStatements forCollectionExpr forStatement
  statement statementOrDeclaration statOrDeclList
  matchKindDeclaration errorDeclaration functionPrototype methodPrototype methodPrototypes externDeclaration externName 
  functionDeclaration objInitializer instantiation objDeclaration objDeclarations actionDeclaration 
  keyElement keyElementList actionRef action actionList entryPriority entry entriesList tableProperty tablePropertyList tableDeclaration
  controlBody controlLocalDeclaration controlLocalDeclarations controlTypeDeclaration controlDeclaration
  valueSetType valueSetDeclaration selectCase selectCaseList selectExpression stateExpression transitionStatement
  parserBlockStatement parserStatement parserStatements parserState parserStates parserTypeDeclaration parserLocalElement parserLocalElements parserDeclaration
  packageTypeDeclaration
  specifiedName specifiedNameList enumDeclaration structField structFieldList
  headerUnionDeclaration structTypeDeclaration headerTypeDeclaration derivedTypeDeclaration
  typeDefType typedefDeclaration typeDeclarationWithoutSemicolon (*typeDeclaration*) declaration
  annotationToken annotationBody structuredAnnotationBody annotation annotations optAnnotations 
  declarationList (*p4program*)
%type <Il.Ast.value> push_name push_externName
%type <string> binop
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

(**************************** P4-16 GRAMMAR ******************************)
(******** Built-in ********)
int:
| int = NUMBER_INT
    { fst int }
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

(******** Misc ********)
optTrailingComma:
| (* empty *)
    { [ Term "`EMPTY" ] #@ "optTrailingComma" }
| COMMA
    { [ Term "," ] #@ "optTrailingComma" }
;

optCONST:
| (* empty *)
    { [ Term "`EMPTY" ] #@ "optCONST" }
| CONST
    { [ Term "CONST" ] #@ "optCONST" }
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
| id = identifier { id }
| APPLY { [ Term "APPLY" ] #@ "nonTypeName" }
| KEY { [ Term "KEY" ] #@ "nonTypeName" }
| ACTIONS { [ Term "ACTIONS" ] #@ "nonTypeName" }
| STATE { [ Term "STATE" ] #@ "nonTypeName" }
| ENTRIES { [ Term "ENTRIES" ] #@ "nonTypeName" }
| TYPE { [ Term "TYPE" ] #@ "nonTypeName" }
| PRIORITY { [ Term "PRIORITY" ] #@ "nonTypeName" }
;

prefixedNonTypeName:
| n = nonTypeName { n }
| go_toplevel n = nonTypeName go_local
    { [ Term "."; NT n ] #@ "prefixedNonTypeName" }
;

typeName:
| n = typeIdentifier { n }
;

prefixedTypeName:
| n = typeName { n }
| go_toplevel tid = typeIdentifier go_local
    { [ Term ".."; NT tid ] #@ "prefixedType" }
;

name:
| n = nonTypeName
| n = typeName
    { n }
| LIST { [ Term "LIST" ] #@ "name" }
;

nameList:
| n = name { n }
| ns = nameList COMMA n = name
    { [ NT ns; Term ","; NT n ]
       #@ "nameList" }
;

nonTableKwName:
| id = identifier { id }
| tid = typeIdentifier { tid }
| APPLY { [ Term "APPLY" ] #@ "nonTypeName" }
| STATE { [ Term "STATE" ] #@ "nonTypeName" }
| TYPE { [ Term "TYPE" ] #@ "nonTypeName" }
| PRIORITY { [ Term "PRIORITY" ] #@ "nonTypeName" }
;

member:
| name = name
    { name }
;

(******** Directions ********)
direction:
| (* empty *) { [ Term "`EMPTY" ] #@ "direction" }
| IN { [ Term "IN" ] #@ "direction" }
| OUT { [ Term "OUT" ] #@ "direction" }
| INOUT { [ Term "INOUT" ] #@ "direction" }
;

(******** Types ********)
baseType:
| BOOL { [ Term "BOOL" ] #@ "baseType" }
| MATCH_KIND { [ Term "MATCH_KIND" ] #@ "baseType" }
| ERROR { [ Term "ERROR" ] #@ "baseType" }
| BIT { [ Term "BIT" ] #@ "baseType" }
| STRING { [ Term "STRING" ] #@ "baseType"}
| INT
    { [ Term "INT" ] #@ "baseType" }
| BIT l_angle v = int r_angle
    { [ Term "BIT"; Term "<"; NT v; Term ">" ]
      #@ "baseType" }
| INT l_angle v = int r_angle
    { [ Term "INT"; Term "<"; NT v; Term ">" ]
      #@ "baseType" }
| VARBIT l_angle v = int r_angle
    { [ Term "VARBIT"; Term "<"; NT v; Term ">" ] #@ "baseType" }
| BIT l_angle L_PAREN e = expression R_PAREN r_angle
    { [ Term "BIT"; Term "<"; Term "("; NT e; Term ")"; Term ">" ] #@ "baseType" }
| INT l_angle L_PAREN e = expression R_PAREN r_angle
    { [ Term "INT"; Term "<"; Term "("; NT e; Term ")"; Term ">" ]
      #@ "baseType" }
| VARBIT l_angle L_PAREN e = expression R_PAREN r_angle
    { [ Term "VARBIT"; Term "<"; Term "("; NT e; Term ")"; Term ">" ] #@ "baseType" }
;

specializedType:
| n = prefixedTypeName l_angle targL = typeArgList r_angle
    { [ NT n; Term "<"; NT targL; Term ">" ] #@ "specializedType" }
;

namedType:
| n = prefixedTypeName { n }
| t = specializedType { t }
;

headerStackBaseType: (*TODO: inline?*)
| n = prefixedTypeName { n }
| t = specializedType { t }
;

headerStackType:
| t = headerStackBaseType L_BRACKET e = expression R_BRACKET
    { [ NT t; Term "["; NT e; Term "]" ] #@ "headerStackType" }
;

listType:
| LIST l_angle ta = typeArg r_angle
    { [ Term "LIST"; Term "<"; NT ta; Term ">" ] #@ "listType" }
;

tupleType:
| TUPLE l_angle tas = typeArgList r_angle
    { [ Term "TUPLE"; Term "<"; NT tas; Term ">" ] #@ "tupleType" }
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
| t = typeRef { t }
| VOID { [ Term "VOID" ] #@ "typeOrVoid" }
(* From Petr4: HACK for generic return type *)
| id = identifier
    { match flatten_case_v id with
        | "identifier", [ ["`ID"]; [] ], [ value_text ]  ->
          [ Term "`TID"; NT value_text ] #@ "typeIdentifier"
        | _ -> failwith "@typeOrVoid: expected identifier" }
;

(******** Type parameters ********)
typeParameterList:
| n = name { n }
| tps = typeParameterList COMMA n = name
    { [ NT tps; Term ","; NT n ] #@ "typeParameterList" }
;

typeParameters:
| l_angle tps = typeParameterList r_angle
    { declare_types_of_il tps;
      [ Term "<"; NT tps; Term ">" ] #@ "typeParameters" }
;

optTypeParameters:
| (* empty *) { [ Term "`EMPTY" ] #@ "optTypeParameters" }
| tps = typeParameters { tps }
;

(******** Parameters ********)
parameter:
| anno = optAnnotations dir = direction t = typeRef n = name
    { [ NT anno; NT dir; NT t; NT n ] #@ "parameter" }
| anno = optAnnotations dir = direction t = typeRef n = name i = initialValue
    { [ NT anno; NT dir; NT t; NT n; NT i ] #@ "parameter" }
;

nonEmptyParameterList:
| p = parameter { p }
| ps = nonEmptyParameterList COMMA p = parameter
    { [ NT ps; Term ","; NT p ] #@ "nonEmptyParameterList" }
;

parameterList:
| (* empty *) { [ Term "`EMPTY" ] #@ "parameterList" }
| ps = nonEmptyParameterList { ps }
;

(******** Constructor parameters ********)
constructorParameters:
| L_PAREN ps = parameterList R_PAREN
    { [ Term "("; NT ps; Term ")" ] #@ "constructorParameters" }
;

optConstructorParameters:
| (* empty *) { [ Term "`EMPTY" ] #@ "optConstructorParameters" }
| cps = constructorParameters { cps }
;

(******** Expression key-value pairs ********)
kvPair:
| n = name ASSIGN e = expression { [ NT n; Term "="; NT e ] #@ "kvPair" }
;

kvList: (* TODO: inline? *)
| kv = kvPair { kv }
| kvs = kvList COMMA kv = kvPair { [ NT kvs; Term ","; NT kv ] #@ "kvList" }
;

(******** Expressions ********)
nonBraceExpression:
| num = number { num }
| str = stringLiteral { str }
| TRUE { [ Term "TRUE" ] #@ "nonBraceExpression" }
| FALSE { [ Term "FALSE" ] #@ "nonBraceExpression" }
| THIS { [ Term "THIS" ] #@ "nonBraceExpression" }
| n = prefixedNonTypeName { n }
| a = nonBraceExpression L_BRACKET i = expression R_BRACKET
  { [ NT a; Term "["; NT i; Term "]" ] #@ "nonBraceExpression" }
| b = nonBraceExpression L_BRACKET h = expression COLON l = expression R_BRACKET
  { [ NT b; Term "["; NT h; Term ":"; NT l; Term "]" ] #@ "nonBraceExpression" }
| L_PAREN e = expression R_PAREN 
  { [ Term "("; NT e; Term ")" ] #@ "nonBraceExpression" }
| NOT e = expression %prec PREFIX 
  { [ Term "!"; NT e ] #@ "nonBraceExpression" }
| COMPLEMENT e = expression %prec PREFIX 
  { [ Term "~"; NT e ] #@ "nonBraceExpression" }
| MINUS e = expression %prec PREFIX 
  { [ Term "-"; NT e ] #@ "nonBraceExpression" }
| PLUS e = expression %prec PREFIX 
  { [ Term "+"; NT e ] #@ "nonBraceExpression" }
| b = accessBaseNonBrace DOT m = member 
  { [ NT b; Term "."; NT m ] #@ "nonBraceExpression" }
(* TODO: binop *)
| l = nonBraceExpression op_str = binop r = expression
  { [ NT l; Term op_str; NT r ] #@ "nonBraceExpression" }
| c = nonBraceExpression QUESTION t = expression COLON f = expression
  { [ NT c; Term "?"; NT t; Term ":"; NT f ] #@ "nonBraceExpression" }
| f = nonBraceExpression l_angle targs = realTypeArgList r_angle
  L_PAREN args = argumentList R_PAREN
  { [ NT f; Term "<"; NT targs; Term ">"; Term "("; NT args; Term ")" ]
    #@ "nonBraceExpression" }
| b = callBaseNonBrace L_PAREN args = argumentList R_PAREN
  { [ NT b; Term "("; NT args; Term ")" ]
    #@ "nonBraceExpression" }
| L_PAREN t = typeRef R_PAREN e = expression %prec PREFIX
    { [ Term "("; NT t; Term ")"; NT e ] #@ "nonBraceExpression" }
;

%inline binop:
| MUL { "*" }
| DIV { "/" }
| MOD { "%" }
| PLUS { "+" }
| PLUS_SAT { "|+|" }
| MINUS { "-" }
| MINUS_SAT { "|-|" }
| SHL { "<<" }
| r_angle R_ANGLE_SHIFT { ">>" }
| LE { "<=" }
| GE { ">=" }
| l_angle { "<" }
| r_angle { ">" }
| NE { "!=" }
| EQ { "==" }
| BIT_AND { "&" }
| BIT_XOR { "^" }
| BIT_OR { "|" }
| PLUSPLUS { "++" }
| AND { "&&" }
| OR { "||" }
;

accessBaseNonBrace: (*TODO: inline?*)
| n = prefixedTypeName { n }
| ERROR { [ Term "ERROR" ] #@ "accessBaseNonBrace" }
| e = nonBraceExpression { e }
;

callBaseNonBrace: (*TODO: inline?*)
| t = namedType { t }
| e = nonBraceExpression { e }
;

expression:
| num = number { num }
| DOTS { [ Term "..." ] #@ "expression" }
| str = stringLiteral { str }
| TRUE { [ Term "TRUE" ] #@ "expression" }
| FALSE { [ Term "FALSE" ] #@ "expression" }
| THIS { [ Term "THIS" ] #@ "expression" }
| n = prefixedNonTypeName { n }
| a = expression L_BRACKET i = expression R_BRACKET
  { [ NT a; Term "["; NT i; Term "]" ] #@ "expression" }
| b = expression L_BRACKET h = expression COLON l = expression R_BRACKET
  { [ NT b; Term "["; NT h; Term ":"; NT l; Term "]" ] #@ "expression" }
| INVALID { [ Term "INVALID" ] #@ "expression" }
| L_BRACE r = recordExpression c = optTrailingComma R_BRACE
  { [ Term "{"; NT r; NT c; Term "}" ] #@ "expression" }
| L_PAREN e = expression R_PAREN { [ Term "("; NT e; Term ")" ] #@ "expression" }
| NOT e = expression %prec PREFIX
  { [ Term "!"; NT e ] #@ "expression" }
| COMPLEMENT e = expression %prec PREFIX
  { [ Term "~"; NT e ] #@ "expression" }
| MINUS e = expression %prec PREFIX
  { [ Term "-"; NT e ] #@ "expression" }
| PLUS e = expression %prec PREFIX
  { [ Term "+"; NT e ] #@ "expression" }
| b = accessBase DOT m = member
  { [ NT b; Term "."; NT m ] #@ "expression" }
| l = expression op = binop r = expression
  { [ NT l; Term op; NT r ] #@ "expression" }
| c = expression QUESTION t = expression COLON f = expression
  { [ NT c; Term "?"; NT t; Term ":"; NT f ] #@ "expression" }
| f = expression l_angle targs = realTypeArgList r_angle
  L_PAREN args = argumentList R_PAREN
  { [ NT f; Term "<"; NT targs; Term ">"; Term "("; NT args; Term ")" ] #@ "expression" }
| b = callBase L_PAREN args = argumentList R_PAREN
  { [ NT b; Term "("; NT args; Term ")" ] #@ "expression" }
| L_PAREN t = typeRef R_PAREN e = expression %prec PREFIX
  { [ Term "("; NT t; Term ")"; NT e ] #@ "expression" }
;

expressionList:
| (* empty *) { [ Term "`EMPTY" ] #@ "expressionList" }
| e = expression { e }
| es = expressionList COMMA e = expression
  { [ NT es; Term ","; NT e ] #@ "expressionList" }
;

accessBase: (*TODO: inline?*)
| n = prefixedTypeName { n }
| ERROR { [ Term "ERROR" ] #@ "accessBase" }
| e = expression { e }
;

callBase: (*TODO: inline?*)
| t = namedType { t }
| e = expression { e }
;

kvPairRecordExpression:
| n = name ASSIGN e = expression
  { [ NT n; Term "="; NT e ] #@ "kvPairRecordExpression" }
| n = name ASSIGN e = expression COMMA DOTS
  { [ NT n; Term "="; NT e; Term ","; Term "..."]
    #@ "kvPairRecordExpression" }
| n = name ASSIGN e = expression COMMA kvs = kvList
  { [ NT n; Term "="; NT e; Term ","; NT kvs ]
    #@ "kvPairRecordExpression" }
| n = name ASSIGN e = expression COMMA kvs = kvList COMMA DOTS
  { [ NT n; Term "="; NT e; Term ","; NT kvs; Term ","; Term "..." ]
    #@ "kvPairRecordExpression" }
;

recordExpression:
| es = expressionList { es }
| kvs = kvPairRecordExpression { kvs }
;

(******** Keyset Expressions ********)
simpleKeysetExpression:
| e = expression { e }
| b = expression MASK m = expression
    { [ NT b; Term "&&&"; NT m ] #@ "simpleKeysetExpression" }
| l = expression RANGE h = expression
    { [ NT l; Term ".."; NT h ] #@ "simpleKeysetExpression" }
| DEFAULT
    { [ Term "DEFAULT" ] #@ "simpleKeysetExpression" }
| DONTCARE
    { [ Term "_" ] #@ "simpleKeysetExpression" }
;

simpleExpressionList:
| e = simpleKeysetExpression { e }
| es = simpleExpressionList COMMA e = simpleKeysetExpression
    { [ NT es; Term ","; NT e ] #@ "simpleExpressionList" }
;

tupleKeysetExpression:
| L_PAREN e = simpleKeysetExpression COMMA es = simpleExpressionList R_PAREN (* TODO: revisit *)
      { [ Term "("; NT e; Term ","; NT es; Term ")" ]
       #@ "tupleKeysetExpression" }
| L_PAREN e = reducedSimpleKeysetExpression R_PAREN
      { [ Term "("; NT e; Term ")" ]
           #@ "tupleKeysetExpression" }
;

reducedSimpleKeysetExpression:
| b = expression MASK m = expression
    { [ NT b; Term "&&&"; NT m ] #@ "reducedSimpleKeysetExpression" }
| l = expression RANGE h = expression
    { [ NT l; Term ".."; NT h ] #@ "reducedSimpleKeysetExpression" }
| DEFAULT
    { [ Term "DEFAULT" ] #@ "reducedSimpleKeysetExpression" }
| DONTCARE
    { [ Term "_" ] #@ "reducedSimpleKeysetExpression" }
;

keysetExpression:
| e = tupleKeysetExpression
| e = simpleKeysetExpression
    { e }
;

(******** Type arguments ********)
realTypeArg:
| t = typeRef { t }
| VOID
    { [ Term "VOID" ] #@ "realTypeArg" }
| DONTCARE
    { [ Term "_" ] #@ "realTypeArg" }
;

realTypeArgList:
| targ = realTypeArg { targ }
| targs = realTypeArgList COMMA targ = realTypeArg
    { [ NT targs; Term ","; NT targ ] #@ "realTypeArgList" }
;

typeArg:
| t = typeRef { t }
| n = nonTypeName { n }
| VOID
    { [ Term "VOID" ] #@ "typeArg" }
| DONTCARE
    { [ Term "_" ] #@ "typeArg" }
;

typeArgList:
| (* empty *) { [ Term "`EMPTY" ] #@ "typeArgList" }
| targ = typeArg { targ }
| targs = typeArgList COMMA targ = typeArg
    { [ NT targs; Term ","; NT targ ] #@ "typeArgList" }
;

(******** Arguments ********)
argument:
| e = expression { e }
| n = name ASSIGN e = expression 
  { [ NT n; Term "="; NT e ] #@ "argument" }
| DONTCARE
  { [ Term "_" ] #@ "argument" }
| name = name ASSIGN DONTCARE
  { [ NT name; Term "="; Term "_" ] #@ "argument" }
;

nonEmptyArgumentList:
| arg = argument { arg }
| args = nonEmptyArgumentList COMMA arg = argument
    { [ NT args; Term ","; NT arg ] #@ "nonEmptyArgumentList" }
;

argumentList:
| (* empty *) { [ Term "`EMPTY" ] #@ "argumentList" }
| args = nonEmptyArgumentList { args }
;

(******** L-values ********)
lvalue:
| n = prefixedNonTypeName { n }
| THIS { [ Term "THIS" ] #@ "lvalue" }
| lv = lvalue DOT m = member
  { [ NT lv; Term "."; NT m ] #@ "lvalue" }
| lv = lvalue L_BRACKET i = expression R_BRACKET
  { [ NT lv; Term "["; NT i; Term "]" ] #@ "lvalue" }
| lv = lvalue L_BRACKET h = expression COLON l = expression R_BRACKET
  { [ NT lv; Term "["; NT h; Term ":"; NT l; Term "]" ] #@ "lvalue" }
| L_PAREN lv = lvalue R_PAREN
  { [ Term "("; NT lv; Term ")" ] #@ "lvalue" }
;

(******** Variable and constant declarations ********)
(* initializer -> initialValue due to reserved word in OCaml *)
initialValue:
| ASSIGN e = expression
  { [ Term "="; NT e ] #@ "initializer" }
;

optInitializer:
| (* empty *)
  { [ Term "`EMPTY" ] #@ "optInitializer" }
| i = initialValue { i }
;

variableDeclarationWithoutSemicolon: (* TODO: inline? *)
| anno = optAnnotations t = typeRef n = name i = optInitializer
  { declare_var_of_il n false;
    [ NT anno; NT t; NT n; NT i ]
    #@ "variableDeclarationWithoutSemicolon" }
;

variableDeclaration: (* TODO: revisit *)
| d = variableDeclarationWithoutSemicolon SEMICOLON
  { [ NT d; Term ";" ] #@ "variableDeclaration" }
;

constantDeclaration:
| anno = optAnnotations CONST t = typeRef n = name i = initialValue SEMICOLON
  { [ NT anno; Term "CONST"; NT t; NT n; NT i; Term ";" ]
    #@ "constantDeclaration" }
;

(******** Statements ********)
assignmentOrMethodCallStatementWithoutSemicolon:
| f= lvalue L_PAREN args = argumentList R_PAREN
  { [ NT f; Term "("; NT args; Term ")" ] #@ "assignmentOrMethodCallStatementWithoutSemicolon" }
| f = lvalue l_angle targs = typeArgList r_angle L_PAREN args = argumentList R_PAREN
  { [ NT f; Term "<"; NT targs; Term ">"; Term "("; NT args; Term ")" ]
    #@ "assignmentOrMethodCallStatementWithoutSemicolon" }
| lv = lvalue ASSIGN r = expression
  { [ NT lv; Term "="; NT r ] #@ "assignmentOrMethodCallStatementWithoutSemicolon" }
| lv = lvalue PLUS_ASSIGN r = expression
  { [ NT lv; Term "+="; NT r ] #@ "assignmentOrMethodCallStatementWithoutSemicolon" }
| lv = lvalue PLUS_SAT_ASSIGN r = expression
  { [ NT lv; Term "|+|="; NT r ] #@ "assignmentOrMethodCallStatementWithoutSemicolon" }
| lv = lvalue MINUS_ASSIGN r = expression
  { [ NT lv; Term "-="; NT r ] #@ "assignmentOrMethodCallStatementWithoutSemicolon" }
| lv = lvalue MINUS_SAT_ASSIGN r = expression
  { [ NT lv; Term "|-|="; NT r ] #@ "assignmentOrMethodCallStatementWithoutSemicolon" }
| lv = lvalue MUL_ASSIGN r = expression
  { [ NT lv; Term "*="; NT r ] #@ "assignmentOrMethodCallStatementWithoutSemicolon" }
| lv = lvalue DIV_ASSIGN r = expression
  { [ NT lv; Term "/="; NT r ] #@ "assignmentOrMethodCallStatementWithoutSemicolon" }
| lv = lvalue MOD_ASSIGN r = expression
  { [ NT lv; Term "%="; NT r ] #@ "assignmentOrMethodCallStatementWithoutSemicolon" }
| lv = lvalue SHL_ASSIGN r = expression
  { [ NT lv; Term "<<="; NT r ] #@ "assignmentOrMethodCallStatementWithoutSemicolon" }
| lv = lvalue SHR_ASSIGN r = expression
  { [ NT lv; Term ">>="; NT r ] #@ "assignmentOrMethodCallStatementWithoutSemicolon" }
| lv = lvalue BIT_AND_ASSIGN r = expression
  { [ NT lv; Term "&="; NT r ] #@ "assignmentOrMethodCallStatementWithoutSemicolon" }
| lv = lvalue BIT_XOR_ASSIGN r = expression
  { [ NT lv; Term "^="; NT r ] #@ "assignmentOrMethodCallStatementWithoutSemicolon" }
| lv = lvalue BIT_OR_ASSIGN r = expression
  { [ NT lv; Term "|="; NT r ] #@ "assignmentOrMethodCallStatementWithoutSemicolon" }
;

assignmentOrMethodCallStatement:
| s = assignmentOrMethodCallStatementWithoutSemicolon SEMICOLON
  { [ NT s; Term ";" ] #@ "assignmentOrMethodCallStatement" }
;

directApplication:
| t = namedType DOT APPLY L_PAREN args = argumentList R_PAREN SEMICOLON
    { [ NT t; Term "."; Term "APPLY"; Term "("; NT args; Term ")"; Term ";" ]
       #@ "directApplication" }
;

conditionalStatement:
| IF L_PAREN c = expression R_PAREN t = statement %prec THEN
    { [ Term "IF"; Term "("; NT c; Term ")"; NT t ]
       #@ "conditionalStatement" }
| IF L_PAREN c = expression R_PAREN t = statement ELSE f = statement
    { [ Term "IF"; Term "("; NT c; Term ")"; NT t; Term "ELSE"; NT f ]
       #@ "conditionalStatement" }
;

emptyStatement:
| SEMICOLON { [ Term ";" ] #@ "emptyStatement" }
;

blockStatement:
| anno = optAnnotations L_BRACE
  push_scope
  ss = statOrDeclList R_BRACE
  pop_scope
  { [ NT anno; Term "{"; NT ss; Term "}" ] #@ "blockStatement" }
;

returnStatement:
| RETURN SEMICOLON
    { [ Term "RETURN"; Term ";" ] #@ "returnStatement" }
| RETURN e = expression SEMICOLON
    { [ Term "RETURN"; NT e; Term ";" ] #@ "returnStatement" }
;

breakStatement:
| BREAK SEMICOLON
    { [ Term "BREAK"; Term ";" ] #@ "breakStatement" }
;

continueStatement:
| CONTINUE SEMICOLON
    { [ Term "CONTINUE"; Term ";" ] #@ "continueStatement" }
;

exitStatement:
| EXIT SEMICOLON
    { [ Term "EXIT"; Term ";" ] #@ "exitStatement" }
;

switchLabel:
| DEFAULT
    { [ Term "DEFAULT" ] #@ "switchLabel" }
| e = nonBraceExpression
    { e }
;

switchCase:
| label = switchLabel COLON s = blockStatement
    { [ NT label; Term ":"; NT s ] #@ "switchCase" }
| label = switchLabel COLON
    { [ NT label; Term ":" ] #@ "switchCase" }
;

switchCases:
| (* empty *)
    { [ Term "`EMPTY" ] #@ "switchCases" }
| cs = switchCases c = switchCase
    { [ NT cs; NT c ] #@ "switchCases" }
;

switchStatement:
| SWITCH L_PAREN e = expression R_PAREN L_BRACE cs = switchCases R_BRACE
    { [ Term "SWITCH"; Term "("; NT e; Term ")"; Term "{"; NT cs; Term "}" ]
       #@ "switchStatement" }
;

declOrAssignmentOrMethodCallStatement:
| d = variableDeclarationWithoutSemicolon { d }
| s = assignmentOrMethodCallStatementWithoutSemicolon { s }
;

forInitStatementsNonEmpty:
| s = declOrAssignmentOrMethodCallStatement { s }
| ss = forInitStatementsNonEmpty COMMA s = declOrAssignmentOrMethodCallStatement
    { [ NT ss; Term ","; NT s ] #@ "forInitStatementsNonEmpty" }
;

forInitStatements:
| (* empty *) { [ Term "`EMPTY" ] #@ "forInitStatements" }
| ss = forInitStatementsNonEmpty { ss }
;

forUpdateStatementsNonEmpty:
| s = assignmentOrMethodCallStatementWithoutSemicolon { s }
| ss = forUpdateStatementsNonEmpty COMMA s = assignmentOrMethodCallStatementWithoutSemicolon
    { [ NT ss; Term ","; NT s ] #@ "forUpdateStatementsNonEmpty" }
;

forUpdateStatements:
| (* empty *) { [ Term "`EMPTY" ] #@ "forUpdateStatements" }
| ss = forUpdateStatementsNonEmpty { ss }
;

forCollectionExpr:
| e = expression { e }
| l = expression RANGE h = expression
    { [ NT l; Term ".."; NT h ] #@ "forCollectionExpr" }
;

forStatement:
| anno = optAnnotations FOR
  L_PAREN inits = forInitStatements SEMICOLON c = expression SEMICOLON updates = forUpdateStatements R_PAREN
  b = statement
    { [ NT anno; Term "FOR"; Term "("; NT inits; Term ";"; NT c; Term ";"; NT updates; Term ")"; NT b ]
       #@ "forStatement" }
| anno = optAnnotations FOR L_PAREN
    t = typeRef n = name IN coll = forCollectionExpr R_PAREN b = statement
    { [ NT anno; Term "FOR"; Term "("; NT t; NT n; Term "IN"; NT coll; Term ")"; NT b ]
       #@ "forStatement" }
| anno = optAnnotations FOR L_PAREN
    anno_in = annotations t = typeRef n = name IN
    coll = forCollectionExpr R_PAREN b = statement
    { [ NT anno; Term "FOR"; Term "("; NT anno_in; NT t; NT n; Term "IN"; NT coll; Term ")"; NT b ]
       #@ "forStatement" }
;

statement:
| s = assignmentOrMethodCallStatement
| s = directApplication
| s = conditionalStatement
| s = emptyStatement
| s = blockStatement
| s = returnStatement
| s = breakStatement
| s = continueStatement
| s = exitStatement
| s = switchStatement
| s = forStatement
    { s }
;

statementOrDeclaration:
| d = variableDeclaration
| d = constantDeclaration
    { d }
| s = statement
    { s }
;

statOrDeclList:
| (* empty *) { [ Term "`EMPTY" ] #@ "statOrDeclList" }
| ss = statOrDeclList s = statementOrDeclaration
    { [ NT ss; NT s ] #@ "statOrDeclList" }
;

(******** Error and match kind declarations ********)
matchKindDeclaration:
| MATCH_KIND L_BRACE ns = nameList c = optTrailingComma R_BRACE
    { declare_vars_of_il ns;
      [ Term "MATCH_KIND"; Term "{"; NT ns; NT c; Term "}" ] #@ "matchKindDeclaration" }
;

errorDeclaration:
| ERROR L_BRACE ns = nameList R_BRACE
    { declare_vars_of_il ns;
      [ Term "ERROR"; Term "{"; NT ns; Term "}" ] #@ "errorDeclaration" }
;

(******** Extern declarations ********)
functionPrototype:
| t = typeOrVoid n = name push_scope
  tparams = optTypeParameters
  L_PAREN params = parameterList R_PAREN
    { [ NT t; NT n; NT tparams; Term "("; NT params; Term ")" ]
       #@ "functionPrototype" }
;

methodPrototype:
| anno = optAnnotations p = functionPrototype pop_scope SEMICOLON
    { [ NT anno; NT p; Term ";" ] #@ "methodPrototype" }
| anno = optAnnotations ABSTRACT p = functionPrototype
    pop_scope SEMICOLON
    { [ NT anno; Term "ABSTRACT"; NT p; Term ";" ] #@ "methodPrototype" }
(* Petr4: alias methodName in place of typeIdentifier *)
| anno = optAnnotations tid = typeIdentifier L_PAREN params = parameterList R_PAREN SEMICOLON
    { [ NT anno; NT tid; Term "("; NT params; Term ")"; Term ";" ] #@ "methodPrototype" }
;

methodPrototypes:
| (* empty *) { [ Term "`EMPTY" ] #@ "methodPrototypes" }
| ps = methodPrototypes p = methodPrototype
    { [ NT ps; NT p ] #@ "methodPrototypes" }
;

externDeclaration:
| anno = optAnnotations EXTERN n = push_externName tparams = optTypeParameters
  L_BRACE ps = methodPrototypes R_BRACE pop_scope
  { let decl =
      [ NT anno; Term "EXTERN"; NT n; NT tparams; Term "{"; NT ps; Term "}" ]
        #@ "externDeclaration"
    in
    declare_type_of_il n (has_type_params_declaration decl);
    decl }
| anno = optAnnotations EXTERN p = functionPrototype pop_scope SEMICOLON
  { let decl =
      [ NT anno; Term "EXTERN"; NT p; Term ";" ] #@ "externDeclaration"
    in
    declare_var (id_of_declaration decl) (has_type_params_declaration decl);
    decl }
;

(* Auxiliary for push_externName, changed from name to nonTypeName *)
externName:
| n = nonTypeName
  { declare_type_of_il n false;
    n }
;

(******** Function declarations ********)
functionDeclaration:
| anno = optAnnotations p = functionPrototype b = blockStatement pop_scope
    { [ NT anno; NT p; NT b ] #@ "functionDeclaration" }
;

(******** Instantiations ********)
objInitializer:
| ASSIGN L_BRACE ds = objDeclarations R_BRACE
    { [ Term "="; Term "{"; NT ds; Term "}" ] #@ "objInitializer" }
;

instantiation:
| anno = optAnnotations t = typeRef L_PAREN args = argumentList R_PAREN n = name SEMICOLON
    { [ NT anno; NT t; Term "("; NT args; Term ")"; NT n; Term ";" ]
       #@ "instantiation" }
| anno = optAnnotations t = typeRef L_PAREN args = argumentList R_PAREN n = name i = objInitializer SEMICOLON
    { [ NT anno; NT t; Term "("; NT args; Term ")"; NT n; NT i; Term ";" ]
       #@ "instantiation" }
;

objDeclaration:
| d = functionDeclaration
| d = instantiation
    { d }
;

objDeclarations:
| (* empty *) { [ Term "`EMPTY" ] #@ "objDeclarations" }
| ds = objDeclarations d = objDeclaration
    { [ NT ds; NT d ] #@ "objDeclarations" }
;

(******** Action declarations ********)
actionDeclaration:
| anno = optAnnotations ACTION n = name L_PAREN params = parameterList R_PAREN b = blockStatement
    { [ NT anno; Term "ACTION"; NT n; Term "("; NT params; Term ")"; NT b ]
       #@ "actionDeclaration" }
;

(******** Table declarations ********)
keyElement:
| key = expression COLON match_kind = name anno = optAnnotations SEMICOLON
    { [ NT key; Term ":"; NT match_kind; NT anno; Term ";" ] #@ "keyElement" }
;

keyElementList:
| (* empty *) { [ Term "`EMPTY" ] #@ "keyElementList" }
| ks = keyElementList k = keyElement
    { [ NT ks; NT k ] #@ "keyElementList" }
;

(* Petr4: contains optAnnotations, name = name *)
actionRef:
| n = prefixedNonTypeName
    { n }
| n = prefixedNonTypeName L_PAREN args = argumentList R_PAREN
    { [ NT n; Term "("; NT args; Term ")" ] #@ "actionRef" }
;

action:
| anno = optAnnotations a = actionRef SEMICOLON
    { [ NT anno; NT a; Term ";" ] #@ "action" }
;

actionList:
| (* empty *) { [ Term "`EMPTY" ] #@ "actionList" }
| acs = actionList a = action
    { [ NT acs; NT a ] #@ "actionList" }
;

entryPriority:
| PRIORITY ASSIGN num = number COLON
    { [ Term "PRIORITY"; Term "="; NT num; Term ":" ] #@ "entryPriority" }
| PRIORITY ASSIGN L_PAREN e = expression R_PAREN COLON
    { [ Term "PRIORITY"; Term "="; Term "("; NT e; Term ")"; Term ":" ] #@ "entryPriority" }
;

entry:
| c = optCONST prio = entryPriority k = keysetExpression COLON a = actionRef anno = optAnnotations SEMICOLON
    { [ NT c; NT prio; NT k; Term ":"; NT a; NT anno; Term ";" ] #@ "entry" }
| c = optCONST k = keysetExpression COLON a = actionRef anno = optAnnotations SEMICOLON
    { [ NT c; NT k; Term ":"; NT a; NT anno; Term ";" ] #@ "entry" }
;
entriesList:
| (* empty *) { [ Term "`EMPTY" ] #@ "entriesList" }
| es = entriesList e = entry
    { [ NT es; NT e ] #@ "entriesList" }
;

tableProperty:
| KEY ASSIGN L_BRACE keys = keyElementList R_BRACE
    { [ Term "KEY"; Term "="; Term "{"; NT keys; Term "}" ] #@ "tableProperty" }
| ACTIONS ASSIGN L_BRACE actions = actionList R_BRACE
    { [ Term "ACTIONS"; Term "="; Term "{"; NT actions; Term "}" ] #@ "tableProperty" }
| anno = optAnnotations optConst = optCONST ENTRIES ASSIGN L_BRACE entries = entriesList R_BRACE
    { [ NT anno; NT optConst; Term "ENTRIES"; Term "="; Term "{"; NT entries; Term "}" ] #@ "tableProperty" }
| anno = optAnnotations optConst = optCONST name = nonTableKwName init = initialValue SEMICOLON
    { [ NT anno; NT optConst; NT name; NT init; Term ";" ] #@ "tableProperty" }
;

tablePropertyList:
| (* empty *) { [ Term "`EMPTY" ] #@ "tablePropertyList" }
| ps = tablePropertyList p = tableProperty
    { [ NT ps; NT p ] #@ "tablePropertyList" }
;

tableDeclaration:
| anno = optAnnotations TABLE n = name L_BRACE ps = tablePropertyList R_BRACE
    { [ NT anno; Term "TABLE"; NT n; Term "{"; NT ps; Term "}" ] #@ "tableDeclaration" }
;

(******** Control and control type declarations ********)
controlBody:
| b = blockStatement { b }
;

controlLocalDeclaration:
| d = constantDeclaration { d }
| d = actionDeclaration
| d = tableDeclaration
    { declare_var (id_of_declaration d) false;
      d }
| d = instantiation
| d = variableDeclaration
    { d }
;

controlLocalDeclarations:
| (* empty *) { [ Term "`EMPTY" ] #@ "controlLocalDeclarations" }
| ds = controlLocalDeclarations d = controlLocalDeclaration
    { [ NT ds; NT d ] #@ "controlLocalDeclarations" }
;

controlTypeDeclaration:
| anno = optAnnotations CONTROL n = push_name tparams = optTypeParameters
    L_PAREN params = parameterList R_PAREN
    { [ NT anno; Term "CONTROL"; NT n; NT tparams; Term "("; NT params; Term ")" ]
       #@ "controlTypeDeclaration" }
;

controlDeclaration:
| d = controlTypeDeclaration cparams = optConstructorParameters
    L_BRACE locals = controlLocalDeclarations APPLY apply = controlBody R_BRACE pop_scope
    { [ NT d; NT cparams; Term "{"; NT locals; Term "APPLY"; NT apply; Term "}" ]
       #@ "controlDeclaration" }
;

(******** Value set declarations ********)
valueSetType: (* TODO: inline? *)
| t = baseType
| t = tupleType
| t = prefixedTypeName
    { t }
;

valueSetDeclaration:
| anno = optAnnotations VALUESET l_angle t = valueSetType r_angle
    L_PAREN size = expression R_PAREN n = name SEMICOLON
    { [ NT anno; Term "VALUESET"; Term "<"; NT t; Term ">"; Term "("; NT size; Term ")"; NT n; Term ";" ]
       #@ "valueSetDeclaration" }
;

(******** Select expressions ********)
selectCase:
| key = keysetExpression COLON n = name SEMICOLON
    { [ NT key; Term ":"; NT n; Term ";" ] #@ "selectCase" }
;

selectCaseList:
| (* empty *) { [ Term "`EMPTY" ] #@ "selectCaseList" }
| cs = selectCaseList c = selectCase
    { [ NT cs; NT c ] #@ "selectCaseList" }
;

selectExpression:
| SELECT L_PAREN es = expressionList R_PAREN L_BRACE cs = selectCaseList R_BRACE
    { [ Term "SELECT"; Term "("; NT es; Term ")"; Term "{"; NT cs; Term "}" ]
       #@ "selectExpression" }
;

(******** Transition statements ********)
stateExpression:
| n = name SEMICOLON
    { [ NT n; Term ";" ] #@ "stateExpression" }
| e = selectExpression
    { e }
;

transitionStatement:
| (* empty *) { [ Term "`EMPTY" ] #@ "transitionStatement" }
| TRANSITION e = stateExpression
    { [ Term "TRANSITION"; NT e ] #@ "transitionStatement" }
;

(******** Parser and parser type declarations ********)
parserBlockStatement:
| anno = optAnnotations L_BRACE ss = parserStatements R_BRACE
    { [ NT anno; Term "{"; NT ss; Term "}" ] #@ "parserBlockStatement" }
;

parserStatement:
| s = assignmentOrMethodCallStatement
| s = directApplication
| s = emptyStatement
| s = variableDeclaration
| s = constantDeclaration
| s = parserBlockStatement
| s = conditionalStatement
    { s }
;

parserStatements:
| (* empty *) { [ Term "`EMPTY" ] #@ "parserStatements" }
| ss = parserStatements s = parserStatement
    { [ NT ss; NT s ] #@ "parserStatements" }
;

parserState:
| anno = optAnnotations STATE n = push_name L_BRACE ss = parserStatements trans = transitionStatement R_BRACE
    { [ NT anno; Term "STATE"; NT n; Term "{"; NT ss; NT trans; Term "}" ]
       #@ "parserState" }
;

parserStates:
| s = parserState { s }
| ss = parserStates s = parserState
    { [ NT ss; NT s ] #@ "parserStates" }
;

parserTypeDeclaration:
| anno = optAnnotations PARSER name = push_name tparams = optTypeParameters
    L_PAREN params = parameterList R_PAREN
    { [ NT anno; Term "PARSER"; NT name; NT tparams; Term "("; NT params; Term ")" ]
       #@ "parserTypeDeclaration" }
;

parserLocalElement:
| d = constantDeclaration
| d = variableDeclaration
| d = instantiation
| d = valueSetDeclaration
  { d }
;

parserLocalElements:
| (* empty *) { [ Term "`EMPTY" ] #@ "parserLocalElements" }
| ls = parserLocalElements l = parserLocalElement
  { [ NT ls; NT l ] #@ "parserLocalElements" }
;

parserDeclaration:
| d = parserTypeDeclaration params = optConstructorParameters
    L_BRACE locals = parserLocalElements states = parserStates R_BRACE pop_scope
    { [ NT d; NT params; Term "{"; NT locals; NT states; Term "}" ]
       #@ "parserDeclaration" }
;

packageTypeDeclaration:
| anno = optAnnotations PACKAGE name = push_name type_params = optTypeParameters
    L_PAREN params = parameterList R_PAREN
    { [ NT anno; Term "PACKAGE"; NT name; NT type_params; Term "("; NT params; Term ")" ]
       #@ "packageTypeDeclaration" }
;

specifiedName:
| n = name i = initialValue
    { [ NT n; NT i ] #@ "specifiedName" }
;

specifiedNameList:
| n = specifiedName { n }
| ns = specifiedNameList COMMA n = specifiedName
    { [ NT ns; Term ","; NT n ] #@ "specifiedNameList" }
;

enumDeclaration:
| anno = optAnnotations ENUM name = name L_BRACE ns = nameList comma = optTrailingComma R_BRACE
    { [ NT anno; Term "ENUM"; NT name; Term "{"; NT ns; NT comma; Term "}" ]
       #@ "enumDeclaration" }
| anno = optAnnotations ENUM typeRef = typeRef name = name L_BRACE ns = specifiedNameList comma = optTrailingComma R_BRACE
    { [ NT anno; Term "ENUM"; NT typeRef; NT name; Term "{"; NT ns; NT comma; Term "}" ]
       #@ "enumDeclaration" }
;

structField:
| anno = optAnnotations t = typeRef n = name SEMICOLON
    { [ NT anno; NT t; NT n; Term ";" ] #@ "structField" }
;
structFieldList:
| (* empty *) { [ Term "`EMPTY" ] #@ "structFieldList" }
| fs = structFieldList f = structField
    { [ NT fs; NT f ] #@ "structFieldList" }
;

headerUnionDeclaration:
| anno = optAnnotations HEADER_UNION name = name type_params = optTypeParameters
    L_BRACE fields = structFieldList R_BRACE
    { [ NT anno; Term "HEADER_UNION"; NT name; NT type_params; Term "{"; NT fields; Term "}" ]
       #@ "headerUnionDeclaration" }
;

structTypeDeclaration:
| anno = optAnnotations STRUCT name = name type_params = optTypeParameters
    L_BRACE fields = structFieldList R_BRACE
    { [ NT anno; Term "STRUCT"; NT name; NT type_params; Term "{"; NT fields; Term "}" ]
       #@ "structTypeDeclaration" }
;
headerTypeDeclaration:
| anno = optAnnotations HEADER name = name type_params = optTypeParameters
    L_BRACE fields = structFieldList R_BRACE
    { [ NT anno; Term "HEADER"; NT name; NT type_params; Term "{"; NT fields; Term "}" ]
       #@ "headerTypeDeclaration" }
;

derivedTypeDeclaration:
| d = headerTypeDeclaration
| d = headerUnionDeclaration
| d = structTypeDeclaration
| d = enumDeclaration
    { d }
;

typeDefType: (*TODO: inline? *)
| t = typeRef { t }
| d = derivedTypeDeclaration { d }
;

typedefDeclaration:
| anno = optAnnotations TYPEDEF t = typeDefType name = name
    { [ NT anno; Term "TYPEDEF"; NT t; NT name ] #@ "typedefDeclaration" }
| anno = optAnnotations TYPE t = typeRef name = name
    { [ NT anno; Term "TYPE"; NT t; NT name ] #@ "typedefDeclaration" }
;

typeDeclarationWithoutSemicolon: (* TODO: inline? *)
| d = typedefDeclaration
| d = parserTypeDeclaration pop_scope
| d = controlTypeDeclaration pop_scope
| d = packageTypeDeclaration pop_scope
  { d }
;

typeDeclaration:
| d = derivedTypeDeclaration { d }
| d = typeDeclarationWithoutSemicolon SEMICOLON
    { [ NT d; Term ";" ] #@ "typeDeclaration" }
;

declaration:
| const = constantDeclaration
    { declare_var (id_of_declaration const) (has_type_params_declaration const);
      const }
| d = errorDeclaration
| d = matchKindDeclaration
| d = externDeclaration
    { d }
| inst = instantiation
    { declare_var (id_of_declaration inst) false;
      inst }
| func = functionDeclaration
    { declare_var (id_of_declaration func) (has_type_params_declaration func);
      func }
| action = actionDeclaration
    { declare_var (id_of_declaration action) false;
      action }
| d = parserDeclaration
| d = controlDeclaration
| d = typeDeclaration
    { declare_type (id_of_declaration d) (has_type_params_declaration d);
      d }
;

annotationToken:
| UNEXPECTED_TOKEN
    { [ Term "UNEXPECTED_TOKEN" ] #@ "annotationToken" }
| ABSTRACT
    { [ Term "ABSTRACT" ] #@ "annotationToken" }
| ACTION
    { [ Term "ACTION" ] #@ "annotationToken" }
| ACTIONS
    { [ Term "ACTIONS" ] #@ "annotationToken" }
| APPLY
    { [ Term "APPLY" ] #@ "annotationToken" }
| BOOL
    { [ Term "BOOL" ] #@ "annotationToken" }
| BIT
    { [ Term "BIT" ] #@ "annotationToken" }
| BREAK
    { [ Term "BREAK" ] #@ "annotationToken" }
| CONST
    { [ Term "CONST" ] #@ "annotationToken" }
| CONTINUE
    { [ Term "CONTINUE" ] #@ "annotationToken" }
| CONTROL
    { [ Term "CONTROL" ] #@ "annotationToken" }
| DEFAULT
    { [ Term "DEFAULT" ] #@ "annotationToken" }
| ELSE
    { [ Term "ELSE" ] #@ "annotationToken" }
| ENTRIES
    { [ Term "ENTRIES" ] #@ "annotationToken" }
| ENUM
    { [ Term "ENUM" ] #@ "annotationToken" }
| ERROR
    { [ Term "ERROR" ] #@ "annotationToken" }
| EXIT
    { [ Term "EXIT" ] #@ "annotationToken" }
| EXTERN
    { [ Term "EXTERN" ] #@ "annotationToken" }
| FALSE
    { [ Term "FALSE" ] #@ "annotationToken" }
| FOR
    { [ Term "FOR" ] #@ "annotationToken" }
| HEADER
    { [ Term "HEADER" ] #@ "annotationToken" }
| HEADER_UNION
    { [ Term "HEADER_UNION" ] #@ "annotationToken" }
| IF
    { [ Term "IF" ] #@ "annotationToken" }
| IN
    { [ Term "IN" ] #@ "annotationToken" }
| INOUT
    { [ Term "INOUT" ] #@ "annotationToken" }
| INT
    { [ Term "INT" ] #@ "annotationToken" }
| KEY
    { [ Term "KEY" ] #@ "annotationToken" }
| MATCH_KIND
    { [ Term "MATCH_KIND" ] #@ "annotationToken" }
| TYPE
    { [ Term "TYPE" ] #@ "annotationToken" }
| OUT
    { [ Term "OUT" ] #@ "annotationToken" }
| PARSER
    { [ Term "PARSER" ] #@ "annotationToken" }
| PACKAGE
    { [ Term "PACKAGE" ] #@ "annotationToken" }
| PRAGMA
    { [ Term "PRAGMA" ] #@ "annotationToken" }
| RETURN
    { [ Term "RETURN" ] #@ "annotationToken" }
| SELECT
    { [ Term "SELECT" ] #@ "annotationToken" }
| STATE
    { [ Term "STATE" ] #@ "annotationToken" }
| STRING
    { [ Term "STRING" ] #@ "annotationToken" }
| STRUCT
    { [ Term "STRUCT" ] #@ "annotationToken" }
| SWITCH
    { [ Term "SWITCH" ] #@ "annotationToken" }
| TABLE
    { [ Term "TABLE" ] #@ "annotationToken" }
| THIS
    { [ Term "THIS" ] #@ "annotationToken" }
| TRANSITION
    { [ Term "TRANSITION" ] #@ "annotationToken" }
| TRUE
    { [ Term "TRUE" ] #@ "annotationToken" }
| TUPLE
    { [ Term "TUPLE" ] #@ "annotationToken" }
| TYPEDEF
    { [ Term "TYPEDEF" ] #@ "annotationToken" }
| VARBIT
    { [ Term "VARBIT" ] #@ "annotationToken" }
| VALUESET
    { [ Term "VALUESET" ] #@ "annotationToken" }
| LIST
    { [ Term "LIST" ] #@ "annotationToken" }
| VOID
    { [ Term "VOID" ] #@ "annotationToken" }
| DONTCARE
    { [ Term "_" ] #@ "annotationToken" }
| id = identifier
    { id }
| tid = typeIdentifier
    { tid }
| str = stringLiteral
    { str }
| num = number
    { num }
| MASK
    { [ Term "&&&" ] #@ "annotationToken" }
  (* TODO: missing DOTS "..." in spec *)
| RANGE
    { [ Term ".." ] #@ "annotationToken" }
| SHL
    { [ Term "<<" ] #@ "annotationToken" }
| AND
    { [ Term "&&" ] #@ "annotationToken" }
| OR
    { [ Term "||" ] #@ "annotationToken" }
| EQ
    { [ Term "==" ] #@ "annotationToken" }
| NE
    { [ Term "!=" ] #@ "annotationToken" }
| GE
    { [ Term ">=" ] #@ "annotationToken" }
| LE
    { [ Term "<=" ] #@ "annotationToken" }
| PLUSPLUS
    { [ Term "++" ] #@ "annotationToken" }
| PLUS
    { [ Term "+" ] #@ "annotationToken" }
| PLUS_SAT
    { [ Term "|+|" ] #@ "annotationToken" }
| MINUS
    { [ Term "-" ] #@ "annotationToken" }
| MINUS_SAT
    { [ Term "|-|" ] #@ "annotationToken" }
| MUL
    { [ Term "*" ] #@ "annotationToken" }
| DIV
    { [ Term "/" ] #@ "annotationToken" }
| MOD
    { [ Term "%" ] #@ "annotationToken" }
| BIT_OR
    { [ Term "|" ] #@ "annotationToken" }
| BIT_AND
    { [ Term "&" ] #@ "annotationToken" }
| BIT_XOR
    { [ Term "^" ] #@ "annotationToken" }
| COMPLEMENT
    { [ Term "~" ] #@ "annotationToken" }
| L_BRACKET
    { [ Term "[" ] #@ "annotationToken" }
| R_BRACKET
    { [ Term "]" ] #@ "annotationToken" }
| L_BRACE
    { [ Term "{" ] #@ "annotationToken" }
| R_BRACE
    { [ Term "}" ] #@ "annotationToken" }
| L_ANGLE
    { [ Term "<" ] #@ "annotationToken" }
| R_ANGLE
    { [ Term ">" ] #@ "annotationToken" }
| NOT
    { [ Term "!" ] #@ "annotationToken" }
| COLON
    { [ Term ":" ] #@ "annotationToken" }
| COMMA
    { [ Term "," ] #@ "annotationToken" }
| QUESTION
    { [ Term "?" ] #@ "annotationToken" }
| DOT
    { [ Term "." ] #@ "annotationToken" }
| ASSIGN
    { [ Term "=" ] #@ "annotationToken" }
| SEMICOLON
    { [ Term ";" ] #@ "annotationToken" }
| AT
    { [ Term "@" ] #@ "annotationToken" }
;

annotationBody:
| (* empty *) { [ Term "`EMPTY" ] #@ "annotationBody" }
| ab = annotationBody L_PAREN ab_in = annotationBody R_PAREN
    { [ NT ab; Term "("; NT ab_in; Term ")" ] #@ "annotationBody" }
| ab = annotationBody at = annotationToken
    { [ NT ab; NT at ] #@ "annotationBody" }
;

structuredAnnotationBody:
| r = recordExpression comma = optTrailingComma
    { [ NT r; NT comma ] #@ "structuredAnnotationBody" }
;

annotation:
| AT name = name
    { [ Term "@"; NT name ] #@ "annotation" }
| AT name = name L_PAREN body = annotationBody R_PAREN
    { [ Term "@"; NT name; Term "("; NT body; Term ")" ] #@ "annotation" }
| AT name = name L_BRACKET body = structuredAnnotationBody R_BRACKET
    { [ Term "@"; NT name; Term "["; NT body; Term "]" ] #@ "annotation" }
(* From Petr4: PRAGMA not in Spec, but in Petr4/p4c *)
| PRAGMA name = name body = annotationBody PRAGMA_END
  { [ Term "PRAGMA"; NT name; NT body; Term "PRAGMA_END" ] #@ "annotation" }
;

annotations:
| an = annotation { an }
| ans = annotations an = annotation
  { [ NT ans; NT an ] #@ "annotations" }
;

optAnnotations: (* TODO: inline? *)
| (* empty *) { [ Term "`EMPTY" ] #@ "optAnnotations" }
| anno = annotations { anno }
;

(******** P4 program ********)
declarationList:
| (* empty *) { [ Term "`EMPTY" ] #@ "declarationList" }
| ds = declarationList d = declaration
  { [ NT ds; NT d ] #@ "declarationList" }
| ds = declarationList SEMICOLON
  { [ NT ds; Term ";" ] #@ "declarationList" }
;

p4program:
| ds = declarationList END { ds }
