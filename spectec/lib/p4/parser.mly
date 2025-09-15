%{
  open Il.Ast
  open Context
  open Concrete.Ast_utils
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
%token<Source.info> SWITCH TABLE THIS TRANSITION TUPLE TYPEDEF TYPE VALUE_SET VARBIT VOID
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

%start p4program

(**************************** TYPES ******************************)
%type <Il.Ast.value>
  (* Aux *) int externName declarationList
  (* Misc *) trailingCommaOpt (* Numbers *) number (* Strings *) stringLiteral
  (* Names *)
  identifier typeIdentifier nonTypeName prefixedNonTypeName typeName prefixedTypeName tableCustomName name nameList member
  (* Directions *) direction
  (* Types *)
  baseType specializedType namedType headerStackType listType tupleType typeRef typeOrVoid
  (* Type parameters *) typeParameter typeParameterList typeParameterListOpt
  (* Parameters *) parameter nonEmptyParameterList parameterList 
  (* Constructor parameters *) constructorParameterListOpt
  (* Expression key-value pairs *) namedExpression namedExpressionList
  (* Expressions *)
  literalExpression referenceExpression defaultExpression 
  (* >> Unary, binary, and ternary expressions *) 
  unop unaryExpression binop binaryExpression binaryExpressionNonBrace ternaryExpression ternaryExpressionNonBrace 
  (* >> Cast expressions *) castExpression 
  (* >> Data (aggregate) expressions *) dataExpression
  (* >> Member and index access expressions *)
  errorAccessExpression memberAccessExpression indexAccessExpression accessExpression
  memberAccessExpressionNonBrace indexAccessExpressionNonBrace accessExpressionNonBrace
  (* >> Call expressions *)
  routineTarget constructorTarget callTarget callExpression
  routineTargetNonBrace callTargetNonBrace callExpressionNonBrace
  (* >> Parenthesized Expressions *) parenthesizedExpression
  (* >> Expressions *)
  expression expressionList memberAccessBase sequenceElementExpression recordElementExpression dataElementExpression
  (* >> Non-brace Expressions *) expressionNonBrace memberAccessBaseNonBrace
  (* Keyset Expressions *) simpleKeysetExpression simpleKeysetExpressionList tupleKeysetExpression keysetExpression
  (* Type arguments *)
  realTypeArgument realTypeArgumentList typeArgument typeArgumentList argument argumentListNonEmpty argumentList
  (* L-values *) lvalue
  (* Statements *)
  emptyStatement assignop assignmentStatement callStatement directApplicationStatement returnStatement exitStatement blockStatement conditionalStatement 
  (* >> For statements *)
  forInitStatement forInitStatementListNonEmpty forInitStatementList forUpdateStatement forUpdateStatementListNonEmpty
  forUpdateStatementList forCollectionExpression forStatement
  (* >> Switch statements *) switchLabel switchCase switchCaseList switchStatement
  breakStatement continueStatement statement
  (* Declarations *)
  (* >> Constant and variable declarations *)
  initialValue constantDeclaration initializerOpt variableDeclaration blockElementStatement blockElementStatementList
  (* >> Function declarations *) functionPrototype functionDeclaration 
  (* >> Action declarations *) actionDeclaration
  (* >> Instantiations *) objectInitializer instantiation objectDeclaration objectDeclarationList
  (* >> Error declarations *) errorDeclaration
  (* >> Match kind declarations *) matchKindDeclaration
  (* >> Derived type declarations *)
  enumTypeDeclaration typeField typeFieldList structTypeDeclaration headerTypeDeclaration headerUnionTypeDeclaration derivedTypeDeclaration
  (* >> Typedef and newtype declarations *) typedefType typedefDeclaration
  (* >> Extern declarations *)
  externFunctionDeclaration methodPrototype methodPrototypeList externObjectDeclaration externDeclaration
  (* >> Parser statements and declarations *)
  (* >>>> Select expressions *) selectCase selectCaseList selectExpression
  (* >>>> Transition statements *) stateExpression transitionStatement
  (* >>>> Value set declarations *) valueSetType valueSetDeclaration
  (* >>>> Parser type declarations *) parserTypeDeclaration
  (* >>>> Parser Declarations *)
  parserBlockStatement parserStatement parserStatementList parserState
  parserStateList parserLocalDeclaration parserLocalDeclarationList parserDeclaration
  (* >> Control statements and declarations *)
  (* >>>> Table declarations *) constOpt
  (* >>>>>> Table key property *) tableKey tableKeyList
  (* >>>>>> Table actions property *) tableActionReference tableAction tableActionList
  (* >>>>>> Table entry property *) tableEntryPriority tableEntry tableEntryList
  (* >>>>>> Table properties *) tableProperty tablePropertyList tableDeclaration
  (* >>>> Control type declarations *) controlTypeDeclaration
  (* >>>> Control declarations *) controlBody controlLocalDeclaration controlLocalDeclarationList controlDeclaration
  (* >> Package type declarations *) packageTypeDeclaration
  (* >> Type declarations *) typeDeclaration
  (* >> Declaration *) declaration
  (* Annotations *) annotationToken annotationBody structuredAnnotationBody annotation annotationListNonEmpty annotationList p4program
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
;
push_externName:
  | n = externName
    { push_scope();
      declare_type_of_il n false;
      n }
;
pop_scope:
  | (* empty *)
    { pop_scope() }
;
go_toplevel:
  | (* empty *)
    { go_toplevel () }
;
go_local:
  | (* empty *)
    { go_local () }
;
toplevel(X):
  | go_toplevel x = X go_local
    { x }
;

(**************************** P4-16 GRAMMAR ******************************)
(* Aux *)
externName:
	| n = nonTypeName
		{ declare_type_of_il n false;
      n }
;
int:
	| int = NUMBER_INT
    { fst int }
;

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

(* Misc *)
trailingCommaOpt:
	| (* empty *)
    { [ Term "`EMPTY" ] #@ "trailingCommaOpt" }
	| COMMA
    { [ Term "," ] #@ "trailingCommaOpt" }
;

(* Numbers *)
number:
	| int = int
    { [ Term "D"; NT int ] #@ "number" }
(* Processed by lexer *)
	| number = NUMBER
    { fst number }
;

(* Strings *)
stringLiteral:
	| text = STRING_LITERAL
    { [ Term (Char.escaped '"'); NT text; Term (Char.escaped '"') ] #@ "stringLiteral"}
;

(* Names *)
identifier:
	| text = NAME IDENTIFIER
    { [ Term "`ID"; NT text ] #@ "identifier" }
;

typeIdentifier:
	| text = NAME TYPENAME
    { [ Term "`TID"; NT text ] #@ "typeIdentifier" }
;

(* >> Non-type names *)
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
	| DOT go_toplevel n = nonTypeName go_local
    { [ Term "`ID"; Term "."; NT n ] #@ "prefixedNonTypeName" }
;

(* >> Type names *)
typeName:
	| n = typeIdentifier { n }
;

prefixedTypeName:
	| n = typeName { n }
	| DOT go_toplevel tid = typeName go_local
		{ [ Term "`TID"; Term "."; NT tid ] #@ "prefixedType" }
;

(* >> Table custom property names *)
tableCustomName:
	| id = identifier { id }
	| tid = typeIdentifier { tid }
	| APPLY { [ Term "APPLY" ] #@ "tableCustomName" }
	| STATE { [ Term "STATE" ] #@ "tableCustomName" }
	| TYPE { [ Term "TYPE" ] #@ "tableCustomName" }
	| PRIORITY { [ Term "PRIORITY" ] #@ "tableCustomName" }
;

(* >> Names *)
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

member:
	| name = name
    { name }
;

(* Directions *)
direction:
	| (* empty *) { [ Term "`EMPTY" ] #@ "direction" }
	| IN { [ Term "IN" ] #@ "direction" }
	| OUT { [ Term "OUT" ] #@ "direction" }
	| INOUT { [ Term "INOUT" ] #@ "direction" }
;

(* Types *)
(* >> Base types *)
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

(* >> Named types *)
specializedType:
  | n = prefixedTypeName l_angle tal = typeArgumentList r_angle
    { [ NT n; Term "<"; NT tal; Term ">" ] #@ "specializedType" }
;

namedType:
  | t = prefixedTypeName
  | t = specializedType
    { t }
;

(* >> Header stack types *)
headerStackType:
  | t = namedType L_BRACKET e = expression R_BRACKET
    { [ NT t; Term "["; NT e; Term "]" ] #@ "headerStackType" }
;

(* >> List types *)
listType:
  | LIST l_angle targ = typeArgument r_angle
    { [ Term "LIST"; Term "<"; NT targ; Term ">" ] #@ "listType" }
;

(* >> Tuple types *)
tupleType:
	| TUPLE l_angle targs = typeArgumentList r_angle
    { [ Term "TUPLE"; Term "<"; NT targs; Term ">" ] #@ "tupleType" }
;

(* >> Types *)
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

(* Type parameters *)
typeParameter:
	| n = name { n }

typeParameterList:
	| tp = typeParameter { tp }
	| tps = typeParameterList COMMA tp = typeParameter
    { [ NT tps; Term ","; NT tp ] #@ "typeParameterList" }
;

typeParameterListOpt:
	| (* empty *) { [ Term "`EMPTY" ] #@ "typeParameterListOpt" }
	| l_angle tps = typeParameterList r_angle
    { declare_types_of_il tps;
      [ Term "<"; NT tps; Term ">" ] #@ "typeParameterListOpt" }
;

(* Parameters *)
parameter:
	| al = annotationList dir = direction t = typeRef n = name i = initializerOpt
		{ declare_var_of_il n false;
      [ NT al; NT dir; NT t; NT n; NT i ] #@ "parameter" }
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

(* Constructor parameters *)
constructorParameterListOpt:
	| (* empty *) { [ Term "`EMPTY" ] #@ "constructorParameterListOpt" }
	| L_PAREN ps = parameterList R_PAREN
    { [ Term "("; NT ps; Term ")" ] #@ "constructorParameterListOpt" }
;

(* Expression key-value pairs *)
namedExpression:
	| n = name ASSIGN e = expression { [ NT n; Term "="; NT e ] #@ "namedExpression" }
;

namedExpressionList:
	| e = namedExpression { e }
	| es = namedExpressionList COMMA e = namedExpression { [ NT es; Term ","; NT e ] #@ "namedExpressionList" }
;

(* Expressions *)
(* >> Literal expressions *)
%inline literalExpression:
	| TRUE { [ Term "TRUE" ] #@ "literalExpression" }
	| FALSE { [ Term "FALSE" ] #@ "literalExpression" }
	| num = number { num }
	| str = stringLiteral { str }
;

(* >> Reference expressions *)
%inline referenceExpression:
	| n = prefixedNonTypeName { n }
	| THIS { [ Term "THIS" ] #@ "referenceExpression" }
;

(* >> Default expressions *)
%inline defaultExpression:
	| DOTS { [ Term "..." ] #@ "defaultExpression" }
;

(* >> Unary, binary, and ternary expressions *)
%inline unop: 
	| NOT { [ Term "!" ] #@ "unop" }
	| COMPLEMENT { [ Term "~" ] #@ "unop" }
	| MINUS { [ Term "-" ] #@ "unop" }
	| PLUS { [ Term "+" ] #@ "unop" }
;

%inline unaryExpression:
	| o = unop e = expression %prec PREFIX
		{ [ NT o; NT e ] #@ "unaryExpression" }
;

%inline binop:
  | MUL { [ Term "*" ] #@ "binop" }
  | DIV { [ Term "/" ] #@ "binop" }
  | MOD { [ Term "%" ] #@ "binop" }
  | PLUS { [ Term "+" ] #@ "binop" }
  | PLUS_SAT { [ Term "|+|" ] #@ "binop" }
  | MINUS { [ Term "-" ] #@ "binop" }
  | MINUS_SAT { [ Term "|-|" ] #@ "binop" }
  | SHL { [ Term "<<" ] #@ "binop" }
  | r_angle R_ANGLE_SHIFT { [ Term ">>" ] #@ "binop" }
  | LE { [ Term "<=" ] #@ "binop" }
  | GE { [ Term ">=" ] #@ "binop" }
  | l_angle { [ Term "<" ] #@ "binop" }
  | r_angle { [ Term ">" ] #@ "binop" }
  | NE { [ Term "!=" ] #@ "binop" }
  | EQ { [ Term "==" ] #@ "binop" }
  | BIT_AND { [ Term "&" ] #@ "binop" }
  | BIT_XOR { [ Term "^" ] #@ "binop" }
  | BIT_OR { [ Term "|" ] #@ "binop" }
  | PLUSPLUS { [ Term "++" ] #@ "binop" }
  | AND { [ Term "&&" ] #@ "binop" }
  | OR { [ Term "||" ] #@ "binop" }
;

%inline binaryExpression:
	| l = expression o = binop r = expression
		{ [ NT l; NT o; NT r ] #@ "binaryExpression" }
;

%inline binaryExpressionNonBrace:
	| l = expressionNonBrace o = binop r = expression
		{ [ NT l; NT o; NT r ] #@ "binaryExpressionNonBrace" }
;

%inline ternaryExpression:
	| c = expression QUESTION t = expression COLON f = expression
		{ [ NT c; Term "?"; NT t; Term ":"; NT f ] #@ "ternaryExpression" }
;

%inline ternaryExpressionNonBrace:
	| c = expressionNonBrace QUESTION t = expression COLON f = expression
		{ [ NT c; Term "?"; NT t; Term ":"; NT f ] #@ "ternaryExpressionNonBrace" }
;

(* >> Cast expressions *)
%inline castExpression:
	| L_PAREN t = typeRef R_PAREN e = expression %prec PREFIX
    { [ Term "("; NT t; Term ")"; NT e ] #@ "castExpression" }
;

(* >> Data (aggregate) expressions *)
%inline dataExpression:
	| INVALID { [ Term "{#}" ] #@ "dataExpression" }
	| L_BRACE e = dataElementExpression c = trailingCommaOpt R_BRACE
    { [ Term "{"; NT e; NT c; Term "}" ] #@ "dataExpression" }
;

(* >> Member and index access expressions *)
%inline errorAccessExpression:
	| ERROR DOT m = member
		{ [ Term "ERROR"; Term "."; NT m ] #@ "errorAccessExpression" }
;

%inline memberAccessExpression:
	| e = memberAccessBase DOT m = member %prec DOT
		{ [ NT e; Term "."; NT m ] #@ "memberAccessExpression" }
;

%inline indexAccessExpression:
	| a = expression L_BRACKET i = expression R_BRACKET
		{ [ NT a; Term "["; NT i; Term "]" ] #@ "indexAccessExpression" }
	| a = expression L_BRACKET h = expression COLON l = expression R_BRACKET
		{ [ NT a; Term "["; NT h; Term ":"; NT l; Term "]" ] #@ "indexAccessExpression" }
;

%inline accessExpression:
	| e = errorAccessExpression
	| e = memberAccessExpression
	| e = indexAccessExpression
		{ e }
;

%inline memberAccessExpressionNonBrace:
	| e = memberAccessBaseNonBrace DOT m = member %prec DOT
		{ [ NT e; Term "."; NT m ] #@ "memberAccessExpressionNonBrace" }
;

%inline indexAccessExpressionNonBrace:
	| a = expressionNonBrace L_BRACKET i = expression R_BRACKET
		{ [ NT a; Term "["; NT i; Term "]" ] #@ "indexAccessExpressionNonBrace" }
	| a = expressionNonBrace L_BRACKET h = expression COLON l = expression R_BRACKET
		{ [ NT a; Term "["; NT h; Term ":"; NT l; Term "]" ] #@ "indexAccessExpressionNonBrace" }
;

%inline accessExpressionNonBrace:
	| e = errorAccessExpression
	| e = memberAccessExpressionNonBrace
	| e = indexAccessExpressionNonBrace
		{ e }
;

(* >> Call expressions *)
%inline routineTarget:
  | e = expression { e }
;

%inline constructorTarget:
	| n = namedType { n }
;

%inline callTarget:
	| t = routineTarget
	| t = constructorTarget
		{ t }
;

%inline callExpression:
	| t = callTarget L_PAREN args = argumentList R_PAREN
		{ [ NT t; Term "("; NT args; Term ")" ] #@ "callExpression" }
	| t = routineTarget l_angle targs = realTypeArgumentList r_angle L_PAREN args = argumentList R_PAREN
		{ [ NT t; Term "<"; NT targs; Term ">"; Term "("; NT args; Term ")" ]
      #@ "callExpression" }
;

%inline routineTargetNonBrace:
  | e = expressionNonBrace { e }
;

%inline callTargetNonBrace:
	| t = routineTargetNonBrace
	| t = constructorTarget
		{ t }
;

%inline callExpressionNonBrace:
	| t = callTargetNonBrace L_PAREN args = argumentList R_PAREN
		{ [ NT t; Term "("; NT args; Term ")" ] #@ "callExpressionNonBrace" }
	| t = routineTargetNonBrace l_angle targs = realTypeArgumentList r_angle L_PAREN args = argumentList R_PAREN
		{ [ NT t; Term "<"; NT targs; Term ">"; Term "("; NT args; Term ")" ]
      #@ "callExpressionNonBrace" }

(* >> Parenthesized Expressions *)

%inline parenthesizedExpression:
	| L_PAREN e = expression R_PAREN
		{ [ Term "("; NT e; Term ")" ] #@ "parenthesizedExpression" }
;

(* >> Expressions *)
expression:
	| e = literalExpression
	| e = referenceExpression
	| e = defaultExpression
	| e = unaryExpression
	| e = binaryExpression
	| e = ternaryExpression
	| e = castExpression
	| e = dataExpression
	| e = accessExpression
	| e = callExpression
	| e = parenthesizedExpression
		{ e }
;

expressionList:
	| (* empty *) { [ Term "`EMPTY" ] #@ "expressionList" }
	| e = expression { e }
	| el = expressionList COMMA e = expression
		{ [ NT el; Term ","; NT e ] #@ "expressionList" }
;

%inline memberAccessBase:
	| e = prefixedTypeName
	| e = expression
		{ e }
;

%inline sequenceElementExpression:
	| el = expressionList { el }
;

%inline recordElementExpression:
  | n = name ASSIGN e = expression
    { [ NT n; Term "="; NT e ]
      #@ "recordElementExpression" }
  | n = name ASSIGN e = expression COMMA DOTS
    { [ NT n; Term "="; NT e; Term ","; Term "..." ]
      #@ "recordElementExpression" }
	| n = name ASSIGN e = expression COMMA el = namedExpressionList
    { [ NT n; Term "="; NT e; Term ","; NT el ]
      #@ "recordElementExpression" }
  | n = name ASSIGN e = expression COMMA el = namedExpressionList COMMA DOTS
    { [ NT n; Term "="; NT e; Term ","; NT el; Term ","; Term "..." ]
      #@ "recordElementExpression" }
;

%inline dataElementExpression:
	| e = sequenceElementExpression
	| e = recordElementExpression 
    { e }
;

(* >> Non-brace Expressions *)
expressionNonBrace:
	| e = literalExpression
	| e = referenceExpression
	| e = unaryExpression
	| e = binaryExpressionNonBrace
	| e = ternaryExpressionNonBrace
	| e = castExpression
	| e = accessExpressionNonBrace
	| e = callExpressionNonBrace
	| e = parenthesizedExpression
		{ e }
;

%inline memberAccessBaseNonBrace:
	| e = prefixedTypeName
	| e = expressionNonBrace
		{ e }
;

(* Keyset Expressions *)
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

simpleKeysetExpressionList:
	| e = simpleKeysetExpression { e }
	| el = simpleKeysetExpressionList COMMA e = simpleKeysetExpression
    { [ NT el; Term ","; NT e ] #@ "simpleKeysetExpressionList" }
;

tupleKeysetExpression:
	| L_PAREN b = expression MASK m = expression R_PAREN
		{ [ Term "("; NT b; Term "&&&"; NT m; Term ")" ] #@ "tupleKeysetExpression" }
	| L_PAREN l = expression RANGE h = expression R_PAREN
		{ [ Term "("; NT l; Term ".."; NT h; Term ")" ] #@ "tupleKeysetExpression" }
	| L_PAREN DEFAULT R_PAREN
		{ [ Term "("; Term "DEFAULT"; Term ")" ] #@ "tupleKeysetExpression" }
	| L_PAREN DONTCARE R_PAREN
		{ [ Term "("; Term "_"; Term ")" ] #@ "tupleKeysetExpression" }
	| L_PAREN e = simpleKeysetExpression COMMA es = simpleKeysetExpressionList R_PAREN
		{ [ Term "("; NT e; Term ","; NT es; Term ")" ] #@ "tupleKeysetExpression" }
;

keysetExpression:
	| e = simpleKeysetExpression
	| e = tupleKeysetExpression
    { e }
;

(* Type arguments *)
realTypeArgument:
	| t = typeRef { t }
	| VOID
    { [ Term "VOID" ] #@ "realTypeArgument" }
	| DONTCARE
    { [ Term "_" ] #@ "realTypeArgument" }
;

realTypeArgumentList:
	| targ = realTypeArgument { targ }
	| targs = realTypeArgumentList COMMA targ = realTypeArgument
    { [ NT targs; Term ","; NT targ ] #@ "realTypeArgumentList" }
;

typeArgument:
	| t = typeRef
	| t = nonTypeName 
		{ t }
	| VOID
    { [ Term "VOID" ] #@ "typeArgument" }
	| DONTCARE
    { [ Term "_" ] #@ "typeArgument" }
;

typeArgumentList:
	| (* empty *) { [ Term "`EMPTY" ] #@ "typeArgumentList" }
	| targ = typeArgument { targ }
	| targs = typeArgumentList COMMA targ = typeArgument
    { [ NT targs; Term ","; NT targ ] #@ "typeArgumentList" }
;

(* Arguments *)
argument:
	| e = expression { e }
	| n = name ASSIGN e = expression 
		{ [ NT n; Term "="; NT e ] #@ "argument" }
	| name = name ASSIGN DONTCARE
		{ [ NT name; Term "="; Term "_" ] #@ "argument" }
	| DONTCARE
		{ [ Term "_" ] #@ "argument" }
;

argumentListNonEmpty:
	| arg = argument { arg }
	| args = argumentListNonEmpty COMMA arg = argument
    { [ NT args; Term ","; NT arg ] #@ "argumentListNonEmpty" }
;

argumentList:
	| (* empty *) { [ Term "`EMPTY" ] #@ "argumentList" }
	| args = argumentListNonEmpty { args }
;

(* L-values *)
lvalue:
	| e = referenceExpression { e }
	| lv = lvalue DOT m = member %prec DOT
		{ [ NT lv; Term "."; NT m ] #@ "lvalue" }
	| lv = lvalue L_BRACKET i = expression R_BRACKET
		{ [ NT lv; Term "["; NT i; Term "]" ] #@ "lvalue" }
	| lv = lvalue L_BRACKET h = expression COLON l = expression R_BRACKET
		{ [ NT lv; Term "["; NT h; Term ":"; NT l; Term "]" ] #@ "lvalue" }
	| L_PAREN lv = lvalue R_PAREN
		{ [ Term "("; NT lv; Term ")" ] #@ "lvalue" }
;

(* Statements *)
(* >> Empty statements *)
emptyStatement:
	| SEMICOLON { [ Term ";" ] #@ "emptyStatement" }
;

(* >> Assignment statements *)
assignop:
	| ASSIGN { [ Term "=" ] #@ "assignop" }
	| PLUS_ASSIGN { [ Term "+=" ] #@ "assignop" }
	| PLUS_SAT_ASSIGN { [ Term "|+|=" ] #@ "assignop" }
	| MINUS_ASSIGN { [ Term "-=" ] #@ "assignop" }
	| MINUS_SAT_ASSIGN { [ Term "|-|=" ] #@ "assignop" }
	| MUL_ASSIGN { [ Term "*=" ] #@ "assignop" }
	| DIV_ASSIGN { [ Term "/=" ] #@ "assignop" }
	| MOD_ASSIGN { [ Term "%=" ] #@ "assignop" }
	| SHL_ASSIGN { [ Term "<<=" ] #@ "assignop" }
	| SHR_ASSIGN { [ Term ">>=" ] #@ "assignop" }
	| BIT_AND_ASSIGN { [ Term "&=" ] #@ "assignop" }
	| BIT_XOR_ASSIGN { [ Term "^=" ] #@ "assignop" }
	| BIT_OR_ASSIGN { [ Term "|=" ] #@ "assignop" }
;

assignmentStatement:
	| lv = lvalue o = assignop e = expression SEMICOLON
		{ [ NT lv; NT o; NT e; Term ";" ] #@ "assignmentStatement" }
;

(* >> Call statements *)
callStatement:
	| lv = lvalue L_PAREN args = argumentList R_PAREN SEMICOLON
		{ [ NT lv; Term "("; NT args; Term ")"; Term ";" ] #@ "callStatement" }
	| lv = lvalue l_angle targs = typeArgumentList r_angle L_PAREN args = argumentList R_PAREN SEMICOLON
		{ [ NT lv; Term "<"; NT targs; Term ">"; Term "("; NT args; Term ")"; Term ";" ]
      #@ "callStatement" }
;

(* >> Direct application statements *)
directApplicationStatement:
	| t = namedType DOT APPLY L_PAREN args = argumentList R_PAREN SEMICOLON
    { [ NT t; Term "."; Term "APPLY"; Term "("; NT args; Term ")"; Term ";" ]
      #@ "directApplicationStatement" }
;

(* >> Return statements *)
returnStatement:
	| RETURN SEMICOLON
    { [ Term "RETURN"; Term ";" ] #@ "returnStatement" }
	| RETURN e = expression SEMICOLON
    { [ Term "RETURN"; NT e; Term ";" ] #@ "returnStatement" }
;

(* >> Exit statements *)
exitStatement:
	| EXIT SEMICOLON
    { [ Term "EXIT"; Term ";" ] #@ "exitStatement" }
;

(* >> Block statements *)
blockStatement:
	| al = annotationList L_BRACE
  push_scope
  sl = blockElementStatementList R_BRACE
  pop_scope
		{ [ NT al; Term "{"; NT sl; Term "}" ] #@ "blockStatement" }
;

(* >> Conditional statements *)
conditionalStatement:
	| IF L_PAREN c = expression R_PAREN t = statement %prec THEN
    { [ Term "IF"; Term "("; NT c; Term ")"; NT t ]
      #@ "conditionalStatement" }
	| IF L_PAREN c = expression R_PAREN t = statement ELSE f = statement
    { [ Term "IF"; Term "("; NT c; Term ")"; NT t; Term "ELSE"; NT f ]
      #@ "conditionalStatement" }
;

(* >> For statements *)
forInitStatement:
	| al = annotationList t = typeRef n = name i = initializerOpt
		{ [ NT al; NT t; NT n; NT i ] #@ "forInitStatement" }
	| lv = lvalue L_PAREN args = argumentList R_PAREN
		{ [ NT lv; Term "("; NT args; Term ")" ] #@ "forInitStatement" }
	| lv = lvalue l_angle targs = typeArgumentList r_angle L_PAREN args = argumentList R_PAREN
		{ [ NT lv; Term "<"; NT targs; Term ">"; Term "("; NT args; Term ")" ]
      #@ "forInitStatement" }
	| lv = lvalue o = assignop e = expression
		{ [ NT lv; NT o; NT e ] #@ "forInitStatement" }
;

forInitStatementListNonEmpty:
	| s = forInitStatement { s }
	| sl = forInitStatementListNonEmpty COMMA s = forInitStatement
    { [ NT sl; Term ","; NT s ] #@ "forInitStatementListNonEmpty" }
;

forInitStatementList:
	| (* empty *) { [ Term "`EMPTY" ] #@ "forInitStatementList" }
	| sl = forInitStatementListNonEmpty { sl }
;

forUpdateStatement:
	| s = forInitStatement { s }
;

forUpdateStatementListNonEmpty:
	| s = forUpdateStatement { s }
	| sl = forUpdateStatementListNonEmpty COMMA s = forUpdateStatement
    { [ NT sl; Term ","; NT s ] #@ "forUpdateStatementListNonEmpty" }
;

forUpdateStatementList:
	| (* empty *) { [ Term "`EMPTY" ] #@ "forUpdateStatementList" }
	| sl = forUpdateStatementListNonEmpty { sl }
;

forCollectionExpression:
	| e = expression { e }
	| l = expression RANGE h = expression
    { [ NT l; Term ".."; NT h ] #@ "forCollectionExpr" }
;

forStatement:
  | al = annotationList FOR L_PAREN il = forInitStatementList SEMICOLON c = expression SEMICOLON ul = forUpdateStatementList R_PAREN b = statement
		{ [ NT al; Term "FOR"; Term "("; NT il; Term ";"; NT c; Term ";"; NT ul; Term ")"; NT b ]
      #@ "forStatement" }
  | al = annotationList FOR L_PAREN
    t = typeRef n = name IN e = forCollectionExpression R_PAREN b = statement
    { [ NT al; Term "FOR"; Term "("; NT t; NT n; Term "IN"; NT e; Term ")"; NT b ]
      #@ "forStatement" }
  | al = annotationList FOR L_PAREN
    al_in = annotationList t = typeRef n = name IN e = forCollectionExpression R_PAREN b = statement
    { [ NT al; Term "FOR"; Term "("; NT al_in; NT t; NT n; Term "IN"; NT e; Term ")"; NT b ]
      #@ "forStatement" }
;

(* >> Switch statements *)
switchLabel:
  | DEFAULT
    { [ Term "DEFAULT" ] #@ "switchLabel" }
  | e = expressionNonBrace
    { e }
;

switchCase:
  | l = switchLabel COLON s = blockStatement
    { [ NT l; Term ":"; NT s ] #@ "switchCase" }
  | l = switchLabel COLON
    { [ NT l; Term ":" ] #@ "switchCase" }
;

switchCaseList:
  | (* empty *)
    { [ Term "`EMPTY" ] #@ "switchCaseList" }
  | cs = switchCaseList c = switchCase
    { [ NT cs; NT c ] #@ "switchCaseList" }
;

switchStatement:
  | SWITCH L_PAREN e = expression R_PAREN L_BRACE cs = switchCaseList R_BRACE
    { [ Term "SWITCH"; Term "("; NT e; Term ")"; Term "{"; NT cs; Term "}" ]
      #@ "switchStatement" }

(* >> Break and continue statements *)
breakStatement:
  | BREAK SEMICOLON
    { [ Term "BREAK"; Term ";" ] #@ "breakStatement" }
;

continueStatement:
  | CONTINUE SEMICOLON
    { [ Term "CONTINUE"; Term ";" ] #@ "continueStatement" }
;

(* >> Statements *)
statement:
  | s = emptyStatement
  | s = assignmentStatement
  | s = callStatement
  | s = directApplicationStatement
  | s = returnStatement
  | s = exitStatement
  | s = blockStatement
  | s = conditionalStatement
  | s = forStatement
  | s = breakStatement
  | s = continueStatement
  | s = switchStatement
    { s }
;

(* Declarations *)
(* >> Constant and variable declarations *)

(* initializer -> initialValue due to reserved word in OCaml *)
initialValue:
	| ASSIGN e = expression
		{ [ Term "="; NT e ] #@ "initializer" }
;

constantDeclaration:
  | al = annotationList CONST t = typeRef n = name i = initialValue SEMICOLON
    { [ NT al; Term "CONST"; NT t; NT n; NT i; Term ";" ] #@ "constantDeclaration" }
;

initializerOpt:
	| (* empty *)
		{ [ Term "`EMPTY" ] #@ "initializerOpt" }
	| i = initialValue { i }
;

variableDeclaration:
  | al = annotationList t = typeRef n = name i = initializerOpt SEMICOLON
    { declare_var_of_il n false;
      [ NT al; NT t; NT n; NT i; Term ";" ] #@ "variableDeclaration" }
;

blockElementStatement:
  | d = constantDeclaration
  | d = variableDeclaration
  | d = statement
    { d }
;

blockElementStatementList:
  | (* empty *)
    { [ Term "`EMPTY" ] #@ "blockElementStatementList" }
  | sl = blockElementStatementList s = blockElementStatement
    { [ NT sl; NT s ] #@ "blockElementStatementList" }
;

(* >> Function declarations *)
functionPrototype:
	| t = typeOrVoid n = name push_scope
  tpl = typeParameterListOpt
  L_PAREN pl = parameterList R_PAREN
    { [ NT t; NT n; NT tpl; Term "("; NT pl; Term ")" ]
      #@ "functionPrototype" }
;

functionDeclaration:
	| al = annotationList p = functionPrototype b = blockStatement pop_scope
    { [ NT al; NT p; NT b ] #@ "functionDeclaration" }
;

(* >> Action declarations *)
actionDeclaration: 
  | al = annotationList ACTION n = name L_PAREN pl = parameterList R_PAREN s = blockStatement
    { [ NT al; Term "ACTION"; NT n; Term "("; NT pl; Term ")"; NT s ]
      #@ "actionDeclaration" }
;

(* >> Instantiations *)
objectInitializer:
	| ASSIGN L_BRACE ds = objectDeclarationList R_BRACE
    { [ Term "="; Term "{"; NT ds; Term "}" ] #@ "objectInitializer" }
;

instantiation:
	| al = annotationList t = typeRef L_PAREN args = argumentList R_PAREN n = name SEMICOLON
    { [ NT al; NT t; Term "("; NT args; Term ")"; NT n; Term ";" ]
      #@ "instantiation" }
	| al = annotationList t = typeRef L_PAREN args = argumentList R_PAREN n = name i = objectInitializer SEMICOLON
    { [ NT al; NT t; Term "("; NT args; Term ")"; NT n; NT i; Term ";" ]
      #@ "instantiation" }
;

objectDeclaration:
	| d = functionDeclaration
	| d = instantiation
    { d }
;

objectDeclarationList:
	| (* empty *) { [ Term "`EMPTY" ] #@ "objectDeclarationList" }
	| ds = objectDeclarationList d = objectDeclaration
    { [ NT ds; NT d ] #@ "objectDeclarationList" }
;

(* >> Error declarations *)
errorDeclaration:
	| ERROR L_BRACE nl = nameList R_BRACE
    { declare_vars_of_il nl;
      [ Term "ERROR"; Term "{"; NT nl; Term "}" ] #@ "errorDeclaration" }
;

(* >> Match kind declarations *)
matchKindDeclaration:
	| MATCH_KIND L_BRACE nl = nameList c = trailingCommaOpt R_BRACE
    { declare_vars_of_il nl;
      [ Term "MATCH_KIND"; Term "{"; NT nl; NT c; Term "}" ] #@ "matchKindDeclaration" }
;

(* >> Derived type declarations *)
(* >>>> Enum type declarations *)
enumTypeDeclaration:
  | al = annotationList ENUM n = name L_BRACE
    nl = nameList c = trailingCommaOpt R_BRACE
    { [ NT al; Term "ENUM"; NT n; Term "{"; NT nl; NT c; Term "}" ]
      #@ "enumTypeDeclaration" }
  | al = annotationList ENUM t = typeRef n = name L_BRACE
    el = namedExpressionList c = trailingCommaOpt R_BRACE
    { [ NT al; Term "ENUM"; NT t; NT n; Term "{"; NT el; NT c; Term "}" ]
      #@ "enumTypeDeclaration" }
;

(* >>>>>> Struct, header, and union type declarations *)
typeField:
  | al = annotationList t = typeRef n = name SEMICOLON
    { [ NT al; NT t; NT n; Term ";" ] #@ "typeField" }
;

typeFieldList:
  | (* empty *) { [ Term "`EMPTY" ] #@ "typeFieldList" }
  | fl = typeFieldList f = typeField
    { [ NT fl; NT f ] #@ "typeFieldList" }
;

structTypeDeclaration:
  | al = annotationList STRUCT n = name tpl = typeParameterListOpt
      L_BRACE fl = typeFieldList R_BRACE
    { [ NT al; Term "STRUCT"; NT n; NT tpl; Term "{"; NT fl; Term "}" ]
      #@ "structTypeDeclaration" }
;

headerTypeDeclaration:
  | al = annotationList HEADER n = name tpl = typeParameterListOpt
      L_BRACE fl = typeFieldList R_BRACE
    { [ NT al; Term "HEADER"; NT n; NT tpl; Term "{"; NT fl; Term "}" ]
      #@ "headerTypeDeclaration" }
;

headerUnionTypeDeclaration:
  | al = annotationList HEADER_UNION n = name tpl = typeParameterListOpt
      L_BRACE fl = typeFieldList R_BRACE
    { [ NT al; Term "HEADER_UNION"; NT n; NT tpl; Term "{"; NT fl; Term "}" ]
      #@ "headerUnionTypeDeclaration" }
;

derivedTypeDeclaration:
  | d = enumTypeDeclaration
  | d = structTypeDeclaration
  | d = headerTypeDeclaration
  | d = headerUnionTypeDeclaration
    { d }
;

(* >> Typedef and newtype declarations *)
typedefType:
	| t = typeRef
	| t = derivedTypeDeclaration
		{ t }
;

typedefDeclaration:
	| al = annotationList TYPEDEF t = typedefType n = name SEMICOLON
    { [ NT al; Term "TYPEDEF"; NT t; NT n; Term ";" ] #@ "typedefDeclaration" }
	| al = annotationList TYPE t = typeRef n = name SEMICOLON
    { [ NT al; Term "TYPE"; NT t; NT n; Term ";" ] #@ "typedefDeclaration" }
;

(* >> Extern declarations *)
externFunctionDeclaration:
	| al = annotationList EXTERN p = functionPrototype pop_scope SEMICOLON
		{ let decl =
        [ NT al; Term "EXTERN"; NT p; Term ";" ] #@ "externFunctionDeclaration"
      in
      declare_var (id_of_function_prototype p) (has_type_params_function_prototype p);
      decl }
;

methodPrototype:
	| al = annotationList tid = typeIdentifier L_PAREN pl = parameterList R_PAREN SEMICOLON
    { [ NT al; NT tid; Term "("; NT pl; Term ")"; Term ";" ] #@ "methodPrototype" }
	| al = annotationList p = functionPrototype pop_scope SEMICOLON
    { [ NT al; NT p; Term ";" ] #@ "methodPrototype" }
	| al = annotationList ABSTRACT p = functionPrototype
    pop_scope SEMICOLON
    { [ NT al; Term "ABSTRACT"; NT p; Term ";" ] #@ "methodPrototype" }
;

methodPrototypeList:
  | (* empty *) { [ Term "`EMPTY" ] #@ "methodPrototypeList" }
  | ps = methodPrototypeList p = methodPrototype
    { [ NT ps; NT p ] #@ "methodPrototypeList" }
;

externObjectDeclaration:
  | al = annotationList EXTERN n = push_externName tpl = typeParameterListOpt
    L_BRACE pl = methodPrototypeList R_BRACE pop_scope
    { let decl =
        [ NT al; Term "EXTERN"; NT n; NT tpl; Term "{"; NT pl; Term "}" ]
          #@ "externObjectDeclaration"
      in
      declare_type_of_il n (has_type_params_declaration decl);
      decl }
;

externDeclaration:
  | d = externFunctionDeclaration
  | d = externObjectDeclaration
    { d }
;

(* >> Parser statements and declarations *)
(* >>>> Select expressions *)
selectCase:
  | k = keysetExpression COLON n = name SEMICOLON
    { [ NT k; Term ":"; NT n; Term ";" ] #@ "selectCase" }
;

selectCaseList:
  | (* empty *) { [ Term "`EMPTY" ] #@ "selectCaseList" }
  | cl = selectCaseList c = selectCase
    { [ NT cl; NT c ] #@ "selectCaseList" }
;

selectExpression:
  | SELECT L_PAREN el = expressionList R_PAREN L_BRACE cl = selectCaseList R_BRACE
    { [ Term "SELECT"; Term "("; NT el; Term ")"; Term "{"; NT cl; Term "}" ]
      #@ "selectExpression" }
;

(* >>>> Transition statements *)
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

(* >>>> Value set declarations *)
valueSetType:
	| t = baseType
	| t = tupleType
	| t = prefixedTypeName
    { t }
;

valueSetDeclaration:
	| al = annotationList VALUE_SET l_angle t = valueSetType r_angle
    L_PAREN s = expression R_PAREN n = name SEMICOLON
    { [ NT al; Term "VALUE_SET"; Term "<"; NT t; Term ">"; Term "("; NT s; Term ")"; NT n; Term ";" ]
       #@ "valueSetDeclaration" }
;

(* >>>> Parser type declarations *)
parserTypeDeclaration:
  | al = annotationList PARSER n = push_name tpl = typeParameterListOpt
      L_PAREN pl = parameterList R_PAREN pop_scope SEMICOLON
    { [ NT al; Term "PARSER"; NT n; NT tpl; Term "("; NT pl; Term ")"; Term ";" ]
       #@ "parserTypeDeclaration" }
;

(* >>>> Parser declarations *)
parserBlockStatement:
  | al = annotationList L_BRACE sl = parserStatementList R_BRACE
    { [ NT al; Term "{"; NT sl; Term "}" ] #@ "parserBlockStatement" }
;

parserStatement:
  | s = constantDeclaration
  | s = variableDeclaration
  | s = emptyStatement
  | s = assignmentStatement
  | s = callStatement
  | s = directApplicationStatement
  | s = parserBlockStatement
  | s = conditionalStatement
    { s }
;

parserStatementList:
  | (* empty *) { [ Term "`EMPTY" ] #@ "parserStatementList" }
  | sl = parserStatementList s = parserStatement
    { [ NT sl; NT s ] #@ "parserStatementList" }
;

parserState:
  | al = annotationList STATE n = push_name L_BRACE sl = parserStatementList t = transitionStatement R_BRACE
    { [ NT al; Term "STATE"; NT n; Term "{"; NT sl; NT t; Term "}" ]
      #@ "parserState" }
;

parserStateList:
  | s = parserState { s }
  | sl = parserStateList s = parserState
    { [ NT sl; NT s ] #@ "parserStateList" }
;

parserLocalDeclaration:
  | d = constantDeclaration
  | d = instantiation
  | d = variableDeclaration
  | d = valueSetDeclaration
    { d }
;

parserLocalDeclarationList:
  | (* empty *) { [ Term "`EMPTY" ] #@ "parserLocalDeclarationList" }
  | dl = parserLocalDeclarationList d = parserLocalDeclaration
    { [ NT dl; NT d ] #@ "parserLocalDeclarationList" }
;

parserDeclaration:
  | al = annotationList PARSER n = push_name tpl = typeParameterListOpt
    L_PAREN pl = parameterList R_PAREN cpl = constructorParameterListOpt
    L_BRACE dl = parserLocalDeclarationList sl = parserStateList R_BRACE pop_scope
		{ [ NT al; Term "PARSER"; NT n; NT tpl; Term "("; NT pl; Term ")"; NT cpl;
      Term "{"; NT dl; NT sl; Term "}" ] #@ "parserDeclaration" }
;

(* >> Control statements and declarations *)
(* >>>> Table declarations *)
constOpt:
  | (* empty *) { [ Term "`EMPTY" ] #@ "constOpt" }
  | CONST { [ Term "CONST" ] #@ "constOpt" }
;

(* >>>>>> Table key property *)
tableKey:
  | e = expression COLON n = name al = annotationList SEMICOLON
    { [ NT e; Term ":"; NT n; NT al; Term ";" ] #@ "tableKey" }
;

tableKeyList:
  | (* empty *) { [ Term "`EMPTY" ] #@ "tableKeyList" }
  | kl = tableKeyList k = tableKey
    { [ NT kl; NT k ] #@ "tableKeyList" }
;

(* >>>>>> Table actions property *)
tableActionReference:
  | n = prefixedNonTypeName
    { n }
  | n = prefixedNonTypeName L_PAREN al = argumentList R_PAREN
    { [ NT n; Term "("; NT al; Term ")" ] #@ "tableActionReference" }
;

tableAction:
  | al = annotationList ac = tableActionReference SEMICOLON
    { [ NT al; NT ac; Term ";" ] #@ "tableAction" }
;

tableActionList:
  | (* empty *) { [ Term "`EMPTY" ] #@ "tableActionList" }
  | acl = tableActionList ac = tableAction
    { [ NT acl; NT ac ] #@ "tableActionList" }
;

(* >>>>>> Table entry property *)
tableEntryPriority:
  | PRIORITY ASSIGN num = number COLON
    { [ Term "PRIORITY"; Term "="; NT num; Term ":" ] #@ "tableEntryPriority" }
  | PRIORITY ASSIGN L_PAREN e = expression R_PAREN COLON
    { [ Term "PRIORITY"; Term "="; Term "("; NT e; Term ")"; Term ":" ] #@ "tableEntryPriority" }
;

tableEntry:
  | c = constOpt p = tableEntryPriority k = keysetExpression COLON ac = tableActionReference al = annotationList SEMICOLON
    { [ NT c; NT p; NT k; Term ":"; NT ac; NT al; Term ";" ] #@ "tableEntry" }
  | c = constOpt k = keysetExpression COLON ac = tableActionReference al = annotationList SEMICOLON
    { [ NT c; NT k; Term ":"; NT ac; NT al; Term ";" ] #@ "tableEntry" }
;

tableEntryList:
  | (* empty *) { [ Term "`EMPTY" ] #@ "tableEntryList" }
  | el = tableEntryList e = tableEntry
    { [ NT el; NT e ] #@ "tableEntryList" }
;

(* >>>>>> Table properties *)
tableProperty:
  | KEY ASSIGN L_BRACE kl = tableKeyList R_BRACE
    { [ Term "KEY"; Term "="; Term "{"; NT kl; Term "}" ] #@ "tableProperty" }
  | ACTIONS ASSIGN L_BRACE acl = tableActionList R_BRACE
    { [ Term "ACTIONS"; Term "="; Term "{"; NT acl; Term "}" ] #@ "tableProperty" }
  | al = annotationList c = constOpt ENTRIES ASSIGN L_BRACE el = tableEntryList R_BRACE
    { [ NT al; NT c; Term "ENTRIES"; Term "="; Term "{"; NT el; Term "}" ] #@ "tableProperty" }
  | al = annotationList c = constOpt n = tableCustomName i = initialValue SEMICOLON
    { [ NT al; NT c; NT n; NT i; Term ";" ] #@ "tableProperty" }
;

tablePropertyList:
  | (* empty *) { [ Term "`EMPTY" ] #@ "tablePropertyList" }
  | pl = tablePropertyList p = tableProperty
    { [ NT pl; NT p ] #@ "tablePropertyList" }
;

tableDeclaration:
  | al = annotationList TABLE n = name L_BRACE pl = tablePropertyList R_BRACE
    { [ NT al; Term "TABLE"; NT n; Term "{"; NT pl; Term "}" ] #@ "tableDeclaration" }

(* >>>> Control type declarations *)
controlTypeDeclaration:
  | al = annotationList CONTROL n = push_name tpl = typeParameterListOpt
    L_PAREN pl = parameterList R_PAREN pop_scope SEMICOLON
    { [ NT al; Term "CONTROL"; NT n; NT tpl; Term "("; NT pl; Term ")"; Term ";" ]
       #@ "controlTypeDeclaration" }
;

(* >>>> Control declarations *)
controlBody:
  | b = blockStatement { b }
;

controlLocalDeclaration:
  | d = constantDeclaration 
  | d = instantiation 
  | d = variableDeclaration
    { d }
  | d = actionDeclaration
  | d = tableDeclaration
    { declare_var (id_of_declaration d) false;
      d }
;

controlLocalDeclarationList:
  | (* empty *) { [ Term "`EMPTY" ] #@ "controlLocalDeclarationList" }
  | dl = controlLocalDeclarationList d = controlLocalDeclaration
    { [ NT dl; NT d ] #@ "controlLocalDeclarationList" }
;

controlDeclaration:
  | al = annotationList CONTROL n = push_name tpl = typeParameterListOpt
    L_PAREN pl = parameterList R_PAREN cpl = constructorParameterListOpt
    L_BRACE dl = controlLocalDeclarationList APPLY b = controlBody R_BRACE pop_scope
    { [ NT al; Term "CONTROL"; NT n; NT tpl; Term "("; NT pl; Term ")"; NT cpl;
      Term "{"; NT dl; Term "APPLY"; NT b; Term "}" ] #@ "controlDeclaration" }
;

(* >> Package type declarations *)
packageTypeDeclaration:
  | al = annotationList PACKAGE n = push_name tpl = typeParameterListOpt
    L_PAREN pl = parameterList R_PAREN pop_scope SEMICOLON
    { [ NT al; Term "PACKAGE"; NT n; NT tpl; Term "("; NT pl; Term ")"; Term ";" ]
       #@ "packageTypeDeclaration" }
;

(* >> Type declarations *)
typeDeclaration:
  | d = derivedTypeDeclaration
  | d = typedefDeclaration
  | d = parserTypeDeclaration
  | d = controlTypeDeclaration
  | d = packageTypeDeclaration
    { d }
;

(* >> Declarations *)
declaration:
  | const = constantDeclaration
    { declare_var (id_of_declaration const) (has_type_params_declaration const);
      const }
  | inst = instantiation
    { declare_var (id_of_declaration inst) false;
      inst }
  | func = functionDeclaration
    { declare_var (id_of_declaration func) (has_type_params_declaration func);
      func }
  | action = actionDeclaration
    { declare_var (id_of_declaration action) false;
      action }
  | d = errorDeclaration
  | d = matchKindDeclaration
  | d = externDeclaration
    { d }
  | d = parserDeclaration
  | d = controlDeclaration
  | d = typeDeclaration
    { declare_type (id_of_declaration d) (has_type_params_declaration d);
      d }
;

(* Annotations *)
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
	| VALUE_SET
    { [ Term "VALUE_SET" ] #@ "annotationToken" }
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
	| e = dataElementExpression c = trailingCommaOpt
    { [ NT e; NT c ] #@ "structuredAnnotationBody" }
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

annotationListNonEmpty:
	| a = annotation { a }
	| al = annotationListNonEmpty a = annotation
		{ [ NT al; NT a ] #@ "annotationListNonEmpty" }
;

%inline annotationList:
	| (* empty *) { [ Term "`EMPTY" ] #@ "annotationList" }
	| al = annotationListNonEmpty { al }
;

(******** P4 program ********)
declarationList:
  | (* empty *) { [ Term "`EMPTY" ] #@ "p4program" }
  | ds = declarationList d = declaration
    { [ NT ds; NT d ] #@ "p4program" }
  | ds = declarationList SEMICOLON
    { [ NT ds; Term ";" ] #@ "p4program" }
;

p4program:
	| ds = declarationList END { ds }
