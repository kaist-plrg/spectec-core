%{
  open Lang.Il
  open Lang.Il.Value
  open Context
  open Extract

  let declare_var_of_il (v: value) (b: bool) : unit =
    let id = id_of_name v in
    declare_var id b

  let rec declare_vars_of_il (v: value) : unit =
    match flatten_case_v v with
    | "nameList", [","], [ v_nameList; v_name ] ->
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
    | "typeParameterList", [","], [ v_tpList; v_name ] ->
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
%token<Lang.Il.value> NAME STRING_LITERAL
%token<Lang.Il.value * string> NUMBER_INT NUMBER
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
%token<Lang.Il.value> UNEXPECTED_TOKEN

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
%type <Lang.Il.value>
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
%type <Lang.Il.value> push_name push_externName
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
    { [ atom "_EMPTY" ] |> case_v ~var:"trailingCommaOpt" }
	| COMMA
    { [ atom "," ] |> case_v ~var:"trailingCommaOpt" }
;

(* Numbers *)
number:
	| int = int
    { [ atom "D"; arg int ] |> case_v ~var:"number" }
(* Processed by lexer *)
	| number = NUMBER
    { fst number }
;

(* Strings *)
stringLiteral:
	| text = STRING_LITERAL
    { [ atom (Char.escaped '"'); arg text; atom (Char.escaped '"') ] |> case_v ~var:"stringLiteral"}
;

(* Names *)
identifier:
	| text = NAME IDENTIFIER
    { [ atom "_ID"; arg text ] |> case_v ~var:"identifier" }
;

typeIdentifier:
	| text = NAME TYPENAME
    { [ atom "_TID"; arg text ] |> case_v ~var:"typeIdentifier" }
;

(* >> Non-type names *)
nonTypeName:
	| id = identifier { id }
	| APPLY { [ atom "APPLY" ] |> case_v ~var:"nonTypeName" }
	| KEY { [ atom "KEY" ] |> case_v ~var:"nonTypeName" }
	| ACTIONS { [ atom "ACTIONS" ] |> case_v ~var:"nonTypeName" }
	| STATE { [ atom "STATE" ] |> case_v ~var:"nonTypeName" }
	| ENTRIES { [ atom "ENTRIES" ] |> case_v ~var:"nonTypeName" }
	| TYPE { [ atom "TYPE" ] |> case_v ~var:"nonTypeName" }
	| PRIORITY { [ atom "PRIORITY" ] |> case_v ~var:"nonTypeName" }
;

prefixedNonTypeName:
	| n = nonTypeName { n }
	| DOT go_toplevel n = nonTypeName go_local
    { [ atom "_ID"; atom "`."; arg n ] |> case_v ~var:"prefixedNonTypeName" }
;

(* >> Type names *)
typeName:
	| n = typeIdentifier { n }
;

prefixedTypeName:
	| n = typeName { n }
	| DOT go_toplevel tid = typeName go_local
		{ [ atom "_TID"; atom "`."; arg tid ] |> case_v ~var:"prefixedType" }
;

(* >> Table custom property names *)
tableCustomName:
	| id = identifier { id }
	| tid = typeIdentifier { tid }
	| APPLY { [ atom "APPLY" ] |> case_v ~var:"tableCustomName" }
	| STATE { [ atom "STATE" ] |> case_v ~var:"tableCustomName" }
	| TYPE { [ atom "TYPE" ] |> case_v ~var:"tableCustomName" }
	| PRIORITY { [ atom "PRIORITY" ] |> case_v ~var:"tableCustomName" }
;

(* >> Names *)
name:
	| n = nonTypeName
	| n = typeName
    { n }
	| LIST { [ atom "LIST" ] |> case_v ~var:"name" }
;

nameList:
	| n = name { n }
	| ns = nameList COMMA n = name
    { [ arg ns; atom ","; arg n ]
      |> case_v ~var:"nameList" }
;

member:
	| name = name
    { name }
;

(* Directions *)
direction:
	| (* empty *) { [ atom "_EMPTY" ] |> case_v ~var:"direction" }
	| IN { [ atom "IN" ] |> case_v ~var:"direction" }
	| OUT { [ atom "OUT" ] |> case_v ~var:"direction" }
	| INOUT { [ atom "INOUT" ] |> case_v ~var:"direction" }
;

(* Types *)
(* >> Base types *)
baseType:
	| BOOL { [ atom "BOOL" ] |> case_v ~var:"baseType" }
	| MATCH_KIND { [ atom "MATCH_KIND" ] |> case_v ~var:"baseType" }
	| ERROR { [ atom "ERROR" ] |> case_v ~var:"baseType" }
	| BIT { [ atom "BIT" ] |> case_v ~var:"baseType" }
	| STRING { [ atom "STRING" ] |> case_v ~var:"baseType"}
	| INT
    { [ atom "INT" ] |> case_v ~var:"baseType" }
	| BIT l_angle v = int r_angle
    { [ atom "BIT"; atom "<"; arg v; atom ">" ]
      |> case_v ~var:"baseType" }
	| INT l_angle v = int r_angle
    { [ atom "INT"; atom "<"; arg v; atom ">" ]
      |> case_v ~var:"baseType" }
	| VARBIT l_angle v = int r_angle
    { [ atom "VARBIT"; atom "<"; arg v; atom ">" ] |> case_v ~var:"baseType" }
	| BIT l_angle L_PAREN e = expression R_PAREN r_angle
    { [ atom "BIT"; atom "<"; atom "("; arg e; atom ")"; atom ">" ] |> case_v ~var:"baseType" }
	| INT l_angle L_PAREN e = expression R_PAREN r_angle
    { [ atom "INT"; atom "<"; atom "("; arg e; atom ")"; atom ">" ]
      |> case_v ~var:"baseType" }
	| VARBIT l_angle L_PAREN e = expression R_PAREN r_angle
    { [ atom "VARBIT"; atom "<"; atom "("; arg e; atom ")"; atom ">" ] |> case_v ~var:"baseType" }
;

(* >> Named types *)
specializedType:
  | n = prefixedTypeName l_angle tal = typeArgumentList r_angle
    { [ arg n; atom "<"; arg tal; atom ">" ] |> case_v ~var:"specializedType" }
;

namedType:
  | t = prefixedTypeName
  | t = specializedType
    { t }
;

(* >> Header stack types *)
headerStackType:
  | t = namedType L_BRACKET e = expression R_BRACKET
    { [ arg t; atom "["; arg e; atom "]" ] |> case_v ~var:"headerStackType" }
;

(* >> List types *)
listType:
  | LIST l_angle targ = typeArgument r_angle
    { [ atom "LIST"; atom "<"; arg targ; atom ">" ] |> case_v ~var:"listType" }
;

(* >> Tuple types *)
tupleType:
	| TUPLE l_angle targs = typeArgumentList r_angle
    { [ atom "TUPLE"; atom "<"; arg targs; atom ">" ] |> case_v ~var:"tupleType" }
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
	| VOID { [ atom "VOID" ] |> case_v ~var:"typeOrVoid" }
  (* From Petr4: HACK for generic return type *)
	| id = identifier
    { match flatten_case_v id with
      | "identifier", ["_ID"], [ value_text ]  ->
        [ atom "_TID"; arg value_text ] |> case_v ~var:"typeIdentifier"
      | _ -> failwith "@typeOrVoid: expected identifier" }
;

(* Type parameters *)
typeParameter:
	| n = name { n }

typeParameterList:
	| tp = typeParameter { tp }
	| tps = typeParameterList COMMA tp = typeParameter
    { [ arg tps; atom ","; arg tp ] |> case_v ~var:"typeParameterList" }
;

typeParameterListOpt:
	| (* empty *) { [ atom "_EMPTY" ] |> case_v ~var:"typeParameterListOpt" }
	| l_angle tps = typeParameterList r_angle
    { declare_types_of_il tps;
      [ atom "<"; arg tps; atom ">" ] |> case_v ~var:"typeParameterListOpt" }
;

(* Parameters *)
parameter:
	| al = annotationList dir = direction t = typeRef n = name i = initializerOpt
		{ declare_var_of_il n false;
      [ arg al; arg dir; arg t; arg n; arg i ] |> case_v ~var:"parameter" }
;

nonEmptyParameterList:
	| p = parameter { p }
	| ps = nonEmptyParameterList COMMA p = parameter
    { [ arg ps; atom ","; arg p ] |> case_v ~var:"nonEmptyParameterList" }
;

parameterList:
	| (* empty *) { [ atom "_EMPTY" ] |> case_v ~var:"parameterList" }
	| ps = nonEmptyParameterList { ps }
;

(* Constructor parameters *)
constructorParameterListOpt:
	| (* empty *) { [ atom "_EMPTY" ] |> case_v ~var:"constructorParameterListOpt" }
	| L_PAREN ps = parameterList R_PAREN
    { [ atom "("; arg ps; atom ")" ] |> case_v ~var:"constructorParameterListOpt" }
;

(* Expression key-value pairs *)
namedExpression:
	| n = name ASSIGN e = expression { [ arg n; atom "="; arg e ] |> case_v ~var:"namedExpression" }
;

namedExpressionList:
	| e = namedExpression { e }
	| es = namedExpressionList COMMA e = namedExpression { [ arg es; atom ","; arg e ] |> case_v ~var:"namedExpressionList" }
;

(* Expressions *)
(* >> Literal expressions *)
%inline literalExpression:
	| TRUE { [ atom "TRUE" ] |> case_v ~var:"literalExpression" }
	| FALSE { [ atom "FALSE" ] |> case_v ~var:"literalExpression" }
	| num = number { num }
	| str = stringLiteral { str }
;

(* >> Reference expressions *)
%inline referenceExpression:
	| n = prefixedNonTypeName { n }
	| THIS { [ atom "THIS" ] |> case_v ~var:"referenceExpression" }
;

(* >> Default expressions *)
%inline defaultExpression:
	| DOTS { [ atom "`..." ] |> case_v ~var:"defaultExpression" }
;

(* >> Unary, binary, and ternary expressions *)
%inline unop: 
	| NOT { [ atom "!" ] |> case_v ~var:"unop" }
	| COMPLEMENT { [ atom "~" ] |> case_v ~var:"unop" }
	| MINUS { [ atom "-" ] |> case_v ~var:"unop" }
	| PLUS { [ atom "+" ] |> case_v ~var:"unop" }
;

%inline unaryExpression:
	| o = unop e = expression %prec PREFIX
		{ [ arg o; arg e ] |> case_v ~var:"unaryExpression" }
;

%inline binop:
  | MUL { [ atom "*" ] |> case_v ~var:"binop" }
  | DIV { [ atom "/" ] |> case_v ~var:"binop" }
  | MOD { [ atom "%" ] |> case_v ~var:"binop" }
  | PLUS { [ atom "+" ] |> case_v ~var:"binop" }
  | PLUS_SAT { [ atom "|+|" ] |> case_v ~var:"binop" }
  | MINUS { [ atom "-" ] |> case_v ~var:"binop" }
  | MINUS_SAT { [ atom "|-|" ] |> case_v ~var:"binop" }
  | SHL { [ atom "<<" ] |> case_v ~var:"binop" }
  | r_angle R_ANGLE_SHIFT { [ atom ">>" ] |> case_v ~var:"binop" }
  | LE { [ atom "<=" ] |> case_v ~var:"binop" }
  | GE { [ atom ">=" ] |> case_v ~var:"binop" }
  | l_angle { [ atom "``<" ] |> case_v ~var:"binop" }
  | r_angle { [ atom "``>" ] |> case_v ~var:"binop" }
  | NE { [ atom "!=" ] |> case_v ~var:"binop" }
  | EQ { [ atom "==" ] |> case_v ~var:"binop" }
  | BIT_AND { [ atom "&" ] |> case_v ~var:"binop" }
  | BIT_XOR { [ atom "^" ] |> case_v ~var:"binop" }
  | BIT_OR { [ atom "|" ] |> case_v ~var:"binop" }
  | PLUSPLUS { [ atom "++" ] |> case_v ~var:"binop" }
  | AND { [ atom "&&" ] |> case_v ~var:"binop" }
  | OR { [ atom "||" ] |> case_v ~var:"binop" }
;

%inline binaryExpression:
	| l = expression o = binop r = expression
		{ [ arg l; arg o; arg r ] |> case_v ~var:"binaryExpression" }
;

%inline binaryExpressionNonBrace:
	| l = expressionNonBrace o = binop r = expression
		{ [ arg l; arg o; arg r ] |> case_v ~var:"binaryExpressionNonBrace" }
;

%inline ternaryExpression:
	| c = expression QUESTION t = expression COLON f = expression
		{ [ arg c; atom "?"; arg t; atom "`:"; arg f ] |> case_v ~var:"ternaryExpression" }
;

%inline ternaryExpressionNonBrace:
	| c = expressionNonBrace QUESTION t = expression COLON f = expression
		{ [ arg c; atom "?"; arg t; atom "`:"; arg f ] |> case_v ~var:"ternaryExpressionNonBrace" }
;

(* >> Cast expressions *)
%inline castExpression:
	| L_PAREN t = typeRef R_PAREN e = expression %prec PREFIX
    { [ atom "("; arg t; atom ")"; arg e ] |> case_v ~var:"castExpression" }
;

(* >> Data (aggregate) expressions *)
%inline dataExpression:
	| INVALID { [ atom "{#}" ] |> case_v ~var:"dataExpression" }
	| L_BRACE e = dataElementExpression c = trailingCommaOpt R_BRACE
    { [ atom "{"; arg e; arg c; atom "}" ] |> case_v ~var:"dataExpression" }
;

(* >> Member and index access expressions *)
%inline errorAccessExpression:
	| ERROR DOT m = member
		{ [ atom "ERROR"; atom "`."; arg m ] |> case_v ~var:"errorAccessExpression" }
;

%inline memberAccessExpression:
	| e = memberAccessBase DOT m = member %prec DOT
		{ [ arg e; atom "`."; arg m ] |> case_v ~var:"memberAccessExpression" }
;

%inline indexAccessExpression:
	| a = expression L_BRACKET i = expression R_BRACKET
		{ [ arg a; atom "["; arg i; atom "]" ] |> case_v ~var:"indexAccessExpression" }
	| a = expression L_BRACKET h = expression COLON l = expression R_BRACKET
		{ [ arg a; atom "["; arg h; atom "`:"; arg l; atom "]" ] |> case_v ~var:"indexAccessExpression" }
;

%inline accessExpression:
	| e = errorAccessExpression
	| e = memberAccessExpression
	| e = indexAccessExpression
		{ e }
;

%inline memberAccessExpressionNonBrace:
	| e = memberAccessBaseNonBrace DOT m = member %prec DOT
		{ [ arg e; atom "`."; arg m ] |> case_v ~var:"memberAccessExpressionNonBrace" }
;

%inline indexAccessExpressionNonBrace:
	| a = expressionNonBrace L_BRACKET i = expression R_BRACKET
		{ [ arg a; atom "["; arg i; atom "]" ] |> case_v ~var:"indexAccessExpressionNonBrace" }
	| a = expressionNonBrace L_BRACKET h = expression COLON l = expression R_BRACKET
		{ [ arg a; atom "["; arg h; atom "`:"; arg l; atom "]" ] |> case_v ~var:"indexAccessExpressionNonBrace" }
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
		{ [ arg t; atom "("; arg args; atom ")" ] |> case_v ~var:"callExpression" }
	| t = routineTarget l_angle targs = realTypeArgumentList r_angle L_PAREN args = argumentList R_PAREN
		{ [ arg t; atom "<"; arg targs; atom ">"; atom "("; arg args; atom ")" ]
      |> case_v ~var:"callExpression" }
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
		{ [ arg t; atom "("; arg args; atom ")" ] |> case_v ~var:"callExpressionNonBrace" }
	| t = routineTargetNonBrace l_angle targs = realTypeArgumentList r_angle L_PAREN args = argumentList R_PAREN
		{ [ arg t; atom "<"; arg targs; atom ">"; atom "("; arg args; atom ")" ]
      |> case_v ~var:"callExpressionNonBrace" }

(* >> Parenthesized Expressions *)

%inline parenthesizedExpression:
	| L_PAREN e = expression R_PAREN
		{ [ atom "("; arg e; atom ")" ] |> case_v ~var:"parenthesizedExpression" }
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
	| (* empty *) { [ atom "_EMPTY" ] |> case_v ~var:"expressionList" }
	| e = expression { e }
	| el = expressionList COMMA e = expression
		{ [ arg el; atom ","; arg e ] |> case_v ~var:"expressionList" }
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
    { [ arg n; atom "="; arg e ]
      |> case_v ~var:"recordElementExpression" }
  | n = name ASSIGN e = expression COMMA DOTS
    { [ arg n; atom "="; arg e; atom ","; atom "`..." ]
      |> case_v ~var:"recordElementExpression" }
	| n = name ASSIGN e = expression COMMA el = namedExpressionList
    { [ arg n; atom "="; arg e; atom ","; arg el ]
      |> case_v ~var:"recordElementExpression" }
  | n = name ASSIGN e = expression COMMA el = namedExpressionList COMMA DOTS
    { [ arg n; atom "="; arg e; atom ","; arg el; atom ","; atom "`..." ]
      |> case_v ~var:"recordElementExpression" }
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
    { [ arg b; atom "&&&"; arg m ] |> case_v ~var:"simpleKeysetExpression" }
	| l = expression RANGE h = expression
    { [ arg l; atom "`.."; arg h ] |> case_v ~var:"simpleKeysetExpression" }
	| DEFAULT
    { [ atom "DEFAULT" ] |> case_v ~var:"simpleKeysetExpression" }
	| DONTCARE
    { [ atom "_" ] |> case_v ~var:"simpleKeysetExpression" }
;

simpleKeysetExpressionList:
	| e = simpleKeysetExpression { e }
	| el = simpleKeysetExpressionList COMMA e = simpleKeysetExpression
    { [ arg el; atom ","; arg e ] |> case_v ~var:"simpleKeysetExpressionList" }
;

tupleKeysetExpression:
	| L_PAREN b = expression MASK m = expression R_PAREN
		{ [ atom "("; arg b; atom "&&&"; arg m; atom ")" ] |> case_v ~var:"tupleKeysetExpression" }
	| L_PAREN l = expression RANGE h = expression R_PAREN
		{ [ atom "("; arg l; atom "`.."; arg h; atom ")" ] |> case_v ~var:"tupleKeysetExpression" }
	| L_PAREN DEFAULT R_PAREN
		{ [ atom "("; atom "DEFAULT"; atom ")" ] |> case_v ~var:"tupleKeysetExpression" }
	| L_PAREN DONTCARE R_PAREN
		{ [ atom "("; atom "_"; atom ")" ] |> case_v ~var:"tupleKeysetExpression" }
	| L_PAREN e = simpleKeysetExpression COMMA es = simpleKeysetExpressionList R_PAREN
		{ [ atom "("; arg e; atom ","; arg es; atom ")" ] |> case_v ~var:"tupleKeysetExpression" }
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
    { [ atom "VOID" ] |> case_v ~var:"realTypeArgument" }
	| DONTCARE
    { [ atom "_" ] |> case_v ~var:"realTypeArgument" }
;

realTypeArgumentList:
	| targ = realTypeArgument { targ }
	| targs = realTypeArgumentList COMMA targ = realTypeArgument
    { [ arg targs; atom ","; arg targ ] |> case_v ~var:"realTypeArgumentList" }
;

typeArgument:
	| t = typeRef
	| t = nonTypeName 
		{ t }
	| VOID
    { [ atom "VOID" ] |> case_v ~var:"typeArgument" }
	| DONTCARE
    { [ atom "_" ] |> case_v ~var:"typeArgument" }
;

typeArgumentList:
	| (* empty *) { [ atom "_EMPTY" ] |> case_v ~var:"typeArgumentList" }
	| targ = typeArgument { targ }
	| targs = typeArgumentList COMMA targ = typeArgument
    { [ arg targs; atom ","; arg targ ] |> case_v ~var:"typeArgumentList" }
;

(* Arguments *)
argument:
	| e = expression { e }
	| n = name ASSIGN e = expression 
		{ [ arg n; atom "="; arg e ] |> case_v ~var:"argument" }
	| name = name ASSIGN DONTCARE
		{ [ arg name; atom "="; atom "_" ] |> case_v ~var:"argument" }
	| DONTCARE
		{ [ atom "_" ] |> case_v ~var:"argument" }
;

argumentListNonEmpty:
	| a = argument { a }
	| args = argumentListNonEmpty COMMA a = argument
    { [ arg args; atom ","; arg a ] |> case_v ~var:"argumentListNonEmpty" }
;

argumentList:
	| (* empty *) { [ atom "_EMPTY" ] |> case_v ~var:"argumentList" }
	| args = argumentListNonEmpty { args }
;

(* L-values *)
lvalue:
	| e = referenceExpression { e }
	| lv = lvalue DOT m = member %prec DOT
		{ [ arg lv; atom "`."; arg m ] |> case_v ~var:"lvalue" }
	| lv = lvalue L_BRACKET i = expression R_BRACKET
		{ [ arg lv; atom "["; arg i; atom "]" ] |> case_v ~var:"lvalue" }
	| lv = lvalue L_BRACKET h = expression COLON l = expression R_BRACKET
		{ [ arg lv; atom "["; arg h; atom "`:"; arg l; atom "]" ] |> case_v ~var:"lvalue" }
	| L_PAREN lv = lvalue R_PAREN
		{ [ atom "("; arg lv; atom ")" ] |> case_v ~var:"lvalue" }
;

(* Statements *)
(* >> Empty statements *)
emptyStatement:
	| SEMICOLON { [ atom "`;" ] |> case_v ~var:"emptyStatement" }
;

(* >> Assignment statements *)
assignop:
	| ASSIGN { [ atom "=" ] |> case_v ~var:"assignop" }
	| PLUS_ASSIGN { [ atom "+=" ] |> case_v ~var:"assignop" }
	| PLUS_SAT_ASSIGN { [ atom "|+|=" ] |> case_v ~var:"assignop" }
	| MINUS_ASSIGN { [ atom "-=" ] |> case_v ~var:"assignop" }
	| MINUS_SAT_ASSIGN { [ atom "|-|=" ] |> case_v ~var:"assignop" }
	| MUL_ASSIGN { [ atom "*=" ] |> case_v ~var:"assignop" }
	| DIV_ASSIGN { [ atom "/=" ] |> case_v ~var:"assignop" }
	| MOD_ASSIGN { [ atom "%=" ] |> case_v ~var:"assignop" }
	| SHL_ASSIGN { [ atom "<<=" ] |> case_v ~var:"assignop" }
	| SHR_ASSIGN { [ atom ">>=" ] |> case_v ~var:"assignop" }
	| BIT_AND_ASSIGN { [ atom "&=" ] |> case_v ~var:"assignop" }
	| BIT_XOR_ASSIGN { [ atom "^=" ] |> case_v ~var:"assignop" }
	| BIT_OR_ASSIGN { [ atom "|=" ] |> case_v ~var:"assignop" }
;

assignmentStatement:
	| lv = lvalue o = assignop e = expression SEMICOLON
		{ [ arg lv; arg o; arg e; atom "`;" ] |> case_v ~var:"assignmentStatement" }
;

(* >> Call statements *)
callStatement:
	| lv = lvalue L_PAREN args = argumentList R_PAREN SEMICOLON
		{ [ arg lv; atom "("; arg args; atom ")"; atom "`;" ] |> case_v ~var:"callStatement" }
	| lv = lvalue l_angle targs = typeArgumentList r_angle L_PAREN args = argumentList R_PAREN SEMICOLON
		{ [ arg lv; atom "<"; arg targs; atom ">"; atom "("; arg args; atom ")"; atom "`;" ]
      |> case_v ~var:"callStatement" }
;

(* >> Direct application statements *)
directApplicationStatement:
	| t = namedType DOT APPLY L_PAREN args = argumentList R_PAREN SEMICOLON
    { [ arg t; atom "`."; atom "APPLY"; atom "("; arg args; atom ")"; atom "`;" ]
      |> case_v ~var:"directApplicationStatement" }
;

(* >> Return statements *)
returnStatement:
	| RETURN SEMICOLON
    { [ atom "RETURN"; atom "`;" ] |> case_v ~var:"returnStatement" }
	| RETURN e = expression SEMICOLON
    { [ atom "RETURN"; arg e; atom "`;" ] |> case_v ~var:"returnStatement" }
;

(* >> Exit statements *)
exitStatement:
	| EXIT SEMICOLON
    { [ atom "EXIT"; atom "`;" ] |> case_v ~var:"exitStatement" }
;

(* >> Block statements *)
blockStatement:
	| al = annotationList L_BRACE
  push_scope
  sl = blockElementStatementList R_BRACE
  pop_scope
		{ [ arg al; atom "{"; arg sl; atom "}" ] |> case_v ~var:"blockStatement" }
;

(* >> Conditional statements *)
conditionalStatement:
	| IF L_PAREN c = expression R_PAREN t = statement %prec THEN
    { [ atom "IF"; atom "("; arg c; atom ")"; arg t ]
      |> case_v ~var:"conditionalStatement" }
	| IF L_PAREN c = expression R_PAREN t = statement ELSE f = statement
    { [ atom "IF"; atom "("; arg c; atom ")"; arg t; atom "ELSE"; arg f ]
      |> case_v ~var:"conditionalStatement" }
;

(* >> For statements *)
forInitStatement:
	| al = annotationList t = typeRef n = name i = initializerOpt
		{ [ arg al; arg t; arg n; arg i ] |> case_v ~var:"forInitStatement" }
	| lv = lvalue L_PAREN args = argumentList R_PAREN
		{ [ arg lv; atom "("; arg args; atom ")" ] |> case_v ~var:"forInitStatement" }
	| lv = lvalue l_angle targs = typeArgumentList r_angle L_PAREN args = argumentList R_PAREN
		{ [ arg lv; atom "<"; arg targs; atom ">"; atom "("; arg args; atom ")" ]
      |> case_v ~var:"forInitStatement" }
	| lv = lvalue o = assignop e = expression
		{ [ arg lv; arg o; arg e ] |> case_v ~var:"forInitStatement" }
;

forInitStatementListNonEmpty:
	| s = forInitStatement { s }
	| sl = forInitStatementListNonEmpty COMMA s = forInitStatement
    { [ arg sl; atom ","; arg s ] |> case_v ~var:"forInitStatementListNonEmpty" }
;

forInitStatementList:
	| (* empty *) { [ atom "_EMPTY" ] |> case_v ~var:"forInitStatementList" }
	| sl = forInitStatementListNonEmpty { sl }
;

forUpdateStatement:
	| s = forInitStatement { s }
;

forUpdateStatementListNonEmpty:
	| s = forUpdateStatement { s }
	| sl = forUpdateStatementListNonEmpty COMMA s = forUpdateStatement
    { [ arg sl; atom ","; arg s ] |> case_v ~var:"forUpdateStatementListNonEmpty" }
;

forUpdateStatementList:
	| (* empty *) { [ atom "_EMPTY" ] |> case_v ~var:"forUpdateStatementList" }
	| sl = forUpdateStatementListNonEmpty { sl }
;

forCollectionExpression:
	| e = expression { e }
	| l = expression RANGE h = expression
    { [ arg l; atom "`.."; arg h ] |> case_v ~var:"forCollectionExpr" }
;

forStatement:
  | al = annotationList FOR L_PAREN il = forInitStatementList SEMICOLON c = expression SEMICOLON ul = forUpdateStatementList R_PAREN b = statement
		{ [ arg al; atom "FOR"; atom "("; arg il; atom "`;"; arg c; atom "`;"; arg ul; atom ")"; arg b ]
      |> case_v ~var:"forStatement" }
  | al = annotationList FOR L_PAREN
    t = typeRef n = name IN e = forCollectionExpression R_PAREN b = statement
    { [ arg al; atom "FOR"; atom "("; arg t; arg n; atom "IN"; arg e; atom ")"; arg b ]
      |> case_v ~var:"forStatement" }
  | al = annotationList FOR L_PAREN
    al_in = annotationList t = typeRef n = name IN e = forCollectionExpression R_PAREN b = statement
    { [ arg al; atom "FOR"; atom "("; arg al_in; arg t; arg n; atom "IN"; arg e; atom ")"; arg b ]
      |> case_v ~var:"forStatement" }
;

(* >> Switch statements *)
switchLabel:
  | DEFAULT
    { [ atom "DEFAULT" ] |> case_v ~var:"switchLabel" }
  | e = expressionNonBrace
    { e }
;

switchCase:
  | l = switchLabel COLON s = blockStatement
    { [ arg l; atom "`:"; arg s ] |> case_v ~var:"switchCase" }
  | l = switchLabel COLON
    { [ arg l; atom "`:" ] |> case_v ~var:"switchCase" }
;

switchCaseList:
  | (* empty *)
    { [ atom "_EMPTY" ] |> case_v ~var:"switchCaseList" }
  | cs = switchCaseList c = switchCase
    { [ arg cs; arg c ] |> case_v ~var:"switchCaseList" }
;

switchStatement:
  | SWITCH L_PAREN e = expression R_PAREN L_BRACE cs = switchCaseList R_BRACE
    { [ atom "SWITCH"; atom "("; arg e; atom ")"; atom "{"; arg cs; atom "}" ]
      |> case_v ~var:"switchStatement" }

(* >> Break and continue statements *)
breakStatement:
  | BREAK SEMICOLON
    { [ atom "BREAK"; atom "`;" ] |> case_v ~var:"breakStatement" }
;

continueStatement:
  | CONTINUE SEMICOLON
    { [ atom "CONTINUE"; atom "`;" ] |> case_v ~var:"continueStatement" }
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
		{ [ atom "="; arg e ] |> case_v ~var:"initializer" }
;

constantDeclaration:
  | al = annotationList CONST t = typeRef n = name i = initialValue SEMICOLON
    { [ arg al; atom "CONST"; arg t; arg n; arg i; atom "`;" ] |> case_v ~var:"constantDeclaration" }
;

initializerOpt:
	| (* empty *)
		{ [ atom "_EMPTY" ] |> case_v ~var:"initializerOpt" }
	| i = initialValue { i }
;

variableDeclaration:
  | al = annotationList t = typeRef n = name i = initializerOpt SEMICOLON
    { declare_var_of_il n false;
      [ arg al; arg t; arg n; arg i; atom "`;" ] |> case_v ~var:"variableDeclaration" }
;

blockElementStatement:
  | d = constantDeclaration
  | d = variableDeclaration
  | d = statement
    { d }
;

blockElementStatementList:
  | (* empty *)
    { [ atom "_EMPTY" ] |> case_v ~var:"blockElementStatementList" }
  | sl = blockElementStatementList s = blockElementStatement
    { [ arg sl; arg s ] |> case_v ~var:"blockElementStatementList" }
;

(* >> Function declarations *)
functionPrototype:
	| t = typeOrVoid n = name push_scope
  tpl = typeParameterListOpt
  L_PAREN pl = parameterList R_PAREN
    { [ arg t; arg n; arg tpl; atom "("; arg pl; atom ")" ]
      |> case_v ~var:"functionPrototype" }
;

functionDeclaration:
	| al = annotationList p = functionPrototype b = blockStatement pop_scope
    { [ arg al; arg p; arg b ] |> case_v ~var:"functionDeclaration" }
;

(* >> Action declarations *)
actionDeclaration: 
  | al = annotationList ACTION n = name L_PAREN pl = parameterList R_PAREN s = blockStatement
    { [ arg al; atom "ACTION"; arg n; atom "("; arg pl; atom ")"; arg s ]
      |> case_v ~var:"actionDeclaration" }
;

(* >> Instantiations *)
objectInitializer:
	| ASSIGN L_BRACE ds = objectDeclarationList R_BRACE
    { [ atom "="; atom "{"; arg ds; atom "}" ] |> case_v ~var:"objectInitializer" }
;

instantiation:
	| al = annotationList t = typeRef L_PAREN args = argumentList R_PAREN n = name SEMICOLON
    { [ arg al; arg t; atom "("; arg args; atom ")"; arg n; atom "`;" ]
      |> case_v ~var:"instantiation" }
	| al = annotationList t = typeRef L_PAREN args = argumentList R_PAREN n = name i = objectInitializer SEMICOLON
    { [ arg al; arg t; atom "("; arg args; atom ")"; arg n; arg i; atom "`;" ]
      |> case_v ~var:"instantiation" }
;

objectDeclaration:
	| d = functionDeclaration
	| d = instantiation
    { d }
;

objectDeclarationList:
	| (* empty *) { [ atom "_EMPTY" ] |> case_v ~var:"objectDeclarationList" }
	| ds = objectDeclarationList d = objectDeclaration
    { [ arg ds; arg d ] |> case_v ~var:"objectDeclarationList" }
;

(* >> Error declarations *)
errorDeclaration:
	| ERROR L_BRACE nl = nameList R_BRACE
    { declare_vars_of_il nl;
      [ atom "ERROR"; atom "{"; arg nl; atom "}" ] |> case_v ~var:"errorDeclaration" }
;

(* >> Match kind declarations *)
matchKindDeclaration:
	| MATCH_KIND L_BRACE nl = nameList c = trailingCommaOpt R_BRACE
    { declare_vars_of_il nl;
      [ atom "MATCH_KIND"; atom "{"; arg nl; arg c; atom "}" ] |> case_v ~var:"matchKindDeclaration" }
;

(* >> Derived type declarations *)
(* >>>> Enum type declarations *)
enumTypeDeclaration:
  | al = annotationList ENUM n = name L_BRACE
    nl = nameList c = trailingCommaOpt R_BRACE
    { [ arg al; atom "ENUM"; arg n; atom "{"; arg nl; arg c; atom "}" ]
      |> case_v ~var:"enumTypeDeclaration" }
  | al = annotationList ENUM t = typeRef n = name L_BRACE
    el = namedExpressionList c = trailingCommaOpt R_BRACE
    { [ arg al; atom "ENUM"; arg t; arg n; atom "{"; arg el; arg c; atom "}" ]
      |> case_v ~var:"enumTypeDeclaration" }
;

(* >>>>>> Struct, header, and union type declarations *)
typeField:
  | al = annotationList t = typeRef n = name SEMICOLON
    { [ arg al; arg t; arg n; atom "`;" ] |> case_v ~var:"typeField" }
;

typeFieldList:
  | (* empty *) { [ atom "_EMPTY" ] |> case_v ~var:"typeFieldList" }
  | fl = typeFieldList f = typeField
    { [ arg fl; arg f ] |> case_v ~var:"typeFieldList" }
;

structTypeDeclaration:
  | al = annotationList STRUCT n = name tpl = typeParameterListOpt
      L_BRACE fl = typeFieldList R_BRACE
    { [ arg al; atom "STRUCT"; arg n; arg tpl; atom "{"; arg fl; atom "}" ]
      |> case_v ~var:"structTypeDeclaration" }
;

headerTypeDeclaration:
  | al = annotationList HEADER n = name tpl = typeParameterListOpt
      L_BRACE fl = typeFieldList R_BRACE
    { [ arg al; atom "HEADER"; arg n; arg tpl; atom "{"; arg fl; atom "}" ]
      |> case_v ~var:"headerTypeDeclaration" }
;

headerUnionTypeDeclaration:
  | al = annotationList HEADER_UNION n = name tpl = typeParameterListOpt
      L_BRACE fl = typeFieldList R_BRACE
    { [ arg al; atom "HEADER_UNION"; arg n; arg tpl; atom "{"; arg fl; atom "}" ]
      |> case_v ~var:"headerUnionTypeDeclaration" }
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
    { [ arg al; atom "TYPEDEF"; arg t; arg n; atom "`;" ] |> case_v ~var:"typedefDeclaration" }
	| al = annotationList TYPE t = typeRef n = name SEMICOLON
    { [ arg al; atom "TYPE"; arg t; arg n; atom "`;" ] |> case_v ~var:"typedefDeclaration" }
;

(* >> Extern declarations *)
externFunctionDeclaration:
	| al = annotationList EXTERN p = functionPrototype pop_scope SEMICOLON
		{ let decl =
        [ arg al; atom "EXTERN"; arg p; atom "`;" ] |> case_v ~var:"externFunctionDeclaration"
      in
      declare_var (id_of_function_prototype p) (has_type_params_function_prototype p);
      decl }
;

methodPrototype:
	| al = annotationList tid = typeIdentifier L_PAREN pl = parameterList R_PAREN SEMICOLON
    { [ arg al; arg tid; atom "("; arg pl; atom ")"; atom "`;" ] |> case_v ~var:"methodPrototype" }
	| al = annotationList p = functionPrototype pop_scope SEMICOLON
    { [ arg al; arg p; atom "`;" ] |> case_v ~var:"methodPrototype" }
	| al = annotationList ABSTRACT p = functionPrototype
    pop_scope SEMICOLON
    { [ arg al; atom "ABSTRACT"; arg p; atom "`;" ] |> case_v ~var:"methodPrototype" }
;

methodPrototypeList:
  | (* empty *) { [ atom "_EMPTY" ] |> case_v ~var:"methodPrototypeList" }
  | ps = methodPrototypeList p = methodPrototype
    { [ arg ps; arg p ] |> case_v ~var:"methodPrototypeList" }
;

externObjectDeclaration:
  | al = annotationList EXTERN n = push_externName tpl = typeParameterListOpt
    L_BRACE pl = methodPrototypeList R_BRACE pop_scope
    { let decl =
        [ arg al; atom "EXTERN"; arg n; arg tpl; atom "{"; arg pl; atom "}" ]
      |> case_v ~var:"externObjectDeclaration"
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
    { [ arg k; atom "`:"; arg n; atom "`;" ] |> case_v ~var:"selectCase" }
;

selectCaseList:
  | (* empty *) { [ atom "_EMPTY" ] |> case_v ~var:"selectCaseList" }
  | cl = selectCaseList c = selectCase
    { [ arg cl; arg c ] |> case_v ~var:"selectCaseList" }
;

selectExpression:
  | SELECT L_PAREN el = expressionList R_PAREN L_BRACE cl = selectCaseList R_BRACE
    { [ atom "SELECT"; atom "("; arg el; atom ")"; atom "{"; arg cl; atom "}" ]
      |> case_v ~var:"selectExpression" }
;

(* >>>> Transition statements *)
stateExpression:
  | n = name SEMICOLON
    { [ arg n; atom "`;" ] |> case_v ~var:"stateExpression" }
  | e = selectExpression
    { e }
;

transitionStatement:
  | (* empty *) { [ atom "_EMPTY" ] |> case_v ~var:"transitionStatement" }
  | TRANSITION e = stateExpression
    { [ atom "TRANSITION"; arg e ] |> case_v ~var:"transitionStatement" }
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
    { [ arg al; atom "VALUE_SET"; atom "<"; arg t; atom ">"; atom "("; arg s; atom ")"; arg n; atom "`;" ]
      |> case_v ~var:"valueSetDeclaration" }
;

(* >>>> Parser type declarations *)
parserTypeDeclaration:
  | al = annotationList PARSER n = push_name tpl = typeParameterListOpt
      L_PAREN pl = parameterList R_PAREN pop_scope SEMICOLON
    { [ arg al; atom "PARSER"; arg n; arg tpl; atom "("; arg pl; atom ")"; atom "`;" ]
      |> case_v ~var:"parserTypeDeclaration" }
;

(* >>>> Parser declarations *)
parserBlockStatement:
  | al = annotationList L_BRACE sl = parserStatementList R_BRACE
    { [ arg al; atom "{"; arg sl; atom "}" ] |> case_v ~var:"parserBlockStatement" }
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
  | (* empty *) { [ atom "_EMPTY" ] |> case_v ~var:"parserStatementList" }
  | sl = parserStatementList s = parserStatement
    { [ arg sl; arg s ] |> case_v ~var:"parserStatementList" }
;

parserState:
  | al = annotationList STATE n = push_name L_BRACE sl = parserStatementList t = transitionStatement R_BRACE
    { [ arg al; atom "STATE"; arg n; atom "{"; arg sl; arg t; atom "}" ]
      |> case_v ~var:"parserState" }
;

parserStateList:
  | s = parserState { s }
  | sl = parserStateList s = parserState
    { [ arg sl; arg s ] |> case_v ~var:"parserStateList" }
;

parserLocalDeclaration:
  | d = constantDeclaration
  | d = instantiation
  | d = variableDeclaration
  | d = valueSetDeclaration
    { d }
;

parserLocalDeclarationList:
  | (* empty *) { [ atom "_EMPTY" ] |> case_v ~var:"parserLocalDeclarationList" }
  | dl = parserLocalDeclarationList d = parserLocalDeclaration
    { [ arg dl; arg d ] |> case_v ~var:"parserLocalDeclarationList" }
;

parserDeclaration:
  | al = annotationList PARSER n = push_name tpl = typeParameterListOpt
    L_PAREN pl = parameterList R_PAREN cpl = constructorParameterListOpt
    L_BRACE dl = parserLocalDeclarationList sl = parserStateList R_BRACE pop_scope
		{ [ arg al; atom "PARSER"; arg n; arg tpl; atom "("; arg pl; atom ")"; arg cpl;
      atom "{"; arg dl; arg sl; atom "}" ] |> case_v ~var:"parserDeclaration" }
;

(* >> Control statements and declarations *)
(* >>>> Table declarations *)
constOpt:
  | (* empty *) { [ atom "_EMPTY" ] |> case_v ~var:"constOpt" }
  | CONST { [ atom "CONST" ] |> case_v ~var:"constOpt" }
;

(* >>>>>> Table key property *)
tableKey:
  | e = expression COLON n = name al = annotationList SEMICOLON
    { [ arg e; atom "`:"; arg n; arg al; atom "`;" ] |> case_v ~var:"tableKey" }
;

tableKeyList:
  | (* empty *) { [ atom "_EMPTY" ] |> case_v ~var:"tableKeyList" }
  | kl = tableKeyList k = tableKey
    { [ arg kl; arg k ] |> case_v ~var:"tableKeyList" }
;

(* >>>>>> Table actions property *)
tableActionReference:
  | n = prefixedNonTypeName
    { n }
  | n = prefixedNonTypeName L_PAREN al = argumentList R_PAREN
    { [ arg n; atom "("; arg al; atom ")" ] |> case_v ~var:"tableActionReference" }
;

tableAction:
  | al = annotationList ac = tableActionReference SEMICOLON
    { [ arg al; arg ac; atom "`;" ] |> case_v ~var:"tableAction" }
;

tableActionList:
  | (* empty *) { [ atom "_EMPTY" ] |> case_v ~var:"tableActionList" }
  | acl = tableActionList ac = tableAction
    { [ arg acl; arg ac ] |> case_v ~var:"tableActionList" }
;

(* >>>>>> Table entry property *)
tableEntryPriority:
  | PRIORITY ASSIGN num = number COLON
    { [ atom "PRIORITY"; atom "="; arg num; atom "`:" ] |> case_v ~var:"tableEntryPriority" }
  | PRIORITY ASSIGN L_PAREN e = expression R_PAREN COLON
    { [ atom "PRIORITY"; atom "="; atom "("; arg e; atom ")"; atom "`:" ] |> case_v ~var:"tableEntryPriority" }
;

tableEntry:
  | c = constOpt p = tableEntryPriority k = keysetExpression COLON ac = tableActionReference al = annotationList SEMICOLON
    { [ arg c; arg p; arg k; atom "`:"; arg ac; arg al; atom "`;" ] |> case_v ~var:"tableEntry" }
  | c = constOpt k = keysetExpression COLON ac = tableActionReference al = annotationList SEMICOLON
    { [ arg c; arg k; atom "`:"; arg ac; arg al; atom "`;" ] |> case_v ~var:"tableEntry" }
;

tableEntryList:
  | (* empty *) { [ atom "_EMPTY" ] |> case_v ~var:"tableEntryList" }
  | el = tableEntryList e = tableEntry
    { [ arg el; arg e ] |> case_v ~var:"tableEntryList" }
;

(* >>>>>> Table properties *)
tableProperty:
  | KEY ASSIGN L_BRACE kl = tableKeyList R_BRACE
    { [ atom "KEY"; atom "="; atom "{"; arg kl; atom "}" ] |> case_v ~var:"tableProperty" }
  | ACTIONS ASSIGN L_BRACE acl = tableActionList R_BRACE
    { [ atom "ACTIONS"; atom "="; atom "{"; arg acl; atom "}" ] |> case_v ~var:"tableProperty" }
  | al = annotationList c = constOpt ENTRIES ASSIGN L_BRACE el = tableEntryList R_BRACE
    { [ arg al; arg c; atom "ENTRIES"; atom "="; atom "{"; arg el; atom "}" ] |> case_v ~var:"tableProperty" }
  | al = annotationList c = constOpt n = tableCustomName i = initialValue SEMICOLON
    { [ arg al; arg c; arg n; arg i; atom "`;" ] |> case_v ~var:"tableProperty" }
;

tablePropertyList:
  | (* empty *) { [ atom "_EMPTY" ] |> case_v ~var:"tablePropertyList" }
  | pl = tablePropertyList p = tableProperty
    { [ arg pl; arg p ] |> case_v ~var:"tablePropertyList" }
;

tableDeclaration:
  | al = annotationList TABLE n = name L_BRACE pl = tablePropertyList R_BRACE
    { [ arg al; atom "TABLE"; arg n; atom "{"; arg pl; atom "}" ] |> case_v ~var:"tableDeclaration" }

(* >>>> Control type declarations *)
controlTypeDeclaration:
  | al = annotationList CONTROL n = push_name tpl = typeParameterListOpt
    L_PAREN pl = parameterList R_PAREN pop_scope SEMICOLON
    { [ arg al; atom "CONTROL"; arg n; arg tpl; atom "("; arg pl; atom ")"; atom "`;" ]
      |> case_v ~var:"controlTypeDeclaration" }
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
  | (* empty *) { [ atom "_EMPTY" ] |> case_v ~var:"controlLocalDeclarationList" }
  | dl = controlLocalDeclarationList d = controlLocalDeclaration
    { [ arg dl; arg d ] |> case_v ~var:"controlLocalDeclarationList" }
;

controlDeclaration:
  | al = annotationList CONTROL n = push_name tpl = typeParameterListOpt
    L_PAREN pl = parameterList R_PAREN cpl = constructorParameterListOpt
    L_BRACE dl = controlLocalDeclarationList APPLY b = controlBody R_BRACE pop_scope
    { [ arg al; atom "CONTROL"; arg n; arg tpl; atom "("; arg pl; atom ")"; arg cpl;
      atom "{"; arg dl; atom "APPLY"; arg b; atom "}" ] |> case_v ~var:"controlDeclaration" }
;

(* >> Package type declarations *)
packageTypeDeclaration:
  | al = annotationList PACKAGE n = push_name tpl = typeParameterListOpt
    L_PAREN pl = parameterList R_PAREN pop_scope SEMICOLON
    { [ arg al; atom "PACKAGE"; arg n; arg tpl; atom "("; arg pl; atom ")"; atom "`;" ]
      |> case_v ~var:"packageTypeDeclaration" }
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
    { [ atom "UNEXPECTED_TOKEN" ] |> case_v ~var:"annotationToken" }
	| ABSTRACT
    { [ atom "ABSTRACT" ] |> case_v ~var:"annotationToken" }
	| ACTION
    { [ atom "ACTION" ] |> case_v ~var:"annotationToken" }
	| ACTIONS
    { [ atom "ACTIONS" ] |> case_v ~var:"annotationToken" }
	| APPLY
    { [ atom "APPLY" ] |> case_v ~var:"annotationToken" }
	| BOOL
    { [ atom "BOOL" ] |> case_v ~var:"annotationToken" }
	| BIT
    { [ atom "BIT" ] |> case_v ~var:"annotationToken" }
	| BREAK
    { [ atom "BREAK" ] |> case_v ~var:"annotationToken" }
	| CONST
    { [ atom "CONST" ] |> case_v ~var:"annotationToken" }
	| CONTINUE
    { [ atom "CONTINUE" ] |> case_v ~var:"annotationToken" }
	| CONTROL
    { [ atom "CONTROL" ] |> case_v ~var:"annotationToken" }
	| DEFAULT
    { [ atom "DEFAULT" ] |> case_v ~var:"annotationToken" }
	| ELSE
    { [ atom "ELSE" ] |> case_v ~var:"annotationToken" }
	| ENTRIES
    { [ atom "ENTRIES" ] |> case_v ~var:"annotationToken" }
	| ENUM
    { [ atom "ENUM" ] |> case_v ~var:"annotationToken" }
	| ERROR
    { [ atom "ERROR" ] |> case_v ~var:"annotationToken" }
	| EXIT
    { [ atom "EXIT" ] |> case_v ~var:"annotationToken" }
	| EXTERN
    { [ atom "EXTERN" ] |> case_v ~var:"annotationToken" }
	| FALSE
    { [ atom "FALSE" ] |> case_v ~var:"annotationToken" }
	| FOR
    { [ atom "FOR" ] |> case_v ~var:"annotationToken" }
	| HEADER
    { [ atom "HEADER" ] |> case_v ~var:"annotationToken" }
	| HEADER_UNION
    { [ atom "HEADER_UNION" ] |> case_v ~var:"annotationToken" }
	| IF
    { [ atom "IF" ] |> case_v ~var:"annotationToken" }
	| IN
    { [ atom "IN" ] |> case_v ~var:"annotationToken" }
	| INOUT
    { [ atom "INOUT" ] |> case_v ~var:"annotationToken" }
	| INT
    { [ atom "INT" ] |> case_v ~var:"annotationToken" }
	| KEY
    { [ atom "KEY" ] |> case_v ~var:"annotationToken" }
	| MATCH_KIND
    { [ atom "MATCH_KIND" ] |> case_v ~var:"annotationToken" }
	| TYPE
    { [ atom "TYPE" ] |> case_v ~var:"annotationToken" }
	| OUT
    { [ atom "OUT" ] |> case_v ~var:"annotationToken" }
	| PARSER
    { [ atom "PARSER" ] |> case_v ~var:"annotationToken" }
	| PACKAGE
    { [ atom "PACKAGE" ] |> case_v ~var:"annotationToken" }
	| PRAGMA
    { [ atom "PRAGMA" ] |> case_v ~var:"annotationToken" }
	| RETURN
    { [ atom "RETURN" ] |> case_v ~var:"annotationToken" }
	| SELECT
    { [ atom "SELECT" ] |> case_v ~var:"annotationToken" }
	| STATE
    { [ atom "STATE" ] |> case_v ~var:"annotationToken" }
	| STRING
    { [ atom "STRING" ] |> case_v ~var:"annotationToken" }
	| STRUCT
    { [ atom "STRUCT" ] |> case_v ~var:"annotationToken" }
	| SWITCH
    { [ atom "SWITCH" ] |> case_v ~var:"annotationToken" }
	| TABLE
    { [ atom "TABLE" ] |> case_v ~var:"annotationToken" }
	| THIS
    { [ atom "THIS" ] |> case_v ~var:"annotationToken" }
	| TRANSITION
    { [ atom "TRANSITION" ] |> case_v ~var:"annotationToken" }
	| TRUE
    { [ atom "TRUE" ] |> case_v ~var:"annotationToken" }
	| TUPLE
    { [ atom "TUPLE" ] |> case_v ~var:"annotationToken" }
	| TYPEDEF
    { [ atom "TYPEDEF" ] |> case_v ~var:"annotationToken" }
	| VARBIT
    { [ atom "VARBIT" ] |> case_v ~var:"annotationToken" }
	| VALUE_SET
    { [ atom "VALUE_SET" ] |> case_v ~var:"annotationToken" }
	| LIST
    { [ atom "LIST" ] |> case_v ~var:"annotationToken" }
	| VOID
    { [ atom "VOID" ] |> case_v ~var:"annotationToken" }
	| DONTCARE
    { [ atom "_" ] |> case_v ~var:"annotationToken" }
	| id = identifier
    { id }
	| tid = typeIdentifier
    { tid }
	| str = stringLiteral
    { str }
	| num = number
    { num }
	| MASK
    { [ atom "&&&" ] |> case_v ~var:"annotationToken" }
  (* TODO: missing DOTS "..." in spec *)
	| RANGE
    { [ atom "`.." ] |> case_v ~var:"annotationToken" }
	| SHL
    { [ atom "<<" ] |> case_v ~var:"annotationToken" }
	| AND
    { [ atom "&&" ] |> case_v ~var:"annotationToken" }
	| OR
    { [ atom "||" ] |> case_v ~var:"annotationToken" }
	| EQ
    { [ atom "==" ] |> case_v ~var:"annotationToken" }
	| NE
    { [ atom "!=" ] |> case_v ~var:"annotationToken" }
	| GE
    { [ atom ">=" ] |> case_v ~var:"annotationToken" }
	| LE
    { [ atom "<=" ] |> case_v ~var:"annotationToken" }
	| PLUSPLUS
    { [ atom "++" ] |> case_v ~var:"annotationToken" }
	| PLUS
    { [ atom "+" ] |> case_v ~var:"annotationToken" }
	| PLUS_SAT
    { [ atom "|+|" ] |> case_v ~var:"annotationToken" }
	| MINUS
    { [ atom "-" ] |> case_v ~var:"annotationToken" }
	| MINUS_SAT
    { [ atom "|-|" ] |> case_v ~var:"annotationToken" }
	| MUL
    { [ atom "*" ] |> case_v ~var:"annotationToken" }
	| DIV
    { [ atom "/" ] |> case_v ~var:"annotationToken" }
	| MOD
    { [ atom "%" ] |> case_v ~var:"annotationToken" }
	| BIT_OR
    { [ atom "|" ] |> case_v ~var:"annotationToken" }
	| BIT_AND
    { [ atom "&" ] |> case_v ~var:"annotationToken" }
	| BIT_XOR
    { [ atom "^" ] |> case_v ~var:"annotationToken" }
	| COMPLEMENT
    { [ atom "~" ] |> case_v ~var:"annotationToken" }
	| L_BRACKET
    { [ atom "``[" ] |> case_v ~var:"annotationToken" }
	| R_BRACKET
    { [ atom "``]" ] |> case_v ~var:"annotationToken" }
	| L_BRACE
    { [ atom "``{" ] |> case_v ~var:"annotationToken" }
	| R_BRACE
    { [ atom "``}" ] |> case_v ~var:"annotationToken" }
	| L_ANGLE
    { [ atom "``<" ] |> case_v ~var:"annotationToken" }
	| R_ANGLE
    { [ atom "``>" ] |> case_v ~var:"annotationToken" }
	| NOT
    { [ atom "!" ] |> case_v ~var:"annotationToken" }
	| COLON
    { [ atom "`:" ] |> case_v ~var:"annotationToken" }
	| COMMA
    { [ atom "," ] |> case_v ~var:"annotationToken" }
	| QUESTION
    { [ atom "?" ] |> case_v ~var:"annotationToken" }
	| DOT
    { [ atom "`." ] |> case_v ~var:"annotationToken" }
	| ASSIGN
    { [ atom "=" ] |> case_v ~var:"annotationToken" }
	| SEMICOLON
    { [ atom "`;" ] |> case_v ~var:"annotationToken" }
	| AT
    { [ atom "@" ] |> case_v ~var:"annotationToken" }
;

annotationBody:
	| (* empty *) { [ atom "_EMPTY" ] |> case_v ~var:"annotationBody" }
	| ab = annotationBody L_PAREN ab_in = annotationBody R_PAREN
    { [ arg ab; atom "("; arg ab_in; atom ")" ] |> case_v ~var:"annotationBody" }
	| ab = annotationBody at = annotationToken
    { [ arg ab; arg at ] |> case_v ~var:"annotationBody" }
;

structuredAnnotationBody:
	| e = dataElementExpression c = trailingCommaOpt
    { [ arg e; arg c ] |> case_v ~var:"structuredAnnotationBody" }
;

annotation:
	| AT name = name
    { [ atom "@"; arg name ] |> case_v ~var:"annotation" }
	| AT name = name L_PAREN body = annotationBody R_PAREN
    { [ atom "@"; arg name; atom "("; arg body; atom ")" ] |> case_v ~var:"annotation" }
	| AT name = name L_BRACKET body = structuredAnnotationBody R_BRACKET
    { [ atom "@"; arg name; atom "["; arg body; atom "]" ] |> case_v ~var:"annotation" }
(* From Petr4: PRAGMA not in Spec, but in Petr4/p4c *)
	| PRAGMA name = name body = annotationBody PRAGMA_END
    { [ atom "@"; atom "PRAGMA"; arg name; arg body ] |> case_v ~var:"annotation" }
;

annotationListNonEmpty:
	| a = annotation { a }
	| al = annotationListNonEmpty a = annotation
		{ [ arg al; arg a ] |> case_v ~var:"annotationListNonEmpty" }
;

%inline annotationList:
	| (* empty *) { [ atom "_EMPTY" ] |> case_v ~var:"annotationList" }
	| al = annotationListNonEmpty { al }
;

(******** P4 program ********)
declarationList:
  | (* empty *) { [ atom "_EMPTY" ] |> case_v ~var:"p4program" }
  | ds = declarationList d = declaration
    { [ arg ds; arg d ] |> case_v ~var:"p4program" }
  | ds = declarationList SEMICOLON
    { [ arg ds; atom "`;" ] |> case_v ~var:"p4program" }
;

p4program:
	| ds = declarationList END { ds }
