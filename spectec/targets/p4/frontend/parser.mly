%{
  open Lang.Il
  open Lang.Il.Value
  open Lang.Il.Case
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
    { [ tag "EMPTY" ] |> case_v ~var:"trailingCommaOpt" }
	| COMMA
    { [ op "," ] |> case_v ~var:"trailingCommaOpt" }
;

(* Numbers *)
number:
	| int = int
    { [ kw "D"; arg int ] |> case_v ~var:"number" }
(* Processed by lexer *)
	| number = NUMBER
    { fst number }
;

(* Strings *)
stringLiteral:
	| text = STRING_LITERAL
    { [ op (Char.escaped '"'); arg text; op (Char.escaped '"') ] |> case_v ~var:"stringLiteral"}
;

(* Names *)
identifier:
	| text = NAME IDENTIFIER
    { [ tag "ID"; arg text ] |> case_v ~var:"identifier" }
;

typeIdentifier:
	| text = NAME TYPENAME
    { [ tag "TID"; arg text ] |> case_v ~var:"typeIdentifier" }
;

(* >> Non-type names *)
nonTypeName:
	| id = identifier { id }
	| APPLY { [ kw "APPLY" ] |> case_v ~var:"nonTypeName" }
	| KEY { [ kw "KEY" ] |> case_v ~var:"nonTypeName" }
	| ACTIONS { [ kw "ACTIONS" ] |> case_v ~var:"nonTypeName" }
	| STATE { [ kw "STATE" ] |> case_v ~var:"nonTypeName" }
	| ENTRIES { [ kw "ENTRIES" ] |> case_v ~var:"nonTypeName" }
	| TYPE { [ kw "TYPE" ] |> case_v ~var:"nonTypeName" }
	| PRIORITY { [ kw "PRIORITY" ] |> case_v ~var:"nonTypeName" }
;

prefixedNonTypeName:
	| n = nonTypeName { n }
	| DOT go_toplevel n = nonTypeName go_local
    { [ tag "ID"; op "."; arg n ] |> case_v ~var:"prefixedNonTypeName" }
;

(* >> Type names *)
typeName:
	| n = typeIdentifier { n }
;

prefixedTypeName:
	| n = typeName { n }
	| DOT go_toplevel tid = typeName go_local
		{ [ tag "TID"; op "."; arg tid ] |> case_v ~var:"prefixedType" }
;

(* >> Table custom property names *)
tableCustomName:
	| id = identifier { id }
	| tid = typeIdentifier { tid }
	| APPLY { [ kw "APPLY" ] |> case_v ~var:"tableCustomName" }
	| STATE { [ kw "STATE" ] |> case_v ~var:"tableCustomName" }
	| TYPE { [ kw "TYPE" ] |> case_v ~var:"tableCustomName" }
	| PRIORITY { [ kw "PRIORITY" ] |> case_v ~var:"tableCustomName" }
;

(* >> Names *)
name:
	| n = nonTypeName
	| n = typeName
    { n }
	| LIST { [ kw "LIST" ] |> case_v ~var:"name" }
;

nameList:
	| n = name { n }
	| ns = nameList COMMA n = name
    { [ arg ns; op ","; arg n ]
      |> case_v ~var:"nameList" }
;

member:
	| name = name
    { name }
;

(* Directions *)
direction:
	| (* empty *) { [ tag "EMPTY" ] |> case_v ~var:"direction" }
	| IN { [ kw "IN" ] |> case_v ~var:"direction" }
	| OUT { [ kw "OUT" ] |> case_v ~var:"direction" }
	| INOUT { [ kw "INOUT" ] |> case_v ~var:"direction" }
;

(* Types *)
(* >> Base types *)
baseType:
	| BOOL { [ kw "BOOL" ] |> case_v ~var:"baseType" }
	| MATCH_KIND { [ kw "MATCH_KIND" ] |> case_v ~var:"baseType" }
	| ERROR { [ kw "ERROR" ] |> case_v ~var:"baseType" }
	| BIT { [ kw "BIT" ] |> case_v ~var:"baseType" }
	| STRING { [ kw "STRING" ] |> case_v ~var:"baseType"}
	| INT
    { [ kw "INT" ] |> case_v ~var:"baseType" }
	| BIT l_angle v = int r_angle
    { [ kw "BIT"; langle (); arg v; rangle () ]
      |> case_v ~var:"baseType" }
	| INT l_angle v = int r_angle
    { [ kw "INT"; langle (); arg v; rangle () ]
      |> case_v ~var:"baseType" }
	| VARBIT l_angle v = int r_angle
    { [ kw "VARBIT"; langle (); arg v; rangle () ] |> case_v ~var:"baseType" }
	| BIT l_angle L_PAREN e = expression R_PAREN r_angle
    { [ kw "BIT"; langle (); lparen (); arg e; rparen (); rangle () ] |> case_v ~var:"baseType" }
	| INT l_angle L_PAREN e = expression R_PAREN r_angle
    { [ kw "INT"; langle (); lparen (); arg e; rparen (); rangle () ]
      |> case_v ~var:"baseType" }
	| VARBIT l_angle L_PAREN e = expression R_PAREN r_angle
    { [ kw "VARBIT"; langle (); lparen (); arg e; rparen (); rangle () ] |> case_v ~var:"baseType" }
;

(* >> Named types *)
specializedType:
  | n = prefixedTypeName l_angle tal = typeArgumentList r_angle
    { [ arg n; langle (); arg tal; rangle () ] |> case_v ~var:"specializedType" }
;

namedType:
  | t = prefixedTypeName
  | t = specializedType
    { t }
;

(* >> Header stack types *)
headerStackType:
  | t = namedType L_BRACKET e = expression R_BRACKET
    { [ arg t; lbrack (); arg e; rbrack () ] |> case_v ~var:"headerStackType" }
;

(* >> List types *)
listType:
  | LIST l_angle targ = typeArgument r_angle
    { [ kw "LIST"; langle (); arg targ; rangle () ] |> case_v ~var:"listType" }
;

(* >> Tuple types *)
tupleType:
	| TUPLE l_angle targs = typeArgumentList r_angle
    { [ kw "TUPLE"; langle (); arg targs; rangle () ] |> case_v ~var:"tupleType" }
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
	| VOID { [ kw "VOID" ] |> case_v ~var:"typeOrVoid" }
  (* From Petr4: HACK for generic return type *)
	| id = identifier
    { match flatten_case_v id with
      | "identifier", ["_ID"], [ value_text ]  ->
        [ tag "TID"; arg value_text ] |> case_v ~var:"typeIdentifier"
      | _ -> failwith "@typeOrVoid: expected identifier" }
;

(* Type parameters *)
typeParameter:
	| n = name { n }

typeParameterList:
	| tp = typeParameter { tp }
	| tps = typeParameterList COMMA tp = typeParameter
    { [ arg tps; op ","; arg tp ] |> case_v ~var:"typeParameterList" }
;

typeParameterListOpt:
	| (* empty *) { [ tag "EMPTY" ] |> case_v ~var:"typeParameterListOpt" }
	| l_angle tps = typeParameterList r_angle
    { declare_types_of_il tps;
      [ langle (); arg tps; rangle () ] |> case_v ~var:"typeParameterListOpt" }
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
    { [ arg ps; op ","; arg p ] |> case_v ~var:"nonEmptyParameterList" }
;

parameterList:
	| (* empty *) { [ tag "EMPTY" ] |> case_v ~var:"parameterList" }
	| ps = nonEmptyParameterList { ps }
;

(* Constructor parameters *)
constructorParameterListOpt:
	| (* empty *) { [ tag "EMPTY" ] |> case_v ~var:"constructorParameterListOpt" }
	| L_PAREN ps = parameterList R_PAREN
    { [ lparen (); arg ps; rparen () ] |> case_v ~var:"constructorParameterListOpt" }
;

(* Expression key-value pairs *)
namedExpression:
	| n = name ASSIGN e = expression { [ arg n; op "="; arg e ] |> case_v ~var:"namedExpression" }
;

namedExpressionList:
	| e = namedExpression { e }
	| es = namedExpressionList COMMA e = namedExpression { [ arg es; op ","; arg e ] |> case_v ~var:"namedExpressionList" }
;

(* Expressions *)
(* >> Literal expressions *)
%inline literalExpression:
	| TRUE { [ kw "TRUE" ] |> case_v ~var:"literalExpression" }
	| FALSE { [ kw "FALSE" ] |> case_v ~var:"literalExpression" }
	| num = number { num }
	| str = stringLiteral { str }
;

(* >> Reference expressions *)
%inline referenceExpression:
	| n = prefixedNonTypeName { n }
	| THIS { [ kw "THIS" ] |> case_v ~var:"referenceExpression" }
;

(* >> Default expressions *)
%inline defaultExpression:
	| DOTS { [ op "..." ] |> case_v ~var:"defaultExpression" }
;

(* >> Unary, binary, and ternary expressions *)
%inline unop: 
	| NOT { [ op "!" ] |> case_v ~var:"unop" }
	| COMPLEMENT { [ op "~" ] |> case_v ~var:"unop" }
	| MINUS { [ op "-" ] |> case_v ~var:"unop" }
	| PLUS { [ op "+" ] |> case_v ~var:"unop" }
;

%inline unaryExpression:
	| o = unop e = expression %prec PREFIX
		{ [ arg o; arg e ] |> case_v ~var:"unaryExpression" }
;

%inline binop:
  | MUL { [ op "*" ] |> case_v ~var:"binop" }
  | DIV { [ op "/" ] |> case_v ~var:"binop" }
  | MOD { [ op "%" ] |> case_v ~var:"binop" }
  | PLUS { [ op "+" ] |> case_v ~var:"binop" }
  | PLUS_SAT { [ op "|+|" ] |> case_v ~var:"binop" }
  | MINUS { [ op "-" ] |> case_v ~var:"binop" }
  | MINUS_SAT { [ op "|-|" ] |> case_v ~var:"binop" }
  | SHL { [ op "<<" ] |> case_v ~var:"binop" }
  | r_angle R_ANGLE_SHIFT { [ op ">>" ] |> case_v ~var:"binop" }
  | LE { [ op "<=" ] |> case_v ~var:"binop" }
  | GE { [ op ">=" ] |> case_v ~var:"binop" }
  | l_angle { [ op "<" ] |> case_v ~var:"binop" }
  | r_angle { [ op ">" ] |> case_v ~var:"binop" }
  | NE { [ op "!=" ] |> case_v ~var:"binop" }
  | EQ { [ op "==" ] |> case_v ~var:"binop" }
  | BIT_AND { [ op "&" ] |> case_v ~var:"binop" }
  | BIT_XOR { [ op "^" ] |> case_v ~var:"binop" }
  | BIT_OR { [ op "|" ] |> case_v ~var:"binop" }
  | PLUSPLUS { [ op "++" ] |> case_v ~var:"binop" }
  | AND { [ op "&&" ] |> case_v ~var:"binop" }
  | OR { [ op "||" ] |> case_v ~var:"binop" }
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
		{ [ arg c; op "?"; arg t; op ":"; arg f ] |> case_v ~var:"ternaryExpression" }
;

%inline ternaryExpressionNonBrace:
	| c = expressionNonBrace QUESTION t = expression COLON f = expression
		{ [ arg c; op "?"; arg t; op ":"; arg f ] |> case_v ~var:"ternaryExpressionNonBrace" }
;

(* >> Cast expressions *)
%inline castExpression:
	| L_PAREN t = typeRef R_PAREN e = expression %prec PREFIX
    { [ lparen (); arg t; rparen (); arg e ] |> case_v ~var:"castExpression" }
;

(* >> Data (aggregate) expressions *)
%inline dataExpression:
	| INVALID { [ op "{#}" ] |> case_v ~var:"dataExpression" }
	| L_BRACE e = dataElementExpression c = trailingCommaOpt R_BRACE
    { [ lbrace (); arg e; arg c; rbrace () ] |> case_v ~var:"dataExpression" }
;

(* >> Member and index access expressions *)
%inline errorAccessExpression:
	| ERROR DOT m = member
		{ [ kw "ERROR"; op "."; arg m ] |> case_v ~var:"errorAccessExpression" }
;

%inline memberAccessExpression:
	| e = memberAccessBase DOT m = member %prec DOT
		{ [ arg e; op "."; arg m ] |> case_v ~var:"memberAccessExpression" }
;

%inline indexAccessExpression:
	| a = expression L_BRACKET i = expression R_BRACKET
		{ [ arg a; lbrack (); arg i; rbrack () ] |> case_v ~var:"indexAccessExpression" }
	| a = expression L_BRACKET h = expression COLON l = expression R_BRACKET
		{ [ arg a; lbrack (); arg h; op ":"; arg l; rbrack () ] |> case_v ~var:"indexAccessExpression" }
;

%inline accessExpression:
	| e = errorAccessExpression
	| e = memberAccessExpression
	| e = indexAccessExpression
		{ e }
;

%inline memberAccessExpressionNonBrace:
	| e = memberAccessBaseNonBrace DOT m = member %prec DOT
		{ [ arg e; op "."; arg m ] |> case_v ~var:"memberAccessExpressionNonBrace" }
;

%inline indexAccessExpressionNonBrace:
	| a = expressionNonBrace L_BRACKET i = expression R_BRACKET
		{ [ arg a; lbrack (); arg i; rbrack () ] |> case_v ~var:"indexAccessExpressionNonBrace" }
	| a = expressionNonBrace L_BRACKET h = expression COLON l = expression R_BRACKET
		{ [ arg a; lbrack (); arg h; op ":"; arg l; rbrack () ] |> case_v ~var:"indexAccessExpressionNonBrace" }
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
		{ [ arg t; lparen (); arg args; rparen () ] |> case_v ~var:"callExpression" }
	| t = routineTarget l_angle targs = realTypeArgumentList r_angle L_PAREN args = argumentList R_PAREN
		{ [ arg t; langle (); arg targs; rangle (); lparen (); arg args; rparen () ]
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
		{ [ arg t; lparen (); arg args; rparen () ] |> case_v ~var:"callExpressionNonBrace" }
	| t = routineTargetNonBrace l_angle targs = realTypeArgumentList r_angle L_PAREN args = argumentList R_PAREN
		{ [ arg t; langle (); arg targs; rangle (); lparen (); arg args; rparen () ]
      |> case_v ~var:"callExpressionNonBrace" }

(* >> Parenthesized Expressions *)

%inline parenthesizedExpression:
	| L_PAREN e = expression R_PAREN
		{ [ lparen (); arg e; rparen () ] |> case_v ~var:"parenthesizedExpression" }
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
	| (* empty *) { [ tag "EMPTY" ] |> case_v ~var:"expressionList" }
	| e = expression { e }
	| el = expressionList COMMA e = expression
		{ [ arg el; op ","; arg e ] |> case_v ~var:"expressionList" }
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
    { [ arg n; op "="; arg e ]
      |> case_v ~var:"recordElementExpression" }
  | n = name ASSIGN e = expression COMMA DOTS
    { [ arg n; op "="; arg e; op ","; op "..." ]
      |> case_v ~var:"recordElementExpression" }
	| n = name ASSIGN e = expression COMMA el = namedExpressionList
    { [ arg n; op "="; arg e; op ","; arg el ]
      |> case_v ~var:"recordElementExpression" }
  | n = name ASSIGN e = expression COMMA el = namedExpressionList COMMA DOTS
    { [ arg n; op "="; arg e; op ","; arg el; op ","; op "..." ]
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
    { [ arg b; op "&&&"; arg m ] |> case_v ~var:"simpleKeysetExpression" }
	| l = expression RANGE h = expression
    { [ arg l; op ".."; arg h ] |> case_v ~var:"simpleKeysetExpression" }
	| DEFAULT
    { [ kw "DEFAULT" ] |> case_v ~var:"simpleKeysetExpression" }
	| DONTCARE
    { [ op "_" ] |> case_v ~var:"simpleKeysetExpression" }
;

simpleKeysetExpressionList:
	| e = simpleKeysetExpression { e }
	| el = simpleKeysetExpressionList COMMA e = simpleKeysetExpression
    { [ arg el; op ","; arg e ] |> case_v ~var:"simpleKeysetExpressionList" }
;

tupleKeysetExpression:
	| L_PAREN b = expression MASK m = expression R_PAREN
		{ [ lparen (); arg b; op "&&&"; arg m; rparen () ] |> case_v ~var:"tupleKeysetExpression" }
	| L_PAREN l = expression RANGE h = expression R_PAREN
		{ [ lparen (); arg l; op ".."; arg h; rparen () ] |> case_v ~var:"tupleKeysetExpression" }
	| L_PAREN DEFAULT R_PAREN
		{ [ lparen (); kw "DEFAULT"; rparen () ] |> case_v ~var:"tupleKeysetExpression" }
	| L_PAREN DONTCARE R_PAREN
		{ [ lparen (); op "_"; rparen () ] |> case_v ~var:"tupleKeysetExpression" }
	| L_PAREN e = simpleKeysetExpression COMMA es = simpleKeysetExpressionList R_PAREN
		{ [ lparen (); arg e; op ","; arg es; rparen () ] |> case_v ~var:"tupleKeysetExpression" }
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
    { [ kw "VOID" ] |> case_v ~var:"realTypeArgument" }
	| DONTCARE
    { [ op "_" ] |> case_v ~var:"realTypeArgument" }
;

realTypeArgumentList:
	| targ = realTypeArgument { targ }
	| targs = realTypeArgumentList COMMA targ = realTypeArgument
    { [ arg targs; op ","; arg targ ] |> case_v ~var:"realTypeArgumentList" }
;

typeArgument:
	| t = typeRef
	| t = nonTypeName 
		{ t }
	| VOID
    { [ kw "VOID" ] |> case_v ~var:"typeArgument" }
	| DONTCARE
    { [ op "_" ] |> case_v ~var:"typeArgument" }
;

typeArgumentList:
	| (* empty *) { [ tag "EMPTY" ] |> case_v ~var:"typeArgumentList" }
	| targ = typeArgument { targ }
	| targs = typeArgumentList COMMA targ = typeArgument
    { [ arg targs; op ","; arg targ ] |> case_v ~var:"typeArgumentList" }
;

(* Arguments *)
argument:
	| e = expression { e }
	| n = name ASSIGN e = expression 
		{ [ arg n; op "="; arg e ] |> case_v ~var:"argument" }
	| name = name ASSIGN DONTCARE
		{ [ arg name; op "="; op "_" ] |> case_v ~var:"argument" }
	| DONTCARE
		{ [ op "_" ] |> case_v ~var:"argument" }
;

argumentListNonEmpty:
	| a = argument { a }
	| args = argumentListNonEmpty COMMA a = argument
    { [ arg args; op ","; arg a ] |> case_v ~var:"argumentListNonEmpty" }
;

argumentList:
	| (* empty *) { [ tag "EMPTY" ] |> case_v ~var:"argumentList" }
	| args = argumentListNonEmpty { args }
;

(* L-values *)
lvalue:
	| e = referenceExpression { e }
	| lv = lvalue DOT m = member %prec DOT
		{ [ arg lv; op "."; arg m ] |> case_v ~var:"lvalue" }
	| lv = lvalue L_BRACKET i = expression R_BRACKET
		{ [ arg lv; lbrack (); arg i; rbrack () ] |> case_v ~var:"lvalue" }
	| lv = lvalue L_BRACKET h = expression COLON l = expression R_BRACKET
		{ [ arg lv; lbrack (); arg h; op ":"; arg l; rbrack () ] |> case_v ~var:"lvalue" }
	| L_PAREN lv = lvalue R_PAREN
		{ [ lparen (); arg lv; rparen () ] |> case_v ~var:"lvalue" }
;

(* Statements *)
(* >> Empty statements *)
emptyStatement:
	| SEMICOLON { [ op ";" ] |> case_v ~var:"emptyStatement" }
;

(* >> Assignment statements *)
assignop:
	| ASSIGN { [ op "=" ] |> case_v ~var:"assignop" }
	| PLUS_ASSIGN { [ op "+=" ] |> case_v ~var:"assignop" }
	| PLUS_SAT_ASSIGN { [ op "|+|=" ] |> case_v ~var:"assignop" }
	| MINUS_ASSIGN { [ op "-=" ] |> case_v ~var:"assignop" }
	| MINUS_SAT_ASSIGN { [ op "|-|=" ] |> case_v ~var:"assignop" }
	| MUL_ASSIGN { [ op "*=" ] |> case_v ~var:"assignop" }
	| DIV_ASSIGN { [ op "/=" ] |> case_v ~var:"assignop" }
	| MOD_ASSIGN { [ op "%=" ] |> case_v ~var:"assignop" }
	| SHL_ASSIGN { [ op "<<=" ] |> case_v ~var:"assignop" }
	| SHR_ASSIGN { [ op ">>=" ] |> case_v ~var:"assignop" }
	| BIT_AND_ASSIGN { [ op "&=" ] |> case_v ~var:"assignop" }
	| BIT_XOR_ASSIGN { [ op "^=" ] |> case_v ~var:"assignop" }
	| BIT_OR_ASSIGN { [ op "|=" ] |> case_v ~var:"assignop" }
;

assignmentStatement:
	| lv = lvalue o = assignop e = expression SEMICOLON
		{ [ arg lv; arg o; arg e; op ";" ] |> case_v ~var:"assignmentStatement" }
;

(* >> Call statements *)
callStatement:
	| lv = lvalue L_PAREN args = argumentList R_PAREN SEMICOLON
		{ [ arg lv; lparen (); arg args; rparen (); op ";" ] |> case_v ~var:"callStatement" }
	| lv = lvalue l_angle targs = typeArgumentList r_angle L_PAREN args = argumentList R_PAREN SEMICOLON
		{ [ arg lv; langle (); arg targs; rangle (); lparen (); arg args; rparen (); op ";" ]
      |> case_v ~var:"callStatement" }
;

(* >> Direct application statements *)
directApplicationStatement:
	| t = namedType DOT APPLY L_PAREN args = argumentList R_PAREN SEMICOLON
    { [ arg t; op "."; kw "APPLY"; lparen (); arg args; rparen (); op ";" ]
      |> case_v ~var:"directApplicationStatement" }
;

(* >> Return statements *)
returnStatement:
	| RETURN SEMICOLON
    { [ kw "RETURN"; op ";" ] |> case_v ~var:"returnStatement" }
	| RETURN e = expression SEMICOLON
    { [ kw "RETURN"; arg e; op ";" ] |> case_v ~var:"returnStatement" }
;

(* >> Exit statements *)
exitStatement:
	| EXIT SEMICOLON
    { [ kw "EXIT"; op ";" ] |> case_v ~var:"exitStatement" }
;

(* >> Block statements *)
blockStatement:
	| al = annotationList L_BRACE
  push_scope
  sl = blockElementStatementList R_BRACE
  pop_scope
		{ [ arg al; lbrace (); arg sl; rbrace () ] |> case_v ~var:"blockStatement" }
;

(* >> Conditional statements *)
conditionalStatement:
	| IF L_PAREN c = expression R_PAREN t = statement %prec THEN
    { [ kw "IF"; lparen (); arg c; rparen (); arg t ]
      |> case_v ~var:"conditionalStatement" }
	| IF L_PAREN c = expression R_PAREN t = statement ELSE f = statement
    { [ kw "IF"; lparen (); arg c; rparen (); arg t; kw "ELSE"; arg f ]
      |> case_v ~var:"conditionalStatement" }
;

(* >> For statements *)
forInitStatement:
	| al = annotationList t = typeRef n = name i = initializerOpt
		{ [ arg al; arg t; arg n; arg i ] |> case_v ~var:"forInitStatement" }
	| lv = lvalue L_PAREN args = argumentList R_PAREN
		{ [ arg lv; lparen (); arg args; rparen () ] |> case_v ~var:"forInitStatement" }
	| lv = lvalue l_angle targs = typeArgumentList r_angle L_PAREN args = argumentList R_PAREN
		{ [ arg lv; langle (); arg targs; rangle (); lparen (); arg args; rparen () ]
      |> case_v ~var:"forInitStatement" }
	| lv = lvalue o = assignop e = expression
		{ [ arg lv; arg o; arg e ] |> case_v ~var:"forInitStatement" }
;

forInitStatementListNonEmpty:
	| s = forInitStatement { s }
	| sl = forInitStatementListNonEmpty COMMA s = forInitStatement
    { [ arg sl; op ","; arg s ] |> case_v ~var:"forInitStatementListNonEmpty" }
;

forInitStatementList:
	| (* empty *) { [ tag "EMPTY" ] |> case_v ~var:"forInitStatementList" }
	| sl = forInitStatementListNonEmpty { sl }
;

forUpdateStatement:
	| s = forInitStatement { s }
;

forUpdateStatementListNonEmpty:
	| s = forUpdateStatement { s }
	| sl = forUpdateStatementListNonEmpty COMMA s = forUpdateStatement
    { [ arg sl; op ","; arg s ] |> case_v ~var:"forUpdateStatementListNonEmpty" }
;

forUpdateStatementList:
	| (* empty *) { [ tag "EMPTY" ] |> case_v ~var:"forUpdateStatementList" }
	| sl = forUpdateStatementListNonEmpty { sl }
;

forCollectionExpression:
	| e = expression { e }
	| l = expression RANGE h = expression
    { [ arg l; op ".."; arg h ] |> case_v ~var:"forCollectionExpr" }
;

forStatement:
  | al = annotationList FOR L_PAREN il = forInitStatementList SEMICOLON c = expression SEMICOLON ul = forUpdateStatementList R_PAREN b = statement
		{ [ arg al; kw "FOR"; lparen (); arg il; op ";"; arg c; op ";"; arg ul; rparen (); arg b ]
      |> case_v ~var:"forStatement" }
  | al = annotationList FOR L_PAREN
    t = typeRef n = name IN e = forCollectionExpression R_PAREN b = statement
    { [ arg al; kw "FOR"; lparen (); arg t; arg n; kw "IN"; arg e; rparen (); arg b ]
      |> case_v ~var:"forStatement" }
  | al = annotationList FOR L_PAREN
    al_in = annotationList t = typeRef n = name IN e = forCollectionExpression R_PAREN b = statement
    { [ arg al; kw "FOR"; lparen (); arg al_in; arg t; arg n; kw "IN"; arg e; rparen (); arg b ]
      |> case_v ~var:"forStatement" }
;

(* >> Switch statements *)
switchLabel:
  | DEFAULT
    { [ kw "DEFAULT" ] |> case_v ~var:"switchLabel" }
  | e = expressionNonBrace
    { e }
;

switchCase:
  | l = switchLabel COLON s = blockStatement
    { [ arg l; op ":"; arg s ] |> case_v ~var:"switchCase" }
  | l = switchLabel COLON
    { [ arg l; op ":" ] |> case_v ~var:"switchCase" }
;

switchCaseList:
  | (* empty *)
    { [ tag "EMPTY" ] |> case_v ~var:"switchCaseList" }
  | cs = switchCaseList c = switchCase
    { [ arg cs; arg c ] |> case_v ~var:"switchCaseList" }
;

switchStatement:
  | SWITCH L_PAREN e = expression R_PAREN L_BRACE cs = switchCaseList R_BRACE
    { [ kw "SWITCH"; lparen (); arg e; rparen (); lbrace (); arg cs; rbrace () ]
      |> case_v ~var:"switchStatement" }

(* >> Break and continue statements *)
breakStatement:
  | BREAK SEMICOLON
    { [ kw "BREAK"; op ";" ] |> case_v ~var:"breakStatement" }
;

continueStatement:
  | CONTINUE SEMICOLON
    { [ kw "CONTINUE"; op ";" ] |> case_v ~var:"continueStatement" }
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
		{ [ op "="; arg e ] |> case_v ~var:"initializer" }
;

constantDeclaration:
  | al = annotationList CONST t = typeRef n = name i = initialValue SEMICOLON
    { [ arg al; kw "CONST"; arg t; arg n; arg i; op ";" ] |> case_v ~var:"constantDeclaration" }
;

initializerOpt:
	| (* empty *)
		{ [ tag "EMPTY" ] |> case_v ~var:"initializerOpt" }
	| i = initialValue { i }
;

variableDeclaration:
  | al = annotationList t = typeRef n = name i = initializerOpt SEMICOLON
    { declare_var_of_il n false;
      [ arg al; arg t; arg n; arg i; op ";" ] |> case_v ~var:"variableDeclaration" }
;

blockElementStatement:
  | d = constantDeclaration
  | d = variableDeclaration
  | d = statement
    { d }
;

blockElementStatementList:
  | (* empty *)
    { [ tag "EMPTY" ] |> case_v ~var:"blockElementStatementList" }
  | sl = blockElementStatementList s = blockElementStatement
    { [ arg sl; arg s ] |> case_v ~var:"blockElementStatementList" }
;

(* >> Function declarations *)
functionPrototype:
	| t = typeOrVoid n = name push_scope
  tpl = typeParameterListOpt
  L_PAREN pl = parameterList R_PAREN
    { [ arg t; arg n; arg tpl; lparen (); arg pl; rparen () ]
      |> case_v ~var:"functionPrototype" }
;

functionDeclaration:
	| al = annotationList p = functionPrototype b = blockStatement pop_scope
    { [ arg al; arg p; arg b ] |> case_v ~var:"functionDeclaration" }
;

(* >> Action declarations *)
actionDeclaration: 
  | al = annotationList ACTION n = name L_PAREN pl = parameterList R_PAREN s = blockStatement
    { [ arg al; kw "ACTION"; arg n; lparen (); arg pl; rparen (); arg s ]
      |> case_v ~var:"actionDeclaration" }
;

(* >> Instantiations *)
objectInitializer:
	| ASSIGN L_BRACE ds = objectDeclarationList R_BRACE
    { [ op "="; lbrace (); arg ds; rbrace () ] |> case_v ~var:"objectInitializer" }
;

instantiation:
	| al = annotationList t = typeRef L_PAREN args = argumentList R_PAREN n = name SEMICOLON
    { [ arg al; arg t; lparen (); arg args; rparen (); arg n; op ";" ]
      |> case_v ~var:"instantiation" }
	| al = annotationList t = typeRef L_PAREN args = argumentList R_PAREN n = name i = objectInitializer SEMICOLON
    { [ arg al; arg t; lparen (); arg args; rparen (); arg n; arg i; op ";" ]
      |> case_v ~var:"instantiation" }
;

objectDeclaration:
	| d = functionDeclaration
	| d = instantiation
    { d }
;

objectDeclarationList:
	| (* empty *) { [ tag "EMPTY" ] |> case_v ~var:"objectDeclarationList" }
	| ds = objectDeclarationList d = objectDeclaration
    { [ arg ds; arg d ] |> case_v ~var:"objectDeclarationList" }
;

(* >> Error declarations *)
errorDeclaration:
	| ERROR L_BRACE nl = nameList R_BRACE
    { declare_vars_of_il nl;
      [ kw "ERROR"; lbrace (); arg nl; rbrace () ] |> case_v ~var:"errorDeclaration" }
;

(* >> Match kind declarations *)
matchKindDeclaration:
	| MATCH_KIND L_BRACE nl = nameList c = trailingCommaOpt R_BRACE
    { declare_vars_of_il nl;
      [ kw "MATCH_KIND"; lbrace (); arg nl; arg c; rbrace () ] |> case_v ~var:"matchKindDeclaration" }
;

(* >> Derived type declarations *)
(* >>>> Enum type declarations *)
enumTypeDeclaration:
  | al = annotationList ENUM n = name L_BRACE
    nl = nameList c = trailingCommaOpt R_BRACE
    { [ arg al; kw "ENUM"; arg n; lbrace (); arg nl; arg c; rbrace () ]
      |> case_v ~var:"enumTypeDeclaration" }
  | al = annotationList ENUM t = typeRef n = name L_BRACE
    el = namedExpressionList c = trailingCommaOpt R_BRACE
    { [ arg al; kw "ENUM"; arg t; arg n; lbrace (); arg el; arg c; rbrace () ]
      |> case_v ~var:"enumTypeDeclaration" }
;

(* >>>>>> Struct, header, and union type declarations *)
typeField:
  | al = annotationList t = typeRef n = name SEMICOLON
    { [ arg al; arg t; arg n; op ";" ] |> case_v ~var:"typeField" }
;

typeFieldList:
  | (* empty *) { [ tag "EMPTY" ] |> case_v ~var:"typeFieldList" }
  | fl = typeFieldList f = typeField
    { [ arg fl; arg f ] |> case_v ~var:"typeFieldList" }
;

structTypeDeclaration:
  | al = annotationList STRUCT n = name tpl = typeParameterListOpt
      L_BRACE fl = typeFieldList R_BRACE
    { [ arg al; kw "STRUCT"; arg n; arg tpl; lbrace (); arg fl; rbrace () ]
      |> case_v ~var:"structTypeDeclaration" }
;

headerTypeDeclaration:
  | al = annotationList HEADER n = name tpl = typeParameterListOpt
      L_BRACE fl = typeFieldList R_BRACE
    { [ arg al; kw "HEADER"; arg n; arg tpl; lbrace (); arg fl; rbrace () ]
      |> case_v ~var:"headerTypeDeclaration" }
;

headerUnionTypeDeclaration:
  | al = annotationList HEADER_UNION n = name tpl = typeParameterListOpt
      L_BRACE fl = typeFieldList R_BRACE
    { [ arg al; kw "HEADER_UNION"; arg n; arg tpl; lbrace (); arg fl; rbrace () ]
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
    { [ arg al; kw "TYPEDEF"; arg t; arg n; op ";" ] |> case_v ~var:"typedefDeclaration" }
	| al = annotationList TYPE t = typeRef n = name SEMICOLON
    { [ arg al; kw "TYPE"; arg t; arg n; op ";" ] |> case_v ~var:"typedefDeclaration" }
;

(* >> Extern declarations *)
externFunctionDeclaration:
	| al = annotationList EXTERN p = functionPrototype pop_scope SEMICOLON
		{ let decl =
        [ arg al; kw "EXTERN"; arg p; op ";" ] |> case_v ~var:"externFunctionDeclaration"
      in
      declare_var (id_of_function_prototype p) (has_type_params_function_prototype p);
      decl }
;

methodPrototype:
	| al = annotationList tid = typeIdentifier L_PAREN pl = parameterList R_PAREN SEMICOLON
    { [ arg al; arg tid; lparen (); arg pl; rparen (); op ";" ] |> case_v ~var:"methodPrototype" }
	| al = annotationList p = functionPrototype pop_scope SEMICOLON
    { [ arg al; arg p; op ";" ] |> case_v ~var:"methodPrototype" }
	| al = annotationList ABSTRACT p = functionPrototype
    pop_scope SEMICOLON
    { [ arg al; kw "ABSTRACT"; arg p; op ";" ] |> case_v ~var:"methodPrototype" }
;

methodPrototypeList:
  | (* empty *) { [ tag "EMPTY" ] |> case_v ~var:"methodPrototypeList" }
  | ps = methodPrototypeList p = methodPrototype
    { [ arg ps; arg p ] |> case_v ~var:"methodPrototypeList" }
;

externObjectDeclaration:
  | al = annotationList EXTERN n = push_externName tpl = typeParameterListOpt
    L_BRACE pl = methodPrototypeList R_BRACE pop_scope
    { let decl =
        [ arg al; kw "EXTERN"; arg n; arg tpl; lbrace (); arg pl; rbrace () ]
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
    { [ arg k; op ":"; arg n; op ";" ] |> case_v ~var:"selectCase" }
;

selectCaseList:
  | (* empty *) { [ tag "EMPTY" ] |> case_v ~var:"selectCaseList" }
  | cl = selectCaseList c = selectCase
    { [ arg cl; arg c ] |> case_v ~var:"selectCaseList" }
;

selectExpression:
  | SELECT L_PAREN el = expressionList R_PAREN L_BRACE cl = selectCaseList R_BRACE
    { [ kw "SELECT"; lparen (); arg el; rparen (); lbrace (); arg cl; rbrace () ]
      |> case_v ~var:"selectExpression" }
;

(* >>>> Transition statements *)
stateExpression:
  | n = name SEMICOLON
    { [ arg n; op ";" ] |> case_v ~var:"stateExpression" }
  | e = selectExpression
    { e }
;

transitionStatement:
  | (* empty *) { [ tag "EMPTY" ] |> case_v ~var:"transitionStatement" }
  | TRANSITION e = stateExpression
    { [ kw "TRANSITION"; arg e ] |> case_v ~var:"transitionStatement" }
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
    { [ arg al; kw "VALUE_SET"; langle (); arg t; rangle (); lparen (); arg s; rparen (); arg n; op ";" ]
      |> case_v ~var:"valueSetDeclaration" }
;

(* >>>> Parser type declarations *)
parserTypeDeclaration:
  | al = annotationList PARSER n = push_name tpl = typeParameterListOpt
      L_PAREN pl = parameterList R_PAREN pop_scope SEMICOLON
    { [ arg al; kw "PARSER"; arg n; arg tpl; lparen (); arg pl; rparen (); op ";" ]
      |> case_v ~var:"parserTypeDeclaration" }
;

(* >>>> Parser declarations *)
parserBlockStatement:
  | al = annotationList L_BRACE sl = parserStatementList R_BRACE
    { [ arg al; lbrace (); arg sl; rbrace () ] |> case_v ~var:"parserBlockStatement" }
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
  | (* empty *) { [ tag "EMPTY" ] |> case_v ~var:"parserStatementList" }
  | sl = parserStatementList s = parserStatement
    { [ arg sl; arg s ] |> case_v ~var:"parserStatementList" }
;

parserState:
  | al = annotationList STATE n = push_name L_BRACE sl = parserStatementList t = transitionStatement R_BRACE
    { [ arg al; kw "STATE"; arg n; lbrace (); arg sl; arg t; rbrace () ]
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
  | (* empty *) { [ tag "EMPTY" ] |> case_v ~var:"parserLocalDeclarationList" }
  | dl = parserLocalDeclarationList d = parserLocalDeclaration
    { [ arg dl; arg d ] |> case_v ~var:"parserLocalDeclarationList" }
;

parserDeclaration:
  | al = annotationList PARSER n = push_name tpl = typeParameterListOpt
    L_PAREN pl = parameterList R_PAREN cpl = constructorParameterListOpt
    L_BRACE dl = parserLocalDeclarationList sl = parserStateList R_BRACE pop_scope
		{ [ arg al; kw "PARSER"; arg n; arg tpl; lparen (); arg pl; rparen (); arg cpl;
      lbrace (); arg dl; arg sl; rbrace () ] |> case_v ~var:"parserDeclaration" }
;

(* >> Control statements and declarations *)
(* >>>> Table declarations *)
constOpt:
  | (* empty *) { [ tag "EMPTY" ] |> case_v ~var:"constOpt" }
  | CONST { [ kw "CONST" ] |> case_v ~var:"constOpt" }
;

(* >>>>>> Table key property *)
tableKey:
  | e = expression COLON n = name al = annotationList SEMICOLON
    { [ arg e; op ":"; arg n; arg al; op ";" ] |> case_v ~var:"tableKey" }
;

tableKeyList:
  | (* empty *) { [ tag "EMPTY" ] |> case_v ~var:"tableKeyList" }
  | kl = tableKeyList k = tableKey
    { [ arg kl; arg k ] |> case_v ~var:"tableKeyList" }
;

(* >>>>>> Table actions property *)
tableActionReference:
  | n = prefixedNonTypeName
    { n }
  | n = prefixedNonTypeName L_PAREN al = argumentList R_PAREN
    { [ arg n; lparen (); arg al; rparen () ] |> case_v ~var:"tableActionReference" }
;

tableAction:
  | al = annotationList ac = tableActionReference SEMICOLON
    { [ arg al; arg ac; op ";" ] |> case_v ~var:"tableAction" }
;

tableActionList:
  | (* empty *) { [ tag "EMPTY" ] |> case_v ~var:"tableActionList" }
  | acl = tableActionList ac = tableAction
    { [ arg acl; arg ac ] |> case_v ~var:"tableActionList" }
;

(* >>>>>> Table entry property *)
tableEntryPriority:
  | PRIORITY ASSIGN num = number COLON
    { [ kw "PRIORITY"; op "="; arg num; op ":" ] |> case_v ~var:"tableEntryPriority" }
  | PRIORITY ASSIGN L_PAREN e = expression R_PAREN COLON
    { [ kw "PRIORITY"; op "="; lparen (); arg e; rparen (); op ":" ] |> case_v ~var:"tableEntryPriority" }
;

tableEntry:
  | c = constOpt p = tableEntryPriority k = keysetExpression COLON ac = tableActionReference al = annotationList SEMICOLON
    { [ arg c; arg p; arg k; op ":"; arg ac; arg al; op ";" ] |> case_v ~var:"tableEntry" }
  | c = constOpt k = keysetExpression COLON ac = tableActionReference al = annotationList SEMICOLON
    { [ arg c; arg k; op ":"; arg ac; arg al; op ";" ] |> case_v ~var:"tableEntry" }
;

tableEntryList:
  | (* empty *) { [ tag "EMPTY" ] |> case_v ~var:"tableEntryList" }
  | el = tableEntryList e = tableEntry
    { [ arg el; arg e ] |> case_v ~var:"tableEntryList" }
;

(* >>>>>> Table properties *)
tableProperty:
  | KEY ASSIGN L_BRACE kl = tableKeyList R_BRACE
    { [ kw "KEY"; op "="; lbrace (); arg kl; rbrace () ] |> case_v ~var:"tableProperty" }
  | ACTIONS ASSIGN L_BRACE acl = tableActionList R_BRACE
    { [ kw "ACTIONS"; op "="; lbrace (); arg acl; rbrace () ] |> case_v ~var:"tableProperty" }
  | al = annotationList c = constOpt ENTRIES ASSIGN L_BRACE el = tableEntryList R_BRACE
    { [ arg al; arg c; kw "ENTRIES"; op "="; lbrace (); arg el; rbrace () ] |> case_v ~var:"tableProperty" }
  | al = annotationList c = constOpt n = tableCustomName i = initialValue SEMICOLON
    { [ arg al; arg c; arg n; arg i; op ";" ] |> case_v ~var:"tableProperty" }
;

tablePropertyList:
  | (* empty *) { [ tag "EMPTY" ] |> case_v ~var:"tablePropertyList" }
  | pl = tablePropertyList p = tableProperty
    { [ arg pl; arg p ] |> case_v ~var:"tablePropertyList" }
;

tableDeclaration:
  | al = annotationList TABLE n = name L_BRACE pl = tablePropertyList R_BRACE
    { [ arg al; kw "TABLE"; arg n; lbrace (); arg pl; rbrace () ] |> case_v ~var:"tableDeclaration" }

(* >>>> Control type declarations *)
controlTypeDeclaration:
  | al = annotationList CONTROL n = push_name tpl = typeParameterListOpt
    L_PAREN pl = parameterList R_PAREN pop_scope SEMICOLON
    { [ arg al; kw "CONTROL"; arg n; arg tpl; lparen (); arg pl; rparen (); op ";" ]
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
  | (* empty *) { [ tag "EMPTY" ] |> case_v ~var:"controlLocalDeclarationList" }
  | dl = controlLocalDeclarationList d = controlLocalDeclaration
    { [ arg dl; arg d ] |> case_v ~var:"controlLocalDeclarationList" }
;

controlDeclaration:
  | al = annotationList CONTROL n = push_name tpl = typeParameterListOpt
    L_PAREN pl = parameterList R_PAREN cpl = constructorParameterListOpt
    L_BRACE dl = controlLocalDeclarationList APPLY b = controlBody R_BRACE pop_scope
    { [ arg al; kw "CONTROL"; arg n; arg tpl; lparen (); arg pl; rparen (); arg cpl;
      lbrace (); arg dl; kw "APPLY"; arg b; rbrace () ] |> case_v ~var:"controlDeclaration" }
;

(* >> Package type declarations *)
packageTypeDeclaration:
  | al = annotationList PACKAGE n = push_name tpl = typeParameterListOpt
    L_PAREN pl = parameterList R_PAREN pop_scope SEMICOLON
    { [ arg al; kw "PACKAGE"; arg n; arg tpl; lparen (); arg pl; rparen (); op ";" ]
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
    { [ kw "UNEXPECTED_TOKEN" ] |> case_v ~var:"annotationToken" }
	| ABSTRACT
    { [ kw "ABSTRACT" ] |> case_v ~var:"annotationToken" }
	| ACTION
    { [ kw "ACTION" ] |> case_v ~var:"annotationToken" }
	| ACTIONS
    { [ kw "ACTIONS" ] |> case_v ~var:"annotationToken" }
	| APPLY
    { [ kw "APPLY" ] |> case_v ~var:"annotationToken" }
	| BOOL
    { [ kw "BOOL" ] |> case_v ~var:"annotationToken" }
	| BIT
    { [ kw "BIT" ] |> case_v ~var:"annotationToken" }
	| BREAK
    { [ kw "BREAK" ] |> case_v ~var:"annotationToken" }
	| CONST
    { [ kw "CONST" ] |> case_v ~var:"annotationToken" }
	| CONTINUE
    { [ kw "CONTINUE" ] |> case_v ~var:"annotationToken" }
	| CONTROL
    { [ kw "CONTROL" ] |> case_v ~var:"annotationToken" }
	| DEFAULT
    { [ kw "DEFAULT" ] |> case_v ~var:"annotationToken" }
	| ELSE
    { [ kw "ELSE" ] |> case_v ~var:"annotationToken" }
	| ENTRIES
    { [ kw "ENTRIES" ] |> case_v ~var:"annotationToken" }
	| ENUM
    { [ kw "ENUM" ] |> case_v ~var:"annotationToken" }
	| ERROR
    { [ kw "ERROR" ] |> case_v ~var:"annotationToken" }
	| EXIT
    { [ kw "EXIT" ] |> case_v ~var:"annotationToken" }
	| EXTERN
    { [ kw "EXTERN" ] |> case_v ~var:"annotationToken" }
	| FALSE
    { [ kw "FALSE" ] |> case_v ~var:"annotationToken" }
	| FOR
    { [ kw "FOR" ] |> case_v ~var:"annotationToken" }
	| HEADER
    { [ kw "HEADER" ] |> case_v ~var:"annotationToken" }
	| HEADER_UNION
    { [ kw "HEADER_UNION" ] |> case_v ~var:"annotationToken" }
	| IF
    { [ kw "IF" ] |> case_v ~var:"annotationToken" }
	| IN
    { [ kw "IN" ] |> case_v ~var:"annotationToken" }
	| INOUT
    { [ kw "INOUT" ] |> case_v ~var:"annotationToken" }
	| INT
    { [ kw "INT" ] |> case_v ~var:"annotationToken" }
	| KEY
    { [ kw "KEY" ] |> case_v ~var:"annotationToken" }
	| MATCH_KIND
    { [ kw "MATCH_KIND" ] |> case_v ~var:"annotationToken" }
	| TYPE
    { [ kw "TYPE" ] |> case_v ~var:"annotationToken" }
	| OUT
    { [ kw "OUT" ] |> case_v ~var:"annotationToken" }
	| PARSER
    { [ kw "PARSER" ] |> case_v ~var:"annotationToken" }
	| PACKAGE
    { [ kw "PACKAGE" ] |> case_v ~var:"annotationToken" }
	| PRAGMA
    { [ kw "PRAGMA" ] |> case_v ~var:"annotationToken" }
	| RETURN
    { [ kw "RETURN" ] |> case_v ~var:"annotationToken" }
	| SELECT
    { [ kw "SELECT" ] |> case_v ~var:"annotationToken" }
	| STATE
    { [ kw "STATE" ] |> case_v ~var:"annotationToken" }
	| STRING
    { [ kw "STRING" ] |> case_v ~var:"annotationToken" }
	| STRUCT
    { [ kw "STRUCT" ] |> case_v ~var:"annotationToken" }
	| SWITCH
    { [ kw "SWITCH" ] |> case_v ~var:"annotationToken" }
	| TABLE
    { [ kw "TABLE" ] |> case_v ~var:"annotationToken" }
	| THIS
    { [ kw "THIS" ] |> case_v ~var:"annotationToken" }
	| TRANSITION
    { [ kw "TRANSITION" ] |> case_v ~var:"annotationToken" }
	| TRUE
    { [ kw "TRUE" ] |> case_v ~var:"annotationToken" }
	| TUPLE
    { [ kw "TUPLE" ] |> case_v ~var:"annotationToken" }
	| TYPEDEF
    { [ kw "TYPEDEF" ] |> case_v ~var:"annotationToken" }
	| VARBIT
    { [ kw "VARBIT" ] |> case_v ~var:"annotationToken" }
	| VALUE_SET
    { [ kw "VALUE_SET" ] |> case_v ~var:"annotationToken" }
	| LIST
    { [ kw "LIST" ] |> case_v ~var:"annotationToken" }
	| VOID
    { [ kw "VOID" ] |> case_v ~var:"annotationToken" }
	| DONTCARE
    { [ op "_" ] |> case_v ~var:"annotationToken" }
	| id = identifier
    { id }
	| tid = typeIdentifier
    { tid }
	| str = stringLiteral
    { str }
	| num = number
    { num }
	| MASK
    { [ op "&&&" ] |> case_v ~var:"annotationToken" }
  (* TODO: missing DOTS "..." in spec *)
	| RANGE
    { [ op ".." ] |> case_v ~var:"annotationToken" }
	| SHL
    { [ op "<<" ] |> case_v ~var:"annotationToken" }
	| AND
    { [ op "&&" ] |> case_v ~var:"annotationToken" }
	| OR
    { [ op "||" ] |> case_v ~var:"annotationToken" }
	| EQ
    { [ op "==" ] |> case_v ~var:"annotationToken" }
	| NE
    { [ op "!=" ] |> case_v ~var:"annotationToken" }
	| GE
    { [ op ">=" ] |> case_v ~var:"annotationToken" }
	| LE
    { [ op "<=" ] |> case_v ~var:"annotationToken" }
	| PLUSPLUS
    { [ op "++" ] |> case_v ~var:"annotationToken" }
	| PLUS
    { [ op "+" ] |> case_v ~var:"annotationToken" }
	| PLUS_SAT
    { [ op "|+|" ] |> case_v ~var:"annotationToken" }
	| MINUS
    { [ op "-" ] |> case_v ~var:"annotationToken" }
	| MINUS_SAT
    { [ op "|-|" ] |> case_v ~var:"annotationToken" }
	| MUL
    { [ op "*" ] |> case_v ~var:"annotationToken" }
	| DIV
    { [ op "/" ] |> case_v ~var:"annotationToken" }
	| MOD
    { [ op "%" ] |> case_v ~var:"annotationToken" }
	| BIT_OR
    { [ op "|" ] |> case_v ~var:"annotationToken" }
	| BIT_AND
    { [ op "&" ] |> case_v ~var:"annotationToken" }
	| BIT_XOR
    { [ op "^" ] |> case_v ~var:"annotationToken" }
	| COMPLEMENT
    { [ op "~" ] |> case_v ~var:"annotationToken" }
	| L_BRACKET
    { [ op "[" ] |> case_v ~var:"annotationToken" }
	| R_BRACKET
    { [ op "]" ] |> case_v ~var:"annotationToken" }
	| L_BRACE
    { [ op "{" ] |> case_v ~var:"annotationToken" }
	| R_BRACE
    { [ op "}" ] |> case_v ~var:"annotationToken" }
	| L_ANGLE
    { [ op "<" ] |> case_v ~var:"annotationToken" }
	| R_ANGLE
    { [ op ">" ] |> case_v ~var:"annotationToken" }
	| NOT
    { [ op "!" ] |> case_v ~var:"annotationToken" }
	| COLON
    { [ op ":" ] |> case_v ~var:"annotationToken" }
	| COMMA
    { [ op "," ] |> case_v ~var:"annotationToken" }
	| QUESTION
    { [ op "?" ] |> case_v ~var:"annotationToken" }
	| DOT
    { [ op "." ] |> case_v ~var:"annotationToken" }
	| ASSIGN
    { [ op "=" ] |> case_v ~var:"annotationToken" }
	| SEMICOLON
    { [ op ";" ] |> case_v ~var:"annotationToken" }
	| AT
    { [ op "@" ] |> case_v ~var:"annotationToken" }
;

annotationBody:
	| (* empty *) { [ tag "EMPTY" ] |> case_v ~var:"annotationBody" }
	| ab = annotationBody L_PAREN ab_in = annotationBody R_PAREN
    { [ arg ab; lparen (); arg ab_in; rparen () ] |> case_v ~var:"annotationBody" }
	| ab = annotationBody at = annotationToken
    { [ arg ab; arg at ] |> case_v ~var:"annotationBody" }
;

structuredAnnotationBody:
	| e = dataElementExpression c = trailingCommaOpt
    { [ arg e; arg c ] |> case_v ~var:"structuredAnnotationBody" }
;

annotation:
	| AT name = name
    { [ op "@"; arg name ] |> case_v ~var:"annotation" }
	| AT name = name L_PAREN body = annotationBody R_PAREN
    { [ op "@"; arg name; lparen (); arg body; rparen () ] |> case_v ~var:"annotation" }
	| AT name = name L_BRACKET body = structuredAnnotationBody R_BRACKET
    { [ op "@"; arg name; lbrack (); arg body; rbrack () ] |> case_v ~var:"annotation" }
(* From Petr4: PRAGMA not in Spec, but in Petr4/p4c *)
	| PRAGMA name = name body = annotationBody PRAGMA_END
    { [ op "@"; kw "PRAGMA"; arg name; arg body ] |> case_v ~var:"annotation" }
;

annotationListNonEmpty:
	| a = annotation { a }
	| al = annotationListNonEmpty a = annotation
		{ [ arg al; arg a ] |> case_v ~var:"annotationListNonEmpty" }
;

%inline annotationList:
	| (* empty *) { [ tag "EMPTY" ] |> case_v ~var:"annotationList" }
	| al = annotationListNonEmpty { al }
;

(******** P4 program ********)
declarationList:
  | (* empty *) { [ tag "EMPTY" ] |> case_v ~var:"p4program" }
  | ds = declarationList d = declaration
    { [ arg ds; arg d ] |> case_v ~var:"p4program" }
  | ds = declarationList SEMICOLON
    { [ arg ds; op ";" ] |> case_v ~var:"p4program" }
;

p4program:
	| ds = declarationList END { ds }
