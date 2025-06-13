%{
  open Il.Ast
  open Xl.Atom
  open Util.Source
  open Context

  let wrap_atom (s : string) : atom = Atom s $ no_region
  let wrap_var_t (s : string) : typ' = VarT (s $ no_region, [])
  let wrap_iter_t (i : iter) (t: typ') : typ' = IterT (t $ no_region, i)
  
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

  let with_fresh_val (typ: typ') : vnote = 
    let vid = Value.fresh () in
    { vid; typ }
  let with_typ (typ: typ') (v: value') : value =
    v $$$ with_fresh_val typ

  let name_of_declaration (_d: value) : string =
    (* TODO: fetch name from CaseV fields *)
    "temp_name"

  let has_typ_params_declaration (_d: value) : bool =
    (* TODO: check for type parameters in CaseV *)
    false
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
%token<Source.info> ABSTRACT ACTION ACTIONS APPLY BOOL BIT CONST CONTROL DEFAULT DEFAULT_ACTION
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

(* push_name: *)
(* | n = name *)
(*    { push_scope(); *)
(*      declare_type n false; *)
(*      n } *)
(**)
(* push_externName: *)
(* | n = externName *)
(*     { push_scope(); *)
(*       declare_type n false; *)
(*       n } *)
(**)
(* pop_scope: *)
(* | (* empty *) *)
(*     { pop_scope() } *)
(* ; *)

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

(******** Misc ********)
(** TODO: are these necessary? **)
(* trailingComma: *)
(* optTrailingComma: *)
const:
| CONST
    { Term "CONST" |> wrap_case_v |> with_typ (wrap_var_t "const") }
;

(* TODO: optCONST: *)

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
      [ Term "LIST" ] |> wrap_case_v |> with_typ (wrap_var_t "name") }
| typeIdentifier = typeIdentifier
    { [ NT typeIdentifier ] |> wrap_case_v |> with_typ (wrap_var_t "name") }
;

(* TODO: ListV *)
identifierList:
| ids = separated_nonempty_list(COMMA, id = name { id })
    { [ NT ids ] |> wrap_case_v |> with_typ (wrap_var_t "identifierList") }
;
member:
| name = name
    { [ NT name ] |> wrap_case_v |> with_typ (wrap_var_t "member") }
;
(******** Directions ********)
direction:
| IN
    { [ Term "IN" ] |> wrap_case_v |> with_typ (wrap_var_t "direction") }
| OUT
    { [ Term "OUT" ] |> wrap_case_v |> with_typ (wrap_var_t "direction") }
| INOUT
    { [ Term "INOUT" ] |> wrap_case_v |> with_typ (wrap_var_t "direction") }
| NONE
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
(* TODO *)
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
    { [ NT t ] |> wrap_case_v |> with_typ (wrap_var_t "typeRef") }
;

typeOrVoid:
| typeRef = typeRef
    { [ NT typeRef ] |> wrap_case_v |> with_typ (wrap_var_t "typeOrVoid") }
| info = VOID
    { info |> ignore;
      [ Term "VOID" ] |> wrap_case_v |> with_typ (wrap_var_t "typeOrVoid") }
;
(* Petr4 O / Spec X *)
(* | name = varName *)
(*   { let tags: P4util.Source.info = Text.tags name in *)
(*     Type.TypeName { tags; name = BareName name } } *)

(******** Type parameters ********)

(* TODO: listify *)
typeParameterList:
| names = separated_nonempty_list(COMMA, name)
;
typeParameters:
| info_l = l_angle typeParameterList info_r = r_angle
    { let tags = Source.merge info_l info_r in
      tags |> ignore;
      [ Term "<"; NT typeParameterList; Term ">" ]
      |> wrap_case_v 
      |> with_typ (wrap_var_t "typeParameters") }
;
(* TODO: optTypeParameters *)

(******** Parameters ********)

parameter:
| optAnnotations = optAnnotations directon = directon
    typeRef = typeRef name = name
    { 
      [ NT optAnnotations; NT directon; NT typeRef; NT name ]
      |> wrap_case_v 
      |> with_typ (wrap_var_t "parameter")
    }
  (* TODO: ASSIGN terminal in grammar? *)
| optAnnotations = optAnnotations directon = directon
    typeRef = typeRef name = name init = initialValue
    { 
      [ NT optAnnotations; NT directon; NT typeRef; NT name; NT init ]
      |> wrap_case_v 
      |> with_typ (wrap_var_t "parameter")
    }
;

parameterList:
| params = separated_list(COMMA, parameter)
    { declare_vars (List.map name_of_declaration params);
      [ NT params ] |> wrap_case_v |> with_typ (wrap_var_t "parameterList") }
;

constructorParameters:
| L_PAREN parameterList = parameterList R_PAREN
    { [ Term "("; NT parameterList; Term ")" ] |> wrap_case_v |> with_typ (wrap_var_t "constructorParameters") }
;
(* TODO: optional *)
(* optConstructorParameters: *)

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
    { info |> ignore;
      [ NT bits; Term "["; NT hi; Term ":"; NT lo; Term "]" ]
    |> wrap_case_v |> with_typ (wrap_var_t "nonBraceExpression") }
| L_PAREN exp = expression R_PAREN
    { exp }
| info1 = NOT arg = expression %prec PREFIX
    { info1 |> ignore;
      [ Term "!", NT arg ] |> wrap_case_v |> with_typ (wrap_var_t "nonBraceExpression") }
| info1 = COMPLEMENT arg = expression %prec PREFIX
    { info1 |> ignore;
      [ Term "~", NT arg ] |> wrap_case_v |> with_typ (wrap_var_t "nonBraceExpression") }
| info1 = MINUS arg = expression %prec PREFIX
    { info1 |> ignore;
      [ Term "-", NT arg ] |> wrap_case_v |> with_typ (wrap_var_t "nonBraceExpression") }
| info1 = PLUS arg = expression %prec PREFIX
    { info1 |> ignore;
      [ Term "+", NT arg ] |> wrap_case_v |> with_typ (wrap_var_t "nonBraceExpression") }
| typ = prefixedTypeName DOT member = member
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
      [ NT typ; Term "("; NT args; Term ")"; PHTM_6 ]
      |> wrap_case_v |> with_typ (wrap_var_t "nonBraceExpression") }
| info1 = L_PAREN typ = typeRef R_PAREN expr = expression %prec PREFIX
    { info1 |> ignore;
      [ Term "("; NT typ; Term ")"; NT expr ]
      |> wrap_case_v |> with_typ (wrap_var_t "nonBraceExpression") }
;

%inline binop:
| info = MUL { info |> ignore; "*" }
| info = DIV
    { Op.Div { tags = info } }
| info = MOD
    { Op.Mod { tags = info } }
| info = PLUS
    { Op.Plus { tags = info } }
| info = PLUS_SAT
    { Op.PlusSat { tags = info }}
| info = MINUS
    { Op.Minus { tags = info } }
| info = MINUS_SAT
    { Op.MinusSat { tags = info } }
| info = SHL
    { Op.Shl { tags = info } }
| info_r = r_angle info2 = R_ANGLE_SHIFT
    { let tags = Source.merge info_r info2 in
      Op.Shr { tags } }
| info = LE
    { Op.Le { tags = info } }
| info = GE
    { Op.Ge { tags = info } }
| info = l_angle
    { Op.Lt { tags = info } }
| info_r = r_angle
    { Op.Gt { tags = info_r } }
| info = NE
    { Op.NotEq { tags = info } }
| info = EQ
    { Op.Eq { tags = info } }
| info = BIT_AND
    { Op.BitAnd { tags = info } }
| info = BIT_XOR
    { Op.BitXor { tags = info } }
| info = BIT_OR
    { Op.BitOr { tags = info } }
| info = PLUSPLUS
    { Op.PlusPlus { tags = info } }
| info = AND
    { Op.And { tags = info } }
| info = OR
    { Op.Or { tags = info } }
;

(* TODO: convert *)
expression:
| value = NUMBER
    { let value_int = fst value in 
      let tags = Number.tags value_int in 
      Expression.Int { tags; i = value_int } }
| info1 = DOTS
    { Expression.Dots { tags = info1 } }
| info1 = TRUE
    { Expression.True { tags = info1 } }
| info1 = FALSE
    { Expression.False { tags = info1 } }
| value = STRING_LITERAL
    { let tags = Text.tags value in
      Expression.String { tags; text = value } }
| info1 = THIS
    { let name = Text.{ tags = info1; str = "this" } in
      let tags = Text.tags name in
      Expression.Name { tags; name = BareName name } }
| name = nonTypeName
    { let tags = Text.tags name in
      Expression.Name { tags; name = BareName name } }
| info1 = dotPrefix go_toplevel name = nonTypeName go_local
    { let tags = Source.merge info1 (Text.tags name) in
      Expression.Name { tags; name = QualifiedName ([], name) } }
| array = expression L_BRACKET index = expression info2 = R_BRACKET
    { let tags = Source.merge (Expression.tags array) info2 in
      Expression.ArrayAccess { tags; array; index } }
| bits = expression L_BRACKET hi = expression COLON lo = expression info2 = R_BRACKET
    { let tags = Source.merge (Expression.tags bits) info2 in
      Expression.BitStringAccess { tags; bits; lo; hi } }
(* (HACK) To syntactically disallow dots in the middle of a tuple expression *)
| info1 = L_BRACE values = expressionOptTrailingList info2 = R_BRACE
    { let tags = Source.merge info1 info2 in
      let is_dots expr = match expr with Expression.Dots _ -> true | _ -> false in
      if List.length values = 0 then Expression.List { tags; values }
      else
        let values, value_last =
          List.rev values |> List.tl |> List.rev, List.rev values |> List.hd
        in
        if List.exists is_dots values then
          raise Parsing.Parse_error;
        if is_dots value_last then Expression.ListDots { tags; values }
        else Expression.List { tags; values = values @ [value_last] } }
| info = INVALID
    { Expression.Invalid { tags = info } }
| info1 = L_BRACE entries = kvOptTrailingList info2 = R_BRACE 
    { let tags = Source.merge info1 info2 in 
      Expression.Record { tags; entries } }
| info1 = L_BRACE entries = kvTrailingList DOTS option(COMMA) info2 = R_BRACE
    { let tags = Source.merge info1 info2 in 
      Expression.RecordDots { tags; entries } }
| L_PAREN exp = expression R_PAREN
    { exp }
| info1 = NOT arg = expression %prec PREFIX
    { let tags = Source.merge info1 (Expression.tags arg) in
      Expression.UnaryOp { tags; op = Op.Not {tags = info1}; arg } }
| info1 = COMPLEMENT arg = expression %prec PREFIX
    { let tags = Source.merge info1 (Expression.tags arg) in
    Expression.UnaryOp { tags; op = Op.BitNot {tags = info1}; arg } }
| info1 = MINUS arg = expression %prec PREFIX
    { let tags = Source.merge info1 (Expression.tags arg) in
      Expression.UnaryOp { tags; op = UMinus {tags = info1}; arg } }
| info1 = PLUS arg = expression %prec PREFIX
    { let tags = Source.merge info1 (Expression.tags arg) in
      Expression.UnaryOp { tags; op = UPlus {tags = info1}; arg } }
| info1 = L_PAREN typ = typeRef R_PAREN expr = expression %prec PREFIX
    { let tags = Source.merge info1 (Expression.tags expr) in
      Expression.Cast { tags; typ; expr } }
| typ = prefixedTypeName DOT name = member
    { let tags = Text.tags name in
      Expression.TypeMember { tags; typ; name } }
| info1 = ERROR DOT name = member
    { let tags = Source.merge info1 (Text.tags name) in
      Expression.ErrorMember { tags; err = name } }
| expr = expression DOT name = member
    { let tags = Source.merge (Expression.tags expr) (Text.tags name) in
      Expression.ExpressionMember { tags; expr; name } }
| arg1 = expression op = binop arg2 = expression
    { let tags = Source.merge (Expression.tags arg1) (Expression.tags arg2) in
      Expression.BinaryOp { tags; op; args = (arg1, arg2) } }
| cond = expression QUESTION tru = expression COLON fls = expression
    { let tags = Source.merge (Expression.tags cond) (Expression.tags fls) in
      Expression.Ternary { tags; cond; tru; fls } }
| func = expression l_angle type_args = realTypeArgumentList r_angle
  L_PAREN args = argumentList info2 = R_PAREN
    { let tags = Source.merge (Expression.tags func) info2 in
      Expression.FunctionCall { tags; func; type_args; args } }
| func = expression L_PAREN args = argumentList info2 = R_PAREN
    { let type_args = [] in
      let tags = Source.merge (Expression.tags func) info2 in
      Expression.FunctionCall { tags; func; type_args; args } }
| typ = namedType L_PAREN args = argumentList info2 = R_PAREN
    { let tags = Source.merge (Type.tags typ) info2 in
      Expression.NamelessInstantiation { tags; typ; args } }
;

(* TODO: convert *)
expressionList:
| exprs = separated_list(COMMA, expression)
    { exprs }
;

(* TODO: convert *)
simpleKeysetExpression:
| expr = expression
    { let tags = Expression.tags expr in
      Match.Expression { tags; expr } }
| info = DONTCARE
    { Match.DontCare { tags = info } }
| info = DEFAULT
    { Match.Default { tags = info } }
| expr = expression MASK mask = expression
    { let tags = Source.merge (Expression.tags expr) (Expression.tags mask) in
      Match.Expression { tags; expr = Expression.Mask { tags; expr; mask } } }
| lo = expression RANGE hi = expression
    { let tags = Source.merge (Expression.tags lo) (Expression.tags hi) in
      Match.Expression { tags; expr = Expression.Range { tags; lo; hi } } }
;

(* TODO: convert *)
reducedSimpleKeysetExpression:
| info = DONTCARE
    { Match.DontCare { tags = info } }
| info = DEFAULT
    { Match.Default { tags = info } }
| expr = expression MASK mask = expression
    { let tags = Source.merge (Expression.tags expr) (Expression.tags mask) in
      Match.Expression { tags; expr = Expression.Mask { tags; expr; mask } } }
| lo = expression RANGE hi = expression
    { let tags = Source.merge (Expression.tags lo) (Expression.tags hi) in
      Match.Expression { tags; expr = Expression.Range { tags; lo; hi } } }
;

(* TODO: convert *)
(* simpleExressionList: *)

(* TODO: convert *)
tupleKeysetExpression:
| L_PAREN exprs = separated_atLeastTwo_list(COMMA, simpleKeysetExpression) R_PAREN
    { exprs }
| L_PAREN expr = reducedSimpleKeysetExpression R_PAREN
    { [ expr ] }
;

(* TODO: convert *)
keysetExpression:
| exprs = tupleKeysetExpression
    { exprs }
| expr  = simpleKeysetExpression
    { [ expr ] }

(* TODO: convert *)
%inline kvPair:
| key = name ASSIGN value = expression 
    { let tags = Source.merge (Text.tags key) (Expression.tags value) in
      KeyValue.{ tags; key; value } }


(* kvTrailingList: *)
(* | kvs = separated_nonempty_trailing_list(COMMA, kvPair) *)
(*     { kvs } *)
(* ; *)
(**)
(* kvOptTrailingList: *)
(* | kvs = separated_nonempty_opt_trailing_list(COMMA, kvPair) *)
(*     { kvs } *)
(* ; *)

(******** Type arguments ********)

realTypeArg:
| typeRef = typeRef
    { [ NT typeRef ] |> wrap_case_v |> with_typ (wrap_var_t "realTypeArg") }
| info = VOID
    { info |> ignore;
      [ Term "VOID" ] |> wrap_case_v |> with_typ (wrap_var_t "realTypeArg") }
| info = DONTCARE
    { info |> ignore;
      [ Term "_" ] |> wrap_case_v |> with_typ (wrap_var_t "realTypeArg") }
;

(* TODO: is this special case necessary? *)
realTypeArgumentList:
| t = realTypeArg
    { [ NT t ] |> wrap_case_v |> with_typ (wrap_var_t "realTypeArgumentList") }
| t = realTypeArg COMMA ts = separated_list(COMMA, typeArg)
    { [ NT t; Term ","; NT ts ] |> wrap_case_v |> with_typ (wrap_var_t "realTypeArgumentList") }
;

typeArg:
| typeRef = typeRef
    { [ NT typeRef ] |> wrap_case_v |> with_typ (wrap_var_t "typeArg") }
| nonTypeName = nonTypeName
    { [ NT nonTypeName ] |> wrap_case_v |> with_typ (wrap_var_t "typeArg") }
| info = VOID
    { info |> ignore;
      [ Term "VOID" ] |> wrap_case_v |> with_typ (wrap_var_t "typeArg") }
| info = DONTCARE
    { info |> ignore;
      [ Term "_" ] |> wrap_case_v |> with_typ (wrap_var_t "typeArg") }
;

typeArgumentList:
| ts = separated_list(COMMA, typeArg)
    { [ NT ts ] |> wrap_case_v |> with_typ (wrap_var_t "typeArgumentList") }
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
  args = separated_list(COMMA, argument)
    { [ NT args ] |> wrap_case_v |> with_typ (wrap_var_t "argumentList") }
;
(******** L-values ********)
(** TODO **)

lvalue:
| prefixedNonTypeName = prefixedNonTypeName
    { [ NT prefixedNonTypeName ] |> wrap_case_v |> with_typ (wrap_var_t "lvalue") }
(* TODO: convert *)
| info1 = THIS
    { let name = Text.{ tags = info1; str = "this" } in
      let tags = Text.tags name in
      Expression.Name { tags; name = BareName name } }
| expr = lvalue DOT name = member
    { let tags = Source.merge (Expression.tags expr) (Text.tags name) in
      Expression.ExpressionMember { tags; expr; name } }
| array = lvalue L_BRACKET index = expression info2 = R_BRACKET
    { let tags = Source.merge (Expression.tags array) info2 in
      Expression.ArrayAccess { tags; array; index } }
| bits = lvalue L_BRACKET hi = expression COLON lo = expression info2 = R_BRACKET
    { let tags = Source.merge (Expression.tags bits) info2 in
      Expression.BitStringAccess { tags; bits; lo; hi } }
| L_PAREN expr = lvalue R_PAREN
    { expr }
;

(******** Variable and constant declarations ********)

(* initializer -> initialValue due to reserved word in OCaml *)
initialValue:
| value = ASSIGN expression
    { [ Term "="; NT value ] |> wrap_case_v |> with_typ (wrap_var_t "initializer") }
;

optInitializer: 
  | opt = option(initialValue) { opt }
;
(* Pert4 X / Spec O *)
variableDeclarationWithoutSemicolon:
| optAnnotations = optAnnotations
  typeRef = typeRef
  name = name
  optInitializer = optInitializer
    { declare_var name false;
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
      [ NT optAnnotations; Term "const"; NT typeRef; NT name; NT init; Term ";" ] |> wrap_case_v |> with_typ (wrap_var_t "constantDeclaration")
    }
;

(******** Statements ********)

(* Petr4 X / Spec O *)
(* TODO: convert *)
assignmentOrMethodCallStatementWithoutSemicolon:
| func = lvalue L_PAREN args = argumentList R_PAREN
    { let type_args = [] in
      let tags = Source.merge (Expression.tags func) info2 in 
      Statement.MethodCall { tags; func; type_args; args } }
| func = lvalue l_angle type_args = typeArgumentList r_angle
    L_PAREN args = argumentList R_PAREN
    { let tags = Source.merge (Expression.tags func) info2 in
      Statement.MethodCall { tags; func; type_args; args } }
| lhs = lvalue ASSIGN rhs = expression
    { let tags = Source.merge (Expression.tags lhs) info2 in 
      Statement.Assignment { tags; lhs; rhs } }
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

(* TODO: convert *)
conditionalStatement:
| info1 = IF L_PAREN cond = expression R_PAREN tru = statement ELSE fls = statement
    { let info2 = Statement.tags fls in
      let fls = Some fls in
      let tags = Source.merge info1 info2 in
      Statement.Conditional { tags; cond; tru; fls } }
| info1 = IF L_PAREN cond = expression R_PAREN tru = statement   %prec THEN
    { let fls = None in
      let tags = Source.merge info1 (Statement.tags tru) in
      Statement.Conditional { tags; cond; tru; fls } }


(* TODO: convert *)
emptyStatement:
| info = SEMICOLON
    { Statement.EmptyStatement { tags = info } }
;

(* TODO: convert *)
blockStatement:
| annotations = optAnnotations
  info1 = L_BRACE
  push_scope
  statements = list(statementOrDeclaration) info2 = R_BRACE
  pop_scope
    { let tags = Source.merge info1 info2 in 
      Block.{ tags; annotations; statements } }
;

(* TODO: convert *)
returnStatement:
| info1 = RETURN info2 = SEMICOLON
    { let tags = Source.merge info1 info2 in 
      Statement.Return { tags; expr = None } }
| info1 = RETURN expr = expression info2 = SEMICOLON
    { let tags = Source.merge info1 info2 in
      Statement.Return { tags; expr = Some expr } }
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



(******** Annotations ********)
(** TODO **)




(******** Extern declarations ********)

(* externDeclaration: *)
(* | optAnnotations = optAnnotations *)
(*   info1 = EXTERN *)
(*   nonTypeName = externName (* TODO: -> nonTypeName *) *)
(*   optTypeParameters = optTypeParameters *)
(*   L_BRACE *)
(*   methodPrototypes = list(methodPrototype) *)
(*   info2 = R_BRACE *)
(*   pop_scope *)
(*     { let tags = Source.merge info1 info2 |> Source.merge info3 |> Source.merge info4 in *)
(*       tags |> ignore; *)
(*       (* []; ["EXTERN"]; []; ["{"]; ["}"] *) *)
(*       wrap_case_v [ optAnnotations; term "EXTERN"; nonTypeName; optTypeParameters; term "{"; methodPrototypes; term "}" ] *)
(*           |> with_typ (wrap_var_t "declaration") *)
(*     } *)
(* | optAnnotations = optAnnotations *)
(*   info1 = EXTERN *)
(*   functionPrototype = functionPrototype *)
(*   pop_scope *)
(*   info2 = SEMICOLON *)
(*     { let tags = Source.merge info1 info2 in *)
(*       tags |> ignore; *)
(*       let mixop = [ []; [ wrap_atom "EXTERN" ]; [ wrap_atom ";" ] ] in *)
(*       CaseV (mixop, [ optAnnotations; functionPrototype ]) *)
(*           |> with_typ (wrap_var_t "declaration") *)
(*     } *)
(* ; *)

(******** Declarations ********)
declaration:
| c = constantDeclaration
    { declare_var (name_of_declaration c) (has_typ_params_declaration c);
      c }
(* TODO: remove expressionStatement *)
| e = expressionStatement
    { e }
(* | e = errorDeclaration *)
(*     { e } *)
(* | m = matchKindDeclaration *)
(*     { m } *)
(* | e = externDeclaration *)
(*     { e } *)
(* | i = instantiation *)
(*     { i } *)
(* | f = functionDeclaration *)
(*     { f } *)
(* | a = actionDeclaration *)
(*     { a } *)
(* | p = parserDeclaration *)
(*     { p } *)
(* | c = controlDeclaration *)
(*     { c } *)
(* | t = typeDeclaration *)
(*     { t } *)
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
(* TODO: remove expressionStatement *)
expressionStatement:
| expr = expression SEMICOLON
    { expr }
;

expression:
| value = NUMBER { fst value }
| name = NAME { name }
| TRUE
    { BoolV true |> with_typ BoolT }
| FALSE
    { BoolV false |> with_typ BoolT }
| L_PAREN expr = expression R_PAREN { expr } 
| expressionStatement
;

