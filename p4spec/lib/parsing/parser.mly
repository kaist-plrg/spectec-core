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

  let name_of_declaration (declaration: value) : string =
    (* TODO: fetch name from CaseV fields *)
    assert (declaration.note.typ = VarT ("declaration", []));
    match declaration.it with
    | Il.Ast.CaseV (mixop, values) ->
      failwith "todo"
    | _ -> assert false

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
    { [ NT ids ] |> wrap_case_v |> with_typ (wrap_var_t "identifierList") }
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
| NONE
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
      | names = separated_nonempty_list(COMMA, name) { declare_types names }
;
typeParameters:
| info_l = l_angle type_params = typeParameterList info_r = r_angle
    { let tags = Source.merge info_l info_r in
      tags |> ignore;
      [ Term "<"; NT type_params; Term ">" ]
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

(* TODO: convert *)
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
| value = ASSIGN expression
    { [ Term "="; NT value ] |> wrap_case_v |> with_typ (wrap_var_t "initializer") }
;

optInitializer: 
| opt = option(initialValue) 
    {  let typ = value.note.typ |> wrap_iter_t Option in
      OptV opt |> with_typ typ }
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
    { Statement.EmptyStatement { tags = info } }
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
    { let typ = switchCase.note.typ |> wrap_iter_t List in
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

(* Petr4 X / Spec O whole for statement missing *)
declOrAssignmentOrMethodCallStatement:
| variableDeclarationWithoutSemicolon = variableDeclarationWithoutSemicolon
    { [ NT variableDeclarationWithoutSemicolon ]
      |> wrap_case_v |> with_typ (wrap_var_t "declOrAssignmentOrMethodCallStatement") }
| assignmentOrMethodCallStatementWithoutSemicolon = assignmentOrMethodCallStatementWithoutSemicolon
    { [ NT assignmentOrMethodCallStatementWithoutSemicolon ]
      |> wrap_case_v |> with_typ (wrap_var_t "declOrAssignmentOrMethodCallStatement") }
;

  forInitStatementNonEmpty:
| ls = separated_nonempty_list(COMMA, declOrAssignmentOrMethodCallStatement)
    { let typ = wrap_var_t "declOrAssignmentOrMethodCallStatement" |> wrap_iter_t List in
      ListV ls |> with_typ typ }
  ;
forInitStatements:
| ls = separated_list(COMMA, declOrAssignmentOrMethodCallStatement)
    { let typ = wrap_var_t "declOrAssignmentOrMethodCallStatement" |> wrap_iter_t List in
      ListV ls |> with_typ typ }
  ;
forUpdateStatementsNonEmpty:
| assignments = separated_nonempty_list(COMMA, assignmentOrMethodCallStatement)
    { let typ = wrap_var_t "assignmentOrMethodCallStatement" |> wrap_iter_t List in
      ListV assignments |> with_typ typ }
;
    
forUpdateStatements:
| assignments = separated_list(COMMA, assignmentOrMethodCallStatement)
    { assignments }

forCollectionExpr:
| expr = expression
    { [ NT expr ] |> wrap_case_v |> with_typ (wrap_var_t "forCollectionExpr") }
| expr_l = expression DOTDOT expr_r = expression
    { [ NT expr_l; Term ".."; NT expr_r ]
      |> wrap_case_v |> with_typ (wrap_var_t "forCollectionExpr") }
;

forStatement:
| anno = optAnnotations
  info1 = FOR
  L_PAREN init = forInitStatements SEMICOLON cond = expression SEMICOLON update = forUpdateStatements R_PAREN
  body = statement
    { [ NT anno; Term "for"; Term "("; NT init; Term ";"; NT cond; Term ";"; NT update; Term ")"; NT body ]
      |> wrap_case_v |> with_typ (wrap_var_t "forStatement") }
| anno = optAnnotations info1 = FOR L_PAREN
    typ = typeRef name = name IN collection = forCollectionExpr R_PAREN body = statement
    { [ NT anno; Term "for"; Term "("; NT typ; NT name; Term "in"; NT collection; Term ")"; NT body ]
      |> wrap_case_v |> with_typ (wrap_var_t "forStatement") }
| anno = optAnnotations info1 = FOR L_PAREN
    anno_in = optAnnotations typ = typeRef name = name IN 
    collection = forCollectionExpr R_PAREN body = statement
    { [ NT anno; Term "for"; Term "("; NT anno_in; NT typ; NT name; Term "in"; NT collection; Term ")"; NT body ]
      |> wrap_case_v |> with_typ (wrap_var_t "forStatement") }
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
| list(statementOrDeclaration)
    { let typ = wrap_var_t "statementOrDeclaration" |> wrap_iter_t List in
      ListV (List.map (fun x -> x) s) |> with_typ typ }


(******** Annotations ********)
(* TODO: optional *)
%inline optAnnotations:
| (* empty *)
    { [ Term "empty" ] |> wrap_case_v |> with_typ (wrap_var_t "optAnnotations") }
| annos = nonempty_list(annotation)
    { let typ = wrap_var_t "annotation" |> wrap_iter_t List in
      ListV annos |> with_typ typ }
;

annotations:
| annos = nonempty_list(annotation)
    { let typ = wrap_var_t "annotation" |> wrap_iter_t List in
      ListV annos |> with_typ typ }
;

(* TODO: compare with Petr4 *)
annotation:
| info1 = AT name = name
    { let tags = Source.merge info1 (Text.tags name) in
      tags |> ignore;
      [ Term "@"; NT name ] |> wrap_case_v |> with_typ (wrap_var_t "annotation") }
| info1 = AT name = name L_PAREN body = simpleAnnotationBody R_PAREN
    { info1 |> ignore;
      [ Term "@"; NT name; Term "("; NT body; Term ")" ] |> wrap_case_v |> with_typ (wrap_var_t "annotation") }
| info1 = AT name = name L_BRACKET body = structuredAnnotationBody R_BRACKET
    { info1 |> ignore;
      [ Term "@"; NT name; Term "["; NT body; Term "]" ] |> wrap_case_v |> with_typ (wrap_var_t "annotation") }
;

(******** Error and match kind declarations ********)

matchKindDeclaration:
| info1 = MATCH_KIND L_BRACE ids = identifierList comma = optTrailingComma info2 = R_BRACE
    { let tags = Source.merge info1 info2 in
      tags |> ignore;
      [ Term "MATCH_KIND"; Term "{"; NT ids; NT comma; Term "}" ] |> wrap_case_v |> with_typ (wrap_var_t "matchKindDeclaration") }
;

errorDeclaration:
| info1 = ERROR L_BRACE ids = identifierList info2 = R_BRACE
    { let tags = Source.merge info1 info2 in
      tags |> ignore;
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
      [ NT anno; Term "extern"; NT name; NT optTypeParams; Term "{"; NT protos; Term "}" ]
      |> wrap_case_v |> with_typ (wrap_var_t "externDeclaration") }
| anno = optAnnotations info1 = EXTERN proto = functionPrototype pop_scope info2 = SEMICOLON
    { let tags = Source.merge info1 info2 in
      tags |> ignore;
      [ NT anno; Term "extern"; NT proto; Term ";" ] |> wrap_case_v |> with_typ (wrap_var_t "externDeclaration") }
;

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
| (* empty *) { [ ] }
| actions = separated_nonempty_list_aux(SEMICOLON, action)
    { let typ = wrap_var_t "action" |> wrap_iter_t List in
      ListV actions |> with_typ typ }
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
      [ NT anno; NT optConst; Term "ENTRIES"; Term "="; Term "{"; NT entries; Term "}" ] |> wrap_case_v |> with_typ (wrap_var_t "tableProperty") }
| anno = optAnnotations optConst = optCONST name = nonTableKwName init = initializer info2 = SEMICOLON
    { let tags = Source.merge (Type.tags init) info2 in
      tags |> ignore;
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
    { declare_var (name_of_declaration action) false;
      [ NT action ] |> wrap_case_v |> with_typ (wrap_var_t "controlLocalDeclaration") }
| table = tableDeclaration
    { declare_var (name_of_declaration table) false;
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
    { let tags = Source.merge (Type.tags controlTypeDeclaration) info2 in
      tags |> ignore; 
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
      [ NT anno; Term "STATE"; NT name; Term "{"; NT stmts; NT trans; Term "}" ]
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
    { let tags = Source.merge info1 (Type.tags params) in
      tags |> ignore;
      [ NT anno; Term "PARSER"; NT name; NT optTypeParams; Term "("; NT params; Term ")" ]
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
    { [ NT name; NT initialValue ] |> wrap_case_v |> with_typ (wrap_var_t "specifiedIdentifier") }
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
| parser = parserTypeDeclaration pop_scope info2 = SEMICOLON
    { info2 |> ignore;
      [ NT parser; Term ";"; Term "PHTM_13" ] |> wrap_case_v |> with_typ (wrap_var_t "typeDeclaration") }
| control = controlTypeDeclaration pop_scope info2 = SEMICOLON
    { info2 |> ignore;
      [ NT control; Term ";"; Term "PHTM_14" ] |> wrap_case_v |> with_typ (wrap_var_t "typeDeclaration") }
| package = packageTypeDeclaration pop_scope info2 = SEMICOLON
    { info2 |> ignore;
      [ NT package; Term ";"; Term "PHTM_15" ] |> wrap_case_v |> with_typ (wrap_var_t "typeDeclaration") }
;

declaration:
| const = constantDeclaration
    { declare_var (name_of_declaration const) (has_typ_params_declaration const);
      [ NT const ] |> wrap_case_v |> with_typ (wrap_var_t "declaration") }
| error = errorDeclaration
    { [ NT error ] |> wrap_case_v |> with_typ (wrap_var_t "declaration") }
| matchKind = matchKindDeclaration
    { [ NT matchKind ] |> wrap_case_v |> with_typ (wrap_var_t "declaration") }
| extern = externDeclaration
    { [ NT extern ] |> wrap_case_v |> with_typ (wrap_var_t "declaration") }
| inst = instantiation
    { declare_var (name_of_declaration inst) false;
      [ NT inst ] |> wrap_case_v |> with_typ (wrap_var_t "declaration") }
| func = functionDeclaration
    { declare_var (name_of_declaration func) (has_typ_params_declaration func);
      [ NT func ] |> wrap_case_v |> with_typ (wrap_var_t "declaration") }
| action = actionDeclaration
    { declare_var (name_of_declaration action) false;
      [ NT action ] |> wrap_case_v |> with_typ (wrap_var_t "declaration") }
| parser = parserDeclaration
    { declare_type (name_of_declaration parser) (has_typ_params_declaration parser);
      [ NT parser ] |> wrap_case_v |> with_typ (wrap_var_t "declaration") }
| control = controlDeclaration
    { declare_type (name_of_declaration control) (has_typ_params_declaration control);
      [ NT control ] |> wrap_case_v |> with_typ (wrap_var_t "declaration") }
| typeDeclaration = typeDeclaration
    { declare_type (name_of_declaration typeDeclaration) (has_typ_params_declaration typeDeclaration);
      [ NT typeDeclaration ] |> wrap_case_v |> with_typ (wrap_var_t "declaration") }
;

annotationToken:
| info = UNEXPECTED_TOKEN
    { info |> ignore;
      [ Term "UNEXPECTED_TOKEN" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| ABSTRACT
    { info |> ignore;
      [ Term "ABSTRACT" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| info = ACTION
    { info |> ignore;
      [ Term "ACTION" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| info = ACTIONS
    { info |> ignore;
      [ Term "ACTIONS" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| info = APPLY
    { info |> ignore;
      [ Term "APPLY" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| info = BOOL
    { info |> ignore;
      [ Term "BOOL" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| info = BIT
    { info |> ignore;
      [ Term "BIT" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| info = BREAK
    { info |> ignore;
      [ Term "BREAK" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| info = CONST
    { info |> ignore;
      [ Term "CONST" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| info = CONTINUE
    { info |> ignore;
      [ Term "CONTINUE" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| info = CONTROL
    { info |> ignore;
      [ Term "CONTROL" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| info = DEFAULT
    { info |> ignore;
      [ Term "DEFAULT" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| info = ELSE
    { info |> ignore;
      [ Term "ELSE" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| info = ENTRIES
    { info |> ignore;
      [ Term "ENTRIES" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| info = ENUM
    { info |> ignore;
      [ Term "ENUM" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| info = ERROR
    { info |> ignore;
      [ Term "ERROR" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| info = EXIT
    { info |> ignore;
      [ Term "EXIT" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| info = EXTERN
    { info |> ignore;
      [ Term "EXTERN" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| info = FALSE
    { info |> ignore;
      [ Term "FALSE" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| info = FOR
    { info |> ignore;
      [ Term "FOR" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| info = HEADER
    { info |> ignore;
      [ Term "HEADER" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| info = HEADER_UNION
    { info |> ignore;
      [ Term "HEADER_UNION" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| info = IF
    { info |> ignore;
      [ Term "IF" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| info = IN
    { info |> ignore;
      [ Term "IN" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| info = INOUT
    { info |> ignore;
      [ Term "INOUT" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| info = INT
    { info |> ignore;
      [ Term "INT" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| info = KEY
    { info |> ignore;
      [ Term "KEY" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| info = MATCH_KIND
    { info |> ignore;
      [ Term "MATCH_KIND" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| info = TYPE
    { info |> ignore;
      [ Term "TYPE" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| info = OUT
    { info |> ignore;
      [ Term "OUT" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| info = PARSER
    { info |> ignore;
      [ Term "PARSER" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| info = PACKAGE
    { info |> ignore;
      [ Term "PACKAGE" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| info = PRAGMA
    { info |> ignore;
      [ Term "PRAGMA" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| info = RETURN
    { info |> ignore;
      [ Term "RETURN" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| info = SELECT
    { info |> ignore;
      [ Term "SELECT" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| info = STATE
    { info |> ignore;
      [ Term "STATE" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| info = STRING
    { info |> ignore;
      [ Term "STRING" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| info = STRUCT
    { info |> ignore;
      [ Term "STRUCT" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| info = SWITCH
    { info |> ignore;
      [ Term "SWITCH" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| info = TABLE
    { info |> ignore;
      [ Term "TABLE" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| info = THIS
    { info |> ignore;
      [ Term "THIS" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| info = TRANSITION
    { info |> ignore;
      [ Term "TRANSITION" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| info = TRUE
    { info |> ignore;
      [ Term "TRUE" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| info = TUPLE
    { info |> ignore;
      [ Term "TUPLE" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| info = TYPEDEF
    { info |> ignore;
      [ Term "TYPEDEF" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| info = VARBIT
    { info |> ignore;
      [ Term "VARBIT" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| info = VALUESET
    { info |> ignore;
      [ Term "VALUESET" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| info = LIST
    { info |> ignore;
      [ Term "LIST" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| info = VOID
    { info |> ignore;
      [ Term "VOID" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| info = `_
    { info |> ignore;
      [ Term "_" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| id = identifier
    { [ NT id ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| tid = typeIdentifier
    { [ NT tid ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| str = stringLiteral
    { [ NT str ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| num = number
    { [ NT num ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| info = `&&&
    { info |> ignore;
      [ Term "&&&" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| info = `..
    { info |> ignore;
      [ Term ".." ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| info = `<<
    { info |> ignore;
      [ Term "<<" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| info = `&&
    { info |> ignore;
      [ Term "&&" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| info = `||
    { info |> ignore;
      [ Term "||" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| info = `==
    { info |> ignore;
      [ Term "==" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| info = `!=
    { info |> ignore;
      [ Term "!=" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| info = `>=
    { info |> ignore;
      [ Term ">=" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| info = `<=
    { info |> ignore;
      [ Term "<=" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| info = `++
    { info |> ignore;
      [ Term "++" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| info = `+
    { info |> ignore;
      [ Term "+" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| info = `|+|
    { info |> ignore;
      [ Term "|+|" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| info = `-
    { info |> ignore;
      [ Term "-" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| info = `|-|
    { info |> ignore;
      [ Term "|-|" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| info = `*
    { info |> ignore;
      [ Term "*" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| info = `/
    { info |> ignore;
      [ Term "/" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| info = `%
    { info |> ignore;
      [ Term "%" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| info = `|
    { info |> ignore;
      [ Term "|" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| info = `&
    { info |> ignore;
      [ Term "&" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| info = `^
    { info |> ignore;
      [ Term "^" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| info = `~
    { info |> ignore;
      [ Term "~" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| info = ``[
    { info |> ignore;
      [ Term "[" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| info = ``]
    { info |> ignore;
      [ Term "]" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| info = ``{
    { info |> ignore;
      [ Term "{" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| info = ``}
    { info |> ignore;
      [ Term "}" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| info = ``<
    { info |> ignore;
      [ Term "<" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| info = ``>
    { info |> ignore;
      [ Term ">" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| info = `!
    { info |> ignore;
      [ Term "!" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| info = `:
    { info |> ignore;
      [ Term ":" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| info = `,
    { info |> ignore;
      [ Term ","; Term "PHTM_21" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| info = `?
    { info |> ignore;
      [ Term "?" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| info = `.
    { info |> ignore;
      [ Term "." ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| info = `=
    { info |> ignore;
      [ Term "=" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| info = `;
    { info |> ignore;
      [ Term ";" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
| info = `@
    { info |> ignore;
      [ Term "@" ] |> wrap_case_v |> with_typ (wrap_var_t "annotationToken") }
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

annotations:
| annos = list(annotation)
    { let typ = wrap_var_t "annotation" |> wrap_iter_t List in
      ListV annos |> with_typ typ }
;

optAnnotations:
| (* empty *)
    { [ Term "empty" ] |> wrap_case_v |> with_typ (wrap_var_t "optAnnotations") }
| annos = nonempty_list(annotation)
    { let typ = wrap_var_t "annotation" |> wrap_iter_t List in
      ListV annos |> with_typ typ }
;

