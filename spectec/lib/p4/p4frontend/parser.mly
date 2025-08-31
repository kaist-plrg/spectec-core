(* Copyright 2018-present Cornell University
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not
 * use this file except in compliance with the License. You may obtain a copy
 * of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
 * WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
 * License for the specific language governing permissions and limitations
 * under the License.
 *)

%{
module P4cherry = struct end
open Context

(* A hack to avoid conflict btw this module and Surface.Parser *)
module Parser = P4surface.Ast.Parser
open P4surface.Ast
open P4util

let rec smash_annotations (l: Text.t list) (tok2: Text.t): Text.t list =
  match l with
  | [] -> [ tok2 ]
  | [ tok1 ] ->
     if Source.follows tok1.tags tok2.tags then
       [{ tags = Source.merge tok1.tags tok2.tags;
         str = tok1.str ^ tok2.str }]
     else
       [ tok1; tok2 ]
  | h :: t -> h :: smash_annotations t tok2

%}

(**************************** TOKENS ******************************)

%token<P4util.Source.info> END
%token TYPENAME IDENTIFIER
%token<P4surface.Ast.Text.t> NAME STRING_LITERAL
%token<P4surface.Ast.Number.t * string> NUMBER
%token<P4util.Source.info> LE GE SHL AND OR NE EQ
%token<P4util.Source.info> PLUS MINUS PLUS_SAT MINUS_SAT MUL INVALID DIV MOD
%token<P4util.Source.info> BIT_OR BIT_AND BIT_XOR COMPLEMENT
%token<P4util.Source.info> L_BRACKET R_BRACKET L_BRACE R_BRACE L_ANGLE L_ANGLE_ARGS R_ANGLE R_ANGLE_SHIFT L_PAREN R_PAREN
%token<P4util.Source.info> ASSIGN COLON COMMA QUESTION DOT NOT SEMICOLON
%token<P4util.Source.info> AT PLUSPLUS
%token<P4util.Source.info> DONTCARE
%token<P4util.Source.info> MASK DOTS RANGE
%token<P4util.Source.info> TRUE FALSE
%token<P4util.Source.info> ABSTRACT ACTION ACTIONS APPLY BOOL BIT CONST CONTROL DEFAULT DEFAULT_ACTION
%token<P4util.Source.info> ELSE ENTRIES ENUM ERROR EXIT EXTERN HEADER HEADER_UNION IF IN INOUT
%token<P4util.Source.info> INT KEY LIST SELECT MATCH_KIND OUT PACKAGE PARSER PRIORITY RETURN STATE STRING STRUCT
%token<P4util.Source.info> SWITCH TABLE THEN THIS TRANSITION TUPLE TYPE TYPEDEF VARBIT VALUESET VOID
%token<P4util.Source.info> PRAGMA PRAGMA_END
%token<P4surface.Ast.Text.t> UNEXPECTED_TOKEN

(**************************** PRIORITY AND ASSOCIATIVITY ******************************)

(* Precedence of THEN token is artificial. *)
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


%start <p4program> p4program
%start <Declaration.t> variableDeclaration
%start <Declaration.t> typeDeclaration

%%

(**************************** CONTEXTS ******************************)

push_scope:
| (* empty *)
    { push_scope() }
;

push_name:
| n = name
   { push_scope();
     declare_type n false;
     n }

push_externName:
| n = externName
    { push_scope();
      declare_type n false;
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

p4program : ds = topDeclarationList END { Program ds };

topDeclarationList:
| (* empty *)
    { [] }
| SEMICOLON ds = topDeclarationList
    { ds }
| d = topDeclaration ds = topDeclarationList
    { d :: ds }

topDeclaration:
| c = constantDeclaration
    { declare_var (Declaration.name c) (Declaration.has_type_params c);
      c }
| e = externDeclaration
    { e }
| a = actionDeclaration
    { declare_var (Declaration.name a) false;
      a }
| p = parserDeclaration
    { declare_type (Declaration.name p) (Declaration.has_type_params p);
      p }
| c = controlDeclaration
    { declare_type (Declaration.name c) (Declaration.has_type_params c);
      c }
| i = instantiation
    { declare_var (Declaration.name i) false;
      i }
| t = typeDeclaration
    { declare_type (Declaration.name t) (Declaration.has_type_params t);
      t }
| e = errorDeclaration
    { (* declare_type (Declaration.name e) false; *)
      e }
| m = matchKindDeclaration
    { m }
| f = functionDeclaration
    { declare_var (Declaration.name f) (Declaration.has_type_params f);
      f }
;

varName:
| id = NAME IDENTIFIER
    { id }
;

tableKwName:
| info = KEY
    { Text.{ tags = info; str = "key" } }
| info = ACTIONS
    { Text.{ tags = info; str = "actions" } }
| info = ENTRIES
    { Text.{ tags = info; str = "entries" } }
;

nonTableKwName:
| n = varName
    { n }
| n = NAME TYPENAME
    { n }
| info = APPLY
    { Text.{ tags = info; str = "apply" } }
| info = STATE
    { Text.{ tags = info; str = "state" } }
| info = TYPE
    { Text.{ tags = info; str = "type" } }
| info = PRIORITY
    { Text.{ tags = info; str = "priority" } }
;

nonTypeName:
| n = varName
    { n }
| n = tableKwName
    { n }
| info = APPLY
    { Text.{ tags = info; str = "apply" } }
| info = STATE
    { Text.{ tags = info; str = "state" } }
| info = TYPE
    { Text.{ tags = info; str = "type" } }
| info = PRIORITY
    { Text.{ tags = info; str = "priority" } }
;

name:
| n = nonTypeName
    { n }
| info = LIST
    { Text.{ tags = info; str = "list" } }
| n = NAME TYPENAME
    { n }
;

%inline optAnnotations:
| (* empty *)
    { [] }
| annotations = annotations
    { annotations }
;

annotations:
| annotations = nonempty_list(annotation)
    { annotations }
;

annotation:
| info1 = AT name = name
    { let info2 = Text.tags name in
      let body = Annotation.Empty { tags = info2 } in
      let tags = Source.merge info1 info2 in
      Annotation.{ tags; name; body } }
| info1 = AT name = name info2 = L_PAREN body = annotationBody info3 = R_PAREN
    { let tags = Source.merge info2 info3 in
      let body = Annotation.Unparsed { tags; str = body } in
      let tags = Source.merge info1 info3 in
      Annotation.{ tags; name; body } }
| info1 = AT name = name info2 = L_BRACKET body = expressionOptTrailingList info3 = R_BRACKET
    { let tags = Source.merge info2 info3 in
      let body = Annotation.Expression { tags; exprs = body } in
      let tags = Source.merge info1 info3 in
      Annotation.{ tags; name; body } }
| info1 = AT name = name info2 = L_BRACKET body = kvOptTrailingList info3 = R_BRACKET
    { let tags = Source.merge info2 info3 in
      let body = Annotation.KeyValue { tags; key_values = body } in
      let tags = Source.merge info1 info3 in
      Annotation.{ tags; name; body } }
| info1 = PRAGMA name = name body = annotationBody info2 = PRAGMA_END
    { let body = Annotation.Unparsed { tags = info2; str = body } in
      let tags = Source.merge info1 info2 in
      Annotation.{ tags; name; body }}
;

annotationBody:
| (* empty *)
    { [] }
| body1 = annotationBody L_PAREN body2 = annotationBody R_PAREN
    { body1 @ body2 }
(* Not sure if this is correct. *)
| body = annotationBody token = annotationToken
    { smash_annotations body token }
;

annotationToken:
| UNEXPECTED_TOKEN
    { $1 }
| ABSTRACT
    { Text.{ tags = $1; str = "abstract" } }
| ACTION
    { Text.{ tags = $1; str = "action" } }
| ACTIONS
    { Text.{ tags = $1; str = "actions" } }
| APPLY
    { Text.{ tags = $1; str = "apply" } }
| BOOL
    { Text.{ tags = $1; str = "bool" } }
| BIT
    { Text.{ tags = $1; str = "bit" } }
| CONST
    { Text.{ tags = $1; str = "const" } }
| CONTROL
    { Text.{ tags = $1; str = "control" } }
| DEFAULT
    { Text.{ tags = $1; str = "default" } }
| ELSE
    { Text.{ tags = $1; str = "else" } }
| ENTRIES
    { Text.{ tags = $1; str = "entries" } }
| ENUM
    { Text.{ tags = $1; str = "enum" } }
| ERROR
    { Text.{ tags = $1; str = "error" } }
| EXIT
    { Text.{ tags = $1; str = "exit" } }
| EXTERN
    { Text.{ tags = $1; str = "extern" } }
| FALSE
    { Text.{ tags = $1; str = "false" } }
| HEADER
    { Text.{ tags = $1; str = "header" } }
| HEADER_UNION
    { Text.{ tags = $1; str = "header_union" } }
| IF
    { Text.{ tags = $1; str = "if" } }
| IN
    { Text.{ tags = $1; str = "in" } }
| INOUT
    { Text.{ tags = $1; str = "inout" } }
| INT
    { Text.{ tags = $1; str = "int" } }
| KEY
    { Text.{ tags = $1; str = "key" } }
| MATCH_KIND
    { Text.{ tags = $1; str = "match_kind" } }
| TYPE
    { Text.{ tags = $1; str = "type" } }
| OUT
    { Text.{ tags = $1; str = "out" } }
| PARSER
    { Text.{ tags = $1; str = "parser" } }
| PACKAGE
    { Text.{ tags = $1; str = "package" } }
| PRAGMA
    { Text.{ tags = $1; str = "pragma" } }
| RETURN
    { Text.{ tags = $1; str = "return" } }
| SELECT
    { Text.{ tags = $1; str = "select" } }
| STATE
    { Text.{ tags = $1; str = "state" } }
| STRING
    { Text.{ tags = $1; str = "string" } }
| STRUCT
    { Text.{ tags = $1; str = "struct" } }
| SWITCH
    { Text.{ tags = $1; str = "switch" } }
| TABLE
    { Text.{ tags = $1; str = "table" } }
| THIS
    { Text.{ tags = $1; str = "this" } }
| TRANSITION
    { Text.{ tags = $1; str = "transition" } }
| TRUE
    { Text.{ tags = $1; str = "true" } }
| TUPLE
    { Text.{ tags = $1; str = "tuple" } }
| TYPEDEF
    { Text.{ tags = $1; str = "typedef" } }
| VARBIT
    { Text.{ tags = $1; str = "varbit" } }
| VALUESET
    { Text.{ tags = $1; str = "valueset" } }
| LIST
    { Text.{ tags = $1; str = "list" } }
| VOID
    { Text.{ tags = $1; str = "void" } }
| DONTCARE
    { Text.{ tags = $1; str = "_" } }
| NAME IDENTIFIER
    { $1 }
| NAME TYPENAME
    { $1 }
| STRING_LITERAL
    { let info, str = Text.tags $1, $1.str in
      Text.{ tags = info; str = "\"" ^ str ^ "\"" } }
| NUMBER
    { let num: Number.t = fst $1 in
      let str: string = snd $1 in
      Text.{ tags = num.tags; str } }
| MASK
    { Text.{ tags = $1; str = "&&&" } }
| DOTS
    { Text.{ tags = $1; str = "..." } }
| RANGE
    { Text.{ tags = $1; str = ".." } }
| SHL
    { Text.{ tags = $1; str = "<<" } }
| AND
    { Text.{ tags = $1; str = "&&" } }
| OR
    { Text.{ tags = $1; str = "||" } }
| EQ
    { Text.{ tags = $1; str = "==" } }
| NE
    { Text.{ tags = $1; str = "!=" } }
| GE
    { Text.{ tags = $1; str = ">=" } }
| LE
    { Text.{ tags = $1; str = "<=" } }
| PLUSPLUS
    { Text.{ tags = $1; str = "++" } }
| PLUS
    { Text.{ tags = $1; str = "+" } }
| PLUS_SAT
    { Text.{ tags = $1; str = "|+|" } }
| MINUS
    { Text.{ tags = $1; str = "-" } }
| MINUS_SAT
    { Text.{ tags = $1; str = "|-|" } }
| MUL
    { Text.{ tags = $1; str = "*" } }
| DIV
    { Text.{ tags = $1; str = "/" } }
| MOD
    { Text.{ tags = $1; str = "%" } }
| BIT_OR
    { Text.{ tags = $1; str = "|" } }
| BIT_AND
    { Text.{ tags = $1; str = "&" } }
| BIT_XOR
    { Text.{ tags = $1; str = "^" } }
| COMPLEMENT
    { Text.{ tags = $1; str = "~" } }
| L_BRACKET
    { Text.{ tags = $1; str = "[" } }
| R_BRACKET
    { Text.{ tags = $1; str = "]" } }
| L_BRACE
    { Text.{ tags = $1; str = "{" } }
| R_BRACE
    { Text.{ tags = $1; str = "}" } }
| L_ANGLE
    { Text.{ tags = $1; str = "<" } }
| R_ANGLE
    { Text.{ tags = $1; str = ">" } }
| NOT
    { Text.{ tags =$1; str = "!" } }
| COLON
    { Text.{ tags = $1; str = ":" } }
| COMMA
    { Text.{ tags = $1; str = "," } }
| QUESTION
    { Text.{ tags = $1; str = "?" } }
| DOT
    { Text.{ tags = $1; str = "." } }
| ASSIGN
    { Text.{ tags = $1; str = "=" } }
| SEMICOLON
    { Text.{ tags = $1; str = ";" } }
| AT
    { Text.{ tags = $1; str = "@" } }
;

parameterList:
| params = separated_list(COMMA, parameter)
    { let names = List.map (fun (p : Parameter.t) -> p.Parameter.variable) params in
      declare_vars names; params }
;

parameter:
| annotations = optAnnotations
  direction = direction typ = typeRef variable = name
    { let info1 =
        match direction with
        | None -> Type.tags typ
        | Some dir -> Direction.tags dir
      in
      let info = Source.merge info1 (Text.tags variable) in
      Parameter.{ tags = info; annotations; direction; typ; variable; opt_value = None } }
| annotations = optAnnotations
  direction = direction typ = typeRef variable = name ASSIGN value = expression
    { let info1 =
        match (direction : Direction.t option) with
        | None -> Type.tags typ
        | Some dir -> Direction.tags dir in
      let tags = Source.merge info1 (Text.tags variable) in
      Parameter.{ tags; annotations; direction; typ; variable; opt_value = Some value } }
;

direction:
| info = IN
    { Some (Direction.In { tags = info }) }
| info = OUT
    { Some (Direction.Out { tags = info }) }
| info = INOUT
    { Some (Direction.InOut { tags = info }) }
| (* empty *)
    { None }
;

packageTypeDeclaration:
| annotations = optAnnotations info1 = PACKAGE
  name = push_name
  type_params = optTypeParameters
  L_PAREN params = parameterList info2 = R_PAREN
    { let tags = Source.merge info1 info2 in
      Declaration.PackageType { tags; annotations; name; type_params; params } }
;

instantiation:
| annotations = optAnnotations typ = typeRef
    L_PAREN args = argumentList R_PAREN name = name info2 = SEMICOLON
    { let info' = Source.merge (Type.tags typ) info2 in
    Declaration.Instantiation { annotations; typ; args; name; init = []; tags = info' } }
| annotations = optAnnotations typ = typeRef
    L_PAREN args = argumentList R_PAREN name = name ASSIGN init = objInitializer info2 = SEMICOLON
    { let info' = Source.merge (Type.tags typ) info2 in
       Declaration.Instantiation { annotations; typ; args; name; init; tags = info' } }
;

objInitializer:
| L_BRACE decls = list(objDeclaration) R_BRACE
    { decls }
;

objDeclaration:
| decl = functionDeclaration
    { decl }
| decl = instantiation
    { decl }
;

optConstructorParameters:
| (* empty *)
    { [] }
| L_PAREN params = parameterList R_PAREN
    { params }
;

dotPrefix:
| info = DOT
    { info }
;

(**************************** PARSER ******************************)

parserDeclaration:
| p_type = parserTypeDeclaration constructor_params = optConstructorParameters
  L_BRACE locals = list_aux(parserLocalElement)
  states = nonempty_list(parserState)
  info2 = R_BRACE
  pop_scope
    { let (info1, annotations, name, type_params, params) = p_type in
      let tags = Source.merge info1 info2 in
      let locals = List.rev locals in
      Declaration.Parser { tags; annotations; name; type_params; params; constructor_params; locals; states } }
;

parserLocalElement:
| c = constantDeclaration
    { c }
| v = variableDeclaration
    { v }
| i = instantiation
    { i }
| vs = valueSetDeclaration
    { vs }
;

parserTypeDeclaration:
| annotations = optAnnotations info1 = PARSER
  name = push_name type_params = optTypeParameters
  L_PAREN params = parameterList info2 = R_PAREN
    { let info = Source.merge info1 info2 in
      (info, annotations, name, type_params, params) }
;

parserState:
| annotations = optAnnotations info1 = STATE name = push_name
  L_BRACE statements = list(parserStatement) transition = transitionStatement
  info2 = R_BRACE
  pop_scope
     { let tags = Source.merge info1 info2 in
       { tags; annotations; name; statements; transition }: Parser.state }

;

parserStatement:
| s = assignmentOrMethodCallStatement
| s = directApplication
| s = emptyStatement
| s = parserBlockStatement
| s = conditionalStatement
    { s }
| decl = constantDeclaration
| decl = variableDeclaration
    { let tags = Declaration.tags decl in
      Statement.DeclarationStatement { tags; decl } }
;

parserBlockStatement:
| annotations = optAnnotations
  info1 = L_BRACE statements = list(parserStatement) info2 = R_BRACE
     { let tags = Source.merge info1 info2 in
       let block = Block.{ annotations; statements; tags } in
       Statement.BlockStatement { tags; block } }
;

transitionStatement:
| (* empty *)
    { let tags = Source.M "Compiler-generated reject transition" in
      Parser.Direct { tags; next = { tags; str = "reject" } } }
| info1 = TRANSITION transition = stateExpression
    { (*let tags = Source.merge info1 (tags transition)
       snd transition)*)
      (* Not sure what's the type of transition but I'm guessing it's 'a transition'.*)
      Parser.update_transition_tags transition (Source.merge info1 (Parser.transition_tags transition)) }
;

stateExpression:
| next = name info2 = SEMICOLON
    { let tags = Source.merge (Text.tags next) info2 in
      Parser.Direct { tags; next } }
| select = selectExpression
    { select }
;

selectExpression:
| info1 = SELECT L_PAREN exprs = expressionList R_PAREN
  L_BRACE cases = list(selectCase) info2 = R_BRACE
    { let tags = Source.merge info1 info2 in
      Parser.Select { tags; exprs; cases } }
;

selectCase:
| matches = keysetExpression COLON next = name info2 = SEMICOLON
    { let info1 =
        match (matches : Match.t list) with
        | expr::_ -> Match.tags expr
        | _ -> assert false
      in
      let tags = Source.merge info1 info2 in
      Parser.{ tags; matches; next } }
;

keysetExpression:
| exprs = tupleKeysetExpression
    { exprs }
| expr  = simpleKeysetExpression
    { [ expr ] }
;

tupleKeysetExpression:
| L_PAREN exprs = separated_atLeastTwo_list(COMMA, simpleKeysetExpression) R_PAREN
    { exprs }
| L_PAREN expr = reducedSimpleKeysetExpression R_PAREN
    { [ expr ] }
;

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

valueSetDeclaration:
| annotations = optAnnotations
  info1 = VALUESET l_angle typ = baseType r_angle
  L_PAREN size = expression R_PAREN name = name info2 = SEMICOLON
| annotations = optAnnotations
  info1 = VALUESET l_angle typ = tupleType r_angle
  L_PAREN size = expression R_PAREN name = name info2 = SEMICOLON
| annotations = optAnnotations
  info1 = VALUESET l_angle typ = typeName r_angle
  L_PAREN size = expression R_PAREN name = name info2 = SEMICOLON
    { let tags = Source.merge info1 info2 in
      Declaration.ValueSet { tags; annotations; typ; size; name } }
;

(**************************** CONTROL ******************************)

controlDeclaration:
| ct_decl = controlTypeDeclaration constructor_params = optConstructorParameters
  L_BRACE locals = list(controlLocalDeclaration) APPLY apply = controlBody
  info2 = R_BRACE
  pop_scope
    { let info1, annotations, name, type_params, params = ct_decl in
      let tags = Source.merge info1 info2 in
      Declaration.Control
        { tags; annotations; name; type_params;
          params; constructor_params; locals; apply } }
;

controlTypeDeclaration:
| annotations = optAnnotations info1 = CONTROL
  name = push_name
  type_params = optTypeParameters
  L_PAREN params = parameterList info2 = R_PAREN
    { let info = Source.merge info1 info2 in
      (info, annotations, name, type_params, params) }
;

controlLocalDeclaration:
| c = constantDeclaration
    { c }
| a = actionDeclaration
    { declare_var (Declaration.name a) false;
      a }
| t = tableDeclaration
    { declare_var (Declaration.name t) false;
      t }
| i = instantiation
    { i }
| v = variableDeclaration
    { v }
;

controlBody:
| b = blockStatement
    { b }
;

(**************************** EXTERN ******************************)

externDeclaration:
| annotations = optAnnotations info1 = EXTERN
  name = push_externName
  type_params = optTypeParameters
  L_BRACE methods = list(methodPrototype) info2 = R_BRACE
  pop_scope
     { let tags = Source.merge info1 info2 in
       let type_decl =
           (Declaration.ExternObject { tags; annotations; name; type_params; methods }) in
       declare_type name (Declaration.has_type_params type_decl);
       type_decl }
| annotations = optAnnotations info1 = EXTERN
  func = functionPrototype
  pop_scope
  info2 = SEMICOLON
     { let (_, return, name, type_params, params) = func in
       let tags = Source.merge info1 info2 in
       let decl =
           Declaration.ExternFunction { tags; annotations; return; name; type_params; params } in
       declare_var name (Declaration.has_type_params decl);
       decl }
;

externName:
| n = nonTypeName
    { declare_type n false; n }

functionPrototype:
| typ = typeOrVoid name = name
  push_scope
  type_params = optTypeParameters
  L_PAREN params = parameterList info2 = R_PAREN
    { let tags = Source.merge (Type.tags typ) info2 in 
      (tags, typ, name, type_params, params) }
;

methodPrototype:
| annotations = optAnnotations ABSTRACT func = functionPrototype
  pop_scope
  info2 = SEMICOLON
    { let (info1, return, name, type_params, params) = func in
      let tags = Source.merge info1 info2 in
      MethodPrototype.AbstractMethod { tags; annotations; return; name; type_params; params } }
| annotations = optAnnotations func = functionPrototype
  pop_scope
  info2 = SEMICOLON
    { let (info1, return, name, type_params, params) = func in
      let tags = Source.merge info1 info2 in
      MethodPrototype.Method { tags; annotations; return; name; type_params; params } }
| annotations = optAnnotations name = methodName
  L_PAREN params = parameterList R_PAREN info2 = SEMICOLON
    { let tags = Source.merge (Text.tags name) info2 in
      MethodPrototype.Constructor { tags; annotations; name; params } }
;

methodName:
| n = NAME TYPENAME
    { n }

(**************************** TYPES ******************************)

typeRef:
| t = baseType
| t = typeName
| t = specializedType
| t = headerStackType
| t = p4listType
| t = tupleType
    { t }
;

namedType:
| t = typeName
| t = specializedType
    { t }
;

prefixedTypeName:
| name = NAME TYPENAME
    { Name.BareName name }
| dotPrefix go_toplevel name = NAME TYPENAME go_local
    { Name.QualifiedName ([], name) }
;

prefixedType:
| name = prefixedTypeName
    { let tags = Name.tags name in
       Type.TypeName { tags; name } }

typeName:
| typ = prefixedType
    { typ }
;

p4listType:
| info1 = LIST l_angle typ = typeArg info_r = r_angle
    { let tags = Source.merge info1 info_r in
      Type.List { tags; typ } }
;

tupleType:
| info1 = TUPLE l_angle elements = typeArgumentList info_r = r_angle
    { let tags = Source.merge info1 info_r in
       Type.Tuple { tags; args = elements } }
;

headerStackType:
| header = typeName L_BRACKET size = expression info2 = R_BRACKET
    { let tags = Source.merge (Type.tags header) info2 in
       Type.HeaderStack { tags; header; size } }
| header = specializedType L_BRACKET size = expression info2 = R_BRACKET
    { let tags = Source.merge (Type.tags header) info2 in
       Type.HeaderStack { tags; header; size } }
;

specializedType:
| base = typeName l_angle args = typeArgumentList info_r = r_angle
    { let tags = Source.merge (Type.tags base) info_r in
      Type.SpecializedType { tags; base; args } }
;

baseType:
| info = BOOL
    { Type.Bool { tags = info } }
| info = MATCH_KIND
    { Type.MatchKind { tags = info } }
| info = ERROR
    { Type.Error { tags = info } }
| info = BIT
    { let width =
        Expression.Int
          { tags = info;
            i = { tags = info;
                  value = Bigint.of_int 1;
                  width_signed = None; } }
      in
      Type.BitType { tags = info; expr = width } }
| info1 = BIT l_angle value = NUMBER info_r = r_angle
    { let value_int : Number.t = fst value in 
      let value_info = value_int.tags in
      let width = Expression.Int { i = value_int; tags = value_info } in
      let tags = Source.merge info1 info_r in
      Type.BitType { tags; expr = width } }
| info1 = INT l_angle value = NUMBER info_r = r_angle
     { let value_int : Number.t = fst value in 
       let value_info = value_int.tags in 
       let width = Expression.Int { tags = value_info; i = value_int } in
       let tags = Source.merge info1 info_r in
      Type.IntType { tags; expr = width } }
| info1 = VARBIT l_angle value = NUMBER info_r = r_angle 
     { let value_int : Number.t = fst value in 
       let value_info = value_int.tags in
       let max_width = Expression.Int { tags = value_info; i = value_int } in
       let tags = Source.merge info1 info_r in
      Type.VarBit { tags; expr = max_width } }
| info1 = BIT l_angle L_PAREN width = expression R_PAREN info_r = r_angle
    { let tags = Source.merge info1 info_r in
       Type.BitType { tags; expr = width } }
| info1 = INT l_angle L_PAREN width = expression R_PAREN info_r = r_angle
    { let tags = Source.merge info1 info_r in
       Type.IntType { tags; expr = width } }
| info1 = VARBIT l_angle L_PAREN max_width = expression R_PAREN info_r = r_angle
    { let tags = Source.merge info1 info_r in
       Type.VarBit { tags; expr = max_width; } }
| info = INT
    { Type.Integer { tags = info } }
| info = STRING
    { Type.String { tags = info } }
;

typeOrVoid:
| t = typeRef
    { t }
| info = VOID
    { Type.Void { tags = info } }
| name = varName
  { let tags: P4util.Source.info = Text.tags name in
    Type.TypeName { tags; name = BareName name } }
;

optTypeParameters:
| (* empty *)
    { [] }
| l_angle types = separated_nonempty_list(COMMA, typeParameter) r_angle
    { declare_types types;
      types }
;

typeParameter:
| name = name
    { name }
;

realTypeArg:
| info = DONTCARE
    { Type.DontCare { tags = info } }
| t = typeRef
    { t }
| info = VOID
    { Type.Void { tags = info } }
;

typeArg:
| info = DONTCARE
    { Type.DontCare { tags = info } }
| typ = typeRef
    { typ }
| name = nonTypeName
    { let tags: P4util.Source.info = Text.tags name in
      Type.TypeName { tags; name = BareName name } }
| info = VOID
    { Type.Void { tags = info } }
;

typeArgumentList:
| ts = separated_list(COMMA, typeArg)
    { ts }
;

realTypeArgumentList:
| t = realTypeArg
    { [ t ] }
| t = realTypeArg COMMA ts = separated_list(COMMA, typeArg)
    { t :: ts }
;

typeDeclaration:
| d = derivedTypeDeclaration
| d = typedefDeclaration
| d = packageTypeDeclaration pop_scope SEMICOLON
    { d }
| ctd = controlTypeDeclaration pop_scope SEMICOLON
    { let tags, annotations, name, type_params, params = ctd in
      Declaration.ControlType { tags; annotations; name; type_params; params } }
| ptd = parserTypeDeclaration pop_scope SEMICOLON
    { let tags, annotations, name, type_params, params = ptd in
      Declaration.ParserType { tags; annotations; name; type_params; params } }
;

derivedTypeDeclaration:
| d = headerTypeDeclaration
| d = headerUnionDeclaration
| d = structTypeDeclaration
| d = enumDeclaration
    { d }
;

headerTypeDeclaration:
| annotations = optAnnotations info1 = HEADER name = name
  type_params = optTypeParameters
  L_BRACE fields = list(structField) info2 = R_BRACE
     { let tags = Source.merge info1 info2 in 
       Declaration.Header { tags; annotations; name; type_params; fields } }
;

headerUnionDeclaration:
| annotations = optAnnotations info1 = HEADER_UNION name = name
  type_params = optTypeParameters
  L_BRACE fields = list(structField) info2 = R_BRACE
     { let tags = Source.merge info1 info2 in
       Declaration.HeaderUnion { tags; annotations; name; type_params; fields } }
;

structTypeDeclaration:
| annotations = optAnnotations info1 = STRUCT name = name
  type_params = optTypeParameters
  L_BRACE fields = list(structField) info2 = R_BRACE
     { let tags = Source.merge info1 info2 in 
       Declaration.Struct { tags; annotations; name; type_params; fields } }
;

structField:
| annotations = optAnnotations typ = typeRef name = name info2 = SEMICOLON
    { let tags = Source.merge (Type.tags typ) info2 in
      { tags; annotations; typ; name }: Declaration.field }
;

enumDeclaration:
| annotations = optAnnotations info1 = ENUM name = name
  L_BRACE members = identifierOptTrailingList info2 = R_BRACE
    { let tags = Source.merge info1 info2 in
      Declaration.Enum { tags; annotations; name; members } }
| annotations = optAnnotations info1 = ENUM typ = typeRef
  name = name L_BRACE members = specifiedIdentifierOptTrailingList R_BRACE
    { let tags = Source.merge info1 (Type.tags typ) in
      Declaration.SerializableEnum { tags; annotations; typ; name; members } }
;

errorDeclaration:
| info1 = ERROR L_BRACE members = identifierList info2 = R_BRACE
    { declare_vars members;
      let tags = Source.merge info1 info2 in 
      Declaration.Error { tags; members } }
;

matchKindDeclaration:
| info1 = MATCH_KIND L_BRACE members = identifierOptTrailingList info2 = R_BRACE
    { declare_vars members;
      let tags = Source.merge info1 info2 in
      Declaration.MatchKind { tags; members } }
;

identifierList:
| ids = separated_nonempty_list(COMMA, id = name {id})
    { ids };

identifierOptTrailingList:
| ids = separated_nonempty_opt_trailing_list(COMMA, id = name {id})
    { ids };

specifiedIdentifier:
| name = name ASSIGN init = expression
    { (name, init) }

specifiedIdentifierOptTrailingList:
| specIds = separated_nonempty_opt_trailing_list(COMMA, specId = specifiedIdentifier { specId })
    { specIds };

typedefDeclaration:
| annotations = optAnnotations info1 = TYPEDEF
  typ = typeRef name = name info2 = SEMICOLON
    { let tags = Source.merge info1 info2 in 
      Declaration.TypeDef { tags; annotations; name; typ_or_decl = Left typ }  }
| annotations = optAnnotations info1 = TYPEDEF
  decl = derivedTypeDeclaration name = name info2 = SEMICOLON
    { let tags = Source.merge info1 info2 in
      Declaration.TypeDef { tags; annotations; name; typ_or_decl = Right decl }  }
| annotations = optAnnotations info1 = TYPE
  typ = typeRef name = name info2 = SEMICOLON
    { let tags = Source.merge info1 info2 in 
      Declaration.NewType { tags; annotations; name; typ_or_decl = Left typ }  }
| annotations = optAnnotations info1 = TYPE
  decl = derivedTypeDeclaration name = name info2 = SEMICOLON
    { let tags = Source.merge info1 info2 in 
      Declaration.NewType { tags; annotations; name; typ_or_decl = Right decl }  }
;

(**************************** STATEMENTS ******************************)

assignmentOrMethodCallStatement:
| func = lvalue L_PAREN args = argumentList R_PAREN info2 = SEMICOLON
    { let type_args = [] in
      let tags = Source.merge (Expression.tags func) info2 in 
      Statement.MethodCall { tags; func; type_args; args } }
| func = lvalue l_angle type_args = typeArgumentList r_angle
    L_PAREN args = argumentList R_PAREN info2 = SEMICOLON
    { let tags = Source.merge (Expression.tags func) info2 in
      Statement.MethodCall { tags; func; type_args; args } }
| lhs = lvalue ASSIGN rhs = expression info2 = SEMICOLON
    { let tags = Source.merge (Expression.tags lhs) info2 in 
      Statement.Assignment { tags; lhs; rhs } }
;

emptyStatement:
| info = SEMICOLON
    { Statement.EmptyStatement { tags = info } }
;

returnStatement:
| info1 = RETURN info2 = SEMICOLON
    { let tags = Source.merge info1 info2 in 
      Statement.Return { tags; expr = None } }
| info1 = RETURN expr = expression info2 = SEMICOLON
    { let tags = Source.merge info1 info2 in
      Statement.Return { tags; expr = Some expr } }
;

exitStatement:
| info1 = EXIT info2 = SEMICOLON
    { let tags = Source.merge info1 info2 in
       Statement.Exit { tags } }
;

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
;

directApplication:
| typ = typeName DOT APPLY
  L_PAREN args = argumentList R_PAREN info2 = SEMICOLON
    { let tags = Source.merge (Type.tags typ) info2 in
      Statement.DirectApplication { tags; typ; args } }
| typ = specializedType DOT APPLY
  L_PAREN args = argumentList R_PAREN info2 = SEMICOLON
    { let tags = Source.merge (Type.tags typ) info2 in
      Statement.DirectApplication { tags; typ; args } }
;

statement:
| s = assignmentOrMethodCallStatement
| s = directApplication
| s = conditionalStatement
| s = emptyStatement
| s = exitStatement
| s = returnStatement
| s = switchStatement
    { s }
| block = blockStatement
    { Statement.BlockStatement { tags = block.Block.tags; block } }
;

blockStatement:
| annotations = optAnnotations
  info1 = L_BRACE
  push_scope
  statements = list(statementOrDeclaration) info2 = R_BRACE
  pop_scope
    { let tags = Source.merge info1 info2 in 
      Block.{ tags; annotations; statements } }
;

switchStatement:
| info1 = SWITCH
  L_PAREN expr = expression R_PAREN
  L_BRACE cases = switchCases info2 = R_BRACE
    { let tags = Source.merge info1 info2 in
      Statement.Switch { tags; expr; cases } }
;

switchCases: cases = list(switchCase) { cases };

switchCase:
| label = switchLabel COLON code = blockStatement
    { let tags = Source.merge (Statement.tags_label label) code.Block.tags in
      Statement.Action { tags; label; code } }
| label = switchLabel info2 = COLON
    { let tags = Source.merge (Statement.tags_label label) info2 in
      Statement.FallThrough { tags; label } }
;

switchLabel:
| expr = nonBraceExpression
    { let tags = Expression.tags expr in
      Statement.Expression { tags; expr } }
| info = DEFAULT
    { Statement.Default { tags = info } }
;

statementOrDeclaration:
| decl = variableDeclaration
| decl = constantDeclaration
    { let tags = Declaration.tags decl in
      Statement.DeclarationStatement { tags; decl } }
| s = statement
    { s }
;

(**************************** TABLES ******************************)

tableDeclaration:
| annotations = optAnnotations
  info1 = TABLE name = name
  L_BRACE properties = tablePropertyList info2 = R_BRACE
    { let tags = Source.merge info1 info2 in
      Declaration.Table { tags; annotations; name; properties } }
;

tablePropertyList:
| props = nonempty_list(tableProperty)
    { props }
;

tableProperty:
| info1 = KEY ASSIGN L_BRACE elts = keyElementList info2 = R_BRACE
    { let tags = Source.merge info1 info2 in
    Table.Key { tags; keys = elts } }
| info1 = ACTIONS ASSIGN L_BRACE acts = actionList info2 = R_BRACE
    { let tags = Source.merge info1 info2 in
      Table.Actions { tags; actions = acts } }
| annos = optAnnotations info1 = CONST ENTRIES ASSIGN L_BRACE entries = entriesList info2 = R_BRACE
    { let tags = Source.merge info1 info2 in
      Table.Entries { tags; entries = entries; const = true; annotations = annos } }
| annos = optAnnotations info1 = ENTRIES ASSIGN L_BRACE entries = entriesList info2 = R_BRACE
    { let tags = Source.merge info1 info2 in
      Table.Entries { tags; entries = entries; const = false; annotations = annos } }
| info1 = CONST DEFAULT_ACTION ASSIGN act = actionRef info2 = SEMICOLON
    { let tags = Source.merge info1 info2 in
      Table.DefaultAction { tags; action = act; const = true } }
| info1 = DEFAULT_ACTION ASSIGN act = actionRef info2 = SEMICOLON
    { let tags = Source.merge info1 info2 in
      Table.DefaultAction { tags; action = act; const = false } }
| annos = optAnnotations
  info1 = CONST n = nonTableKwName ASSIGN v = initialValue info2 = SEMICOLON
    { let tags = Source.merge info1 info2 in
      Table.Custom { tags; annotations = annos; const = true; name = n; value = v } }
| annos = optAnnotations
  n = nonTableKwName ASSIGN v = initialValue info2 = SEMICOLON
    { let tags = Source.merge (Text.tags n) info2 in
      Table.Custom { tags; annotations = annos; const = false; name = n; value = v } }
;

keyElementList: elts = list(keyElement) { elts };

keyElement:
| key = expression COLON
  match_kind = name annotations = optAnnotations info2 = SEMICOLON
    { let tags = Source.merge (Expression.tags key) info2 in
      Table.{ tags; annotations; key; match_kind } }
;

actionList:
| (* empty *)
    { [] }
| acts = separated_nonempty_list_aux(SEMICOLON, actionRef) SEMICOLON
    { List.rev acts }
;

entriesList:
| entries = list(entry) { entries }
;

entry:
| matches = keysetExpression
  info1 = COLON act = actionRef annos = optAnnotations info2 = SEMICOLON
    { let tags = Source.merge info1 info2 in
        Table.{ tags; annotations = annos; matches = matches; action = act; priority = None; const = false } }
| CONST matches = keysetExpression
  info1 = COLON act = actionRef annos = optAnnotations info2 = SEMICOLON
    { let tags = Source.merge info1 info2 in
      Table.{ tags; annotations = annos; matches = matches; action = act; priority = None; const = true } }
| priority = entryPriority matches = keysetExpression
  info1 = COLON act = actionRef annos = optAnnotations info2 = SEMICOLON
    { let tags = Source.merge info1 info2 in
      Table.{ tags; annotations = annos; matches = matches; action = act; priority = Some(priority); const = false } }
| CONST priority = entryPriority matches = keysetExpression
  info1 = COLON act = actionRef annos = optAnnotations info2 = SEMICOLON
    { let tags = Source.merge info1 info2 in
      Table.{ tags; annotations = annos; matches = matches; action = act; priority = Some(priority); const = true } }
;

entryPriority:
| PRIORITY ASSIGN value = NUMBER COLON
    { let value_int = fst value in 
      let tags = Number.tags value_int in 
      Expression.Int { tags; i = value_int } }
| PRIORITY ASSIGN L_PAREN expr = expression R_PAREN COLON
    { expr }

actionRef:
| annotations = optAnnotations name = name
    { let tags = Text.tags name in
      { tags; annotations; name = BareName name; args = [] } }
| annotations = optAnnotations name = name
  L_PAREN args = argumentList info2 = R_PAREN
    { let tags = Source.merge (Text.tags name) info2 in
      { tags; annotations; name = BareName name; args } }
| annotations = optAnnotations
  dotPrefix go_toplevel name = nonTypeName go_local
    { let tags = Text.tags name in
      { tags; annotations; name = QualifiedName ([], name); args = [] } }
| annotations = optAnnotations 
  dotPrefix go_toplevel name = nonTypeName go_local
  L_PAREN args = argumentList info2 = R_PAREN
    { let tags = Source.merge (Text.tags name) info2 in
      { tags; annotations; name = QualifiedName ([], name); args } }
;

(**************************** ACTION ******************************)

actionDeclaration:
| annotations = optAnnotations
  info1 = ACTION name = name L_PAREN params = parameterList R_PAREN
  body = blockStatement
    { let tags = Source.merge info1 body.Block.tags in
      Declaration.Action { tags; annotations; name; params; body } }
;

(**************************** VARIABLES ******************************)

variableDeclaration:
| annotations = optAnnotations
  typ = typeRef name = name init = optInitialValue info2 = SEMICOLON
    { declare_var name false;
      let tags = Source.merge (Type.tags typ) info2 in
      Declaration.Variable { tags; annotations; typ; name; init } }
;

constantDeclaration:
| annotations = optAnnotations
  info1 = CONST typ = typeRef name = name ASSIGN value = initialValue
  info2 = SEMICOLON
    { let tags = Source.merge info1 info2 in
      Declaration.Constant { tags; annotations; typ; name; value } }
;

optInitialValue:
| (* empty *)
    { None }
| ASSIGN v = initialValue
    { Some v }
;

initialValue:
| v = expression
    { v }
;

(**************************** EXPRESSIONS ******************************)

functionDeclaration:
| annotations = optAnnotations func = functionPrototype body = blockStatement pop_scope
    { let (info1, return, name, type_params, params) = func in
      let tags = Source.merge info1 body.Block.tags in
      Declaration.Function { tags; annotations; return; name; type_params; params; body } }
;

argumentList: args = separated_list(COMMA, argument) { args };

argument:
| value = expression
    { let tags = Expression.tags value in
      Argument.Expression { tags; value } }
| key = name ASSIGN value = expression
    { let tags = Source.merge (Text.tags key) (Expression.tags value) in
      Argument.KeyValue { tags; key; value = Some value } }
| info = DONTCARE
    { Argument.Missing { tags = info } }
| key = name ASSIGN info = DONTCARE
    { let tags = Source.merge (Text.tags key) info in
      Argument.KeyValue { tags; key; value = None } }
;

%inline kvPair:
| key = name ASSIGN value = expression 
    { let tags = Source.merge (Text.tags key) (Expression.tags value) in
      KeyValue.{ tags; key; value } }

kvTrailingList:
| kvs = separated_nonempty_trailing_list(COMMA, kvPair)
    { kvs }
;

kvOptTrailingList:
| kvs = separated_nonempty_opt_trailing_list(COMMA, kvPair)
    { kvs }
;

expressionList:
| exprs = separated_list(COMMA, expression) 
    { exprs }
;

expressionOptTrailingList:
| (* empty *)
    { [] }
| exprs = separated_nonempty_opt_trailing_list(COMMA, expression) 
    { exprs }
;

member:
| n = name
    { n }
;

prefixedNonTypeName:
| name = nonTypeName
    { let tags = Text.tags name in
      Expression.Name { tags; name = BareName name } }
| info1 = dotPrefix go_toplevel name = nonTypeName go_local
    { let tags = Source.merge info1 (Text.tags name) in
      Expression.Name { tags; name = QualifiedName ([], name) } }
;

lvalue:
| info1 = THIS
    { let name = Text.{ tags = info1; str = "this" } in
      let tags = Text.tags name in
      Expression.Name { tags; name = BareName name } }
| expr = prefixedNonTypeName
    { expr }
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

nonBraceExpression:
| value = NUMBER
    { let value_int = fst value in 
      let tags = Number.tags value_int in 
      Expression.Int { tags; i = value_int } }
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
| expr = prefixedNonTypeName
    { expr }
| array = nonBraceExpression L_BRACKET index = expression info2 = R_BRACKET
    { let tags = Source.merge (Expression.tags array) info2 in
      Expression.ArrayAccess { tags; array; index } }
| bits = nonBraceExpression L_BRACKET hi = expression COLON lo = expression info2 = R_BRACKET
    { let tags = Source.merge (Expression.tags bits) info2 in
      Expression.BitStringAccess { tags; bits; lo; hi } }
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
| expr = nonBraceExpression DOT name = member
    { let tags = Source.merge (Expression.tags expr) (Text.tags name) in
      Expression.ExpressionMember { tags; expr; name } }
| arg1 = nonBraceExpression op = binop arg2 = expression
    { let tags = Source.merge (Expression.tags arg1) (Expression.tags arg2) in
      Expression.BinaryOp { tags; op; args = (arg1, arg2) } }
| cond = nonBraceExpression QUESTION tru = expression COLON fls = expression
    { let tags = Source.merge (Expression.tags cond) (Expression.tags fls) in
      Expression.Ternary { tags; cond; tru; fls } }
| func = nonBraceExpression l_angle type_args = realTypeArgumentList r_angle
  L_PAREN args = argumentList info2 = R_PAREN
    { let tags = Source.merge (Expression.tags func) info2 in
      Expression.FunctionCall { tags; func; type_args; args } }
| func = nonBraceExpression L_PAREN args = argumentList info2 = R_PAREN
    { let type_args = [] in
      let tags = Source.merge (Expression.tags func) info2 in
      Expression.FunctionCall { tags; func; type_args; args } }
| typ = namedType L_PAREN args = argumentList info2 = R_PAREN
    { let tags = Source.merge (Type.tags typ) info2 in
      Expression.NamelessInstantiation { tags; typ; args } }

%inline binop:
| info = MUL
    { Op.Mul { tags = info } }
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

%inline r_angle:
| info_r = R_ANGLE
    { info_r } 
| info_r = R_ANGLE_SHIFT
    { info_r }

%inline l_angle:
| info_r = L_ANGLE
    { info_r } 
| info_r = L_ANGLE_ARGS
    { info_r }
