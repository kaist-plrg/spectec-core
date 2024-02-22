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
module P4Cherry = struct end
open Ast

let rec _smash_annotations (l: Text.t list) (tok2: Text.t): Text.t list =
  match l with 
  | [] -> [ tok2 ]
  | [ tok1 ] ->
     if Info.follows tok1.tags tok2.tags then
       [{ tags = Info.merge tok1.tags tok2.tags;
         str = tok1.str ^ tok2.str }]
     else
       [ tok1; tok2 ]
  | h :: t -> h :: _smash_annotations t tok2

%}

(*************************** TOKENS *******************************)
%token<Info.t> END
%token TYPENAME IDENTIFIER
%token<Text.t> NAME STRING_LITERAL
%token<Number.t * string> NUMBER
%token<Info.t> LE GE SHL AND OR NE EQ
%token<Info.t> PLUS MINUS PLUS_SAT MINUS_SAT MUL DIV MOD
%token<Info.t> BIT_OR BIT_AND BIT_XOR COMPLEMENT
%token<Info.t> L_BRACKET R_BRACKET L_BRACE R_BRACE L_ANGLE L_ANGLE_ARGS R_ANGLE R_ANGLE_SHIFT L_PAREN R_PAREN
%token<Info.t> ASSIGN COLON COMMA QUESTION DOT NOT SEMICOLON
%token<Info.t> AT PLUSPLUS
%token<Info.t> DONTCARE
%token<Info.t> MASK RANGE
%token<Info.t> TRUE FALSE
%token<Info.t> ABSTRACT ACTION ACTIONS APPLY BOOL BIT CONST CONTROL DEFAULT DEFAULT_ACTION
%token<Info.t> ELSE ENTRIES ENUM ERROR EXIT EXTERN HEADER HEADER_UNION IF IN INOUT
%token<Info.t> INT KEY SELECT MATCH_KIND OUT PACKAGE PARSER RETURN STATE STRING STRUCT
%token<Info.t> SWITCH TABLE THEN TRANSITION TUPLE TYPE TYPEDEF VARBIT VALUESET VOID
%token<Info.t> PRAGMA PRAGMA_END
%token<Text.t> UNEXPECTED_TOKEN

(********************** PRIORITY AND ASSOCIATIVITY ************************)
%right THEN ELSE   (* Precedence of THEN token is artificial *)
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


%start <Ast.program> p4program

%%

(********************************** CONTEXTS ***********************************)

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

(************************************ LISTS **************************************)

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

(*p4program : ds = topDeclarationList END { Program(ds) };*)
p4program: _ds = END { Program([]) };

varName:
| id = NAME IDENTIFIER
    { id }
;

tableKwName:
| info = KEY
    {{ str = "key"; tags = info}}
| info = ACTIONS
    {{ str = "actions"; tags = info}}
| info = ENTRIES
    {{ str = "entries"; tags = info}}
;

nonTypeName:
| n = varName
    { n }
| n = tableKwName
    { n }
| info = APPLY
    {{ str = "apply"; tags = info}}
| info = STATE
    {{ str = "state"; tags = info}}
| info = TYPE
    {{ str = "type"; tags = info}}
;

name:
| n = nonTypeName
| n = NAME TYPENAME
    { n }
;

(**************************** EXTERN ******************************)

externName:
| n = nonTypeName
    { declare_type n false; n }
