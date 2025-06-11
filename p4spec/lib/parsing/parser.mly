%{
  open Il.Ast
  open Util.Source
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

%left OR
%left AND
%left EQ NE
%left L_ANGLE R_ANGLE LE GE
%left BIT_OR
%left BIT_XOR
%left BIT_AND
%left SHL R_ANGLE_SHIFT
%left PLUS MINUS PLUS_SAT MINUS_SAT
%left MUL DIV MOD
%right PREFIX
%nonassoc L_PAREN L_BRACKET
%left DOT

%start <Il.Ast.value> p4program

%%

(**************************** P4-16 GRAMMAR ******************************)

p4program:
| declarations = declaration_list END
    { (* Create a list of declarations *)
      let value =
        let vid = Value.fresh () in
        let typ = Il.Ast.IterT (Il.Ast.VarT ("decl" $ no_region, []) $ no_region, List) in
        ListV declarations $$$ { vid; typ }
      in
      value }
| END
    { (* Empty program *)
      let value =
        let vid = Value.fresh () in
        let typ = Il.Ast.IterT (Il.Ast.VarT ("decl" $ no_region, []) $ no_region, List) in
        ListV [] $$$ { vid; typ }
      in
      value }

declaration_list:
| (* empty *) { [] }
| decl = declaration rest = declaration_list { decl :: rest }

declaration:
| expr = expression SEMICOLON { expr }
| name = NAME SEMICOLON { name }

expression:
| value = NUMBER { fst value }
| name = NAME { name }
| TRUE 
    { let value =
        let vid = Value.fresh () in
        let typ = Il.Ast.BoolT in
        BoolV true $$$ { vid; typ }
      in
      value }
| FALSE
    { let value =
        let vid = Value.fresh () in
        let typ = Il.Ast.BoolT in
        BoolV false $$$ { vid; typ }
      in
      value }
| L_PAREN expr = expression R_PAREN { expr } 