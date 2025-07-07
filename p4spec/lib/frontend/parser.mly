%{
open Xl
open El.Ast
open Error
open Util.Source

(* Position handling *)

let position_to_pos position =
  {
    file = position.Lexing.pos_fname;
    line = position.Lexing.pos_lnum;
    column = position.Lexing.pos_cnum - position.Lexing.pos_bol
  }

let positions_to_region position_left position_right =
  {
    left = position_to_pos position_left;
    right = position_to_pos position_right
  }

let at (position_left, position_right) = positions_to_region position_left position_right
let (@@@) it pos = it $ at pos

(* Identifiers *)

module Ids = Set.Make (String)

let vars = ref Ids.empty
let scopes = ref []

let is_var id = Ids.mem id !vars
let add_var id = vars := Ids.add id !vars

let enter_scope () = scopes := !vars :: !scopes
let exit_scope () = vars := List.hd !scopes; scopes := List.tl !scopes

%}

%token TICK_UNDERSCORE TICK_ARROW TICK_DOUBLE_ARROW
%token TICK_DOT TICK_DOT2 TICK_DOT3
%token TICK_COMMA TICK_SEMICOLON TICK_COLON
%token TICK_HASH TICK_DOLLAR TICK_AT TICK_QUEST
%token TICK_BANG TICK_BANG_EQ TICK_TILDE
%token TICK2_LANGLE TICK_LANGLE TICK_LANGLE2 TICK_LANGLE_EQ TICK_LANGLE2_EQ
%token TICK2_RANGLE TICK_RANGLE2 TICK_RANGLE_EQ TICK_RANGLE2_EQ
%token TICK_LPAREN TICK_LBRACK TICK2_LBRACK TICK2_RBRACK
%token TICK_LBRACE TICK_LBRACE_HASH_RBRACE TICK2_LBRACE TICK2_RBRACE
%token TICK_PLUS TICK_PLUS2 TICK_PLUS_EQ TICK_MINUS TICK_MINUS_EQ
%token TICK_STAR TICK_STAR_EQ TICK_SLASH TICK_SLASH_EQ
%token TICK_PERCENT TICK_PERCENT_EQ TICK_EQ TICK_EQ2
%token TICK_AMP TICK_AMP2 TICK_AMP3 TICK_AMP_EQ
%token TICK_UP TICK_UP_EQ
%token TICK_BAR TICK_BAR2 TICK_BAR_EQ
%token TICK_BAR_PLUS_BAR TICK_BAR_PLUS_BAR_EQ TICK_BAR_MINUS_BAR TICK_BAR_MINUS_BAR_EQ

%token NL_BAR NL2 NL3
%token SUB SUP TURNSTILE TILESTURN ENTAIL
%token ARROW ARROW_SUB
%token DOUBLE_ARROW DOUBLE_ARROW_SUB DOUBLE_ARROW_BOTH
%token SQARROW SQARROW_STAR
%token AND OR
%token DOT DOT2 DOT3
%token COMMA COMMA_NL SEMICOLON COLON COLON2 COLON_SLASH
%token HASH HASH2 DOLLAR QUEST TILDE TILDE2
%token LANGLE LANGLE_DASH LANGLE_EQ
%token RANGLE RANGLE_EQ RANGLE_LPAREN
%token LPAREN RPAREN LBRACK RBRACK LBRACE RBRACE
%token PLUS PLUS2 MINUS DASH STAR SLASH BACKSLASH
%token HOLE
%token<int> HOLE_NUM
%token HOLE_MULTI HOLE_NIL
%token EQ NEQ UP BAR
%token LATEX BOOL NAT INT TEXT
%token SYNTAX RELATION RULE VAR DEC DEF
%token IF OTHERWISE HINT_LPAREN EPS
%token<bool> BOOLLIT
%token<Bigint.t> NATLIT HEXLIT
%token<string> TEXTLIT
%token<string> UPID LOID DOTID
%token<string> UPID_LPAREN LOID_LPAREN UPID_LANGLE LOID_LANGLE
%token EOF

%right DOUBLE_ARROW DOUBLE_ARROW_BOTH DOUBLE_ARROW_SUB
%left OR
%left AND
%nonassoc TURNSTILE
%nonassoc TILESTURN
%right SQARROW SQARROW_STAR
%left COLON SUB SUP TILDE2
%right EQ NEQ LANGLE RANGLE LANGLE_EQ RANGLE_EQ LANGLE_DASH
%right COLON2
%right ARROW ARROW_SUB
%left SEMICOLON
%left DOT DOT2 DOT3
%left PLUS MINUS PLUS2 
%left STAR SLASH BACKSLASH

%start spec check_atom
%type<El.Ast.spec> spec
%type<bool> check_atom

%%

(* Scopes *)

enter_scope :
  | (* empty *) { enter_scope () }
exit_scope :
  | (* empty *) { exit_scope () }

check_atom :
  | UPID EOF { is_var (Var.strip_var_suffix ($1 @@@ $sloc)).it }

(* Lists *)

%inline bar :
  | BAR {}
  | NL_BAR {}

%inline comma :
  | COMMA {}
  | COMMA_NL {}

comma_list(X) :
  | (* empty *) { [] }
  | X { [ $1 ] }
  | X comma comma_list(X) { $1 :: $3 }

bar_list(X) :
  | (* empty *) { [] }
  | X { [ $1 ] }
  | X bar bar_list(X) { $1 :: $3 }

dash_list(X) :
  | (* empty *) { [] }
  | DASH DASH dash_list(X) { $3 }
  | DASH X dash_list(X) { $2 :: $3 }

(* Identifiers *)

id : UPID { $1 } | LOID { $1 }
id_lparen : UPID_LPAREN { $1 } | LOID_LPAREN { $1 }
id_langle : UPID_LANGLE { $1 } | LOID_LANGLE { $1 }

varid : LOID { $1 @@@ $sloc }
varid_bind :
  | varid { add_var $1.it; $1 }
  | atomid_ { add_var $1; $1 @@@ $sloc }
varid_langle : LOID_LANGLE { $1 @@@ $sloc }
varid_langle_bind :
  | varid_langle { add_var $1.it; $1 }
  | atomid_langle { add_var $1; $1 @@@ $sloc }

atomid_ : UPID { $1 }
atomid_lparen : UPID_LPAREN { $1 }
atomid_langle : UPID_LANGLE { $1 }
atomid : atomid_ { $1 } | atomid DOTID { $1 ^ "." ^ $2 }

dotid : DOTID { Atom.Atom $1 @@@ $sloc }

fieldid :
  | atomid_ { Atom.Atom $1 @@@ $sloc }

relid : id { $1 @@@ $sloc }

ruleid : ruleid_ { $1 }
ruleid_ :
  | id { $1 }
  | NATLIT { Bigint.to_string $1 }
  | BOOLLIT { Bool.string_of_bool $1 }
  | ruleid_ DOTID { $1 ^ "." ^ $2 }
ruleids :
  | (* empty *) { "" }
  | SLASH ruleid ruleids { "/" ^ $2 ^ $3 }
  | MINUS ruleid ruleids { "-" ^ $2 ^ $3 }

defid : id { $1 @@@ $sloc }
defid_lparen : id_lparen { $1 @@@ $sloc }
defid_langle : id_langle { $1 @@@ $sloc }

hintid : id { $1 }

synid :
  | varid { ($1, []) }
  | varid_langle_bind enter_scope comma_list(tparam) exit_scope RANGLE { ($1, $3) }

(* Atoms *)

atom :
  | atom_ { $1 @@@ $sloc }
atom_ :
  | atomid { Atom.Atom $1 }
  | atom_escape { $1 }
atom_escape :
  | TICK_UNDERSCORE { Atom.Underscore }
  | TICK_ARROW { Atom.Arrow }
  | TICK_DOUBLE_ARROW { Atom.DoubleArrow }
  | TICK_DOT { Atom.Dot }
  | TICK_DOT2 { Atom.Dot2 }
  | TICK_DOT3 { Atom.Dot3 }
  | TICK_COMMA { Atom.Comma }
  | TICK_SEMICOLON { Atom.Semicolon }
  | TICK_COLON { Atom.Colon }
  | TICK_HASH { Atom.Hash }
  | TICK_DOLLAR { Atom.Dollar }
  | TICK_AT { Atom.At }
  | TICK_QUEST { Atom.Quest }
  | TICK_BANG { Atom.Bang }
  | TICK_BANG_EQ { Atom.BangEq }
  | TICK_TILDE { Atom.Tilde }
  | TICK2_LANGLE { Atom.LAngle }
  | TICK_LANGLE2 { Atom.LAngle2 }
  | TICK_LANGLE_EQ { Atom.LAngleEq }
  | TICK_LANGLE2_EQ { Atom.LAngle2Eq }
  | TICK2_RANGLE { Atom.RAngle }
  | TICK_RANGLE2 { Atom.RAngle2 }
  | TICK_RANGLE_EQ { Atom.RAngleEq }
  | TICK_RANGLE2_EQ { Atom.RAngle2Eq }
  | TICK2_LBRACK { Atom.LBrack }
  | TICK2_RBRACK { Atom.RBrack }
  | TICK2_LBRACE { Atom.LBrace }
  | TICK_LBRACE_HASH_RBRACE { Atom.LBraceHashRBrace }
  | TICK2_RBRACE { Atom.RBrace }
  | TICK_PLUS { Atom.Plus }
  | TICK_PLUS2 { Atom.Plus2 }
  | TICK_PLUS_EQ { Atom.PlusEq }
  | TICK_MINUS { Atom.Minus }
  | TICK_MINUS_EQ { Atom.MinusEq }
  | TICK_STAR { Atom.Star }
  | TICK_STAR_EQ { Atom.StarEq }
  | TICK_SLASH { Atom.Slash }
  | TICK_SLASH_EQ { Atom.SlashEq }
  | TICK_PERCENT { Atom.Percent }
  | TICK_PERCENT_EQ { Atom.PercentEq }
  | TICK_EQ { Atom.Eq }
  | TICK_EQ2 { Atom.Eq2 }
  | TICK_AMP { Atom.Amp }
  | TICK_AMP2 { Atom.Amp2 }
  | TICK_AMP3 { Atom.Amp3 }
  | TICK_AMP_EQ { Atom.AmpEq }
  | TICK_UP { Atom.Up }
  | TICK_UP_EQ { Atom.UpEq }
  | TICK_BAR { Atom.Bar }
  | TICK_BAR2 { Atom.Bar2 }
  | TICK_BAR_EQ { Atom.BarEq }
  | TICK_BAR_PLUS_BAR { Atom.SPlus }
  | TICK_BAR_PLUS_BAR_EQ { Atom.SPlusEq }
  | TICK_BAR_MINUS_BAR { Atom.SMinus }
  | TICK_BAR_MINUS_BAR_EQ { Atom.SMinusEq }

(* Iterations *)

iter :
  | QUEST { Opt }
  | STAR { List }

(* Types *)

plaintyp_prim_ :
  | varid { VarT ($1, []) }
  | varid_langle comma_list(targ) RANGLE { VarT ($1, $2) }
  | BOOL { BoolT }
  | NAT { NumT `NatT }
  | INT { NumT `IntT }
  | TEXT { TextT }

plaintyp : plaintyp_ { $1 @@@ $sloc }
plaintyp_ :
  | plaintyp_prim_ { $1 }
  | LPAREN comma_list(plaintyp) RPAREN
    { match $2 with
      | [] -> ParenT (TupleT [] @@@ $sloc)
      | [ typ ] -> ParenT typ
      | typs -> TupleT typs }
  | plaintyp iter { IterT ($1, $2) }

nottyp :
  | typ
    {
      match $1 with
      | NotationT nottyp -> nottyp
      | _ -> error (at $sloc) "expected notation type"
    }

typ_prim : typ_prim_ { $1 }
typ_prim_ :
  | plaintyp
    {
      PlainT $1
    }
  | atom
    {
      NotationT (AtomT $1 @@@ $loc($1))
    }
  | TICK_LANGLE typ RANGLE
    {
      NotationT (BrackT (Atom.LAngle @@@ $loc($1), $2, Atom.RAngle @@@ $loc($3)) @@@ $loc($1))
    }
  | TICK_LPAREN typ RPAREN
    {
      NotationT (BrackT (Atom.LParen @@@ $loc($1), $2, Atom.RParen @@@ $loc($3)) @@@ $loc($1))
    }
  | TICK_LBRACK typ RBRACK
    {
      NotationT (BrackT (Atom.LBrack @@@ $loc($1), $2, Atom.RBrack @@@ $loc($3)) @@@ $loc($1))
    }
  | TICK_LBRACE typ RBRACE
    {
      NotationT (BrackT (Atom.LBrace @@@ $loc($1), $2, Atom.RBrace @@@ $loc($3)) @@@ $loc($1))
    }

typ_seq : typ_seq_ { $1 }
typ_seq_ :
  | typ_prim_ { $1 }
  | typ_prim typ_seq
    {
      let typ = $1 in
      let typs =
        match $2 with
        | NotationT ({ it = SeqT (_ :: _ :: _ as typs); _ }) -> typs
        | _ -> [ $2 ]
      in
      NotationT (SeqT (typ :: typs) @@@ $loc($1))
    }

typ_un : typ_un_ { $1 }
typ_un_ :
  | typ_seq_ { $1 }
  | infixop typ_un
    {
      NotationT (InfixT (NotationT (SeqT [] @@@ $loc($1)), $1, $2) @@@ $loc($1))
    }

typ_bin : typ_bin_ { $1 }
typ_bin_ :
  | typ_un_ { $1 }
  | typ_bin infixop typ_bin
    {
      NotationT (InfixT ($1, $2, $3) @@@ $loc($2))
    }

typ_rel : typ_rel_ { $1 }
typ_rel_ :
  | typ_bin_ { $1 }
  | relop typ_rel
    {
      NotationT (InfixT (NotationT (SeqT [] @@@ $loc($1)), $1, $2) @@@ $loc($1))
    }
  | typ_rel relop typ_rel
    {
      NotationT (InfixT ($1, $2, $3) @@@ $loc($2))
    }

typ : typ_rel { $1 }

(* Type definitions *)

fieldtyp :
  | fieldid plaintyp hint* { ($1, $2, $3) }

casetyp :
  | typ hint* { ($1, $2) }

deftyp : deftyp_ { $1 @@@ $sloc }
deftyp_ :
  | LBRACE comma_list(fieldtyp) RBRACE
    { 
      match $2 with
      | [] -> error (at $sloc) "empty struct type"
      | _ -> StructTD $2
    }
  | bar bar_list(casetyp)
    {
      match $2 with
      | [] -> error (at $sloc) "empty variant type"
      | [ (PlainT plaintyp, hints) ] ->
          if hints <> [] then
            error (at $sloc) "hints not allowed in plain type definition";
          PlainTD plaintyp
      | _ ->
          List.iter
            (fun (typ, hints) ->
              match typ with
              | PlainT _ when hints <> [] -> error (at $sloc) "hints not allowed in plain type definition"
              | _ -> ())
            $2;
          VariantTD $2
    }
  | bar_list(casetyp)
    {
      match $1 with
      | [] -> error (at $sloc) "empty type"
      | [ (PlainT plaintyp, hints) ] ->
          if hints <> [] then
            error (at $sloc) "hints not allowed in plain type definition";
          PlainTD plaintyp
      | _ ->
          List.iter
            (fun (typ, hints) ->
              match typ with
              | PlainT _ when hints <> [] -> error (at $sloc) "hints not allowed in plain type definition"
              | _ -> ())
            $1;
          VariantTD $1
    }

(* Operations *)

%inline unop :
  | TILDE { `NotOp }
  | PLUS { `PlusOp }
  | MINUS { `MinusOp }

%inline binop :
  | PLUS { `AddOp }
  | MINUS { `SubOp }
  | STAR { `MulOp }
  | SLASH { `DivOp }
  | BACKSLASH { `ModOp }

%inline cmpop :
  | EQ { `EqOp }
  | NEQ { `NeOp }

%inline cmpop_arith :
  | EQ { `EqOp }
  | NEQ { `NeOp }
  | LANGLE { `LtOp }
  | RANGLE { `GtOp }
  | LANGLE_EQ { `LeOp }
  | RANGLE_EQ { `GeOp }

%inline boolop :
  | AND { `AndOp }
  | OR { `OrOp }
  | DOUBLE_ARROW { `ImplOp }
  | DOUBLE_ARROW_BOTH { `EquivOp }

%inline infixop :
  | infixop_ { $1 @@@ $sloc }
%inline infixop_ :
  | DOT { Atom.Dot }
  | DOT2 { Atom.Dot2 }
  | DOT3 { Atom.Dot3 }
  | SEMICOLON { Atom.Semicolon }
  | BACKSLASH { Atom.Backslash }
  | ARROW { Atom.Arrow }
  | ARROW_SUB { Atom.ArrowSub }
  | DOUBLE_ARROW_SUB { Atom.DoubleArrowSub }

%inline relop :
  | relop_ { $1 @@@ $sloc }
%inline relop_ :
  | COLON { Atom.Colon }
  | SUB { Atom.Sub }
  | SUP { Atom.Sup }
  | TILDE2 { Atom.Tilde2 }
  | SQARROW { Atom.SqArrow }
  | SQARROW_STAR { Atom.SqArrowStar }
  | TILESTURN { Atom.Tilesturn }
  | TURNSTILE { Atom.Turnstile }

(* Arithmetics *)

arith_prim : arith_prim_ { $1 @@@ $sloc }
arith_prim_ :
  | exp_lit_ { $1 }
  | exp_var_ { $1 }
  | exp_call_ { $1 }
  | exp_hole_ { $1 }
  | LPAREN arith RPAREN { ParenE $2 }
  | DOLLAR LPAREN exp RPAREN { $3.it }

arith_post : arith_post_ { $1 @@@ $sloc }
arith_post_ :
  | arith_prim_ { $1 }
  | arith_atom UP arith_prim { BinE ($1, `PowOp, $3) }
  | arith_atom LBRACK arith RBRACK { IdxE ($1, $3) }
  | arith_post dotid { DotE ($1, $2) }

arith_atom : arith_atom_ { $1 @@@ $sloc }
arith_atom_ :
  | arith_post_ { $1 }
  | atom { AtomE $1 }

arith_un : arith_un_ { $1 @@@ $sloc }
arith_un_ :
  | arith_atom_ { $1 }
  | bar exp bar { LenE $2 }
  | unop arith_un { UnE ($1, $2) }

arith_bin : arith_bin_ { $1 @@@ $sloc }
arith_bin_ :
  | arith_un_ { $1 }
  | arith_bin binop arith_bin { BinE ($1, $2, $3) }
  | arith_bin cmpop_arith arith_bin { CmpE ($1, $2, $3) }
  | arith_bin boolop arith_bin { BinE ($1, $2, $3) }
  | arith_bin PLUS2 arith_bin { CatE ($1, $3) }
  | arith_bin LANGLE_DASH arith_bin { MemE ($1, $3) }

arith : arith_bin { $1 }

(* Expressions *)

exp_lit_ :
  | BOOLLIT { BoolE $1 }
  | NATLIT { NumE (`DecOp, `Nat $1) }
  | HEXLIT { NumE (`HexOp, `Nat $1) }
  | TEXTLIT { TextE $1 }

exp_var_ :
  | varid { VarE $1 }
  | BOOL { VarE ("bool" @@@ $sloc) }
  | NAT { VarE ("nat" @@@ $sloc) }
  | INT { VarE ("int" @@@ $sloc) }
  | TEXT { VarE ("text" @@@ $sloc) }

exp_call_ :
  | DOLLAR defid { CallE ($2, [], []) }
  | DOLLAR defid_lparen comma_list(arg) RPAREN
    { CallE ($2, [], $3) }
  | DOLLAR defid_langle comma_list(targ) RANGLE 
    { CallE ($2, $3, []) }
  | DOLLAR defid_langle comma_list(targ) RANGLE_LPAREN comma_list(arg) RPAREN
    { CallE ($2, $3, $5) }

exp_hole_ :
  | HOLE_NUM { HoleE (`Num $1) }
  | HOLE { HoleE `Next }
  | HOLE_MULTI { HoleE `Rest }
  | HOLE_NIL { HoleE `None }
  | LATEX LPAREN list(TEXTLIT) RPAREN { LatexE (String.concat " " $3) }

fieldexp :
  | fieldid exp_atom+
    { ($1, match $2 with [ exp ] -> exp | exps -> SeqE exps @@@ $loc($2)) }

exp_prim : exp_prim_ { $1 @@@ $sloc }
exp_prim_ :
  | exp_lit_ { $1 }
  | exp_var_ { $1 }
  | exp_call_ { $1 }
  | exp_hole_ { $1 }
  | EPS { EpsE }
  | LBRACE comma_list(fieldexp) RBRACE { StrE $2 }
  | LPAREN comma_list(exp_bin) RPAREN
    { 
      match $2 with
      | [] -> ParenE (TupleE [] @@@ $sloc)
      | [ exp ] -> ParenE exp
      | exps -> TupleE exps
    }
  | TICK_LANGLE exp RANGLE
    { BrackE (Atom.LAngle @@@ $loc($1), $2, Atom.RAngle @@@ $loc($3)) }
  | TICK_LPAREN exp RPAREN
    { BrackE (Atom.LParen @@@ $loc($1), $2, Atom.RParen @@@ $loc($3)) }
  | TICK_LBRACK exp RBRACK
    { BrackE (Atom.LBrack @@@ $loc($1), $2, Atom.RBrack @@@ $loc($3)) }
  | TICK_LBRACE exp RBRACE
    { BrackE (Atom.LBrace @@@ $loc($1), $2, Atom.RBrace @@@ $loc($3)) }
  | DOLLAR LPAREN arith RPAREN { $3.it }
  | HASH2 exp_prim { UnparenE $2 }

exp_post : exp_post_ { $1 @@@ $sloc }
exp_post_ :
  | exp_prim_ { $1 }
  | exp_atom LBRACK arith RBRACK { IdxE ($1, $3) }
  | exp_atom LBRACK arith COLON arith RBRACK { SliceE ($1, $3, $5) }
  | exp_atom LBRACK path EQ exp RBRACK { UpdE ($1, $3, $5) }
  | exp_atom iter { IterE ($1, $2) }
  | exp_post dotid { DotE ($1, $2) }

exp_atom : exp_atom_ { $1 @@@ $sloc }
exp_atom_ :
  | exp_post_ { $1 }
  | atom { AtomE $1 }
  | atomid_lparen exp RPAREN
    { SeqE [
        AtomE (Atom.Atom $1 @@@ $loc($1)) @@@ $loc($1);
        ParenE $2 @@@ $loc($2)
      ] }

exp_list : exp_list_ { $1 @@@ $sloc }
exp_list_ :
  | LBRACK comma_list(exp_bin) RBRACK { ListE $2 }
  | exp_list iter { IterE ($1, $2) }

exp_seq : exp_seq_ { $1 @@@ $sloc }
exp_seq_ :
  | exp_atom_ { $1 }
  | exp_list_ { $1 }
  | exp_seq exp_atom
    {
      let exps =
        match $1.it with
        | SeqE (_ :: _ :: _ as exps) -> exps
        | _ -> [ $1 ]
      in
      SeqE (exps @ [ $2 ])
    }
  | exp_seq HASH exp_atom { FuseE ($1, $3) }

exp_un : exp_un_ { $1 @@@ $sloc }
exp_un_ :
  | exp_seq_ { $1 }
  | bar exp bar { LenE $2 }
  | unop exp_un { UnE ($1, $2) }
  | infixop exp_un { InfixE (SeqE [] @@@ $loc($1), $1, $2) }

exp_bin : exp_bin_ { $1 @@@ $sloc }
exp_bin_ :
  | exp_un_ { $1 }
  | exp_bin infixop exp_bin { InfixE ($1, $2, $3) }
  | exp_bin cmpop exp_bin { CmpE ($1, $2, $3) }
  | exp_bin boolop exp_bin { BinE ($1, $2, $3) }
  | exp_bin COLON2 exp_bin { ConsE ($1, $3) }
  | exp_bin PLUS2 exp_bin { CatE ($1, $3) }
  | exp_bin LANGLE_DASH exp_bin { MemE ($1, $3) }

exp_rel : exp_rel_ { $1 @@@ $sloc }
exp_rel_ :
  | exp_bin_ { $1 }
  | relop exp_rel { InfixE (SeqE [] @@@ $loc($1), $1, $2) }
  | exp_rel relop exp_rel { InfixE ($1, $2, $3) }

exp : exp_rel { $1 }

(* Paths *)

path : path_ { $1 @@@ $sloc }
path_ :
  | (* empty *) { RootP }
  | path LBRACK arith RBRACK { IdxP ($1, $3) }
  | path LBRACK arith COLON arith RBRACK { SliceP ($1, $3, $5) }
  | path dotid { DotP ($1, $2) }

(* Parameters *)

param : param_ { $1 @@@ $sloc }
param_ :
  | plaintyp { ExpP $1 }
  | DEF DOLLAR defid COLON plaintyp
    { DefP ($3, [], [], $5) }
  | DEF DOLLAR defid_lparen enter_scope comma_list(param) RPAREN COLON plaintyp exit_scope
    { DefP ($3, [], $5, $8) }
  | DEF DOLLAR defid_langle enter_scope comma_list(tparam) RANGLE COLON plaintyp exit_scope
    { DefP ($3, $5, [], $8) }
  | DEF DOLLAR defid_langle enter_scope comma_list(tparam) RANGLE_LPAREN comma_list(param) RPAREN COLON plaintyp exit_scope
    { DefP ($3, $5, $7, $10) }

(* Type parameters *)

tparam : varid_bind { $1 }

(* Arguments *)

arg : arg_ { $1 @@@ $sloc }
arg_ :
  | exp_bin { ExpA $1 }
  | DEF DOLLAR defid { DefA $3 }

(* Type arguments *)

targ : plaintyp { $1 }

(* Premises *)

prem_list :
  | enter_scope dash_list(prem) exit_scope { $2 }

prem_post_ :
  | OTHERWISE { ElsePr }
  | LPAREN prem RPAREN iter*
    { 
      let rec iterate prem = function
        | [] -> prem
        | iter :: iters -> iterate (IterPr (prem, iter) @@@ $sloc) iters
      in
      (iterate $2 $4).it
    }

prem : prem_ { $1 @@@ $sloc }
prem_ :
  | prem_post_ { $1 }
  | relid COLON exp { RulePr ($1, $3) }
  | relid COLON_SLASH exp { RuleNotPr ($1, $3) }
  | VAR varid_bind COLON plaintyp { VarPr ($2, $4) }
  | IF exp
    { 
      let rec iterate exp =
        match exp.it with
        | IterE (exp, iter) -> IterPr ((iterate exp $ exp.at), iter)
        | _ -> IfPr exp
      in
      iterate $2
    }

(* Hints *)

hint :
  | HINT_LPAREN hintid exp RPAREN
    { { hintid = $2 @@@ $loc($2); hintexp = $3 } }
  | HINT_LPAREN hintid RPAREN
    { { hintid = $2 @@@ $loc($2); hintexp = SeqE [] @@@ $loc($2) } }

(* Definitions *)

def :
  | def_ NL2* { $1 @@@ $loc($1) }
def_ :
  | SYNTAX comma_list(synid)
    {
      match $2 with
      | [] -> error (at $sloc) "empty syntax declaration"
      | _ -> SynD $2
    }
  | SYNTAX varid_bind hint* EQ deftyp
    { TypD ($2, [], $5, $3) }
  | SYNTAX varid_langle_bind enter_scope comma_list(tparam) RANGLE hint* EQ deftyp exit_scope
    { TypD ($2, $4, $8, $6) }
  | VAR varid_bind COLON plaintyp hint*
    { VarD ($2, $4, $5) }
  | RELATION relid COLON nottyp hint*
    { RelD ($2, $4, $5) }
  | RULE relid ruleids COLON exp prem_list
    { let id = if $3 = "" then "" else String.sub $3 1 (String.length $3 - 1) in
      RuleD ($2, id @@@ $loc($3), $5, $6) }
  | DEC DOLLAR defid COLON plaintyp hint*
    { DecD ($3, [], [], $5, $6) }
  | DEC DOLLAR defid_lparen enter_scope comma_list(param) RPAREN COLON plaintyp hint* exit_scope
    { DecD ($3, [], $5, $8, $9) }
  | DEC DOLLAR defid_langle enter_scope comma_list(tparam) RANGLE COLON plaintyp hint* exit_scope
    { DecD ($3, $5, [], $8, $9) }
  | DEC DOLLAR defid_langle enter_scope comma_list(tparam) RANGLE_LPAREN comma_list(param) RPAREN COLON plaintyp hint* exit_scope
    { DecD ($3, $5, $7, $10, $11) }
  | DEF DOLLAR defid EQ exp prem_list
    { DefD ($3, [], [], $5, $6) }
  | DEF DOLLAR defid_lparen enter_scope comma_list(arg) RPAREN EQ exp prem_list exit_scope
    { DefD ($3, [], $5, $8, $9) }
  | DEF DOLLAR defid_langle enter_scope comma_list(tparam) RANGLE EQ exp prem_list exit_scope
    { DefD ($3, $5, [], $8, $9) }
  | DEF DOLLAR defid_langle enter_scope comma_list(tparam) RANGLE_LPAREN comma_list(arg) RPAREN EQ exp prem_list exit_scope
    { DefD ($3, $5, $7, $10, $11) }
  | NL3
    { SepD }

(* Spec *)

spec :
  | NL2* def* EOF { $2 }
