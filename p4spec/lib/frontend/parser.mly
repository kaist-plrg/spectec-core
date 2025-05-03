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

%token LPAREN RPAREN LBRACK RBRACK LBRACE RBRACE
%token COLON COLONCOLON COLONSLASH SEMICOLON COMMA DOT DOTDOT DOTDOTDOT BAR DASH
%token BIGAND BIGOR BIGADD BIGMUL BIGCAT
%token COMMA_NL NL_BAR NL_NL NL_NL_NL
%token EQ NE LANGLE RANGLE RANGLE_LPAREN LE GE APPROX EQUIV ASSIGN SUB SUP
%token NOT AND OR
%token QUEST PLUS MINUS STAR SLASH BACKSLASH UP CAT
%token ARROW ARROW2 ARROWSUB ARROW2SUB DARROW2 SQARROW SQARROWSTAR
%token MEM PREC SUCC TURNSTILE TILESTURN
%token DOLLAR TICK
%token BOT TOP
%token HOLE MULTIHOLE NOTHING FUSE FUSEFUSE LATEX
%token<int> HOLEN
%token BOOL NAT INT TEXT
%token SYNTAX RELATION RULE VAR DEC DEF
%token IF OTHERWISE HINT_LPAREN
%token EPS INFINITY
%token<bool> BOOLLIT
%token<Bigint.t> NATLIT HEXLIT
%token<string> TEXTLIT
%token<string> UPID LOID DOTID UPID_LPAREN LOID_LPAREN UPID_LANGLE LOID_LANGLE
%token EOF

%right ARROW2 DARROW2 ARROW2SUB
%left OR
%left AND
%nonassoc TURNSTILE
%nonassoc TILESTURN
%right SQARROW SQARROWSTAR PREC SUCC BIGAND BIGOR BIGADD BIGMUL BIGCAT
%left COLON SUB SUP ASSIGN EQUIV APPROX
%right EQ NE LANGLE RANGLE LE GE MEM
%right COLONCOLON
%right ARROW ARROWSUB
%left SEMICOLON
%left DOT DOTDOT DOTDOTDOT
%left PLUS MINUS CAT
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
  | TICK EQ { Atom.Equal }
  | TICK NE { Atom.NotEqual }
  | TICK LANGLE { Atom.Less }
  | TICK RANGLE { Atom.Greater }
  | TICK LE { Atom.LessEqual }
  | TICK GE { Atom.GreaterEqual }
  | TICK MEM { Atom.Mem }
  | TICK QUEST { Atom.Quest }
  | TICK PLUS { Atom.Plus }
  | TICK STAR { Atom.Star }
  | TICK BAR { Atom.Bar }
  | TICK CAT { Atom.Cat }
  | TICK COMMA { Atom.Comma }
  | TICK ARROW2 { Atom.Arrow2 }
  | TICK infixop_ { $2 }
  | TICK relop_ { $2 }
  | BOT { Atom.Bot }
  | TOP { Atom.Top }
  | INFINITY { Atom.Infinity }

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
  | TICK LPAREN typ RPAREN
    {
      NotationT (BrackT (Atom.LParen @@@ $loc($2), $3, Atom.RParen @@@ $loc($4)) @@@ $loc($1))
    }
  | TICK LBRACK typ RBRACK
    {
      NotationT (BrackT (Atom.LBrack @@@ $loc($2), $3, Atom.RBrack @@@ $loc($4)) @@@ $loc($1))
    }
  | TICK LBRACE typ RBRACE
    {
      NotationT (BrackT (Atom.LBrace @@@ $loc($2), $3, Atom.RBrace @@@ $loc($4)) @@@ $loc($1))
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
  | NOT { `NotOp }
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
  | NE { `NeOp }
  | LANGLE { `LtOp }
  | RANGLE { `GtOp }
  | LE { `LeOp }
  | GE { `GeOp }

%inline boolop :
  | AND { `AndOp }
  | OR { `OrOp }
  | ARROW2 { `ImplOp }
  | DARROW2 { `EquivOp }

%inline infixop :
  | infixop_ { $1 @@@ $sloc }
%inline infixop_ :
  | DOT { Atom.Dot }
  | DOTDOT { Atom.Dot2 }
  | DOTDOTDOT { Atom.Dot3 }
  | SEMICOLON { Atom.Semicolon }
  | BACKSLASH { Atom.Backslash }
  | ARROW { Atom.Arrow }
  | ARROWSUB { Atom.ArrowSub }
  | ARROW2SUB { Atom.Arrow2Sub }
  | BIGAND { Atom.BigAnd }
  | BIGOR { Atom.BigOr }
  | BIGADD { Atom.BigAdd }
  | BIGMUL { Atom.BigMul }
  | BIGCAT { Atom.BigCat }

%inline relop :
  | relop_ { $1 @@@ $sloc }
%inline relop_ :
  | COLON { Atom.Colon }
  | SUB { Atom.Sub }
  | SUP { Atom.Sup }
  | ASSIGN { Atom.Assign }
  | EQUIV { Atom.Equiv }
  | APPROX { Atom.Approx }
  | SQARROW { Atom.SqArrow }
  | SQARROWSTAR { Atom.SqArrowStar }
  | PREC { Atom.Prec }
  | SUCC { Atom.Succ }
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
  | arith_bin cmpop arith_bin { CmpE ($1, $2, $3) }
  | arith_bin boolop arith_bin { BinE ($1, $2, $3) }
  | arith_bin CAT arith_bin { CatE ($1, $3) }
  | arith_bin MEM arith_bin { MemE ($1, $3) }

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
  | HOLEN { HoleE (`Num $1) }
  | HOLE { HoleE `Next }
  | MULTIHOLE { HoleE `Rest }
  | NOTHING { HoleE `None }
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
  | TICK LPAREN exp RPAREN
    { BrackE (Atom.LParen @@@ $loc($2), $3, Atom.RParen @@@ $loc($4)) }
  | TICK LBRACK exp RBRACK
    { BrackE (Atom.LBrack @@@ $loc($2), $3, Atom.RBrack @@@ $loc($4)) }
  | TICK LBRACE exp RBRACE
    { BrackE (Atom.LBrace @@@ $loc($2), $3, Atom.RBrace @@@ $loc($4)) }
  | DOLLAR LPAREN arith RPAREN { $3.it }
  | FUSEFUSE exp_prim { UnparenE $2 }

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
  | exp_seq FUSE exp_atom { FuseE ($1, $3) }

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
  | exp_bin COLONCOLON exp_bin { ConsE ($1, $3) }
  | exp_bin CAT exp_bin { CatE ($1, $3) }
  | exp_bin MEM exp_bin { MemE ($1, $3) }

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
  | relid COLONSLASH exp { RuleNotPr ($1, $3) }
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
  | def_ NL_NL* { $1 @@@ $loc($1) }
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
  | NL_NL_NL
    { SepD }

(* Spec *)

spec :
  | NL_NL* def* EOF { $2 }
