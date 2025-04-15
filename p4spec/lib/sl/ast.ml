open Util.Source

[@@@ocamlformat "disable"]

(* Numbers *)

type num = Il.Ast.num

(* Texts *)

type text = Il.Ast.text

(* Identifiers *)

type id = Il.Ast.id
type id' = Il.Ast.id'

(* Atoms *)

type atom = Il.Ast.atom
type atom' = Il.Ast.atom'

(* Mixfix operators *)

type mixop = Il.Ast.mixop

(* Iterators *)

type iter = Il.Ast.iter

(* Variables *)

type var = Il.Ast.var

(* Types *)

type typ = Il.Ast.typ
type typ' = Il.Ast.typ'

type nottyp = Il.Ast.nottyp
type nottyp' = Il.Ast.nottyp'

type deftyp = Il.Ast.deftyp
type deftyp' = Il.Ast.deftyp'

type typfield = Il.Ast.typfield
type typcase = Il.Ast.typcase

(* Values *)

type value = Il.Ast.value

type valuefield = Il.Ast.valuefield
type valuecase = Il.Ast.valuecase

(* Operators *)

type numop = Il.Ast.numop
type unop = Il.Ast.unop
type binop = Il.Ast.binop
type cmpop = Il.Ast.cmpop
type optyp = Il.Ast.optyp

(* Expressions *)

type exp = Il.Ast.exp
type exp' = Il.Ast.exp'

type notexp = Il.Ast.notexp
type iterexp = Il.Ast.iterexp

(* Patterns *)

type pattern = Il.Ast.pattern

(* Path *)

type path = Il.Ast.path
type path' = Il.Ast.path'

(* Parameters *)

type param = Il.Ast.param
type param' = Il.Ast.param'

(* Type parameters *)

type tparam = Il.Ast.tparam
type tparam' = Il.Ast.tparam'

(* Arguments *)

type arg = Il.Ast.arg
type arg' = Il.Ast.arg'

(* Type arguments *)

type targ = Il.Ast.targ
type targ' = Il.Ast.targ'

(* Instructions *)

and instr = instr' phrase
and instr' =
  | RuleI of id * notexp * iterexp list
  | IfI of exp * iterexp list * instr list * instr list
  | OtherwiseI of instr
  | LetI of exp * exp * iterexp list
  | ResultI of exp list
  | ReturnI of exp

(* Hints *)

type hint = { hintid : id; hintexp : El.Ast.exp }

(* Definitions *)

type def = def' phrase
and def' =
  (* `syntax` id `<` list(tparam, `,`) `>` `=` deftyp *)
  | TypD of id * tparam list * deftyp
  (* `relation` id list(exp, `,`) `:` instr* *)
  | RelD of id * exp list * instr list
  (* `dec` id `<` list(tparam, `,`) `>` list(param, `,`) `:` typ instr* *)
  | DecD of id * tparam list * arg list * instr list

(* Spec *)

type spec = def list
