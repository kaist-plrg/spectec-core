open Common.Source
module Sl = Lang.Sl

[@@@ocamlformat "disable"]

type id = Sl.id
type atom = Sl.atom
type mixop = Sl.mixop
type exp = Sl.exp
type notexp = Sl.notexp
type relcall = Sl.relcall
type iterexp = Sl.iterexp
type pattern = Sl.pattern
type tparam = Sl.tparam
type typ = Sl.typ
type cmpop = Sl.cmpop
type optyp = Sl.optyp
type arg = Sl.arg
type deftyp = Sl.deftyp
type phantom = Sl.phantom

(* Case analysis *)

type case = guard * block

and guard =
  | BoolG of bool
  | CmpG of cmpop * optyp * exp
  | SubG of typ
  | MatchG of pattern
  | MemG of exp

(* Instructions *)

and instr = instr' phrase
and instr' =
  | IfI of exp * iterexp list * block * phantom option
  | RelAssertI of { call : relcall; expect : bool; iterexps : iterexp list; block : block; phantom : phantom option }
  | CaseI of exp * case list * phantom option
  | OtherwiseI of block
  | TryI of block list
  | LetI of exp * exp * iterexp list
  | RelI of { call : relcall; iterexps : iterexp list }
  | ResultI of exp list
  | ReturnI of exp
  | DebugI of exp

and block = instr list
and elseblock = instr list

(* Definitions *)

type def = def' phrase
and def' =
  | TypD of id * tparam list * deftyp
  | RelD of id * (mixop * int list) * exp list * block * elseblock option
  | BuiltinDecD of id * tparam list * arg list
  | DecD of id * tparam list * arg list * block * elseblock option

type spec = def list
