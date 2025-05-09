include Sl.Ast
open Util.Source

(* Intermediate representation of SL instructions,
   for the optimization pass *)

type instr = instr' phrase

and instr' =
  | IfI of exp * iterexp list * instr list
  | CaseI of exp * case list * bool
  | OtherwiseI of instr
  | LetI of exp * exp * iterexp list
  | RuleI of id * notexp * iterexp list
  | ResultI of exp list
  | ReturnI of exp

and case = guard * instr list

and guard =
  | BoolG of bool
  | CmpG of cmpop * optyp * exp
  | SubG of typ
  | MatchG of pattern
  | MemG of exp
