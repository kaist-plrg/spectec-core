include Lang.Sl
open Common.Source

(* Intermediate representation of SL instructions,
   for the optimization pass *)

type instr = instr' phrase

and instr' =
  | RelI of { call : relcall; iterexps : iterexp list; block : instr list }
  | RelAssertI of {
      call : relcall;
      expect : bool;
      iterexps : iterexp list;
      block : instr list;
    }
  | IfI of exp * iterexp list * instr list
  | CaseI of exp * case list * bool
  | OtherwiseI of instr
  | LetI of exp * exp * iterexp list * instr list
  | ResultI of exp list
  | ReturnI of exp
  | DebugI of exp * instr

and case = guard * instr list

and guard =
  | BoolG of bool
  | CmpG of cmpop * optyp * exp
  | SubG of typ
  | MatchG of pattern
  | MemG of exp
