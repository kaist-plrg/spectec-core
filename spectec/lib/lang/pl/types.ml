open Common.Source

[@@@ocamlformat "disable"]

(* Leaf type aliases from SL *)

type num = Sl.num
type text = Sl.text

type id = Sl.id
type id' = Sl.id'

type atom = Sl.atom
type atom' = Sl.atom'

type mixop = Sl.mixop

type iter = Sl.iter

type var = Sl.var

type value = Sl.value
type value' = Sl.value'

type typ = Sl.typ
type typ' = Sl.typ'

type nottyp = Sl.nottyp
type nottyp' = Sl.nottyp'

type deftyp = Sl.deftyp
type deftyp' = Sl.deftyp'

type typfield = Sl.typfield
type typcase = Sl.typcase

type unop = Sl.unop
type binop = Sl.binop
type cmpop = Sl.cmpop
type optyp = Sl.optyp

type pattern = Sl.pattern
type tparam = Sl.tparam
type targ = Sl.targ
type iterexp = Sl.iterexp

type pid = Sl.pid
type phantom = Sl.phantom
type pathcond = Sl.pathcond

(* Expressions *)

type exp = ((exp', typ') note_phrase) Annot.t
and exp' =
  | BoolE of bool
  | NumE of num
  | TextE of text
  | VarE of id
  | UnE of unop * optyp * exp
  | BinE of binop * optyp * exp * exp
  | CmpE of cmpop * optyp * exp * exp
  | UpCastE of typ * exp
  | DownCastE of typ * exp
  | SubE of exp * typ
  | MatchE of exp * pattern
  | TupleE of exp list
  | CaseE of notexp
  | StrE of (atom * exp) list
  | OptE of exp option
  | ListE of exp list
  | ConsE of exp * exp
  | CatE of exp * exp
  | MemE of exp * exp
  | LenE of exp
  | DotE of exp * atom
  | IdxE of exp * exp
  | SliceE of exp * exp * exp
  | UpdE of exp * path * exp
  | CallE of id * targ list * arg list
  | IterE of exp * iterexp

and notexp = exp Il.Mixfix.t

(* Relation calls *)

and relcall = { relid : id; notexp : notexp }

(* Paths *)

and path = (path', typ') note_phrase
and path' =
  | RootP
  | IdxP of path * exp
  | SliceP of path * exp * exp
  | DotP of path * atom

(* Arguments *)

and arg = arg' phrase
and arg' =
  | ExpA of exp
  | DefA of id

(* Parameters *)

and param = param' phrase
and param' =
  | ExpP of typ * exp
  | DefP of id

(* Case analysis *)

and case = guard * instr list

and guard =
  | BoolG of bool
  | CmpG of cmpop * optyp * exp
  | SubG of typ
  | MatchG of pattern
  | MemG of exp
  (* Shorthand guards — only emitted by the shorten pass *)
  | CheckLetSubG of typ * exp        (* scrut <: typ, bind scrut as exp *)
  | CheckLetMatchG of pattern * exp  (* scrut matches pattern, bind scrut as exp *)

(* Instructions *)

and instr = (instr' phrase) Annot.t
and instr' =
  | RelI of { call : relcall; iterexps : iterexp list }
  | RelAssertI of { call : relcall; expect : bool; iterexps : iterexp list; block : instr list; phantom : phantom option }
  | IfI of exp * iterexp list * instr list * phantom option
  | CaseI of exp * case list * phantom option
  | OtherwiseI of instr
  | TryI of block list
  | LetI of exp * exp * iterexp list
  | ResultI of exp list
  | ReturnI of exp
  | DebugI of exp
  (* Shorthands — only emitted by the shorten pass *)
  (* DestructI: bind each [exp] from [exp_source]'s positional CaseV args;
     [string option] is the prose field name (None = underscore, hidden). *)
  | DestructI of (string option * exp) list * exp
  (* CheckLetI: bind [exp_target] to [exp_source] after a subtype-or-match
     check, then run [block]. *)
  | CheckLetI of exp * exp * instr list
  (* OptionGetI: bind [exp_target] to the inner value of [exp_source],
     asserting it is [Some _]. *)
  | OptionGetI of exp * exp

and block = instr list
and elseblock = instr list

(* Definitions *)

type def = (def' phrase) Annot.t
and def' =
  | TypD of id * tparam list * deftyp
  | RelD of id * (mixop * int list) * exp list * block * elseblock option
  | BuiltinDecD of id * tparam list * arg list
  | DecD of id * tparam list * arg list * block * elseblock option

type spec = def list
