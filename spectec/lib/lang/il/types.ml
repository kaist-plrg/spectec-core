open Xl
open Common.Source

[@@@ocamlformat "disable"]
[@@@warning "-30"]  (* Several AST records intentionally share semantic field names. *)

(* Numbers *)

type num = Num.t

(* Texts *)

type text = string

(* Identifiers *)

type id = id' phrase
and id' = string

(* Atoms *)

type atom = atom' phrase
and atom' = Atom.t

(* Mixfix operators *)

type mixop = Mixfix.mixop

(* Iterators *)

type iter =
  | Opt       (* `?` *)
  | List      (* `*` *)

(* Hints *)

type hint = { hintid : id; hintexp : El.exp }

(* Types *)

type typ = typ' phrase
and typ' =
  | BoolT                   (* `bool` *)
  | NumT of Num.typ         (* numtyp *)
  | TextT                   (* `text` *)
  | VarT of { synid : id; targs : targ list }  (* id (`<` list(targ, `,`) `>`)? *)
  | TupleT of typ list      (* `(` list(typ, `,`) `)` *)
  | IterT of { typ : typ; iter : iter }  (* typ iter *)
  | FuncT                   (* `func` *)

(* Type arguments *)

and targ = targ' phrase
and targ' = typ'

(* Variables *)

type var = { varid : id; typ : typ; iters : iter list }

type nottyp = nottyp' phrase
and nottyp' = typ Mixfix.t

and reltyp = reltyp' phrase
and reltyp' = (typ, typ) Mode.t

and deftyp = deftyp' phrase
and deftyp' =
  | PlainT of typ
  | StructT of typfield list
  | VariantT of typcase list

and typfield = atom * typ
and typorigin = typorigin' Common.Source.phrase
and typorigin' = { synid : id; targs : targ list }
and typcase = { notation : nottyp; origin : typorigin; hints : hint list }

(* Values *)

type vid = int
and vnote = { vid : vid; vhash : int; typ : typ' }

and value = (value', vnote) note
and value' =
  | BoolV of bool
  | NumV of Num.t
  | TextV of string
  | StructV of valuefield list
  | CaseV of valuecase
  | TupleV of value list
  | OptV of value option
  | ListV of value list
  | FuncV of id

and valuefield = atom * value
and valuecase = value Mixfix.t

(* Operators *)

type numop = [ `DecOp | `HexOp ]
and unop = [ Bool.unop | Num.unop ]
and binop = [ Bool.binop | Num.binop ]
and cmpop = [ Bool.cmpop | Num.cmpop ]
and optyp = [ Bool.typ | Num.typ ]

(* Expressions *)

and exp = (exp', typ') note_phrase
and exp' =
  | BoolE of bool                         (* bool *)
  | NumE of num                           (* num *)
  | TextE of text                         (* text *)
  | VarE of id                            (* varid *)
  | UnE of unop * optyp * exp             (* unop exp *)
  | BinE of binop * optyp * exp * exp     (* exp binop exp *)
  | CmpE of cmpop * optyp * exp * exp     (* exp cmpop exp *)
  | UpCastE of typ * exp                  (* exp as typ *)
  | DownCastE of typ * exp                (* exp as typ *)
  | SubE of exp * typ                     (* exp `<:` typ *)
  | MatchE of exp * pattern               (* exp `matches` pattern *)
  | TupleE of exp list                    (* `(` exp* `)` *)
  | CaseE of notexp                       (* notexp *)
  | StrE of (atom * exp) list             (* { expfield* } *)
  | OptE of exp option                    (* exp? *)
  | ListE of exp list                     (* `[` exp* `]` *)
  | ConsE of exp * exp                    (* exp `::` exp *)
  | CatE of exp * exp                     (* exp `++` exp *)
  | MemE of exp * exp                     (* exp `<-` exp *)
  | LenE of exp                           (* `|` exp `|` *)
  | DotE of exp * atom                    (* exp.atom *)
  | IdxE of exp * exp                     (* exp `[` exp `]` *)
  | SliceE of exp * exp * exp             (* exp `[` exp `:` exp `]` *)
  | UpdE of exp * path * exp              (* exp `[` path `=` exp `]` *)
  | CallE of id * targ list * arg list    (* $id`<` targ* `>``(` arg* `)` *)
  | IterE of exp * iterexp                (* exp iterexp *)

and notexp = exp Mixfix.t
and iterexp = iter * var list

(* Patterns *)

and pattern =
  | CaseP of mixop
  | ListP of [ `Cons | `Fixed of int | `Nil ]
  | OptP of [ `Some | `None ]

(* Path *)

and path = (path', typ') note_phrase
and path' =
  | RootP                        (*  *)
  | IdxP of path * exp           (* path `[` exp `]` *)
  | SliceP of path * exp * exp   (* path `[` exp `:` exp `]` *)
  | DotP of path * atom          (* path `.` atom *)

(* Parameters *)

and param = param' phrase
and param' =
  (* typ *)
  | ExpP of typ
  (* `def` `$`id ` (`<` list(tparam, `,`) `>`)? (`(` list(param, `,`) `)`)? `:` typ *)
  | DefP of { defid : id; tparams : tparam list; params : param list; typ : typ }

(* Type parameters *)

and tparam = tparam' phrase
and tparam' = id'

(* Arguments *)

and arg = arg' phrase
and arg' =
  | ExpA of exp   (* exp *)
  | DefA of id    (* `$`id *)

(* Rules *)

and rule = rule' phrase
and rule' = { ruleid : id; concl : notexp; prems : prem list }

(* Clauses *)

and clause = clause' phrase
and clause' = { args : arg list; body : exp; prems : prem list }

(* Premises *)

and relcall = { relid : id; notexp : notexp }

and prem = (prem', prem_prov) phrase_prov
and prem_prov =
  | Source of El.prem  (* lowered from this EL premise *)
  | Synthesized        (* introduced by elaboration; no EL source *)
and prem' =
  | RelPr of relcall               (* id `:` notexp *)
  | RelAssertPr of { call : relcall; expect : bool }
                                   (* `if` id `:` notexp `holds` when expect, else `does not hold` *)
  | IfPr of { cond : exp; role : if_role }  (* `if` exp *)
  | ElsePr                         (* `otherwise` *)
  | LetPr of exp * exp             (* `let` exp `=` exp *)
  | IterPr of prem * iterexp       (* prem iterexp *)
  | DebugPr of exp                 (* `debug` exp *)

and if_role =
  | Condition
  | Guard  (* failing it means the rule does not apply *)

(* Definitions *)

type def = def' phrase
and def' =
  (* `syntax` id `<` list(tparam, `,`) `>` `=` deftyp *)
  | TypD of { synid : id; tparams : tparam list; deftyp : deftyp }
  (* `relation` id `:` reltyp rule* *)
  | RelD of { relid : id; reltyp : reltyp; rules : rule list }
  (* `dec` id `<` list(tparam, `,`) `>` list(param, `,`) `:` typ clause* *)
  | DecD of { defid : id; tparams : tparam list; params : param list; typ : typ; clauses : clause list }
  (* `builtin` `dec` id `<` list(tparam, `,`) `>` list(param, `,`) `:` typ hint* *)
  | BuiltinDecD of { defid : id; tparams : tparam list; params : param list; typ : typ; hints : hint list }

(* Spec *)

type spec = def list
