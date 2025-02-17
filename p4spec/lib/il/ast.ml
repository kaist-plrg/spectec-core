open Xl
open Util.Source

[@@@ocamlformat "disable"]

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

type mixop = Mixop.t

(* Iterators *)

type iter =
  | Opt       (* `?` *)
  | List      (* `*` *)

(* Types *)

and typ = typ' phrase
and typ' =
  | BoolT                   (* `bool` *)
  | NumT of Num.typ         (* numtyp *)
  | TextT                   (* `text` *)
  | VarT of id * targ list  (* id (`<` list(targ, `,`) `>`)? *)
  | TupT of typ list        (* `(` list(typ, `,`) `)` *)
  | IterT of typ * iter     (* typ iter *)

and nottyp = nottyp' phrase
and nottyp' = mixop * typ list

and deftyp = deftyp' phrase
and deftyp' =
  | AliasT of typ
  | NotationT of nottyp
  | StructT of typfield list
  | VariantT of typcase list

and typfield = atom * typ list
and typcase = nottyp

(* Operators *)

and numop = [ `DecOp | `HexOp ]
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
  | TupE of exp list                      (* `(` exp* `)` *)
  | ProjE of exp * int                    (* exp.i *)
  | CaseE of notexp                       (* notexp *)
  | UncaseE of exp list * mixop           (* exp* `!` mixop *)
  | OptE of exp option                    (* exp? *)
  | TheE of exp                           (* exp! *)
  | StrE of (atom * exp) list             (* { expfield* } *)
  | DotE of exp * atom                    (* exp.atom *)
  | CompE of exp * exp                    (* exp @ exp *)
  | ListE of exp list                     (* `[` exp* `]` *)
  | MemE of exp * exp                     (* exp `<-` exp *)
  | LenE of exp                           (* `|` exp `|` *)
  | CatE of exp * exp                     (* exp `::` exp *)
  | IdxE of exp * exp                     (* exp `[` exp `]` *)
  | SliceE of exp * exp * exp             (* exp `[` exp `:` exp `]` *)
  | UpdE of exp * path * exp              (* exp `[` path `=` exp `]` *)
  | CallE of id * arg list                (* $id`(` arg* `)` *)
  | IterE of exp * iterexp                (* exp iterexp *)
  | CastE of exp * typ                    (* `(` typ `)` exp *)

and notexp = mixop * exp list
and iterexp = iter * (id * exp) list

(* Path *)

and path = path' phrase
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
  | DefP of id * tparam list * param list * typ

(* Type parameters *)

and tparam = tparam' phrase
and tparam' = id'

(* Arguments *)

and arg = arg' phrase
and arg' =
  | ExpA of exp   (* exp *)
  | DefA of id    (* `$`id *)

(* Type arguments *)

and targ = targ' phrase
and targ' = typ'

(* Rules *)

and rule = rule' phrase
and rule' = id * notexp * prem list

(* Clauses *)

and clause = clause' phrase
and clause' = arg list * exp * prem list

(* Premises *)

and prem = prem' phrase
and prem' =
  | RulePr of id * exp             (* id `:` exp *)
  | IfPr of exp                    (* `if` exp *)
  | ElsePr                         (* `otherwise` *)
  | LetPr of exp * exp             (* `let` exp `=` exp *)
  | IfLetPr of exp * exp           (* `iflet` exp `=` exp *)
  | IterPr of prem * iterexp       (* prem iterexp *)

(* Hints *)

and hint = { hintid : id; hintexp : El.Ast.exp }

(* Definitions *)

type def = def' phrase
and def' =
  (* `syntax` id `<` list(tparam, `,`) `>` `=` deftyp *)
  | TypD of id * tparam list * deftyp
  (* `relation` id `:` nottyp rule* *)
  | RelD of id * nottyp * rule list
  (* `dec` id `<` list(tparam, `,`) `>` list(param, `,`) `:` typ clause* *)
  | DecD of id * tparam list * param list * typ * clause list
  (* `rec` `{` def* `}` *)
  | RecD of def list

(* Spec *)

type spec = def list
