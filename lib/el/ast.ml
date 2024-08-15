open Util.Source
module Lang = Common.Ast

(* Numbers *)
type num = Lang.num
type num' = Lang.num'
type integer = Lang.integer
type width = Lang.width
type signed = Lang.signed

(* Texts *)
type text = Lang.text
type text' = Lang.text'

(* Identifiers *)
type id = Lang.id
type id' = Lang.id'

(* Variables (scoped identifiers) *)
type var = Lang.var
type var' = Lang.var'

(* Members *)
type member = Lang.member
type member' = Lang.member'

(* Match kinds *)
type match_kind = Lang.match_kind
type match_kind' = Lang.match_kind'

(* State labels *)
type state_label = Lang.state_label
type state_label' = Lang.state_label'

(* Unary operators *)
type unop = Lang.unop
type unop' = Lang.unop'

(* Binary operators *)
type binop = Lang.binop
type binop' = Lang.binop'

(* Directions *)
type dir = dir' phrase
and dir' = No | In | Out | InOut

(* Types *)
type typ = typ' phrase

and typ' =
  | VoidT
  | BoolT
  | ErrT
  | StrT
  | IntT
  | FIntT of expr
  | FBitT of expr
  | VBitT of expr
  | NameT of var
  | SpecT of var * typ' list
  | StackT of typ' * expr
  | TupleT of typ' list
  | AnyT

(* Annotations *)
and anno = typ Lang.anno
and anno' = typ Lang.anno'

(* Type parameters *)
and tparam = Lang.tparam
and tparam' = Lang.tparam'

(* Parameters *)
and param = (typ, expr, dir) Lang.param
and param' = (typ, expr, dir) Lang.param'

(* Constructor parameters *)
and cparam = (typ, expr, dir) Lang.cparam
and cparam' = (typ, expr, dir) Lang.cparam'

(* Type arguments *)
and targ = typ Lang.targ
and targ' = typ Lang.targ'

(* Arguments *)
and arg = typ Lang.arg
and arg' = typ Lang.arg'

(* Expressions *)
and expr = typ Lang.expr
and expr' = typ Lang.expr'

(* Keyset expressions *)
and keyset = typ Lang.keyset
and keyset' = typ Lang.keyset'

(* Select-cases for select *)
and select_case = typ Lang.select_case
and select_case' = typ Lang.select_case'

(* Statements *)
and stmt = (typ, expr, dir) Lang.stmt
and stmt' = (typ, expr, dir) Lang.stmt'

(* Blocks (sequence of statements) *)
and block = (typ, expr, dir) Lang.block
and block' = (typ, expr, dir) Lang.block'

(* Match-cases for switch *)
and switch_label = Lang.switch_label
and switch_label' = Lang.switch_label'
and switch_case = (typ, expr, dir) Lang.switch_case
and switch_case' = (typ, expr, dir) Lang.switch_case'

(* Declarations *)
and decl = (typ, expr, dir) Lang.decl
and decl' = (typ, expr, dir) Lang.decl'

(* Parser state machine *)
and parser_state = (typ, expr, dir) Lang.parser_state
and parser_state' = (typ, expr, dir) Lang.parser_state'

(* Table *)
and table = typ Lang.table

(* Table keys *)
and table_key = typ Lang.table_key
and table_key' = typ Lang.table_key'

(* Table action references *)
and table_action = typ Lang.table_action
and table_action' = typ Lang.table_action'

(* Table entries *)
and table_entry = typ Lang.table_entry
and table_entry' = typ Lang.table_entry'

(* Table default properties *)
and table_default = typ Lang.table_default
and table_default' = typ Lang.table_default'
and table_default_const = Lang.table_default_const

(* Table custom properties *)
and table_custom = typ Lang.table_custom
and table_custom' = typ Lang.table_custom'
and table_custom_const = Lang.table_custom_const

(* Program *)
type program = (typ, expr, dir) Lang.program
