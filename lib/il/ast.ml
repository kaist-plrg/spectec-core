open Util.Source
module L = Lang.Ast

type ('a, 'b) alt = ('a, 'b) L.alt

(* Numbers *)
type num = L.num
type num' = L.num'
type integer = L.integer
type width = L.width
type signed = L.signed

(* Texts *)
type text = L.text
type text' = L.text'

(* Identifiers *)
type id = L.id
type id' = L.id'

(* Variables (scoped identifiers) *)
type var = L.var
type var' = L.var'

(* Members *)
type member = L.member
type member' = L.member'

(* Match kinds *)
type match_kind = L.match_kind
type match_kind' = L.match_kind'

(* State labels *)
type state_label = L.state_label
type state_label' = L.state_label'

(* Unary operators *)
type unop = L.unop
type unop' = L.unop'

(* Binary operators *)
type binop = L.binop
type binop' = L.binop'

(* Directions *)
type dir = dir' phrase
and dir' = Runtime.Dir.t

(* Types *)
type typ = typ' phrase
and typ' = Runtime.Types.Type.t

(* Values *)
type value = value' phrase
and value' = Runtime.Value.t

(* Annotations *)
and anno = typ L.anno
and anno' = typ L.anno'

(* Type parameters *)
and tparam = L.tparam
and tparam' = L.tparam'

(* Parameters *)
and param = (typ, value, dir) L.param
and param' = (typ, value, dir) L.param'

(* Constructor parameters *)
and cparam = (typ, value, dir) L.cparam
and cparam' = (typ, value, dir) L.cparam'

(* Type arguments *)
and targ = typ L.targ
and targ' = typ'

(* Arguments *)
and arg = typ L.arg
and arg' = typ L.arg'

(* Expressions *)
and expr = typ L.expr
and expr' = typ L.expr'

(* Keyset expressions *)
and keyset = typ L.keyset
and keyset' = typ L.keyset'

(* Select-cases for select *)
and select_case = typ L.select_case
and select_case' = typ L.select_case'

(* Statements *)
and stmt = (typ, value, dir) L.stmt
and stmt' = (typ, value, dir) L.stmt'

(* Blocks (sequence of statements) *)
and block = (typ, value, dir) L.block
and block' = (typ, value, dir) L.block'

(* Match-cases for switch *)
and switch_label = L.switch_label
and switch_label' = L.switch_label'
and switch_case = (typ, value, dir) L.switch_case
and switch_case' = (typ, value, dir) L.switch_case'

(* Declarations *)
and decl = (typ, value, dir) L.decl
and decl' = (typ, value, dir) L.decl'

(* Parser state machine *)
and parser_state = (typ, value, dir) L.parser_state
and parser_state' = (typ, value, dir) L.parser_state'

(* Table *)
and table = typ L.table

(* Table keys *)
and table_key = typ L.table_key
and table_key' = typ L.table_key'

(* Table action references *)
and table_action = typ L.table_action
and table_action' = typ L.table_action'

(* Table entries *)
and table_entry = typ L.table_entry
and table_entry' = typ L.table_entry'

(* Table default properties *)
and table_default = typ L.table_default
and table_default' = typ L.table_default'
and table_default_const = L.table_default_const

(* Table custom properties *)
and table_custom = typ L.table_custom
and table_custom' = typ L.table_custom'
and table_custom_const = L.table_custom_const

(* Program *)
type program = (typ, value, dir) L.program
