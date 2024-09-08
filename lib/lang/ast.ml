open Util.Source

type ('a, 'b) alt = Left of 'a | Right of 'b

(* Numbers *)
type num = num' phrase
and num' = integer * (width * signed) option
and integer = Bigint.t
and width = Bigint.t
and signed = bool

(* Texts *)
type text = text' phrase
and text' = string

(* Identifiers *)
type id = id' phrase
and id' = string

(* Variables (scoped identifiers) *)
type var = var' phrase
and var' = Top of id | Current of id

(* Members *)
type member = member' phrase
and member' = string

(* Match kinds *)
type match_kind = match_kind' phrase
and match_kind' = string

(* State labels *)
type state_label = state_label' phrase
and state_label' = string

(* Unary operators *)
type unop = unop' phrase
and unop' = BNotOp | LNotOp | UMinusOp

(* Binary operators *)
type binop = binop' phrase

and binop' =
  | PlusOp
  | SPlusOp
  | MinusOp
  | SMinusOp
  | MulOp
  | DivOp
  | ModOp
  | ShlOp
  | ShrOp
  | LeOp
  | GeOp
  | LtOp
  | GtOp
  | EqOp
  | NeOp
  | BAndOp
  | BXorOp
  | BOrOp
  | ConcatOp
  | LAndOp
  | LOrOp

(* Directions *)
and dir = dir' phrase
and dir' = No | In | Out | InOut

(* Types : parameterized by 'typ *)
and 'typ typ = 'typ typ' phrase
and 'typ typ' = 'typ

(* Values : parameterized by 'value *)
and 'value value = 'value value' phrase
and 'value value' = 'value

(* Annotations *)
type 'expr anno = 'expr anno' phrase

and 'expr anno' =
  | EmptyN of text
  | TextN of text * text list
  | ExprN of text * 'expr expr list
  | RecordN of text * (member * 'expr expr) list

(* Type parameters *)
and tparam = id
and tparam' = id'

(* Parameters : parameterized by 'param *)
and 'param param = 'param param' phrase
and 'param param' = 'param

(* Constructor parameters : parameterized by 'param *)
and 'param cparam = 'param cparam' phrase
and 'param cparam' = 'param

(* Type arguments : parameterized by 'typ *)
and 'typ targ = 'typ targ' phrase
and 'typ targ' = 'typ

(* Arguments *)
and 'expr arg = 'expr arg' phrase
and 'expr arg' = ExprA of 'expr expr | NameA of id * 'expr expr | AnyA

(* Expressions : parameterized by 'expr *)
and 'expr expr = 'expr expr' phrase
and 'expr expr' = 'expr

(* Keyset expressions *)
and 'expr keyset = 'expr keyset' phrase
and 'expr keyset' = ExprK of 'expr expr | DefaultK | AnyK

(* Select-cases for select *)
and 'expr select_case = 'expr select_case' phrase
and 'expr select_case' = 'expr keyset list * state_label

(* Statements *)
and ('typ, 'expr, 'decl) stmt = ('typ, 'expr, 'decl) stmt' phrase

and ('typ, 'expr, 'decl) stmt' =
  | EmptyS
  | AssignS of { expr_l : 'expr expr; expr_r : 'expr expr }
  | SwitchS of {
      expr_switch : 'expr expr;
      cases : ('typ, 'expr, 'decl) switch_case list;
    }
  | IfS of {
      expr_cond : 'expr expr;
      stmt_then : ('typ, 'expr, 'decl) stmt;
      stmt_else : ('typ, 'expr, 'decl) stmt;
    }
  | BlockS of { block : ('typ, 'expr, 'decl) block }
  | ExitS
  | RetS of { expr_ret : 'expr expr option }
  | CallS of {
      expr_func : 'expr expr;
      targs : 'typ targ list;
      args : 'expr arg list;
    }
  | TransS of { expr_label : 'expr expr }
  | DeclS of { decl : 'decl decl }

(* Blocks (sequence of statements) *)
and ('typ, 'expr, 'decl) block = ('typ, 'expr, 'decl) block' phrase

and ('typ, 'expr, 'decl) block' =
  ('typ, 'expr, 'decl) stmt list * 'expr anno list

(* Match-cases for switch *)
and switch_label = switch_label' phrase
and switch_label' = NameL of text | DefaultL
and ('typ, 'expr, 'decl) switch_case = ('typ, 'expr, 'decl) switch_case' phrase

and ('typ, 'expr, 'decl) switch_case' =
  | MatchC of switch_label * ('typ, 'expr, 'decl) block
  | FallC of switch_label

(* Declarations : parameterized by 'decl *)
and 'decl decl = 'decl decl' phrase
and 'decl decl' = 'decl

(* Parser state machine *)
and ('typ, 'expr, 'decl) parser_state =
  ('typ, 'expr, 'decl) parser_state' phrase

and ('typ, 'expr, 'decl) parser_state' =
  state_label * ('typ, 'expr, 'decl) block * 'expr anno list

(* Table *)
and 'expr table =
  'expr table_key list
  * 'expr table_action list
  * 'expr table_entry list
  * 'expr table_default option
  * 'expr table_custom list

(* Table keys *)
and 'expr table_key = 'expr table_key' phrase
and 'expr table_key' = 'expr expr * match_kind * 'expr anno list

(* Table action references *)
and 'expr table_action = 'expr table_action' phrase
and 'expr table_action' = var * 'expr arg list * 'expr anno list

(* Table entries *)
and 'expr table_entry = 'expr table_entry' phrase

and 'expr table_entry' =
  'expr keyset list * 'expr table_action * 'expr anno list

(* Table default properties *)
and 'expr table_default = 'expr table_default' phrase
and 'expr table_default' = 'expr table_action * table_default_const
and table_default_const = bool

(* Table custom properties *)
and 'expr table_custom = 'expr table_custom' phrase

and 'expr table_custom' =
  member * 'expr expr * table_custom_const * 'expr anno list

and table_custom_const = bool

(* Program *)
type 'decl program = 'decl decl list
