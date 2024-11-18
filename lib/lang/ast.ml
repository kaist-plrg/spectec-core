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
and unop' = BNotOp | LNotOp | UPlusOp | UMinusOp

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
type ('note, 'expr) anno = ('note, 'expr) anno' phrase

and ('note, 'expr) anno' =
  | EmptyN of text
  | TextN of text * text list
  | ExprN of text * ('note, 'expr) expr list
  | RecordN of text * (member * ('note, 'expr) expr) list

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
and ('note, 'expr) arg = ('note, 'expr) arg' phrase

and ('note, 'expr) arg' =
  | ExprA of ('note, 'expr) expr
  | NameA of id * ('note, 'expr) expr option
  | AnyA

(* Expressions : parameterized by 'expr *)
and ('note, 'expr) expr = ('expr expr', 'note) note_phrase
and 'expr expr' = 'expr

(* Keyset expressions *)
and ('note, 'expr) keyset = ('note, 'expr) keyset' phrase
and ('note, 'expr) keyset' = ExprK of ('note, 'expr) expr | DefaultK | AnyK

(* Select-cases for select *)
and ('note, 'expr) select_case = ('note, 'expr) select_case' phrase
and ('note, 'expr) select_case' = ('note, 'expr) keyset list * state_label

(* Statements *)
and ('typ, 'note, 'expr, 'decl) stmt = ('typ, 'note, 'expr, 'decl) stmt' phrase

and ('typ, 'note, 'expr, 'decl) stmt' =
  | EmptyS
  | AssignS of { expr_l : ('note, 'expr) expr; expr_r : ('note, 'expr) expr }
  | SwitchS of {
      expr_switch : ('note, 'expr) expr;
      cases : ('typ, 'note, 'expr, 'decl) switch_case list;
    }
  | IfS of {
      expr_cond : ('note, 'expr) expr;
      stmt_then : ('typ, 'note, 'expr, 'decl) stmt;
      stmt_else : ('typ, 'note, 'expr, 'decl) stmt;
    }
  | BlockS of { block : ('typ, 'note, 'expr, 'decl) block }
  | ExitS
  | RetS of { expr_ret : ('note, 'expr) expr option }
  | CallFuncS of {
      var_func : var;
      targs : 'typ targ list;
      args : ('note, 'expr) arg list;
    }
  | CallMethodS of {
      expr_base : ('note, 'expr) expr;
      member : member;
      targs : 'typ targ list;
      args : ('note, 'expr) arg list;
    }
  | CallInstS of {
      var_inst : var;
      targs : 'typ targ list;
      args : ('note, 'expr) arg list;
    }
  | TransS of { expr_label : ('note, 'expr) expr }
  | DeclS of { decl : 'decl decl }

(* Blocks (sequence of statements) *)
and ('typ, 'note, 'expr, 'decl) block =
  ('typ, 'note, 'expr, 'decl) block' phrase

and ('typ, 'note, 'expr, 'decl) block' =
  ('typ, 'note, 'expr, 'decl) stmt list * ('note, 'expr) anno list

(* Match-cases for switch *)
and ('note, 'expr) switch_label = ('note, 'expr) switch_label' phrase
and ('note, 'expr) switch_label' = ExprL of ('note, 'expr) expr | DefaultL

and ('typ, 'note, 'expr, 'decl) switch_case =
  ('typ, 'note, 'expr, 'decl) switch_case' phrase

and ('typ, 'note, 'expr, 'decl) switch_case' =
  | MatchC of ('note, 'expr) switch_label * ('typ, 'note, 'expr, 'decl) block
  | FallC of ('note, 'expr) switch_label

(* Declarations : parameterized by 'decl *)
and 'decl decl = 'decl decl' phrase
and 'decl decl' = 'decl

(* Parser state machine *)
and ('typ, 'note, 'expr, 'decl) parser_state =
  ('typ, 'note, 'expr, 'decl) parser_state' phrase

and ('typ, 'note, 'expr, 'decl) parser_state' =
  state_label * ('typ, 'note, 'expr, 'decl) block * ('note, 'expr) anno list

(* Table *)
and ('note, 'expr) table = {
  keys : ('note, 'expr) table_key list;
  actions : ('note, 'expr) table_action list;
  entries : ('note, 'expr) table_entry list * table_entries_const;
  default : ('note, 'expr) table_default option;
  customs : ('note, 'expr) table_custom list;
}

(* Table keys *)
and ('note, 'expr) table_key = ('note, 'expr) table_key' phrase

and ('note, 'expr) table_key' =
  ('note, 'expr) expr * match_kind * ('note, 'expr) anno list

(* Table action references *)
and ('note, 'expr) table_action = ('note, 'expr) table_action' phrase

and ('note, 'expr) table_action' =
  var * ('note, 'expr) arg list * ('note, 'expr) anno list

(* Table entries *)
and ('note, 'expr) table_entry = ('note, 'expr) table_entry' phrase

and ('note, 'expr) table_entry' =
  ('note, 'expr) keyset list
  * ('note, 'expr) table_action
  * ('note, 'expr) expr option
  * table_entry_const
  * ('note, 'expr) anno list

and table_entries_const = bool
and table_entry_const = bool

(* Table default properties *)
and ('note, 'expr) table_default = ('note, 'expr) table_default' phrase

and ('note, 'expr) table_default' =
  ('note, 'expr) table_action * table_default_const

and table_default_const = bool

(* Table custom properties *)
and ('note, 'expr) table_custom = ('note, 'expr) table_custom' phrase

and ('note, 'expr) table_custom' =
  member * ('note, 'expr) expr * table_custom_const * ('note, 'expr) anno list

and table_custom_const = bool

(* Program *)
type 'decl program = 'decl decl list
