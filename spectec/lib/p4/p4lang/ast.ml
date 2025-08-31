open P4util.Source

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

(* Statements : parameterized by 'stmt *)
and 'stmt stmt = 'stmt stmt' phrase
and 'stmt stmt' = 'stmt

(* Blocks (sequence of statements) *)
and ('note, 'expr, 'stmt) block = ('note, 'expr, 'stmt) block' phrase
and ('note, 'expr, 'stmt) block' = 'stmt stmt list * ('note, 'expr) anno list

(* Match-cases for switch *)
and ('note, 'expr) switch_label = ('note, 'expr) switch_label' phrase
and ('note, 'expr) switch_label' = ExprL of ('note, 'expr) expr | DefaultL

and ('note, 'expr, 'stmt) switch_case =
  ('note, 'expr, 'stmt) switch_case' phrase

and ('note, 'expr, 'stmt) switch_case' =
  | MatchC of ('note, 'expr) switch_label * ('note, 'expr, 'stmt) block
  | FallC of ('note, 'expr) switch_label

(* Declarations : parameterized by 'decl *)
and 'decl decl = 'decl decl' phrase
and 'decl decl' = 'decl

(* Parser state machine *)
and ('note, 'expr, 'stmt) parser_state =
  ('note, 'expr, 'stmt) parser_state' phrase

and ('note, 'expr, 'stmt) parser_state' =
  state_label * ('note, 'expr, 'stmt) block * ('note, 'expr) anno list

(* Tables *)
and ('note, 'expr, 'table_action, 'table_entry) table =
  ('note, 'expr, 'table_action, 'table_entry) table_property list

(* Table properties *)
and ('note, 'expr, 'table_action, 'table_entry) table_property =
  | KeyP of ('note, 'expr) table_keys
  | ActionP of 'table_action table_actions
  | EntryP of 'table_entry table_entries
  | DefaultP of 'table_action table_default
  | CustomP of ('note, 'expr) table_custom

(* Table keys *)
and ('note, 'expr) table_keys = ('note, 'expr) table_keys' phrase
and ('note, 'expr) table_keys' = ('note, 'expr) table_key list
and ('note, 'expr) table_key = ('note, 'expr) table_key' phrase

and ('note, 'expr) table_key' =
  ('note, 'expr) expr * match_kind * ('note, 'expr) anno list

(* Table action references : parameterized by `table_action *)
and 'table_action table_actions = 'table_action table_actions' phrase
and 'table_action table_actions' = 'table_action table_action list
and 'table_action table_action = 'table_action table_action' phrase
and 'table_action table_action' = 'table_action

(* Table entries : parameterized by 'table_entry *)
and 'table_entry table_entries = 'table_entry table_entries' phrase

and 'table_entry table_entries' =
  table_entries_const * 'table_entry table_entry list

and table_entries_const = bool
and 'table_entry table_entry = 'table_entry table_entry' phrase
and 'table_entry table_entry' = 'table_entry
and table_entry_const = bool

(* Table default properties *)
and 'table_action table_default = 'table_action table_default' phrase

and 'table_action table_default' =
  table_default_const * 'table_action table_action

and table_default_const = bool

(* Table custom properties *)
and ('note, 'expr) table_custom = ('note, 'expr) table_custom' phrase

and ('note, 'expr) table_custom' =
  table_custom_const * member * ('note, 'expr) expr * ('note, 'expr) anno list

and table_custom_const = bool

(* Methods : parameterized by 'mthd *)
and 'mthd mthd = 'mthd mthd' phrase
and 'mthd mthd' = 'mthd

(* Program *)
type 'decl program = 'decl decl list
