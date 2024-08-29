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

(* Directions : parameterized by 'dir *)
(* Types : parameterized by 'typ *)
(* Compile-time known values : parameterized by 'svalue *)

(* Annotations *)
type 'typ anno = 'typ anno' phrase

and 'typ anno' =
  | EmptyN of text
  | TextN of text * text list
  | ExprN of text * 'typ expr list
  | RecordN of text * (member * 'typ expr) list

(* Type parameters *)
and tparam = id
and tparam' = id'

(* Parameters *)
and ('typ, 'svalue, 'dir) param = ('typ, 'svalue, 'dir) param' phrase

and ('typ, 'svalue, 'dir) param' =
  id * 'dir * 'typ * 'svalue option * 'typ anno list

(* Constructor parameters *)
and ('typ, 'svalue, 'dir) cparam = ('typ, 'svalue, 'dir) param
and ('typ, 'svalue, 'dir) cparam' = ('typ, 'svalue, 'dir) param'

(* Type arguments : parameterized by 'typ *)

(* Arguments *)
and 'typ arg = 'typ arg' phrase
and 'typ arg' = ExprA of 'typ expr | NameA of id * 'typ expr | AnyA

(* Expressions *)
and 'typ expr = 'typ expr' phrase

and 'typ expr' =
  | BoolE of bool
  | StrE of text
  | NumE of num
  | VarE of var
  | TupleE of 'typ expr list
  | RecordE of (member * 'typ expr) list
  | UnE of unop * 'typ expr
  | BinE of binop * 'typ expr * 'typ expr
  | TernE of 'typ expr * 'typ expr * 'typ expr
  | CastE of 'typ * 'typ expr
  | MaskE of 'typ expr * 'typ expr
  | RangeE of 'typ expr * 'typ expr
  | SelectE of 'typ expr list * 'typ select_case list
  | ArrAccE of 'typ expr * 'typ expr
  | BitAccE of 'typ expr * 'typ expr * 'typ expr
  | ErrAccE of member
  | TypeAccE of var * member
  | ExprAccE of 'typ expr * member
  | CallE of 'typ expr * 'typ list * 'typ arg list
  | InstE of 'typ * 'typ arg list

(* Keyset expressions *)
and 'typ keyset = 'typ keyset' phrase
and 'typ keyset' = ExprK of 'typ expr | DefaultK | AnyK

(* Select-cases for select *)
and 'typ select_case = 'typ select_case' phrase
and 'typ select_case' = 'typ keyset list * state_label

(* Statements *)
and ('typ, 'svalue, 'dir) stmt = ('typ, 'svalue, 'dir) stmt' phrase

and ('typ, 'svalue, 'dir) stmt' =
  | EmptyS
  | AssignS of 'typ expr * 'typ expr
  | SwitchS of 'typ expr * ('typ, 'svalue, 'dir) switch_case list
  | IfS of 'typ expr * ('typ, 'svalue, 'dir) stmt * ('typ, 'svalue, 'dir) stmt
  | BlockS of ('typ, 'svalue, 'dir) block
  | ExitS
  | RetS of 'typ expr option
  | CallS of 'typ expr * 'typ list * 'typ arg list
  | TransS of 'typ expr
  | DeclS of ('typ, 'svalue, 'dir) decl

(* Blocks (sequence of statements) *)
and ('typ, 'svalue, 'dir) block = ('typ, 'svalue, 'dir) block' phrase

and ('typ, 'svalue, 'dir) block' =
  ('typ, 'svalue, 'dir) stmt list * 'typ anno list

(* Match-cases for switch *)
and switch_label = switch_label' phrase
and switch_label' = NameL of text | DefaultL

and ('typ, 'svalue, 'dir) switch_case =
  ('typ, 'svalue, 'dir) switch_case' phrase

and ('typ, 'svalue, 'dir) switch_case' =
  | MatchC of switch_label * ('typ, 'svalue, 'dir) block
  | FallC of switch_label

(* Declarations *)
and ('typ, 'svalue, 'dir) decl = ('typ, 'svalue, 'dir) decl' phrase

and ('typ, 'svalue, 'dir) decl' =
  (* Constant, variable, error, match_kind, and instance declarations *)
  | ConstD of { id : id; typ : 'typ; value : 'svalue; annos : 'typ anno list }
  | VarD of {
      id : id;
      typ : 'typ;
      init : 'typ expr option;
      annos : 'typ anno list;
    }
  | ErrD of { members : member list }
  | MatchKindD of { members : member list }
  | InstD of {
      id : id;
      typ : 'typ;
      args : 'typ arg list;
      init : ('typ, 'svalue, 'dir) block option;
      annos : 'typ anno list;
    }
  (* Type declarations *)
  | StructD of {
      id : id;
      fields : (member * 'typ * 'typ anno list) list;
      annos : 'typ anno list;
    }
  | HeaderD of {
      id : id;
      fields : (member * 'typ * 'typ anno list) list;
      annos : 'typ anno list;
    }
  | UnionD of {
      id : id;
      fields : (member * 'typ * 'typ anno list) list;
      annos : 'typ anno list;
    }
  | EnumD of { id : id; members : member list; annos : 'typ anno list }
  | SEnumD of {
      id : id;
      typ : 'typ;
      fields : (member * 'svalue) list;
      annos : 'typ anno list;
    }
  | NewTypeD of {
      id : id;
      typdef : ('typ, ('typ, 'svalue, 'dir) decl) alt;
      annos : 'typ anno list;
    }
  | TypeDefD of {
      id : id;
      typdef : ('typ, ('typ, 'svalue, 'dir) decl) alt;
      annos : 'typ anno list;
    }
  (* Object declarations *)
  (* Value Set *)
  | ValueSetD of {
      id : id;
      typ : 'typ;
      size : 'typ expr;
      annos : 'typ anno list;
    }
  (* Parser *)
  | ParserTypeD of {
      id : id;
      tparams : tparam list;
      params : ('typ, 'svalue, 'dir) param list;
      annos : 'typ anno list;
    }
  | ParserD of {
      id : id;
      tparams : tparam list;
      params : ('typ, 'svalue, 'dir) param list;
      cparams : ('typ, 'svalue, 'dir) cparam list;
      locals : ('typ, 'svalue, 'dir) decl list;
      states : ('typ, 'svalue, 'dir) parser_state list;
      annos : 'typ anno list;
    }
  (* Table *)
  | TableD of { id : id; table : 'typ table; annos : 'typ anno list; }
  (* Control *)
  | ControlTypeD of {
      id : id;
      tparams : tparam list;
      params : ('typ, 'svalue, 'dir) param list;
      annos : 'typ anno list;
    }
  | ControlD of {
      id : id;
      tparams : tparam list;
      params : ('typ, 'svalue, 'dir) param list;
      cparams : ('typ, 'svalue, 'dir) cparam list;
      locals : ('typ, 'svalue, 'dir) decl list;
      body : ('typ, 'svalue, 'dir) block;
      annos : 'typ anno list;
    }
  (* Functions *)
  | ActionD of {
      id : id;
      params : ('typ, 'svalue, 'dir) param list;
      body : ('typ, 'svalue, 'dir) block;
      annos : 'typ anno list;
    }
  | FuncD of {
      id : id;
      typ_ret : 'typ;
      tparams : tparam list;
      params : ('typ, 'svalue, 'dir) param list;
      body : ('typ, 'svalue, 'dir) block;
    }
  | ExternFuncD of {
      id : id;
      typ_ret : 'typ;
      tparams : tparam list;
      params : ('typ, 'svalue, 'dir) param list;
      annos : 'typ anno list;
    }
  (* Extern objects *)
  | ExternConstructorD of {
      id : id;
      cparams : ('typ, 'svalue, 'dir) cparam list;
      annos : 'typ anno list;
    }
  | ExternAbstractMethodD of {
      id : id;
      typ_ret : 'typ;
      tparams : tparam list;
      params : ('typ, 'svalue, 'dir) param list;
      annos : 'typ anno list;
    }
  | ExternMethodD of {
      id : id;
      typ_ret : 'typ;
      tparams : tparam list;
      params : ('typ, 'svalue, 'dir) param list;
      annos : 'typ anno list;
    }
  | ExternObjectD of {
      id : id;
      tparams : tparam list;
      mthds : ('typ, 'svalue, 'dir) decl list;
      annos : 'typ anno list;
    }
  (* Package *)
  | PackageTypeD of {
      id : id;
      tparams : tparam list;
      cparams : ('typ, 'svalue, 'dir) cparam list;
      annos : 'typ anno list;
    }

(* Parser state machine *)
and ('typ, 'svalue, 'dir) parser_state =
  ('typ, 'svalue, 'dir) parser_state' phrase

and ('typ, 'svalue, 'dir) parser_state' =
  state_label * ('typ, 'svalue, 'dir) block * 'typ anno list

(* Table *)
and 'typ table =
  'typ table_key list
  * 'typ table_action list
  * 'typ table_entry list
  * 'typ table_default option
  * 'typ table_custom list

(* Table keys *)
and 'typ table_key = 'typ table_key' phrase
and 'typ table_key' = 'typ expr * match_kind * 'typ anno list

(* Table action references *)
and 'typ table_action = 'typ table_action' phrase
and 'typ table_action' = var * 'typ arg list * 'typ anno list

(* Table entries *)
and 'typ table_entry = 'typ table_entry' phrase
and 'typ table_entry' = 'typ keyset list * 'typ table_action * 'typ anno list

(* Table default properties *)
and 'typ table_default = 'typ table_default' phrase
and 'typ table_default' = 'typ table_action * table_default_const
and table_default_const = bool

(* Table custom properties *)
and 'typ table_custom = 'typ table_custom' phrase

and 'typ table_custom' =
  member * 'typ expr * table_custom_const * 'typ anno list

and table_custom_const = bool

(* Program *)
type ('typ, 'svalue, 'dir) program = ('typ, 'svalue, 'dir) decl list
