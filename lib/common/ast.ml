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

(* Annotations *)
type 't anno = 't anno' phrase

and 't anno' =
  | EmptyN of text
  | TextN of text * text list
  | ExprN of text * 't expr list
  | RecordN of text * (member * 't expr) list

(* Type parameters *)
and tparam = id
and tparam' = id'

(* Parameters *)
and ('t, 'v, 'd) param = ('t, 'v, 'd) param' phrase
and ('t, 'v, 'd) param' = id * 'd * 't * 'v option * 't anno list

(* Constructor parameters *)
and ('t, 'v, 'd) cparam = ('t, 'v, 'd) param
and ('t, 'v, 'd) cparam' = ('t, 'v, 'd) param'

(* Type arguments *)
and 't targ = 't targ' phrase
and 't targ' = 't

(* Arguments *)
and 't arg = 't arg' phrase
and 't arg' = ExprA of 't expr | NameA of id * 't expr | AnyA

(* Expressions *)
and 't expr = 't expr' phrase

and 't expr' =
  | BoolE of bool
  | StrE of text
  | NumE of num
  | VarE of var
  | TupleE of 't expr list
  | RecordE of (member * 't expr) list
  | UnE of unop * 't expr
  | BinE of binop * 't expr * 't expr
  | TernE of 't expr * 't expr * 't expr
  | CastE of 't * 't expr
  | MaskE of 't expr * 't expr
  | RangeE of 't expr * 't expr
  | SelectE of 't expr list * 't select_case list
  | ArrAccE of 't expr * 't expr
  | BitAccE of 't expr * 't expr * 't expr
  | ErrAccE of member
  | TypeAccE of var * member
  | ExprAccE of 't expr * member
  | CallE of 't expr * 't targ list * 't arg list
  | InstE of 't * 't arg list

(* Keyset expressions *)
and 't keyset = 't keyset' phrase
and 't keyset' = ExprK of 't expr | DefaultK | AnyK

(* Select-cases for select *)
and 't select_case = 't select_case' phrase
and 't select_case' = 't keyset list * state_label

(* Statements *)
and ('t, 'v, 'd) stmt = ('t, 'v, 'd) stmt' phrase

and ('t, 'v, 'd) stmt' =
  | EmptyS
  | AssignS of 't expr * 't expr
  | SwitchS of 't expr * ('t, 'v, 'd) switch_case list
  | IfS of 't expr * ('t, 'v, 'd) stmt * ('t, 'v, 'd) stmt
  | BlockS of ('t, 'v, 'd) block
  | ExitS
  | RetS of 't expr option
  | CallS of 't expr * 't targ list * 't arg list
  | TransS of 't expr
  | DeclS of ('t, 'v, 'd) decl

(* Blocks (sequence of statements) *)
and ('t, 'v, 'd) block = ('t, 'v, 'd) block' phrase
and ('t, 'v, 'd) block' = ('t, 'v, 'd) stmt list * 't anno list

(* Match-cases for switch *)
and switch_label = switch_label' phrase
and switch_label' = NameL of text | DefaultL
and ('t, 'v, 'd) switch_case = ('t, 'v, 'd) switch_case' phrase

and ('t, 'v, 'd) switch_case' =
  | MatchC of switch_label * ('t, 'v, 'd) block
  | FallC of switch_label

(* Declarations *)
and ('t, 'v, 'd) decl = ('t, 'v, 'd) decl' phrase

and ('t, 'v, 'd) decl' =
  (* Constant, variable, error, match_kind, and instance declarations *)
  | ConstD of { id : id; typ : 't; value : 'v; annos : 't anno list }
  | VarD of { id : id; typ : 't; init : 't expr option; annos : 't anno list }
  | ErrD of { members : member list }
  | MatchKindD of { members : member list }
  | InstD of {
      id : id;
      typ : 't;
      args : 't arg list;
      init : ('t, 'v, 'd) block option;
      annos : 't anno list;
    }
  (* Type declarations *)
  | StructD of {
      id : id;
      fields : (member * 't * 't anno list) list;
      annos : 't anno list;
    }
  | HeaderD of {
      id : id;
      fields : (member * 't * 't anno list) list;
      annos : 't anno list;
    }
  | UnionD of {
      id : id;
      fields : (member * 't * 't anno list) list;
      annos : 't anno list;
    }
  | EnumD of { id : id; members : member list; annos : 't anno list }
  | SEnumD of {
      id : id;
      typ : 't;
      fields : (member * 't expr) list;
      annos : 't anno list;
    }
  | NewTypeD of {
      id : id;
      typdef : ('t, ('t, 'v, 'd) decl) alt;
      annos : 't anno list;
    }
  | TypeDefD of {
      id : id;
      typdef : ('t, ('t, 'v, 'd) decl) alt;
      annos : 't anno list;
    }
  (* Object declarations *)
  (* Value Set *)
  | ValueSetD of { id : id; typ : 't; size : 't expr; annos : 't anno list }
  (* Parser *)
  | ParserTypeD of {
      id : id;
      tparams : tparam list;
      params : ('t, 'v, 'd) param list;
      annos : 't anno list;
    }
  | ParserD of {
      id : id;
      tparams : tparam list;
      params : ('t, 'v, 'd) param list;
      cparams : ('t, 'v, 'd) cparam list;
      locals : ('t, 'v, 'd) decl list;
      states : ('t, 'v, 'd) parser_state list;
      annos : 't anno list;
    }
  (* Table *)
  | TableD of { id : id; table : 't table; annos : 't anno list }
  (* Control *)
  | ControlTypeD of {
      id : id;
      tparams : tparam list;
      params : ('t, 'v, 'd) param list;
      annos : 't anno list;
    }
  | ControlD of {
      id : id;
      tparams : tparam list;
      params : ('t, 'v, 'd) param list;
      cparams : ('t, 'v, 'd) cparam list;
      locals : ('t, 'v, 'd) decl list;
      body : ('t, 'v, 'd) block;
      annos : 't anno list;
    }
  (* Functions *)
  | ActionD of {
      id : id;
      params : ('t, 'v, 'd) param list;
      body : ('t, 'v, 'd) block;
      annos : 't anno list;
    }
  | FuncD of {
      id : id;
      typ_ret : 't;
      tparams : tparam list;
      params : ('t, 'v, 'd) param list;
      body : ('t, 'v, 'd) block;
    }
  | ExternFuncD of {
      id : id;
      typ_ret : 't;
      tparams : tparam list;
      params : ('t, 'v, 'd) param list;
      annos : 't anno list;
    }
  (* Extern objects *)
  | ExternConstructorD of {
      id : id;
      cparams : ('t, 'v, 'd) cparam list;
      annos : 't anno list;
    }
  | ExternAbstractMethodD of {
      id : id;
      typ_ret : 't;
      tparams : tparam list;
      params : ('t, 'v, 'd) param list;
      annos : 't anno list;
    }
  | ExternMethodD of {
      id : id;
      typ_ret : 't;
      tparams : tparam list;
      params : ('t, 'v, 'd) param list;
      annos : 't anno list;
    }
  | ExternObjectD of {
      id : id;
      tparams : tparam list;
      mthds : ('t, 'v, 'd) decl list;
      annos : 't anno list;
    }
  (* Package *)
  | PackageTypeD of {
      id : id;
      tparams : tparam list;
      cparams : ('t, 'v, 'd) cparam list;
      annos : 't anno list;
    }

(* Parser state machine *)
and ('t, 'v, 'd) parser_state = ('t, 'v, 'd) parser_state' phrase
and ('t, 'v, 'd) parser_state' = state_label * ('t, 'v, 'd) block * 't anno list

(* Table *)
and 't table =
  't table_key list
  * 't table_action list
  * 't table_entry list
  * 't table_default option
  * 't table_custom list

(* Table keys *)
and 't table_key = 't table_key' phrase
and 't table_key' = 't expr * match_kind * 't anno list

(* Table action references *)
and 't table_action = 't table_action' phrase
and 't table_action' = var * 't arg list * 't anno list

(* Table entries *)
and 't table_entry = 't table_entry' phrase
and 't table_entry' = 't keyset list * 't table_action * 't anno list

(* Table default properties *)
and 't table_default = 't table_default' phrase
and 't table_default' = 't table_action * table_default_const
and table_default_const = bool

(* Table custom properties *)
and 't table_custom = 't table_custom' phrase
and 't table_custom' = member * 't expr * table_custom_const * 't anno list
and table_custom_const = bool

(* Program *)
type ('t, 'v, 'd) program = ('t, 'v, 'd) decl list
