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

(* Paths *)
type path = id list
type path' = id' list

(* Variables (scoped identifiers) *)
type var = var' phrase
and var' = Top of id | Current of id

(* Members *)
type member = member' phrase
and member' = string

(* State labels *)
type state_label = state_label' phrase
and state_label' = string

(* Match kinds *)
type match_kind = match_kind' phrase
and match_kind' = string

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
type anno = anno' phrase

and anno' =
  | EmptyN of text
  | TextN of text * text list
  | ExprN of text * expr list
  | RecordN of text * (member * expr) list

(* Types *)
and typ = typ' phrase

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
  | SpecT of var * typ list
  | StackT of typ * expr
  | TupleT of typ list
  | AnyT

(* Directions *)
and dir = dir' phrase
and dir' = No | In | Out | InOut

(* Type parameters *)
and tparam = id
and tparam' = id'

(* Parameters *)
and param = param' phrase
and param' = id * dir * typ * expr option * anno list

(* Constructor parameters *)
and cparam = param
and cparam' = param'

(* Type arguments *)
and targ = typ
and targ' = typ'

(* Arguments *)
and arg = arg' phrase
and arg' = ExprA of expr | NameA of id * expr | AnyA

(* Expressions *)
and expr = expr' phrase

and expr' =
  | BoolE of bool
  | StrE of text
  | NumE of num
  | VarE of var
  | ListE of expr list
  | RecordE of (member * expr) list
  | UnE of unop * expr
  | BinE of binop * expr * expr
  | TernE of expr * expr * expr
  | CastE of typ * expr
  | MaskE of expr * expr
  | RangeE of expr * expr
  | SelectE of expr list * select_case list
  | ArrAccE of expr * expr
  | BitAccE of expr * expr * expr
  | ErrAccE of member
  | TypeAccE of var * member
  | ExprAccE of expr * member
  | CallE of expr * targ list * arg list
  | InstE of typ * arg list

(* Keyset expressions *)
and keyset = keyset' phrase
and keyset' = ExprK of expr | DefaultK | AnyK

(* Select-cases for select *)
and select_case = select_case' phrase
and select_case' = keyset list * state_label

(* Statements *)
and stmt = stmt' phrase

and stmt' =
  | EmptyS
  | AssignS of expr * expr
  | SwitchS of expr * switch_case list
  | IfS of expr * stmt * stmt
  | BlockS of block
  | ExitS
  | RetS of expr option
  | CallS of expr * targ list * arg list
  | TransS of expr
  | DeclS of decl

(* Blocks (sequence of statements) *)
and block = block' phrase
and block' = stmt list * anno list

(* Match-cases for switch *)
and switch_label = switch_label' phrase
and switch_label' = NameL of text | DefaultL
and switch_case = switch_case' phrase
and switch_case' = MatchC of switch_label * block | FallC of switch_label

(* Declarations *)
and decl = decl' phrase

and decl' =
  (* Constant, variable, and instance declarations *)
  | ConstD of { id : id; typ : typ; value : expr; annos : anno list }
  | VarD of { id : id; typ : typ; init : expr option; annos : anno list }
  | InstD of {
      id : id;
      typ : typ;
      args : arg list;
      init : block option;
      annos : anno list;
    }
  (* Type declarations *)
  | ErrD of { members : member list }
  | MatchKindD of { members : member list }
  | StructD of {
      id : id;
      tparams : tparam list;
      fields : (member * typ * anno list) list;
      annos : anno list;
    }
  | HeaderD of {
      id : id;
      tparams : tparam list;
      fields : (member * typ * anno list) list;
      annos : anno list;
    }
  | UnionD of {
      id : id;
      tparams : tparam list;
      fields : (member * typ * anno list) list;
      annos : anno list;
    }
  | EnumD of { id : id; members : member list; annos : anno list }
  | SEnumD of {
      id : id;
      typ : typ;
      fields : (member * expr) list;
      annos : anno list;
    }
  | NewTypeD of { id : id; typ : (typ, decl) alt; annos : anno list }
  | TypeDefD of { id : id; typ : (typ, decl) alt; annos : anno list }
  (* Object declarations *)
  (* Value Set *)
  | ValueSetD of { id : id; typ : typ; size : expr; annos : anno list }
  (* Parser *)
  | ParserTypeD of {
      id : id;
      tparams : tparam list;
      params : param list;
      annos : anno list;
    }
  | ParserD of {
      id : id;
      tparams : tparam list;
      params : param list;
      cparams : cparam list;
      locals : decl list;
      states : parser_state list;
      annos : anno list;
    }
  (* Table *)
  | TableD of { id : id; table : table; annos : anno list }
  (* Control *)
  | ControlTypeD of {
      id : id;
      tparams : tparam list;
      params : param list;
      annos : anno list;
    }
  | ControlD of {
      id : id;
      tparams : tparam list;
      params : param list;
      cparams : cparam list;
      locals : decl list;
      body : block;
      annos : anno list;
    }
  (* Functions *)
  | ActionD of { id : id; params : param list; body : block; annos : anno list }
  | FuncD of {
      id : id;
      rettyp : typ;
      tparams : tparam list;
      params : param list;
      body : block;
    }
  | ExternFuncD of {
      id : id;
      rettyp : typ;
      tparams : tparam list;
      params : param list;
      annos : anno list;
    }
  (* Extern objects *)
  | ConsD of { id : id; cparams : cparam list; annos : anno list }
  | AbstractD of {
      id : id;
      rettyp : typ;
      tparams : tparam list;
      params : param list;
      annos : anno list;
    }
  | MethodD of {
      id : id;
      rettyp : typ;
      tparams : tparam list;
      params : param list;
      annos : anno list;
    }
  | ExternObjectD of {
      id : id;
      tparams : tparam list;
      mthds : decl list;
      annos : anno list;
    }
  (* Package *)
  | PackageTypeD of {
      id : id;
      tparams : tparam list;
      cparams : cparam list;
      annos : anno list;
    }

(* Parser state machine *)
and parser_state = parser_state' phrase
and parser_state' = state_label * block * anno list

(* Table *)
and table =
  table_key list
  * table_action list
  * table_entry list
  * table_default option
  * table_custom list

(* Table keys *)
and table_key = table_key' phrase
and table_key' = expr * match_kind * anno list

(* Table action references *)
and table_action = table_action' phrase
and table_action' = var * arg list * anno list

(* Table entries *)
and table_entry = table_entry' phrase
and table_entry' = keyset list * table_action * anno list

(* Table default properties *)
and table_default = table_default' phrase
and table_default' = table_action * table_default_const
and table_default_const = bool

(* Table custom properties *)
and table_custom = table_custom' phrase
and table_custom' = member * expr * table_custom_const * anno list
and table_custom_const = bool

(* Program *)
type program = decl list
