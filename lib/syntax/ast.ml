open Util.Source

type ('a, 'b) alt = Left of 'a | Right of 'b

(* Numbers *)

type num = num' phrase
and num' = Bigint.t * (Bigint.t * bool) option

(* Names *)

type id = id' phrase
and id' = string

type path = id list
type path' = id' list

type var = var' phrase
and var' = Top of id | Bare of id

type member = member' phrase
and member' = string

type label = label' phrase
and label' = string

type mtch_kind = mtch_kind' phrase
and mtch_kind' = string

(* Unary and binary operators *)

type unop = unop' phrase
and unop' = BNotOp | LNotOp | UMinusOp

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

(* Types *)

type typ = typ' phrase

and typ' =
  | VoidT
  | BoolT
  | MatchKindT
  | ErrT
  | StrT
  | AIntT
  | IntT of expr
  | BitT of expr
  | VBitT of expr
  | NameT of var
  | SpecT of var * typ list
  | StackT of typ * expr
  | TupleT of typ list
  | AnyT

(* Directions *)
and dir = dir' phrase
and dir' = No | In | Out | InOut

(* Parameters *)
and tparam = id
and tparam' = id'
and param = param' phrase
and param' = id * dir * typ * expr option
and cparam = param
and cparam' = param'

(* Arguments *)
and arg = arg' phrase
and arg' = ExprA of expr | NameA of id * expr | AnyA

(* Expressions *)
and expr = expr' phrase

and expr' =
  | BoolE of bool
  | StrE of string
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
  | ArrAccE of expr * expr
  | BitAccE of expr * expr * expr
  | TypeAccE of var * member
  | ErrAccE of member
  | ExprAccE of expr * member
  | CallE of expr * typ list * arg list
  | InstE of typ * arg list

(* Statements *)
and stmt = stmt' phrase

and stmt' =
  | EmptyI
  | AssignI of expr * expr
  | SwitchI of expr * switch_case list
  | IfI of expr * stmt * stmt
  | BlockI of block
  | ExitI
  | RetI of expr option
  | CallI of expr * typ list * arg list
  | TransI of label
  | SelectI of expr list * select_case list
  | DeclI of decl

and block = block' phrase
and block' = stmt list

(* Match-cases for switch *)
and case = case' phrase
and case' = CaseC of string | FallC of string | DefaultC
and switch_case = switch_case' phrase
and switch_case' = case * block

(* Select-cases for select *)
and mtch = mtch' phrase
and mtch' = ExprM of expr | DefaultM | AnyM
and select_case = select_case' phrase
and select_case' = mtch list * label

(* Declarations *)
and decl = decl' phrase

and decl' =
  (* Constant, variable, and instance declarations *)
  | ConstD of { id : id; typ : typ; value : expr }
  | VarD of { id : id; typ : typ; init : expr option }
  | InstD of { id : id; typ : typ; args : arg list; init : block option }
  (* Type declarations *)
  | ErrD of { members : member list }
  | MatchKindD of { members : member list }
  | StructD of { id : id; fields : (member * typ) list }
  | HeaderD of { id : id; fields : (member * typ) list }
  | UnionD of { id : id; fields : (member * typ) list }
  | EnumD of { id : id; members : member list }
  | SEnumD of { id : id; typ : typ; fields : (member * expr) list }
  | NewTypeD of { id : id; typ : (typ, decl) alt }
  | TypeDefD of { id : id; typ : (typ, decl) alt }
  (* Object declarations *)
  (* Value Set *)
  | ValueSetD of { id : id; typ : typ; size : expr }
  (* Parser *)
  | ParserTypeD of { id : id; tparams : tparam list; params : param list }
  | ParserD of {
      id : id;
      tparams : tparam list;
      params : param list;
      cparams : cparam list;
      locals : decl list;
      states : parser_state list;
    }
  (* Table *)
  | TableD of { id : id; table : table }
  (* Control *)
  | ControlTypeD of { id : id; tparams : tparam list; params : param list }
  | ControlD of {
      id : id;
      tparams : tparam list;
      params : param list;
      cparams : cparam list;
      locals : decl list;
      body : block;
    }
  (* Functions *)
  | ActionD of { id : id; params : param list; body : block }
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
    }
  (* Extern objects *)
  | ConsD of { id : id; cparams : cparam list }
  | AbstractD of {
      id : id;
      rettyp : typ;
      tparams : tparam list;
      params : param list;
    }
  | MethodD of {
      id : id;
      rettyp : typ;
      tparams : tparam list;
      params : param list;
    }
  | ExternObjectD of { id : id; tparams : tparam list; mthds : decl list }
  (* Package *)
  | PackageTypeD of { id : id; tparams : tparam list; cparams : cparam list }

(* Parser state machine *)
and parser_state = parser_state' phrase
and parser_state' = label * block

(* Table *)
and table_key = table_key' phrase
and table_key' = expr * mtch_kind
and table_action = table_action' phrase
and table_action' = var * arg list
and table_entry = table_entry' phrase
and table_entry' = mtch list * table_action
and table_default = table_default' phrase
and table_default' = table_action * bool
and table_custom = table_custom' phrase
and table_custom' = member * expr * bool

and table =
  table_key list
  * table_action list
  * table_entry list
  * table_default option
  * table_custom list

(* Program *)
type program = decl list
