(* Numbers : value, width, signed *)

type num = Bigint.t * (Bigint.t * bool) option

(* Variable : top-level (prefixed with a dot) or bare name *)

type var = Top of string | Bare of string

(* Unary and binary operators *)

type unop = BNotOp | LNotOp | UMinusOp

type binop =
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

type typ =
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

(* Parameters and Arguments *)
and dir = No | In | Out | InOut
and param = string * dir * typ * expr option
and arg = ExprA of expr | NameA of string * expr | AnyA

(* Expressions *)
and expr =
  | BoolE of bool
  | NumE of num
  | StrE of string
  | VarE of var
  | ListE of expr list
  | RecordE of (string * expr) list
  | UnE of unop * expr
  | BinE of binop * expr * expr
  | TernE of expr * expr * expr
  | CastE of typ * expr
  | MaskE of expr * expr
  | RangeE of expr * expr
  | ArrAccE of expr * expr
  | BitAccE of expr * expr * expr
  | TypeAccE of var * string
  | ErrAccE of string
  | ExprAccE of expr * string
  | CallE of expr * typ list * arg list
  | InstE of typ * arg list

(* Match-cases *)
and mtch = ExprM of expr | DefaultM | AnyM
and case = CaseC of string | DefaultC

(* Statements *)
and stmt =
  | EmptyI
  | AssignI of expr * expr
  | SwitchI of expr * switch_case list
  | IfI of expr * stmt * stmt
  | BlockI of block
  | ExitI
  | RetI of expr option
  | CallI of expr * typ list * arg list
  | TransI of string
  | SelectI of expr list * select_case list
  | DeclI of decl

and switch_case = case * block
and select_case = mtch list * string
and block = stmt list

(* Declarations *)
and decl =
  (* Constant, variable, and instance declarations *)
  | ConstD of { name : string; typ : typ; value : expr }
  | VarD of { name : string; typ : typ; init : expr option }
  | InstD of { name : string; typ : typ; args : arg list; init : block option }
  (* Type declarations *)
  | ErrD of { members : string list }
  | MatchKindD of { members : string list }
  | StructD of { name : string; fields : (string * typ) list }
  | HeaderD of { name : string; fields : (string * typ) list }
  | UnionD of { name : string; fields : (string * typ) list }
  | EnumD of { name : string; members : string list }
  | SEnumD of { name : string; typ : typ; members : (string * expr) list }
  | NewTypeD of { name : string; typ : typ option; decl : decl option }
  | TypeDefD of { name : string; typ : typ option; decl : decl option }
  (* Object declarations *)
  (* Value Set *)
  | ValSetD of { name : string; typ : typ; size : expr }
  (* Parser *)
  | ParserTypeD of { name : string; tparams : string list; params : param list }
  | ParserD of {
      name : string;
      tparams : string list;
      params : param list;
      cparams : param list;
      locals : decl list;
      states : parser_state list;
    }
  (* Control *)
  | ActionD of { name : string; params : param list; body : block }
  | TableD of {
      name : string;
      key : table_key list;
      actions : table_action list;
      entries : table_entry list;
      default : table_default option;
      custom : table_custom list;
    }
  | ControlTypeD of { name : string; tparams : string list; params : param list }
  | ControlD of {
      name : string;
      tparams : string list;
      params : param list;
      cparams : param list;
      locals : decl list;
      body : block;
    }
  (* Functions *)
  | FuncD of {
      name : string;
      rettyp : typ;
      tparams : string list;
      params : param list;
      body : block;
    }
  | ExtFuncD of {
      name : string;
      rettyp : typ;
      tparams : string list;
      params : param list;
    }
  (* Extern objects *)
  | ConsD of { name : string; params : param list }
  | AbstractD of {
      name : string;
      rettyp : typ;
      tparams : string list;
      params : param list;
    }
  | MethodD of {
      name : string;
      rettyp : typ;
      tparams : string list;
      params : param list;
    }
  | ExtObjD of { name : string; tparams : string list; methods : decl list }
  (* Package *)
  | PackageTypeD of { name : string; tparams : string list; params : param list }

and parser_state = string * block
and table_key = expr * string
and table_action = var * arg list
and table_entry = mtch list * table_action
and table_default = table_action * bool
and table_custom = string * expr * bool

(* Program *)
type program = decl list
