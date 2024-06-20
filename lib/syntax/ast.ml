(* Numbers : value, width, signed *)

type num = Bigint.t * (Bigint.t * bool) option

(* Variable : top-level (prefixed with a dot) or bare name *)

type id = string
type var = Top of id | Bare of id
type field = string
type path = id list

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
and param = id * dir * typ * expr option
and tparam = id
and arg = ExprA of expr | NameA of id * expr | AnyA

(* Expressions *)
and expr =
  | BoolE of bool
  | StrE of string
  | NumE of num
  | VarE of var
  | ListE of expr list
  | RecordE of (field * expr) list
  | UnE of unop * expr
  | BinE of binop * expr * expr
  | TernE of expr * expr * expr
  | CastE of typ * expr
  | MaskE of expr * expr
  | RangeE of expr * expr
  | ArrAccE of expr * expr
  | BitAccE of expr * expr * expr
  | TypeAccE of var * field
  | ErrAccE of field
  | ExprAccE of expr * field
  | CallE of expr * typ list * arg list
  | InstE of typ * arg list

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

and block = stmt list

(* Match-cases for switch *)
and case = CaseC of string | FallC of string | DefaultC
and mtch = ExprM of expr | DefaultM | AnyM
and switch_case = case * block
and mtchkind = string

(* Select-cases for select *)
and label = string
and select_case = mtch list * label

(* Declarations *)
and decl =
  (* Constant, variable, and instance declarations *)
  | ConstD of { name : id; typ : typ; value : expr }
  | VarD of { name : id; typ : typ; init : expr option }
  | InstD of { name : id; typ : typ; args : arg list; init : block option }
  (* Type declarations *)
  | ErrD of { fields : field list }
  | MatchKindD of { fields : field list }
  | StructD of { name : id; fields : (field * typ) list }
  | HeaderD of { name : id; fields : (field * typ) list }
  | UnionD of { name : id; fields : (field * typ) list }
  | EnumD of { name : id; fields : field list }
  | SEnumD of { name : id; typ : typ; fields : (field * expr) list }
  | NewTypeD of { name : id; typ : typ option; decl : decl option }
  | TypeDefD of { name : id; typ : typ option; decl : decl option }
  (* Object declarations *)
  (* Value Set *)
  | ValueSetD of { name : id; typ : typ; size : expr }
  (* Parser *)
  | ParserTypeD of { name : id; tparams : tparam list; params : param list }
  | ParserD of {
      name : id;
      tparams : tparam list;
      params : param list;
      cparams : param list;
      locals : decl list;
      states : parser_state list;
    }
  (* Control *)
  | ActionD of { name : id; params : param list; body : block }
  | TableD of { name : id; table : table }
  | ControlTypeD of { name : id; tparams : tparam list; params : param list }
  | ControlD of {
      name : id;
      tparams : tparam list;
      params : param list;
      cparams : param list;
      locals : decl list;
      body : block;
    }
  (* Functions *)
  | FuncD of {
      name : id;
      rettyp : typ;
      tparams : tparam list;
      params : param list;
      body : block;
    }
  | ExternFuncD of {
      name : id;
      rettyp : typ;
      tparams : tparam list;
      params : param list;
    }
  (* Extern objects *)
  | ConsD of { name : id; cparams : param list }
  | AbstractD of {
      name : id;
      rettyp : typ;
      tparams : tparam list;
      params : param list;
    }
  | MethodD of {
      name : id;
      rettyp : typ;
      tparams : tparam list;
      params : param list;
    }
  | ExternObjectD of { name : id; tparams : tparam list; mthds : decl list }
  (* Package *)
  | PackageTypeD of { name : id; tparams : tparam list; cparams : param list }

(* Parser state machine *)
and parser_state = label * block

(* Table *)
and table_key = expr * mtchkind
and table_action = var * arg list
and table_entry = mtch list * table_action
and table_default = table_action * bool
and table_custom = field * expr * bool

and table =
  table_key list
  * table_action list
  * table_entry list
  * table_default option
  * table_custom list

(* Program *)
type program = decl list
