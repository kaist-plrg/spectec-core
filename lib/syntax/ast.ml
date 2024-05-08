(* Numbers : value, width, signed *)

type num = Bigint.t * (Bigint.t * bool) option

(* Variable : top-level (prefixed with a dot) or bare name *)

type var = Top of string | Bare of string

(* Unary and binary operators *)

type unop = BNot | LNot | UMinus

type binop =
  | Plus
  | SPlus
  | Minus
  | SMinus
  | Mul
  | Div
  | Mod
  | Shl
  | Shr
  | Le
  | Ge
  | Lt
  | Gt
  | Eq
  | Ne
  | BAnd
  | BXor
  | BOr
  | Concat
  | LAnd
  | LOr

(* Types *)

type typ =
  | TVoid
  | TBool
  | TMatchKind
  | TErr
  | TStr
  | TAInt
  | TInt of expr
  | TBit of expr
  | TVBit of expr
  | TName of var
  | TSpec of var * typ list
  | TStack of typ * expr
  | TTuple of typ list
  | TAny

(* Parameters and Arguments *)
and dir = No | In | Out | InOut
and param = string * dir * typ * expr option
and arg = AExpr of expr | AName of string * expr | AAny

(* Expressions *)
and expr =
  | EBool of bool
  | ENum of num
  | EStr of string
  | EVar of var
  | EList of expr list
  | ERecord of (string * expr) list
  | EUnop of unop * expr
  | EBinop of binop * expr * expr
  | ETern of expr * expr * expr
  | ECast of typ * expr
  | EMask of expr * expr
  | ERange of expr * expr
  | EAccArr of expr * expr
  | EAccBit of expr * expr * expr
  | EMemTyp of var * string
  | EMemErr of string
  | EMemExpr of expr * string
  | ECall of expr * typ list * arg list
  | EInst of typ * arg list

(* Match-cases *)
and mtch = MExpr of expr | MDefault | MAny
and case = CCase of string | CDefault

(* Statements *)
and stmt =
  | SEmpty
  | SAssign of expr * expr
  | SSwitch of expr * switch_case list
  | SCond of expr * stmt * stmt
  | SBlock of block
  | SExit
  | SReturn of expr option
  | SCall of expr * typ list * arg list
  | STrans of string
  | SSelect of expr list * select_case list
  | SDecl of decl

and switch_case = case * block
and select_case = mtch list * string
and block = stmt list

(* Declarations *)
and decl =
  (* Constant, variable, and instance declarations *)
  | DConst of { name : string; typ : typ; value : expr }
  | DVar of { name : string; typ : typ; init : expr option }
  | DInst of { name : string; typ : typ; args : arg list; init : block option }
  (* Type declarations *)
  | DErr of { members : string list }
  | DMatchKind of { members : string list }
  | DStruct of { name : string; fields : (string * typ) list }
  | DHeader of { name : string; fields : (string * typ) list }
  | DUnion of { name : string; fields : (string * typ) list }
  | DEnum of { name : string; members : string list }
  | DSEnum of { name : string; typ : typ; members : (string * expr) list }
  | DNewTyp of { name : string; typ : typ option; decl : decl option }
  | DDefTyp of { name : string; typ : typ option; decl : decl option }
  (* Object declarations *)
  (* Value Set *)
  | DVSet of { name : string; typ : typ; size : expr }
  (* Parser *)
  | DParserTyp of { name : string; tparams : string list; params : param list }
  | DParser of {
      name : string;
      tparams : string list;
      params : param list;
      cparams : param list;
      locals : decl list;
      states : parser_state list;
    }
  (* Control *)
  | DAction of { name : string; params : param list; body : block }
  | DTable of {
      name : string;
      key : table_key list;
      actions : table_action list;
      entries : table_entry list;
      default : table_default option;
      custom : table_custom list;
    }
  | DControlTyp of { name : string; tparams : string list; params : param list }
  | DControl of {
      name : string;
      tparams : string list;
      params : param list;
      cparams : param list;
      locals : decl list;
      body : block;
    }
  (* Functions *)
  | DFunc of {
      name : string;
      rettyp : typ;
      tparams : string list;
      params : param list;
      body : block;
    }
  | DExtFunc of {
      name : string;
      rettyp : typ;
      tparams : string list;
      params : param list;
    }
  (* Extern objects *)
  | DCons of { name : string; params : param list }
  | DAbstract of {
      name : string;
      rettyp : typ;
      tparams : string list;
      params : param list;
    }
  | DMethod of {
      name : string;
      rettyp : typ;
      tparams : string list;
      params : param list;
    }
  | DExtObj of { name : string; tparams : string list; methods : decl list }
  (* Package *)
  | DPkgTyp of { name : string; tparams : string list; params : param list }

and parser_state = string * block
and table_key = expr * string
and table_action = var * arg list
and table_entry = mtch list * table_action
and table_default = table_action * bool
and table_custom = string * expr * bool

(* Program *)
type program = decl list
