module L = Lang.Ast

type ('a, 'b) alt = ('a, 'b) L.alt

(* Numbers *)
type num = L.num
type num' = L.num'
type integer = L.integer
type width = L.width
type signed = L.signed

(* Texts *)
type text = L.text
type text' = L.text'

(* Identifiers *)
type id = L.id
type id' = L.id'

(* Variables (scoped identifiers) *)
type var = L.var
type var' = L.var'

(* Members *)
type member = L.member
type member' = L.member'

(* Match kinds *)
type match_kind = L.match_kind
type match_kind' = L.match_kind'

(* State labels *)
type state_label = L.state_label
type state_label' = L.state_label'

(* Unary operators *)
type unop = L.unop
type unop' = L.unop'

(* Binary operators *)
type binop = L.binop
type binop' = L.binop'

(* Directions *)
type dir = dir' L.dir
and dir' = No | In | Out | InOut

(* Types *)
type typ = typ' L.typ

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
  | SpecT of var * targ list
  | StackT of typ * expr
  | TupleT of typ list
  | AnyT

(* Annotations *)
and anno = expr' L.anno
and anno' = expr' L.anno'

(* Type parameters *)
and tparam = L.tparam
and tparam' = L.tparam'

(* Parameters *)
and param = param' L.param
and param' = L.id * dir * typ * expr option * anno list

(* Constructor parameters *)
and cparam = cparam' L.cparam
and cparam' = param'

(* Type arguments *)
and targ = typ' L.targ
and targ' = typ'

(* Arguments *)
and arg = expr' L.arg
and arg' = expr' L.arg'

(* Expressions *)
and expr = expr' L.expr

and expr' =
  | BoolE of { boolean : bool }
  | StrE of { text : text }
  | NumE of { num : num }
  | VarE of { var : var }
  | TupleE of { exprs : expr list }
  | RecordE of { fields : (member * expr) list }
  | UnE of { unop : unop; expr : expr }
  | BinE of { binop : binop; expr_l : expr; expr_r : expr }
  | TernE of { expr_cond : expr; expr_then : expr; expr_else : expr }
  | CastE of { typ : typ; expr : expr }
  | MaskE of { expr_base : expr; expr_mask : expr }
  | RangeE of { expr_lb : expr; expr_ub : expr }
  | SelectE of { exprs_select : expr list; cases : select_case list }
  | ArrAccE of { expr_base : expr; expr_idx : expr }
  | BitAccE of { expr_base : expr; expr_lo : expr; expr_hi : expr }
  | ErrAccE of { member : member }
  | TypeAccE of { var_base : var; member : member }
  | ExprAccE of { expr_base : expr; member : member }
  | CallE of { expr_func : expr; targs : typ list; args : arg list }
  | InstE of { var_inst : var; targs : typ list; args : arg list }

(* Keyset expressions *)
and keyset = expr' L.keyset
and keyset' = expr' L.keyset'

(* Select-cases for select *)
and select_case = expr' L.select_case
and select_case' = expr' L.select_case'

(* Statements *)
and stmt = (typ', expr', decl') L.stmt
and stmt' = (typ', expr', decl') L.stmt'

(* Blocks (sequence of statements) *)
and block = (typ', expr', decl') L.block
and block' = (typ', expr', decl') L.block'

(* Match-cases for switch *)
and switch_label = L.switch_label
and switch_label' = L.switch_label'
and switch_case = (typ', expr', decl') L.switch_case
and switch_case' = (typ', expr', decl') L.switch_case'

(* Declarations *)
and decl = decl' L.decl

and decl' =
  (* Constant, variable, error, match_kind, and instance declarations *)
  | ConstD of { id : id; typ : typ; value : expr; annos : anno list }
  | VarD of { id : id; typ : typ; init : expr option; annos : anno list }
  | ErrD of { members : member list }
  | MatchKindD of { members : member list }
  | InstD of {
      id : id;
      var_inst : var;
      targs : typ list;
      args : arg list;
      init : decl list;
      annos : anno list;
    }
  (* Type declarations *)
  | StructD of {
      id : id;
      fields : (member * typ * anno list) list;
      annos : anno list;
    }
  | HeaderD of {
      id : id;
      fields : (member * typ * anno list) list;
      annos : anno list;
    }
  | UnionD of {
      id : id;
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
  | NewTypeD of { id : id; typdef : (typ, decl) alt; annos : anno list }
  | TypeDefD of { id : id; typdef : (typ, decl) alt; annos : anno list }
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
      typ_ret : typ;
      tparams : tparam list;
      params : param list;
      body : block;
    }
  | ExternFuncD of {
      id : id;
      typ_ret : typ;
      tparams : tparam list;
      params : param list;
      annos : anno list;
    }
  (* Extern objects *)
  | ExternConstructorD of { id : id; cparams : cparam list; annos : anno list }
  | ExternAbstractMethodD of {
      id : id;
      typ_ret : typ;
      tparams : tparam list;
      params : param list;
      annos : anno list;
    }
  | ExternMethodD of {
      id : id;
      typ_ret : typ;
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
and parser_state = (typ', expr', decl') L.parser_state
and parser_state' = (typ', expr', decl') L.parser_state'

(* Table *)
and table = expr' L.table

(* Table keys *)
and table_key = expr' L.table_key
and table_key' = expr' L.table_key'

(* Table action references *)
and table_action = expr' L.table_action
and table_action' = expr' L.table_action'

(* Table entries *)
and table_entry = expr' L.table_entry
and table_entry' = expr' L.table_entry'

(* Table default properties *)
and table_default = expr' L.table_default
and table_default' = expr' L.table_default'
and table_default_const = L.table_default_const

(* Table custom properties *)
and table_custom = expr' L.table_custom
and table_custom' = expr' L.table_custom'
and table_custom_const = L.table_custom_const

(* Program *)
type program = decl' L.program
