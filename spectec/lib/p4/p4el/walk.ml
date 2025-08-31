module W = P4lang.Walk
open Ast
open P4util.Source

type walker =
  ( unit,
    typ',
    value',
    param',
    expr',
    stmt',
    decl',
    table_action',
    table_entry',
    mthd' )
  W.walker

(* Numbers *)

let walk_num (walker : walker) num = W.walk_num walker num

(* Texts *)

let walk_text (walker : walker) text = W.walk_text walker text

(* Identifiers *)

let walk_id (walker : walker) id = W.walk_id walker id

(* Variables (scoped identifiers) *)

let walk_var (walker : walker) var = W.walk_var walker var

(* Members *)

let walk_member (walker : walker) member = W.walk_member walker member

(* State labels *)

let walk_state_label (walker : walker) state_label =
  W.walk_state_label walker state_label

(* Match kinds *)

let walk_match_kind (walker : walker) match_kind =
  W.walk_match_kind walker match_kind

(* Unary operators *)

let walk_unop (walker : walker) unop = W.walk_unop walker unop

(* Binary operators *)

let walk_binop (walker : walker) binop = W.walk_binop walker binop

(* Directions *)

let walk_dir (walker : walker) dir = W.walk_dir walker dir

(* Types *)

let walk_typ (walker : walker) typ =
  let walk_var = walker.walk_var walker in
  let walk_typ = walker.walk_typ walker in
  let walk_targ = walker.walk_targ walker in
  let walk_expr = walker.walk_expr walker in
  match typ.it with
  | VoidT | ErrT | MatchKindT | StrT | BoolT | IntT -> ()
  | FIntT expr_width | FBitT expr_width | VBitT expr_width ->
      walk_expr expr_width
  | NameT var -> walk_var var
  | SpecT (var, targs) ->
      walk_var var;
      W.walk_list walk_targ targs
  | StackT (typ, expr_size) ->
      walk_typ typ;
      walk_expr expr_size
  | ListT typ -> walk_typ typ
  | TupleT typs -> W.walk_list walk_typ typs
  | AnyT -> ()

(* Values *)

let walk_value (_walker : walker) _value = failwith "TODO"

(* Annotations *)

let walk_anno (walker : walker) anno = W.walk_anno walker anno

(* Type parameters *)

let walk_tparam (walker : walker) tparam = W.walk_tparam walker tparam

(* Parameters *)

let walk_param (walker : walker) param =
  let walk_id = walker.walk_id walker in
  let walk_dir = walker.walk_dir walker in
  let walk_typ = walker.walk_typ walker in
  let walk_expr = walker.walk_expr walker in
  let id, dir, typ, expr_default, _annos = param.it in
  walk_id id;
  walk_dir dir;
  walk_typ typ;
  W.walk_option walk_expr expr_default

(* Constructor parameters *)

let walk_cparam (walker : walker) cparam = walker.walk_param walker cparam

(* Type arguments *)

let walk_targ (walker : walker) targ = W.walk_targ walker targ

(* Arguments *)

let walk_arg (walker : walker) arg = W.walk_arg walker arg

(* Expressions *)

let walk_expr (walker : walker) expr =
  let walk_num = walker.walk_num walker in
  let walk_text = walker.walk_text walker in
  let walk_var = walker.walk_var walker in
  let walk_member = walker.walk_member walker in
  let walk_unop = walker.walk_unop walker in
  let walk_binop = walker.walk_binop walker in
  let walk_typ = walker.walk_typ walker in
  let walk_targ = walker.walk_targ walker in
  let walk_arg = walker.walk_arg walker in
  let walk_expr = walker.walk_expr walker in
  let walk_select_case = walker.walk_select_case walker in
  match expr.it with
  | BoolE _ -> ()
  | StrE { text } -> walk_text text
  | NumE { num } -> walk_num num
  | VarE { var } -> walk_var var
  | SeqE { exprs } | SeqDefaultE { exprs } -> W.walk_list walk_expr exprs
  | RecordE { fields } | RecordDefaultE { fields } ->
      W.walk_list (W.walk_pair walk_member walk_expr) fields
  | DefaultE | InvalidE -> ()
  | UnE { unop; expr } ->
      walk_unop unop;
      walk_expr expr
  | BinE { binop; expr_l; expr_r } ->
      walk_binop binop;
      walk_expr expr_l;
      walk_expr expr_r
  | TernE { expr_cond; expr_then; expr_else } ->
      walk_expr expr_cond;
      walk_expr expr_then;
      walk_expr expr_else
  | CastE { typ; expr } ->
      walk_typ typ;
      walk_expr expr
  | MaskE { expr_base; expr_mask } ->
      walk_expr expr_base;
      walk_expr expr_mask
  | RangeE { expr_lb; expr_ub } ->
      walk_expr expr_lb;
      walk_expr expr_ub
  | SelectE { exprs_select; cases } ->
      W.walk_list walk_expr exprs_select;
      W.walk_list walk_select_case cases
  | ArrAccE { expr_base; expr_idx } ->
      walk_expr expr_base;
      walk_expr expr_idx
  | BitAccE { expr_base; expr_lo; expr_hi } ->
      walk_expr expr_base;
      walk_expr expr_lo;
      walk_expr expr_hi
  | TypeAccE { var_base; member } ->
      walk_var var_base;
      walk_member member
  | ErrAccE { member } -> walk_member member
  | ExprAccE { expr_base; member } ->
      walk_expr expr_base;
      walk_member member
  | CallFuncE { var_func; targs; args } ->
      walk_var var_func;
      W.walk_list walk_targ targs;
      W.walk_list walk_arg args
  | CallMethodE { expr_base; member; targs; args } ->
      walk_expr expr_base;
      walk_member member;
      W.walk_list walk_targ targs;
      W.walk_list walk_arg args
  | CallTypeE { var_typ; member; targs; args } ->
      walk_var var_typ;
      walk_member member;
      W.walk_list walk_targ targs;
      W.walk_list walk_arg args
  | InstE { var_inst; targs; args } ->
      walk_var var_inst;
      W.walk_list walk_targ targs;
      W.walk_list walk_arg args

(* Keyset expressions *)

let walk_keyset (walker : walker) keyset = W.walk_keyset walker keyset

(* Select-cases for select *)

let walk_select_case (walker : walker) select_case =
  W.walk_select_case walker select_case

(* Statements *)

let walk_stmt (walker : walker) stmt =
  let walk_var = walker.walk_var walker in
  let walk_member = walker.walk_member walker in
  let walk_targ = walker.walk_targ walker in
  let walk_arg = walker.walk_arg walker in
  let walk_expr = walker.walk_expr walker in
  let walk_stmt = walker.walk_stmt walker in
  let walk_block = walker.walk_block walker in
  let walk_switch_case = walker.walk_switch_case walker in
  let walk_decl = walker.walk_decl walker in
  match stmt.it with
  | EmptyS -> ()
  | AssignS { expr_l; expr_r } ->
      walk_expr expr_l;
      walk_expr expr_r
  | SwitchS { expr_switch; cases } ->
      walk_expr expr_switch;
      W.walk_list walk_switch_case cases
  | IfS { expr_cond; stmt_then; stmt_else } ->
      walk_expr expr_cond;
      walk_stmt stmt_then;
      walk_stmt stmt_else
  | BlockS { block } -> walk_block block
  | ExitS -> ()
  | RetS { expr_ret } -> W.walk_option walk_expr expr_ret
  | CallFuncS { var_func; targs; args } ->
      walk_var var_func;
      W.walk_list walk_targ targs;
      W.walk_list walk_arg args
  | CallMethodS { expr_base; member; targs; args } ->
      walk_expr expr_base;
      walk_member member;
      W.walk_list walk_targ targs;
      W.walk_list walk_arg args
  | CallInstS { var_inst; targs; args } ->
      walk_var var_inst;
      W.walk_list walk_targ targs;
      W.walk_list walk_arg args
  | TransS { expr_label } -> walk_expr expr_label
  | DeclS { decl } -> walk_decl decl

(* Blocks (sequence of statements) *)

let walk_block (walker : walker) block = W.walk_block walker block

(* Match-cases for switch *)

let walk_switch_label (walker : walker) switch_label =
  W.walk_switch_label walker switch_label

let walk_switch_case (walker : walker) switch_case =
  W.walk_switch_case walker switch_case

(* Declarations *)

let walk_decl (walker : walker) decl =
  let walk_id = walker.walk_id walker in
  let walk_var = walker.walk_var walker in
  let walk_member = walker.walk_member walker in
  let walk_typ = walker.walk_typ walker in
  let walk_tparam = walker.walk_tparam walker in
  let walk_param = walker.walk_param walker in
  let walk_cparam = walker.walk_cparam walker in
  let walk_targ = walker.walk_targ walker in
  let walk_arg = walker.walk_arg walker in
  let walk_expr = walker.walk_expr walker in
  let walk_block = walker.walk_block walker in
  let walk_decl = walker.walk_decl walker in
  let walk_parser_state = walker.walk_parser_state walker in
  let walk_table = walker.walk_table walker in
  let walk_mthd = walker.walk_mthd walker in
  match decl.it with
  | ConstD { id; typ; value; annos = _annos } ->
      walk_id id;
      walk_typ typ;
      walk_expr value
  | VarD { id; typ; init; annos = _annos } ->
      walk_id id;
      walk_typ typ;
      W.walk_option walk_expr init
  | ErrD { members } | MatchKindD { members } -> W.walk_list walk_member members
  | InstD { id; var_inst; targs; args; init; annos = _annos } ->
      walk_id id;
      walk_var var_inst;
      W.walk_list walk_targ targs;
      W.walk_list walk_arg args;
      W.walk_list walk_decl init
  | StructD { id; tparams; fields; annos = _annos }
  | HeaderD { id; tparams; fields; annos = _annos }
  | UnionD { id; tparams; fields; annos = _annos } ->
      walk_id id;
      W.walk_list walk_tparam tparams;
      List.map (fun (member, typ, _) -> (member, typ)) fields
      |> W.walk_list (W.walk_pair walk_member walk_typ)
  | EnumD { id; members; annos = _annos } ->
      walk_id id;
      W.walk_list walk_member members
  | SEnumD { id; typ; fields; annos = _annos } ->
      walk_id id;
      walk_typ typ;
      W.walk_list (W.walk_pair walk_member walk_expr) fields
  | NewTypeD { id; typdef; annos = _annos }
  | TypeDefD { id; typdef; annos = _annos } ->
      walk_id id;
      W.walk_alt walk_typ walk_decl typdef
  | ValueSetD { id; typ; size; annos = _annos } ->
      walk_id id;
      walk_typ typ;
      walk_expr size
  | ParserTypeD { id; tparams; params; annos = _annos } ->
      walk_id id;
      W.walk_list walk_tparam tparams;
      W.walk_list walk_param params
  | ParserD { id; tparams; params; cparams; locals; states; annos = _annos } ->
      walk_id id;
      W.walk_list walk_tparam tparams;
      W.walk_list walk_param params;
      W.walk_list walk_cparam cparams;
      W.walk_list walk_decl locals;
      W.walk_list walk_parser_state states
  | TableD { id; table; annos = _annos } ->
      walk_id id;
      walk_table table
  | ControlTypeD { id; tparams; params; annos = _annos } ->
      walk_id id;
      W.walk_list walk_tparam tparams;
      W.walk_list walk_param params
  | ControlD { id; tparams; params; cparams; locals; body; annos = _annos } ->
      walk_id id;
      W.walk_list walk_tparam tparams;
      W.walk_list walk_param params;
      W.walk_list walk_cparam cparams;
      W.walk_list walk_decl locals;
      walk_block body
  | ActionD { id; params; body; annos = _annos } ->
      walk_id id;
      W.walk_list walk_param params;
      walk_block body
  | FuncD { id; typ_ret; tparams; params; body } ->
      walk_id id;
      walk_typ typ_ret;
      W.walk_list walk_tparam tparams;
      W.walk_list walk_param params;
      walk_block body
  | ExternFuncD { id; typ_ret; tparams; params; annos = _annos } ->
      walk_id id;
      walk_typ typ_ret;
      W.walk_list walk_tparam tparams;
      W.walk_list walk_param params
  | ExternObjectD { id; tparams; mthds; annos = _annos } ->
      walk_id id;
      W.walk_list walk_tparam tparams;
      W.walk_list walk_mthd mthds
  | PackageTypeD { id; tparams; cparams; annos = _annos } ->
      walk_id id;
      W.walk_list walk_tparam tparams;
      W.walk_list walk_param cparams

(* Parser state machine *)

let walk_parser_state (walker : walker) parser_state =
  W.walk_parser_state walker parser_state

(* Tables *)

let walk_table (walker : walker) table = W.walk_table walker table

(* Table properties *)

let walk_table_property (walker : walker) table_property =
  W.walk_table_property walker table_property

(* Table keys *)

let walk_table_key (walker : walker) table_key =
  W.walk_table_key walker table_key

let walk_table_keys (walker : walker) table_keys =
  W.walk_table_keys walker table_keys

(* Table action references *)

let walk_table_action (walker : walker) table_action =
  let walk_var = walker.walk_var walker in
  let walk_arg = walker.walk_arg walker in
  let var, args, _annos = table_action.it in
  walk_var var;
  W.walk_list walk_arg args

let walk_table_actions (walker : walker) table_actions =
  W.walk_table_actions walker table_actions

(* Table entries *)

let walk_table_entry (walker : walker) table_entry =
  let walk_expr = walker.walk_expr walker in
  let walk_keyset = walker.walk_keyset walker in
  let walk_table_action = walker.walk_table_action walker in
  let _table_entry_const, keysets, table_action, table_entry_priority, _annos =
    table_entry.it
  in
  W.walk_list walk_keyset keysets;
  walk_table_action table_action;
  W.walk_option walk_expr table_entry_priority

let walk_table_entries (walker : walker) table_entries =
  W.walk_table_entries walker table_entries

(* Table default properties *)

let walk_table_default (walker : walker) table_default =
  W.walk_table_default walker table_default

(* Table custom properties *)

let walk_table_custom (walker : walker) table_custom =
  W.walk_table_custom walker table_custom

(* Methods *)

let walk_mthd (walker : walker) mthd =
  let walk_id = walker.walk_id walker in
  let walk_tparam = walker.walk_tparam walker in
  let walk_cparam = walker.walk_cparam walker in
  let walk_typ = walker.walk_typ walker in
  match mthd.it with
  | ExternConsM { id; cparams; annos = _annos } ->
      walk_id id;
      W.walk_list walk_cparam cparams
  | ExternAbstractM { id; typ_ret; tparams; params; annos = _annos }
  | ExternM { id; typ_ret; tparams; params; annos = _annos } ->
      walk_id id;
      walk_typ typ_ret;
      W.walk_list walk_tparam tparams;
      W.walk_list walk_cparam params

(* Program *)

let walk_program (walker : walker) program = W.walk_program walker program

(* Walker *)

let walker : walker =
  {
    walk_num;
    walk_text;
    walk_id;
    walk_var;
    walk_member;
    walk_state_label;
    walk_match_kind;
    walk_unop;
    walk_binop;
    walk_dir;
    walk_typ;
    walk_value;
    walk_anno;
    walk_tparam;
    walk_param;
    walk_cparam;
    walk_targ;
    walk_arg;
    walk_expr;
    walk_keyset;
    walk_select_case;
    walk_stmt;
    walk_block;
    walk_switch_label;
    walk_switch_case;
    walk_decl;
    walk_parser_state;
    walk_table;
    walk_table_property;
    walk_table_key;
    walk_table_keys;
    walk_table_action;
    walk_table_actions;
    walk_table_entry;
    walk_table_entries;
    walk_table_default;
    walk_table_custom;
    walk_mthd;
    walk_program;
  }
