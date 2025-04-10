module W = Lang.Walk_transform
open Ast
open Util.Source

type walker =
  ( expr',
    note,
    typ',
    value',
    param',
    expr',
    stmt',
    decl',
    table_action',
    table_entry',
    mthd' )
  W.transform_walker

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

let walk_typ (_walker : walker) typ = typ

(* Values *)

let walk_value (_walker : walker) value = value

(* Annotations *)

let walk_anno (walker : walker) anno = W.walk_anno walker anno

(* Type parameters *)

let walk_tparam (walker : walker) tparam = W.walk_tparam walker tparam

(* Parameters *)

let walk_param (walker : walker) param =
  let walk_id = walker.walk_id walker in
  let walk_dir = walker.walk_dir walker in
  let walk_typ = walker.walk_typ walker in
  let walk_value = walker.walk_value walker in
  let it =
    let id, dir, typ, value_default, annos = param.it in
    let id = walk_id id in
    let dir = walk_dir dir in
    let typ = walk_typ typ in
    let value_default = W.walk_option walk_value value_default in
    (id, dir, typ, value_default, annos)
  in
  { param with it }

(* Constructor parameters *)

let walk_cparam (walker : walker) cparam = walk_param walker cparam

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
  let walk_value = walker.walk_value walker in
  let walk_targ = walker.walk_targ walker in
  let walk_arg = walker.walk_arg walker in
  let walk_expr = walker.walk_expr walker in
  let walk_select_case = walker.walk_select_case walker in
  let it =
    match expr.it with
    | ValueE { value } -> ValueE { value = walk_value value }
    | BoolE b -> BoolE b
    | StrE { text } -> StrE { text = walk_text text }
    | NumE { num } -> NumE { num = walk_num num }
    | VarE { var } -> VarE { var = walk_var var }
    | SeqE { exprs } ->
        let exprs = W.walk_list walk_expr exprs in
        SeqE { exprs }
    | SeqDefaultE { exprs } ->
        let exprs = W.walk_list walk_expr exprs in
        SeqDefaultE { exprs }
    | RecordE { fields } ->
        let fields = W.walk_list (W.walk_pair walk_member walk_expr) fields in
        RecordE { fields }
    | RecordDefaultE { fields } ->
        let fields = W.walk_list (W.walk_pair walk_member walk_expr) fields in
        RecordDefaultE { fields }
    | DefaultE -> DefaultE
    | InvalidE -> InvalidE
    | UnE { unop; expr } ->
        let unop = walk_unop unop in
        let expr = walk_expr expr in
        UnE { unop; expr }
    | BinE { binop; expr_l; expr_r } ->
        let binop = walk_binop binop in
        let expr_l = walk_expr expr_l in
        let expr_r = walk_expr expr_r in
        BinE { binop; expr_l; expr_r }
    | TernE { expr_cond; expr_then; expr_else } ->
        let expr_cond = walk_expr expr_cond in
        let expr_then = walk_expr expr_then in
        let expr_else = walk_expr expr_else in
        TernE { expr_cond; expr_then; expr_else }
    | CastE { typ; expr } ->
        let typ = walk_typ typ in
        let expr = walk_expr expr in
        CastE { typ; expr }
    | MaskE { expr_base; expr_mask } ->
        let expr_base = walk_expr expr_base in
        let expr_mask = walk_expr expr_mask in
        MaskE { expr_base; expr_mask }
    | RangeE { expr_lb; expr_ub } ->
        let expr_lb = walk_expr expr_lb in
        let expr_ub = walk_expr expr_ub in
        RangeE { expr_lb; expr_ub }
    | SelectE { exprs_select; cases } ->
        let exprs_select = W.walk_list walk_expr exprs_select in
        let cases = W.walk_list walk_select_case cases in
        SelectE { exprs_select; cases }
    | ArrAccE { expr_base; expr_idx } ->
        let expr_base = walk_expr expr_base in
        let expr_idx = walk_expr expr_idx in
        ArrAccE { expr_base; expr_idx }
    | BitAccE { expr_base; value_lo; value_hi } ->
        let expr_base = walk_expr expr_base in
        let value_lo = walk_value value_lo in
        let value_hi = walk_value value_hi in
        BitAccE { expr_base; value_lo; value_hi }
    | ErrAccE { member } ->
        let member = walk_member member in
        ErrAccE { member }
    | TypeAccE { var_base; member } ->
        let var_base = walk_var var_base in
        let member = walk_member member in
        TypeAccE { var_base; member }
    | ExprAccE { expr_base; member } ->
        let expr_base = walk_expr expr_base in
        let member = walk_member member in
        ExprAccE { expr_base; member }
    | CallFuncE { var_func; targs; args } ->
        let var_func = walk_var var_func in
        let targs = W.walk_list walk_targ targs in
        let args = W.walk_list walk_arg args in
        CallFuncE { var_func; targs; args }
    | CallMethodE { expr_base; member; targs; args } ->
        let expr_base = walk_expr expr_base in
        let member = walk_member member in
        let targs = W.walk_list walk_targ targs in
        let args = W.walk_list walk_arg args in
        CallMethodE { expr_base; member; targs; args }
    | CallTypeE { typ; member } ->
        let typ = walk_typ typ in
        let member = walk_member member in
        CallTypeE { typ; member }
    | InstE { var_inst; targs; targs_hidden; args } ->
        let var_inst = walk_var var_inst in
        let targs = W.walk_list walk_targ targs in
        let targs_hidden = W.walk_list walk_targ targs_hidden in
        let args = W.walk_list walk_arg args in
        InstE { var_inst; targs; targs_hidden; args }
  in
  { expr with it }

(* Keyset expressions *)

let walk_keyset (walker : walker) keyset = W.walk_keyset walker keyset

(* Select-cases for select *)

let walk_select_case (walker : walker) select_case =
  W.walk_select_case walker select_case

(* Statements *)

let walk_stmt (walker : walker) stmt =
  let walk_var = walker.walk_var walker in
  let walk_member = walker.walk_member walker in
  let walk_typ = walker.walk_typ walker in
  let walk_targ = walker.walk_targ walker in
  let walk_arg = walker.walk_arg walker in
  let walk_expr = walker.walk_expr walker in
  let walk_stmt = walker.walk_stmt walker in
  let walk_block = walker.walk_block walker in
  let walk_switch_case = walker.walk_switch_case walker in
  let walk_decl = walker.walk_decl walker in
  let it =
    match stmt.it with
    | EmptyS -> EmptyS
    | AssignS { expr_l; expr_r } ->
        let expr_l = walk_expr expr_l in
        let expr_r = walk_expr expr_r in
        AssignS { expr_l; expr_r }
    | SwitchS { expr_switch; cases } ->
        let expr_switch = walk_expr expr_switch in
        let cases = W.walk_list walk_switch_case cases in
        SwitchS { expr_switch; cases }
    | IfS { expr_cond; stmt_then; stmt_else } ->
        let expr_cond = walk_expr expr_cond in
        let stmt_then = walk_stmt stmt_then in
        let stmt_else = walk_stmt stmt_else in
        IfS { expr_cond; stmt_then; stmt_else }
    | BlockS { block } ->
        let block = walk_block block in
        BlockS { block }
    | ExitS -> ExitS
    | RetS { expr_ret } ->
        let expr_ret = W.walk_option walk_expr expr_ret in
        RetS { expr_ret }
    | CallFuncS { var_func; targs; args } ->
        let var_func = walk_var var_func in
        let targs = W.walk_list walk_targ targs in
        let args = W.walk_list walk_arg args in
        CallFuncS { var_func; targs; args }
    | CallMethodS { expr_base; member; targs; args } ->
        let expr_base = walk_expr expr_base in
        let member = walk_member member in
        let targs = W.walk_list walk_targ targs in
        let args = W.walk_list walk_arg args in
        CallMethodS { expr_base; member; targs; args }
    | CallInstS { typ; var_inst; targs; args } ->
        let typ = walk_typ typ in
        let var_inst = walk_var var_inst in
        let targs = W.walk_list walk_targ targs in
        let args = W.walk_list walk_arg args in
        CallInstS { typ; var_inst; targs; args }
    | TransS { expr_label } ->
        let expr_label = walk_expr expr_label in
        TransS { expr_label }
    | DeclS { decl } ->
        let decl = walk_decl decl in
        DeclS { decl }
  in
  { stmt with it }

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
  let walk_value = walker.walk_value walker in
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
  let it =
    match decl.it with
    | ConstD { id; typ; value; annos } ->
        let id = walk_id id in
        let typ = walk_typ typ in
        let value = walk_value value in
        ConstD { id; typ; value; annos }
    | VarD { id; typ; init; annos } ->
        let id = walk_id id in
        let typ = walk_typ typ in
        let init = W.walk_option walk_expr init in
        VarD { id; typ; init; annos }
    | ErrD { members } ->
        let members = W.walk_list walk_member members in
        ErrD { members }
    | MatchKindD { members } ->
        let members = W.walk_list walk_member members in
        MatchKindD { members }
    | InstD { id; typ; var_inst; targs; targs_hidden; args; init; annos } ->
        let id = walk_id id in
        let typ = walk_typ typ in
        let var_inst = walk_var var_inst in
        let targs = W.walk_list walk_targ targs in
        let targs_hidden = W.walk_list walk_targ targs_hidden in
        let args = W.walk_list walk_arg args in
        let init = W.walk_list walk_decl init in
        InstD { id; typ; var_inst; targs; targs_hidden; args; init; annos }
    | StructD { id; tparams; tparams_hidden; fields; annos } ->
        let id = walk_id id in
        let tparams = W.walk_list walk_tparam tparams in
        let tparams_hidden = W.walk_list walk_tparam tparams_hidden in
        let fields =
          List.map
            (fun (member, typ, annos) ->
              let member = walk_member member in
              let typ = walk_typ typ in
              (member, typ, annos))
            fields
        in
        StructD { id; tparams; tparams_hidden; fields; annos }
    | HeaderD { id; tparams; tparams_hidden; fields; annos } ->
        let id = walk_id id in
        let tparams = W.walk_list walk_tparam tparams in
        let tparams_hidden = W.walk_list walk_tparam tparams_hidden in
        let fields =
          List.map
            (fun (member, typ, annos) ->
              let member = walk_member member in
              let typ = walk_typ typ in
              (member, typ, annos))
            fields
        in
        HeaderD { id; tparams; tparams_hidden; fields; annos }
    | UnionD { id; tparams; tparams_hidden; fields; annos } ->
        let id = walk_id id in
        let tparams = W.walk_list walk_tparam tparams in
        let tparams_hidden = W.walk_list walk_tparam tparams_hidden in
        let fields =
          List.map
            (fun (member, typ, annos) ->
              let member = walk_member member in
              let typ = walk_typ typ in
              (member, typ, annos))
            fields
        in
        UnionD { id; tparams; tparams_hidden; fields; annos }
    | EnumD { id; members; annos } ->
        let id = walk_id id in
        let members = W.walk_list walk_member members in
        EnumD { id; members; annos }
    | SEnumD { id; typ; fields; annos } ->
        let id = walk_id id in
        let typ = walk_typ typ in
        let fields = W.walk_list (W.walk_pair walk_member walk_value) fields in
        SEnumD { id; typ; fields; annos }
    | NewTypeD { id; typdef; annos } ->
        let id = walk_id id in
        let typdef = W.walk_alt walk_typ walk_decl typdef in
        NewTypeD { id; typdef; annos }
    | TypeDefD { id; typdef; annos } ->
        let id = walk_id id in
        let typdef = W.walk_alt walk_typ walk_decl typdef in
        TypeDefD { id; typdef; annos }
    | ValueSetD { id; typ; size; annos } ->
        let id = walk_id id in
        let typ = walk_typ typ in
        let size = walk_expr size in
        ValueSetD { id; typ; size; annos }
    | ParserTypeD { id; tparams; tparams_hidden; params; annos } ->
        let id = walk_id id in
        let tparams = W.walk_list walk_tparam tparams in
        let tparams_hidden = W.walk_list walk_tparam tparams_hidden in
        let params = W.walk_list walk_param params in
        ParserTypeD { id; tparams; tparams_hidden; params; annos }
    | ParserD { id; tparams; params; cparams; locals; states; annos } ->
        let id = walk_id id in
        let tparams = W.walk_list walk_tparam tparams in
        let params = W.walk_list walk_param params in
        let cparams = W.walk_list walk_cparam cparams in
        let locals = W.walk_list walk_decl locals in
        let states = W.walk_list walk_parser_state states in
        ParserD { id; tparams; params; cparams; locals; states; annos }
    | TableD { id; typ; table; annos } ->
        let id = walk_id id in
        let typ = walk_typ typ in
        let table = walk_table table in
        TableD { id; typ; table; annos }
    | ControlTypeD { id; tparams; tparams_hidden; params; annos } ->
        let id = walk_id id in
        let tparams = W.walk_list walk_tparam tparams in
        let tparams_hidden = W.walk_list walk_tparam tparams_hidden in
        let params = W.walk_list walk_param params in
        ControlTypeD { id; tparams; tparams_hidden; params; annos }
    | ControlD { id; tparams; params; cparams; locals; body; annos } ->
        let id = walk_id id in
        let tparams = W.walk_list walk_tparam tparams in
        let params = W.walk_list walk_param params in
        let cparams = W.walk_list walk_cparam cparams in
        let locals = W.walk_list walk_decl locals in
        let body = walk_block body in
        ControlD { id; tparams; params; cparams; locals; body; annos }
    | ActionD { id; params; body; annos } ->
        let id = walk_id id in
        let params = W.walk_list walk_param params in
        let body = walk_block body in
        ActionD { id; params; body; annos }
    | FuncD { id; typ_ret; tparams; tparams_hidden; params; body } ->
        let id = walk_id id in
        let typ_ret = walk_typ typ_ret in
        let tparams = W.walk_list walk_tparam tparams in
        let tparams_hidden = W.walk_list walk_tparam tparams_hidden in
        let params = W.walk_list walk_param params in
        let body = walk_block body in
        FuncD { id; typ_ret; tparams; tparams_hidden; params; body }
    | ExternFuncD { id; typ_ret; tparams; tparams_hidden; params; annos } ->
        let id = walk_id id in
        let typ_ret = walk_typ typ_ret in
        let tparams = W.walk_list walk_tparam tparams in
        let tparams_hidden = W.walk_list walk_tparam tparams_hidden in
        let params = W.walk_list walk_param params in
        ExternFuncD { id; typ_ret; tparams; tparams_hidden; params; annos }
    | ExternObjectD { id; tparams; mthds; annos } ->
        let id = walk_id id in
        let tparams = W.walk_list walk_tparam tparams in
        let mthds = W.walk_list walk_mthd mthds in
        ExternObjectD { id; tparams; mthds; annos }
    | PackageTypeD { id; tparams; tparams_hidden; cparams; annos } ->
        let id = walk_id id in
        let tparams = W.walk_list walk_tparam tparams in
        let tparams_hidden = W.walk_list walk_tparam tparams_hidden in
        let cparams = W.walk_list walk_param cparams in
        PackageTypeD { id; tparams; tparams_hidden; cparams; annos }
  in
  { decl with it }

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
  let it =
    let var, args, annos, params_data, params_control = table_action.it in
    let var = walk_var var in
    let args = W.walk_list walk_arg args in
    (var, args, annos, params_data, params_control)
  in
  { table_action with it }

let walk_table_actions (walker : walker) table_actions =
  W.walk_table_actions walker table_actions

(* Table entries *)

let walk_table_entry (walker : walker) table_entry =
  let walk_value = walker.walk_value walker in
  let walk_keyset = walker.walk_keyset walker in
  let walk_table_action = walker.walk_table_action walker in
  let it =
    let table_entry_const, keysets, table_action, table_entry_priority, annos =
      table_entry.it
    in
    let keysets = W.walk_list walk_keyset keysets in
    let table_action = walk_table_action table_action in
    let table_entry_priority = W.walk_option walk_value table_entry_priority in
    (table_entry_const, keysets, table_action, table_entry_priority, annos)
  in
  { table_entry with it }

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
  let it =
    match mthd.it with
    | ExternConsM { id; tparams_hidden; cparams; annos } ->
        let id = walk_id id in
        let tparams_hidden = W.walk_list walk_tparam tparams_hidden in
        let cparams = W.walk_list walk_cparam cparams in
        ExternConsM { id; tparams_hidden; cparams; annos }
    | ExternAbstractM { id; typ_ret; tparams; tparams_hidden; params; annos } ->
        let id = walk_id id in
        let typ_ret = walk_typ typ_ret in
        let tparams = W.walk_list walk_tparam tparams in
        let tparams_hidden = W.walk_list walk_tparam tparams_hidden in
        let params = W.walk_list walk_cparam params in
        ExternAbstractM { id; typ_ret; tparams; tparams_hidden; params; annos }
    | ExternM { id; typ_ret; tparams; tparams_hidden; params; annos } ->
        let id = walk_id id in
        let typ_ret = walk_typ typ_ret in
        let tparams = W.walk_list walk_tparam tparams in
        let tparams_hidden = W.walk_list walk_tparam tparams_hidden in
        let params = W.walk_list walk_cparam params in
        ExternM { id; typ_ret; tparams; tparams_hidden; params; annos }
  in
  { mthd with it }

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
