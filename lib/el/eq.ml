module E = Lang.Eq
open Ast
open Util.Source

(* Numbers *)

let eq_num' num_a num_b = E.eq_num' num_a num_b
let eq_num num_a num_b = E.eq_num num_a num_b

(* Texts *)

let eq_text' text_a text_b = E.eq_text' text_a text_b
let eq_text text_a text_b = E.eq_text text_a text_b
let eq_texts texts_a texts_b = E.eq_texts texts_a texts_b

(* Identifiers *)

let eq_id' id_a id_b = E.eq_id' id_a id_b
let eq_id id_a id_b = E.eq_id id_a id_b

(* Variables (scoped identifiers) *)

let eq_var' var_a var_b = E.eq_var' var_a var_b
let eq_var var_a var_b = E.eq_var var_a var_b

(* Members *)

let eq_member' member_a member_b = E.eq_member' member_a member_b
let eq_member member_a member_b = E.eq_member member_a member_b
let eq_members members_a members_b = E.eq_members members_a members_b

(* State labels *)

let eq_state_label' state_label_a state_label_b =
  E.eq_state_label' state_label_a state_label_b

let eq_state_label state_label_a state_label_b =
  E.eq_state_label state_label_a state_label_b

(* Match kinds *)

let eq_match_kind' match_kind_a match_kind_b =
  E.eq_match_kind' match_kind_a match_kind_b

let eq_match_kind match_kind_a match_kind_b =
  E.eq_match_kind match_kind_a match_kind_b

(* Unary operators *)

let eq_unop' unop_a unop_b = E.eq_unop' unop_a unop_b
let eq_unop unop_a unop_b = E.eq_unop unop_a unop_b

(* Binary operators *)

let eq_binop' binop_a binop_b = E.eq_binop' binop_a binop_b
let eq_binop binop_a binop_b = E.eq_binop binop_a binop_b

(* Directions *)

let eq_dir' dir_a dir_b = E.eq_dir' dir_a dir_b
let eq_dir dir_a dir_b = E.eq_dir dir_a dir_b

(* Types *)

let rec eq_typ' typ_a typ_b =
  match (typ_a, typ_b) with
  | VoidT, VoidT
  | MatchKindT, MatchKindT
  | ErrT, ErrT
  | StrT, StrT
  | BoolT, BoolT
  | IntT, IntT ->
      true
  | FIntT expr_width_a, FIntT expr_width_b
  | FBitT expr_width_a, FBitT expr_width_b
  | VBitT expr_width_a, VBitT expr_width_b ->
      eq_expr expr_width_a expr_width_b
  | NameT var_a, NameT var_b -> eq_var var_a var_b
  | SpecT (var_a, targs_a), SpecT (var_b, targs_b) ->
      eq_var var_a var_b && eq_targs targs_a targs_b
  | StackT (typ_a, expr_size_a), StackT (typ_b, expr_size_b) ->
      eq_typ typ_a typ_b && eq_expr expr_size_a expr_size_b
  | ListT typ_a, ListT typ_b -> eq_typ typ_a typ_b
  | TupleT typs_a, TupleT typs_b -> eq_typs typs_a typs_b
  | AnyT, AnyT -> true
  | _ -> false

and eq_typ typ_a typ_b = eq_typ' typ_a.it typ_b.it
and eq_typs typs_a typs_b = E.eq_list eq_typ typs_a typs_b

(* Annotations *)

and eq_anno' anno_a anno_b = E.eq_anno' eq_expr anno_a anno_b
and eq_anno anno_a anno_b = E.eq_anno eq_expr anno_a anno_b
and eq_annos annos_a annos_b = E.eq_annos eq_expr annos_a annos_b

(* Type parameters *)

and eq_tparam' tparam_a tparam_b = E.eq_tparam' tparam_a tparam_b
and eq_tparam tparam_a tparam_b = E.eq_tparam tparam_a tparam_b
and eq_tparams tparams_a tparams_b = E.eq_list eq_tparam tparams_a tparams_b

(* Parameters *)

and eq_param' param_a param_b =
  let id_a, dir_a, typ_a, value_default_a, annos_a = param_a in
  let id_b, dir_b, typ_b, value_default_b, annos_b = param_b in
  eq_id id_a id_b && eq_dir dir_a dir_b && eq_typ typ_a typ_b
  && E.eq_option eq_expr value_default_a value_default_b
  && eq_annos annos_a annos_b

and eq_param param_a param_b = eq_param' param_a.it param_b.it
and eq_params params_a params_b = E.eq_list eq_param params_a params_b

(* Constructor parameters *)

and eq_cparam' cparam_a cparam_b = eq_param' cparam_a cparam_b
and eq_cparam cparam_a cparam_b = eq_cparam' cparam_a.it cparam_b.it
and eq_cparams cparams_a cparams_b = E.eq_list eq_cparam cparams_a cparams_b

(* Type arguments *)

and eq_targ' targ_a targ_b = eq_typ targ_a targ_b
and eq_targ targ_a targ_b = E.eq_targ eq_typ targ_a targ_b
and eq_targs targs_a targs_b = E.eq_targs eq_typ targs_a targs_b

(* Arguments *)

and eq_arg' arg_a arg_b = E.eq_arg' eq_expr arg_a arg_b
and eq_arg arg_a arg_b = E.eq_arg eq_expr arg_a arg_b
and eq_args args_a args_b = E.eq_args eq_expr args_a args_b

(* Expressions *)

and eq_expr' expr_a expr_b =
  match (expr_a, expr_b) with
  | BoolE { boolean = boolean_a }, BoolE { boolean = boolean_b } ->
      boolean_a = boolean_b
  | StrE { text = text_a }, StrE { text = text_b } -> eq_text text_a text_b
  | NumE { num = num_a }, NumE { num = num_b } -> eq_num num_a num_b
  | VarE { var = var_a }, VarE { var = var_b } -> eq_var var_a var_b
  | SeqE { exprs = exprs_a }, SeqE { exprs = exprs_b } ->
      eq_exprs exprs_a exprs_b
  | RecordE { fields = fields_a }, RecordE { fields = fields_b } ->
      E.eq_pairs eq_id eq_expr fields_a fields_b
  | UnE { unop = unop_a; expr = expr_a }, UnE { unop = unop_b; expr = expr_b }
    ->
      eq_unop unop_a unop_b && eq_expr expr_a expr_b
  | ( BinE { binop = binop_a; expr_l = expr_l_a; expr_r = expr_r_a },
      BinE { binop = binop_b; expr_l = expr_l_b; expr_r = expr_r_b } ) ->
      eq_binop binop_a binop_b && eq_expr expr_l_a expr_l_b
      && eq_expr expr_r_a expr_r_b
  | ( TernE
        {
          expr_cond = expr_cond_a;
          expr_then = expr_then_a;
          expr_else = expr_else_a;
        },
      TernE
        {
          expr_cond = expr_cond_b;
          expr_then = expr_then_b;
          expr_else = expr_else_b;
        } ) ->
      eq_expr expr_cond_a expr_cond_b
      && eq_expr expr_then_a expr_then_b
      && eq_expr expr_else_a expr_else_b
  | CastE { typ = typ_a; expr = expr_a }, CastE { typ = typ_b; expr = expr_b }
    ->
      eq_typ typ_a typ_b && eq_expr expr_a expr_b
  | ( MaskE { expr_base = expr_base_a; expr_mask = expr_mask_a },
      MaskE { expr_base = expr_base_b; expr_mask = expr_mask_b } ) ->
      eq_expr expr_base_a expr_base_b && eq_expr expr_mask_a expr_mask_b
  | ( RangeE { expr_lb = expr_lb_a; expr_ub = expr_ub_a },
      RangeE { expr_lb = expr_lb_b; expr_ub = expr_ub_b } ) ->
      eq_expr expr_lb_a expr_lb_b && eq_expr expr_ub_a expr_ub_b
  | ( SelectE { exprs_select = exprs_select_a; cases = cases_a },
      SelectE { exprs_select = exprs_select_b; cases = cases_b } ) ->
      eq_exprs exprs_select_a exprs_select_b && eq_select_cases cases_a cases_b
  | ( ArrAccE { expr_base = expr_base_a; expr_idx = expr_idx_a },
      ArrAccE { expr_base = expr_base_b; expr_idx = expr_idx_b } ) ->
      eq_expr expr_base_a expr_base_b && eq_expr expr_idx_a expr_idx_b
  | ( BitAccE
        { expr_base = expr_base_a; expr_lo = expr_lo_a; expr_hi = expr_hi_a },
      BitAccE
        { expr_base = expr_base_b; expr_lo = expr_lo_b; expr_hi = expr_hi_b } )
    ->
      eq_expr expr_base_a expr_base_b
      && eq_expr expr_lo_a expr_lo_b
      && eq_expr expr_hi_a expr_hi_b
  | ( TypeAccE { var_base = var_base_a; member = member_a },
      TypeAccE { var_base = var_base_b; member = member_b } ) ->
      eq_var var_base_a var_base_b && eq_member member_a member_b
  | ErrAccE { member = member_a }, ErrAccE { member = member_b } ->
      eq_member member_a member_b
  | ( ExprAccE { expr_base = expr_base_a; member = member_a },
      ExprAccE { expr_base = expr_base_b; member = member_b } ) ->
      eq_expr expr_base_a expr_base_b && eq_member member_a member_b
  | ( CallFuncE { var_func = var_func_a; targs = targs_a; args = args_a },
      CallFuncE { var_func = var_func_b; targs = targs_b; args = args_b } ) ->
      eq_var var_func_a var_func_b
      && eq_targs targs_a targs_b && eq_args args_a args_b
  | ( CallMethodE
        {
          expr_base = expr_base_a;
          member = member_a;
          targs = targs_a;
          args = args_a;
        },
      CallMethodE
        {
          expr_base = expr_base_b;
          member = member_b;
          targs = targs_b;
          args = args_b;
        } ) ->
      eq_expr expr_base_a expr_base_b
      && eq_member member_a member_b
      && eq_targs targs_a targs_b && eq_args args_a args_b
  | ( InstE { var_inst = var_inst_a; targs = targs_a; args = args_a },
      InstE { var_inst = var_inst_b; targs = targs_b; args = args_b } ) ->
      eq_var var_inst_a var_inst_b
      && eq_targs targs_a targs_b && eq_args args_a args_b
  | _ -> false

and eq_expr expr_a expr_b = eq_expr' expr_a.it expr_b.it
and eq_exprs exprs_a exprs_b = E.eq_list eq_expr exprs_a exprs_b

(* Keyset expressions *)

and eq_keyset' keyset_a keyset_b = E.eq_keyset' eq_expr keyset_a keyset_b
and eq_keyset keyset_a keyset_b = E.eq_keyset eq_expr keyset_a keyset_b
and eq_keysets keysets_a keysets_b = E.eq_keysets eq_expr keysets_a keysets_b

(* Select-cases for select *)

and eq_select_case' select_case_a select_case_b =
  E.eq_select_case' eq_expr select_case_a select_case_b

and eq_select_case select_case_a select_case_b =
  E.eq_select_case eq_expr select_case_a select_case_b

and eq_select_cases select_cases_a select_cases_b =
  E.eq_select_cases eq_expr select_cases_a select_cases_b

(* Statements *)

and eq_stmt' stmt_a stmt_b = E.eq_stmt' eq_typ eq_expr eq_decl stmt_a stmt_b
and eq_stmt stmt_a stmt_b = E.eq_stmt eq_typ eq_expr eq_decl stmt_a stmt_b
and eq_stmts stmts_a stmts_b = E.eq_stmts eq_typ eq_expr eq_decl stmts_a stmts_b

(* Blocks (sequence of statements) *)

and eq_block' block_a block_b =
  E.eq_block' eq_typ eq_expr eq_decl block_a block_b

and eq_block block_a block_b = E.eq_block eq_typ eq_expr eq_decl block_a block_b

(* Match-cases for switch *)

and eq_switch_label' switch_label_a switch_label_b =
  E.eq_switch_label' switch_label_a switch_label_b

and eq_switch_label switch_label_a switch_label_b =
  E.eq_switch_label switch_label_a switch_label_b

and eq_switch_case' switch_case_a switch_case_b =
  E.eq_switch_case' eq_typ eq_expr eq_decl switch_case_a switch_case_b

and eq_switch_case switch_case_a switch_case_b =
  E.eq_switch_case eq_typ eq_expr eq_decl switch_case_a switch_case_b

and eq_switch_cases switch_cases_a switch_cases_b =
  E.eq_switch_cases eq_typ eq_expr eq_decl switch_cases_a switch_cases_b

(* Declarations *)

and eq_decl' decl_a decl_b =
  match (decl_a, decl_b) with
  | ( ConstD { id = id_a; typ = typ_a; value = value_a; annos = annos_a },
      ConstD { id = id_b; typ = typ_b; value = value_b; annos = annos_b } ) ->
      eq_id id_a id_b && eq_typ typ_a typ_b && eq_expr value_a value_b
      && eq_annos annos_a annos_b
  | ( VarD { id = id_a; typ = typ_a; init = init_a; annos = annos_a },
      VarD { id = id_b; typ = typ_b; init = init_b; annos = annos_b } ) ->
      eq_id id_a id_b && eq_typ typ_a typ_b
      && E.eq_option eq_expr init_a init_b
      && eq_annos annos_a annos_b
  | ( InstD
        {
          id = id_a;
          var_inst = var_inst_a;
          targs = targs_a;
          args = args_a;
          init = init_a;
          annos = annos_a;
        },
      InstD
        {
          id = id_b;
          var_inst = var_inst_b;
          targs = targs_b;
          args = args_b;
          init = init_b;
          annos = annos_b;
        } ) ->
      eq_id id_a id_b
      && eq_var var_inst_a var_inst_b
      && eq_targs targs_a targs_b && eq_args args_a args_b
      && eq_decls init_a init_b && eq_annos annos_a annos_b
  | ErrD { members = members_a }, ErrD { members = members_b }
  | MatchKindD { members = members_a }, MatchKindD { members = members_b } ->
      eq_members members_a members_b
  | ( StructD { id = id_a; fields = fields_a; annos = annos_a },
      StructD { id = id_b; fields = fields_b; annos = annos_b } )
  | ( HeaderD { id = id_a; fields = fields_a; annos = annos_a },
      HeaderD { id = id_b; fields = fields_b; annos = annos_b } )
  | ( UnionD { id = id_a; fields = fields_a; annos = annos_a },
      UnionD { id = id_b; fields = fields_b; annos = annos_b } ) ->
      eq_id id_a id_b
      && E.eq_triples eq_id eq_typ eq_annos fields_a fields_b
      && eq_annos annos_a annos_b
  | ( EnumD { id = id_a; members = members_a; annos = annos_a },
      EnumD { id = id_b; members = members_b; annos = annos_b } ) ->
      eq_id id_a id_b
      && eq_members members_a members_b
      && eq_annos annos_a annos_b
  | ( SEnumD { id = id_a; typ = typ_a; fields = fields_a; annos = annos_a },
      SEnumD { id = id_b; typ = typ_b; fields = fields_b; annos = annos_b } ) ->
      eq_id id_a id_b && eq_typ typ_a typ_b
      && E.eq_pairs eq_id eq_expr fields_a fields_b
      && eq_annos annos_a annos_b
  | ( NewTypeD { id = id_a; typdef = typdef_a; annos = annos_a },
      NewTypeD { id = id_b; typdef = typdef_b; annos = annos_b } )
  | ( TypeDefD { id = id_a; typdef = typdef_a; annos = annos_a },
      TypeDefD { id = id_b; typdef = typdef_b; annos = annos_b } ) ->
      eq_id id_a id_b
      && E.eq_alt eq_typ eq_decl typdef_a typdef_b
      && eq_annos annos_a annos_b
  | ( ValueSetD { id = id_a; typ = typ_a; size = size_a; annos = annos_a },
      ValueSetD { id = id_b; typ = typ_b; size = size_b; annos = annos_b } ) ->
      eq_id id_a id_b && eq_typ typ_a typ_b && eq_expr size_a size_b
      && eq_annos annos_a annos_b
  | ( ParserTypeD
        { id = id_a; tparams = tparams_a; params = params_a; annos = annos_a },
      ParserTypeD
        { id = id_b; tparams = tparams_b; params = params_b; annos = annos_b } )
    ->
      eq_id id_a id_b
      && eq_tparams tparams_a tparams_b
      && eq_params params_a params_b
      && eq_annos annos_a annos_b
  | ( ParserD
        {
          id = id_a;
          tparams = tparams_a;
          params = params_a;
          cparams = cparams_a;
          locals = locals_a;
          states = states_a;
          annos = annos_a;
        },
      ParserD
        {
          id = id_b;
          tparams = tparams_b;
          params = params_b;
          cparams = cparams_b;
          locals = locals_b;
          states = states_b;
          annos = annos_b;
        } ) ->
      eq_id id_a id_b
      && eq_tparams tparams_a tparams_b
      && eq_params params_a params_b
      && eq_cparams cparams_a cparams_b
      && eq_decls locals_a locals_b
      && eq_parser_states states_a states_b
      && eq_annos annos_a annos_b
  | ( ActionD { id = id_a; params = params_a; body = body_a; annos = annos_a },
      ActionD { id = id_b; params = params_b; body = body_b; annos = annos_b } )
    ->
      eq_id id_a id_b
      && eq_params params_a params_b
      && eq_block body_a body_b && eq_annos annos_a annos_b
  | ( TableD { id = id_a; table = table_a; annos = annos_a },
      TableD { id = id_b; table = table_b; annos = annos_b } ) ->
      eq_id id_a id_b && eq_table table_a table_b && eq_annos annos_a annos_b
  | ( ControlTypeD
        { id = id_a; tparams = tparams_a; params = params_a; annos = annos_a },
      ControlTypeD
        { id = id_b; tparams = tparams_b; params = params_b; annos = annos_b } )
    ->
      eq_id id_a id_b
      && eq_tparams tparams_a tparams_b
      && eq_params params_a params_b
      && eq_annos annos_a annos_b
  | ( ControlD
        {
          id = id_a;
          tparams = tparams_a;
          params = params_a;
          cparams = cparams_a;
          locals = locals_a;
          body = body_a;
          annos = annos_a;
        },
      ControlD
        {
          id = id_b;
          tparams = tparams_b;
          params = params_b;
          cparams = cparams_b;
          locals = locals_b;
          body = body_b;
          annos = annos_b;
        } ) ->
      eq_id id_a id_b
      && eq_tparams tparams_a tparams_b
      && eq_params params_a params_b
      && eq_cparams cparams_a cparams_b
      && eq_decls locals_a locals_b && eq_block body_a body_b
      && eq_annos annos_a annos_b
  | ( FuncD
        {
          id = id_a;
          typ_ret = typ_ret_a;
          tparams = tparams_a;
          params = params_a;
          body = body_a;
        },
      FuncD
        {
          id = id_b;
          typ_ret = typ_ret_b;
          tparams = tparams_b;
          params = params_b;
          body = body_b;
        } ) ->
      eq_id id_a id_b && eq_typ typ_ret_a typ_ret_b
      && eq_tparams tparams_a tparams_b
      && eq_params params_a params_b
      && eq_block body_a body_b
  | ( ExternFuncD
        {
          id = id_a;
          typ_ret = typ_ret_a;
          tparams = tparams_a;
          params = params_a;
          annos = annos_a;
        },
      ExternFuncD
        {
          id = id_b;
          typ_ret = typ_ret_b;
          tparams = tparams_b;
          params = params_b;
          annos = annos_b;
        } ) ->
      eq_id id_a id_b && eq_typ typ_ret_a typ_ret_b
      && eq_tparams tparams_a tparams_b
      && eq_params params_a params_b
      && eq_annos annos_a annos_b
  | ( ExternConstructorD { id = id_a; cparams = cparams_a; annos = annos_a },
      ExternConstructorD { id = id_b; cparams = cparams_b; annos = annos_b } )
    ->
      eq_id id_a id_b
      && eq_cparams cparams_a cparams_b
      && eq_annos annos_a annos_b
  | ( ExternAbstractMethodD
        {
          id = id_a;
          typ_ret = typ_ret_a;
          tparams = tparams_a;
          params = params_a;
          annos = annos_a;
        },
      ExternAbstractMethodD
        {
          id = id_b;
          typ_ret = typ_ret_b;
          tparams = tparams_b;
          params = params_b;
          annos = annos_b;
        } )
  | ( ExternMethodD
        {
          id = id_a;
          typ_ret = typ_ret_a;
          tparams = tparams_a;
          params = params_a;
          annos = annos_a;
        },
      ExternMethodD
        {
          id = id_b;
          typ_ret = typ_ret_b;
          tparams = tparams_b;
          params = params_b;
          annos = annos_b;
        } ) ->
      eq_id id_a id_b && eq_typ typ_ret_a typ_ret_b
      && eq_tparams tparams_a tparams_b
      && eq_params params_a params_b
      && eq_annos annos_a annos_b
  | ( ExternObjectD
        { id = id_a; tparams = tparams_a; mthds = mthds_a; annos = annos_a },
      ExternObjectD
        { id = id_b; tparams = tparams_b; mthds = mthds_b; annos = annos_b } )
    ->
      eq_id id_a id_b
      && eq_tparams tparams_a tparams_b
      && eq_decls mthds_a mthds_b && eq_annos annos_a annos_b
  | ( PackageTypeD
        { id = id_a; tparams = tparams_a; cparams = cparams_a; annos = annos_a },
      PackageTypeD
        { id = id_b; tparams = tparams_b; cparams = cparams_b; annos = annos_b }
    ) ->
      eq_id id_a id_b
      && eq_tparams tparams_a tparams_b
      && eq_cparams cparams_a cparams_b
      && eq_annos annos_a annos_b
  | _ -> false

and eq_decl decl_a decl_b = eq_decl' decl_a.it decl_b.it
and eq_decls decls_a decls_b = E.eq_list eq_decl decls_a decls_b

(* Parser states *)

and eq_parser_state' parser_state_a parser_state_b =
  E.eq_parser_state' eq_typ eq_expr eq_decl parser_state_a parser_state_b

and eq_parser_state parser_state_a parser_state_b =
  E.eq_parser_state eq_typ eq_expr eq_decl parser_state_a parser_state_b

and eq_parser_states parser_states_a parser_states_b =
  E.eq_parser_states eq_typ eq_expr eq_decl parser_states_a parser_states_b

(* Tables *)

and eq_table table_a table_b = E.eq_table eq_expr table_a table_b

(* Table keys *)

and eq_table_key' table_key_a table_key_b =
  E.eq_table_key' eq_expr table_key_a table_key_b

and eq_table_key table_key_a table_key_b =
  E.eq_table_key eq_expr table_key_a table_key_b

and eq_table_keys table_keys_a table_keys_b =
  E.eq_table_keys eq_expr table_keys_a table_keys_b

(* Table action references *)

and eq_table_action' table_action_a table_action_b =
  E.eq_table_action' eq_expr table_action_a table_action_b

and eq_table_action table_action_a table_action_b =
  E.eq_table_action eq_expr table_action_a table_action_b

and eq_table_actions table_actions_a table_actions_b =
  E.eq_table_actions eq_expr table_actions_a table_actions_b

(* Table entries *)

and eq_table_entry' table_entry_a table_entry_b =
  E.eq_table_entry' eq_expr table_entry_a table_entry_b

and eq_table_entry table_entry_a table_entry_b =
  E.eq_table_entry eq_expr table_entry_a table_entry_b

and eq_table_entries table_entries_a table_entries_b =
  E.eq_table_entries eq_expr table_entries_a table_entries_b

(* Table default properties *)

and eq_table_default' table_default_a table_default_b =
  E.eq_table_default' eq_expr table_default_a table_default_b

and eq_table_default table_default_a table_default_b =
  E.eq_table_default eq_expr table_default_a table_default_b

(* Table custorm properties *)

and eq_table_custom' table_custom_a table_custom_b =
  E.eq_table_custom' eq_expr table_custom_a table_custom_b

and eq_table_custom table_custom_a table_custom_b =
  E.eq_table_custom eq_expr table_custom_a table_custom_b

and eq_table_customs table_customs_a table_customs_b =
  E.eq_table_customs eq_expr table_customs_a table_customs_b

(* Program *)

let eq_program program_a program_b = E.eq_program eq_decl program_a program_b
