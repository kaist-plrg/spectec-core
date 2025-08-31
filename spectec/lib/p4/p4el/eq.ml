module E = P4lang.Eq
module P = Pp
open Ast
open P4util.Source

(* Syntactic equality, modulo annotations for now *)

(* Numbers *)

let eq_num' num_a num_b = E.eq_num' num_a num_b
let eq_num ?(dbg = false) num_a num_b = E.eq_num ~dbg num_a num_b

(* Texts *)

let eq_text' text_a text_b = E.eq_text' text_a text_b
let eq_text ?(dbg = false) text_a text_b = E.eq_text ~dbg text_a text_b
let eq_texts ?(dbg = false) texts_a texts_b = E.eq_texts ~dbg texts_a texts_b

(* Identifiers *)

let eq_id' id_a id_b = E.eq_id' id_a id_b
let eq_id ?(dbg = false) id_a id_b = E.eq_id ~dbg id_a id_b

(* Variables (scoped identifiers) *)

let eq_var' ?(dbg = false) var_a var_b = E.eq_var' ~dbg var_a var_b
let eq_var ?(dbg = false) var_a var_b = E.eq_var ~dbg var_a var_b

(* Members *)

let eq_member' member_a member_b = E.eq_member' member_a member_b

let eq_member ?(dbg = false) member_a member_b =
  E.eq_member ~dbg member_a member_b

let eq_members ?(dbg = false) members_a members_b =
  E.eq_members ~dbg members_a members_b

(* State labels *)

let eq_state_label' state_label_a state_label_b =
  E.eq_state_label' state_label_a state_label_b

let eq_state_label ?(dbg = false) state_label_a state_label_b =
  E.eq_state_label ~dbg state_label_a state_label_b

(* Match kinds *)

let eq_match_kind' match_kind_a match_kind_b =
  E.eq_match_kind' match_kind_a match_kind_b

let eq_match_kind ?(dbg = false) match_kind_a match_kind_b =
  E.eq_match_kind ~dbg match_kind_a match_kind_b

(* Unary operators *)

let eq_unop' unop_a unop_b = E.eq_unop' unop_a unop_b
let eq_unop ?(dbg = false) unop_a unop_b = E.eq_unop ~dbg unop_a unop_b

(* Binary operators *)

let eq_binop' binop_a binop_b = E.eq_binop' binop_a binop_b
let eq_binop ?(dbg = false) binop_a binop_b = E.eq_binop ~dbg binop_a binop_b

(* Directions *)

let eq_dir' dir_a dir_b = E.eq_dir' dir_a dir_b
let eq_dir ?(dbg = false) dir_a dir_b = E.eq_dir ~dbg dir_a dir_b

(* Types *)

let rec eq_typ' ?(dbg = false) typ_a typ_b =
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
      eq_expr ~dbg expr_width_a expr_width_b
  | NameT var_a, NameT var_b -> eq_var ~dbg var_a var_b
  | SpecT (var_a, targs_a), SpecT (var_b, targs_b) ->
      eq_var ~dbg var_a var_b && eq_targs ~dbg targs_a targs_b
  | StackT (typ_a, expr_size_a), StackT (typ_b, expr_size_b) ->
      eq_typ ~dbg typ_a typ_b && eq_expr ~dbg expr_size_a expr_size_b
  | ListT typ_a, ListT typ_b -> eq_typ ~dbg typ_a typ_b
  | TupleT typs_a, TupleT typs_b -> eq_typs ~dbg typs_a typs_b
  | AnyT, AnyT -> true
  | _ -> false

and eq_typ ?(dbg = false) typ_a typ_b =
  eq_typ' ~dbg typ_a.it typ_b.it |> E.check ~dbg "typ" P.pp_typ typ_a typ_b

and eq_typs ?(dbg = false) typs_a typs_b = E.eq_list (eq_typ ~dbg) typs_a typs_b

(* Values *)

(* Annotations *)

and eq_anno' ?(dbg = false) anno_a anno_b =
  E.eq_anno' ~dbg eq_expr anno_a anno_b

and eq_anno ?(dbg = false) anno_a anno_b =
  E.eq_anno ~dbg P.pp_expr eq_expr anno_a anno_b

and eq_annos ?(dbg = false) annos_a annos_b =
  E.eq_annos ~dbg P.pp_expr eq_expr annos_a annos_b

(* Type parameters *)

and eq_tparam' tparam_a tparam_b = E.eq_tparam' tparam_a tparam_b

and eq_tparam ?(dbg = false) tparam_a tparam_b =
  E.eq_tparam ~dbg tparam_a tparam_b

and eq_tparams ?(dbg = false) tparams_a tparams_b =
  E.eq_tparams ~dbg tparams_a tparams_b

(* Parameters *)

and eq_param' ?(dbg = false) param_a param_b =
  let id_a, dir_a, typ_a, expr_default_a, _annos_a = param_a in
  let id_b, dir_b, typ_b, expr_default_b, _annos_b = param_b in
  eq_id ~dbg id_a id_b && eq_dir ~dbg dir_a dir_b && eq_typ ~dbg typ_a typ_b
  && E.eq_option (eq_expr ~dbg) expr_default_a expr_default_b

and eq_param ?(dbg = false) param_a param_b =
  eq_param' ~dbg param_a.it param_b.it

and eq_params ?(dbg = false) params_a params_b =
  E.eq_list (eq_param ~dbg) params_a params_b

(* Constructor parameters *)

and eq_cparam' ?(dbg = false) cparam_a cparam_b =
  eq_param' ~dbg cparam_a cparam_b

and eq_cparam ?(dbg = false) cparam_a cparam_b =
  eq_cparam' ~dbg cparam_a.it cparam_b.it

and eq_cparams ?(dbg = false) cparams_a cparams_b =
  E.eq_list (eq_cparam ~dbg) cparams_a cparams_b

(* Type arguments *)

and eq_targ' ?(dbg = false) targ_a targ_b = eq_typ ~dbg targ_a targ_b

and eq_targ ?(dbg = false) targ_a targ_b =
  E.eq_targ ~dbg P.pp_typ eq_typ targ_a targ_b

and eq_targs ?(dbg = false) targs_a targs_b =
  E.eq_targs ~dbg P.pp_typ eq_typ targs_a targs_b

(* Arguments *)

and eq_arg' ?(dbg = false) arg_a arg_b = E.eq_arg' ~dbg eq_expr arg_a arg_b

and eq_arg ?(dbg = false) arg_a arg_b =
  E.eq_arg ~dbg P.pp_expr eq_expr arg_a arg_b

and eq_args ?(dbg = false) args_a args_b =
  E.eq_args ~dbg P.pp_expr eq_expr args_a args_b

(* Expressions *)

and eq_expr' ?(dbg = false) expr_a expr_b =
  match (expr_a, expr_b) with
  | BoolE { boolean = boolean_a }, BoolE { boolean = boolean_b } ->
      boolean_a = boolean_b
  | StrE { text = text_a }, StrE { text = text_b } -> eq_text ~dbg text_a text_b
  | NumE { num = num_a }, NumE { num = num_b } -> eq_num ~dbg num_a num_b
  | VarE { var = var_a }, VarE { var = var_b } -> eq_var ~dbg var_a var_b
  | SeqE { exprs = exprs_a }, SeqE { exprs = exprs_b }
  | SeqDefaultE { exprs = exprs_a }, SeqDefaultE { exprs = exprs_b } ->
      eq_exprs ~dbg exprs_a exprs_b
  | RecordE { fields = fields_a }, RecordE { fields = fields_b }
  | RecordDefaultE { fields = fields_a }, RecordDefaultE { fields = fields_b }
    ->
      E.eq_pairs (eq_id ~dbg) (eq_expr ~dbg) fields_a fields_b
  | DefaultE, DefaultE | InvalidE, InvalidE -> true
  | UnE { unop = unop_a; expr = expr_a }, UnE { unop = unop_b; expr = expr_b }
    ->
      eq_unop ~dbg unop_a unop_b && eq_expr ~dbg expr_a expr_b
  | ( BinE { binop = binop_a; expr_l = expr_l_a; expr_r = expr_r_a },
      BinE { binop = binop_b; expr_l = expr_l_b; expr_r = expr_r_b } ) ->
      eq_binop ~dbg binop_a binop_b
      && eq_expr ~dbg expr_l_a expr_l_b
      && eq_expr ~dbg expr_r_a expr_r_b
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
      eq_expr ~dbg expr_cond_a expr_cond_b
      && eq_expr ~dbg expr_then_a expr_then_b
      && eq_expr ~dbg expr_else_a expr_else_b
  | CastE { typ = typ_a; expr = expr_a }, CastE { typ = typ_b; expr = expr_b }
    ->
      eq_typ ~dbg typ_a typ_b && eq_expr ~dbg expr_a expr_b
  | ( MaskE { expr_base = expr_base_a; expr_mask = expr_mask_a },
      MaskE { expr_base = expr_base_b; expr_mask = expr_mask_b } ) ->
      eq_expr ~dbg expr_base_a expr_base_b
      && eq_expr ~dbg expr_mask_a expr_mask_b
  | ( RangeE { expr_lb = expr_lb_a; expr_ub = expr_ub_a },
      RangeE { expr_lb = expr_lb_b; expr_ub = expr_ub_b } ) ->
      eq_expr ~dbg expr_lb_a expr_lb_b && eq_expr ~dbg expr_ub_a expr_ub_b
  | ( SelectE { exprs_select = exprs_select_a; cases = cases_a },
      SelectE { exprs_select = exprs_select_b; cases = cases_b } ) ->
      eq_exprs ~dbg exprs_select_a exprs_select_b
      && eq_select_cases ~dbg cases_a cases_b
  | ( ArrAccE { expr_base = expr_base_a; expr_idx = expr_idx_a },
      ArrAccE { expr_base = expr_base_b; expr_idx = expr_idx_b } ) ->
      eq_expr ~dbg expr_base_a expr_base_b && eq_expr ~dbg expr_idx_a expr_idx_b
  | ( BitAccE
        { expr_base = expr_base_a; expr_lo = expr_lo_a; expr_hi = expr_hi_a },
      BitAccE
        { expr_base = expr_base_b; expr_lo = expr_lo_b; expr_hi = expr_hi_b } )
    ->
      eq_expr ~dbg expr_base_a expr_base_b
      && eq_expr ~dbg expr_lo_a expr_lo_b
      && eq_expr ~dbg expr_hi_a expr_hi_b
  | ( TypeAccE { var_base = var_base_a; member = member_a },
      TypeAccE { var_base = var_base_b; member = member_b } ) ->
      eq_var ~dbg var_base_a var_base_b && eq_member ~dbg member_a member_b
  | ErrAccE { member = member_a }, ErrAccE { member = member_b } ->
      eq_member ~dbg member_a member_b
  | ( ExprAccE { expr_base = expr_base_a; member = member_a },
      ExprAccE { expr_base = expr_base_b; member = member_b } ) ->
      eq_expr ~dbg expr_base_a expr_base_b && eq_member ~dbg member_a member_b
  | ( CallFuncE { var_func = var_func_a; targs = targs_a; args = args_a },
      CallFuncE { var_func = var_func_b; targs = targs_b; args = args_b } ) ->
      eq_var ~dbg var_func_a var_func_b
      && eq_targs ~dbg targs_a targs_b
      && eq_args ~dbg args_a args_b
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
      eq_expr ~dbg expr_base_a expr_base_b
      && eq_member ~dbg member_a member_b
      && eq_targs ~dbg targs_a targs_b
      && eq_args ~dbg args_a args_b
  | ( CallTypeE
        {
          var_typ = var_typ_a;
          member = member_a;
          targs = targs_a;
          args = args_a;
        },
      CallTypeE
        {
          var_typ = var_typ_b;
          member = member_b;
          targs = targs_b;
          args = args_b;
        } ) ->
      eq_var ~dbg var_typ_a var_typ_b
      && eq_member ~dbg member_a member_b
      && eq_targs ~dbg targs_a targs_b
      && eq_args ~dbg args_a args_b
  | ( InstE { var_inst = var_inst_a; targs = targs_a; args = args_a },
      InstE { var_inst = var_inst_b; targs = targs_b; args = args_b } ) ->
      eq_var ~dbg var_inst_a var_inst_b
      && eq_targs ~dbg targs_a targs_b
      && eq_args ~dbg args_a args_b
  | _ -> false

and eq_expr ?(dbg = false) expr_a expr_b =
  eq_expr' ~dbg expr_a.it expr_b.it
  |> E.check ~dbg "expr" P.pp_expr expr_a expr_b

and eq_exprs ?(dbg = false) exprs_a exprs_b =
  E.eq_list (eq_expr ~dbg) exprs_a exprs_b

(* Keyset expressions *)

and eq_keyset' ?(dbg = false) keyset_a keyset_b =
  E.eq_keyset' ~dbg eq_expr keyset_a keyset_b

and eq_keyset ?(dbg = false) keyset_a keyset_b =
  E.eq_keyset ~dbg P.pp_expr eq_expr keyset_a keyset_b

and eq_keysets ?(dbg = false) keysets_a keysets_b =
  E.eq_keysets ~dbg P.pp_expr eq_expr keysets_a keysets_b

(* Select-cases for select *)

and eq_select_case' ?(dbg = false) select_case_a select_case_b =
  E.eq_select_case' ~dbg P.pp_expr eq_expr select_case_a select_case_b

and eq_select_case ?(dbg = false) select_case_a select_case_b =
  E.eq_select_case ~dbg P.pp_expr eq_expr select_case_a select_case_b

and eq_select_cases ?(dbg = false) select_cases_a select_cases_b =
  E.eq_select_cases ~dbg P.pp_expr eq_expr select_cases_a select_cases_b

(* Statements *)

and eq_stmt' ?(dbg = false) stmt_a stmt_b =
  match (stmt_a, stmt_b) with
  | EmptyS, EmptyS -> true
  | ( AssignS { expr_l = expr_l_a; expr_r = expr_r_a },
      AssignS { expr_l = expr_l_b; expr_r = expr_r_b } ) ->
      eq_expr ~dbg expr_l_a expr_l_b && eq_expr ~dbg expr_r_a expr_r_b
  | ( SwitchS { expr_switch = expr_switch_a; cases = cases_a },
      SwitchS { expr_switch = expr_switch_b; cases = cases_b } ) ->
      eq_expr ~dbg expr_switch_a expr_switch_b
      && eq_switch_cases ~dbg cases_a cases_b
  | ( IfS
        {
          expr_cond = expr_cond_a;
          stmt_then = stmt_then_a;
          stmt_else = stmt_else_a;
        },
      IfS
        {
          expr_cond = expr_cond_b;
          stmt_then = stmt_then_b;
          stmt_else = stmt_else_b;
        } ) ->
      eq_expr ~dbg expr_cond_a expr_cond_b
      && eq_stmt ~dbg stmt_then_a stmt_then_b
      && eq_stmt ~dbg stmt_else_a stmt_else_b
  | BlockS { block = block_a }, BlockS { block = block_b } ->
      eq_block ~dbg block_a block_b
  | ExitS, ExitS -> true
  | RetS { expr_ret = expr_ret_a }, RetS { expr_ret = expr_ret_b } ->
      E.eq_option (eq_expr ~dbg) expr_ret_a expr_ret_b
  | ( CallFuncS { var_func = var_func_a; targs = targs_a; args = args_a },
      CallFuncS { var_func = var_func_b; targs = targs_b; args = args_b } ) ->
      eq_var ~dbg var_func_a var_func_b
      && eq_targs ~dbg targs_a targs_b
      && eq_args ~dbg args_a args_b
  | ( CallMethodS
        {
          expr_base = expr_base_a;
          member = member_a;
          targs = targs_a;
          args = args_a;
        },
      CallMethodS
        {
          expr_base = expr_base_b;
          member = member_b;
          targs = targs_b;
          args = args_b;
        } ) ->
      eq_expr ~dbg expr_base_a expr_base_b
      && eq_member ~dbg member_a member_b
      && eq_targs ~dbg targs_a targs_b
      && eq_args ~dbg args_a args_b
  | ( CallInstS { var_inst = var_inst_a; targs = targs_a; args = args_a },
      CallInstS { var_inst = var_inst_b; targs = targs_b; args = args_b } ) ->
      eq_var ~dbg var_inst_a var_inst_b
      && eq_targs ~dbg targs_a targs_b
      && eq_args ~dbg args_a args_b
  | TransS { expr_label = expr_label_a }, TransS { expr_label = expr_label_b }
    ->
      eq_expr ~dbg expr_label_a expr_label_b
  | DeclS { decl = decl_a }, DeclS { decl = decl_b } ->
      eq_decl ~dbg decl_a decl_b
  | _ -> false

and eq_stmt ?(dbg = false) stmt_a stmt_b =
  eq_stmt' ~dbg stmt_a.it stmt_b.it
  |> E.check ~dbg "stmt" Pp.pp_stmt stmt_a stmt_b

and eq_stmts ?(dbg = false) stmts_a stmts_b =
  E.eq_list (eq_stmt ~dbg) stmts_a stmts_b

(* Blocks (sequence of statements) *)

and eq_block' ?(dbg = false) block_a block_b =
  E.eq_block' ~dbg P.pp_expr P.pp_stmt eq_expr eq_stmt block_a block_b

and eq_block ?(dbg = false) block_a block_b =
  E.eq_block ~dbg P.pp_expr P.pp_stmt eq_expr eq_stmt block_a block_b

(* Match-cases for switch *)

and eq_switch_label' ?(dbg = false) switch_label_a switch_label_b =
  E.eq_switch_label' ~dbg switch_label_a switch_label_b

and eq_switch_label ?(dbg = false) switch_label_a switch_label_b =
  E.eq_switch_label ~dbg P.pp_expr switch_label_a switch_label_b

and eq_switch_case' ?(dbg = false) switch_case_a switch_case_b =
  E.eq_switch_case' ~dbg P.pp_expr P.pp_stmt eq_expr eq_stmt switch_case_a
    switch_case_b

and eq_switch_case ?(dbg = false) switch_case_a switch_case_b =
  E.eq_switch_case ~dbg P.pp_expr P.pp_stmt eq_expr eq_stmt switch_case_a
    switch_case_b

and eq_switch_cases ?(dbg = false) switch_cases_a switch_cases_b =
  E.eq_switch_cases ~dbg P.pp_expr P.pp_stmt eq_expr eq_stmt switch_cases_a
    switch_cases_b

(* Declarations *)

and eq_decl' ?(dbg = false) decl_a decl_b =
  match (decl_a, decl_b) with
  | ( ConstD { id = id_a; typ = typ_a; value = value_a; annos = _annos_a },
      ConstD { id = id_b; typ = typ_b; value = value_b; annos = _annos_b } ) ->
      eq_id ~dbg id_a id_b && eq_typ ~dbg typ_a typ_b
      && eq_expr ~dbg value_a value_b
  | ( VarD { id = id_a; typ = typ_a; init = init_a; annos = _annos_a },
      VarD { id = id_b; typ = typ_b; init = init_b; annos = _annos_b } ) ->
      eq_id ~dbg id_a id_b && eq_typ ~dbg typ_a typ_b
      && E.eq_option (eq_expr ~dbg) init_a init_b
  | ErrD { members = members_a }, ErrD { members = members_b }
  | MatchKindD { members = members_a }, MatchKindD { members = members_b } ->
      eq_members ~dbg members_a members_b
  | ( InstD
        {
          id = id_a;
          var_inst = var_inst_a;
          targs = targs_a;
          args = args_a;
          init = init_a;
          annos = _annos_a;
        },
      InstD
        {
          id = id_b;
          var_inst = var_inst_b;
          targs = targs_b;
          args = args_b;
          init = init_b;
          annos = _annos_b;
        } ) ->
      eq_id ~dbg id_a id_b
      && eq_var ~dbg var_inst_a var_inst_b
      && eq_targs ~dbg targs_a targs_b
      && eq_args ~dbg args_a args_b
      && eq_decls ~dbg init_a init_b
  | ( StructD
        { id = id_a; tparams = tparams_a; fields = fields_a; annos = _annos_a },
      StructD
        { id = id_b; tparams = tparams_b; fields = fields_b; annos = _annos_b }
    )
  | ( HeaderD
        { id = id_a; tparams = tparams_a; fields = fields_a; annos = _annos_a },
      HeaderD
        { id = id_b; tparams = tparams_b; fields = fields_b; annos = _annos_b }
    )
  | ( UnionD
        { id = id_a; tparams = tparams_a; fields = fields_a; annos = _annos_a },
      UnionD
        { id = id_b; tparams = tparams_b; fields = fields_b; annos = _annos_b }
    ) ->
      let fields_a =
        List.map (fun (member, typ, _) -> (member, typ)) fields_a
      in
      let fields_b =
        List.map (fun (member, typ, _) -> (member, typ)) fields_b
      in
      eq_id ~dbg id_a id_b
      && eq_tparams ~dbg tparams_a tparams_b
      && E.eq_pairs (eq_member ~dbg) (eq_typ ~dbg) fields_a fields_b
  | ( EnumD { id = id_a; members = members_a; annos = _annos_a },
      EnumD { id = id_b; members = members_b; annos = _annos_b } ) ->
      eq_id ~dbg id_a id_b && eq_members ~dbg members_a members_b
  | ( SEnumD { id = id_a; typ = typ_a; fields = fields_a; annos = _annos_a },
      SEnumD { id = id_b; typ = typ_b; fields = fields_b; annos = _annos_b } )
    ->
      eq_id ~dbg id_a id_b && eq_typ ~dbg typ_a typ_b
      && E.eq_pairs (eq_id ~dbg) (eq_expr ~dbg) fields_a fields_b
  | ( NewTypeD { id = id_a; typdef = typdef_a; annos = _annos_a },
      NewTypeD { id = id_b; typdef = typdef_b; annos = _annos_b } )
  | ( TypeDefD { id = id_a; typdef = typdef_a; annos = _annos_a },
      TypeDefD { id = id_b; typdef = typdef_b; annos = _annos_b } ) ->
      eq_id ~dbg id_a id_b
      && E.eq_alt (eq_typ ~dbg) (eq_decl ~dbg) typdef_a typdef_b
  | ( ValueSetD { id = id_a; typ = typ_a; size = size_a; annos = _annos_a },
      ValueSetD { id = id_b; typ = typ_b; size = size_b; annos = _annos_b } ) ->
      eq_id ~dbg id_a id_b && eq_typ ~dbg typ_a typ_b
      && eq_expr ~dbg size_a size_b
  | ( ParserTypeD
        { id = id_a; tparams = tparams_a; params = params_a; annos = _annos_a },
      ParserTypeD
        { id = id_b; tparams = tparams_b; params = params_b; annos = _annos_b }
    ) ->
      eq_id ~dbg id_a id_b
      && eq_tparams ~dbg tparams_a tparams_b
      && eq_params ~dbg params_a params_b
  | ( ParserD
        {
          id = id_a;
          tparams = tparams_a;
          params = params_a;
          cparams = cparams_a;
          locals = locals_a;
          states = states_a;
          annos = _annos_a;
        },
      ParserD
        {
          id = id_b;
          tparams = tparams_b;
          params = params_b;
          cparams = cparams_b;
          locals = locals_b;
          states = states_b;
          annos = _annos_b;
        } ) ->
      eq_id ~dbg id_a id_b
      && eq_tparams ~dbg tparams_a tparams_b
      && eq_params ~dbg params_a params_b
      && eq_cparams ~dbg cparams_a cparams_b
      && eq_decls ~dbg locals_a locals_b
      && eq_parser_states ~dbg states_a states_b
  | ( TableD { id = id_a; table = table_a; annos = _annos_a },
      TableD { id = id_b; table = table_b; annos = _annos_b } ) ->
      eq_id ~dbg id_a id_b && eq_table ~dbg table_a table_b
  | ( ControlTypeD
        { id = id_a; tparams = tparams_a; params = params_a; annos = _annos_a },
      ControlTypeD
        { id = id_b; tparams = tparams_b; params = params_b; annos = _annos_b }
    ) ->
      eq_id ~dbg id_a id_b
      && eq_tparams ~dbg tparams_a tparams_b
      && eq_params ~dbg params_a params_b
  | ( ControlD
        {
          id = id_a;
          tparams = tparams_a;
          params = params_a;
          cparams = cparams_a;
          locals = locals_a;
          body = body_a;
          annos = _annos_a;
        },
      ControlD
        {
          id = id_b;
          tparams = tparams_b;
          params = params_b;
          cparams = cparams_b;
          locals = locals_b;
          body = body_b;
          annos = _annos_b;
        } ) ->
      eq_id ~dbg id_a id_b
      && eq_tparams ~dbg tparams_a tparams_b
      && eq_params ~dbg params_a params_b
      && eq_cparams ~dbg cparams_a cparams_b
      && eq_decls ~dbg locals_a locals_b
      && eq_block ~dbg body_a body_b
  | ( ActionD { id = id_a; params = params_a; body = body_a; annos = _annos_a },
      ActionD { id = id_b; params = params_b; body = body_b; annos = _annos_b }
    ) ->
      eq_id ~dbg id_a id_b
      && eq_params ~dbg params_a params_b
      && eq_block ~dbg body_a body_b
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
      eq_id ~dbg id_a id_b
      && eq_typ ~dbg typ_ret_a typ_ret_b
      && eq_tparams ~dbg tparams_a tparams_b
      && eq_params ~dbg params_a params_b
      && eq_block ~dbg body_a body_b
  | ( ExternFuncD
        {
          id = id_a;
          typ_ret = typ_ret_a;
          tparams = tparams_a;
          params = params_a;
          annos = _annos_a;
        },
      ExternFuncD
        {
          id = id_b;
          typ_ret = typ_ret_b;
          tparams = tparams_b;
          params = params_b;
          annos = _annos_b;
        } ) ->
      eq_id ~dbg id_a id_b
      && eq_typ ~dbg typ_ret_a typ_ret_b
      && eq_tparams ~dbg tparams_a tparams_b
      && eq_params ~dbg params_a params_b
  | ( ExternObjectD
        { id = id_a; tparams = tparams_a; mthds = mthds_a; annos = _annos_a },
      ExternObjectD
        { id = id_b; tparams = tparams_b; mthds = mthds_b; annos = _annos_b } )
    ->
      eq_id ~dbg id_a id_b
      && eq_tparams ~dbg tparams_a tparams_b
      && E.eq_list (eq_mthd ~dbg) mthds_a mthds_b
  | ( PackageTypeD
        {
          id = id_a;
          tparams = tparams_a;
          cparams = cparams_a;
          annos = _annos_a;
        },
      PackageTypeD
        {
          id = id_b;
          tparams = tparams_b;
          cparams = cparams_b;
          annos = _annos_b;
        } ) ->
      eq_id ~dbg id_a id_b
      && eq_tparams ~dbg tparams_a tparams_b
      && eq_cparams ~dbg cparams_a cparams_b
  | _ -> false

and eq_decl ?(dbg = false) decl_a decl_b =
  eq_decl' ~dbg decl_a.it decl_b.it
  |> E.check ~dbg "decl" P.pp_decl decl_a decl_b

and eq_decls ?(dbg = false) decls_a decls_b =
  E.eq_list (eq_decl ~dbg) decls_a decls_b

(* Parser states *)

and eq_parser_state' ?(dbg = false) parser_state_a parser_state_b =
  E.eq_parser_state' ~dbg P.pp_expr P.pp_stmt eq_expr eq_stmt parser_state_a
    parser_state_b

and eq_parser_state ?(dbg = false) parser_state_a parser_state_b =
  E.eq_parser_state ~dbg P.pp_expr P.pp_stmt eq_expr eq_stmt parser_state_a
    parser_state_b

and eq_parser_states ?(dbg = false) parser_states_a parser_states_b =
  E.eq_parser_states ~dbg P.pp_expr P.pp_stmt eq_expr eq_stmt parser_states_a
    parser_states_b

(* Tables *)

and eq_table ?(dbg = false) table_a table_b =
  E.eq_table ~dbg P.pp_expr P.pp_table_action P.pp_table_entry eq_expr
    eq_table_action eq_table_entry table_a table_b

(* Table properties *)

and eq_table_property ?(dbg = false) table_property_a table_property_b =
  E.eq_table_property ~dbg P.pp_expr P.pp_table_action P.pp_table_entry eq_expr
    eq_table_action eq_table_entry table_property_a table_property_b

and eq_table_properties ?(dbg = false) table_properties_a table_properties_b =
  E.eq_table_properties ~dbg P.pp_expr P.pp_table_action P.pp_table_entry
    eq_expr eq_table_action eq_table_entry table_properties_a table_properties_b

(* Table keys *)

and eq_table_key' ?(dbg = false) table_key_a table_key_b =
  E.eq_table_key' ~dbg eq_expr table_key_a table_key_b

and eq_table_key ?(dbg = false) table_key_a table_key_b =
  E.eq_table_key ~dbg P.pp_expr eq_expr table_key_a table_key_b

and eq_table_keys' ?(dbg = false) table_keys_a table_keys_b =
  E.eq_table_keys' ~dbg P.pp_expr eq_expr table_keys_a table_keys_b

and eq_table_keys ?(dbg = false) table_keys_a table_keys_b =
  E.eq_table_keys ~dbg P.pp_expr eq_expr table_keys_a table_keys_b

(* Table action references *)

and eq_table_action' ?(dbg = false) table_action_a table_action_b =
  let var_a, args_a, _annos_a = table_action_a in
  let var_b, args_b, _annos_b = table_action_b in
  eq_var ~dbg var_a var_b && eq_args ~dbg args_a args_b

and eq_table_action ?(dbg = false) table_action_a table_action_b =
  eq_table_action' ~dbg table_action_a.it table_action_b.it
  |> E.check ~dbg "table_action" P.pp_table_action table_action_a table_action_b

and eq_table_actions' ?(dbg = false) table_actions_a table_actions_b =
  E.eq_table_actions' ~dbg P.pp_table_action eq_table_action table_actions_a
    table_actions_b

and eq_table_actions ?(dbg = false) table_actions_a table_actions_b =
  E.eq_table_actions ~dbg P.pp_table_action eq_table_action table_actions_a
    table_actions_b

(* Table entries *)

and eq_table_entry' ?(dbg = false) table_entry_a table_entry_b =
  let ( table_entry_const_a,
        keysets_a,
        table_action_a,
        table_entry_priority_a,
        _annos_a ) =
    table_entry_a
  in
  let ( table_entry_const_b,
        keysets_b,
        table_action_b,
        table_entry_priority_b,
        _annos_b ) =
    table_entry_b
  in
  table_entry_const_a = table_entry_const_b
  && eq_keysets ~dbg keysets_a keysets_b
  && eq_table_action ~dbg table_action_a table_action_b
  && E.eq_option eq_expr table_entry_priority_a table_entry_priority_b

and eq_table_entry ?(dbg = false) table_entry_a table_entry_b =
  eq_table_entry' ~dbg table_entry_a.it table_entry_b.it
  |> E.check ~dbg "table_entry" P.pp_table_entry table_entry_a table_entry_b

and eq_table_entries' ?(dbg = false) table_entries_a table_entries_b =
  E.eq_table_entries' ~dbg P.pp_expr eq_expr table_entries_a table_entries_b

and eq_table_entries ?(dbg = false) table_entries_a table_entries_b =
  E.eq_table_entries ~dbg P.pp_expr eq_expr table_entries_a table_entries_b

(* Table default properties *)

and eq_table_default' ?(dbg = false) table_default_a table_default_b =
  E.eq_table_default' ~dbg P.pp_table_action eq_table_action table_default_a
    table_default_b

and eq_table_default ?(dbg = false) table_default_a table_default_b =
  E.eq_table_default ~dbg P.pp_table_action eq_table_action table_default_a
    table_default_b

(* Table custorm properties *)

and eq_table_custom' ?(dbg = false) table_custom_a table_custom_b =
  E.eq_table_custom' ~dbg eq_expr table_custom_a table_custom_b

and eq_table_custom ?(dbg = false) table_custom_a table_custom_b =
  E.eq_table_custom ~dbg P.pp_expr eq_expr table_custom_a table_custom_b

(* Methods *)

and eq_mthd' ?(dbg = false) mthd_a mthd_b =
  match (mthd_a, mthd_b) with
  | ( ExternConsM { id = id_a; cparams = cparams_a; annos = _annos_a },
      ExternConsM { id = id_b; cparams = cparams_b; annos = _annos_b } ) ->
      eq_id ~dbg id_a id_b && E.eq_list (eq_cparam ~dbg) cparams_a cparams_b
  | ( ExternAbstractM
        {
          id = id_a;
          typ_ret = typ_ret_a;
          tparams = tparams_a;
          params = params_a;
          annos = _annos_a;
        },
      ExternAbstractM
        {
          id = id_b;
          typ_ret = typ_ret_b;
          tparams = tparams_b;
          params = params_b;
          annos = _annos_b;
        } )
  | ( ExternM
        {
          id = id_a;
          typ_ret = typ_ret_a;
          tparams = tparams_a;
          params = params_a;
          annos = _annos_a;
        },
      ExternM
        {
          id = id_b;
          typ_ret = typ_ret_b;
          tparams = tparams_b;
          params = params_b;
          annos = _annos_b;
        } ) ->
      eq_id ~dbg id_a id_b && eq_typ typ_ret_a typ_ret_b
      && eq_tparams ~dbg tparams_a tparams_b
      && E.eq_list (eq_param ~dbg) params_a params_b
  | _ -> false

and eq_mthd ?(dbg = false) mthd_a mthd_b =
  eq_mthd' ~dbg mthd_a.it mthd_b.it
  |> E.check ~dbg "mthd" P.pp_mthd mthd_a mthd_b

(* Program *)

let eq_program ?(dbg = false) program_a program_b =
  E.eq_program ~dbg eq_decl program_a program_b
