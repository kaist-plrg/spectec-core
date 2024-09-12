open Ast
open Util.Source

let eq_alt eq_l eq_r alt_a alt_b =
  match (alt_a, alt_b) with
  | Left a, Left b -> eq_l a b
  | Right a, Right b -> eq_r a b
  | _ -> false

let eq_list eq xs ys = List.length xs = List.length ys && List.for_all2 eq xs ys

let eq_option eq x y =
  match (x, y) with Some x, Some y -> eq x y | None, None -> true | _ -> false

let eq_pairs eq_k eq_v pairs_a pairs_b =
  List.length pairs_a = List.length pairs_b
  && List.for_all2
       (fun (k_a, v_a) (k_b, v_b) -> eq_k k_a k_b && eq_v v_a v_b)
       pairs_a pairs_b

let eq_triples eq_a eq_b eq_c triples_a triples_b =
  List.length triples_a = List.length triples_b
  && List.for_all2
       (fun (a_a, b_a, c_a) (a_b, b_b, c_b) ->
         eq_a a_a a_b && eq_b b_a b_b && eq_c c_a c_b)
       triples_a triples_b

(* Parameterized equality types *)

type 'typ eq_typ = 'typ typ -> 'typ typ -> bool
type ('note, 'expr) eq_expr = ('note, 'expr) expr -> ('note, 'expr) expr -> bool
type 'decl eq_decl = 'decl decl -> 'decl decl -> bool

(* Numbers *)

let eq_num' num_a num_b =
  match (num_a, num_b) with
  | (i_a, Some (width_a, signed_a)), (i_b, Some (width_b, signed_b)) ->
      Bigint.(i_a = i_b && width_a = width_b) && signed_a = signed_b
  | (i_a, None), (i_b, None) -> Bigint.(i_a = i_b)
  | _ -> false

let eq_num num_a num_b = eq_num' num_a.it num_b.it

(* Texts *)

let eq_text' text_a text_b = text_a = text_b
let eq_text text_a text_b = eq_text' text_a.it text_b.it
let eq_texts = eq_list eq_text

(* Identifiers *)

let eq_id' id_a id_b = id_a = id_b
let eq_id id_a id_b = eq_id' id_a.it id_b.it

(* Variables (scoped identifiers) *)

let eq_var' var_a var_b =
  match (var_a, var_b) with
  | Top id_a, Top id_b -> eq_id id_a id_b
  | Current id_a, Current id_b -> eq_id id_a id_b
  | _ -> false

let eq_var var_a var_b = eq_var' var_a.it var_b.it

(* Members *)

let eq_member' member_a member_b = member_a = member_b
let eq_member member_a member_b = eq_member' member_a.it member_b.it
let eq_members members_a members_b = eq_list eq_member members_a members_b

(* State labels *)

let eq_state_label' state_label_a state_label_b = state_label_a = state_label_b

let eq_state_label state_label_a state_label_b =
  eq_state_label' state_label_a.it state_label_b.it

(* Match kinds *)

let eq_match_kind' match_kind_a match_kind_b = match_kind_a = match_kind_b

let eq_match_kind match_kind_a match_kind_b =
  eq_match_kind' match_kind_a.it match_kind_b.it

(* Unary operators *)

let eq_unop' unop_a unop_b =
  match (unop_a, unop_b) with
  | BNotOp, BNotOp | LNotOp, LNotOp | UMinusOp, UMinusOp -> true
  | _ -> false

let eq_unop unop_a unop_b = eq_unop' unop_a.it unop_b.it

(* Binary operators *)

let eq_binop' binop_a binop_b =
  match (binop_a, binop_b) with
  | PlusOp, PlusOp
  | SPlusOp, SPlusOp
  | MinusOp, MinusOp
  | SMinusOp, SMinusOp
  | MulOp, MulOp
  | DivOp, DivOp
  | ModOp, ModOp
  | ShlOp, ShlOp
  | ShrOp, ShrOp
  | LeOp, LeOp
  | GeOp, GeOp
  | LtOp, LtOp
  | GtOp, GtOp
  | EqOp, EqOp
  | NeOp, NeOp
  | BAndOp, BAndOp
  | BXorOp, BXorOp
  | BOrOp, BOrOp
  | ConcatOp, ConcatOp
  | LAndOp, LAndOp
  | LOrOp, LOrOp ->
      true
  | _ -> false

let eq_binop binop_a binop_b = eq_binop' binop_a.it binop_b.it

(* Directions *)

let eq_dir' dir_a dir_b =
  match (dir_a, dir_b) with
  | No, No | In, In | Out, Out | InOut, InOut -> true
  | _ -> false

let eq_dir dir_a dir_b = eq_dir' dir_a.it dir_b.it

(* Types *)

(* Annotations *)

let rec eq_anno' (eq_expr : ('note, 'expr) eq_expr) anno_a anno_b =
  match (anno_a, anno_b) with
  | EmptyN text_a, EmptyN text_b -> eq_text text_a text_b
  | TextN (text_a, texts_a), TextN (text_b, texts_b) ->
      eq_text text_a text_b && eq_texts texts_a texts_b
  | ExprN (text_a, exprs_a), ExprN (text_b, exprs_b) ->
      eq_text text_a text_b && eq_list eq_expr exprs_a exprs_b
  | RecordN (text_a, fields_a), RecordN (text_b, fields_b) ->
      eq_text text_a text_b && eq_pairs eq_text eq_expr fields_a fields_b
  | _ -> false

and eq_anno (eq_expr : ('note, 'expr) eq_expr) anno_a anno_b =
  eq_anno' eq_expr anno_a.it anno_b.it

and eq_annos (eq_expr : ('note, 'expr) eq_expr) anno_a anno_b =
  eq_list (eq_anno eq_expr) anno_a anno_b

(* Type parameters *)

and eq_tparam' = eq_id'
and eq_tparam tparam_a tparam_b = eq_tparam' tparam_a.it tparam_b.it
and eq_tparams tparams_a tparams_b = eq_list eq_tparam tparams_a tparams_b

(* Parameters *)

(* Constructor parameters *)

(* Type arguments *)

and eq_targ (eq_typ : 'typ eq_typ) targ_a targ_b = eq_typ targ_a targ_b

and eq_targs (eq_typ : 'typ eq_typ) targs_a targs_b =
  eq_list (eq_targ eq_typ) targs_a targs_b

(* Arguments *)

and eq_arg' (eq_expr : ('note, 'expr) eq_expr) arg_a arg_b =
  match (arg_a, arg_b) with
  | ExprA expr_a, ExprA expr_b -> eq_expr expr_a expr_b
  | NameA (id_a, expr_a), NameA (id_b, expr_b) ->
      eq_id id_a id_b && eq_expr expr_a expr_b
  | AnyA, AnyA -> true
  | _ -> false

and eq_arg (eq_expr : ('note, 'expr) eq_expr) arg_a arg_b =
  eq_arg' eq_expr arg_a.it arg_b.it

and eq_args (eq_expr : ('note, 'expr) eq_expr) args_a args_b =
  eq_list (eq_arg eq_expr) args_a args_b

(* Expressions *)

(* Keyset expressions *)

and eq_keyset' (eq_expr : ('note, 'expr) eq_expr) keyset_a keyset_b =
  match (keyset_a, keyset_b) with
  | ExprK expr_a, ExprK expr_b -> eq_expr expr_a expr_b
  | DefaultK, DefaultK | AnyK, AnyK -> true
  | _ -> false

and eq_keyset eq_expr keyset_a keyset_b =
  eq_keyset' eq_expr keyset_a.it keyset_b.it

and eq_keysets eq_expr keysets_a keysets_b =
  eq_list (eq_keyset eq_expr) keysets_a keysets_b

(* Select-cases for select *)

and eq_select_case' (eq_expr : ('note, 'expr) eq_expr) select_case_a
    select_case_b =
  let keysets_a, state_label_a = select_case_a in
  let keysets_b, state_label_b = select_case_b in
  eq_keysets eq_expr keysets_a keysets_b
  && eq_state_label state_label_a state_label_b

and eq_select_case eq_expr select_case_a select_case_b =
  eq_select_case' eq_expr select_case_a.it select_case_b.it

and eq_select_cases eq_expr select_cases_a select_cases_b =
  eq_list (eq_select_case eq_expr) select_cases_a select_cases_b

(* Statements *)

and eq_stmt' (eq_typ : 'typ eq_typ) (eq_expr : ('note, 'expr) eq_expr)
    (eq_decl : 'decl eq_decl) stmt_a stmt_b =
  match (stmt_a, stmt_b) with
  | EmptyS, EmptyS -> true
  | ( AssignS { expr_l = expr_l_a; expr_r = expr_r_a },
      AssignS { expr_l = expr_l_b; expr_r = expr_r_b } ) ->
      eq_expr expr_l_a expr_l_b && eq_expr expr_r_a expr_r_b
  | ( SwitchS { expr_switch = expr_switch_a; cases = cases_a },
      SwitchS { expr_switch = expr_switch_b; cases = cases_b } ) ->
      eq_expr expr_switch_a expr_switch_b
      && eq_switch_cases eq_typ eq_expr eq_decl cases_a cases_b
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
      eq_expr expr_cond_a expr_cond_b
      && eq_stmt eq_typ eq_expr eq_decl stmt_then_a stmt_then_b
      && eq_stmt eq_typ eq_expr eq_decl stmt_else_a stmt_else_b
  | BlockS { block = block_a }, BlockS { block = block_b } ->
      eq_block eq_typ eq_expr eq_decl block_a block_b
  | ExitS, ExitS -> true
  | RetS { expr_ret = expr_ret_a }, RetS { expr_ret = expr_ret_b } ->
      eq_option eq_expr expr_ret_a expr_ret_b
  | ( CallFuncS { var_func = var_func_a; targs = targs_a; args = args_a },
      CallFuncS { var_func = var_func_b; targs = targs_b; args = args_b } ) ->
      eq_var var_func_a var_func_b
      && eq_targs eq_typ targs_a targs_b
      && eq_args eq_expr args_a args_b
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
      eq_expr expr_base_a expr_base_b
      && eq_member member_a member_b
      && eq_targs eq_typ targs_a targs_b
      && eq_args eq_expr args_a args_b
  | TransS { expr_label = expr_label_a }, TransS { expr_label = expr_label_b }
    ->
      eq_expr expr_label_a expr_label_b
  | DeclS { decl = decl_a }, DeclS { decl = decl_b } -> eq_decl decl_a decl_b
  | _ -> false

and eq_stmt (eq_typ : 'typ eq_typ) (eq_expr : ('note, 'expr) eq_expr)
    (eq_decl : 'decl eq_decl) stmt_a stmt_b =
  eq_stmt' eq_typ eq_expr eq_decl stmt_a.it stmt_b.it

and eq_stmts (eq_typ : 'typ eq_typ) (eq_expr : ('note, 'expr) eq_expr)
    (eq_decl : 'decl eq_decl) stmts_a stmts_b =
  eq_list (eq_stmt eq_typ eq_expr eq_decl) stmts_a stmts_b

(* Blocks (sequence of statements) *)

and eq_block' (eq_typ : 'typ eq_typ) (eq_expr : ('note, 'expr) eq_expr)
    (eq_decl : 'decl eq_decl) block_a block_b =
  let stmts_a, annos_a = block_a in
  let stmts_b, annos_b = block_b in
  eq_stmts eq_typ eq_expr eq_decl stmts_a stmts_b
  && eq_annos eq_expr annos_a annos_b

and eq_block (eq_typ : 'typ eq_typ) (eq_expr : ('note, 'expr) eq_expr)
    (eq_decl : 'decl eq_decl) block_a block_b =
  eq_block' eq_typ eq_expr eq_decl block_a.it block_b.it

(* Match-cases for switch *)

and eq_switch_label' switch_label_a switch_label_b =
  match (switch_label_a, switch_label_b) with
  | NameL text_a, NameL text_b -> eq_text text_a = eq_text text_b
  | DefaultL, DefaultL -> true
  | _ -> false

and eq_switch_label switch_label_a switch_label_b =
  eq_switch_label' switch_label_a.it switch_label_b.it

and eq_switch_case' (eq_typ : 'typ eq_typ) (eq_expr : ('note, 'expr) eq_expr)
    (eq_decl : 'decl eq_decl) switch_case_a switch_case_b =
  match (switch_case_a, switch_case_b) with
  | MatchC (switch_label_a, block_a), MatchC (switch_label_b, block_b) ->
      eq_switch_label switch_label_a switch_label_b
      && eq_block eq_typ eq_expr eq_decl block_a block_b
  | FallC switch_label_a, FallC switch_label_b ->
      eq_switch_label switch_label_a switch_label_b
  | _ -> false

and eq_switch_case (eq_typ : 'typ eq_typ) (eq_expr : ('note, 'expr) eq_expr)
    (eq_decl : 'decl eq_decl) switch_case_a switch_case_b =
  eq_switch_case' eq_typ eq_expr eq_decl switch_case_a.it switch_case_b.it

and eq_switch_cases (eq_typ : 'typ eq_typ) (eq_expr : ('note, 'expr) eq_expr)
    (eq_decl : 'decl eq_decl) switch_cases_a switch_cases_b =
  eq_list (eq_switch_case eq_typ eq_expr eq_decl) switch_cases_a switch_cases_b

(* Declarations *)

(* Parser states *)

and eq_parser_state' (eq_typ : 'typ eq_typ) (eq_expr : ('note, 'expr) eq_expr)
    (eq_decl : 'decl eq_decl) parser_state_a parser_state_b =
  let state_label_a, block_a, annos_a = parser_state_a in
  let state_label_b, block_b, annos_b = parser_state_b in
  eq_state_label state_label_a state_label_b
  && eq_block eq_typ eq_expr eq_decl block_a block_b
  && eq_annos eq_expr annos_a annos_b

and eq_parser_state (eq_typ : 'typ eq_typ) (eq_expr : ('note, 'expr) eq_expr)
    (eq_decl : 'decl eq_decl) parser_state_a parser_state_b =
  eq_parser_state' eq_typ eq_expr eq_decl parser_state_a.it parser_state_b.it

and eq_parser_states (eq_typ : 'typ eq_typ) (eq_expr : ('note, 'expr) eq_expr)
    (eq_decl : 'decl eq_decl) parser_states_a parser_states_b =
  eq_list
    (eq_parser_state eq_typ eq_expr eq_decl)
    parser_states_a parser_states_b

(* Tables *)

and eq_table (eq_expr : ('note, 'expr) eq_expr) table_a table_b =
  let ( table_keys_a,
        table_actions_a,
        table_entries_a,
        table_default_a,
        table_custom_a ) =
    table_a
  in
  let ( table_keys_b,
        table_actions_b,
        table_entries_b,
        table_default_b,
        table_custom_b ) =
    table_b
  in
  eq_table_keys eq_expr table_keys_a table_keys_b
  && eq_table_actions eq_expr table_actions_a table_actions_b
  && eq_table_entries eq_expr table_entries_a table_entries_b
  && eq_option (eq_table_default eq_expr) table_default_a table_default_b
  && eq_table_customs eq_expr table_custom_a table_custom_b

(* Table keys *)

and eq_table_key' (eq_expr : ('note, 'expr) eq_expr) table_key_a table_key_b =
  let expr_a, match_kind_a, annos_a = table_key_a in
  let expr_b, match_kind_b, annos_b = table_key_b in
  eq_expr expr_a expr_b
  && eq_match_kind match_kind_a match_kind_b
  && eq_annos eq_expr annos_a annos_b

and eq_table_key (eq_expr : ('note, 'expr) eq_expr) table_key_a table_key_b =
  eq_table_key' eq_expr table_key_a.it table_key_b.it

and eq_table_keys (eq_expr : ('note, 'expr) eq_expr) table_keys_a table_keys_b =
  eq_list (eq_table_key eq_expr) table_keys_a table_keys_b

(* Table action references *)

and eq_table_action' (eq_expr : ('note, 'expr) eq_expr) table_action_a
    table_action_b =
  let var_a, args_a, annos_a = table_action_a in
  let var_b, args_b, annos_b = table_action_b in
  eq_var var_a var_b
  && eq_args eq_expr args_a args_b
  && eq_annos eq_expr annos_a annos_b

and eq_table_action (eq_expr : ('note, 'expr) eq_expr) table_action_a
    table_action_b =
  eq_table_action' eq_expr table_action_a.it table_action_b.it

and eq_table_actions (eq_expr : ('note, 'expr) eq_expr) table_actions_a
    table_actions_b =
  eq_list (eq_table_action eq_expr) table_actions_a table_actions_b

(* Table entries *)

and eq_table_entry' (eq_expr : ('note, 'expr) eq_expr) table_entry_a
    table_entry_b =
  let keysets_a, table_action_a, annos_a = table_entry_a in
  let keysets_b, table_action_b, annos_b = table_entry_b in
  eq_keysets eq_expr keysets_a keysets_b
  && eq_table_action eq_expr table_action_a table_action_b
  && eq_annos eq_expr annos_a annos_b

and eq_table_entry (eq_expr : ('note, 'expr) eq_expr) table_entry_a
    table_entry_b =
  eq_table_entry' eq_expr table_entry_a.it table_entry_b.it

and eq_table_entries (eq_expr : ('note, 'expr) eq_expr) table_entries_a
    table_entries_b =
  eq_list (eq_table_entry eq_expr) table_entries_a table_entries_b

(* Table default properties *)

and eq_table_default' (eq_expr : ('note, 'expr) eq_expr) table_default_a
    table_default_b =
  let table_action_a, table_default_const_a = table_default_a in
  let table_action_b, table_default_const_b = table_default_b in
  eq_table_action eq_expr table_action_a table_action_b
  && table_default_const_a = table_default_const_b

and eq_table_default (eq_expr : ('note, 'expr) eq_expr) table_default_a
    table_default_b =
  eq_table_default' eq_expr table_default_a.it table_default_b.it

(* Table custom properties *)

and eq_table_custom' (eq_expr : ('note, 'expr) eq_expr) table_custom_a
    table_custom_b =
  let id_a, expr_a, table_custom_const_a, annos_a = table_custom_a in
  let id_b, expr_b, table_custom_const_b, annos_b = table_custom_b in
  eq_id id_a id_b && eq_expr expr_a expr_b
  && table_custom_const_a = table_custom_const_b
  && eq_annos eq_expr annos_a annos_b

and eq_table_custom (eq_expr : ('note, 'expr) eq_expr) table_custom_a
    table_custom_b =
  eq_table_custom' eq_expr table_custom_a.it table_custom_b.it

and eq_table_customs (eq_expr : ('note, 'expr) eq_expr) table_customs_a
    table_customs_b =
  eq_list (eq_table_custom eq_expr) table_customs_a table_customs_b

(* Program *)

let eq_program (eq_decl : 'decl eq_decl) program_a program_b =
  eq_list eq_decl program_a program_b
