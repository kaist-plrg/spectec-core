open Ast
open P4util.Source

(* Syntactic equality, modulo annotations for now *)

(* Utility functions *)

let check ?(dbg = false) (category : string)
    (pp : Format.formatter -> 'a -> unit) (a : 'a) (b : 'a) (eq : bool) =
  if dbg && not eq then
    Format.printf "(eq_%s): %a does not equal %a\n" category pp a pp b;
  eq

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

type 'typ eq_typ = ?dbg:bool -> 'typ typ -> 'typ typ -> bool
type 'param eq_param = ?dbg:bool -> 'param param -> 'param param -> bool

type ('note, 'expr) eq_expr =
  ?dbg:bool -> ('note, 'expr) expr -> ('note, 'expr) expr -> bool

type 'stmt eq_stmt = ?dbg:bool -> 'stmt stmt -> 'stmt stmt -> bool
type 'decl eq_decl = ?dbg:bool -> 'decl decl -> 'decl decl -> bool

type 'table_action eq_table_action =
  ?dbg:bool -> 'table_action table_action -> 'table_action table_action -> bool

type 'table_entry eq_table_entry =
  ?dbg:bool -> 'table_entry table_entry -> 'table_entry table_entry -> bool

(* Numbers *)

let eq_num' num_a num_b =
  match (num_a, num_b) with
  | (i_a, Some (width_a, signed_a)), (i_b, Some (width_b, signed_b)) ->
      Bigint.(i_a = i_b && width_a = width_b) && signed_a = signed_b
  | (i_a, None), (i_b, None) -> Bigint.(i_a = i_b)
  | _ -> false

let eq_num ?(dbg = false) num_a num_b =
  eq_num' num_a.it num_b.it |> check ~dbg "num" Pp.pp_num num_a num_b

(* Texts *)

let eq_text' text_a text_b = text_a = text_b

let eq_text ?(dbg = false) text_a text_b =
  eq_text' text_a.it text_b.it |> check ~dbg "text" Pp.pp_text text_a text_b

let eq_texts ?(dbg = false) texts_a texts_b =
  eq_list (eq_text ~dbg) texts_a texts_b

(* Identifiers *)

let eq_id' id_a id_b = id_a = id_b

let eq_id ?(dbg = false) id_a id_b =
  eq_id' id_a.it id_b.it |> check ~dbg "id" Pp.pp_id id_a id_b

(* Variables (scoped identifiers) *)

let eq_var' ?(dbg = false) var_a var_b =
  match (var_a, var_b) with
  | Top id_a, Top id_b -> eq_id ~dbg id_a id_b
  | Current id_a, Current id_b -> eq_id ~dbg id_a id_b
  | _ -> false

let eq_var ?(dbg = false) var_a var_b =
  eq_var' ~dbg var_a.it var_b.it |> check ~dbg "var" Pp.pp_var var_a var_b

(* Members *)

let eq_member' member_a member_b = member_a = member_b

let eq_member ?(dbg = false) member_a member_b =
  eq_member' member_a.it member_b.it
  |> check ~dbg "member" Pp.pp_member member_a member_b

let eq_members ?(dbg = false) members_a members_b =
  eq_list (eq_member ~dbg) members_a members_b

(* State labels *)

let eq_state_label' state_label_a state_label_b = state_label_a = state_label_b

let eq_state_label ?(dbg = false) state_label_a state_label_b =
  eq_state_label' state_label_a.it state_label_b.it
  |> check ~dbg "state_label" Pp.pp_state_label state_label_a state_label_b

(* Match kinds *)

let eq_match_kind' match_kind_a match_kind_b = match_kind_a = match_kind_b

let eq_match_kind ?(dbg = false) match_kind_a match_kind_b =
  eq_match_kind' match_kind_a.it match_kind_b.it
  |> check ~dbg "match_kind" Pp.pp_match_kind match_kind_a match_kind_b

(* Unary operators *)

let eq_unop' unop_a unop_b =
  match (unop_a, unop_b) with
  | BNotOp, BNotOp | LNotOp, LNotOp | UPlusOp, UPlusOp | UMinusOp, UMinusOp ->
      true
  | _ -> false

let eq_unop ?(dbg = false) unop_a unop_b =
  eq_unop' unop_a.it unop_b.it |> check ~dbg "unop" Pp.pp_unop unop_a unop_b

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

let eq_binop ?(dbg = false) binop_a binop_b =
  eq_binop' binop_a.it binop_b.it
  |> check ~dbg "binop" Pp.pp_binop binop_a binop_b

(* Directions *)

let eq_dir' dir_a dir_b =
  match (dir_a, dir_b) with
  | No, No | In, In | Out, Out | InOut, InOut -> true
  | _ -> false

let eq_dir ?(dbg = false) dir_a dir_b =
  eq_dir' dir_a.it dir_b.it |> check ~dbg "dir" Pp.pp_dir dir_a dir_b

(* Types *)

(* Values *)

(* Annotations *)

let rec eq_anno' ?(dbg = false) (eq_expr : ('note, 'expr) eq_expr) anno_a anno_b
    =
  match (anno_a, anno_b) with
  | EmptyN text_a, EmptyN text_b -> eq_text ~dbg text_a text_b
  | TextN (text_a, texts_a), TextN (text_b, texts_b) ->
      eq_text ~dbg text_a text_b && eq_texts ~dbg texts_a texts_b
  | ExprN (text_a, exprs_a), ExprN (text_b, exprs_b) ->
      eq_text ~dbg text_a text_b && eq_list (eq_expr ~dbg) exprs_a exprs_b
  | RecordN (text_a, fields_a), RecordN (text_b, fields_b) ->
      eq_text ~dbg text_a text_b
      && eq_pairs (eq_text ~dbg) (eq_expr ~dbg) fields_a fields_b
  | _ -> false

and eq_anno ?(dbg = false) (pp_expr : ('note, 'expr) Pp.pp_expr)
    (eq_expr : ('note, 'expr) eq_expr) anno_a anno_b =
  eq_anno' ~dbg eq_expr anno_a.it anno_b.it
  |> check ~dbg "anno" (Pp.pp_anno pp_expr) anno_a anno_b

and eq_annos ?(dbg = false) (pp_expr : ('note, 'expr) Pp.pp_expr)
    (eq_expr : ('note, 'expr) eq_expr) anno_a anno_b =
  eq_list (eq_anno ~dbg pp_expr eq_expr) anno_a anno_b

(* Type parameters *)

and eq_tparam' = eq_id'

and eq_tparam ?(dbg = false) tparam_a tparam_b =
  eq_tparam' tparam_a.it tparam_b.it
  |> check ~dbg "tparam" Pp.pp_tparam tparam_a tparam_b

and eq_tparams ?(dbg = false) tparams_a tparams_b =
  eq_list (eq_tparam ~dbg) tparams_a tparams_b

(* Parameters *)

(* Constructor parameters *)

(* Type arguments *)

and eq_targ ?(dbg = false) (pp_typ : 'typ Pp.pp_typ) (eq_typ : 'typ eq_typ)
    targ_a targ_b =
  eq_typ ~dbg targ_a targ_b |> check ~dbg "targ" pp_typ targ_a targ_b

and eq_targs ?(dbg = false) (pp_typ : 'typ Pp.pp_typ) (eq_typ : 'typ eq_typ)
    targs_a targs_b =
  eq_list (eq_targ ~dbg pp_typ eq_typ) targs_a targs_b

(* Arguments *)

and eq_arg' ?(dbg = false) (eq_expr : ('note, 'expr) eq_expr) arg_a arg_b =
  match (arg_a, arg_b) with
  | ExprA expr_a, ExprA expr_b -> eq_expr ~dbg expr_a expr_b
  | NameA (id_a, expr_a), NameA (id_b, expr_b) ->
      eq_id ~dbg id_a id_b && eq_option (eq_expr ~dbg) expr_a expr_b
  | AnyA, AnyA -> true
  | _ -> false

and eq_arg ?(dbg = false) (pp_expr : ('note, 'expr) Pp.pp_expr)
    (eq_expr : ('note, 'expr) eq_expr) arg_a arg_b =
  eq_arg' ~dbg eq_expr arg_a.it arg_b.it
  |> check ~dbg "arg" (Pp.pp_arg pp_expr) arg_a arg_b

and eq_args ?(dbg = false) (pp_expr : ('note, 'expr) Pp.pp_expr)
    (eq_expr : ('note, 'expr) eq_expr) args_a args_b =
  eq_list (eq_arg ~dbg pp_expr eq_expr) args_a args_b

(* Expressions *)

(* Keyset expressions *)

and eq_keyset' ?(dbg = false) (eq_expr : ('note, 'expr) eq_expr) keyset_a
    keyset_b =
  match (keyset_a, keyset_b) with
  | ExprK expr_a, ExprK expr_b -> eq_expr ~dbg expr_a expr_b
  | DefaultK, DefaultK | AnyK, AnyK -> true
  | _ -> false

and eq_keyset ?(dbg = false) (pp_expr : ('note, 'expr) Pp.pp_expr)
    (eq_expr : ('note, 'expr) eq_expr) keyset_a keyset_b =
  eq_keyset' ~dbg eq_expr keyset_a.it keyset_b.it
  |> check ~dbg "keyset" (Pp.pp_keyset pp_expr) keyset_a keyset_b

and eq_keysets ?(dbg = false) (pp_expr : ('note, 'expr) Pp.pp_expr) eq_expr
    keysets_a keysets_b =
  eq_list (eq_keyset ~dbg pp_expr eq_expr) keysets_a keysets_b

(* Select-cases for select *)

and eq_select_case' ?(dbg = false) (pp_expr : ('note, 'expr) Pp.pp_expr)
    (eq_expr : ('note, 'expr) eq_expr) select_case_a select_case_b =
  let keysets_a, state_label_a = select_case_a in
  let keysets_b, state_label_b = select_case_b in
  eq_keysets ~dbg pp_expr eq_expr keysets_a keysets_b
  && eq_state_label ~dbg state_label_a state_label_b

and eq_select_case ?(dbg = false) (pp_expr : ('note, 'expr) Pp.pp_expr)
    (eq_expr : ('note, 'expr) eq_expr) select_case_a select_case_b =
  eq_select_case' ~dbg pp_expr eq_expr select_case_a.it select_case_b.it
  |> check ~dbg "select_case"
       (Pp.pp_select_case pp_expr)
       select_case_a select_case_b

and eq_select_cases ?(dbg = false) (pp_expr : ('note, 'expr) Pp.pp_expr)
    (eq_expr : ('note, 'expr) eq_expr) select_cases_a select_cases_b =
  eq_list (eq_select_case ~dbg pp_expr eq_expr) select_cases_a select_cases_b

(* Statements *)

(* Blocks (sequence of statements) *)

and eq_block' ?(dbg = false) (_pp_expr : ('note, 'expr) Pp.pp_expr)
    (_pp_stmt : 'stmt Pp.pp_stmt) (_eq_expr : ('note, 'expr) eq_expr)
    (eq_stmt : 'stmt eq_stmt) block_a block_b =
  let stmts_a, _annos_a = block_a in
  let stmts_b, _annos_b = block_b in
  eq_list (eq_stmt ~dbg) stmts_a stmts_b

and eq_block ?(dbg = false) (pp_expr : ('note, 'expr) Pp.pp_expr)
    (pp_stmt : 'stmt Pp.pp_stmt) (eq_expr : ('note, 'expr) eq_expr)
    (eq_stmt : 'stmt eq_stmt) block_a block_b =
  eq_block' ~dbg pp_expr pp_stmt eq_expr eq_stmt block_a.it block_b.it
  |> check ~dbg "block" (Pp.pp_block pp_expr pp_stmt) block_a block_b

(* Match-cases for switch *)

and eq_switch_label' ?(dbg = false) (eq_expr : ('note, 'expr) eq_expr)
    switch_label_a switch_label_b =
  match (switch_label_a, switch_label_b) with
  | ExprL expr_a, ExprL expr_b -> eq_expr ~dbg expr_a expr_b
  | DefaultL, DefaultL -> true
  | _ -> false

and eq_switch_label ?(dbg = false) (pp_expr : ('note, 'expr) Pp.pp_expr)
    (eq_expr : ('note, 'expr) eq_expr) switch_label_a switch_label_b =
  eq_switch_label' ~dbg eq_expr switch_label_a.it switch_label_b.it
  |> check ~dbg "switch_label"
       (Pp.pp_switch_label pp_expr)
       switch_label_a switch_label_b

and eq_switch_case' ?(dbg = false) (pp_expr : ('note, 'expr) Pp.pp_expr)
    (pp_stmt : 'stmt Pp.pp_stmt) (eq_expr : ('note, 'expr) eq_expr)
    (eq_stmt : 'stmt eq_stmt) switch_case_a switch_case_b =
  match (switch_case_a, switch_case_b) with
  | MatchC (switch_label_a, block_a), MatchC (switch_label_b, block_b) ->
      eq_switch_label ~dbg pp_expr eq_expr switch_label_a switch_label_b
      && eq_block ~dbg pp_expr pp_stmt eq_expr eq_stmt block_a block_b
  | FallC switch_label_a, FallC switch_label_b ->
      eq_switch_label ~dbg pp_expr eq_expr switch_label_a switch_label_b
  | _ -> false

and eq_switch_case ?(dbg = false) (pp_expr : ('note, 'expr) Pp.pp_expr)
    (pp_stmt : 'stmt Pp.pp_stmt) (eq_expr : ('note, 'expr) eq_expr)
    (eq_stmt : 'stmt eq_stmt) switch_case_a switch_case_b =
  eq_switch_case' ~dbg pp_expr pp_stmt eq_expr eq_stmt switch_case_a.it
    switch_case_b.it
  |> check ~dbg "switch_case"
       (Pp.pp_switch_case pp_expr pp_stmt)
       switch_case_a switch_case_b

and eq_switch_cases ?(dbg = false) (pp_expr : ('note, 'expr) Pp.pp_expr)
    (pp_stmt : 'stmt Pp.pp_stmt) (eq_expr : ('note, 'expr) eq_expr)
    (eq_stmt : 'stmt eq_stmt) switch_cases_a switch_cases_b =
  eq_list
    (eq_switch_case ~dbg pp_expr pp_stmt eq_expr eq_stmt)
    switch_cases_a switch_cases_b

(* Declarations *)

(* Parser states *)

and eq_parser_state' ?(dbg = false) (pp_expr : ('note, 'expr) Pp.pp_expr)
    (pp_stmt : 'stmt Pp.pp_stmt) (eq_expr : ('note, 'expr) eq_expr)
    (eq_stmt : 'stmt eq_stmt) parser_state_a parser_state_b =
  let state_label_a, block_a, _annos_a = parser_state_a in
  let state_label_b, block_b, _annos_b = parser_state_b in
  eq_state_label ~dbg state_label_a state_label_b
  && eq_block ~dbg pp_expr pp_stmt eq_expr eq_stmt block_a block_b

and eq_parser_state ?(dbg = false) (pp_expr : ('note, 'expr) Pp.pp_expr)
    (pp_stmt : 'stmt Pp.pp_stmt) (eq_expr : ('note, 'expr) eq_expr)
    (eq_stmt : 'stmt eq_stmt) parser_state_a parser_state_b =
  eq_parser_state' ~dbg pp_expr pp_stmt eq_expr eq_stmt parser_state_a.it
    parser_state_b.it
  |> check ~dbg "parser_state"
       (Pp.pp_parser_state pp_expr pp_stmt)
       parser_state_a parser_state_b

and eq_parser_states ?(dbg = false) (pp_expr : ('note, 'expr) Pp.pp_expr)
    (pp_stmt : 'stmt Pp.pp_stmt) (eq_expr : ('note, 'expr) eq_expr)
    (eq_stmt : 'stmt eq_stmt) parser_states_a parser_states_b =
  eq_list
    (eq_parser_state ~dbg pp_expr pp_stmt eq_expr eq_stmt)
    parser_states_a parser_states_b

(* Tables *)

and eq_table ?(dbg = false) (pp_expr : ('note, 'expr) Pp.pp_expr)
    (pp_table_action : 'table_action Pp.pp_table_action)
    (pp_table_entry : 'table_entry Pp.pp_table_entry)
    (eq_expr : ('note, 'expr) eq_expr)
    (eq_table_action : 'table_action eq_table_action)
    (eq_table_entry : 'table_entry eq_table_entry)
    (table_a : ('note, 'expr, 'table_action, 'table_entry) table)
    (table_b : ('note, 'expr, 'table_action, 'table_entry) table) =
  eq_table_properties ~dbg pp_expr pp_table_action pp_table_entry eq_expr
    eq_table_action eq_table_entry table_a table_b

(* Table properties *)

and eq_table_properties ?(dbg = false) (pp_expr : ('note, 'expr) Pp.pp_expr)
    (pp_table_action : 'table_action Pp.pp_table_action)
    (pp_table_entry : 'table_entry Pp.pp_table_entry)
    (eq_expr : ('note, 'expr) eq_expr)
    (eq_table_action : 'table_action eq_table_action)
    (eq_table_entry : 'table_entry eq_table_entry) table_properties_a
    table_properties_b =
  eq_list
    (eq_table_property ~dbg pp_expr pp_table_action pp_table_entry eq_expr
       eq_table_action eq_table_entry)
    table_properties_a table_properties_b

and eq_table_property ?(dbg = false) (pp_expr : ('note, 'expr) Pp.pp_expr)
    (pp_table_action : 'table_action Pp.pp_table_action)
    (pp_table_entry : 'table_entry Pp.pp_table_entry)
    (eq_expr : ('note, 'expr) eq_expr)
    (eq_table_action : 'table_action eq_table_action)
    (eq_table_entry : 'table_entry eq_table_entry) table_property_a
    table_property_b =
  match (table_property_a, table_property_b) with
  | KeyP table_keys_a, KeyP table_keys_b ->
      eq_table_keys ~dbg pp_expr eq_expr table_keys_a table_keys_b
  | ActionP table_actions_a, ActionP table_actions_b ->
      eq_table_actions ~dbg pp_table_action eq_table_action table_actions_a
        table_actions_b
  | EntryP table_entries_a, EntryP table_entries_b ->
      eq_table_entries ~dbg pp_table_entry eq_table_entry table_entries_a
        table_entries_b
  | DefaultP table_default_a, DefaultP table_default_b ->
      eq_table_default ~dbg pp_table_action eq_table_action table_default_a
        table_default_b
  | CustomP table_custom_a, CustomP table_custom_b ->
      eq_table_custom ~dbg pp_expr eq_expr table_custom_a table_custom_b
  | _ -> false

(* Table keys *)

and eq_table_key' ?(dbg = false) (eq_expr : ('note, 'expr) eq_expr) table_key_a
    table_key_b =
  let expr_a, match_kind_a, _annos_a = table_key_a in
  let expr_b, match_kind_b, _annos_b = table_key_b in
  eq_expr ~dbg expr_a expr_b && eq_match_kind ~dbg match_kind_a match_kind_b

and eq_table_key ?(dbg = false) (pp_expr : ('note, 'expr) Pp.pp_expr)
    (eq_expr : ('note, 'expr) eq_expr) table_key_a table_key_b =
  eq_table_key' ~dbg eq_expr table_key_a.it table_key_b.it
  |> check ~dbg "table_key" (Pp.pp_table_key pp_expr) table_key_a table_key_b

and eq_table_keys' ?(dbg = false) (pp_expr : ('note, 'expr) Pp.pp_expr)
    (eq_expr : ('note, 'expr) eq_expr) table_keys_a table_keys_b =
  eq_list (eq_table_key ~dbg pp_expr eq_expr) table_keys_a table_keys_b

and eq_table_keys ?(dbg = false) (pp_expr : ('note, 'expr) Pp.pp_expr)
    (eq_expr : ('note, 'expr) eq_expr) table_keys_a table_keys_b =
  eq_table_keys' ~dbg pp_expr eq_expr table_keys_a.it table_keys_b.it
  |> check ~dbg "table_keys" (Pp.pp_table_keys pp_expr) table_keys_a
       table_keys_b

(* Table action references *)

and eq_table_actions' ?(dbg = false)
    (_pp_table_action : 'table_action Pp.pp_table_action)
    (eq_table_action : 'table_action eq_table_action) table_actions_a
    table_actions_b =
  eq_list (eq_table_action ~dbg) table_actions_a table_actions_b

and eq_table_actions ?(dbg = false)
    (pp_table_action : 'table_action Pp.pp_table_action)
    (eq_table_action : 'table_action eq_table_action) table_actions_a
    table_actions_b =
  eq_table_actions' ~dbg pp_table_action eq_table_action table_actions_a.it
    table_actions_b.it
  |> check ~dbg "table_actions"
       (Pp.pp_table_actions pp_table_action)
       table_actions_a table_actions_b

(* Table entries *)

and eq_table_entries' ?(dbg = false)
    (_pp_table_entry : 'table_entry Pp.pp_table_entry)
    (eq_table_entry : 'table_entry eq_table_entry) table_entries_a
    table_entries_b =
  let table_entry_const_a, table_entries_a = table_entries_a in
  let table_entry_const_b, table_entries_b = table_entries_b in
  table_entry_const_a = table_entry_const_b
  && eq_list (eq_table_entry ~dbg) table_entries_a table_entries_b

and eq_table_entries ?(dbg = false)
    (pp_table_entry : 'table_entry Pp.pp_table_entry)
    (eq_table_entry : 'table_entry eq_table_entry) table_entries_a
    table_entries_b =
  eq_table_entries' ~dbg pp_table_entry eq_table_entry table_entries_a.it
    table_entries_b.it
  |> check ~dbg "table_entries"
       (Pp.pp_table_entries pp_table_entry)
       table_entries_a table_entries_b

(* Table default properties *)

and eq_table_default' ?(dbg = false)
    (_pp_table_action : 'table_action Pp.pp_table_action)
    (eq_table_action : 'table_action eq_table_action) table_default_a
    table_default_b =
  let table_default_const_a, table_action_a = table_default_a in
  let table_default_const_b, table_action_b = table_default_b in
  table_default_const_a = table_default_const_b
  && eq_table_action ~dbg table_action_a table_action_b

and eq_table_default ?(dbg = false)
    (pp_table_action : 'table_action Pp.pp_table_action)
    (eq_table_action : 'table_action eq_table_action) table_default_a
    table_default_b =
  eq_table_default' ~dbg pp_table_action eq_table_action table_default_a.it
    table_default_b.it
  |> check ~dbg "table_default"
       (Pp.pp_table_default pp_table_action)
       table_default_a table_default_b

(* Table custom properties *)

and eq_table_custom' ?(dbg = false) (eq_expr : ('note, 'expr) eq_expr)
    table_custom_a table_custom_b =
  let table_custom_const_a, id_a, expr_a, _annos_a = table_custom_a in
  let table_custom_const_b, id_b, expr_b, _annos_b = table_custom_b in
  table_custom_const_a = table_custom_const_b
  && eq_id ~dbg id_a id_b && eq_expr ~dbg expr_a expr_b

and eq_table_custom ?(dbg = false) (pp_expr : ('note, 'expr) Pp.pp_expr)
    (eq_expr : ('note, 'expr) eq_expr) table_custom_a table_custom_b =
  eq_table_custom' ~dbg eq_expr table_custom_a.it table_custom_b.it
  |> check ~dbg "table_custom"
       (Pp.pp_table_custom pp_expr)
       table_custom_a table_custom_b

(* Methods *)

(* Program *)

let eq_program ?(dbg = false) (eq_decl : 'decl eq_decl) program_a program_b =
  eq_list (eq_decl ~dbg) program_a program_b
