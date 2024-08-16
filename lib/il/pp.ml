module P = Lang.Pp
module F = Format

(* Numbers *)

let pp_num' fmt num' = P.pp_num' fmt num'
let pp_num fmt num = P.pp_num fmt num

(* Texts *)

let pp_text' fmt text' = P.pp_text' fmt text'
let pp_text fmt text = P.pp_text fmt text
let pp_texts fmt texts = P.pp_texts fmt texts

(* Identifiers *)

let pp_id' fmt id = P.pp_id' fmt id
let pp_id fmt id = P.pp_id fmt id

(* Variables (scoped identifiers) *)

let pp_var' fmt var' = P.pp_var' fmt var'
let pp_var fmt var = P.pp_var fmt var

(* Members *)

let pp_member' ?(level = 0) fmt member' = P.pp_member' ~level fmt member'
let pp_member ?(level = 0) fmt member = P.pp_member ~level fmt member
let pp_members ?(level = 0) fmt members = P.pp_members ~level fmt members

(* State labels *)

let pp_state_label' fmt state_label' = P.pp_state_label' fmt state_label'
let pp_state_label fmt state_label = P.pp_state_label fmt state_label

(* Match kinds *)

let pp_match_kind' fmt match_kind' = P.pp_match_kind fmt match_kind'
let pp_match_kind fmt match_kind = P.pp_match_kind fmt match_kind

(* Unary operators *)

let pp_unop' fmt unop' = P.pp_unop' fmt unop'
let pp_unop fmt unop = P.pp_unop fmt unop

(* Binary operators *)

let pp_binop' fmt binop' = P.pp_binop' fmt binop'
let pp_binop fmt binop = P.pp_binop fmt binop

(* Directions *)

let pp_d fmt d = Runtime.Dir.pp fmt d
let pp_dir' fmt dir' = P.pp_dir' pp_d fmt dir'
let pp_dir fmt dir = P.pp_dir pp_d fmt dir

(* Types *)

let rec pp_t fmt t = Runtime.Types.Type.pp fmt t
and pp_typ' fmt typ' = P.pp_typ' pp_t fmt typ'
and pp_typ fmt typ = P.pp_typ pp_t fmt typ
and pp_typs fmt typs = P.pp_typs pp_t fmt typs

(* Values *)

and pp_v fmt v = Runtime.Value.pp fmt v
and pp_value' fmt value' = P.pp_value' pp_v fmt value'
and pp_value fmt value = P.pp_value pp_v fmt value

(* Annotations *)

and pp_anno' fmt anno' = P.pp_anno pp_t fmt anno'
and pp_anno fmt anno = P.pp_anno pp_t fmt anno

(* Type parameters *)

and pp_tparam' fmt tparam' = P.pp_tparam' fmt tparam'
and pp_tparam fmt tparam = P.pp_tparam fmt tparam
and pp_tparams fmt tparams = P.pp_tparams fmt tparams

(* Parameters *)

and pp_param' fmt param' = P.pp_param' pp_t pp_v pp_d fmt param'
and pp_param fmt param = P.pp_param pp_t pp_v pp_d fmt param
and pp_params fmt params = P.pp_params pp_t pp_v pp_d fmt params

(* Constructor parameters *)

and pp_cparam' fmt cparam' = P.pp_cparam' pp_t pp_v pp_d fmt cparam'
and pp_cparam fmt cparam = P.pp_cparam pp_t pp_v pp_d fmt cparam
and pp_cparams fmt cparams = P.pp_cparams pp_t pp_v pp_d fmt cparams

(* Type arguments *)

and pp_targ' fmt targ' = P.pp_targ' pp_t fmt targ'
and pp_targ fmt targ = P.pp_targ pp_t fmt targ
and pp_targs fmt targs = P.pp_targs pp_t fmt targs

(* Arguments *)

and pp_arg' fmt arg' = P.pp_arg' pp_t fmt arg'
and pp_arg fmt arg = P.pp_arg pp_t fmt arg
and pp_args fmt args = P.pp_args pp_t fmt args

(* Expressions *)

and pp_expr' ?(level = 0) fmt expr' = P.pp_expr' ~level pp_t fmt expr'
and pp_expr ?(level = 0) fmt expr = P.pp_expr ~level pp_t fmt expr
and pp_exprs ?(level = 0) fmt exprs = P.pp_exprs ~level pp_t fmt exprs

(* Keyset expressions *)

and pp_keyset' fmt keyset' = P.pp_keyset' pp_t fmt keyset'
and pp_keyset fmt keyset = P.pp_keyset pp_t fmt keyset
and pp_keysets fmt keysets = P.pp_keysets pp_t fmt keysets

(* Select-cases for select *)

and pp_select_case' ?(level = 0) fmt select_case' =
  P.pp_select_case' ~level pp_t fmt select_case'

and pp_select_case ?(level = 0) fmt select_case =
  P.pp_select_case ~level pp_t fmt select_case

and pp_select_cases ?(level = 0) fmt select_cases =
  P.pp_select_cases ~level pp_t fmt select_cases

(* Statements *)

and pp_stmt' ?(level = 0) fmt stmt' = P.pp_stmt' ~level pp_t pp_v pp_d fmt stmt'
and pp_stmt ?(level = 0) fmt stmt = P.pp_stmt ~level pp_t pp_v pp_d fmt stmt
and pp_stmts ?(level = 0) fmt stmts = P.pp_stmts ~level pp_t pp_v pp_d fmt stmts

(* Blocks (sequence of statements) *)

and pp_block' ?(level = 0) fmt block' =
  P.pp_block' ~level pp_t pp_v pp_d fmt block'

and pp_block ?(level = 0) fmt block = P.pp_block ~level pp_t pp_v pp_d fmt block

(* Match-cases for switch *)

and pp_switch_label' fmt switch_label' = P.pp_switch_label' fmt switch_label'
and pp_switch_label fmt switch_label = P.pp_switch_label fmt switch_label

and pp_switch_case' ?(level = 0) fmt switch_case' =
  P.pp_switch_case' ~level pp_t pp_v pp_d fmt switch_case'

and pp_switch_case ?(level = 0) fmt switch_case =
  P.pp_switch_case ~level pp_t pp_v pp_d fmt switch_case

and pp_switch_cases ?(level = 0) fmt switch_cases =
  P.pp_switch_cases ~level pp_t pp_v pp_d fmt switch_cases

(* Declarations *)

and pp_decl' ?(level = 0) fmt decl' = P.pp_decl' ~level pp_t pp_v pp_d fmt decl'
and pp_decl ?(level = 0) fmt decl = P.pp_decl ~level pp_t pp_v pp_d fmt decl
and pp_decls ?(level = 0) fmt decls = P.pp_decls ~level pp_t pp_v pp_d fmt decls

(* Parser states *)

and pp_parser_state' ?(level = 0) fmt parser_state' =
  P.pp_parser_state' ~level pp_t pp_v pp_d fmt parser_state'

and pp_parser_state ?(level = 0) fmt parser_state =
  P.pp_parser_state ~level pp_t pp_v pp_d fmt parser_state

and pp_parser_states ?(level = 0) fmt parser_states =
  P.pp_parser_states ~level pp_t pp_v pp_d fmt parser_states

(* Tables *)

and pp_table ?(level = 0) fmt table = P.pp_table ~level pp_t fmt table

(* Table keys *)

and pp_table_key' ?(level = 0) fmt table_key' =
  P.pp_table_key' ~level pp_t fmt table_key'

and pp_table_key ?(level = 0) fmt table_key =
  P.pp_table_key ~level pp_t fmt table_key

and pp_table_keys ?(level = 0) fmt table_keys =
  P.pp_table_keys ~level pp_t fmt table_keys

(* Table action references *)

and pp_table_action' ?(level = 0) fmt table_action' =
  P.pp_table_action' ~level pp_t fmt table_action'

and pp_table_action ?(level = 0) fmt table_action =
  P.pp_table_action ~level pp_t fmt table_action

and pp_table_actions ?(level = 0) fmt table_actions =
  P.pp_table_actions ~level pp_t fmt table_actions

(* Table entries *)

and pp_table_entry' ?(level = 0) fmt table_entry' =
  P.pp_table_entry' ~level pp_t fmt table_entry'

and pp_table_entry ?(level = 0) fmt table_entry =
  P.pp_table_entry ~level pp_t fmt table_entry

and pp_table_entries ?(level = 0) fmt table_entries =
  P.pp_table_entries ~level pp_t fmt table_entries

(* Table default properties *)

and pp_table_default' ?(level = 0) fmt table_default' =
  P.pp_table_default' ~level pp_t fmt table_default'

and pp_table_default ?(level = 0) fmt table_default =
  P.pp_table_default ~level pp_t fmt table_default

(* Table custom properties *)

and pp_table_custom' ?(level = 0) fmt table_custom' =
  P.pp_table_custom' ~level pp_t fmt table_custom'

and pp_table_custom ?(level = 0) fmt table_custom =
  P.pp_table_custom ~level pp_t fmt table_custom

and pp_table_customs ?(level = 0) fmt table_customs =
  P.pp_table_customs ~level pp_t fmt table_customs

(* Program *)

let pp_program fmt program = P.pp_program pp_t pp_v pp_d fmt program
