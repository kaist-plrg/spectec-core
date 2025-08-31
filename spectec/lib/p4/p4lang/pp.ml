module F = Format
open Ast
open P4util.Pp
open P4util.Source

(* Parameterized printer types *)

type 'typ pp_typ = F.formatter -> 'typ typ -> unit
type 'param pp_param = F.formatter -> 'param param -> unit

type ('note, 'expr) pp_expr =
  ?level:int -> F.formatter -> ('note, 'expr) expr -> unit

type 'stmt pp_stmt = ?level:int -> F.formatter -> 'stmt stmt -> unit
type 'decl pp_decl = ?level:int -> F.formatter -> 'decl decl -> unit

type 'table_action pp_table_action =
  F.formatter -> 'table_action table_action -> unit

type 'table_entry pp_table_entry =
  F.formatter -> 'table_entry table_entry -> unit

(* Numbers *)

let pp_num' fmt num' =
  match num' with
  | i, Some (width, signed) ->
      F.fprintf fmt "%s%s%s" (Bigint.to_string width)
        (if signed then "s" else "w")
        (Bigint.to_string i)
  | i, None -> F.fprintf fmt "%s" (Bigint.to_string i)

let pp_num fmt num = pp_num' fmt num.it

(* Texts *)

let pp_text' fmt text' = F.fprintf fmt "%s" text'
let pp_text fmt text = pp_text' fmt text.it
let pp_texts fmt texts = pp_list pp_text ~sep:Comma fmt texts

(* Identifiers *)

let pp_id' fmt id' = F.fprintf fmt "%s" id'
let pp_id fmt id = pp_id' fmt id.it

(* Variables (scoped identifiers) *)

let pp_var' fmt var' =
  match var' with
  | Top id -> F.fprintf fmt ".%a" pp_id id
  | Current id -> F.fprintf fmt "%a" pp_id id

let pp_var fmt var = pp_var' fmt var.it

(* Members *)

let pp_member' fmt member' = F.fprintf fmt "%s" member'
let pp_member fmt member = pp_member' fmt member.it

let pp_members ?(level = 0) fmt members =
  pp_list ~level pp_member ~sep:CommaNl fmt members

(* State labels *)

let pp_state_label' fmt state_label' = F.fprintf fmt "%s" state_label'
let pp_state_label fmt state_label = pp_state_label' fmt state_label.it

(* Match kinds *)

let pp_match_kind' fmt match_kind' = F.fprintf fmt "%s" match_kind'
let pp_match_kind fmt match_kind = pp_match_kind' fmt match_kind.it

(* Unary operators *)

let pp_unop' fmt unop' =
  match unop' with
  | BNotOp -> F.fprintf fmt "~"
  | LNotOp -> F.fprintf fmt "!"
  | UPlusOp -> F.fprintf fmt "+"
  | UMinusOp -> F.fprintf fmt "-"

let pp_unop fmt unop = pp_unop' fmt unop.it

(* Binary operators *)

let pp_binop' fmt binop' =
  match binop' with
  | PlusOp -> F.fprintf fmt "+"
  | SPlusOp -> F.fprintf fmt "|+|"
  | MinusOp -> F.fprintf fmt "-"
  | SMinusOp -> F.fprintf fmt "|-|"
  | MulOp -> F.fprintf fmt "*"
  | DivOp -> F.fprintf fmt "/"
  | ModOp -> F.fprintf fmt "%s" "%"
  | ShlOp -> F.fprintf fmt "<<"
  | ShrOp -> F.fprintf fmt ">>"
  | LeOp -> F.fprintf fmt "<="
  | GeOp -> F.fprintf fmt ">="
  | LtOp -> F.fprintf fmt "<"
  | GtOp -> F.fprintf fmt ">"
  | EqOp -> F.fprintf fmt "=="
  | NeOp -> F.fprintf fmt "!="
  | BAndOp -> F.fprintf fmt "&"
  | BXorOp -> F.fprintf fmt "^"
  | BOrOp -> F.fprintf fmt "|"
  | ConcatOp -> F.fprintf fmt "++"
  | LAndOp -> F.fprintf fmt "&&"
  | LOrOp -> F.fprintf fmt "||"

let pp_binop fmt binop = pp_binop' fmt binop.it

(* Directions *)

let pp_dir' fmt dir' =
  match dir' with
  | No -> ()
  | In -> F.pp_print_string fmt "in"
  | Out -> F.pp_print_string fmt "out"
  | InOut -> F.pp_print_string fmt "inout"

let pp_dir fmt dir = pp_dir' fmt dir.it

(* Types *)

(* Values *)

(* Annotations *)

let rec pp_anno' (pp_expr : ('note, 'expr) pp_expr) fmt anno' =
  match anno' with
  | EmptyN text -> F.fprintf fmt "@%a" pp_text text
  | TextN (text, texts) -> F.fprintf fmt "@%a(%a)" pp_text text pp_texts texts
  | ExprN (text, exprs) ->
      F.fprintf fmt "@%a(%a)" pp_text text
        (pp_list (pp_expr ~level:0) ~sep:Comma)
        exprs
  | RecordN (text, fields) ->
      F.fprintf fmt "@%a[%a]" pp_text text
        (pp_pairs pp_member (pp_expr ~level:0) ~rel:Eq ~sep:Comma)
        fields

and pp_anno (pp_expr : ('note, 'expr) pp_expr) fmt anno =
  pp_anno' pp_expr fmt anno.it

(* Type parameters *)

and pp_tparam' = pp_id'
and pp_tparam = pp_id

and pp_tparams fmt tparams =
  match tparams with
  | [] -> ()
  | _ -> F.fprintf fmt "<%a>" (pp_list pp_tparam ~sep:Comma) tparams

(* Parameters *)

and pp_params (pp_param : 'param pp_param) fmt params =
  F.fprintf fmt "(%a)" (pp_list pp_param ~sep:Comma) params

(* Constructor parameters *)

and pp_cparams (pp_param : 'param pp_param) fmt cparams =
  F.fprintf fmt "(%a)" (pp_list pp_param ~sep:Comma) cparams

(* Type arguments *)

and pp_targ (pp_typ : 'typ pp_typ) fmt targ = pp_typ fmt targ

and pp_targs (pp_typ : 'typ pp_typ) fmt targs =
  match targs with
  | [] -> ()
  | _ -> F.fprintf fmt "<%a>" (pp_list (pp_targ pp_typ) ~sep:Comma) targs

(* Arguments *)

and pp_arg' (pp_expr : ('note, 'expr) pp_expr) fmt arg' =
  match arg' with
  | ExprA expr -> pp_expr ~level:0 fmt expr
  | NameA (id, expr) -> (
      match expr with
      | Some expr -> F.fprintf fmt "%a = %a" pp_id id (pp_expr ~level:0) expr
      | None -> F.fprintf fmt "%a = _" pp_id id)
  | AnyA -> F.fprintf fmt "_"

and pp_arg (pp_expr : ('note, 'expr) pp_expr) fmt arg =
  pp_arg' pp_expr fmt arg.it

and pp_args (pp_expr : ('note, 'expr) pp_expr) fmt args =
  F.fprintf fmt "(%a)" (pp_list (pp_arg pp_expr) ~sep:Comma) args

(* Expressions *)

(* Keyset expressions *)

and pp_keyset' (pp_expr : ('note, 'expr) pp_expr) fmt keyset' =
  match keyset' with
  | ExprK expr -> pp_expr ~level:0 fmt expr
  | DefaultK -> F.fprintf fmt "default"
  | AnyK -> F.fprintf fmt "_"

and pp_keyset (pp_expr : ('note, 'expr) pp_expr) fmt keyset =
  pp_keyset' pp_expr fmt keyset.it

and pp_keysets (pp_expr : ('note, 'expr) pp_expr) fmt keysets =
  F.fprintf fmt "(%a)" (pp_list (pp_keyset pp_expr) ~sep:Comma) keysets

(* Select-cases for select *)

and pp_select_case' (pp_expr : ('note, 'expr) pp_expr) fmt select_case' =
  let keysets, state_label = select_case' in
  F.fprintf fmt "%a: %a;" (pp_keysets pp_expr) keysets pp_state_label
    state_label

and pp_select_case (pp_expr : ('note, 'expr) pp_expr) fmt select_case =
  pp_select_case' pp_expr fmt select_case.it

and pp_select_cases ?(level = 0) (pp_expr : ('note, 'expr) pp_expr) fmt
    select_cases =
  pp_list ~level (pp_select_case pp_expr) ~sep:Nl fmt select_cases

(* Statements *)

(* Blocks (sequence of statements) *)

and pp_block' ?(level = 0) (_pp_expr : ('note, 'expr) pp_expr)
    (pp_stmt : 'stmt pp_stmt) fmt block' =
  let stmts, _anno = block' in
  if stmts = [] then F.fprintf fmt "{}"
  else
    F.fprintf fmt "{\n%a\n%s}"
      (pp_list ~level:(level + 1) pp_stmt ~sep:Nl)
      stmts (indent level)

and pp_block ?(level = 0) (pp_expr : ('note, 'expr) pp_expr)
    (pp_stmt : 'stmt pp_stmt) fmt block =
  pp_block' ~level pp_expr pp_stmt fmt block.it

(* Match-cases for switch *)

and pp_switch_label' (pp_expr : ('note, 'expr) pp_expr) fmt switch_label' =
  match switch_label' with
  | ExprL expr -> F.fprintf fmt "%a" (pp_expr ~level:0) expr
  | DefaultL -> F.fprintf fmt "default"

and pp_switch_label (pp_expr : ('note, 'expr) pp_expr) fmt switch_label =
  pp_switch_label' pp_expr fmt switch_label.it

and pp_switch_case' ?(level = 0) (pp_expr : ('note, 'expr) pp_expr)
    (pp_stmt : 'stmt pp_stmt) fmt switch_case' =
  match switch_case' with
  | MatchC (switch_label, block) ->
      F.fprintf fmt "%a: %a" (pp_switch_label pp_expr) switch_label
        (pp_block ~level:(level + 1) pp_expr pp_stmt)
        block
  | FallC switch_label ->
      F.fprintf fmt "%a:" (pp_switch_label pp_expr) switch_label

and pp_switch_case ?(level = 0) (pp_expr : ('note, 'expr) pp_expr)
    (pp_stmt : 'stmt pp_stmt) fmt switch_case =
  pp_switch_case' ~level pp_expr pp_stmt fmt switch_case.it

and pp_switch_cases ?(level = 0) (pp_expr : ('note, 'expr) pp_expr)
    (pp_stmt : 'stmt pp_stmt) fmt switch_cases =
  pp_list ~level
    (pp_switch_case ~level pp_expr pp_stmt)
    ~sep:Nl fmt switch_cases

(* Declarations *)

(* Parser states *)

and pp_parser_state' ?(level = 0) (pp_expr : ('note, 'expr) pp_expr)
    (pp_stmt : 'stmt pp_stmt) fmt parser_state' =
  let state_label, block, _annos = parser_state' in
  F.fprintf fmt "state %a %a" pp_state_label state_label
    (pp_block ~level:(level + 1) pp_expr pp_stmt)
    block

and pp_parser_state ?(level = 0) (pp_expr : ('note, 'expr) pp_expr)
    (pp_stmt : 'stmt pp_stmt) fmt parser_state =
  pp_parser_state' ~level pp_expr pp_stmt fmt parser_state.it

and pp_parser_states ?(level = 0) (pp_expr : ('note, 'expr) pp_expr)
    (pp_stmt : 'stmt pp_stmt) fmt parser_states =
  pp_list ~level
    (pp_parser_state ~level pp_expr pp_stmt)
    ~sep:Nl fmt parser_states

(* Tables *)

and pp_table ?(level = 0) (pp_expr : ('note, 'expr) pp_expr)
    (pp_table_action : 'table_action pp_table_action)
    (pp_table_entry : 'table_entry pp_table_entry) fmt
    (table : ('note, 'expr, 'table_action, 'table_entry) table) =
  F.fprintf fmt "{\n%a\n%s}"
    (pp_table_properties ~level:(level + 1) pp_expr pp_table_action
       pp_table_entry)
    table (indent level)

(* Table properties *)

and pp_table_property ?(level = 0) (pp_expr : ('note, 'expr) pp_expr)
    (pp_table_action : 'table_action pp_table_action)
    (pp_table_entry : 'table_entry pp_table_entry) fmt
    (table_property :
      ('note, 'expr, 'table_action, 'table_entry) table_property) =
  match table_property with
  | KeyP table_keys -> pp_table_keys ~level pp_expr fmt table_keys
  | ActionP table_actions ->
      pp_table_actions ~level pp_table_action fmt table_actions
  | EntryP table_entries ->
      pp_table_entries ~level pp_table_entry fmt table_entries
  | DefaultP table_default -> pp_table_default pp_table_action fmt table_default
  | CustomP table_custom -> pp_table_custom pp_expr fmt table_custom

and pp_table_properties ?(level = 0) (pp_expr : ('note, 'expr) pp_expr)
    (pp_table_action : 'table_action pp_table_action)
    (pp_table_entry : 'table_entry pp_table_entry) fmt table_properties =
  pp_list ~level
    (pp_table_property ~level pp_expr pp_table_action pp_table_entry)
    ~sep:Nl fmt table_properties

(* Table keys *)

and pp_table_key' (pp_expr : ('note, 'expr) pp_expr) fmt table_key' =
  let expr, match_kind, _annos = table_key' in
  F.fprintf fmt "%a : %a;" (pp_expr ~level:0) expr pp_match_kind match_kind

and pp_table_key (pp_expr : ('note, 'expr) pp_expr) fmt table_key =
  pp_table_key' pp_expr fmt table_key.it

and pp_table_keys' ?(level = 0) (pp_expr : ('note, 'expr) pp_expr) fmt
    table_keys =
  F.fprintf fmt "key = {\n%a\n%s}"
    (pp_list ~level:(level + 1) (pp_table_key pp_expr) ~sep:Nl)
    table_keys (indent level)

and pp_table_keys ?(level = 0) (pp_expr : ('note, 'expr) pp_expr) fmt table_keys
    =
  pp_table_keys' ~level pp_expr fmt table_keys.it

(* Table action references *)

and pp_table_actions' ?(level = 0)
    (pp_table_action : 'table_action pp_table_action) fmt table_actions =
  F.fprintf fmt "actions = {\n%a\n%s}"
    (pp_list ~level:(level + 1) pp_table_action ~sep:Nl)
    table_actions (indent level)

and pp_table_actions ?(level = 0)
    (pp_table_action : 'table_action pp_table_action) fmt table_actions =
  pp_table_actions' ~level pp_table_action fmt table_actions.it

(* Table entries *)

and pp_table_entries' ?(level = 0)
    (pp_table_entry : 'table_entry pp_table_entry) fmt table_entries =
  let table_entries_const, table_entries = table_entries in
  F.fprintf fmt "%sentries = {\n%a\n%s}"
    (if table_entries_const then "const " else "")
    (pp_list ~level:(level + 1) pp_table_entry ~sep:Nl)
    table_entries (indent level)

and pp_table_entries ?(level = 0) (pp_table_entry : 'table_entry pp_table_entry)
    fmt table_entries =
  pp_table_entries' ~level pp_table_entry fmt table_entries.it

(* Table default properties *)

and pp_table_default' (pp_table_action : 'table_action pp_table_action) fmt
    table_default' =
  let table_default_const, table_action = table_default' in
  F.fprintf fmt "%sdefault_action = %a"
    (if table_default_const then "const " else "")
    pp_table_action table_action

and pp_table_default (pp_table_action : 'table_action pp_table_action) fmt
    table_default =
  pp_table_default' pp_table_action fmt table_default.it

(* Table custom properties *)

and pp_table_custom' (pp_expr : ('note, 'expr) pp_expr) fmt table_custom' =
  let table_custom_const, id, expr, _annos = table_custom' in
  F.fprintf fmt "%s%a = %a;"
    (if table_custom_const then "const " else "")
    pp_id id (pp_expr ~level:0) expr

and pp_table_custom (pp_expr : ('note, 'expr) pp_expr) fmt table_custom =
  pp_table_custom' pp_expr fmt table_custom.it

(* Methods *)

(* Program *)

let pp_program (pp_decl : 'decl pp_decl) fmt program =
  pp_list (pp_decl ~level:0) ~sep:Nl fmt program
