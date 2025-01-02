module F = Format
open Ast
open Util.Pp
open Util.Source

(* Parameterized printer types *)

type 'typ pp_typ = F.formatter -> 'typ typ -> unit
type 'param pp_param = F.formatter -> 'param param -> unit

type ('note, 'expr) pp_expr =
  ?level:int -> F.formatter -> ('note, 'expr) expr -> unit

type 'decl pp_decl = ?level:int -> F.formatter -> 'decl decl -> unit

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

let rec pp_stmt' ?(level = 0) (pp_typ : 'typ pp_typ)
    (pp_expr : ('note, 'expr) pp_expr) (pp_decl : 'decl pp_decl) fmt stmt' =
  match stmt' with
  | EmptyS -> F.fprintf fmt ";"
  | AssignS { expr_l; expr_r } ->
      F.fprintf fmt "%a = %a;"
        (pp_expr ~level:(level + 1))
        expr_l
        (pp_expr ~level:(level + 1))
        expr_r
  | SwitchS { expr_switch; cases } ->
      F.fprintf fmt "switch (%a) {\n%a\n%s}"
        (pp_expr ~level:(level + 1))
        expr_switch
        (pp_switch_cases ~level:(level + 1) pp_typ pp_expr pp_decl)
        cases (indent level)
  | IfS { expr_cond; stmt_then; stmt_else } -> (
      match stmt_else.it with
      | EmptyS ->
          F.fprintf fmt "if (%a) %a"
            (pp_expr ~level:(level + 1))
            expr_cond
            (pp_stmt ~level pp_typ pp_expr pp_decl)
            stmt_then
      | _ ->
          F.fprintf fmt "if (%a) %a\n%selse %a"
            (pp_expr ~level:(level + 1))
            expr_cond
            (pp_stmt ~level pp_typ pp_expr pp_decl)
            stmt_then (indent level)
            (pp_stmt ~level pp_typ pp_expr pp_decl)
            stmt_else)
  | BlockS { block } -> pp_block ~level pp_typ pp_expr pp_decl fmt block
  | ExitS -> F.fprintf fmt "exit;"
  | RetS { expr_ret } -> (
      match expr_ret with
      | Some expr_ret ->
          F.fprintf fmt "return %a;" (pp_expr ~level:(level + 1)) expr_ret
      | None -> F.fprintf fmt "return;")
  | CallFuncS { var_func; targs; args } ->
      F.fprintf fmt "%a%a%a;" pp_var var_func (pp_targs pp_typ) targs
        (pp_args pp_expr) args
  | CallMethodS { expr_base; member; targs; args } ->
      F.fprintf fmt "%a.%a%a%a;"
        (pp_expr ~level:(level + 1))
        expr_base pp_member member (pp_targs pp_typ) targs (pp_args pp_expr)
        args
  | CallInstS { var_inst; targs; args } ->
      F.fprintf fmt "%a%a.apply%a;" pp_var var_inst (pp_targs pp_typ) targs
        (pp_args pp_expr) args
  | TransS { expr_label } ->
      let sexpr_label =
        F.asprintf "%a" (pp_expr ~level:(level + 1)) expr_label
      in
      let trailing_semicolon =
        if String.starts_with ~prefix:"select(" sexpr_label then "" else ";"
      in
      F.fprintf fmt "transition %s%s" sexpr_label trailing_semicolon
  | DeclS { decl } -> pp_decl ~level fmt decl

and pp_stmt ?(level = 0) (pp_typ : 'typ pp_typ)
    (pp_expr : ('note, 'expr) pp_expr) (pp_decl : 'decl pp_decl) fmt stmt =
  pp_stmt' ~level pp_typ pp_expr pp_decl fmt stmt.it

and pp_stmts ?(level = 0) (pp_typ : 'typ pp_typ)
    (pp_expr : ('note, 'expr) pp_expr) (pp_decl : 'decl pp_decl) fmt stmts =
  pp_list ~level (pp_stmt ~level pp_typ pp_expr pp_decl) ~sep:Nl fmt stmts

(* Blocks (sequence of statements) *)

and pp_block' ?(level = 0) (pp_typ : 'typ pp_typ)
    (pp_expr : ('note, 'expr) pp_expr) (pp_decl : 'decl pp_decl) fmt block' =
  let stmts, _anno = block' in
  if stmts = [] then F.fprintf fmt "{}"
  else
    F.fprintf fmt "{\n%a\n%s}"
      (pp_stmts ~level:(level + 1) pp_typ pp_expr pp_decl)
      stmts (indent level)

and pp_block ?(level = 0) (pp_typ : 'typ pp_typ)
    (pp_expr : ('note, 'expr) pp_expr) (pp_decl : 'decl pp_decl) fmt block =
  pp_block' ~level pp_typ pp_expr pp_decl fmt block.it

(* Match-cases for switch *)

and pp_switch_label' (pp_expr : ('note, 'expr) pp_expr) fmt switch_label' =
  match switch_label' with
  | ExprL expr -> F.fprintf fmt "%a" (pp_expr ~level:0) expr
  | DefaultL -> F.fprintf fmt "default"

and pp_switch_label (pp_expr : ('note, 'expr) pp_expr) fmt switch_label =
  pp_switch_label' pp_expr fmt switch_label.it

and pp_switch_case' ?(level = 0) (pp_typ : 'typ pp_typ)
    (pp_expr : ('note, 'expr) pp_expr) (pp_decl : 'decl pp_decl) fmt
    switch_case' =
  match switch_case' with
  | MatchC (switch_label, block) ->
      F.fprintf fmt "%a: %a" (pp_switch_label pp_expr) switch_label
        (pp_block ~level:(level + 1) pp_typ pp_expr pp_decl)
        block
  | FallC switch_label ->
      F.fprintf fmt "%a:" (pp_switch_label pp_expr) switch_label

and pp_switch_case ?(level = 0) (pp_typ : 'typ pp_typ)
    (pp_expr : ('note, 'expr) pp_expr) (pp_decl : 'decl pp_decl) fmt switch_case
    =
  pp_switch_case' ~level pp_typ pp_expr pp_decl fmt switch_case.it

and pp_switch_cases ?(level = 0) (pp_typ : 'typ pp_typ)
    (pp_expr : ('note, 'expr) pp_expr) (pp_decl : 'decl pp_decl) fmt
    switch_cases =
  pp_list ~level
    (pp_switch_case ~level pp_typ pp_expr pp_decl)
    ~sep:Nl fmt switch_cases

(* Declarations *)

(* Parser states *)

and pp_parser_state' ?(level = 0) (pp_typ : 'typ pp_typ)
    (pp_expr : ('note, 'expr) pp_expr) (pp_decl : 'decl pp_decl) fmt
    parser_state' =
  let state_label, block, _annos = parser_state' in
  F.fprintf fmt "state %a %a" pp_state_label state_label
    (pp_block ~level:(level + 1) pp_typ pp_expr pp_decl)
    block

and pp_parser_state ?(level = 0) (pp_typ : 'typ pp_typ)
    (pp_expr : ('note, 'expr) pp_expr) (pp_decl : 'decl pp_decl) fmt
    parser_state =
  pp_parser_state' ~level pp_typ pp_expr pp_decl fmt parser_state.it

and pp_parser_states ?(level = 0) (pp_typ : 'typ pp_typ)
    (pp_expr : ('note, 'expr) pp_expr) (pp_decl : 'decl pp_decl) fmt
    parser_states =
  pp_list ~level
    (pp_parser_state ~level pp_typ pp_expr pp_decl)
    ~sep:Nl fmt parser_states

(* Tables *)

and pp_table ?(level = 0) (pp_expr : ('note, 'expr) pp_expr) fmt
    (table : ('note, 'expr) table) =
  F.fprintf fmt "{\n%a\n%s}"
    (pp_table_properties ~level:(level + 1) pp_expr)
    table (indent level)

(* Table properties *)

and pp_table_property ?(level = 0) (pp_expr : ('note, 'expr) pp_expr) fmt
    (table_property : ('note, 'expr) table_property) =
  match table_property with
  | KeyP table_keys -> pp_table_keys ~level pp_expr fmt table_keys
  | ActionP table_actions -> pp_table_actions ~level pp_expr fmt table_actions
  | EntryP table_entries -> pp_table_entries ~level pp_expr fmt table_entries
  | DefaultP table_default -> pp_table_default pp_expr fmt table_default
  | CustomP table_custom -> pp_table_custom pp_expr fmt table_custom

and pp_table_properties ?(level = 0) (pp_expr : ('note, 'expr) pp_expr) fmt
    table_properties =
  pp_list ~level (pp_table_property ~level pp_expr) ~sep:Nl fmt table_properties

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

and pp_table_action' (pp_expr : ('note, 'expr) pp_expr) fmt table_action' =
  let var, args, _annos = table_action' in
  match args with
  | [] -> F.fprintf fmt "%a;" pp_var var
  | _ -> F.fprintf fmt "%a%a;" pp_var var (pp_args pp_expr) args

and pp_table_action (pp_expr : ('note, 'expr) pp_expr) fmt table_action =
  pp_table_action' pp_expr fmt table_action.it

and pp_table_actions' ?(level = 0) (pp_expr : ('note, 'expr) pp_expr) fmt
    table_actions =
  F.fprintf fmt "actions = {\n%a\n%s}"
    (pp_list ~level:(level + 1) (pp_table_action pp_expr) ~sep:Nl)
    table_actions (indent level)

and pp_table_actions ?(level = 0) (pp_expr : ('note, 'expr) pp_expr) fmt
    table_actions =
  pp_table_actions' ~level pp_expr fmt table_actions.it

(* Table entries *)

and pp_table_entry' (pp_expr : ('note, 'expr) pp_expr) fmt table_entry' =
  let keysets, table_action, table_entry_priority, table_entry_const, _annos =
    table_entry'
  in
  F.fprintf fmt "%s%s%a%s%a : %a"
    (if table_entry_const then "const " else "")
    (if table_entry_priority |> Option.is_some then "priority = " else "")
    (pp_option pp_expr) table_entry_priority
    (if table_entry_priority |> Option.is_some then " : " else "")
    (pp_keysets pp_expr) keysets (pp_table_action pp_expr) table_action

and pp_table_entry (pp_expr : ('note, 'expr) pp_expr) fmt table_entry =
  pp_table_entry' pp_expr fmt table_entry.it

and pp_table_entries' ?(level = 0) (pp_expr : ('note, 'expr) pp_expr) fmt
    table_entries =
  let table_entries, table_entries_const = table_entries in
  F.fprintf fmt "%sentries = {\n%a\n%s}"
    (if table_entries_const then "const " else "")
    (pp_list ~level:(level + 1) (pp_table_entry pp_expr) ~sep:Nl)
    table_entries (indent level)

and pp_table_entries ?(level = 0) (pp_expr : ('note, 'expr) pp_expr) fmt
    table_entries =
  pp_table_entries' ~level pp_expr fmt table_entries.it

(* Table default properties *)

and pp_table_default' (pp_expr : ('note, 'expr) pp_expr) fmt table_default' =
  let table_action, table_default_const = table_default' in
  F.fprintf fmt "%sdefault_action = %a"
    (if table_default_const then "const " else "")
    (pp_table_action pp_expr) table_action

and pp_table_default (pp_expr : ('note, 'expr) pp_expr) fmt table_default =
  pp_table_default' pp_expr fmt table_default.it

(* Table custom properties *)

and pp_table_custom' (pp_expr : ('note, 'expr) pp_expr) fmt table_custom' =
  let id, expr, table_custom_const, _annos = table_custom' in
  F.fprintf fmt "%s%a = %a;"
    (if table_custom_const then "const " else "")
    pp_id id (pp_expr ~level:0) expr

and pp_table_custom (pp_expr : ('note, 'expr) pp_expr) fmt table_custom =
  pp_table_custom' pp_expr fmt table_custom.it

(* Methods *)

and pp_mthd' (pp_typ : 'typ pp_typ) (pp_param : 'param pp_param)
    (_pp_expr : ('note, 'expr) pp_expr) fmt mthd' =
  match mthd' with
  | ExternConsM { id; cparams; annos = _annos } ->
      F.fprintf fmt "%a%a;" pp_id id (pp_params pp_param) cparams
  | ExternAbstractM { id; typ_ret; tparams; params; annos = _annos } ->
      F.fprintf fmt "abstract %a %a%a%a;" pp_typ typ_ret pp_id id pp_tparams
        tparams (pp_params pp_param) params
  | ExternM { id; typ_ret; tparams; params; annos = _annos } ->
      F.fprintf fmt "%a %a%a%a;" pp_typ typ_ret pp_id id pp_tparams tparams
        (pp_params pp_param) params

and pp_mthd (pp_typ : 'typ pp_typ) (pp_param : 'param pp_param)
    (pp_expr : ('note, 'expr) pp_expr) fmt mthd =
  pp_mthd' pp_typ pp_param pp_expr fmt mthd.it

and pp_mthds ?(level = 0) (pp_typ : 'typ pp_typ) (pp_param : 'param pp_param)
    (pp_expr : ('note, 'expr) pp_expr) fmt mthds =
  pp_list ~level (pp_mthd pp_typ pp_param pp_expr) ~sep:Nl fmt mthds

(* Program *)

let pp_program (pp_decl : 'decl pp_decl) fmt program =
  pp_list (pp_decl ~level:0) ~sep:Nl fmt program
