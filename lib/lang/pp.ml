module F = Format
open Ast
open Util.Source

let indent level = String.make (2 * level) ' '

let pp_list pp_elem sep fmt l =
  F.fprintf fmt "%a"
    (Format.pp_print_list ~pp_sep:(fun fmt () -> F.fprintf fmt sep) pp_elem)
    l

let pp_option pp_elem fmt = function Some x -> pp_elem fmt x | None -> ()

let pp_pairs ?(trailing = false) ?(level = 0) pp_k pp_v sep fmt pairs =
  F.fprintf fmt "%a"
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> F.fprintf fmt sep)
       (fun fmt (k, v) ->
         F.fprintf fmt "%s%a = %a" (indent level) pp_k k pp_v v))
    pairs;
  if trailing then F.fprintf fmt sep

(* Parameterized printer types *)

type 'typ pp_typ = F.formatter -> 'typ typ -> unit

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
let pp_texts fmt texts = pp_list pp_text ", " fmt texts

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

let pp_member' ?(level = 0) fmt member' =
  F.fprintf fmt "%s%s" (indent level) member'

let pp_member ?(level = 0) fmt member = pp_member' ~level fmt member.it

let pp_members ?(level = 0) fmt members =
  pp_list (pp_member ~level) ",\n" fmt members

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

(* Annotations *)

let rec pp_anno' (pp_expr : ('note, 'expr) pp_expr) fmt anno' =
  match anno' with
  | EmptyN text -> F.fprintf fmt "@%a" pp_text text
  | TextN (text, texts) -> F.fprintf fmt "@%a(%a)" pp_text text pp_texts texts
  | ExprN (text, exprs) ->
      F.fprintf fmt "@%a(%a)" pp_text text
        (pp_list (pp_expr ~level:0) ", ")
        exprs
  | RecordN (text, fields) ->
      F.fprintf fmt "@%a[%a]" pp_text text
        (pp_pairs pp_member (pp_expr ~level:0) ", ")
        fields

and pp_anno (pp_expr : ('note, 'expr) pp_expr) fmt anno =
  pp_anno' pp_expr fmt anno.it

(* Type parameters *)

and pp_tparam' = pp_id'
and pp_tparam = pp_id

and pp_tparams fmt tparams =
  match tparams with
  | [] -> ()
  | _ -> F.fprintf fmt "<%a>" (pp_list pp_tparam ", ") tparams

(* Parameters *)

(* Constructor parameters *)

(* Type arguments *)

and pp_targ (pp_typ : 'typ pp_typ) fmt targ = pp_typ fmt targ

and pp_targs (pp_typ : 'typ pp_typ) fmt targs =
  match targs with
  | [] -> ()
  | _ -> F.fprintf fmt "<%a>" (pp_list (pp_targ pp_typ) ", ") targs

(* Arguments *)

and pp_arg' (pp_expr : ('note, 'expr) pp_expr) fmt arg' =
  match arg' with
  | ExprA expr -> pp_expr ~level:0 fmt expr
  | NameA (id, expr) -> F.fprintf fmt "%a = %a" pp_id id (pp_expr ~level:0) expr
  | AnyA -> F.fprintf fmt "_"

and pp_arg (pp_expr : ('note, 'expr) pp_expr) fmt arg =
  pp_arg' pp_expr fmt arg.it

and pp_args (pp_expr : ('note, 'expr) pp_expr) fmt args =
  F.fprintf fmt "(%a)" (pp_list (pp_arg pp_expr) ", ") args

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
  F.fprintf fmt "(%a)" (pp_list (pp_keyset pp_expr) ", ") keysets

(* Select-cases for select *)

and pp_select_case' ?(level = 0) (pp_expr : ('note, 'expr) pp_expr) fmt
    select_case' =
  let keysets, state_label = select_case' in
  F.fprintf fmt "%s%a: %a;" (indent level) (pp_keysets pp_expr) keysets
    pp_state_label state_label

and pp_select_case ?(level = 0) (pp_expr : ('note, 'expr) pp_expr) fmt
    select_case =
  pp_select_case' ~level pp_expr fmt select_case.it

and pp_select_cases ?(level = 0) (pp_expr : ('note, 'expr) pp_expr) fmt
    select_cases =
  pp_list (pp_select_case ~level:(level + 1) pp_expr) "\n" fmt select_cases

(* Statements *)

let rec pp_stmt' ?(level = 0) (pp_typ : 'typ pp_typ)
    (pp_expr : ('note, 'expr) pp_expr) (pp_decl : 'decl pp_decl) fmt stmt' =
  match stmt' with
  | EmptyS -> F.fprintf fmt "%s;" (indent level)
  | AssignS { expr_l; expr_r } ->
      F.fprintf fmt "%s%a = %a;" (indent level) (pp_expr ~level:0) expr_l
        (pp_expr ~level:0) expr_r
  | SwitchS { expr_switch; cases } ->
      F.fprintf fmt "%sswitch (%a) {\n%a\n%s}" (indent level) (pp_expr ~level:0)
        expr_switch
        (pp_switch_cases ~level:(level + 1) pp_typ pp_expr pp_decl)
        cases (indent level)
  | IfS { expr_cond; stmt_then; stmt_else } -> (
      match stmt_else.it with
      | EmptyS ->
          F.fprintf fmt "%sif (%a)\n%a" (indent level) (pp_expr ~level:0)
            expr_cond
            (pp_stmt ~level pp_typ pp_expr pp_decl)
            stmt_then
      | _ ->
          F.fprintf fmt "%sif (%a)\n%a\n%selse\n%a" (indent level)
            (pp_expr ~level:0) expr_cond
            (pp_stmt ~level pp_typ pp_expr pp_decl)
            stmt_then (indent level)
            (pp_stmt ~level pp_typ pp_expr pp_decl)
            stmt_else)
  | BlockS { block } -> pp_block ~level pp_typ pp_expr pp_decl fmt block
  | ExitS -> F.fprintf fmt "%sexit;" (indent level)
  | RetS { expr_ret } -> (
      match expr_ret with
      | Some expr_ret ->
          F.fprintf fmt "%sreturn %a;" (indent level) (pp_expr ~level:0)
            expr_ret
      | None -> F.fprintf fmt "%sreturn;" (indent level))
  | CallFuncS { var_func; targs; args } ->
      F.fprintf fmt "%s%a%a%a;" (indent level) pp_var var_func (pp_targs pp_typ)
        targs (pp_args pp_expr) args
  | CallMethodS { expr_base; member; targs; args } ->
      F.fprintf fmt "%s%a.%a%a%a;" (indent level) (pp_expr ~level:0) expr_base
        (pp_member ~level:0) member (pp_targs pp_typ) targs (pp_args pp_expr)
        args
  | TransS { expr_label } ->
      F.fprintf fmt "%stransition %a;" (indent level)
        (pp_expr ~level:(level + 1))
        expr_label
  | DeclS { decl } -> pp_decl ~level fmt decl

and pp_stmt ?(level = 0) (pp_typ : 'typ pp_typ)
    (pp_expr : ('note, 'expr) pp_expr) (pp_decl : 'decl pp_decl) fmt stmt =
  pp_stmt' ~level pp_typ pp_expr pp_decl fmt stmt.it

and pp_stmts ?(level = 0) (pp_typ : 'typ pp_typ)
    (pp_expr : ('note, 'expr) pp_expr) (pp_decl : 'decl pp_decl) fmt stmts =
  pp_list (pp_stmt ~level:(level + 1) pp_typ pp_expr pp_decl) "\n" fmt stmts

(* Blocks (sequence of statements) *)

and pp_block' ?(level = 0) (pp_typ : 'typ pp_typ)
    (pp_expr : ('note, 'expr) pp_expr) (pp_decl : 'decl pp_decl) fmt block' =
  let stmts, _anno = block' in
  F.fprintf fmt "%s{\n%a\n%s}" (indent level)
    (pp_list (pp_stmt ~level:(level + 1) pp_typ pp_expr pp_decl) "\n")
    stmts (indent level)

and pp_block ?(level = 0) (pp_typ : 'typ pp_typ)
    (pp_expr : ('note, 'expr) pp_expr) (pp_decl : 'decl pp_decl) fmt block =
  pp_block' ~level pp_typ pp_expr pp_decl fmt block.it

(* Match-cases for switch *)

and pp_switch_label' fmt switch_label' =
  match switch_label' with
  | NameL text -> pp_text fmt text
  | DefaultL -> F.fprintf fmt "default"

and pp_switch_label fmt switch_label = pp_switch_label' fmt switch_label.it

and pp_switch_case' ?(level = 0) (pp_typ : 'typ pp_typ)
    (pp_expr : ('note, 'expr) pp_expr) (pp_decl : 'decl pp_decl) fmt
    switch_case' =
  match switch_case' with
  | MatchC (switch_label, block) ->
      F.fprintf fmt "%s%a:\n%a" (indent level) pp_switch_label switch_label
        (pp_block ~level:(level + 1) pp_typ pp_expr pp_decl)
        block
  | FallC switch_label ->
      F.fprintf fmt "%s%a;" (indent level) pp_switch_label switch_label

and pp_switch_case ?(level = 0) (pp_typ : 'typ pp_typ)
    (pp_expr : ('note, 'expr) pp_expr) (pp_decl : 'decl pp_decl) fmt switch_case
    =
  pp_switch_case' ~level pp_typ pp_expr pp_decl fmt switch_case.it

and pp_switch_cases ?(level = 0) (pp_typ : 'typ pp_typ)
    (pp_expr : ('note, 'expr) pp_expr) (pp_decl : 'decl pp_decl) fmt
    switch_cases =
  pp_list
    (pp_switch_case ~level:(level + 1) pp_typ pp_expr pp_decl)
    "\n" fmt switch_cases

(* Declarations *)

(* Parser states *)

and pp_parser_state' ?(level = 0) (pp_typ : 'typ pp_typ)
    (pp_expr : ('note, 'expr) pp_expr) (pp_decl : 'decl pp_decl) fmt
    parser_state' =
  let label, block, _annos = parser_state' in
  F.fprintf fmt "%sstate %s\n%a" (indent level) label.it
    (pp_block ~level:(level + 1) pp_typ pp_expr pp_decl)
    block

and pp_parser_state ?(level = 0) (pp_typ : 'typ pp_typ)
    (pp_expr : ('note, 'expr) pp_expr) (pp_decl : 'decl pp_decl) fmt
    parser_state =
  pp_parser_state' ~level pp_typ pp_expr pp_decl fmt parser_state.it

and pp_parser_states ?(level = 0) (pp_typ : 'typ pp_typ)
    (pp_expr : ('note, 'expr) pp_expr) (pp_decl : 'decl pp_decl) fmt
    parser_states =
  pp_list
    (pp_parser_state ~level:(level + 1) pp_typ pp_expr pp_decl)
    "\n" fmt parser_states

(* Tables *)

and pp_table ?(level = 0) (pp_expr : ('note, 'expr) pp_expr) fmt
    (table : ('note, 'expr) table) =
  match table.default with
  | Some table_default ->
      F.fprintf fmt "{\n%a\n%a\n%a\n%a\n%a\n%s}"
        (pp_table_keys ~level:(level + 1) pp_expr)
        table.keys
        (pp_table_actions ~level:(level + 1) pp_expr)
        table.actions
        (pp_table_entries ~level:(level + 1) pp_expr)
        table.entries
        (pp_table_default ~level:(level + 1) pp_expr)
        table_default
        (pp_table_customs ~level:(level + 1) pp_expr)
        table.customs (indent level)
  | None ->
      F.fprintf fmt "{\n%a\n%a\n%a\n%a\n%s}"
        (pp_table_keys ~level:(level + 1) pp_expr)
        table.keys
        (pp_table_actions ~level:(level + 1) pp_expr)
        table.actions
        (pp_table_entries ~level:(level + 1) pp_expr)
        table.entries
        (pp_table_customs ~level:(level + 1) pp_expr)
        table.customs (indent level)

(* Table keys *)

and pp_table_key' ?(level = 0) (pp_expr : ('note, 'expr) pp_expr) fmt table_key'
    =
  let expr, match_kind, _annos = table_key' in
  F.fprintf fmt "%s%a : %a;" (indent level) (pp_expr ~level:0) expr
    pp_match_kind match_kind

and pp_table_key ?(level = 0) (pp_expr : ('note, 'expr) pp_expr) fmt table_key =
  pp_table_key' ~level pp_expr fmt table_key.it

and pp_table_keys ?(level = 0) (pp_expr : ('note, 'expr) pp_expr) fmt table_keys
    =
  match table_keys with
  | [] -> ()
  | _ ->
      F.fprintf fmt "%skey = {\n%a\n%s}" (indent level)
        (pp_list (pp_table_key ~level:(level + 1) pp_expr) "\n")
        table_keys (indent level)

(* Table action references *)

and pp_table_action' ?(level = 0) (pp_expr : ('note, 'expr) pp_expr) fmt
    table_action' =
  let var, args, _annos = table_action' in
  match args with
  | [] -> F.fprintf fmt "%s%a;" (indent level) pp_var var
  | _ ->
      F.fprintf fmt "%s%a%a;" (indent level) pp_var var (pp_args pp_expr) args

and pp_table_action ?(level = 0) (pp_expr : ('note, 'expr) pp_expr) fmt
    table_action =
  pp_table_action' ~level pp_expr fmt table_action.it

and pp_table_actions ?(level = 0) (pp_expr : ('note, 'expr) pp_expr) fmt
    table_actions =
  match table_actions with
  | [] -> ()
  | _ ->
      F.fprintf fmt "%sactions = {\n%a\n%s}" (indent level)
        (pp_list (pp_table_action ~level:(level + 1) pp_expr) "\n")
        table_actions (indent level)

(* Table entries *)

and pp_table_entry' ?(level = 0) (pp_expr : ('note, 'expr) pp_expr) fmt
    table_entry' =
  let keysets, table_action, _annos = table_entry' in
  F.fprintf fmt "%s%a : %a" (indent level) (pp_keysets pp_expr) keysets
    (pp_table_action ~level:(level + 1) pp_expr)
    table_action

and pp_table_entry ?(level = 0) (pp_expr : ('note, 'expr) pp_expr) fmt
    table_entry =
  pp_table_entry' ~level pp_expr fmt table_entry.it

and pp_table_entries ?(level = 0) (pp_expr : ('note, 'expr) pp_expr) fmt
    table_entries =
  match table_entries with
  | [] -> ()
  | _ ->
      F.fprintf fmt "%sconst entries = {\n%a\n%s}" (indent level)
        (pp_list (pp_table_entry ~level:(level + 1) pp_expr) "\n")
        table_entries (indent level)

(* Table default properties *)

and pp_table_default' ?(level = 0) (pp_expr : ('note, 'expr) pp_expr) fmt
    table_default' =
  let table_action, table_default_const = table_default' in
  F.fprintf fmt "%s%sdefault_action = %a" (indent level)
    (if table_default_const then "const " else "")
    (pp_table_action ~level:(level + 1) pp_expr)
    table_action

and pp_table_default ?(level = 0) (pp_expr : ('note, 'expr) pp_expr) fmt
    table_default =
  pp_table_default' ~level pp_expr fmt table_default.it

(* Table custom properties *)

and pp_table_custom' ?(level = 0) (pp_expr : ('note, 'expr) pp_expr) fmt
    table_custom' =
  let id, expr, table_custom_const, _annos = table_custom' in
  F.fprintf fmt "%s%s %a = %a;" (indent level)
    (if table_custom_const then " const" else "")
    pp_id id (pp_expr ~level:0) expr

and pp_table_custom ?(level = 0) (pp_expr : ('note, 'expr) pp_expr) fmt
    table_custom =
  pp_table_custom' ~level pp_expr fmt table_custom.it

and pp_table_customs ?(level = 0) (pp_expr : ('note, 'expr) pp_expr) fmt
    table_customs =
  pp_list (pp_table_custom ~level:(level + 1) pp_expr) "\n" fmt table_customs

(* Program *)

let pp_program (pp_decl : 'decl pp_decl) fmt program =
  pp_list (pp_decl ~level:0) "\n" fmt program
