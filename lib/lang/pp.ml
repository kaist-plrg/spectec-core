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

let pp_dir' pp_d fmt dir' = pp_d fmt dir'
let pp_dir pp_d fmt dir = pp_dir' pp_d fmt dir.it

(* Types *)

let pp_typ' pp_t fmt typ' = pp_t fmt typ'
let pp_typ pp_t fmt typ = pp_typ' pp_t fmt typ.it
let pp_typs pp_t fmt typs = pp_list (pp_typ pp_t) ", " fmt typs

(* Values *)

let pp_value' pp_v fmt value' = pp_v fmt value'
let pp_value pp_v fmt value = pp_value' pp_v fmt value.it

(* Annotations *)

let rec pp_anno' pp_t fmt anno' =
  match anno' with
  | EmptyN text -> F.fprintf fmt "@%a" pp_text text
  | TextN (text, texts) -> F.fprintf fmt "@%a(%a)" pp_text text pp_texts texts
  | ExprN (text, exprs) ->
      F.fprintf fmt "@%a(%a)" pp_text text
        (pp_list (pp_expr ~level:0 pp_t) ", ")
        exprs
  | RecordN (text, fields) ->
      F.fprintf fmt "@%a[%a]" pp_text text
        (pp_pairs pp_member (pp_expr ~level:0 pp_t) ", ")
        fields

and pp_anno pp_t fmt anno = pp_anno' pp_t fmt anno.it

(* Type parameters *)

and pp_tparam' = pp_id'
and pp_tparam = pp_id

and pp_tparams fmt tparams =
  match tparams with
  | [] -> ()
  | _ -> F.fprintf fmt "<%a>" (pp_list pp_tparam ", ") tparams

(* Parameters *)

and pp_param' pp_t pp_v pp_d fmt param' =
  let id, dir, typ, value_default, _annos = param' in
  match value_default with
  | Some value_default ->
      F.fprintf fmt "%a %a %a = %a" (pp_dir pp_d) dir (pp_typ pp_t) typ pp_id id
        (pp_value pp_v) value_default
  | None ->
      F.fprintf fmt "%a %a %a" (pp_dir pp_d) dir (pp_typ pp_t) typ pp_id id

and pp_param pp_t pp_v pp_d fmt param = pp_param' pp_t pp_v pp_d fmt param.it

and pp_params pp_t pp_v pp_d fmt params =
  F.fprintf fmt "(%a)" (pp_list (pp_param pp_t pp_v pp_d) ", ") params

(* Constructor parameters *)

and pp_cparam' pp_t pp_v pp_d fmt cparam' = pp_param' pp_t pp_v pp_d fmt cparam'
and pp_cparam pp_t pp_v pp_d fmt cparam = pp_param pp_t pp_v pp_d fmt cparam
and pp_cparams pp_t pp_v pp_d fmt cparams = pp_params pp_t pp_v pp_d fmt cparams

(* Type arguments *)

and pp_targ' pp_t fmt targ' = pp_typ' pp_t fmt targ'
and pp_targ pp_t fmt targ = pp_typ pp_t fmt targ

and pp_targs pp_t fmt targs =
  match targs with
  | [] -> ()
  | _ -> F.fprintf fmt "<%a>" (pp_list (pp_targ pp_t) ", ") targs

(* Arguments *)

and pp_arg' pp_t fmt arg' =
  match arg' with
  | ExprA expr -> (pp_expr ~level:0 pp_t) fmt expr
  | NameA (id, expr) ->
      F.fprintf fmt "%a = %a" pp_id id (pp_expr ~level:0 pp_t) expr
  | AnyA -> F.fprintf fmt "_"

and pp_arg pp_t fmt arg = pp_arg' pp_t fmt arg.it

and pp_args pp_t fmt args =
  F.fprintf fmt "(%a)" (pp_list (pp_arg pp_t) ", ") args

(* Expressions *)

and pp_expr' ?(level = 0) pp_t fmt expr' =
  match expr' with
  | BoolE b -> F.fprintf fmt "%b" b
  | StrE t -> F.fprintf fmt "\"%a\"" pp_text t
  | NumE num -> pp_num fmt num
  | VarE var -> pp_var fmt var
  | TupleE exprs ->
      F.fprintf fmt "{ %a }" (pp_list (pp_expr ~level:0 pp_t) ", ") exprs
  | RecordE fields ->
      F.fprintf fmt "{ %a }"
        (pp_pairs pp_member (pp_expr ~level:0 pp_t) ", ")
        fields
  | UnE (unop, expr) ->
      F.fprintf fmt "%a%a" pp_unop unop (pp_expr ~level:0 pp_t) expr
  | BinE (binop, expr_l, expr_r) ->
      F.fprintf fmt "((%a) %a (%a))" (pp_expr ~level:0 pp_t) expr_l pp_binop
        binop (pp_expr ~level:0 pp_t) expr_r
  | TernE (expr_cond, expr_then, expr_else) ->
      F.fprintf fmt "((%a) ? (%a) : (%a))" (pp_expr ~level:0 pp_t) expr_cond
        (pp_expr ~level:0 pp_t) expr_then (pp_expr ~level:0 pp_t) expr_else
  | CastE (typ, expr) ->
      F.fprintf fmt "((%a) (%a))" (pp_typ pp_t) typ (pp_expr ~level:0 pp_t) expr
  | MaskE (expr_base, expr_mask) ->
      F.fprintf fmt "%a &&& %a" (pp_expr ~level:0 pp_t) expr_base
        (pp_expr ~level:0 pp_t) expr_mask
  | RangeE (expr_lb, expr_ub) ->
      F.fprintf fmt "%a .. %a" (pp_expr ~level:0 pp_t) expr_lb
        (pp_expr ~level:0 pp_t) expr_ub
  | SelectE (exprs_keyset, select_cases) ->
      F.fprintf fmt "select (%a) {\n%a\n%s}" (pp_exprs ~level:0 pp_t)
        exprs_keyset
        (pp_select_cases ~level:(level + 1) pp_t)
        select_cases (indent level)
  | ArrAccE (expr_base, expr_idx) ->
      F.fprintf fmt "%a[%a]" (pp_expr ~level:0 pp_t) expr_base
        (pp_expr ~level:0 pp_t) expr_idx
  | BitAccE (expr_base, expr_lo, expr_hi) ->
      F.fprintf fmt "%a[%a:%a]" (pp_expr ~level:0 pp_t) expr_base
        (pp_expr ~level:0 pp_t) expr_hi (pp_expr ~level:0 pp_t) expr_lo
  | TypeAccE (var, member) ->
      F.fprintf fmt "%a.%a" pp_var var (pp_member ~level:0) member
  | ErrAccE member -> F.fprintf fmt "error.%a" (pp_member ~level:0) member
  | ExprAccE (expr_base, member) ->
      F.fprintf fmt "%a.%a" (pp_expr ~level:0 pp_t) expr_base
        (pp_member ~level:0) member
  | CallE (expr_func, targs, args) ->
      F.fprintf fmt "%a%a%a" (pp_expr ~level:0 pp_t) expr_func (pp_targs pp_t)
        targs (pp_args pp_t) args
  | InstE (typ, args) ->
      F.fprintf fmt "%a%a" (pp_typ pp_t) typ (pp_args pp_t) args

and pp_expr ?(level = 0) pp_t fmt expr = pp_expr' ~level pp_t fmt expr.it

and pp_exprs ?(level = 0) pp_t fmt exprs =
  pp_list (pp_expr ~level pp_t) ", " fmt exprs

(* Keyset expressions *)

and pp_keyset' pp_t fmt keyset' =
  match keyset' with
  | ExprK expr -> (pp_expr ~level:0 pp_t) fmt expr
  | DefaultK -> F.fprintf fmt "default"
  | AnyK -> F.fprintf fmt "_"

and pp_keyset pp_t fmt keyset = pp_keyset' pp_t fmt keyset.it

and pp_keysets pp_t fmt keysets =
  F.fprintf fmt "(%a)" (pp_list (pp_keyset pp_t) ", ") keysets

(* Select-cases for select *)

and pp_select_case' ?(level = 0) pp_t fmt select_case' =
  let keysets, state_label = select_case' in
  F.fprintf fmt "%s%a: %a;" (indent level) (pp_keysets pp_t) keysets
    pp_state_label state_label

and pp_select_case ?(level = 0) pp_t fmt select_case =
  pp_select_case' ~level pp_t fmt select_case.it

and pp_select_cases ?(level = 0) pp_t fmt select_cases =
  pp_list (pp_select_case ~level:(level + 1) pp_t) "\n" fmt select_cases

(* Statements *)

let rec pp_stmt' ?(level = 0) pp_t pp_v pp_d fmt stmt' =
  match stmt' with
  | EmptyS -> F.fprintf fmt "%s;" (indent level)
  | AssignS (expr_lhs, expr_rhs) ->
      F.fprintf fmt "%s%a = %a;" (indent level) (pp_expr ~level:0 pp_t) expr_lhs
        (pp_expr ~level:0 pp_t) expr_rhs
  | SwitchS (expr_switch, switch_cases) ->
      F.fprintf fmt "%sswitch (%a) {\n%a\n%s}" (indent level)
        (pp_expr ~level:0 pp_t) expr_switch
        (pp_switch_cases ~level:(level + 1) pp_t pp_v pp_d)
        switch_cases (indent level)
  | IfS (expr_cond, stmt_then, stmt_else) -> (
      match stmt_else.it with
      | EmptyS ->
          F.fprintf fmt "%sif (%a)\n%a" (indent level) (pp_expr ~level:0 pp_t)
            expr_cond
            (pp_stmt ~level pp_t pp_v pp_d)
            stmt_then
      | _ ->
          F.fprintf fmt "%sif (%a)\n%a\n%selse\n%a" (indent level)
            (pp_expr ~level:0 pp_t) expr_cond
            (pp_stmt ~level pp_t pp_v pp_d)
            stmt_then (indent level)
            (pp_stmt ~level pp_t pp_v pp_d)
            stmt_else)
  | BlockS block -> pp_block ~level pp_t pp_v pp_d fmt block
  | ExitS -> F.fprintf fmt "%sexit;" (indent level)
  | RetS expr_ret -> (
      match expr_ret with
      | Some expr_ret ->
          F.fprintf fmt "%sreturn %a;" (indent level) (pp_expr ~level:0 pp_t)
            expr_ret
      | None -> F.fprintf fmt "%sreturn;" (indent level))
  | CallS (expr_func, targs, args) ->
      F.fprintf fmt "%s%a%a%a;" (indent level) (pp_expr ~level:0 pp_t) expr_func
        (pp_targs pp_t) targs (pp_args pp_t) args
  | TransS expr ->
      F.fprintf fmt "%stransition %a;" (indent level)
        (pp_expr ~level:(level + 1) pp_t)
        expr
  | DeclS decl -> pp_decl ~level pp_t pp_v pp_d fmt decl

and pp_stmt ?(level = 0) pp_t pp_v pp_d fmt stmt =
  pp_stmt' ~level pp_t pp_v pp_d fmt stmt.it

and pp_stmts ?(level = 0) pp_t pp_v pp_d fmt stmts =
  pp_list (pp_stmt ~level:(level + 1) pp_t pp_v pp_d) "\n" fmt stmts

(* Blocks (sequence of statements) *)

and pp_block' ?(level = 0) pp_t pp_v pp_d fmt block' =
  let stmts, _anno = block' in
  F.fprintf fmt "%s{\n%a\n%s}" (indent level)
    (pp_list (pp_stmt ~level:(level + 1) pp_t pp_v pp_d) "\n")
    stmts (indent level)

and pp_block ?(level = 0) pp_t pp_v pp_d fmt block =
  pp_block' ~level pp_t pp_v pp_d fmt block.it

(* Match-cases for switch *)

and pp_switch_label' fmt switch_label' =
  match switch_label' with
  | NameL text -> pp_text fmt text
  | DefaultL -> F.fprintf fmt "default"

and pp_switch_label fmt switch_label = pp_switch_label' fmt switch_label.it

and pp_switch_case' ?(level = 0) pp_t pp_v pp_d fmt switch_case' =
  match switch_case' with
  | MatchC (switch_label, block) ->
      F.fprintf fmt "%s%a:\n%a" (indent level) pp_switch_label switch_label
        (pp_block ~level:(level + 1) pp_t pp_v pp_d)
        block
  | FallC switch_label ->
      F.fprintf fmt "%s%a;" (indent level) pp_switch_label switch_label

and pp_switch_case ?(level = 0) pp_t pp_v pp_d fmt switch_case =
  pp_switch_case' ~level pp_t pp_v pp_d fmt switch_case.it

and pp_switch_cases ?(level = 0) pp_t pp_v pp_d fmt switch_cases =
  pp_list
    (pp_switch_case ~level:(level + 1) pp_t pp_v pp_d)
    "\n" fmt switch_cases

(* Declarations *)

and pp_decl' ?(level = 0) pp_t pp_v pp_d fmt decl' =
  match decl' with
  | ConstD { id; typ; value; annos = _annos } ->
      F.fprintf fmt "%sconst %a %a = %a;" (indent level) (pp_typ pp_t) typ pp_id
        id (pp_value pp_v) value
  | VarD { id; typ; init; annos = _annos } -> (
      match init with
      | Some expr_init ->
          F.fprintf fmt "%s%a %a = %a;" (indent level) (pp_typ pp_t) typ pp_id
            id (pp_expr ~level:0 pp_t) expr_init
      | None ->
          F.fprintf fmt "%s%a %a;" (indent level) (pp_typ pp_t) typ pp_id id)
  | InstD { id; typ; args; init; annos = _annos } -> (
      match init with
      | Some block_init ->
          F.fprintf fmt "%s%a%a %a = %a;" (indent level) (pp_typ pp_t) typ
            (pp_args pp_t) args pp_id id
            (pp_block ~level:(level + 1) pp_t pp_v pp_d)
            block_init
      | None ->
          F.fprintf fmt "%s%a%a %a;" (indent level) (pp_typ pp_t) typ
            (pp_args pp_t) args pp_id id)
  | ErrD { members } ->
      F.fprintf fmt "%serror {\n%a\n%s}" (indent level)
        (pp_members ~level:(level + 1))
        members (indent level)
  | MatchKindD { members } ->
      F.fprintf fmt "%smatch_kind {\n%a\n%s}" (indent level)
        (pp_members ~level:(level + 1))
        members (indent level)
  | StructD { id; fields; annos = _annos } ->
      let fields = List.map (fun (member, typ, _) -> (member, typ)) fields in
      F.fprintf fmt "%sstruct %a {\n%a\n%s}" (indent level) pp_id id
        (pp_pairs ~trailing:true ~level:(level + 1) pp_member (pp_typ pp_t)
           ";\n")
        fields (indent level)
  | HeaderD { id; fields; annos = _annos } ->
      let fields = List.map (fun (member, typ, _) -> (member, typ)) fields in
      F.fprintf fmt "%sheader %a {\n%a\n%s}" (indent level) pp_id id
        (pp_pairs ~trailing:true ~level:(level + 1) pp_member (pp_typ pp_t)
           ";\n")
        fields (indent level)
  | UnionD { id; fields; annos = _annos } ->
      let fields = List.map (fun (member, typ, _) -> (member, typ)) fields in
      F.fprintf fmt "%sheader_union %a {\n%a\n%s}" (indent level) pp_id id
        (pp_pairs ~trailing:true ~level:(level + 1) pp_member (pp_typ pp_t)
           ";\n")
        fields (indent level)
  | EnumD { id; members; annos = _annos } ->
      F.fprintf fmt "%senum %a {\n%a\n%s}" (indent level) pp_id id
        (pp_members ~level:(level + 1))
        members (indent level)
  | SEnumD { id; typ; fields; annos = _annos } ->
      F.fprintf fmt "%senum %a %a {\n%a\n%s}" (indent level) (pp_typ pp_t) typ
        pp_id id
        (pp_pairs pp_id (pp_expr ~level:0 pp_t) ";\n")
        fields (indent level)
  | NewTypeD { id; typdef; annos = _annos } -> (
      match typdef with
      | Left typ ->
          F.fprintf fmt "%stype %a %a;" (indent level) (pp_typ pp_t) typ pp_id
            id
      | Right decl ->
          F.fprintf fmt "%stype %a %a;" (indent level)
            (pp_decl ~level:(level + 1) pp_t pp_v pp_d)
            decl pp_id id)
  | TypeDefD { id; typdef; annos = _annos } -> (
      match typdef with
      | Left typ ->
          F.fprintf fmt "%stypedef %a %a;" (indent level) (pp_typ pp_t) typ
            pp_id id
      | Right decl ->
          F.fprintf fmt "%stypedef %a %a;" (indent level)
            (pp_decl ~level:(level + 1) pp_t pp_v pp_d)
            decl pp_id id)
  | ValueSetD { id; typ; size; annos = _annos } ->
      F.fprintf fmt "%svalue_set<%a>(%a) %a;" (indent level) (pp_typ pp_t) typ
        (pp_expr ~level:0 pp_t) size pp_id id
  | ParserTypeD { id; tparams; params; annos = _annos } ->
      F.fprintf fmt "%sparser %a%a%a;" (indent level) pp_id id pp_tparams
        tparams (pp_params pp_t pp_v pp_d) params
  | ParserD { id; tparams; params; cparams; locals; states; annos = _annos } ->
      F.fprintf fmt "%sparser %a%a%a%a {\n%a\n%a\n%s}" (indent level) pp_id id
        pp_tparams tparams (pp_params pp_t pp_v pp_d) params
        (pp_cparams pp_t pp_v pp_d)
        cparams
        (pp_decls ~level:(level + 1) pp_t pp_v pp_d)
        locals
        (pp_parser_states ~level:(level + 1) pp_t pp_v pp_d)
        states (indent level)
  | ActionD { id; params; body; annos = _annos } ->
      F.fprintf fmt "%saction %a%a\n%a" (indent level) pp_id id
        (pp_params pp_t pp_v pp_d) params
        (pp_block ~level pp_t pp_v pp_d)
        body
  | TableD { id; table; annos = _annos } ->
      F.fprintf fmt "%stable %a %a" (indent level) pp_id id
        (pp_table ~level pp_t) table
  | ControlTypeD { id; tparams; params; annos = _annos } ->
      F.fprintf fmt "%scontrol %a%a%a;" (indent level) pp_id id pp_tparams
        tparams (pp_params pp_t pp_v pp_d) params
  | ControlD { id; tparams; params; cparams; locals; body; annos = _annos } ->
      F.fprintf fmt "%scontrol %a%a%a%a {\n%a\n%sapply\n%a\n%s}" (indent level)
        pp_id id pp_tparams tparams (pp_params pp_t pp_v pp_d) params
        (pp_cparams pp_t pp_v pp_d)
        cparams
        (pp_decls ~level:(level + 1) pp_t pp_v pp_d)
        locals
        (indent (level + 1))
        (pp_block ~level:(level + 1) pp_t pp_v pp_d)
        body (indent level)
  | FuncD { id; typ_ret; tparams; params; body } ->
      F.fprintf fmt "%s%a %a%a%a\n%a" (indent level) (pp_typ pp_t) typ_ret pp_id
        id pp_tparams tparams (pp_params pp_t pp_v pp_d) params
        (pp_block ~level pp_t pp_v pp_d)
        body
  | ExternFuncD { id; typ_ret; tparams; params; annos = _annos } ->
      F.fprintf fmt "%sextern %a %a%a%a;" (indent level) (pp_typ pp_t) typ_ret
        pp_id id pp_tparams tparams (pp_params pp_t pp_v pp_d) params
  | ExternConstructorD { id; cparams; annos = _annos } ->
      F.fprintf fmt "%s%a%a;" (indent level) pp_id id (pp_params pp_t pp_v pp_d)
        cparams
  | ExternAbstractMethodD { id; typ_ret; tparams; params; annos = _annos } ->
      F.fprintf fmt "%sabstract %a %a%a%a;" (indent level) (pp_typ pp_t) typ_ret
        pp_id id pp_tparams tparams (pp_params pp_t pp_v pp_d) params
  | ExternMethodD { id; typ_ret; tparams; params; annos = _annos } ->
      F.fprintf fmt "%s%a %a%a%a;" (indent level) (pp_typ pp_t) typ_ret pp_id id
        pp_tparams tparams (pp_params pp_t pp_v pp_d) params
  | ExternObjectD { id; tparams; mthds; annos = _annos } ->
      F.fprintf fmt "%sextern %a%a {\n%a\n%s}" (indent level) pp_id id
        pp_tparams tparams
        (pp_decls ~level:(level + 1) pp_t pp_v pp_d)
        mthds (indent level)
  | PackageTypeD { id; tparams; cparams; annos = _annos } ->
      F.fprintf fmt "%spackage %a%a%a;" (indent level) pp_id id pp_tparams
        tparams
        (pp_cparams pp_t pp_v pp_d)
        cparams

and pp_decl ?(level = 0) pp_t pp_v pp_d fmt decl =
  pp_decl' ~level pp_t pp_v pp_d fmt decl.it

and pp_decls ?(level = 0) pp_t pp_v pp_d fmt decls =
  pp_list (pp_decl ~level:(level + 1) pp_t pp_v pp_d) "\n" fmt decls

(* Parser states *)

and pp_parser_state' ?(level = 0) pp_t pp_v pp_d fmt parser_state' =
  let label, block, _annos = parser_state' in
  F.fprintf fmt "%sstate %s\n%a" (indent level) label.it
    (pp_block ~level:(level + 1) pp_t pp_v pp_d)
    block

and pp_parser_state ?(level = 0) pp_t pp_v pp_d fmt parser_state =
  pp_parser_state' ~level pp_t pp_v pp_d fmt parser_state.it

and pp_parser_states ?(level = 0) pp_t pp_v pp_d fmt parser_states =
  pp_list
    (pp_parser_state ~level:(level + 1) pp_t pp_v pp_d)
    "\n" fmt parser_states

(* Tables *)

and pp_table ?(level = 0) pp_t fmt table =
  let table_keys, table_actions, table_entries, table_default, table_custom =
    table
  in
  match table_default with
  | Some table_default ->
      F.fprintf fmt "{\n%a\n%a\n%a\n%a\n%a\n%s}"
        (pp_table_keys ~level:(level + 1) pp_t)
        table_keys
        (pp_table_actions ~level:(level + 1) pp_t)
        table_actions
        (pp_table_entries ~level:(level + 1) pp_t)
        table_entries
        (pp_table_default ~level:(level + 1) pp_t)
        table_default
        (pp_table_customs ~level:(level + 1) pp_t)
        table_custom (indent level)
  | None ->
      F.fprintf fmt "{\n%a\n%a\n%a\n%a\n%s}"
        (pp_table_keys ~level:(level + 1) pp_t)
        table_keys
        (pp_table_actions ~level:(level + 1) pp_t)
        table_actions
        (pp_table_entries ~level:(level + 1) pp_t)
        table_entries
        (pp_table_customs ~level:(level + 1) pp_t)
        table_custom (indent level)

(* Table keys *)

and pp_table_key' ?(level = 0) pp_t fmt table_key' =
  let expr, match_kind, _annos = table_key' in
  F.fprintf fmt "%s%a : %a;" (indent level) (pp_expr ~level:0 pp_t) expr
    pp_match_kind match_kind

and pp_table_key ?(level = 0) pp_t fmt table_key =
  pp_table_key' ~level pp_t fmt table_key.it

and pp_table_keys ?(level = 0) pp_t fmt table_keys =
  match table_keys with
  | [] -> ()
  | _ ->
      F.fprintf fmt "%skey = {\n%a\n%s}" (indent level)
        (pp_list (pp_table_key ~level:(level + 1) pp_t) "\n")
        table_keys (indent level)

(* Table action references *)

and pp_table_action' ?(level = 0) pp_t fmt table_action' =
  let var, args, _annos = table_action' in
  match args with
  | [] -> F.fprintf fmt "%s%a;" (indent level) pp_var var
  | _ -> F.fprintf fmt "%s%a%a;" (indent level) pp_var var (pp_args pp_t) args

and pp_table_action ?(level = 0) pp_t fmt table_action =
  pp_table_action' ~level pp_t fmt table_action.it

and pp_table_actions ?(level = 0) pp_t fmt table_actions =
  match table_actions with
  | [] -> ()
  | _ ->
      F.fprintf fmt "%sactions = {\n%a\n%s}" (indent level)
        (pp_list (pp_table_action ~level:(level + 1) pp_t) "\n")
        table_actions (indent level)

(* Table entries *)

and pp_table_entry' ?(level = 0) pp_t fmt table_entry' =
  let keysets, table_action, _annos = table_entry' in
  F.fprintf fmt "%s%a : %a" (indent level) (pp_keysets pp_t) keysets
    (pp_table_action ~level:(level + 1) pp_t)
    table_action

and pp_table_entry ?(level = 0) pp_t fmt table_entry =
  pp_table_entry' ~level pp_t fmt table_entry.it

and pp_table_entries ?(level = 0) pp_t fmt table_entries =
  match table_entries with
  | [] -> ()
  | _ ->
      F.fprintf fmt "%sconst entries = {\n%a\n%s}" (indent level)
        (pp_list (pp_table_entry ~level:(level + 1) pp_t) "\n")
        table_entries (indent level)

(* Table default properties *)

and pp_table_default' ?(level = 0) pp_t fmt table_default' =
  let table_action, table_default_const = table_default' in
  F.fprintf fmt "%s%sdefault_action = %a" (indent level)
    (if table_default_const then "const " else "")
    (pp_table_action ~level:(level + 1) pp_t)
    table_action

and pp_table_default ?(level = 0) pp_t fmt table_default =
  pp_table_default' ~level pp_t fmt table_default.it

(* Table custom properties *)

and pp_table_custom' ?(level = 0) pp_t fmt table_custom' =
  let id, expr, table_custom_const, _annos = table_custom' in
  F.fprintf fmt "%s%s %a = %a;" (indent level)
    (if table_custom_const then " const" else "")
    pp_id id (pp_expr ~level:0 pp_t) expr

and pp_table_custom ?(level = 0) pp_t fmt table_custom =
  pp_table_custom' ~level pp_t fmt table_custom.it

and pp_table_customs ?(level = 0) pp_t fmt table_customs =
  pp_list (pp_table_custom ~level:(level + 1) pp_t) "\n" fmt table_customs

(* Program *)

let pp_program pp_t pp_v pp_d fmt program = pp_decls pp_t pp_v pp_d fmt program
