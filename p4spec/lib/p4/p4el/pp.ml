module L = P4lang.Ast
module P = P4lang.Pp
module F = Format
open Ast
open P4util.Pp
open P4util.Source

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

let pp_member' fmt member' = P.pp_member' fmt member'
let pp_member fmt member = P.pp_member fmt member
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

let pp_dir' fmt dir' = P.pp_dir' fmt dir'
let pp_dir fmt dir = P.pp_dir fmt dir

(* Types *)

let rec pp_typ' fmt typ' =
  match typ' with
  | VoidT -> F.pp_print_string fmt "void"
  | ErrT -> F.pp_print_string fmt "error"
  | MatchKindT -> F.pp_print_string fmt "match_kind"
  | StrT -> F.pp_print_string fmt "string"
  | BoolT -> F.pp_print_string fmt "bool"
  | IntT -> F.pp_print_string fmt "int"
  | FIntT expr_width -> (
      match expr_width.it with
      | VarE _ -> F.fprintf fmt "int<(%a)>" (pp_expr ~level:0) expr_width
      | _ -> F.fprintf fmt "int<%a>" (pp_expr ~level:0) expr_width)
  | FBitT expr_width -> (
      match expr_width.it with
      | VarE _ -> F.fprintf fmt "bit<(%a)>" (pp_expr ~level:0) expr_width
      | _ -> F.fprintf fmt "bit<%a>" (pp_expr ~level:0) expr_width)
  | VBitT expr_width -> (
      match expr_width.it with
      | VarE _ -> F.fprintf fmt "varbit<(%a)>" (pp_expr ~level:0) expr_width
      | _ -> F.fprintf fmt "varbit<%a>" (pp_expr ~level:0) expr_width)
  | NameT var -> F.fprintf fmt "%a" pp_var var
  | SpecT (var, targs) -> F.fprintf fmt "%a%a" pp_var var pp_targs targs
  | StackT (typ, expr_size) ->
      F.fprintf fmt "%a[%a]" pp_typ typ (pp_expr ~level:0) expr_size
  | ListT typ -> F.fprintf fmt "list<%a>" pp_typ typ
  | TupleT typs -> F.fprintf fmt "tuple<%a>" pp_typs typs
  | AnyT -> F.pp_print_string fmt "_"

and pp_typ fmt typ = pp_typ' fmt typ.it
and pp_typs fmt typs = pp_list pp_typ ~sep:Comma fmt typs

(* Values *)

(* Annotations *)

and pp_anno' fmt anno' = P.pp_anno pp_expr fmt anno'
and pp_anno fmt anno = P.pp_anno pp_expr fmt anno

(* Type parameters *)

and pp_tparam' fmt tparam' = P.pp_tparam' fmt tparam'
and pp_tparam fmt tparam = P.pp_tparam fmt tparam
and pp_tparams fmt tparams = P.pp_tparams fmt tparams

(* Parameters *)

and pp_param' fmt param' =
  let id, dir, typ, value_default, _annos = param' in
  match value_default with
  | Some value_default ->
      if dir.it = L.No then
        F.fprintf fmt "%a %a = %a" pp_typ typ pp_id id (pp_expr ~level:0)
          value_default
      else
        F.fprintf fmt "%a %a %a = %a" pp_dir dir pp_typ typ pp_id id
          (pp_expr ~level:0) value_default
  | None ->
      if dir.it = L.No then F.fprintf fmt "%a %a" pp_typ typ pp_id id
      else F.fprintf fmt "%a %a %a" pp_dir dir pp_typ typ pp_id id

and pp_param fmt param = pp_param' fmt param.it

and pp_params fmt params =
  F.fprintf fmt "(%a)" (pp_list pp_param ~sep:Comma) params

(* Constructor parameters *)

and pp_cparam' fmt cparam' = pp_param' fmt cparam'
and pp_cparam fmt cparam = pp_param fmt cparam
and pp_cparams fmt cparams = pp_params fmt cparams

(* Type arguments *)

and pp_targ' fmt targ' = pp_typ' fmt targ'
and pp_targ fmt targ = P.pp_targ pp_typ fmt targ
and pp_targs fmt targs = P.pp_targs pp_typ fmt targs

(* Arguments *)

and pp_arg' fmt arg' = P.pp_arg' pp_expr fmt arg'
and pp_arg fmt arg = P.pp_arg pp_expr fmt arg
and pp_args fmt args = P.pp_args pp_expr fmt args

(* Expressions *)

and pp_expr' ?(level = 0) fmt expr' =
  match expr' with
  | BoolE { boolean } -> F.fprintf fmt "%b" boolean
  | StrE { text } -> F.fprintf fmt "\"%a\"" pp_text text
  | NumE { num } -> pp_num fmt num
  | VarE { var } -> pp_var fmt var
  | SeqE { exprs } ->
      F.fprintf fmt "{ %a }" (pp_list (pp_expr ~level) ~sep:Comma) exprs
  | SeqDefaultE { exprs } ->
      if exprs = [] then F.pp_print_string fmt "{ ... }"
      else
        F.fprintf fmt "{ %a, ... }" (pp_list (pp_expr ~level) ~sep:Comma) exprs
  | RecordE { fields } ->
      F.fprintf fmt "{ %a }"
        (pp_pairs pp_member (pp_expr ~level) ~rel:Eq ~sep:Comma)
        fields
  | RecordDefaultE { fields } ->
      F.fprintf fmt "{ %a, ... }"
        (pp_pairs pp_member (pp_expr ~level) ~rel:Eq ~sep:Comma)
        fields
  | DefaultE -> F.pp_print_string fmt "..."
  | InvalidE -> F.pp_print_string fmt "{#}"
  | UnE { unop; expr } ->
      F.fprintf fmt "%a%a" pp_unop unop (pp_expr ~level) expr
  | BinE { binop; expr_l; expr_r } ->
      F.fprintf fmt "((%a) %a (%a))" (pp_expr ~level) expr_l pp_binop binop
        (pp_expr ~level) expr_r
  | TernE { expr_cond; expr_then; expr_else } ->
      F.fprintf fmt "((%a) ? (%a) : (%a))" (pp_expr ~level) expr_cond
        (pp_expr ~level) expr_then (pp_expr ~level) expr_else
  | CastE { typ; expr } ->
      F.fprintf fmt "((%a) (%a))" pp_typ typ (pp_expr ~level) expr
  | MaskE { expr_base; expr_mask } ->
      F.fprintf fmt "%a &&& %a" (pp_expr ~level) expr_base (pp_expr ~level)
        expr_mask
  | RangeE { expr_lb; expr_ub } ->
      F.fprintf fmt "%a .. %a" (pp_expr ~level) expr_lb (pp_expr ~level) expr_ub
  | SelectE { exprs_select; cases } ->
      F.fprintf fmt "select(%a) {\n%a\n%s}" (pp_exprs ~level) exprs_select
        (pp_select_cases ~level:(level + 1))
        cases (indent level)
  | ArrAccE { expr_base; expr_idx } ->
      F.fprintf fmt "%a[%a]" (pp_expr ~level) expr_base (pp_expr ~level)
        expr_idx
  | BitAccE { expr_base; expr_lo; expr_hi } ->
      F.fprintf fmt "%a[%a:%a]" (pp_expr ~level) expr_base (pp_expr ~level)
        expr_hi (pp_expr ~level) expr_lo
  | TypeAccE { var_base; member } ->
      F.fprintf fmt "%a.%a" pp_var var_base pp_member member
  | ErrAccE { member } -> F.fprintf fmt "error.%a" pp_member member
  | ExprAccE { expr_base; member } ->
      F.fprintf fmt "%a.%a" (pp_expr ~level) expr_base pp_member member
  | CallFuncE { var_func; targs; args } ->
      F.fprintf fmt "%a%a%a" pp_var var_func pp_targs targs pp_args args
  | CallMethodE { expr_base; member; targs; args } ->
      F.fprintf fmt "%a.%a%a%a" (pp_expr ~level) expr_base pp_member member
        pp_targs targs pp_args args
  | CallTypeE { var_typ; member; targs; args } ->
      F.fprintf fmt "%a.%a%a%a" pp_var var_typ pp_member member pp_targs targs
        pp_args args
  | InstE { var_inst; targs; args } ->
      F.fprintf fmt "%a%a%a" pp_var var_inst pp_targs targs pp_args args

and pp_expr ?(level = 0) fmt expr = pp_expr' ~level fmt expr.it

and pp_exprs ?(level = 0) fmt exprs =
  pp_list (pp_expr ~level) ~sep:Comma fmt exprs

(* Keyset expressions *)

and pp_keyset' fmt keyset' = P.pp_keyset' pp_expr fmt keyset'
and pp_keyset fmt keyset = P.pp_keyset pp_expr fmt keyset
and pp_keysets fmt keysets = P.pp_keysets pp_expr fmt keysets

(* Select-cases for select *)

and pp_select_case' fmt select_case' =
  P.pp_select_case' pp_expr fmt select_case'

and pp_select_case fmt select_case = P.pp_select_case pp_expr fmt select_case

and pp_select_cases ?(level = 0) fmt select_cases =
  P.pp_select_cases ~level pp_expr fmt select_cases

(* Statements *)

and pp_stmt' ?(level = 0) fmt stmt' =
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
        (pp_switch_cases ~level:(level + 1))
        cases (indent level)
  | IfS { expr_cond; stmt_then; stmt_else } -> (
      match stmt_else.it with
      | EmptyS ->
          F.fprintf fmt "if (%a) %a"
            (pp_expr ~level:(level + 1))
            expr_cond (pp_stmt ~level) stmt_then
      | _ ->
          F.fprintf fmt "if (%a) %a\n%selse %a"
            (pp_expr ~level:(level + 1))
            expr_cond (pp_stmt ~level) stmt_then (indent level) (pp_stmt ~level)
            stmt_else)
  | BlockS { block } -> pp_block ~level fmt block
  | ExitS -> F.fprintf fmt "exit;"
  | RetS { expr_ret } -> (
      match expr_ret with
      | Some expr_ret ->
          F.fprintf fmt "return %a;" (pp_expr ~level:(level + 1)) expr_ret
      | None -> F.fprintf fmt "return;")
  | CallFuncS { var_func; targs; args } ->
      F.fprintf fmt "%a%a%a;" pp_var var_func pp_targs targs pp_args args
  | CallMethodS { expr_base; member; targs; args } ->
      F.fprintf fmt "%a.%a%a%a;"
        (pp_expr ~level:(level + 1))
        expr_base pp_member member pp_targs targs pp_args args
  | CallInstS { var_inst; targs; args } ->
      F.fprintf fmt "%a%a.apply%a;" pp_var var_inst pp_targs targs pp_args args
  | TransS { expr_label } ->
      let sexpr_label =
        F.asprintf "%a" (pp_expr ~level:(level + 1)) expr_label
      in
      let trailing_semicolon =
        if String.starts_with ~prefix:"select(" sexpr_label then "" else ";"
      in
      F.fprintf fmt "transition %s%s" sexpr_label trailing_semicolon
  | DeclS { decl } -> pp_decl ~level fmt decl

and pp_stmt ?(level = 0) fmt stmt = pp_stmt' ~level fmt stmt.it

and pp_stmts ?(level = 0) fmt stmts =
  pp_list ~level (pp_stmt ~level) ~sep:Nl fmt stmts

(* Blocks (sequence of statements) *)

and pp_block' ?(level = 0) fmt block' =
  P.pp_block' ~level pp_expr pp_stmt fmt block'

and pp_block ?(level = 0) fmt block =
  P.pp_block ~level pp_expr pp_stmt fmt block

(* Match-cases for switch *)

and pp_switch_label' fmt switch_label' =
  P.pp_switch_label' pp_expr fmt switch_label'

and pp_switch_label fmt switch_label =
  P.pp_switch_label pp_expr fmt switch_label

and pp_switch_case' ?(level = 0) fmt switch_case' =
  P.pp_switch_case' ~level pp_expr pp_stmt fmt switch_case'

and pp_switch_case ?(level = 0) fmt switch_case =
  P.pp_switch_case ~level pp_expr pp_stmt fmt switch_case

and pp_switch_cases ?(level = 0) fmt switch_cases =
  P.pp_switch_cases ~level pp_expr pp_stmt fmt switch_cases

(* Declarations *)

and pp_decl' ?(level = 0) fmt decl' =
  match decl' with
  | ConstD { id; typ; value; annos = _annos } ->
      F.fprintf fmt "const %a %a = %a;" pp_typ typ pp_id id (pp_expr ~level:0)
        value
  | VarD { id; typ; init; annos = _annos } -> (
      match init with
      | Some expr_init ->
          F.fprintf fmt "%a %a = %a;" pp_typ typ pp_id id (pp_expr ~level:0)
            expr_init
      | None -> F.fprintf fmt "%a %a;" pp_typ typ pp_id id)
  | InstD { id; var_inst; targs; args; init; annos = _annos } -> (
      match init with
      | [] ->
          F.fprintf fmt "%a%a%a %a;" pp_var var_inst pp_targs targs pp_args args
            pp_id id
      | init ->
          F.fprintf fmt "%a%a%a %a = {\n%a\n%s};" pp_var var_inst pp_targs targs
            pp_args args pp_id id
            (pp_decls ~level:(level + 1))
            init (indent level))
  | ErrD { members } ->
      F.fprintf fmt "error {\n%a\n%s}"
        (pp_members ~level:(level + 1))
        members (indent level)
  | MatchKindD { members } ->
      F.fprintf fmt "match_kind {\n%a\n%s}"
        (pp_members ~level:(level + 1))
        members (indent level)
  | StructD { id; tparams; fields; annos = _annos } ->
      let fields = List.map (fun (member, typ, _) -> (typ, member)) fields in
      F.fprintf fmt "struct %a%a {\n%a\n%s}" pp_id id pp_tparams tparams
        (pp_pairs ~trailing:true ~level:(level + 1) pp_typ pp_member ~rel:Space
           ~sep:SemicolonNl)
        fields (indent level)
  | HeaderD { id; tparams; fields; annos = _annos } ->
      let fields = List.map (fun (member, typ, _) -> (typ, member)) fields in
      F.fprintf fmt "header %a%a {\n%a\n%s}" pp_id id pp_tparams tparams
        (pp_pairs ~trailing:true ~level:(level + 1) pp_typ pp_member ~rel:Space
           ~sep:SemicolonNl)
        fields (indent level)
  | UnionD { id; tparams; fields; annos = _annos } ->
      let fields = List.map (fun (member, typ, _) -> (typ, member)) fields in
      F.fprintf fmt "header_union %a%a {\n%a\n%s}" pp_id id pp_tparams tparams
        (pp_pairs ~trailing:true ~level:(level + 1) pp_typ pp_member ~rel:Space
           ~sep:SemicolonNl)
        fields (indent level)
  | EnumD { id; members; annos = _annos } ->
      F.fprintf fmt "enum %a {\n%a\n%s};" pp_id id
        (pp_members ~level:(level + 1))
        members (indent level)
  | SEnumD { id; typ; fields; annos = _annos } ->
      F.fprintf fmt "enum %a %a {\n%a\n%s};" pp_typ typ pp_id id
        (pp_pairs pp_id (pp_expr ~level:0) ~rel:Eq ~sep:CommaNl)
        fields (indent level)
  | NewTypeD { id; typdef; annos = _annos } -> (
      match typdef with
      | Left typ -> F.fprintf fmt "type %a %a;" pp_typ typ pp_id id
      | Right decl ->
          F.fprintf fmt "type %a %a;" (pp_decl ~level:(level + 1)) decl pp_id id
      )
  | TypeDefD { id; typdef; annos = _annos } -> (
      match typdef with
      | Left typ -> F.fprintf fmt "typedef %a %a;" pp_typ typ pp_id id
      | Right decl ->
          F.fprintf fmt "typedef %a %a;"
            (pp_decl ~level:(level + 1))
            decl pp_id id)
  | ValueSetD { id; typ; size; annos = _annos } ->
      F.fprintf fmt "value_set<%a>(%a) %a;" pp_typ typ (pp_expr ~level:0) size
        pp_id id
  | ParserTypeD { id; tparams; params; annos = _annos } ->
      F.fprintf fmt "parser %a%a%a;" pp_id id pp_tparams tparams pp_params
        params
  | ParserD { id; tparams; params; cparams; locals; states; annos = _annos } ->
      F.fprintf fmt "parser %a%a%a%a {\n%a\n%a\n%s}" pp_id id pp_tparams tparams
        pp_params params pp_cparams cparams
        (pp_decls ~level:(level + 1))
        locals
        (pp_parser_states ~level:(level + 1))
        states (indent level)
  | TableD { id; table; annos = _annos } ->
      F.fprintf fmt "table %a %a" pp_id id (pp_table ~level) table
  | ControlTypeD { id; tparams; params; annos = _annos } ->
      F.fprintf fmt "control %a%a%a;" pp_id id pp_tparams tparams pp_params
        params
  | ControlD { id; tparams; params; cparams; locals; body; annos = _annos } ->
      F.fprintf fmt "control %a%a%a%a {\n%a\n%sapply %a\n%s}" pp_id id
        pp_tparams tparams pp_params params pp_cparams cparams
        (pp_decls ~level:(level + 1))
        locals
        (indent (level + 1))
        (pp_block ~level:(level + 1))
        body (indent level)
  | ActionD { id; params; body; annos = _annos } ->
      F.fprintf fmt "action %a%a %a" pp_id id pp_params params (pp_block ~level)
        body
  | FuncD { id; typ_ret; tparams; params; body } ->
      F.fprintf fmt "%a %a%a%a %a" pp_typ typ_ret pp_id id pp_tparams tparams
        pp_params params (pp_block ~level) body
  | ExternFuncD { id; typ_ret; tparams; params; annos = _annos } ->
      F.fprintf fmt "extern %a %a%a%a;" pp_typ typ_ret pp_id id pp_tparams
        tparams pp_params params
  | ExternObjectD { id; tparams; mthds; annos = _annos } ->
      F.fprintf fmt "extern %a%a {\n%a\n%s}" pp_id id pp_tparams tparams
        (pp_mthds ~level:(level + 1))
        mthds (indent level)
  | PackageTypeD { id; tparams; cparams; annos = _annos } ->
      F.fprintf fmt "package %a%a%a;" pp_id id pp_tparams tparams pp_cparams
        cparams

and pp_decl ?(level = 0) fmt decl = pp_decl' ~level fmt decl.it

and pp_decls ?(level = 0) fmt decls =
  pp_list ~level (pp_decl ~level) ~sep:Nl fmt decls

(* Parser states *)

and pp_parser_state' ?(level = 0) fmt parser_state' =
  P.pp_parser_state' ~level pp_expr pp_stmt fmt parser_state'

and pp_parser_state ?(level = 0) fmt parser_state =
  P.pp_parser_state ~level pp_expr pp_stmt fmt parser_state

and pp_parser_states ?(level = 0) fmt parser_states =
  P.pp_parser_states ~level pp_expr pp_stmt fmt parser_states

(* Tables *)

and pp_table ?(level = 0) fmt table =
  P.pp_table ~level pp_expr pp_table_action pp_table_entry fmt table

(* Table properties *)

and pp_table_property ?(level = 0) fmt table_properties =
  P.pp_table_property ~level pp_expr pp_table_action pp_table_entry fmt
    table_properties

and pp_table_properties ?(level = 0) fmt table_properties =
  P.pp_table_properties ~level pp_expr pp_table_action pp_table_entry fmt
    table_properties

(* Table keys *)

and pp_table_key' fmt table_key' = P.pp_table_key' pp_expr fmt table_key'
and pp_table_key fmt table_key = P.pp_table_key pp_expr fmt table_key

and pp_table_keys' ?(level = 0) fmt table_keys' =
  P.pp_table_keys' ~level pp_expr fmt table_keys'

and pp_table_keys ?(level = 0) fmt table_keys =
  P.pp_table_keys ~level pp_expr fmt table_keys

(* Table action references *)

and pp_table_action' fmt table_action' =
  let var, args, _annos = table_action' in
  match args with
  | [] -> F.fprintf fmt "%a;" pp_var var
  | _ -> F.fprintf fmt "%a%a;" pp_var var pp_args args

and pp_table_action fmt table_action = pp_table_action' fmt table_action.it

and pp_table_actions' ?(level = 0) fmt table_actions' =
  P.pp_table_actions' ~level pp_table_action fmt table_actions'

and pp_table_actions ?(level = 0) fmt table_actions =
  P.pp_table_actions ~level pp_table_action fmt table_actions

(* Table entries *)

and pp_table_entry' fmt table_entry' =
  let table_entry_const, keysets, table_action, table_entry_priority, _annos =
    table_entry'
  in
  F.fprintf fmt "%s%s%a%s%a : %a"
    (if table_entry_const then "const " else "")
    (if table_entry_priority |> Option.is_some then "priority = " else "")
    (pp_option pp_expr) table_entry_priority
    (if table_entry_priority |> Option.is_some then " : " else "")
    pp_keysets keysets pp_table_action table_action

and pp_table_entry fmt table_entry = pp_table_entry' fmt table_entry.it

and pp_table_entries' ?(level = 0) fmt table_entries' =
  P.pp_table_entries' ~level pp_expr fmt table_entries'

and pp_table_entries ?(level = 0) fmt table_entries =
  P.pp_table_entries ~level pp_expr fmt table_entries

(* Table default properties *)

and pp_table_default' fmt table_default' =
  P.pp_table_default' pp_table_action fmt table_default'

and pp_table_default fmt table_default =
  P.pp_table_default pp_table_action fmt table_default

(* Table custom properties *)

and pp_table_custom' fmt table_custom' =
  P.pp_table_custom' pp_expr fmt table_custom'

and pp_table_custom fmt table_custom =
  P.pp_table_custom pp_expr fmt table_custom

(* Methods *)

and pp_mthd' fmt mthd' =
  match mthd' with
  | ExternConsM { id; cparams; annos = _annos } ->
      F.fprintf fmt "%a%a;" pp_id id pp_cparams cparams
  | ExternAbstractM { id; typ_ret; tparams; params; annos = _annos } ->
      F.fprintf fmt "abstract %a %a%a%a;" pp_typ typ_ret pp_id id pp_tparams
        tparams pp_params params
  | ExternM { id; typ_ret; tparams; params; annos = _annos } ->
      F.fprintf fmt "%a %a%a%a;" pp_typ typ_ret pp_id id pp_tparams tparams
        pp_params params

and pp_mthd fmt mthd = pp_mthd' fmt mthd.it
and pp_mthds ?(level = 0) fmt mthds = pp_list ~level pp_mthd ~sep:Nl fmt mthds

(* Program *)

let pp_program fmt program = P.pp_program pp_decl fmt program
