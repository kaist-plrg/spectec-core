module P = Lang.Pp
module F = Format
open Ast
open Util.Source

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

let pp_dir' fmt dir' = P.pp_dir' fmt dir'
let pp_dir fmt dir = P.pp_dir fmt dir

(* Types *)

let rec pp_typ' fmt typ' =
  match (typ' : Ast.typ') with
  | VoidT -> F.pp_print_string fmt "void"
  | BoolT -> F.pp_print_string fmt "bool"
  | ErrT -> F.pp_print_string fmt "err"
  | StrT -> F.pp_print_string fmt "string"
  | IntT -> F.pp_print_string fmt "int"
  | FIntT expr_width -> F.fprintf fmt "int<%a>" (pp_expr ~level:0) expr_width
  | FBitT expr_width -> F.fprintf fmt "bit<%a>" (pp_expr ~level:0) expr_width
  | VBitT expr_width -> F.fprintf fmt "varbit<%a>" (pp_expr ~level:0) expr_width
  | NameT var -> F.fprintf fmt "%a" pp_var var
  | SpecT (var, targs) -> F.fprintf fmt "%a%a" pp_var var pp_targs targs
  | StackT (typ, expr_size) ->
      F.fprintf fmt "%a[%a]" pp_typ typ (pp_expr ~level:0) expr_size
  | TupleT typs -> F.fprintf fmt "(%a)" pp_typs typs
  | AnyT -> F.pp_print_string fmt "any"

and pp_typ fmt typ = pp_typ' fmt typ.it
and pp_typs fmt typs = P.pp_list pp_typ ", " fmt typs

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
      F.fprintf fmt "%a %a %a = %a" pp_dir dir pp_typ typ pp_id id
        (pp_expr ~level:0) value_default
  | None -> F.fprintf fmt "%a %a %a" pp_dir dir pp_typ typ pp_id id

and pp_param fmt param = pp_param' fmt param.it
and pp_params fmt params = F.fprintf fmt "(%a)" (P.pp_list pp_param ", ") params

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
  | TupleE { exprs } ->
      F.fprintf fmt "{ %a }" (P.pp_list (pp_expr ~level:0) ", ") exprs
  | RecordE { fields } ->
      F.fprintf fmt "{ %a }"
        (P.pp_pairs pp_member (pp_expr ~level:0) ", ")
        fields
  | UnE { unop; expr } ->
      F.fprintf fmt "%a%a" pp_unop unop (pp_expr ~level:0) expr
  | BinE { binop; expr_l; expr_r } ->
      F.fprintf fmt "((%a) %a (%a))" (pp_expr ~level:0) expr_l pp_binop binop
        (pp_expr ~level:0) expr_r
  | TernE { expr_cond; expr_then; expr_else } ->
      F.fprintf fmt "((%a) ? (%a) : (%a))" (pp_expr ~level:0) expr_cond
        (pp_expr ~level:0) expr_then (pp_expr ~level:0) expr_else
  | CastE { typ; expr } ->
      F.fprintf fmt "((%a) (%a))" pp_typ typ (pp_expr ~level:0) expr
  | MaskE { expr_base; expr_mask } ->
      F.fprintf fmt "%a &&& %a" (pp_expr ~level:0) expr_base (pp_expr ~level:0)
        expr_mask
  | RangeE { expr_lb; expr_ub } ->
      F.fprintf fmt "%a .. %a" (pp_expr ~level:0) expr_lb (pp_expr ~level:0)
        expr_ub
  | SelectE { exprs_select; cases } ->
      F.fprintf fmt "select (%a) {\n%a\n%s}" (pp_exprs ~level:0) exprs_select
        (pp_select_cases ~level:(level + 1))
        cases (P.indent level)
  | ArrAccE { expr_base; expr_idx } ->
      F.fprintf fmt "%a[%a]" (pp_expr ~level:0) expr_base (pp_expr ~level:0)
        expr_idx
  | BitAccE { expr_base; expr_lo; expr_hi } ->
      F.fprintf fmt "%a[%a:%a]" (pp_expr ~level:0) expr_base (pp_expr ~level:0)
        expr_hi (pp_expr ~level:0) expr_lo
  | TypeAccE { var_base; member } ->
      F.fprintf fmt "%a.%a" pp_var var_base (pp_member ~level:0) member
  | ErrAccE { member } -> F.fprintf fmt "error.%a" (pp_member ~level:0) member
  | ExprAccE { expr_base; member } ->
      F.fprintf fmt "%a.%a" (pp_expr ~level:0) expr_base (pp_member ~level:0)
        member
  | CallE { expr_func; targs; args } ->
      F.fprintf fmt "%a%a%a" (pp_expr ~level:0) expr_func pp_targs targs pp_args
        args
  | InstE { var_inst; targs; args } ->
      F.fprintf fmt "%a%a%a" pp_var var_inst pp_targs targs pp_args args

and pp_expr ?(level = 0) fmt expr = pp_expr' ~level fmt expr.it
and pp_exprs ?(level = 0) fmt exprs = P.pp_list (pp_expr ~level) ", " fmt exprs

(* Keyset expressions *)

and pp_keyset' fmt keyset' = P.pp_keyset' pp_expr fmt keyset'
and pp_keyset fmt keyset = P.pp_keyset pp_expr fmt keyset
and pp_keysets fmt keysets = P.pp_keysets pp_expr fmt keysets

(* Select-cases for select *)

and pp_select_case' ?(level = 0) fmt select_case' =
  P.pp_select_case' ~level pp_expr fmt select_case'

and pp_select_case ?(level = 0) fmt select_case =
  P.pp_select_case ~level pp_expr fmt select_case

and pp_select_cases ?(level = 0) fmt select_cases =
  P.pp_select_cases ~level pp_expr fmt select_cases

(* Statements *)

and pp_stmt' ?(level = 0) fmt stmt' =
  P.pp_stmt' ~level pp_typ pp_expr pp_decl fmt stmt'

and pp_stmt ?(level = 0) fmt stmt =
  P.pp_stmt ~level pp_typ pp_expr pp_decl fmt stmt

and pp_stmts ?(level = 0) fmt stmts =
  P.pp_stmts ~level pp_typ pp_expr pp_decl fmt stmts

(* Blocks (sequence of statements) *)

and pp_block' ?(level = 0) fmt block' =
  P.pp_block' ~level pp_typ pp_expr pp_decl fmt block'

and pp_block ?(level = 0) fmt block =
  P.pp_block ~level pp_typ pp_expr pp_decl fmt block

(* Match-cases for switch *)

and pp_switch_label' fmt switch_label' = P.pp_switch_label' fmt switch_label'
and pp_switch_label fmt switch_label = P.pp_switch_label fmt switch_label

and pp_switch_case' ?(level = 0) fmt switch_case' =
  P.pp_switch_case' ~level pp_typ pp_expr pp_decl fmt switch_case'

and pp_switch_case ?(level = 0) fmt switch_case =
  P.pp_switch_case ~level pp_typ pp_expr pp_decl fmt switch_case

and pp_switch_cases ?(level = 0) fmt switch_cases =
  P.pp_switch_cases ~level pp_typ pp_expr pp_decl fmt switch_cases

(* Declarations *)

and pp_decl' ?(level = 0) fmt decl' =
  match decl' with
  | ConstD { id; typ; value; annos = _annos } ->
      F.fprintf fmt "%sconst %a %a = %a;" (P.indent level) pp_typ typ pp_id id
        (pp_expr ~level:0) value
  | VarD { id; typ; init; annos = _annos } -> (
      match init with
      | Some expr_init ->
          F.fprintf fmt "%s%a %a = %a;" (P.indent level) pp_typ typ pp_id id
            (pp_expr ~level:0) expr_init
      | None -> F.fprintf fmt "%s%a %a;" (P.indent level) pp_typ typ pp_id id)
  | InstD { id; var_inst; targs; args; init; annos = _annos } -> (
      match init with
      | [] ->
          F.fprintf fmt "%s%a%a%a %a;" (P.indent level) pp_var var_inst pp_targs
            targs pp_args args pp_id id
      | init ->
          F.fprintf fmt "%s%a%a%a %a = {\n%a\n%s};" (P.indent level) pp_var
            var_inst pp_targs targs pp_args args pp_id id
            (pp_decls ~level:(level + 1))
            init (P.indent level))
  | ErrD { members } ->
      F.fprintf fmt "%serror {\n%a\n%s}" (P.indent level)
        (pp_members ~level:(level + 1))
        members (P.indent level)
  | MatchKindD { members } ->
      F.fprintf fmt "%smatch_kind {\n%a\n%s}" (P.indent level)
        (pp_members ~level:(level + 1))
        members (P.indent level)
  | StructD { id; fields; annos = _annos } ->
      let fields = List.map (fun (member, typ, _) -> (member, typ)) fields in
      F.fprintf fmt "%sstruct %a {\n%a\n%s}" (P.indent level) pp_id id
        (P.pp_pairs ~trailing:true ~level:(level + 1) pp_member pp_typ ";\n")
        fields (P.indent level)
  | HeaderD { id; fields; annos = _annos } ->
      let fields = List.map (fun (member, typ, _) -> (member, typ)) fields in
      F.fprintf fmt "%sheader %a {\n%a\n%s}" (P.indent level) pp_id id
        (P.pp_pairs ~trailing:true ~level:(level + 1) pp_member pp_typ ";\n")
        fields (P.indent level)
  | UnionD { id; fields; annos = _annos } ->
      let fields = List.map (fun (member, typ, _) -> (member, typ)) fields in
      F.fprintf fmt "%sheader_union %a {\n%a\n%s}" (P.indent level) pp_id id
        (P.pp_pairs ~trailing:true ~level:(level + 1) pp_member pp_typ ";\n")
        fields (P.indent level)
  | EnumD { id; members; annos = _annos } ->
      F.fprintf fmt "%senum %a {\n%a\n%s}" (P.indent level) pp_id id
        (pp_members ~level:(level + 1))
        members (P.indent level)
  | SEnumD { id; typ; fields; annos = _annos } ->
      F.fprintf fmt "%senum %a %a {\n%a\n%s}" (P.indent level) pp_typ typ pp_id
        id
        (P.pp_pairs pp_id (pp_expr ~level:0) ";\n")
        fields (P.indent level)
  | NewTypeD { id; typdef; annos = _annos } -> (
      match typdef with
      | Left typ ->
          F.fprintf fmt "%stype %a %a;" (P.indent level) pp_typ typ pp_id id
      | Right decl ->
          F.fprintf fmt "%stype %a %a;" (P.indent level)
            (pp_decl ~level:(level + 1))
            decl pp_id id)
  | TypeDefD { id; typdef; annos = _annos } -> (
      match typdef with
      | Left typ ->
          F.fprintf fmt "%stypedef %a %a;" (P.indent level) pp_typ typ pp_id id
      | Right decl ->
          F.fprintf fmt "%stypedef %a %a;" (P.indent level)
            (pp_decl ~level:(level + 1))
            decl pp_id id)
  | ValueSetD { id; typ; size; annos = _annos } ->
      F.fprintf fmt "%svalue_set<%a>(%a) %a;" (P.indent level) pp_typ typ
        (pp_expr ~level:0) size pp_id id
  | ParserTypeD { id; tparams; params; annos = _annos } ->
      F.fprintf fmt "%sparser %a%a%a;" (P.indent level) pp_id id pp_tparams
        tparams pp_params params
  | ParserD { id; tparams; params; cparams; locals; states; annos = _annos } ->
      F.fprintf fmt "%sparser %a%a%a%a {\n%a\n%a\n%s}" (P.indent level) pp_id id
        pp_tparams tparams pp_params params pp_cparams cparams
        (pp_decls ~level:(level + 1))
        locals
        (pp_parser_states ~level:(level + 1))
        states (P.indent level)
  | ActionD { id; params; body; annos = _annos } ->
      F.fprintf fmt "%saction %a%a\n%a" (P.indent level) pp_id id pp_params
        params (pp_block ~level) body
  | TableD { id; table; annos = _annos } ->
      F.fprintf fmt "%stable %a %a" (P.indent level) pp_id id (pp_table ~level)
        table
  | ControlTypeD { id; tparams; params; annos = _annos } ->
      F.fprintf fmt "%scontrol %a%a%a;" (P.indent level) pp_id id pp_tparams
        tparams pp_params params
  | ControlD { id; tparams; params; cparams; locals; body; annos = _annos } ->
      F.fprintf fmt "%scontrol %a%a%a%a {\n%a\n%sapply\n%a\n%s}"
        (P.indent level) pp_id id pp_tparams tparams pp_params params pp_cparams
        cparams
        (pp_decls ~level:(level + 1))
        locals
        (P.indent (level + 1))
        (pp_block ~level:(level + 1))
        body (P.indent level)
  | FuncD { id; typ_ret; tparams; params; body } ->
      F.fprintf fmt "%s%a %a%a%a\n%a" (P.indent level) pp_typ typ_ret pp_id id
        pp_tparams tparams pp_params params (pp_block ~level) body
  | ExternFuncD { id; typ_ret; tparams; params; annos = _annos } ->
      F.fprintf fmt "%sextern %a %a%a%a;" (P.indent level) pp_typ typ_ret pp_id
        id pp_tparams tparams pp_params params
  | ExternConstructorD { id; cparams; annos = _annos } ->
      F.fprintf fmt "%s%a%a;" (P.indent level) pp_id id pp_params cparams
  | ExternAbstractMethodD { id; typ_ret; tparams; params; annos = _annos } ->
      F.fprintf fmt "%sabstract %a %a%a%a;" (P.indent level) pp_typ typ_ret
        pp_id id pp_tparams tparams pp_params params
  | ExternMethodD { id; typ_ret; tparams; params; annos = _annos } ->
      F.fprintf fmt "%s%a %a%a%a;" (P.indent level) pp_typ typ_ret pp_id id
        pp_tparams tparams pp_params params
  | ExternObjectD { id; tparams; mthds; annos = _annos } ->
      F.fprintf fmt "%sextern %a%a {\n%a\n%s}" (P.indent level) pp_id id
        pp_tparams tparams
        (pp_decls ~level:(level + 1))
        mthds (P.indent level)
  | PackageTypeD { id; tparams; cparams; annos = _annos } ->
      F.fprintf fmt "%spackage %a%a%a;" (P.indent level) pp_id id pp_tparams
        tparams pp_cparams cparams

and pp_decl ?(level = 0) fmt decl = pp_decl' ~level fmt decl.it

and pp_decls ?(level = 0) fmt decls =
  P.pp_list (pp_decl ~level:(level + 1)) "\n" fmt decls

(* Parser states *)

and pp_parser_state' ?(level = 0) fmt parser_state' =
  P.pp_parser_state' ~level pp_typ pp_expr pp_decl fmt parser_state'

and pp_parser_state ?(level = 0) fmt parser_state =
  P.pp_parser_state ~level pp_typ pp_expr pp_decl fmt parser_state

and pp_parser_states ?(level = 0) fmt parser_states =
  P.pp_parser_states ~level pp_typ pp_expr pp_decl fmt parser_states

(* Tables *)

and pp_table ?(level = 0) fmt table = P.pp_table ~level pp_expr fmt table

(* Table keys *)

and pp_table_key' ?(level = 0) fmt table_key' =
  P.pp_table_key' ~level pp_expr fmt table_key'

and pp_table_key ?(level = 0) fmt table_key =
  P.pp_table_key ~level pp_expr fmt table_key

and pp_table_keys ?(level = 0) fmt table_keys =
  P.pp_table_keys ~level pp_expr fmt table_keys

(* Table action references *)

and pp_table_action' ?(level = 0) fmt table_action' =
  P.pp_table_action' ~level pp_expr fmt table_action'

and pp_table_action ?(level = 0) fmt table_action =
  P.pp_table_action ~level pp_expr fmt table_action

and pp_table_actions ?(level = 0) fmt table_actions =
  P.pp_table_actions ~level pp_expr fmt table_actions

(* Table entries *)

and pp_table_entry' ?(level = 0) fmt table_entry' =
  P.pp_table_entry' ~level pp_expr fmt table_entry'

and pp_table_entry ?(level = 0) fmt table_entry =
  P.pp_table_entry ~level pp_expr fmt table_entry

and pp_table_entries ?(level = 0) fmt table_entries =
  P.pp_table_entries ~level pp_expr fmt table_entries

(* Table default properties *)

and pp_table_default' ?(level = 0) fmt table_default' =
  P.pp_table_default' ~level pp_expr fmt table_default'

and pp_table_default ?(level = 0) fmt table_default =
  P.pp_table_default ~level pp_expr fmt table_default

(* Table custom properties *)

and pp_table_custom' ?(level = 0) fmt table_custom' =
  P.pp_table_custom' ~level pp_expr fmt table_custom'

and pp_table_custom ?(level = 0) fmt table_custom =
  P.pp_table_custom ~level pp_expr fmt table_custom

and pp_table_customs ?(level = 0) fmt table_customs =
  P.pp_table_customs ~level pp_expr fmt table_customs

(* Program *)

let pp_program fmt program = P.pp_program pp_decl fmt program
