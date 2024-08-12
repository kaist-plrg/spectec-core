open Util.Source
open Ast
module F = Format

let indent level = String.make (2 * level) ' '

(* Numbers *)

let pp_num' fmt num' =
  let i, width_signed = num' in
  match width_signed with
  | Some (width, signed) ->
      F.fprintf fmt "%s%s%s" (Bigint.to_string width)
        (if signed then "s" else "w")
        (Bigint.to_string i)
  | None -> F.fprintf fmt "%s" (Bigint.to_string i)

let pp_num fmt num = pp_num' fmt num.it

(* Texts *)

let pp_text' fmt text' = F.fprintf fmt "%s" text'
let pp_text fmt text = pp_text' fmt text.it

(* Identifiers *)

let pp_id' fmt id' = F.fprintf fmt "%s" id'
let pp_id fmt id = pp_id' fmt id.it

(* Paths *)

let pp_path fmt path =
  F.fprintf fmt "%a"
    (Format.pp_print_list ~pp_sep:(fun fmt () -> F.fprintf fmt ".") pp_id)
    path

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
  F.fprintf fmt "%a"
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> F.fprintf fmt ",\n")
       (fun fmt member -> pp_member ~level fmt member))
    members

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

(* Annotations *)

let pp_anno _fmt _anno = failwith "TODO"

(* Types *)

let rec pp_type' fmt typ' =
  match typ' with
  | VoidT -> F.fprintf fmt "void"
  | BoolT -> F.fprintf fmt "bool"
  | MatchKindT -> F.fprintf fmt "match_kind"
  | ErrT -> F.fprintf fmt "error"
  | StrT -> F.fprintf fmt "string"
  | IntT -> F.fprintf fmt "int"
  | FIntT expr -> F.fprintf fmt "int<%a>" (pp_expr ~level:0) expr
  | FBitT expr -> F.fprintf fmt "bit<%a>" (pp_expr ~level:0) expr
  | VBitT expr -> F.fprintf fmt "varbit<%a>" (pp_expr ~level:0) expr
  | NameT var -> pp_var fmt var
  | SpecT (var, typs) ->
      F.fprintf fmt "%a<%a>" pp_var var
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> F.fprintf fmt ", ")
           pp_type)
        typs
  | StackT (typ, expr) ->
      F.fprintf fmt "%a[%a]" pp_type typ (pp_expr ~level:0) expr
  | TupleT typs ->
      F.fprintf fmt "tuple<%a>"
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> F.fprintf fmt ", ")
           pp_type)
        typs
  | AnyT -> F.fprintf fmt "_"

and pp_type fmt typ = pp_type' fmt typ.it

(* Directions *)

and pp_dir' fmt dir' =
  match dir' with
  | No -> ()
  | In -> F.fprintf fmt "in"
  | Out -> F.fprintf fmt "out"
  | InOut -> F.fprintf fmt "inout"

and pp_dir fmt dir = pp_dir' fmt dir.it

(* Type parameters *)

and pp_tparam' = pp_id'
and pp_tparam = pp_id

and pp_tparams fmt tparams =
  match tparams with
  | [] -> ()
  | _ ->
      F.fprintf fmt "<%a>"
        (F.pp_print_list ~pp_sep:(fun fmt () -> F.fprintf fmt ", ") pp_tparam)
        tparams

(* Parameters *)

and pp_param' fmt param' =
  let id, dir, typ, expr_default, _annos = param' in
  match expr_default with
  | Some expr when dir.it = No ->
      F.fprintf fmt "%a %a = %a" pp_type typ pp_id id (pp_expr ~level:0) expr
  | Some expr ->
      F.fprintf fmt "%a %a %a = %a" pp_dir dir pp_type typ pp_id id
        (pp_expr ~level:0) expr
  | None when dir.it = No -> F.fprintf fmt "%a %a" pp_type typ pp_id id
  | None -> F.fprintf fmt "%a %a %a" pp_dir dir pp_type typ pp_id id

and pp_param fmt param = pp_param' fmt param.it

and pp_params fmt params =
  F.fprintf fmt "(%a)"
    (Format.pp_print_list ~pp_sep:(fun fmt () -> F.fprintf fmt ", ") pp_param)
    params

(* Constant parameters *)

and pp_cparam' fmt cparam' = pp_param' fmt cparam'
and pp_cparam fmt cparam = pp_param fmt cparam
and pp_cparams fmt cparams = pp_params fmt cparams

(* Type arguments *)

and pp_targ' fmt targ' = pp_type' fmt targ'
and pp_targ fmt targ = pp_type fmt targ

and pp_targs fmt targs =
  match targs with
  | [] -> ()
  | _ ->
      F.fprintf fmt "<%a>"
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> F.fprintf fmt ", ")
           pp_targ)
        targs

(* Arguments *)

and pp_arg' fmt arg' =
  match arg' with
  | ExprA expr -> pp_expr fmt expr
  | NameA (id, expr) -> F.fprintf fmt "%a = %a" pp_id id (pp_expr ~level:0) expr
  | AnyA -> F.fprintf fmt "_"

and pp_arg fmt arg = pp_arg' fmt arg.it

and pp_args fmt args =
  F.fprintf fmt "(%a)"
    (Format.pp_print_list ~pp_sep:(fun fmt () -> F.fprintf fmt ", ") pp_arg)
    args

(* Expressions *)

and pp_expr' ?(level = 0) fmt expr' =
  match expr' with
  | BoolE b -> F.fprintf fmt "%b" b
  | StrE t -> F.fprintf fmt "\"%a\"" pp_text t
  | NumE num -> pp_num fmt num
  | VarE var -> pp_var fmt var
  | ListE exprs ->
      F.fprintf fmt "{ %a }"
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> F.fprintf fmt ", ")
           pp_expr)
        exprs
  | RecordE fields ->
      F.fprintf fmt "{ %a }"
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> F.fprintf fmt ", ")
           (fun fmt (name, expr) ->
             F.fprintf fmt "%s = %a" name.it (pp_expr ~level:0) expr))
        fields
  | UnE (unop, expr) ->
      F.fprintf fmt "%a%a" pp_unop unop (pp_expr ~level:0) expr
  | BinE (binop, expr_left, expr_right) ->
      F.fprintf fmt "((%a) %a (%a))" (pp_expr ~level:0) expr_left pp_binop binop
        (pp_expr ~level:0) expr_right
  | TernE (expr_cond, expr_then, expr_else) ->
      F.fprintf fmt "((%a) ? (%a) : (%a))" (pp_expr ~level:0) expr_cond
        (pp_expr ~level:0) expr_then (pp_expr ~level:0) expr_else
  | CastE (typ, expr) ->
      F.fprintf fmt "((%a) (%a))" pp_type typ (pp_expr ~level:0) expr
  | MaskE (expr, mask) ->
      F.fprintf fmt "%a &&& %a" (pp_expr ~level:0) expr (pp_expr ~level:0) mask
  | RangeE (lo, hi) ->
      F.fprintf fmt "%a .. %a" (pp_expr ~level:0) lo (pp_expr ~level:0) hi
  | SelectE (exprs_keyset, select_cases) ->
      F.fprintf fmt "select (%a) {\n%a\n%s}" (pp_exprs ~level:0) exprs_keyset
        (pp_select_cases ~level:(level + 1))
        select_cases (indent level)
  | ArrAccE (expr, index) ->
      F.fprintf fmt "%a[%a]" (pp_expr ~level:0) expr (pp_expr ~level:0) index
  | BitAccE (expr, lo, hi) ->
      F.fprintf fmt "%a[%a:%a]" (pp_expr ~level:0) expr (pp_expr ~level:0) hi
        (pp_expr ~level:0) lo
  | TypeAccE (var, field) -> F.fprintf fmt "%a.%s" pp_var var field.it
  | ErrAccE field -> F.fprintf fmt "error.%s" field.it
  | ExprAccE (expr, field) ->
      F.fprintf fmt "%a.%s" (pp_expr ~level:0) expr field.it
  | CallE (expr, targs, args) ->
      F.fprintf fmt "%a%a%a" (pp_expr ~level:0) expr pp_targs targs pp_args args
  | InstE (typ, args) -> F.fprintf fmt "%a%a" pp_type typ pp_args args

and pp_expr ?(level = 0) fmt expr = pp_expr' ~level fmt expr.it

and pp_exprs ?(level = 0) fmt exprs =
  F.fprintf fmt "%a"
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> F.fprintf fmt ", ")
       (fun fmt expr -> pp_expr ~level fmt expr))
    exprs

(* Keyset expressions *)

and pp_keyset' fmt keyset' =
  match keyset' with
  | ExprK expr -> pp_expr fmt expr
  | DefaultK -> F.fprintf fmt "default"
  | AnyK -> F.fprintf fmt "_"

and pp_keyset fmt keyset = pp_keyset' fmt keyset.it

and pp_keysets fmt keysets =
  F.fprintf fmt "(%a)"
    (Format.pp_print_list ~pp_sep:(fun fmt () -> F.fprintf fmt ", ") pp_keyset)
    keysets

(* Select-cases for select *)

and pp_select_case' ?(level = 0) fmt select_case' =
  let keysets, state_label = select_case' in
  F.fprintf fmt "%s%a: %a;" (indent level) pp_keysets keysets pp_state_label
    state_label

and pp_select_case ?(level = 0) fmt select_case =
  pp_select_case' ~level fmt select_case.it

and pp_select_cases ?(level = 0) fmt select_cases =
  F.fprintf fmt "%a"
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> F.fprintf fmt "\n")
       (fun fmt select_case ->
         pp_select_case ~level:(level + 1) fmt select_case))
    select_cases

(* Statements *)

let rec pp_stmt' ?(level = 0) fmt stmt' =
  match stmt' with
  | EmptyS -> F.fprintf fmt "%s;" (indent level)
  | AssignS (expr_lhs, expr_rhs) ->
      F.fprintf fmt "%s%a = %a;" (indent level) (pp_expr ~level:0) expr_lhs
        (pp_expr ~level:0) expr_rhs
  | SwitchS (expr_switch, switch_cases) ->
      F.fprintf fmt "%sswitch (%a) {\n%a\n%s}" (indent level) (pp_expr ~level:0)
        expr_switch
        (pp_switch_cases ~level:(level + 1))
        switch_cases (indent level)
  | IfS (expr_cond, stmt_then, stmt_else) -> (
      match stmt_else.it with
      | EmptyS ->
          F.fprintf fmt "%sif (%a)\n%a" (indent level) (pp_expr ~level:0)
            expr_cond (pp_stmt ~level) stmt_then
      | _ ->
          F.fprintf fmt "%sif (%a)\n%a\n%selse\n%a" (indent level)
            (pp_expr ~level:0) expr_cond (pp_stmt ~level) stmt_then
            (indent level) (pp_stmt ~level) stmt_else)
  | BlockS block -> pp_block ~level fmt block
  | ExitS -> F.fprintf fmt "%sexit;" (indent level)
  | RetS expr_ret -> (
      match expr_ret with
      | Some expr_ret ->
          F.fprintf fmt "%sreturn %a;" (indent level) (pp_expr ~level:0)
            expr_ret
      | None -> F.fprintf fmt "%sreturn;" (indent level))
  | CallS (expr_func, targs, args) ->
      F.fprintf fmt "%s%a%a%a;" (indent level) (pp_expr ~level:0) expr_func
        pp_targs targs pp_args args
  | TransS expr ->
      F.fprintf fmt "%stransition %a;" (indent level)
        (pp_expr ~level:(level + 1))
        expr
  | DeclS decl -> pp_decl ~level fmt decl

and pp_stmt ?(level = 0) fmt stmt = pp_stmt' ~level fmt stmt.it

(* Blocks (sequence of statements) *)

and pp_block' ?(level = 0) fmt block' =
  let stmts, _anno = block' in
  F.fprintf fmt "%s{\n%a\n%s}" (indent level)
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> F.fprintf fmt "\n")
       (fun fmt stmt -> pp_stmt ~level:(level + 1) fmt stmt))
    stmts (indent level)

and pp_block ?(level = 0) fmt block = pp_block' ~level fmt block.it

(* Match-cases for switch *)

and pp_switch_label' fmt switch_label' =
  match switch_label' with
  | NameL text -> pp_text fmt text
  | DefaultL -> F.fprintf fmt "default"

and pp_switch_label fmt switch_label = pp_switch_label' fmt switch_label.it

and pp_switch_case' ?(level = 0) fmt switch_case' =
  match switch_case' with
  | MatchC (switch_label, block) ->
      F.fprintf fmt "%s%a:\n%a" (indent level) pp_switch_label switch_label
        (pp_block ~level:(level + 1))
        block
  | FallC switch_label ->
      F.fprintf fmt "%s%a;" (indent level) pp_switch_label switch_label

and pp_switch_case ?(level = 0) fmt switch_case =
  pp_switch_case' ~level fmt switch_case.it

and pp_switch_cases ?(level = 0) fmt switch_cases =
  F.fprintf fmt "%a"
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> F.fprintf fmt "\n")
       (fun fmt switch_case ->
         pp_switch_case ~level:(level + 1) fmt switch_case))
    switch_cases

(* Declarations *)

and pp_decl' ?(level = 0) fmt decl' =
  match decl' with
  | ConstD { id; typ; value; annos = _annos } ->
      F.fprintf fmt "%sconst %a %a = %a;" (indent level) pp_type typ pp_id id
        (pp_expr ~level:0) value
  | VarD { id; typ; init; annos = _annos } -> (
      match init with
      | Some init ->
          F.fprintf fmt "%s%a %a = %a;" (indent level) pp_type typ pp_id id
            (pp_expr ~level:0) init
      | None -> F.fprintf fmt "%s%a %a;" (indent level) pp_type typ pp_id id)
  | InstD { id; typ; args; init; annos = _annos } -> (
      match init with
      | Some block ->
          F.fprintf fmt "%s%a%a %a = %a;" (indent level) pp_type typ pp_args
            args pp_id id
            (pp_block ~level:(level + 1))
            block
      | None ->
          F.fprintf fmt "%s%a%a %a;" (indent level) pp_type typ pp_args args
            pp_id id)
  | ErrD { members } ->
      F.fprintf fmt "%serror {\n%a\n%s}" (indent level)
        (pp_members ~level:(level + 1))
        members (indent level)
  | MatchKindD { members } ->
      F.fprintf fmt "%smatch_kind {\n%a\n%s}" (indent level)
        (pp_members ~level:(level + 1))
        members (indent level)
  | StructD { id; fields; annos = _annos } ->
      F.fprintf fmt "%sstruct %a {\n%a\n%s}" (indent level) pp_id id
        (pp_record_fields ~level:(level + 1))
        fields (indent level)
  | HeaderD { id; fields; annos = _annos } ->
      F.fprintf fmt "%sheader %a {\n%a\n%s}" (indent level) pp_id id
        (pp_record_fields ~level:(level + 1))
        fields (indent level)
  | UnionD { id; fields; annos = _annos } ->
      F.fprintf fmt "%sheader_union %a {\n%a\n%s}" (indent level) pp_id id
        (pp_record_fields ~level:(level + 1))
        fields (indent level)
  | EnumD { id; members; annos = _annos } ->
      F.fprintf fmt "%senum %a {\n%a\n%s}" (indent level) pp_id id
        (pp_members ~level:(level + 1))
        members (indent level)
  | SEnumD { id; typ; fields; annos = _annos } ->
      F.fprintf fmt "%senum %a %a {\n%a\n%s}" (indent level) pp_type typ pp_id
        id
        (pp_serial_fields ~level:(level + 1))
        fields (indent level)
  | NewTypeD { id; typ; annos = _annos } -> (
      match typ with
      | Left typ ->
          F.fprintf fmt "%stype %a %a;" (indent level) pp_type typ pp_id id
      | Right decl ->
          F.fprintf fmt "%stype %a %a;" (indent level)
            (pp_decl ~level:(level + 1))
            decl pp_id id)
  | TypeDefD { id; typ; annos = _annos } -> (
      match typ with
      | Left typ ->
          F.fprintf fmt "%stypedef %a %a;" (indent level) pp_type typ pp_id id
      | Right decl ->
          F.fprintf fmt "%stypedef %a %a;" (indent level)
            (pp_decl ~level:(level + 1))
            decl pp_id id)
  | ValueSetD { id; typ; size; annos = _annos } ->
      F.fprintf fmt "%svalue_set<%a>(%a) %a;" (indent level) pp_type typ
        (pp_expr ~level:0) size pp_id id
  | ParserTypeD { id; tparams; params; annos = _annos } ->
      F.fprintf fmt "%sparser %a%a%a;" (indent level) pp_id id pp_tparams
        tparams pp_params params
  | ParserD { id; tparams; params; cparams; locals; states; annos = _annos } ->
      F.fprintf fmt "%sparser %a%a%a%a {\n%a\n%a\n%s}" (indent level) pp_id id
        pp_tparams tparams pp_params params pp_cparams cparams
        (pp_decls ~level:(level + 1))
        locals
        (pp_parser_states ~level:(level + 1))
        states (indent level)
  | ActionD { id; params; body; annos = _annos } ->
      F.fprintf fmt "%saction %a%a\n%a" (indent level) pp_id id pp_params params
        (pp_block ~level) body
  | TableD { id; table; annos = _annos } ->
      F.fprintf fmt "%stable %a %a" (indent level) pp_id id (pp_table ~level)
        table
  | ControlTypeD { id; tparams; params; annos = _annos } ->
      F.fprintf fmt "%scontrol %a%a%a;" (indent level) pp_id id pp_tparams
        tparams pp_params params
  | ControlD { id; tparams; params; cparams; locals; body; annos = _annos } ->
      F.fprintf fmt "%scontrol %a%a%a%a {\n%a\n%sapply\n%a\n%s}" (indent level)
        pp_id id pp_tparams tparams pp_params params pp_cparams cparams
        (pp_decls ~level:(level + 1))
        locals
        (indent (level + 1))
        (pp_block ~level:(level + 1))
        body (indent level)
  | FuncD { id; rettyp; tparams; params; body } ->
      F.fprintf fmt "%s%a %a%a%a\n%a" (indent level) pp_type rettyp pp_id id
        pp_tparams tparams pp_params params (pp_block ~level) body
  | ExternFuncD { id; rettyp; tparams; params; annos = _annos } ->
      F.fprintf fmt "%sextern %a %a%a%a;" (indent level) pp_type rettyp pp_id id
        pp_tparams tparams pp_params params
  | ConsD { id; cparams; annos = _annos } ->
      F.fprintf fmt "%s %a%a;" (indent level) pp_id id pp_params cparams
  | AbstractD { id; rettyp; tparams; params; annos = _annos } ->
      F.fprintf fmt "%sabstract %a %a%a%a;" (indent level) pp_type rettyp pp_id
        id pp_tparams tparams pp_params params
  | MethodD { id; rettyp; tparams; params; annos = _annos } ->
      F.fprintf fmt "%s%a %a%a%a;" (indent level) pp_type rettyp pp_id id
        pp_tparams tparams pp_params params
  | ExternObjectD { id; tparams; mthds; annos = _annos } ->
      F.fprintf fmt "%sextern %a%a {\n%a\n%s}" (indent level) pp_id id
        pp_tparams tparams
        (pp_decls ~level:(level + 1))
        mthds (indent level)
  | PackageTypeD { id; tparams; cparams; annos = _annos } ->
      F.fprintf fmt "%spackage %a%a%a;" (indent level) pp_id id pp_tparams
        tparams pp_params cparams

and pp_decl ?(level = 0) fmt decl = pp_decl' ~level fmt decl.it

and pp_decls ?(level = 0) fmt decls =
  F.fprintf fmt "%a"
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> F.fprintf fmt "\n")
       (fun fmt decl -> pp_decl ~level fmt decl))
    decls

and pp_record_field ?(level = 0) fmt field =
  let id, typ, _annos = field in
  F.fprintf fmt "%s%a %a;" (indent level) pp_type typ pp_id id

and pp_record_fields ?(level = 0) fmt fields =
  F.fprintf fmt "%a"
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> F.fprintf fmt "\n")
       (fun fmt field -> pp_record_field ~level fmt field))
    fields

and pp_serial_field ?(level = 0) fmt field =
  let id, expr = field in
  F.fprintf fmt "%s%a = %a" (indent level) pp_id id (pp_expr ~level:0) expr

and pp_serial_fields ?(level = 0) fmt fields =
  F.fprintf fmt "%a"
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> F.fprintf fmt ",\n")
       (fun fmt field -> pp_serial_field ~level fmt field))
    fields

(* Parser states *)

and pp_parser_state' ?(level = 0) fmt parser_state' =
  let label, block, _annos = parser_state' in
  F.fprintf fmt "%sstate %s\n%a" (indent level) label.it
    (pp_block ~level:(level + 1))
    block

and pp_parser_state ?(level = 0) fmt parser_state =
  pp_parser_state' ~level fmt parser_state.it

and pp_parser_states ?(level = 0) fmt parser_states =
  F.fprintf fmt "%a"
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> F.fprintf fmt "\n")
       (fun fmt parser_state ->
         pp_parser_state ~level:(level + 1) fmt parser_state))
    parser_states

(* Tables *)

and pp_table ?(level = 0) fmt table =
  let table_keys, table_actions, table_entries, table_default, table_custom =
    table
  in
  match table_default with
  | Some table_default ->
      F.fprintf fmt "{\n%a\n%a\n%a\n%a\n%a\n%s}"
        (pp_table_keys ~level:(level + 1))
        table_keys
        (pp_table_actions ~level:(level + 1))
        table_actions
        (pp_table_entries ~level:(level + 1))
        table_entries
        (pp_table_default ~level:(level + 1))
        table_default
        (pp_table_customs ~level:(level + 1))
        table_custom (indent level)
  | None ->
      F.fprintf fmt "{\n%a\n%a\n%a\n%a\n%s}"
        (pp_table_keys ~level:(level + 1))
        table_keys
        (pp_table_actions ~level:(level + 1))
        table_actions
        (pp_table_entries ~level:(level + 1))
        table_entries
        (pp_table_customs ~level:(level + 1))
        table_custom (indent level)

(* Table keys *)

and pp_table_key' ?(level = 0) fmt table_key' =
  let expr, match_kind, _annos = table_key' in
  F.fprintf fmt "%s%a : %a;" (indent level) (pp_expr ~level:0) expr
    pp_match_kind match_kind

and pp_table_key ?(level = 0) fmt table_key =
  pp_table_key' ~level fmt table_key.it

and pp_table_keys ?(level = 0) fmt table_keys =
  match table_keys with
  | [] -> ()
  | _ ->
      F.fprintf fmt "%skey = {\n%a\n%s}" (indent level)
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> F.fprintf fmt "\n")
           (fun fmt table_key -> pp_table_key ~level:(level + 1) fmt table_key))
        table_keys (indent level)

(* Table action references *)

and pp_table_action' ?(level = 0) fmt table_action' =
  let var, args, _annos = table_action' in
  match args with
  | [] -> F.fprintf fmt "%s%a;" (indent level) pp_var var
  | _ -> F.fprintf fmt "%s%a%a;" (indent level) pp_var var pp_args args

and pp_table_action ?(level = 0) fmt table_action =
  pp_table_action' ~level fmt table_action.it

and pp_table_actions ?(level = 0) fmt table_actions =
  match table_actions with
  | [] -> ()
  | _ ->
      F.fprintf fmt "%sactions = {\n%a\n%s}" (indent level)
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> F.fprintf fmt "\n")
           (fun fmt table_action ->
             pp_table_action ~level:(level + 1) fmt table_action))
        table_actions (indent level)

(* Table entries *)

and pp_table_entry' ?(level = 0) fmt table_entry' =
  let keysets, table_action, _annos = table_entry' in
  F.fprintf fmt "%s%a : %a" (indent level) pp_keysets keysets
    (pp_table_action ~level:(level + 1))
    table_action

and pp_table_entry ?(level = 0) fmt table_entry =
  pp_table_entry' ~level fmt table_entry.it

and pp_table_entries ?(level = 0) fmt table_entries =
  match table_entries with
  | [] -> ()
  | _ ->
      F.fprintf fmt "%sconst entries = {\n%a\n%s}" (indent level)
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> F.fprintf fmt "\n")
           (fun fmt entry -> pp_table_entry ~level:(level + 1) fmt entry))
        table_entries (indent level)

(* Table default properties *)

and pp_table_default' ?(level = 0) fmt table_default' =
  let table_action, table_default_const = table_default' in
  F.fprintf fmt "%s%sdefault_action = %a" (indent level)
    (if table_default_const then "const " else "")
    (pp_table_action ~level:(level + 1))
    table_action

and pp_table_default ?(level = 0) fmt table_default =
  pp_table_default' ~level fmt table_default.it

(* Table custom properties *)

and pp_table_custom' ?(level = 0) fmt table_custom' =
  let id, expr, table_custom_const, _annos = table_custom' in
  F.fprintf fmt "%s%s %a = %a;" (indent level)
    (if table_custom_const then " const" else "")
    pp_id id (pp_expr ~level:0) expr

and pp_table_custom ?(level = 0) fmt table_custom =
  pp_table_custom' ~level fmt table_custom.it

and pp_table_customs ?(level = 0) fmt table_customs =
  F.fprintf fmt "%a"
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> F.fprintf fmt "\n")
       (fun fmt table_custom ->
         pp_table_custom ~level:(level + 1) fmt table_custom))
    table_customs

(* Program *)

let pp_program fmt program = pp_decls fmt program
