open Util.Source
open Ast
module F = Format

let indent level = String.make (2 * level) ' '

(* Numbers *)

let pp_num fmt num =
  let i, width_signed = num.it in
  match width_signed with
  | Some (width, signed) ->
      F.fprintf fmt "%s%s%s" (Bigint.to_string width)
        (if signed then "s" else "w")
        (Bigint.to_string i)
  | None -> F.fprintf fmt "%s" (Bigint.to_string i)

(* Names *)

let pp_id fmt id = F.fprintf fmt "%s" id.it

let pp_path fmt path =
  F.fprintf fmt "%a"
    (Format.pp_print_list ~pp_sep:(fun fmt () -> F.fprintf fmt ".") pp_id)
    path

let pp_var fmt var =
  match var.it with
  | Bare id -> F.fprintf fmt "%a" pp_id id
  | Top id -> F.fprintf fmt ".%a" pp_id id

let pp_member fmt member = F.fprintf fmt "%s" member.it
let pp_label fmt label = F.fprintf fmt "%s" label.it
let pp_mtch_kind fmt mtch_kind = F.fprintf fmt "%s" mtch_kind.it

(* Unary and binary operators *)

let pp_unop fmt unop =
  match unop.it with
  | BNotOp -> F.fprintf fmt "~"
  | LNotOp -> F.fprintf fmt "!"
  | UMinusOp -> F.fprintf fmt "-"

let pp_binop fmt binop =
  match binop.it with
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

(* Types *)

let rec pp_type fmt typ =
  match typ.it with
  | VoidT -> F.fprintf fmt "void"
  | BoolT -> F.fprintf fmt "bool"
  | ErrT -> F.fprintf fmt "error"
  | StrT -> F.fprintf fmt "string"
  | AIntT -> F.fprintf fmt "int"
  | IntT expr -> F.fprintf fmt "int<%a>" pp_expr expr
  | BitT expr -> F.fprintf fmt "bit<%a>" pp_expr expr
  | VBitT expr -> F.fprintf fmt "varbit<%a>" pp_expr expr
  | NameT var -> pp_var fmt var
  | SpecT (var, typs) ->
      F.fprintf fmt "%a<%a>" pp_var var
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> F.fprintf fmt ", ")
           pp_type)
        typs
  | StackT (typ, expr) -> F.fprintf fmt "%a[%a]" pp_type typ pp_expr expr
  | TupleT typs ->
      F.fprintf fmt "tuple<%a>"
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> F.fprintf fmt ", ")
           pp_type)
        typs
  | AnyT -> F.fprintf fmt "_"

(* Directions *)

and pp_dir fmt dir =
  match dir.it with
  | No -> ()
  | In -> F.fprintf fmt "in"
  | Out -> F.fprintf fmt "out"
  | InOut -> F.fprintf fmt "inout"

(* Parameters *)

and pp_tparam fmt tparam = F.fprintf fmt "%a" pp_id tparam

and pp_tparams fmt tparams =
  match tparams with
  | [] -> ()
  | _ ->
      F.fprintf fmt "<%a>"
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> F.fprintf fmt ", ")
           pp_tparam)
        tparams

and pp_param fmt param =
  let id, dir, typ, default = param.it in
  match default with
  | Some expr ->
      if dir.it = No then
        F.fprintf fmt "%a %a = %a" pp_type typ pp_id id pp_expr expr
      else
        F.fprintf fmt "%a %a %a = %a" pp_dir dir pp_type typ pp_id id pp_expr
          expr
  | None ->
      if dir.it = No then F.fprintf fmt "%a %a" pp_type typ pp_id id
      else F.fprintf fmt "%a %a %a" pp_dir dir pp_type typ pp_id id

and pp_params fmt params =
  F.fprintf fmt "(%a)"
    (Format.pp_print_list ~pp_sep:(fun fmt () -> F.fprintf fmt ", ") pp_param)
    params

and pp_cparam fmt cparam = pp_param fmt cparam

and pp_cparams fmt cparams =
  match cparams with [] -> () | _ -> pp_params fmt cparams

(* Arguments *)

and pp_arg fmt arg =
  match arg.it with
  | ExprA expr -> pp_expr fmt expr
  | NameA (id, expr) -> F.fprintf fmt "%a = %a" pp_id id pp_expr expr
  | AnyA -> F.fprintf fmt "_"

and pp_args fmt args =
  F.fprintf fmt "(%a)"
    (Format.pp_print_list ~pp_sep:(fun fmt () -> F.fprintf fmt ", ") pp_arg)
    args

and pp_targs fmt typs =
  match typs with
  | [] -> ()
  | _ ->
      F.fprintf fmt "<%a>"
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> F.fprintf fmt ", ")
           pp_type)
        typs

(* Expressions *)

and pp_expr fmt expr =
  match expr.it with
  | BoolE b -> F.fprintf fmt "%b" b
  | StrE s -> F.fprintf fmt "\"%s\"" s
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
             F.fprintf fmt "%s = %a" name.it pp_expr expr))
        fields
  | UnE (unop, expr) -> F.fprintf fmt "%a%a" pp_unop unop pp_expr expr
  | BinE (binop, expr_left, expr_right) ->
      F.fprintf fmt "((%a) %a (%a))" pp_expr expr_left pp_binop binop pp_expr
        expr_right
  | TernE (expr_cond, expr_then, expr_else) ->
      F.fprintf fmt "((%a) ? (%a) : (%a))" pp_expr expr_cond pp_expr expr_then
        pp_expr expr_else
  | CastE (typ, expr) -> F.fprintf fmt "((%a) (%a))" pp_type typ pp_expr expr
  | MaskE (expr, mask) -> F.fprintf fmt "%a &&& %a" pp_expr expr pp_expr mask
  | RangeE (lo, hi) -> F.fprintf fmt "%a .. %a" pp_expr lo pp_expr hi
  | ArrAccE (expr, index) -> F.fprintf fmt "%a[%a]" pp_expr expr pp_expr index
  | BitAccE (expr, lo, hi) ->
      F.fprintf fmt "%a[%a:%a]" pp_expr expr pp_expr hi pp_expr lo
  | TypeAccE (var, field) -> F.fprintf fmt "%a.%s" pp_var var field.it
  | ErrAccE field -> F.fprintf fmt "error.%s" field.it
  | ExprAccE (expr, field) -> F.fprintf fmt "%a.%s" pp_expr expr field.it
  | CallE (expr, targs, args) ->
      F.fprintf fmt "%a%a%a" pp_expr expr pp_targs targs pp_args args
  | InstE (typ, args) -> F.fprintf fmt "%a%a" pp_type typ pp_args args

and pp_exprs fmt exprs =
  F.fprintf fmt "%a"
    (Format.pp_print_list ~pp_sep:(fun fmt () -> F.fprintf fmt ", ") pp_expr)
    exprs

(* Statements *)

let rec pp_stmt fmt (level, stmt) =
  match stmt.it with
  | EmptyI -> F.fprintf fmt "%s;" (indent level)
  | AssignI (lhs, rhs) ->
      F.fprintf fmt "%s%a = %a;" (indent level) pp_expr lhs pp_expr rhs
  | SwitchI (expr, switch_cases) ->
      F.fprintf fmt "%sswitch (%a) {\n%a\n%s}" (indent level) pp_expr expr
        pp_switch_cases
        (level + 1, switch_cases)
        (indent level)
  | IfI (expr, stmt_then, stmt_else) -> (
      match stmt_else.it with
      | EmptyI ->
          F.fprintf fmt "%sif (%a)\n%a" (indent level) pp_expr expr pp_stmt
            (level, stmt_then)
      | _ ->
          F.fprintf fmt "%sif (%a)\n%a\n%selse\n%a" (indent level) pp_expr expr
            pp_stmt (level, stmt_then) (indent level) pp_stmt (level, stmt_else)
      )
  | BlockI block -> pp_block fmt (level, block)
  | ExitI -> F.fprintf fmt "%sexit;" (indent level)
  | RetI expr -> (
      match expr with
      | Some expr -> F.fprintf fmt "%sreturn %a;" (indent level) pp_expr expr
      | None -> F.fprintf fmt "%sreturn;" (indent level))
  | CallI (expr, targs, args) ->
      F.fprintf fmt "%s%a%a%a;" (indent level) pp_expr expr pp_targs targs
        pp_args args
  | TransI trans -> F.fprintf fmt "%stransition %a;" (indent level) pp_id trans
  | SelectI (exprs, select_cases) ->
      F.fprintf fmt "%stransition select (%a) {\n%a\n%s}" (indent level)
        pp_exprs exprs pp_select_cases
        (level + 1, select_cases)
        (indent level)
  | DeclI decl -> pp_decl fmt (level, decl)

and pp_block fmt (level, block) =
  F.fprintf fmt "%s{\n%a\n%s}" (indent level)
    (Format.pp_print_list ~pp_sep:(fun fmt () -> F.fprintf fmt "\n") pp_stmt)
    (List.map (fun stmt -> (level + 1, stmt)) block.it)
    (indent level)

and pp_case fmt case =
  match case.it with
  | CaseC case | FallC case -> F.fprintf fmt "%s" case
  | DefaultC -> F.fprintf fmt "default"

and pp_switch_case fmt (level, switch_case) =
  let case, block = switch_case.it in
  match case.it with
  | FallC _ -> F.fprintf fmt "%s%a:" (indent level) pp_case case
  | _ ->
      F.fprintf fmt "%s%a:\n%a" (indent level) pp_case case pp_block
        (level + 1, block)

and pp_switch_cases fmt (level, switch_cases) =
  F.fprintf fmt "%a"
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> F.fprintf fmt "\n")
       pp_switch_case)
    (List.map (fun switch_case -> (level, switch_case)) switch_cases)

and pp_mtch fmt mtch =
  match mtch.it with
  | ExprM expr -> F.fprintf fmt "%a" pp_expr expr
  | DefaultM -> F.fprintf fmt "default"
  | AnyM -> F.fprintf fmt "_"

and pp_mtchs fmt mtchs =
  F.fprintf fmt
    (if List.length mtchs > 1 then "(%a)" else "%a")
    (Format.pp_print_list ~pp_sep:(fun fmt () -> F.fprintf fmt ", ") pp_mtch)
    mtchs

and pp_select_case fmt (level, select_case) =
  let mtchs, label = select_case.it in
  F.fprintf fmt "%s%a: %s;" (indent level) pp_mtchs mtchs label.it

and pp_select_cases fmt (level, select_cases) =
  F.fprintf fmt "%a"
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> F.fprintf fmt "\n")
       pp_select_case)
    (List.map (fun select_case -> (level, select_case)) select_cases)

(* Declarations *)

and pp_decl fmt (level, decl) =
  match decl.it with
  | ConstD { id; typ; value } ->
      F.fprintf fmt "%sconst %a %a = %a;" (indent level) pp_type typ pp_id id
        pp_expr value
  | VarD { id; typ; init } -> (
      match init with
      | Some init ->
          F.fprintf fmt "%s%a %a = %a;" (indent level) pp_type typ pp_id id
            pp_expr init
      | None -> F.fprintf fmt "%s%a %a;" (indent level) pp_type typ pp_id id)
  | InstD { id; typ; args; init } -> (
      match init with
      | Some block ->
          F.fprintf fmt "%s%a%a %a = %a;" (indent level) pp_type typ pp_args
            args pp_id id pp_block
            (level + 1, block)
      | None ->
          F.fprintf fmt "%s%a%a %a;" (indent level) pp_type typ pp_args args
            pp_id id)
  | ErrD { members } ->
      F.fprintf fmt "%serror {\n%a\n%s}" (indent level) pp_members
        (level + 1, members)
        (indent level)
  | MatchKindD { members } ->
      F.fprintf fmt "%smatch_kind {\n%a\n%s}" (indent level) pp_members
        (level + 1, members)
        (indent level)
  | StructD { id; fields } ->
      F.fprintf fmt "%sstruct %a {\n%a\n%s}" (indent level) pp_id id
        pp_struct_fields
        (level + 1, fields)
        (indent level)
  | HeaderD { id; fields } ->
      F.fprintf fmt "%sheader %a {\n%a\n%s}" (indent level) pp_id id
        pp_struct_fields
        (level + 1, fields)
        (indent level)
  | UnionD { id; fields } ->
      F.fprintf fmt "%sheader_union %a {\n%a\n%s}" (indent level) pp_id id
        pp_struct_fields
        (level + 1, fields)
        (indent level)
  | EnumD { id; members } ->
      F.fprintf fmt "%senum %a {\n%a\n%s}" (indent level) pp_id id pp_members
        (level + 1, members)
        (indent level)
  | SEnumD { id; typ; fields } ->
      F.fprintf fmt "%senum %a %a {\n%a\n%s}" (indent level) pp_type typ pp_id
        id pp_serial_fields
        (level + 1, fields)
        (indent level)
  | NewTypeD { id; typdef } -> (
      match typdef with
      | Left typ ->
          F.fprintf fmt "%stype %a %a;" (indent level) pp_type typ pp_id id
      | Right decl ->
          F.fprintf fmt "%stype %a %a;" (indent level) pp_decl
            (level + 1, decl)
            pp_id id)
  | TypeDefD { id; typdef } -> (
      match typdef with
      | Left typ ->
          F.fprintf fmt "%stypedef %a %a;" (indent level) pp_type typ pp_id id
      | Right decl ->
          F.fprintf fmt "%stypedef %a %a;" (indent level) pp_decl
            (level + 1, decl)
            pp_id id)
  | ValueSetD { id; typ; size } ->
      F.fprintf fmt "%svalue_set<%a>(%a) %a;" (indent level) pp_type typ pp_expr
        size pp_id id
  | ParserTypeD { id; tparams; params } ->
      F.fprintf fmt "%sparser %a%a%a;" (indent level) pp_id id pp_tparams
        tparams pp_params params
  | ParserD { id; tparams; params; cparams; locals; states } ->
      F.fprintf fmt "%sparser %a%a%a%a {\n%a\n%a\n%s}" (indent level) pp_id id
        pp_tparams tparams pp_params params pp_cparams cparams pp_decls
        (level + 1, locals)
        pp_parser_states
        (level + 1, states)
        (indent level)
  | ActionD { id; params; body } ->
      F.fprintf fmt "%saction %a%a\n%a" (indent level) pp_id id pp_params params
        pp_block (level, body)
  | TableD { id; table } ->
      F.fprintf fmt "%stable %a %a" (indent level) pp_id id pp_table
        (level, table)
  | ControlTypeD { id; tparams; params } ->
      F.fprintf fmt "%scontrol %a%a%a;" (indent level) pp_id id pp_tparams
        tparams pp_params params
  | ControlD { id; tparams; params; cparams; locals; body } ->
      F.fprintf fmt "%scontrol %a%a%a%a {\n%a\n%sapply\n%a\n%s}" (indent level)
        pp_id id pp_tparams tparams pp_params params pp_cparams cparams pp_decls
        (level + 1, locals)
        (indent (level + 1))
        pp_block
        (level + 1, body)
        (indent level)
  | FuncD { id; typ_ret; tparams; params; body } ->
      F.fprintf fmt "%s%a %a%a%a\n%a" (indent level) pp_type typ_ret pp_id id
        pp_tparams tparams pp_params params pp_block (level, body)
  | ExtFuncD { id; typ_ret; tparams; params } ->
      F.fprintf fmt "%sextern %a %a%a%a;" (indent level) pp_type typ_ret pp_id
        id pp_tparams tparams pp_params params
  | ExtConstructorD { id; cparams } ->
      F.fprintf fmt "%s %a%a;" (indent level) pp_id id pp_params cparams
  | ExtAbstractMethodD { id; typ_ret; tparams; params } ->
      F.fprintf fmt "%sabstract %a %a%a%a;" (indent level) pp_type typ_ret pp_id
        id pp_tparams tparams pp_params params
  | ExtMethodD { id; typ_ret; tparams; params } ->
      F.fprintf fmt "%s%a %a%a%a;" (indent level) pp_type typ_ret pp_id id
        pp_tparams tparams pp_params params
  | ExtObjectD { id; tparams; mthds } ->
      F.fprintf fmt "%sextern %a%a {\n%a\n%s}" (indent level) pp_id id
        pp_tparams tparams pp_decls
        (level + 1, mthds)
        (indent level)
  | PackageTypeD { id; tparams; cparams } ->
      F.fprintf fmt "%spackage %a%a%a;" (indent level) pp_id id pp_tparams
        tparams pp_params cparams

and pp_decls fmt (indent, decls) =
  F.fprintf fmt "%a"
    (Format.pp_print_list ~pp_sep:(fun fmt () -> F.fprintf fmt "\n") pp_decl)
    (List.map (fun decl -> (indent, decl)) decls)

and pp_struct_fields fmt (level, fields) =
  let pp_struct_field fmt (level, field) =
    let id, typ = field in
    F.fprintf fmt "%s%a %a;" (indent level) pp_type typ pp_id id
  in
  F.fprintf fmt "%a"
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> F.fprintf fmt "\n")
       pp_struct_field)
    (List.map (fun field -> (level, field)) fields)

and pp_serial_fields fmt (level, fields) =
  let pp_serial_field fmt (level, field) =
    let id, expr = field in
    F.fprintf fmt "%s%a = %a" (indent level) pp_id id pp_expr expr
  in
  F.fprintf fmt "%a"
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> F.fprintf fmt ",\n")
       pp_serial_field)
    (List.map (fun field -> (level, field)) fields)

and pp_members fmt (level, members) =
  let pp_member fmt (level, member) =
    F.fprintf fmt "%s%a" (indent level) pp_member member
  in
  F.fprintf fmt "%a"
    (Format.pp_print_list ~pp_sep:(fun fmt () -> F.fprintf fmt ",\n") pp_member)
    (List.map (fun member -> (level, member)) members)

and pp_parser_state fmt (level, parser_state) =
  let label, block = parser_state.it in
  F.fprintf fmt "%sstate %s\n%a" (indent level) label.it pp_block (level, block)

and pp_parser_states fmt (level, parser_states) =
  F.fprintf fmt "%a"
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> F.fprintf fmt "\n")
       pp_parser_state)
    (List.map (fun state -> (level, state)) parser_states)

and pp_table_key fmt (level, table_key) =
  let expr, mtch_kind = table_key.it in
  F.fprintf fmt "%s%a : %a;" (indent level) pp_expr expr pp_mtch_kind mtch_kind

and pp_table_keys fmt (level, table_keys) =
  match table_keys with
  | [] -> ()
  | _ ->
      F.fprintf fmt "%skey = {\n%a\n%s}" (indent level)
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> F.fprintf fmt "\n")
           pp_table_key)
        (List.map (fun key -> (level + 1, key)) table_keys)
        (indent level)

and pp_table_action fmt (level, table_action) =
  let var, args = table_action.it in
  match args with
  | [] -> F.fprintf fmt "%s%a;" (indent level) pp_var var
  | _ -> F.fprintf fmt "%s%a%a;" (indent level) pp_var var pp_args args

and pp_table_actions fmt (level, table_actions) =
  match table_actions with
  | [] -> ()
  | _ ->
      F.fprintf fmt "%sactions = {\n%a\n%s}" (indent level)
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> F.fprintf fmt "\n")
           pp_table_action)
        (List.map (fun action -> (level + 1, action)) table_actions)
        (indent level)

and pp_table_entry fmt (level, table_entry) =
  let mtchs, table_action = table_entry.it in
  F.fprintf fmt "%s%a : %a" (indent level) pp_mtchs mtchs pp_table_action
    (level + 1, table_action)

and pp_table_entries fmt (level, table_entries) =
  match table_entries with
  | [] -> ()
  | _ ->
      F.fprintf fmt "%sconst entries = {\n%a\n%s}" (indent level)
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> F.fprintf fmt "\n")
           pp_table_entry)
        (List.map (fun entry -> (level + 1, entry)) table_entries)
        (indent level)

and pp_table_default fmt (level, table_default) =
  let table_action, const = table_default.it in
  F.fprintf fmt "%s%sdefault_action = %a" (indent level)
    (if const then "const " else "")
    pp_table_action
    (level + 1, table_action)

and pp_table_custom fmt (level, table_custom) =
  let id, expr, const = table_custom.it in
  F.fprintf fmt "%s%s %a = %a;" (indent level)
    (if const then " const" else "")
    pp_id id pp_expr expr

and pp_table_customs fmt (level, table_customs) =
  F.fprintf fmt "%a"
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> F.fprintf fmt "\n")
       pp_table_custom)
    (List.map (fun custom -> (level, custom)) table_customs)

and pp_table fmt (level, table) =
  let key, actions, entries, default, custom = table in
  match default with
  | Some default ->
      F.fprintf fmt "{\n%a\n%a\n%a\n%a\n%a\n%s}" pp_table_keys
        (level + 1, key)
        pp_table_actions
        (level + 1, actions)
        pp_table_entries
        (level + 1, entries)
        pp_table_default
        (level + 1, default)
        pp_table_customs
        (level + 1, custom)
        (indent level)
  | None ->
      F.fprintf fmt "{\n%a\n%a\n%a\n%a\n%s}" pp_table_keys
        (level + 1, key)
        pp_table_actions
        (level + 1, actions)
        pp_table_entries
        (level + 1, entries)
        pp_table_customs
        (level + 1, custom)
        (indent level)

(* Program *)

let pp_program fmt program =
  F.fprintf fmt "%a\n"
    (Format.pp_print_list ~pp_sep:(fun fmt () -> F.fprintf fmt "\n") pp_decl)
    (List.map (fun decl -> (0, decl)) program)
