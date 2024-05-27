open Ast
module F = Format

let indent level = String.make (2 * level) ' '

(* Variable *)

let print_var fmt var =
  match var with
  | Bare var -> F.fprintf fmt "%s" var
  | Top var -> F.fprintf fmt ".%s" var

(* Unary and binary operators *)

let print_unop fmt unop =
  match unop with
  | BNotOp -> F.fprintf fmt "~"
  | LNotOp -> F.fprintf fmt "!"
  | UMinusOp -> F.fprintf fmt "-"

let print_binop fmt binop =
  match binop with
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

let rec print_type fmt typ =
  match typ with
  | VoidT -> F.fprintf fmt "void"
  | BoolT -> F.fprintf fmt "bool"
  | MatchKindT -> F.fprintf fmt "match_kind"
  | ErrT -> F.fprintf fmt "error"
  | StrT -> F.fprintf fmt "string"
  | AIntT -> F.fprintf fmt "int"
  | IntT expr -> F.fprintf fmt "int<%a>" print_expr expr
  | BitT expr -> F.fprintf fmt "bit<%a>" print_expr expr
  | VBitT expr -> F.fprintf fmt "varbit<%a>" print_expr expr
  | NameT var -> print_var fmt var
  | SpecT (var, typs) ->
      F.fprintf fmt "%a<%a>" print_var var
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> F.fprintf fmt ", ")
           print_type)
        typs
  | StackT (typ, expr) -> F.fprintf fmt "%a[%a]" print_type typ print_expr expr
  | TupleT typs ->
      F.fprintf fmt "(%a)"
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> F.fprintf fmt ", ")
           print_type)
        typs
  | AnyT -> F.fprintf fmt "_"

(* Parameters and Arguments *)

and print_dir fmt dir =
  match dir with
  | No -> ()
  | In -> F.fprintf fmt "in"
  | Out -> F.fprintf fmt "out"
  | InOut -> F.fprintf fmt "inout"

and print_param fmt param =
  let name, dir, typ, default = param in
  match default with
  | Some expr ->
      F.fprintf fmt "%a %a %s = %a" print_dir dir print_type typ name print_expr
        expr
  | None -> F.fprintf fmt "%a %a %s" print_dir dir print_type typ name

and print_params fmt params =
  F.fprintf fmt "(%a)"
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> F.fprintf fmt ", ")
       print_param)
    params

and print_cparams fmt cparams =
  match cparams with [] -> () | _ -> print_params fmt cparams

and print_tparams fmt tparams =
  match tparams with
  | [] -> ()
  | _ ->
      F.fprintf fmt "<%a>"
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> F.fprintf fmt ", ")
           (fun fmt tparam -> F.fprintf fmt "%s" tparam))
        tparams

and print_arg fmt arg =
  match arg with
  | ExprA expr -> print_expr fmt expr
  | NameA (name, expr) -> F.fprintf fmt "%s = %a" name print_expr expr
  | AnyA -> F.fprintf fmt "_"

and print_args fmt args =
  F.fprintf fmt "(%a)"
    (Format.pp_print_list ~pp_sep:(fun fmt () -> F.fprintf fmt ", ") print_arg)
    args

and print_targs fmt typs =
  match typs with
  | [] -> ()
  | _ ->
      F.fprintf fmt "<%a>"
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> F.fprintf fmt ", ")
           print_type)
        typs

(* Expressions *)

and print_expr fmt expr =
  match expr with
  | BoolE b -> F.fprintf fmt "%b" b
  | StrE s -> F.fprintf fmt "\"%s\"" s
  | NumE (i, width_signed) -> (
      match width_signed with
      | Some (width, signed) ->
          F.fprintf fmt "%s%s%s" (Bigint.to_string width)
            (if signed then "s" else "w")
            (Bigint.to_string i)
      | None -> F.fprintf fmt "%s" (Bigint.to_string i))
  | VarE var -> print_var fmt var
  | ListE exprs -> (
      match exprs with
      | [] -> ()
      | _ ->
          F.fprintf fmt "{ %a }"
            (Format.pp_print_list
               ~pp_sep:(fun fmt () -> F.fprintf fmt ", ")
               print_expr)
            exprs)
  | RecordE fields ->
      F.fprintf fmt "{ %a }"
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> F.fprintf fmt ", ")
           (fun fmt (name, expr) ->
             F.fprintf fmt "%s = %a" name print_expr expr))
        fields
  | UnE (unop, expr) -> F.fprintf fmt "%a%a" print_unop unop print_expr expr
  | BinE (binop, expr_left, expr_right) ->
      F.fprintf fmt "%a %a %a" print_expr expr_left print_binop binop print_expr
        expr_right
  | TernE (expr_cond, expr_then, expr_else) ->
      F.fprintf fmt "%a ? %a : %a" print_expr expr_cond print_expr expr_then
        print_expr expr_else
  | CastE (typ, expr) -> F.fprintf fmt "(%a) %a" print_type typ print_expr expr
  | MaskE (expr, mask) ->
      F.fprintf fmt "%a & %a" print_expr expr print_expr mask
  | RangeE (lo, hi) -> F.fprintf fmt "%a .. %a" print_expr lo print_expr hi
  | ArrAccE (expr, index) ->
      F.fprintf fmt "%a[%a]" print_expr expr print_expr index
  | BitAccE (expr, lo, hi) ->
      F.fprintf fmt "%a[%a:%a]" print_expr expr print_expr lo print_expr hi
  | TypeAccE (var, member) -> F.fprintf fmt "%a.%s" print_var var member
  | ErrAccE member -> F.fprintf fmt "error.%s" member
  | ExprAccE (expr, member) -> F.fprintf fmt "%a.%s" print_expr expr member
  | CallE (expr, targs, args) ->
      F.fprintf fmt "%a%a%a" print_expr expr print_targs targs print_args args
  | InstE (typ, args) -> F.fprintf fmt "%a%a" print_type typ print_args args

and print_exprs fmt exprs =
  F.fprintf fmt "%a"
    (Format.pp_print_list ~pp_sep:(fun fmt () -> F.fprintf fmt ", ") print_expr)
    exprs

(* Statements *)

let rec print_stmt fmt (level, stmt) =
  match stmt with
  | EmptyI -> F.fprintf fmt "%s{}" (indent level)
  | AssignI (lhs, rhs) ->
      F.fprintf fmt "%s%a = %a;" (indent level) print_expr lhs print_expr rhs
  | SwitchI (expr, switch_cases) ->
      F.fprintf fmt "%sswitch (%a) {\n%a\n%s}" (indent level) print_expr expr
        print_switch_cases
        (level + 1, switch_cases)
        (indent level)
  | IfI (expr, stmt_then, stmt_else) ->
      F.fprintf fmt "%sif (%a)\n%a\n%selse\n%a" (indent level) print_expr expr
        print_stmt (level, stmt_then) (indent level) print_stmt
        (level, stmt_else)
  | BlockI block -> print_block fmt (level, block)
  | ExitI -> F.fprintf fmt "%sexit;" (indent level)
  | RetI expr -> (
      match expr with
      | Some expr -> F.fprintf fmt "%sreturn %a;" (indent level) print_expr expr
      | None -> F.fprintf fmt "%sreturn;" (indent level))
  | CallI (expr, targs, args) ->
      F.fprintf fmt "%s%a%a%a;" (indent level) print_expr expr print_targs targs
        print_args args
  | TransI trans -> F.fprintf fmt "%stransition %s;" (indent level) trans
  | SelectI (exprs, select_cases) ->
      F.fprintf fmt "%sselect (%a) {\n%a\n%s}" (indent level) print_exprs exprs
        print_select_cases
        (level + 1, select_cases)
        (indent level)
  | DeclI decl -> print_decl fmt (level, decl)

and print_block fmt (level, block) =
  F.fprintf fmt "%s{\n%a\n%s}" (indent level)
    (Format.pp_print_list ~pp_sep:(fun fmt () -> F.fprintf fmt "\n") print_stmt)
    (List.map (fun stmt -> (level + 1, stmt)) block)
    (indent level)

and print_case fmt case =
  match case with
  | CaseC case -> F.fprintf fmt "case %s" case
  | DefaultC -> F.fprintf fmt "default"

and print_switch_case fmt (level, switch_case) =
  let case, block = switch_case in
  F.fprintf fmt "%s%a:\n%a" (indent level) print_case case print_block
    (level + 1, block)

and print_switch_cases fmt (level, switch_cases) =
  F.fprintf fmt "%a"
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> F.fprintf fmt "\n")
       print_switch_case)
    (List.map (fun switch_case -> (level, switch_case)) switch_cases)

and print_mtch fmt mtch =
  match mtch with
  | ExprM expr -> F.fprintf fmt "%a" print_expr expr
  | DefaultM -> F.fprintf fmt "default"
  | AnyM -> F.fprintf fmt "_"

and print_mtchs fmt mtchs =
  F.fprintf fmt "%a"
    (Format.pp_print_list ~pp_sep:(fun fmt () -> F.fprintf fmt ", ") print_mtch)
    mtchs

and print_select_case fmt (level, select_case) =
  let mtchs, trans = select_case in
  F.fprintf fmt "%s%a: %s" (indent level) print_mtchs mtchs trans

and print_select_cases fmt (level, select_cases) =
  F.fprintf fmt "%a"
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> F.fprintf fmt ";\n")
       print_select_case)
    (List.map (fun select_case -> (level, select_case)) select_cases)

(* Declarations *)

and print_decl fmt (level, decl) =
  match decl with
  | ConstD { name; typ; value } ->
      F.fprintf fmt "%sconst %a %s = %a;" (indent level) print_type typ name
        print_expr value
  | VarD { name; typ; init } -> (
      match init with
      | Some init ->
          F.fprintf fmt "%s%a %s = %a;" (indent level) print_type typ name
            print_expr init
      | None -> F.fprintf fmt "%s%a %s;" (indent level) print_type typ name)
  | InstD { name; typ; args; init } -> (
      match init with
      | Some block ->
          F.fprintf fmt "%s%a %s = %a;" (indent level) print_type typ name
            print_block
            (level + 1, block)
      | None ->
          F.fprintf fmt "%s%a %s%a;" (indent level) print_type typ name
            print_args args)
  | ErrD { members } ->
      F.fprintf fmt "%serror {\n%a\n%s}" (indent level) print_members
        (level + 1, members)
        (indent level)
  | MatchKindD { members } ->
      F.fprintf fmt "%smatch_kind {\n%a\n%s}" (indent level) print_members
        (level + 1, members)
        (indent level)
  | StructD { name; fields } ->
      F.fprintf fmt "%sstruct %s {\n%a\n%s}" (indent level) name print_fields
        (level + 1, fields)
        (indent level)
  | HeaderD { name; fields } ->
      F.fprintf fmt "%sheader %s {\n%a\n%s}" (indent level) name print_fields
        (level + 1, fields)
        (indent level)
  | UnionD { name; fields } ->
      F.fprintf fmt "%sunion %s {\n%a\n%s}" (indent level) name print_fields
        (level + 1, fields)
        (indent level)
  | EnumD { name; members } ->
      F.fprintf fmt "%senum %s {\n%a\n%s}" (indent level) name print_members
        (level + 1, members)
        (indent level)
  | SEnumD { name; typ; members } ->
      F.fprintf fmt "%senum %a %s {\n%a\n%s}" (indent level) print_type typ name
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> F.fprintf fmt "; ")
           (fun fmt (name, expr) ->
             F.fprintf fmt "%s%s %a" (indent (level + 1)) name print_expr expr))
        members (indent level)
  | NewTypeD { name; typ; decl } -> (
      match (typ, decl) with
      | Some typ, None ->
          F.fprintf fmt "%stype %a %s;" (indent level) print_type typ name
      | None, Some decl ->
          F.fprintf fmt "%stype %a %s;" (indent level) print_decl
            (level + 1, decl)
            name
      | _ -> assert false)
  | TypeDefD { name; typ; decl } -> (
      match (typ, decl) with
      | Some typ, None ->
          F.fprintf fmt "%stypedef %a %s;" (indent level) print_type typ name
      | None, Some decl ->
          F.fprintf fmt "%stypedef %a %s;" (indent level) print_decl
            (level + 1, decl)
            name
      | _ -> assert false)
  | ValueSetD { name; typ; size } ->
      F.fprintf fmt "%svalue_set<%a>(%a) %s;" (indent level) print_type typ
        print_expr size name
  | ParserTypeD { name; tparams; params } ->
      F.fprintf fmt "%sparser %s%a%a;" (indent level) name print_tparams tparams
        print_params params
  | ParserD { name; tparams; params; cparams; locals; states } ->
      F.fprintf fmt "%sparser %s%a%a%a {\n%a\n%a\n%s}" (indent level) name
        print_tparams tparams print_params params print_cparams cparams
        print_decls
        (level + 1, locals)
        print_parser_states
        (level + 1, states)
        (indent level)
  | ActionD { name; params; body } ->
      F.fprintf fmt "%saction %s%a\n%a" (indent level) name print_params params
        print_block (level, body)
  | TableD { name; key; actions; entries; default; custom } ->
      F.fprintf fmt "%stable %s {\n%a\n%a\n%a\n%a\n%a\n%s}" (indent level) name
        print_table_keys
        (level + 1, key)
        print_table_actions
        (level + 1, actions)
        print_table_entries
        (level + 1, entries)
        print_table_default
        (level + 1, default)
        print_table_customs
        (level + 1, custom)
        (indent level)
  | ControlTypeD { name; tparams; params } ->
      F.fprintf fmt "%scontrol %s%a%a;" (indent level) name print_tparams
        tparams print_params params
  | ControlD { name; tparams; params; cparams; locals; body } ->
      F.fprintf fmt "%scontrol %s%a%a%a {\n%a\n%sapply\n%a\n%s}" (indent level)
        name print_tparams tparams print_params params print_cparams cparams
        print_decls
        (level + 1, locals)
        (indent (level + 1))
        print_block
        (level + 1, body)
        (indent level)
  | FuncD { name; rettyp; tparams; params; body } ->
      F.fprintf fmt "%s%a %s%a%a\n%a" (indent level) print_type rettyp name
        print_tparams tparams print_params params print_block (level, body)
  | ExternFuncD { name; rettyp; tparams; params } ->
      F.fprintf fmt "%sextern %a %s%a%a;" (indent level) print_type rettyp name
        print_tparams tparams print_params params
  | ConsD { name; cparams } ->
      F.fprintf fmt "%s %s%a;" (indent level) name print_params cparams
  | AbstractD { name; rettyp; tparams; params } ->
      F.fprintf fmt "%sabstract %a %s%a%a;" (indent level) print_type rettyp
        name print_tparams tparams print_params params
  | MethodD { name; rettyp; tparams; params } ->
      F.fprintf fmt "%s%a %s%a%a;" (indent level) print_type rettyp name
        print_tparams tparams print_params params
  | ExternObjectD { name; tparams; mthds } ->
      F.fprintf fmt "%sextern object %s%a {\n%a\n%s}" (indent level) name
        print_tparams tparams print_decls
        (level + 1, mthds)
        (indent level)
  | PackageTypeD { name; tparams; cparams } ->
      F.fprintf fmt "%spackage %s%a%a;" (indent level) name print_tparams
        tparams print_params cparams

and print_decls fmt (indent, decls) =
  F.fprintf fmt "%a"
    (Format.pp_print_list ~pp_sep:(fun fmt () -> F.fprintf fmt "\n") print_decl)
    (List.map (fun decl -> (indent, decl)) decls)

and print_member fmt (level, member) =
  F.fprintf fmt "%s%s" (indent level) member

and print_members fmt (level, members) =
  F.fprintf fmt "%a"
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> F.fprintf fmt "\n")
       print_member)
    (List.map (fun member -> (level, member)) members)

and print_field fmt (level, field) =
  let name, typ = field in
  F.fprintf fmt "%s%a %s" (indent level) print_type typ name

and print_fields fmt (level, fields) =
  F.fprintf fmt "%a"
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> F.fprintf fmt "\n")
       print_field)
    (List.map (fun field -> (level, field)) fields)

and print_parser_state fmt (level, parser_state) =
  let name, block = parser_state in
  F.fprintf fmt "%sstate %s\n%a" (indent level) name print_block (level, block)

and print_parser_states fmt (level, parser_states) =
  F.fprintf fmt "%a"
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> F.fprintf fmt "\n")
       print_parser_state)
    (List.map (fun state -> (level, state)) parser_states)

and print_table_key fmt (level, table_key) =
  let expr, name = table_key in
  F.fprintf fmt "%s%a : %s" (indent level) print_expr expr name

and print_table_keys fmt (level, table_keys) =
  match table_keys with
  | [] -> ()
  | _ ->
      F.fprintf fmt "%skey {\n%a\n%s}" (indent level)
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> F.fprintf fmt "\n")
           print_table_key)
        (List.map (fun key -> (level + 1, key)) table_keys)
        (indent level)

and print_table_action fmt (level, table_action) =
  let name, args = table_action in
  F.fprintf fmt "%s%a%a" (indent level) print_var name print_args args

and print_table_actions fmt (level, table_actions) =
  match table_actions with
  | [] -> ()
  | _ ->
      F.fprintf fmt "%sactions = {\n%a\n%s}" (indent level)
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> F.fprintf fmt "\n")
           print_table_action)
        (List.map (fun action -> (level + 1, action)) table_actions)
        (indent level)

and print_table_entry fmt (level, table_entry) =
  let mtchs, table_action = table_entry in
  F.fprintf fmt "%s%a : %a" (indent level) print_mtchs mtchs print_table_action
    (level + 1, table_action)

and print_table_entries fmt (level, table_entries) =
  match table_entries with
  | [] -> ()
  | _ ->
      F.fprintf fmt "%sentries {\n%a\n%s}" (indent level)
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> F.fprintf fmt "\n")
           print_table_entry)
        (List.map (fun entry -> (level + 1, entry)) table_entries)
        (indent level)

and print_table_default fmt (level, table_default) =
  match table_default with
  | None -> ()
  | Some table_default ->
      let table_action, const = table_default in
      F.fprintf fmt "%s%sdefault_action =\n%a;" (indent level)
        (if const then " const" else "")
        print_table_action
        (level + 1, table_action)

and print_table_custom fmt (level, table_custom) =
  let name, expr, const = table_custom in
  F.fprintf fmt "%s%s %s = %a" (indent level)
    (if const then " const" else "")
    name print_expr expr

and print_table_customs fmt (level, table_customs) =
  F.fprintf fmt "%a"
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> F.fprintf fmt "\n")
       print_table_custom)
    (List.map (fun custom -> (level, custom)) table_customs)

(* Program *)

let print_program fmt program =
  F.fprintf fmt "%a\n"
    (Format.pp_print_list ~pp_sep:(fun fmt () -> F.fprintf fmt "\n") print_decl)
    (List.map (fun decl -> (0, decl)) program)
