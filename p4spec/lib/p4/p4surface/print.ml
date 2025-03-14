open Ast

(* Basics *)

let print_indent (indent : int) = String.make (indent * 2) ' '
let print_text (text : Text.t) = text.str

let print_number (number : Number.t) =
  let svalue = Bigint.to_string number.value in
  match number.width_signed with
  | None -> svalue
  | Some (width, signed) ->
      let swidth = Bigint.to_string width in
      let ssigned = if signed then "s" else "w" in
      Printf.sprintf "%s%s%s" swidth ssigned svalue

let print_name (name : Name.t) =
  match name with
  | BareName name -> print_text name
  | QualifiedName (prefix, name) ->
      let sprefix = List.map print_text prefix |> String.concat "." in
      let sname = print_text name in
      Printf.sprintf "%s.%s" sprefix sname

let print_direction (dir : Direction.t) =
  match dir with In _ -> "in" | Out _ -> "out" | InOut _ -> "inout"

(* Operators *)

let print_unop (unop : Op.un) =
  match unop with
  | Not _ -> "!"
  | BitNot _ -> "~"
  | UPlus _ -> "+"
  | UMinus _ -> "-"

let print_binop (binop : Op.bin) =
  match binop with
  | Plus _ -> "+"
  | PlusSat _ -> "|+|"
  | Minus _ -> "-"
  | MinusSat _ -> "|-|"
  | Mul _ -> "*"
  | Div _ -> "/"
  | Mod _ -> "%"
  | Shl _ -> "<<"
  | Shr _ -> ">>"
  | Le _ -> "<="
  | Ge _ -> ">="
  | Lt _ -> "<"
  | Gt _ -> ">"
  | Eq _ -> "=="
  | NotEq _ -> "!="
  | BitAnd _ -> "&"
  | BitXor _ -> "^"
  | BitOr _ -> "|"
  | PlusPlus _ -> "++"
  | And _ -> "&&"
  | Or _ -> "||"

(* Types *)

let rec print_type (typ : Type.t) =
  match typ with
  | Bool _ -> "bool"
  | MatchKind _ -> "match_kind"
  | Error _ -> "error"
  | Integer _ -> "int"
  | IntType { expr; _ } ->
      let sexpr = print_expr expr in
      let sexpr = match expr with Int _ -> sexpr | _ -> "(" ^ sexpr ^ ")" in
      Printf.sprintf "int<%s>" sexpr
  | BitType { expr; _ } ->
      let sexpr = print_expr expr in
      let sexpr = match expr with Int _ -> sexpr | _ -> "(" ^ sexpr ^ ")" in
      Printf.sprintf "bit<%s>" sexpr
  | VarBit { expr; _ } ->
      let sexpr = print_expr expr in
      let sexpr = match expr with Int _ -> sexpr | _ -> "(" ^ sexpr ^ ")" in
      Printf.sprintf "varbit<%s>" sexpr
  | TypeName { name; _ } -> print_name name
  | SpecializedType { base; args; _ } ->
      let sbase = print_type base in
      let sargs = List.map print_type args |> String.concat ", " in
      let sargs = if List.length args > 0 then "<" ^ sargs ^ ">" else "" in
      Printf.sprintf "%s%s" sbase sargs
  | HeaderStack { header; size; _ } ->
      let sheader = print_type header in
      let ssize = print_expr size in
      Printf.sprintf "%s[%s]" sheader ssize
  | List { typ; _ } ->
      let styp = print_type typ in
      Printf.sprintf "list<%s>" styp
  | Tuple { args; _ } ->
      let sargs = List.map print_type args |> String.concat ", " in
      Printf.sprintf "tuple<%s>" sargs
  | String _ -> "string"
  | Void _ -> "void"
  | DontCare _ -> "_"

(* Arguments and Prameters *)

and print_arg (arg : Argument.t) =
  match arg with
  | Expression { value; _ } -> print_expr value
  | KeyValue { key; value; _ } ->
      let skey = print_text key in
      let svalue =
        match value with Some value -> print_expr value | None -> "_"
      in
      Printf.sprintf "%s = %s" skey svalue
  | Missing _ -> "_"

and print_param (param : Parameter.t) =
  let sdirection =
    match param.direction with
    | None -> ""
    | Some direction -> print_direction direction ^ " "
  in
  let styp = print_type param.typ in
  let svariable = print_text param.variable in
  let svalue =
    match param.opt_value with
    | None -> ""
    | Some value ->
        let svalue = print_expr value in
        Printf.sprintf " = %s" svalue
  in
  Printf.sprintf "%s%s %s%s" sdirection styp svariable svalue

(* Expressions *)

and print_key_value (key_value : KeyValue.t) =
  let skey = print_text key_value.key in
  let svalue = print_expr key_value.value in
  Printf.sprintf "%s = %s" skey svalue

and print_expr (expr : Expression.t) =
  match expr with
  | True _ -> "true"
  | False _ -> "false"
  | Int { i; _ } -> print_number i
  | String { text; _ } -> "\"" ^ print_text text ^ "\""
  | Name { name; _ } -> print_name name
  | Dots _ -> "..."
  | ArrayAccess { array; index; _ } ->
      let sarray = print_expr array in
      let sindex = print_expr index in
      Printf.sprintf "%s[%s]" sarray sindex
  | BitStringAccess { bits; lo; hi; _ } ->
      let sbits = print_expr bits in
      let slo = print_expr lo in
      let shi = print_expr hi in
      Printf.sprintf "%s[%s:%s]" sbits shi slo
  | List { values; _ } ->
      let svalues = List.map print_expr values |> String.concat ", " in
      Printf.sprintf "{ %s }" svalues
  | ListDots { values; _ } ->
      let svalues = List.map print_expr values |> String.concat ", " in
      Printf.sprintf "{ %s, ... }" svalues
  | Record { entries; _ } ->
      let sentries = List.map print_key_value entries |> String.concat ", " in
      Printf.sprintf "{ %s }" sentries
  | RecordDots { entries; _ } ->
      let sentries = List.map print_key_value entries |> String.concat ", " in
      Printf.sprintf "{ %s, ... }" sentries
  | Invalid _ -> "{#}"
  | UnaryOp { op; arg; _ } ->
      let sop = print_unop op in
      let sarg = print_expr arg in
      Printf.sprintf "%s%s" sop sarg
  | BinaryOp { op; args; _ } ->
      let sop = print_binop op in
      let arg1, arg2 = args in
      let sarg1 = print_expr arg1 in
      let sarg2 = print_expr arg2 in
      Printf.sprintf "((%s) %s (%s))" sarg1 sop sarg2
  | Cast { typ; expr; _ } ->
      let styp = print_type typ in
      let sexpr = print_expr expr in
      Printf.sprintf "((%s) (%s))" styp sexpr
  | TypeMember { typ; name; _ } ->
      let styp = print_name typ in
      let sname = print_text name in
      Printf.sprintf "%s.%s" styp sname
  | ErrorMember { err; _ } ->
      let serr = print_text err in
      Printf.sprintf "error.%s" serr
  | ExpressionMember { expr; name; _ } ->
      let sexpr = print_expr expr in
      let sname = print_text name in
      Printf.sprintf "%s.%s" sexpr sname
  | Ternary { cond; tru; fls; _ } ->
      let scond = print_expr cond in
      let stru = print_expr tru in
      let sfls = print_expr fls in
      Printf.sprintf "((%s) ? (%s) : (%s))" scond stru sfls
  | FunctionCall { func; type_args; args; _ } ->
      let sfunc = print_expr func in
      let stype_args = List.map print_type type_args |> String.concat ", " in
      let stype_args =
        if List.length type_args > 0 then "<" ^ stype_args ^ ">" else ""
      in
      let sargs = List.map print_arg args |> String.concat ", " in
      Printf.sprintf "%s%s(%s)" sfunc stype_args sargs
  | NamelessInstantiation { typ; args; _ } ->
      let styp = print_type typ in
      let sargs = List.map print_arg args |> String.concat ", " in
      Printf.sprintf "%s(%s)" styp sargs
  | Mask { expr; mask; _ } ->
      let sexpr = print_expr expr in
      let smask = print_expr mask in
      Printf.sprintf "%s &&& %s" sexpr smask
  | Range { lo; hi; _ } ->
      let slo = print_expr lo in
      let shi = print_expr hi in
      Printf.sprintf "%s .. %s" slo shi

(* Statements *)

and print_block ?(prefix = "") (indent : int) (block : Block.t) =
  let sstmts =
    List.map (print_stmt (indent + 1)) block.statements |> String.concat ""
  in
  Printf.sprintf "%s{\n%s%s}\n" prefix sstmts (print_indent indent)

and print_switch_label (label : Statement.switch_label) =
  match label with
  | Default _ -> "default"
  | Expression { expr; _ } -> print_expr expr

and print_switch_case (indent : int) (case : Statement.switch_case) =
  match case with
  | Action { label; code; _ } ->
      let slabel = print_switch_label label in
      let scode = print_block indent code in
      Printf.sprintf "%s%s: %s" (print_indent indent) slabel scode
  | FallThrough { label; _ } ->
      let slabel = print_switch_label label in
      Printf.sprintf "%s%s:\n" (print_indent indent) slabel

and print_stmt (indent : int) (stmt : Statement.t) =
  match stmt with
  | MethodCall { func; type_args; args; _ } ->
      let sfunc = print_expr func in
      let stype_args = List.map print_type type_args |> String.concat ", " in
      let stype_args =
        if List.length type_args > 0 then "<" ^ stype_args ^ ">" else ""
      in
      let sargs = List.map print_arg args |> String.concat ", " in
      Printf.sprintf "%s%s%s(%s);\n" (print_indent indent) sfunc stype_args
        sargs
  | Assignment { lhs; rhs; _ } ->
      let slhs = print_expr lhs in
      let srhs = print_expr rhs in
      Printf.sprintf "%s%s = %s;\n" (print_indent indent) slhs srhs
  | DirectApplication { typ; args; _ } ->
      let styp = print_type typ in
      let sargs = List.map print_arg args |> String.concat ", " in
      Printf.sprintf "%s%s.apply(%s);\n" (print_indent indent) styp sargs
  | Conditional { cond; tru; fls; _ } ->
      let scond = print_expr cond in
      let stru = print_stmt indent tru in
      let sfls =
        match fls with
        | None -> ""
        | Some fls ->
            let sfls = print_stmt indent fls in
            Printf.sprintf "%selse\n%s" (print_indent indent) sfls
      in
      Printf.sprintf "%sif (%s)\n%s%s\n" (print_indent indent) scond stru sfls
  | BlockStatement { block; _ } ->
      let sblock = print_block indent block in
      Printf.sprintf "%s%s" (print_indent indent) sblock
  | Exit _ -> Printf.sprintf "%sexit;\n" (print_indent indent)
  | EmptyStatement _ -> Printf.sprintf "%s;\n" (print_indent indent)
  | Return { expr; _ } ->
      let sexpr =
        match expr with
        | None -> ""
        | Some expr ->
            let sexpr = print_expr expr in
            Printf.sprintf " %s" sexpr
      in
      Printf.sprintf "%sreturn%s;\n" (print_indent indent) sexpr
  | Switch { expr; cases; _ } ->
      let sexpr = print_expr expr in
      let scases =
        List.map (print_switch_case (indent + 1)) cases |> String.concat ""
      in
      Printf.sprintf "%sswitch (%s) {\n%s%s}\n" (print_indent indent) sexpr
        scases (print_indent indent)
  | DeclarationStatement { decl; _ } ->
      let sdecl = print_decl indent decl in
      Printf.sprintf "%s%s" (print_indent indent) sdecl

(* Matches *)

and print_match (mtch : Match.t) =
  match mtch with
  | Default _ -> "default"
  | DontCare _ -> "_"
  | Expression { expr; _ } -> print_expr expr

(* Parsers *)

and print_parser_case (indent : int) (case : Parser.case) =
  let smatches = List.map print_match case.matches |> String.concat ", " in
  let smatches =
    if List.length case.matches > 1 then "(" ^ smatches ^ ")" else smatches
  in
  let snext = print_text case.next in
  Printf.sprintf "%s%s: %s;\n" (print_indent indent) smatches snext

and print_parser_transition (indent : int) (transition : Parser.transition) =
  match transition with
  | Direct { next; _ } ->
      let snext = print_text next in
      Printf.sprintf "%stransition %s;\n" (print_indent indent) snext
  | Select { exprs; cases; _ } ->
      let sexprs = List.map print_expr exprs |> String.concat ", " in
      let scases =
        List.map (print_parser_case (indent + 1)) cases |> String.concat ""
      in
      Printf.sprintf "%stransition select (%s) {\n%s%s}\n" (print_indent indent)
        sexprs scases (print_indent indent)

and print_parser_state (indent : int) (state : Parser.state) =
  let sname = print_text state.name in
  let sstmts =
    List.map (print_stmt (indent + 1)) state.statements |> String.concat "\n"
  in
  let strans = print_parser_transition (indent + 1) state.transition in
  Printf.sprintf "%sstate %s {\n%s%s%s}\n" (print_indent indent) sname sstmts
    strans (print_indent indent)

(* Tables *)

and print_table_action_ref (action_ref : Table.action_ref) =
  let sname = print_name action_ref.name in
  let sargs = List.map print_arg action_ref.args |> String.concat ", " in
  Printf.sprintf "%s(%s);\n" sname sargs

and print_table_key (key : Table.key) =
  let skey = print_expr key.key in
  let smatch_kind = print_text key.match_kind in
  Printf.sprintf "%s: %s;\n" skey smatch_kind

and print_table_entry (entry : Table.entry) =
  let smatches = List.map print_match entry.matches |> String.concat ", " in
  let smatches =
    if List.length entry.matches > 1 then "(" ^ smatches ^ ")" else smatches
  in
  let saction = print_table_action_ref entry.action in
  Printf.sprintf "%s: %s" smatches saction

and print_table_property (indent : int) (property : Table.property) =
  match property with
  | Key { keys; _ } ->
      let skeys =
        List.map print_table_key keys
        |> String.concat (Printf.sprintf "%s" (print_indent (indent + 1)))
      in
      let skeys =
        if List.length keys > 0 then print_indent (indent + 1) ^ skeys
        else skeys
      in
      Printf.sprintf "%skey = {\n%s%s}\n" (print_indent indent) skeys
        (print_indent indent)
  | Actions { actions; _ } ->
      let sactions =
        List.map print_table_action_ref actions
        |> String.concat (Printf.sprintf "%s" (print_indent (indent + 1)))
      in
      let sactions =
        if List.length actions > 0 then print_indent (indent + 1) ^ sactions
        else sactions
      in
      Printf.sprintf "%sactions = {\n%s%s}\n" (print_indent indent) sactions
        (print_indent indent)
  | Entries { entries; _ } ->
      let sentries =
        List.map print_table_entry entries
        |> String.concat (Printf.sprintf "%s" (print_indent (indent + 1)))
      in
      let sentries =
        if List.length entries > 0 then print_indent (indent + 1) ^ sentries
        else sentries
      in
      Printf.sprintf "%sconst entries = {\n%s%s}\n" (print_indent indent)
        sentries (print_indent indent)
  | DefaultAction { action; const; _ } ->
      let saction = print_table_action_ref action in
      let sconst = if const then "const " else "" in
      Printf.sprintf "%s%sdefault_action = %s" (print_indent indent) sconst
        saction
  | Custom { const; name; value; _ } ->
      let sconst = if const then "const " else "" in
      let sname = print_text name in
      let svalue = print_expr value in
      Printf.sprintf "%s%s%s = %s;\n" (print_indent indent) sconst sname svalue

(* Methods *)

and print_method_prototype (indent : int) (proto : MethodPrototype.t) =
  match proto with
  | Constructor { name; params; _ } ->
      let sname = print_text name in
      let sparams = List.map print_param params |> String.concat ", " in
      Printf.sprintf "%s%s(%s);\n" (print_indent indent) sname sparams
  | AbstractMethod { return; name; type_params; params; _ } ->
      let sreturn = print_type return in
      let sname = print_text name in
      let stype_params =
        List.map print_text type_params |> String.concat ", "
      in
      let stype_params =
        if List.length type_params > 0 then "<" ^ stype_params ^ ">" else ""
      in
      let sparams = List.map print_param params |> String.concat ", " in
      Printf.sprintf "%sabstract %s %s%s(%s);\n" (print_indent indent) sreturn
        sname stype_params sparams
  | Method { return; name; type_params; params; _ } ->
      let sreturn = print_type return in
      let sname = print_text name in
      let stype_params =
        List.map print_text type_params |> String.concat ", "
      in
      let stype_params =
        if List.length type_params > 0 then "<" ^ stype_params ^ ">" else ""
      in
      let sparams = List.map print_param params |> String.concat ", " in
      Printf.sprintf "%s%s %s%s(%s);\n" (print_indent indent) sreturn sname
        stype_params sparams

(* Declarations *)

and print_decl_field (indent : int) (field : Declaration.field) =
  let styp = print_type field.typ in
  let sname = print_text field.name in
  Printf.sprintf "%s%s %s;\n" (print_indent indent) styp sname

and print_decl (indent : int) (decl : Declaration.t) =
  match decl with
  | Constant { typ; name; value; _ } ->
      let styp = print_type typ in
      let sname = print_text name in
      let svalue = print_expr value in
      Printf.sprintf "%sconst %s %s = %s;\n" (print_indent indent) styp sname
        svalue
  | Instantiation { typ; args; name; init; _ } ->
      let styp = print_type typ in
      let sargs = List.map print_arg args |> String.concat ", " in
      let sname = print_text name in
      let sinit =
        match init with
        | [] -> ""
        | inits ->
            let sinit =
              List.map (print_decl (indent + 1)) inits |> String.concat "\n"
            in
            Printf.sprintf " = {\n%s\n%s\n}" sinit (print_indent indent)
      in
      Printf.sprintf "%s%s(%s) %s%s;\n" (print_indent indent) styp sargs sname
        sinit
  | Parser { name; type_params; params; constructor_params; locals; states; _ }
    ->
      let sname = print_text name in
      let stype_params =
        List.map print_text type_params |> String.concat ", "
      in
      let stype_params =
        if List.length type_params > 0 then "<" ^ stype_params ^ ">" else ""
      in
      let sparams = List.map print_param params |> String.concat ", " in
      let scons_params =
        List.map print_param constructor_params |> String.concat ", "
      in
      let scons_params =
        if List.length constructor_params > 0 then "(" ^ scons_params ^ ")"
        else ""
      in
      let slocals =
        List.map (print_decl (indent + 1)) locals |> String.concat ""
      in
      let sstates =
        List.map (print_parser_state (indent + 1)) states |> String.concat ""
      in
      Printf.sprintf "%sparser %s%s(%s)%s {\n%s%s%s}\n" (print_indent indent)
        sname stype_params sparams scons_params slocals sstates
        (print_indent indent)
  | Control { name; type_params; params; constructor_params; locals; apply; _ }
    ->
      let sname = print_text name in
      let stype_params =
        List.map print_text type_params |> String.concat ", "
      in
      let stype_params =
        if List.length type_params > 0 then "<" ^ stype_params ^ ">" else ""
      in
      let sparams = List.map print_param params |> String.concat ", " in
      let scons_params =
        List.map print_param constructor_params |> String.concat ", "
      in
      let scons_params =
        if List.length constructor_params > 0 then "(" ^ scons_params ^ ")"
        else ""
      in
      let slocals =
        List.map (print_decl (indent + 1)) locals |> String.concat ""
      in
      let sapply =
        let sprefix = print_indent (indent + 1) ^ "apply " in
        print_block ~prefix:sprefix (indent + 1) apply
      in
      Printf.sprintf "%scontrol %s%s(%s)%s {\n%s%s%s}\n" (print_indent indent)
        sname stype_params sparams scons_params slocals sapply
        (print_indent indent)
  | Function { return; name; type_params; params; body; _ } ->
      let sreturn = print_type return in
      let sname = print_text name in
      let stype_params =
        List.map print_text type_params |> String.concat ", "
      in
      let stype_params =
        if List.length type_params > 0 then "<" ^ stype_params ^ ">" else ""
      in
      let sparams = List.map print_param params |> String.concat ", " in
      let sbody = print_block (indent + 1) body in
      Printf.sprintf "%s%s %s%s(%s) %s\n" (print_indent indent) sreturn sname
        stype_params sparams sbody
  | ExternFunction { return; name; type_params; params; _ } ->
      let sreturn = print_type return in
      let sname = print_text name in
      let stype_params =
        List.map print_text type_params |> String.concat ", "
      in
      let stype_params =
        if List.length type_params > 0 then "<" ^ stype_params ^ ">" else ""
      in
      let sparams = List.map print_param params |> String.concat ", " in
      Printf.sprintf "%sextern %s %s%s(%s);\n" (print_indent indent) sreturn
        sname stype_params sparams
  | Variable { typ; name; init; _ } ->
      let styp = print_type typ in
      let sname = print_text name in
      let sinit =
        match init with
        | None -> ""
        | Some init ->
            let sinit = print_expr init in
            Printf.sprintf " = %s" sinit
      in
      Printf.sprintf "%s%s %s%s;\n" (print_indent indent) styp sname sinit
  | ValueSet { typ; size; name; _ } ->
      let styp = print_type typ in
      let ssize = print_expr size in
      let sname = print_text name in
      Printf.sprintf "%svalue_set<%s>(%s) %s;\n" (print_indent indent) styp
        ssize sname
  | Action { name; params; body; _ } ->
      let sname = print_text name in
      let sparams = List.map print_param params |> String.concat ", " in
      let sbody = print_block indent body in
      Printf.sprintf "%saction %s(%s) %s" (print_indent indent) sname sparams
        sbody
  | Table { name; properties; _ } ->
      let sname = print_text name in
      let sproperties =
        List.map (print_table_property (indent + 1)) properties
        |> String.concat ""
      in
      Printf.sprintf "%stable %s {\n%s%s}\n" (print_indent indent) sname
        sproperties (print_indent indent)
  | Header { name; type_params; fields; _ } ->
      let sname = print_text name in
      let stype_params =
        List.map print_text type_params |> String.concat ", "
      in
      let stype_params =
        if List.length type_params > 0 then "<" ^ stype_params ^ ">" else ""
      in
      let sfields =
        List.map (print_decl_field (indent + 1)) fields |> String.concat ""
      in
      Printf.sprintf "%sheader %s%s {\n%s%s}\n" (print_indent indent) sname
        stype_params sfields (print_indent indent)
  | HeaderUnion { name; type_params; fields; _ } ->
      let sname = print_text name in
      let stype_params =
        List.map print_text type_params |> String.concat ", "
      in
      let stype_params =
        if List.length type_params > 0 then "<" ^ stype_params ^ ">" else ""
      in
      let sfields =
        List.map (print_decl_field (indent + 1)) fields |> String.concat ""
      in
      Printf.sprintf "%sheader_union %s%s {\n%s%s}\n" (print_indent indent)
        sname stype_params sfields (print_indent indent)
  | Struct { name; type_params; fields; _ } ->
      let sname = print_text name in
      let stype_params =
        List.map print_text type_params |> String.concat ", "
      in
      let stype_params =
        if List.length type_params > 0 then "<" ^ stype_params ^ ">" else ""
      in
      let sfields =
        List.map (print_decl_field (indent + 1)) fields |> String.concat ""
      in
      Printf.sprintf "%sstruct %s%s {\n%s%s}\n" (print_indent indent) sname
        stype_params sfields (print_indent indent)
  | Error { members; _ } ->
      let smembers =
        List.map print_text members
        |> String.concat (Printf.sprintf ",\n%s" (print_indent (indent + 1)))
      in
      let smembers =
        if List.length members > 0 then print_indent (indent + 1) ^ smembers
        else smembers
      in
      Printf.sprintf "%serror {\n%s\n%s}\n" (print_indent indent) smembers
        (print_indent indent)
  | MatchKind { members; _ } ->
      let smembers =
        List.map print_text members
        |> String.concat (Printf.sprintf ",\n%s" (print_indent (indent + 1)))
      in
      let smembers =
        if List.length members > 0 then print_indent (indent + 1) ^ smembers
        else smembers
      in
      Printf.sprintf "%smatch_kind {\n%s\n%s}\n" (print_indent indent) smembers
        (print_indent indent)
  | Enum { name; members; _ } ->
      let sname = print_text name in
      let smembers =
        List.map print_text members
        |> String.concat (Printf.sprintf ",\n%s" (print_indent (indent + 1)))
      in
      let smembers =
        if List.length members > 0 then print_indent (indent + 1) ^ smembers
        else smembers
      in
      Printf.sprintf "%senum %s {\n%s\n%s}\n" (print_indent indent) sname
        smembers (print_indent indent)
  | SerializableEnum { typ; name; members; _ } ->
      let styp = print_type typ in
      let sname = print_text name in
      let smembers =
        List.map
          (fun (text, expr) ->
            let stext = print_text text in
            let sexpr = print_expr expr in
            Printf.sprintf "%s = %s" stext sexpr)
          members
        |> String.concat (Printf.sprintf ",\n%s" (print_indent (indent + 1)))
      in
      let smembers =
        if List.length members > 0 then print_indent (indent + 1) ^ smembers
        else smembers
      in
      Printf.sprintf "%senum %s %s {\n%s\n%s}\n" (print_indent indent) styp
        sname smembers (print_indent indent)
  | ExternObject { name; type_params; methods; _ } ->
      let sname = print_text name in
      let stype_params =
        List.map print_text type_params |> String.concat ", "
      in
      let stype_params =
        if List.length type_params > 0 then "<" ^ stype_params ^ ">" else ""
      in
      let smethods =
        List.map (print_method_prototype (indent + 1)) methods
        |> String.concat ""
      in
      Printf.sprintf "%sextern %s%s {\n%s%s}\n" (print_indent indent) sname
        stype_params smethods (print_indent indent)
  | TypeDef { name; typ_or_decl; _ } ->
      let sname = print_text name in
      let styp_or_decl =
        match typ_or_decl with
        | Left typ -> print_type typ
        | Right decl -> print_decl (indent + 1) decl
      in
      Printf.sprintf "%stypedef %s %s;\n" (print_indent indent) styp_or_decl
        sname
  | NewType { name; typ_or_decl; _ } ->
      let sname = print_text name in
      let styp_or_decl =
        match typ_or_decl with
        | Left typ -> print_type typ
        | Right decl -> print_decl (indent + 1) decl
      in
      Printf.sprintf "%stype %s %s;\n" (print_indent indent) styp_or_decl sname
  | ControlType { name; type_params; params; _ } ->
      let sname = print_text name in
      let stype_params =
        List.map print_text type_params |> String.concat ", "
      in
      let stype_params =
        if List.length type_params > 0 then "<" ^ stype_params ^ ">" else ""
      in
      let sparams = List.map print_param params |> String.concat ", " in
      Printf.sprintf "%scontrol %s%s(%s);\n" (print_indent indent) sname
        stype_params sparams
  | ParserType { name; type_params; params; _ } ->
      let sname = print_text name in
      let stype_params =
        List.map print_text type_params |> String.concat ", "
      in
      let stype_params =
        if List.length type_params > 0 then "<" ^ stype_params ^ ">" else ""
      in
      let sparams = List.map print_param params |> String.concat ", " in
      Printf.sprintf "%sparser %s%s(%s);\n" (print_indent indent) sname
        stype_params sparams
  | PackageType { name; type_params; params; _ } ->
      let sname = print_text name in
      let stype_params =
        List.map print_text type_params |> String.concat ", "
      in
      let stype_params =
        if List.length type_params > 0 then "<" ^ stype_params ^ ">" else ""
      in
      let sparams = List.map print_param params |> String.concat ", " in
      Printf.sprintf "%spackage %s%s(%s);\n" (print_indent indent) sname
        stype_params sparams

(* Program *)

let print_program (program : p4program) =
  let (Program decls) = program in
  List.map (print_decl 0) decls |> String.concat "\n"
