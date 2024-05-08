open Ast
open Pretty
open Utils

(* Basics *)

let debug_text (text : Text.t) = Printf.sprintf "Text (%s)" (print_text text)

let debug_number (number : Number.t) =
  Printf.sprintf "Number (%s)" (print_number number)

let debug_name (name : Name.t) =
  match name with
  | BareName name -> "BareName (" ^ debug_text name ^ ")"
  | QualifiedName (prefix, name) ->
      let sprefix = List.map debug_text prefix |> String.concat ", " in
      let sname = debug_text name in
      Printf.sprintf "QualifiedName ([%s], %s)" sprefix sname

let debug_direction (dir : Direction.t) =
  Printf.sprintf "Direction (%s)" (print_direction dir)

(* Operators *)

let debug_unop (unop : Op.un) = Printf.sprintf "Unop (%s)" (print_unop unop)

let debug_binop (binop : Op.bin) =
  Printf.sprintf "Binop (%s)" (print_binop binop)

(* Types *)

let rec debug_type (typ : Type.t) =
  match typ with
  | Bool _ -> "Bool"
  | Error _ -> "Error"
  | Integer _ -> "Integer"
  | IntType { expr; _ } ->
      let sexpr = debug_expr expr in
      Printf.sprintf "IntType (%s)" sexpr
  | BitType { expr; _ } ->
      let sexpr = debug_expr expr in
      Printf.sprintf "BitType (%s)" sexpr
  | VarBit { expr; _ } ->
      let sexpr = debug_expr expr in
      Printf.sprintf "VarBit (%s)" sexpr
  | TypeName { name; _ } -> debug_name name
  | SpecializedType { base; args; _ } ->
      let sbase = debug_type base in
      let sargs = List.map debug_type args |> String.concat ", " in
      Printf.sprintf "SpecializedType (%s, [%s])" sbase sargs
  | HeaderStack { header; size; _ } ->
      let sheader = debug_type header in
      let ssize = debug_expr size in
      Printf.sprintf "HeaderStack (%s, %s)" sheader ssize
  | Tuple { args; _ } ->
      let sargs = List.map debug_type args |> String.concat ", " in
      Printf.sprintf "Tuple ([%s])" sargs
  | String _ -> "String"
  | Void _ -> "Void"
  | DontCare _ -> "DontCare"

(* Arguments and Prameters *)

and debug_arg (arg : Argument.t) =
  match arg with
  | Expression { value; _ } ->
      let svalue = debug_expr value in
      Printf.sprintf "Expression (%s)" svalue
  | KeyValue { key; value; _ } ->
      let skey = debug_text key in
      let svalue = debug_expr value in
      Printf.sprintf "KeyValue (%s, %s)" skey svalue
  | Missing _ -> "Missing"

and debug_param (param : Parameter.t) =
  let sdirection =
    match param.direction with
    | None -> ""
    | Some direction -> debug_direction direction
  in
  let styp = debug_type param.typ in
  let svariable = debug_text param.variable in
  let svalue =
    match param.opt_value with
    | None -> ""
    | Some value ->
        let svalue = debug_expr value in
        Printf.sprintf "%s" svalue
  in
  Printf.sprintf "Parameter (%s, %s, %s, %s)" sdirection styp svariable svalue

(* Expressions *)

and debug_key_value (key_value : KeyValue.t) =
  let skey = debug_text key_value.key in
  let svalue = debug_expr key_value.value in
  Printf.sprintf "KeyValue (%s, %s)" skey svalue

and debug_expr (expr : Expression.t) =
  match expr with
  | True _ -> "True"
  | False _ -> "False"
  | Int { i; _ } ->
      let si = debug_number i in
      Printf.sprintf "Int (%s)" si
  | String { text; _ } ->
      let stext = "\"" ^ debug_text text ^ "\"" in
      Printf.sprintf "String (%s)" stext
  | Name { name; _ } ->
      let sname = debug_name name in
      Printf.sprintf "Name (%s)" sname
  | ArrayAccess { array; index; _ } ->
      let sarray = debug_expr array in
      let sindex = debug_expr index in
      Printf.sprintf "ArrayAccess (%s, %s)" sarray sindex
  | BitStringAccess { bits; lo; hi; _ } ->
      let sbits = debug_expr bits in
      let slo = debug_expr lo in
      let shi = debug_expr hi in
      Printf.sprintf "BitStringAccess (%s, %s, %s)" sbits slo shi
  | List { values; _ } ->
      let svalues = List.map debug_expr values |> String.concat ", " in
      Printf.sprintf "List ([%s])" svalues
  | Record { entries; _ } ->
      let sentries = List.map debug_key_value entries |> String.concat ", " in
      Printf.sprintf "Record (%s)" sentries
  | UnaryOp { op; arg; _ } ->
      let sop = debug_unop op in
      let sarg = debug_expr arg in
      Printf.sprintf "UnaryOp (%s, %s)" sop sarg
  | BinaryOp { op; args; _ } ->
      let sop = debug_binop op in
      let arg1, arg2 = args in
      let sarg1 = debug_expr arg1 in
      let sarg2 = debug_expr arg2 in
      Printf.sprintf "BinaryOp (%s, (%s, %s))" sop sarg1 sarg2
  | Cast { typ; expr; _ } ->
      let styp = debug_type typ in
      let sexpr = debug_expr expr in
      Printf.sprintf "Cast (%s, %s)" styp sexpr
  | TypeMember { typ; name; _ } ->
      let styp = debug_name typ in
      let sname = debug_text name in
      Printf.sprintf "TypeMember (%s, %s)" styp sname
  | ErrorMember { err; _ } ->
      let serr = debug_text err in
      Printf.sprintf "ErrorMember (%s)" serr
  | ExpressionMember { expr; name; _ } ->
      let sexpr = debug_expr expr in
      let sname = debug_text name in
      Printf.sprintf "ExpressionMember (%s, %s)" sexpr sname
  | Ternary { cond; tru; fls; _ } ->
      let scond = debug_expr cond in
      let stru = debug_expr tru in
      let sfls = debug_expr fls in
      Printf.sprintf "Ternary (%s, %s, %s)" scond stru sfls
  | FunctionCall { func; type_args; args; _ } ->
      let sfunc = debug_expr func in
      let stype_args = List.map debug_type type_args |> String.concat ", " in
      let sargs = List.map debug_arg args |> String.concat ", " in
      Printf.sprintf "FunctionCall (%s, [%s], [%s])" sfunc stype_args sargs
  | NamelessInstantiation { typ; args; _ } ->
      let styp = debug_type typ in
      let sargs = List.map debug_arg args |> String.concat ", " in
      Printf.sprintf "NamelessInstantiation (%s, [%s])" styp sargs
  | Mask { expr; mask; _ } ->
      let sexpr = debug_expr expr in
      let smask = debug_expr mask in
      Printf.sprintf "Mask (%s, %s)" sexpr smask
  | Range { lo; hi; _ } ->
      let slo = debug_expr lo in
      let shi = debug_expr hi in
      Printf.sprintf "Range (%s, %s)" slo shi

(* Statements *)

and debug_block (indent : int) (block : Block.t) =
  let sstmts =
    List.map (debug_stmt (indent + 1)) block.statements |> String.concat ""
  in
  Printf.sprintf "Block (\n%s%s)" sstmts (Print.print_indent indent)

and debug_switch_label (label : Statement.switch_label) =
  match label with
  | Default _ -> "Default"
  | Name { name; _ } ->
      let sname = debug_text name in
      Printf.sprintf "Name (%s)" sname

and debug_switch_case (indent : int) (case : Statement.switch_case) =
  match case with
  | Action { label; code; _ } ->
      let slabel = debug_switch_label label in
      let scode = debug_block indent code in
      Printf.sprintf "%sAction (%s, %s)"
        (Print.print_indent indent)
        slabel scode
  | FallThrough { label; _ } ->
      let slabel = debug_switch_label label in
      Printf.sprintf "%sFallThrough (%s)" (Print.print_indent indent) slabel

and debug_stmt (indent : int) (stmt : Statement.t) =
  match stmt with
  | MethodCall { func; type_args; args; _ } ->
      let sfunc = debug_expr func in
      let stype_args = List.map debug_type type_args |> String.concat ", " in
      let sargs = List.map debug_arg args |> String.concat ", " in
      Printf.sprintf "%sMethodCall (%s, [%s], [%s])\n"
        (Print.print_indent indent)
        sfunc stype_args sargs
  | Assignment { lhs; rhs; _ } ->
      let slhs = debug_expr lhs in
      let srhs = debug_expr rhs in
      Printf.sprintf "%sAssignment (%s, %s)\n"
        (Print.print_indent indent)
        slhs srhs
  | DirectApplication { typ; args; _ } ->
      let styp = debug_type typ in
      let sargs = List.map debug_arg args |> String.concat ", " in
      Printf.sprintf "%sDirectApplication (%s, %s)\n"
        (Print.print_indent indent)
        styp sargs
  | Conditional { cond; tru; fls; _ } ->
      let scond = debug_expr cond in
      let stru = debug_stmt indent tru in
      let sfls =
        match fls with
        | None -> ""
        | Some fls ->
            let sfls = debug_stmt indent fls in
            Printf.sprintf "\n%s%s" (Print.print_indent indent) sfls
      in
      Printf.sprintf "%sConditional(%s,\n%s%s)\n"
        (Print.print_indent indent)
        scond stru sfls
  | BlockStatement { block; _ } ->
      let sblock = debug_block indent block in
      Printf.sprintf "%sBlockStatement (%s)" (Print.print_indent indent) sblock
  | Exit _ -> Printf.sprintf "%sExit\n" (Print.print_indent indent)
  | EmptyStatement _ ->
      Printf.sprintf "%sEmptyStatement\n" (Print.print_indent indent)
  | Return { expr; _ } ->
      let sexpr = match expr with None -> "" | Some expr -> debug_expr expr in
      Printf.sprintf "%sReturn (%s)\n" (Print.print_indent indent) sexpr
  | Switch { expr; cases; _ } ->
      let sexpr = debug_expr expr in
      let scases =
        List.map (debug_switch_case (indent + 1)) cases |> String.concat ", "
      in
      Printf.sprintf "%sSwitch (%s,\n%s%s)\n"
        (Print.print_indent indent)
        sexpr scases
        (Print.print_indent indent)
  | DeclarationStatement { decl; _ } ->
      let sdecl = debug_decl indent decl in
      Printf.sprintf "%sDeclarationStatement (%s)\n"
        (Print.print_indent indent)
        sdecl

(* Matches *)

and debug_match (mtch : Match.t) =
  match mtch with
  | Default _ -> "Default"
  | DontCare _ -> "DontCare"
  | Expression { expr; _ } ->
      let sexpr = debug_expr expr in
      Printf.sprintf "Expression (%s)" sexpr

(* Parsers *)

and debug_parser_case (indent : int) (case : Parser.case) =
  let smatches = List.map debug_match case.matches |> String.concat ", " in
  let snext = debug_text case.next in
  Printf.sprintf "%sParser.case (%s, %s)\n"
    (Print.print_indent indent)
    smatches snext

and debug_parser_transition (indent : int) (transition : Parser.transition) =
  match transition with
  | Direct { next; _ } ->
      let snext = debug_text next in
      Printf.sprintf "%sDirect (%s)\n" (Print.print_indent indent) snext
  | Select { exprs; cases; _ } ->
      let sexprs = List.map debug_expr exprs |> String.concat ", " in
      let scases =
        List.map (debug_parser_case (indent + 2)) cases |> String.concat ""
      in
      Printf.sprintf "%sSelect ([%s],\n%s[\n%s%s])\n"
        (Print.print_indent indent)
        sexprs
        (Print.print_indent (indent + 1))
        scases
        (Print.print_indent (indent + 1))

and debug_parser_state (indent : int) (state : Parser.state) =
  let sname = debug_text state.name in
  let sstmts =
    List.map (debug_stmt (indent + 2)) state.statements |> String.concat ""
  in
  let strans = debug_parser_transition (indent + 2) state.transition in
  Printf.sprintf "%sParser.state (%s,\n%s[\n%s%s],\n%s[\n%s%s])\n"
    (Print.print_indent indent)
    sname
    (Print.print_indent (indent + 1))
    sstmts
    (Print.print_indent (indent + 1))
    (Print.print_indent (indent + 1))
    strans
    (Print.print_indent (indent + 1))

(* Tables *)

and debug_table_action_ref (action_ref : Table.action_ref) =
  let sname = debug_name action_ref.name in
  let sargs = List.map debug_arg action_ref.args |> String.concat ", " in
  Printf.sprintf "Table.action_ref (%s, [%s])\n" sname sargs

and debug_table_key (key : Table.key) =
  let skey = debug_expr key.key in
  let smatch_kind = debug_text key.match_kind in
  Printf.sprintf "Table.key (%s, %s)\n" skey smatch_kind

and debug_table_entry (entry : Table.entry) =
  let smatches = List.map debug_match entry.matches |> String.concat ", " in
  let saction = debug_table_action_ref entry.action in
  Printf.sprintf "Table.entry ([%s], %s)\n" smatches saction

and debug_table_property (indent : int) (property : Table.property) =
  match property with
  | Key { keys; _ } ->
      let skeys =
        List.map debug_table_key keys
        |> String.concat (Printf.sprintf "%s" (Print.print_indent (indent + 2)))
      in
      Printf.sprintf "%sKey ([\n%s%s%s])\n"
        (Print.print_indent indent)
        (Print.print_indent (indent + 1))
        skeys
        (Print.print_indent (indent + 1))
  | Actions { actions; _ } ->
      let sactions =
        List.map debug_table_action_ref actions
        |> String.concat (Printf.sprintf "%s" (Print.print_indent (indent + 2)))
      in
      Printf.sprintf "%sActions ([\n%s%s%s])\n"
        (Print.print_indent indent)
        (Print.print_indent (indent + 1))
        sactions
        (Print.print_indent (indent + 1))
  | Entries { entries; _ } ->
      let sentries =
        List.map debug_table_entry entries
        |> String.concat (Printf.sprintf "%s" (Print.print_indent (indent + 2)))
      in
      Printf.sprintf "%sEntries (\n%s[\n%s%s])\n"
        (Print.print_indent indent)
        (Print.print_indent (indent + 1))
        sentries
        (Print.print_indent (indent + 1))
  | DefaultAction { action; const; _ } ->
      let saction = debug_table_action_ref action in
      let sconst = if const then "const" else "" in
      Printf.sprintf "%sDefaultAction (%s, %s)\n"
        (Print.print_indent indent)
        sconst saction
  | Custom { const; name; value; _ } ->
      let sconst = if const then "const" else "" in
      let sname = debug_text name in
      let svalue = debug_expr value in
      Printf.sprintf "%sCustom (%s, %s, %s)\n"
        (Print.print_indent indent)
        sconst sname svalue

(* Methods *)

and debug_method_prototype (indent : int) (proto : MethodPrototype.t) =
  match proto with
  | Constructor { name; params; _ } ->
      let sname = debug_text name in
      let sparams = List.map debug_param params |> String.concat ", " in
      Printf.sprintf "%sConstructor (%s, [%s])\n"
        (Print.print_indent indent)
        sname sparams
  | AbstractMethod { return; name; type_params; params; _ } ->
      let sreturn = debug_type return in
      let sname = debug_text name in
      let stype_params =
        List.map debug_text type_params |> String.concat ", "
      in
      let sparams = List.map debug_param params |> String.concat ", " in
      Printf.sprintf "%sAbstractMethod (%s, %s, [%s], [%s])\n"
        (Print.print_indent indent)
        sreturn sname stype_params sparams
  | Method { return; name; type_params; params; _ } ->
      let sreturn = debug_type return in
      let sname = debug_text name in
      let stype_params =
        List.map debug_text type_params |> String.concat ", "
      in
      let sparams = List.map debug_param params |> String.concat ", " in
      Printf.sprintf "%sMethod (%s, %s, [%s], [%s])\n"
        (Print.print_indent indent)
        sreturn sname stype_params sparams

(* Declarations *)

and debug_decl_field (indent : int) (field : Declaration.field) =
  let styp = debug_type field.typ in
  let sname = debug_text field.name in
  Printf.sprintf "%sDeclaration.field (%s, %s)\n"
    (Print.print_indent indent)
    styp sname

and debug_decl (indent : int) (decl : Declaration.t) =
  match decl with
  | Constant { typ; name; value; _ } ->
      let styp = debug_type typ in
      let sname = debug_text name in
      let svalue = debug_expr value in
      Printf.sprintf "%sConstant (%s, %s, %s)\n"
        (Print.print_indent indent)
        styp sname svalue
  | Instantiation { typ; args; name; init; _ } ->
      let styp = debug_type typ in
      let sargs = List.map debug_arg args |> String.concat ", " in
      let sname = debug_text name in
      let sinit =
        match init with
        | None -> ""
        | Some init ->
            let sinit = debug_block (indent + 1) init in
            Printf.sprintf "%s" sinit
      in
      Printf.sprintf "%sInstantiation (%s, [%s], %s, %s)\n"
        (Print.print_indent indent)
        styp sargs sname sinit
  | Parser { name; type_params; params; constructor_params; locals; states; _ }
    ->
      let sname = debug_text name in
      let stype_params =
        List.map debug_text type_params |> String.concat ", "
      in
      let sparams = List.map debug_param params |> String.concat ", " in
      let scons_params =
        List.map debug_param constructor_params |> String.concat ", "
      in
      let slocals =
        List.map (debug_decl (indent + 2)) locals |> String.concat ""
      in
      let sstates =
        List.map (debug_parser_state (indent + 2)) states |> String.concat ""
      in
      Printf.sprintf
        "%sParser (%s, [%s], [%s], [%s],\n%s[\n%s%s],\n%s[\n%s%s])\n"
        (Print.print_indent indent)
        sname stype_params sparams scons_params
        (Print.print_indent (indent + 1))
        slocals
        (Print.print_indent (indent + 1))
        (Print.print_indent (indent + 1))
        sstates
        (Print.print_indent (indent + 1))
  | Control { name; type_params; params; constructor_params; locals; apply; _ }
    ->
      let sname = debug_text name in
      let stype_params =
        List.map debug_text type_params |> String.concat ", "
      in
      let sparams = List.map debug_param params |> String.concat ", " in
      let scons_params =
        List.map debug_param constructor_params |> String.concat ", "
      in
      let slocals =
        List.map (debug_decl (indent + 2)) locals |> String.concat ""
      in
      let sapply = debug_block (indent + 2) apply in
      Printf.sprintf "%sControl (%s, [%s], [%s], [%s],\n%s[\n%s%s],\n%s%s)\n"
        (Print.print_indent indent)
        sname stype_params sparams scons_params
        (Print.print_indent (indent + 1))
        slocals
        (Print.print_indent (indent + 1))
        (Print.print_indent (indent + 1))
        sapply
  | Function { return; name; type_params; params; body; _ } ->
      let sreturn = debug_type return in
      let sname = debug_text name in
      let stype_params =
        List.map debug_text type_params |> String.concat ", "
      in
      let sparams = List.map debug_param params |> String.concat ", " in
      let sbody = debug_block (indent + 1) body in
      Printf.sprintf "%sFunction (%s, %s, [%s], [%s],\n%s%s)\n"
        (Print.print_indent indent)
        sreturn sname stype_params sparams sbody
        (Print.print_indent indent)
  | ExternFunction { return; name; type_params; params; _ } ->
      let sreturn = debug_type return in
      let sname = debug_text name in
      let stype_params =
        List.map debug_text type_params |> String.concat ", "
      in
      let sparams = List.map debug_param params |> String.concat ", " in
      Printf.sprintf "%sExternFunction (%s, %s, [%s], [%s])\n"
        (Print.print_indent indent)
        sreturn sname stype_params sparams
  | Variable { typ; name; init; _ } ->
      let styp = debug_type typ in
      let sname = debug_text name in
      let sinit = match init with None -> "" | Some init -> debug_expr init in
      Printf.sprintf "%sVariable (%s, %s, %s)\n"
        (Print.print_indent indent)
        styp sname sinit
  | ValueSet { typ; size; name; _ } ->
      let styp = debug_type typ in
      let ssize = debug_expr size in
      let sname = debug_text name in
      Printf.sprintf "%sValueSet (%s, %s, %s)\n"
        (Print.print_indent indent)
        styp ssize sname
  | Action { name; params; body; _ } ->
      let sname = debug_text name in
      let sparams = List.map debug_param params |> String.concat ", " in
      let sbody = debug_block indent body in
      Printf.sprintf "%sAction (%s, [%s], %s)\n"
        (Print.print_indent indent)
        sname sparams sbody
  | Table { name; properties; _ } ->
      let sname = debug_text name in
      let sproperties =
        List.map (debug_table_property (indent + 2)) properties
        |> String.concat ""
      in
      Printf.sprintf "%sTable (%s,\n%s[\n%s%s])\n"
        (Print.print_indent indent)
        sname
        (Print.print_indent (indent + 1))
        sproperties
        (Print.print_indent (indent + 1))
  | Header { name; fields; _ } ->
      let sname = debug_text name in
      let sfields =
        List.map (debug_decl_field (indent + 1)) fields |> String.concat ""
      in
      Printf.sprintf "%sHeader (%s, %s)\n"
        (Print.print_indent indent)
        sname sfields
  | HeaderUnion { name; fields; _ } ->
      let sname = debug_text name in
      let sfields =
        List.map (debug_decl_field (indent + 1)) fields |> String.concat ""
      in
      Printf.sprintf "%sHeaderUnion (%s, %s)\n"
        (Print.print_indent indent)
        sname sfields
  | Struct { name; fields; _ } ->
      let sname = debug_text name in
      let sfields =
        List.map (debug_decl_field (indent + 1)) fields |> String.concat ""
      in
      Printf.sprintf "%sStruct (%s,\n%s)\n"
        (Print.print_indent indent)
        sname sfields
  | Error { members; _ } ->
      let smembers =
        List.map debug_text members
        |> String.concat
             (Printf.sprintf ",\n%s" (Print.print_indent (indent + 1)))
      in
      Printf.sprintf "%sError (\n%s%s%s)\n"
        (Print.print_indent indent)
        (Print.print_indent (indent + 1))
        smembers
        (Print.print_indent indent)
  | MatchKind { members; _ } ->
      let smembers =
        List.map debug_text members
        |> String.concat
             (Printf.sprintf ",\n%s" (Print.print_indent (indent + 1)))
      in
      Printf.sprintf "%sMatchKind (\n%s%s%s)\n"
        (Print.print_indent indent)
        (Print.print_indent (indent + 1))
        smembers
        (Print.print_indent indent)
  | Enum { name; members; _ } ->
      let sname = debug_text name in
      let smembers =
        List.map debug_text members
        |> String.concat
             (Printf.sprintf ",\n%s" (Print.print_indent (indent + 1)))
      in
      Printf.sprintf "%sEnum (%s,\n%s%s)\n"
        (Print.print_indent indent)
        sname
        (Print.print_indent (indent + 1))
        smembers
  | SerializableEnum { typ; name; members; _ } ->
      let styp = debug_type typ in
      let sname = debug_text name in
      let smembers =
        List.map
          (fun (text, expr) ->
            let stext = debug_text text in
            let sexpr = debug_expr expr in
            Printf.sprintf "(%s, %s)" stext sexpr)
          members
        |> String.concat
             (Printf.sprintf ",\n%s" (Print.print_indent (indent + 1)))
      in
      Printf.sprintf "%sSerializableEnum (%s, %s,\n%s%s)\n"
        (Print.print_indent indent)
        styp sname
        (Print.print_indent (indent + 1))
        smembers
  | ExternObject { name; type_params; methods; _ } ->
      let sname = debug_text name in
      let stype_params =
        List.map debug_text type_params |> String.concat ", "
      in
      let smethods =
        List.map (debug_method_prototype (indent + 1)) methods
        |> String.concat ""
      in
      Printf.sprintf "%sExternObject (%s, [%s],\n%s%s)\n"
        (Print.print_indent indent)
        sname stype_params
        (Print.print_indent indent)
        smethods
  | TypeDef { name; typ_or_decl; _ } ->
      let sname = debug_text name in
      let styp_or_decl =
        match typ_or_decl with
        | Alternative.Left typ -> debug_type typ
        | Alternative.Right decl -> debug_decl (indent + 1) decl
      in
      Printf.sprintf "%sTypeDef (%s, %s)\n"
        (Print.print_indent indent)
        sname styp_or_decl
  | NewType { name; typ_or_decl; _ } ->
      let sname = debug_text name in
      let styp_or_decl =
        match typ_or_decl with
        | Alternative.Left typ -> debug_type typ
        | Alternative.Right decl -> debug_decl (indent + 1) decl
      in
      Printf.sprintf "%sNewType (%s, %s)\n"
        (Print.print_indent indent)
        sname styp_or_decl
  | ControlType { name; type_params; params; _ } ->
      let sname = debug_text name in
      let stype_params =
        List.map debug_text type_params |> String.concat ", "
      in
      let sparams = List.map debug_param params |> String.concat ", " in
      Printf.sprintf "%sControlType (%s, [%s], [%s])\n"
        (Print.print_indent indent)
        sname stype_params sparams
  | ParserType { name; type_params; params; _ } ->
      let sname = debug_text name in
      let stype_params =
        List.map debug_text type_params |> String.concat ", "
      in
      let sparams = List.map debug_param params |> String.concat ", " in
      Printf.sprintf "%sParserType (%s, [%s], [%s])\n"
        (Print.print_indent indent)
        sname stype_params sparams
  | PackageType { name; type_params; params; _ } ->
      let sname = debug_text name in
      let stype_params =
        List.map debug_text type_params |> String.concat ", "
      in
      let sparams = List.map debug_param params |> String.concat ", " in
      Printf.sprintf "%sPackageType (%s, [%s], [%s])\n"
        (Print.print_indent indent)
        sname stype_params sparams

(* Program *)

let debug_program (program : p4program) =
  let (Program decls) = program in
  List.map (debug_decl 0) decls |> String.concat "\n"
