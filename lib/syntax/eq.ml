open Ast
open Print
open Utils
open Alternative

let dbg (expr: Expression.t) =
  match expr with
  | True _ -> "True"
  | False _ -> "False"
  | Int _ -> "Int"
  | String _ -> "String"
  | Name _ -> "Name"
  | ArrayAccess _ -> "ArrayAccess"
  | BitStringAccess _ -> "BitStringAccess"
  | List _ -> "List"
  | Record _ -> "Record"
  | UnaryOp _ -> "UnaryOp"
  | BinaryOp _ -> "BinaryOp"
  | Cast _ -> "Cast"
  | TypeMember _ -> "TypeMember"
  | ErrorMember _ -> "ErrorMember"
  | ExpressionMember _ -> "ExpressionMember"
  | Ternary _ -> "Ternary"
  | FunctionCall _ -> "FunctionCall"
  | NamelessInstantiation _ -> "NamelessInstantiation"
  | Mask _ -> "Mask"
  | Range _ -> "Range"

(* Utils *)

let eq_option (eq: 'a -> 'a -> bool) (opt: 'a option) (opt': 'a option) =
  let res =
    match opt, opt' with
    | None, None -> true
    | Some x, Some y -> eq x y
    | _ -> false
  in
  if not res then "eq_option: neq" |> print_endline;
  res

let eq_list (eq: 'a -> 'a -> bool) (l: 'a list) (l': 'a list) =
  let res =
    List.length l = List.length l' &&
    List.for_all2 eq l l'
  in
  if not res then "eq_list: neq" |> print_endline;
  res

let eq_alternative
  (eq_a: 'a -> 'a -> bool) (eq_b: 'b -> 'b -> bool)
  (alt: ('a, 'b) alternative) (alt': ('a, 'b) alternative) =
  let res =
    match alt, alt' with
    | Left x, Left y -> eq_a x y
    | Right x, Right y -> eq_b x y
    | _ -> false
  in
  if not res then "eq_alternative: neq" |> print_endline;
  res


(* Basics *)

let eq_text (text: Text.t) (text': Text.t) =
  let res = text.str = text'.str in
  if not res then
    Printf.sprintf "eq_text: %s does not equal %s"
      (print_text text) (print_text text')
    |> print_endline;
  res

let eq_number (number: Number.t) (number': Number.t) =
  let res =
    number.value = number'.value &&
    match number.width_signed, number'.width_signed with
    | None, None -> true
    | Some (width, signed), Some (width', signed') ->
        width = width' && signed = signed'
    | _ -> false
  in
  if not res then
    Printf.sprintf "eq_number: %s does not equal %s"
      (print_number number) (print_number number')
    |> print_endline;
  res

let eq_name (name: Name.t) (name': Name.t) =
  match name, name' with
  | BareName name, BareName name' ->
      eq_text name name'
  | QualifiedName (prefix, name), QualifiedName (prefix', name') ->
      eq_list eq_text prefix prefix' &&
      eq_text name name'
  | _ ->
      Printf.sprintf "eq_name: %s does not equal %s"
        (print_name name) (print_name name')
      |> print_endline;
      false

let eq_direction (dir: Direction.t) (dir': Direction.t) =
  match dir, dir' with
  | In _, In _
  | Out _, Out _
  | InOut _, InOut _ -> true
  | _ ->
      Printf.sprintf "eq_direction: %s does not equal %s"
        (print_direction dir) (print_direction dir')
      |> print_endline;
      false


(* Operators *)

let eq_unop (unop: Op.un) (unop': Op.un) =
  match unop, unop' with
  | Not _ , Not _ -> true
  | BitNot _, BitNot _ -> true
  | UMinus _, UMinus _ -> true
  | _ ->
      Printf.sprintf "eq_unop: %s does not equal %s"
        (print_unop unop) (print_unop unop')
      |> print_endline;
      false

let eq_binop (binop: Op.bin) (binop': Op.bin) =
  match binop, binop' with
  | Plus _, Plus _ -> true
  | PlusSat _, PlusSat _ -> true
  | Minus _, Minus _ -> true
  | MinusSat _, MinusSat _ -> true
  | Mul _, Mul _ -> true
  | Div _, Div _ -> true
  | Mod _, Mod _ -> true
  | Shl _, Shl _ -> true
  | Shr _, Shr _ -> true
  | Le _, Le _ -> true
  | Ge _, Ge _ -> true
  | Lt _, Lt _ -> true
  | Gt _, Gt _ -> true
  | Eq _, Eq _ -> true
  | NotEq _, NotEq _ -> true
  | BitAnd _, BitAnd _ -> true
  | BitXor _, BitXor _ -> true
  | BitOr _, BitOr _ -> true
  | PlusPlus _, PlusPlus _ -> true
  | And _, And _ -> true
  | Or _, Or _ -> true
  | _ ->
      Printf.sprintf "eq_binop: %s does not equal %s"
        (print_binop binop) (print_binop binop')
      |> print_endline;
      false


(* Types *)

let rec eq_type (typ: Type.t) (typ': Type.t) =
  match typ, typ' with
  | Bool _, Bool _ -> true
  | Error _, Error _ -> true
  | Integer _, Integer _ -> true
  | IntType { expr = expr; _ },
    IntType { expr = expr'; _ } ->
      eq_expr expr expr'
  | BitType { expr = expr; _ },
    BitType { expr = expr'; _ } ->
      eq_expr expr expr'
  | VarBit { expr = expr; _ },
    VarBit { expr = expr'; _ } ->
      eq_expr expr expr'
  | TypeName { name = name; _ },
    TypeName { name = name'; _ } ->
      eq_name name name'
  | SpecializedType { base = base; args = args; _ },
    SpecializedType { base = base'; args = args'; _ } ->
      eq_type base base' &&
      eq_list eq_type args args'
  | HeaderStack { header = header; size = size; _ },
    HeaderStack { header = header'; size = size'; _ } ->
      eq_type header header' &&
      eq_expr size size'
  | Tuple { args = args; _ },
    Tuple { args = args'; _ } ->
      eq_list eq_type args args'
  | String _, String _ -> true
  | Void _, Void _ -> true
  | DontCare _, DontCare _ -> true
  | _ ->
      Printf.sprintf "eq_type: %s does not equal %s"
        (print_type typ) (print_type typ')
      |> print_endline;
      false


(* Arguments and Prameters *)

and eq_arg (arg: Argument.t) (arg': Argument.t) =
  match arg, arg' with
  | Expression { value = value; _ },
    Expression { value = value'; _ } ->
      eq_expr value value'
  | KeyValue { key = key; value = value; _ },
    KeyValue { key = key'; value = value'; _ } ->
      eq_text key key' &&
      eq_expr value value'
  | Missing _ , Missing _ -> true
  | _ ->
      Printf.sprintf "eq_arg: %s does not equal %s"
        (print_arg arg) (print_arg arg')
      |> print_endline;
      false

and eq_param (param: Parameter.t) (param': Parameter.t) =
  let res =
    eq_option eq_direction param.direction param'.direction &&
    eq_type param.typ param'.typ &&
    eq_text param.variable param'.variable &&
    eq_option eq_expr param.opt_value param'.opt_value
  in
  if not res then
    Printf.sprintf "eq_param: %s does not equal %s"
      (print_param param) (print_param param')
    |> print_endline;
  res


(* Expressions *)

and eq_key_value (key_value: KeyValue.t) (key_value': KeyValue.t) =
  let res =
    eq_text key_value.key key_value'.key &&
    eq_expr key_value.value key_value'.value
  in
  if not res then
    Printf.sprintf "eq_key_value: %s does not equal %s"
      (print_key_value key_value) (print_key_value key_value')
    |> print_endline;
  res

and eq_expr (expr: Expression.t) (expr': Expression.t) =
  match expr, expr' with
  | True _, True _ -> true
  | False _, False _ -> true
  | Int { i = i; _ },
    Int { i = i'; _ } ->
      eq_number i i'
  | String { text = text; _ },
    String { text = text'; _ } ->
      eq_text text text'
  | Name { name = name; _ },
    Name { name = name'; _ } ->
      eq_name name name'
  | ArrayAccess { array = array; index = index; _ },
    ArrayAccess { array = array'; index = index'; _ } ->
      eq_expr array array' &&
      eq_expr index index'
  | BitStringAccess { bits = bits; lo = lo; hi = hi; _ },
    BitStringAccess { bits = bits'; lo = lo'; hi = hi'; _ } ->
      eq_expr bits bits' &&
      eq_expr lo lo' &&
      eq_expr hi hi'
  | List { values = values; _ },
    List { values = values'; _ } ->
      eq_list eq_expr values values'
  | Record { entries = entries; _ },
    Record { entries = entries'; _ } ->
      eq_list eq_key_value entries entries'
  | UnaryOp { op = op; arg = arg; _ },
    UnaryOp { op = op'; arg = arg'; _ } ->
      eq_unop op op' &&
      eq_expr arg arg'
  | BinaryOp { op = op; args = (arg0, arg1); _ },
    BinaryOp { op = op'; args = (arg0', arg1'); _ } ->
      eq_binop op op' &&
      eq_expr arg0 arg0' &&
      eq_expr arg1 arg1'
  | Cast { typ = typ; expr = expr; _ },
    Cast { typ = typ'; expr = expr'; _ } ->
      eq_type typ typ' &&
      eq_expr expr expr'
  | TypeMember { typ = typ; name = name; _ },
    TypeMember { typ = typ'; name = name'; _ } ->
      eq_name typ typ' &&
      eq_text name name'
  | ErrorMember { err = err; _ },
    ErrorMember { err = err'; _ } ->
      eq_text err err'
  | ExpressionMember { expr = expr; name = name; _ },
    ExpressionMember { expr = expr'; name = name'; _ } ->
      eq_expr expr expr' &&
      eq_text name name'
  | Ternary { cond = cond; tru = tru; fls = fls; _ },
    Ternary { cond = cond'; tru = tru'; fls = fls'; _ } ->
      eq_expr cond cond' &&
      eq_expr tru tru' &&
      eq_expr fls fls'
  | FunctionCall { func = func; type_args = type_args; args = args; _ },
    FunctionCall { func = func'; type_args = type_args'; args = args'; _ } ->
      eq_expr func func' &&
      eq_list eq_type type_args type_args' &&
      eq_list eq_arg args args'
  | NamelessInstantiation { typ = typ; args = args; _ },
    NamelessInstantiation { typ = typ'; args = args'; _ } ->
      eq_type typ typ' &&
      eq_list eq_arg args args'
  | Mask { expr = expr; mask = mask; _ },
    Mask { expr = expr'; mask = mask'; _ } ->
      eq_expr expr expr' &&
      eq_expr mask mask'
  | Range { lo = lo; hi = hi; _ },
    Range { lo = lo'; hi = hi'; _ } ->
      eq_expr lo lo' &&
      eq_expr hi hi'
  | _ ->
      Printf.sprintf "eq_expr: %s does not equal %s"
        (print_expr expr) (print_expr expr')
      |> print_endline;
      Printf.sprintf "lhs %s" (dbg expr) |> print_endline;
      Printf.sprintf "rhs %s" (dbg expr') |> print_endline;
      false


(* Statements *)

and eq_switch_label (label: Statement.switch_label) (label': Statement.switch_label) =
  match label, label' with
  | Default _, Default _ -> true
  | Name { name = name; _ },
    Name { name = name'; _ } ->
      eq_text name name'
  | _ ->
      Printf.sprintf "eq_switch_label: %s does not equal %s"
        (print_switch_label label) (print_switch_label label')
      |> print_endline;
      false

and eq_switch_case (case: Statement.switch_case) (case': Statement.switch_case) =
  match case, case' with
  | Action { label = label; code = code; _ },
    Action { label = label'; code = code'; _ } ->
      eq_switch_label label label' &&
      eq_block code code'
  | FallThrough { label = label; _ },
    FallThrough { label = label'; _ } ->
      eq_switch_label label label'
  | _ ->
      Printf.sprintf "eq_switch_case: %s does not equal %s"
        (print_switch_case 0 case) (print_switch_case 0 case')
      |> print_endline;
      false

and eq_block (block: Block.t) (block': Block.t) =
  let res = eq_list eq_stmt block.statements block'.statements in
  if not res then
    Printf.sprintf "eq_block: %s does not equal %s"
      (print_block 0 block) (print_block 0 block')
    |> print_endline;
  res

and eq_stmt (stmt: Statement.t) (stmt': Statement.t) =
  match stmt, stmt' with
  | MethodCall { func = func; type_args = type_args; args = args; _ },
    MethodCall { func = func'; type_args = type_args'; args = args'; _ } ->
      eq_expr func func' &&
      eq_list eq_type type_args type_args' &&
      eq_list eq_arg args args'
  | Assignment { lhs = lhs; rhs = rhs; _ },
    Assignment { lhs = lhs'; rhs = rhs'; _ } ->
      eq_expr lhs lhs' &&
      eq_expr rhs rhs'
  | DirectApplication { typ = typ; args = args; _ },
    DirectApplication { typ = typ'; args = args'; _ } ->
      eq_type typ typ' &&
      eq_list eq_arg args args'
  | Conditional { cond = cond; tru = tru; fls = fls; _ },
    Conditional { cond = cond'; tru = tru'; fls = fls'; _ } ->
      eq_expr cond cond' &&
      eq_stmt tru tru' &&
      eq_option eq_stmt fls fls'
  | BlockStatement { block = block; _ },
    BlockStatement { block = block'; _ } ->
      eq_block block block'
  | Exit _ , Exit _ -> true
  | EmptyStatement _, EmptyStatement _ -> true
  | Return { expr = expr; _ },
    Return { expr = expr'; _ } ->
      eq_option eq_expr expr expr'
  | Switch { expr = expr; cases = cases; _ },
    Switch { expr = expr'; cases = cases'; _ } ->
      eq_expr expr expr' &&
      eq_list eq_switch_case cases cases'
  | DeclarationStatement { decl = decl; _ },
    DeclarationStatement { decl = decl'; _ } ->
      eq_decl decl decl'
  | _ ->
      Printf.sprintf "eq_stmt: %s does not equal %s"
        (print_stmt 0 stmt) (print_stmt 0 stmt')
      |> print_endline;
      false


(* Matches *)

and eq_match (mtch: Match.t) (mtch': Match.t) =
  match mtch, mtch' with
  | Default _, Default _ -> true
  | DontCare _, DontCare _ -> true
  | Expression { expr = expr; _ },
    Expression { expr = expr'; _ } ->
      eq_expr expr expr'
  | _ ->
      Printf.sprintf "eq_match: %s does not equal %s"
        (print_match mtch) (print_match mtch')
      |> print_endline;
      false


(* Parsers *)

and eq_parser_case (case: Parser.case) (case': Parser.case) =
  let res =
    eq_list eq_match case.matches case'.matches &&
    eq_text case.next case'.next
  in
  if not res then
    Printf.sprintf "eq_parser_case: %s does not equal %s"
      (print_parser_case 0 case) (print_parser_case 0 case')
    |> print_endline;
  res

and eq_parser_transition (transition: Parser.transition) (transition': Parser.transition) =
  match transition, transition' with
  | Direct { next = next; _ },
    Direct { next = next'; _ } ->
      eq_text next next'
  | Select { exprs = exprs; cases = cases; _ },
    Select { exprs = exprs'; cases = cases'; _ } ->
      eq_list eq_expr exprs exprs' &&
      eq_list eq_parser_case cases cases'
  | _ ->
      Printf.sprintf "eq_parser_transition: %s does not equal %s"
        (print_parser_transition 0 transition) (print_parser_transition 0 transition')
      |> print_endline;
      false

and eq_parser_state (state: Parser.state) (state': Parser.state) =
  let res =
    eq_text state.name state'.name &&
    eq_list eq_stmt state.statements state'.statements &&
    eq_parser_transition state.transition state'.transition
  in
  if not res then
    Printf.sprintf "eq_parser_state: %s does not equal %s"
      (print_parser_state 0 state) (print_parser_state 0 state')
    |> print_endline;
  res


(* Tables *)

and eq_table_action_ref (action_ref: Table.action_ref) (action_ref': Table.action_ref) =
  let res =
    eq_name action_ref.name action_ref'.name &&
    eq_list eq_arg action_ref.args action_ref'.args
  in
  if not res then
    Printf.sprintf "eq_table_action_ref: %s does not equal %s"
      (print_table_action_ref action_ref) (print_table_action_ref action_ref')
    |> print_endline;
  res

and eq_table_key (key: Table.key) (key': Table.key) =
  let res =
    eq_expr key.key key'.key &&
    eq_text key.match_kind key'.match_kind
  in
  if not res then
    Printf.sprintf "eq_table_key: %s does not equal %s"
      (print_table_key key) (print_table_key key')
    |> print_endline;
  res

and eq_table_entry (entry: Table.entry) (entry': Table.entry) =
  let res =
    eq_list eq_match entry.matches entry'.matches &&
    eq_table_action_ref entry.action entry'.action
  in
  if not res then
    Printf.sprintf "eq_table_entry: %s does not equal %s"
      (print_table_entry entry) (print_table_entry entry')
    |> print_endline;
  res

and eq_table_property (property: Table.property) (property': Table.property) =
  match property, property' with
  | Key { keys = keys; _ },
    Key { keys = keys'; _ } ->
      eq_list eq_table_key keys keys'
  | Actions { actions = actions; _ },
    Actions { actions = actions'; _ } ->
      eq_list eq_table_action_ref actions actions'
  | Entries { entries = entries; _ },
    Entries { entries = entries'; _ } ->
      eq_list eq_table_entry entries entries'
  | DefaultAction { action = action; const = const; _ },
    DefaultAction { action = action'; const = const'; _ } ->
      eq_table_action_ref action action' &&
      const = const'
  | Custom { const = const; name = name; value = value; _ },
    Custom { const = const'; name = name'; value = value'; _ } ->
      const = const' &&
      eq_text name name' &&
      eq_expr value value'
  | _ ->
      Printf.sprintf "eq_table_property: %s does not equal %s"
        (print_table_property 0 property) (print_table_property 0 property')
      |> print_endline;
      false


(* Methods *)

and eq_method_prototype (methodproto: MethodPrototype.t) (methodproto': MethodPrototype.t) =
  match methodproto, methodproto' with
  | Constructor { name = name; params = params; _ },
    Constructor { name = name'; params = params'; _ } ->
      eq_text name name' &&
      eq_list eq_param params params'
  | AbstractMethod { return = return; name = name;
                     type_params = type_params; params = params; _ },
    AbstractMethod { return = return'; name = name';
                     type_params = type_params'; params = params'; _ } ->
      eq_type return return' &&
      eq_text name name' &&
      eq_list eq_text type_params type_params' &&
      eq_list eq_param params params'
  | Method { return = return; name = name;
             type_params = type_params; params = params; _ },
    Method { return = return'; name = name';
             type_params = type_params'; params = params'; _ } ->
      eq_type return return' &&
      eq_text name name' &&
      eq_list eq_text type_params type_params' &&
      eq_list eq_param params params'
  | _ ->
      Printf.sprintf "eq_method_prototype: %s does not equal %s"
        (print_method_prototype 0 methodproto) (print_method_prototype 0 methodproto')
      |> print_endline;
      false


(* Declarations *)

and eq_decl_field (field: Declaration.field) (field': Declaration.field) =
  let res =
    eq_type field.typ field'.typ &&
    eq_text field.name field'.name
  in
  if not res then
    Printf.sprintf "eq_decl_field: %s does not equal %s"
      (print_decl_field 0 field) (print_decl_field 0 field')
    |> print_endline;
  res

and eq_decl (decl: Declaration.t) (decl': Declaration.t) =
  match decl, decl' with
  | Constant { typ = typ; name = name; value = value; _ },
    Constant { typ = typ'; name = name'; value = value'; _ } ->
      eq_type typ typ' &&
      eq_text name name' &&
      eq_expr value value'
  | Instantiation { typ = typ; args = args; name = name; init = init; _ },
    Instantiation { typ = typ'; args = args'; name = name'; init = init'; _ } ->
      eq_type typ typ' &&
      eq_list eq_arg args args' &&
      eq_text name name' &&
      eq_option eq_block init init'
  | Parser { name = name; type_params = type_params; params = params;
             constructor_params = cons_params; locals = locals; states = states; _ },
    Parser { name = name'; type_params = type_params'; params = params';
             constructor_params = cons_params'; locals = locals'; states = states'; _ } ->
      eq_text name name' &&
      eq_list eq_text type_params type_params' &&
      eq_list eq_param params params' &&
      eq_list eq_param cons_params cons_params' &&
      eq_list eq_decl locals locals' &&
      eq_list eq_parser_state states states'
  | Control { name = name; type_params = type_params; params = params;
              constructor_params = cons_params; locals = locals; apply = apply; _ },
    Control { name = name'; type_params = type_params'; params = params';
              constructor_params = cons_params'; locals = locals'; apply = apply'; _ } ->
      eq_text name name' &&
      eq_list eq_text type_params type_params' &&
      eq_list eq_param params params' &&
      eq_list eq_param cons_params cons_params' &&
      eq_list eq_decl locals locals' &&
      eq_block apply apply'
  | Function { return = return; name = name;
               type_params = type_params; params = params; body = body; _ },
    Function { return = return'; name = name';
               type_params = type_params'; params = params'; body = body'; _ } ->
      eq_type return return' &&
      eq_text name name' &&
      eq_list eq_text type_params type_params' &&
      eq_list eq_param params params' &&
      eq_block body body'
  | ExternFunction { return = return; name = name;
                     type_params = type_params; params = params; _ },
    ExternFunction { return = return'; name = name';
                    type_params = type_params'; params = params'; _ } ->
      eq_type return return' &&
      eq_text name name' &&
      eq_list eq_text type_params type_params' &&
      eq_list eq_param params params'
  | Variable { typ = typ; name = name; init = init; _ },
    Variable { typ = typ'; name = name'; init = init'; _ } ->
      eq_type typ typ' &&
      eq_text name name' &&
      eq_option eq_expr init init'
  | ValueSet { typ = typ; size = size; name = name; _ },
    ValueSet { typ = typ'; size = size'; name = name'; _ } ->
      eq_type typ typ' &&
      eq_expr size size' &&
      eq_text name name'
  | Action { name = name; params = params; body = body; _ },
    Action { name = name'; params = params'; body = body'; _ } ->
      eq_text name name' &&
      eq_list eq_param params params' &&
      eq_block body body'
  | Table { name = name; properties = properties; _ },
    Table { name = name'; properties = properties'; _ } ->
      eq_text name name' &&
      eq_list eq_table_property properties properties'
  | Header { name = name; fields = fields; _ },
    Header { name = name'; fields = fields'; _ } ->
      eq_text name name' &&
      eq_list eq_decl_field fields fields'
  | HeaderUnion { name = name; fields = fields; _ },
    HeaderUnion { name = name'; fields = fields'; _ } ->
      eq_text name name' &&
      eq_list eq_decl_field fields fields'
  | Struct { name = name; fields = fields; _ },
    Struct { name = name'; fields = fields'; _ } ->
      eq_text name name' &&
      eq_list eq_decl_field fields fields'
  | Error { members = members; _ },
    Error { members = members'; _ } ->
      eq_list eq_text members members'
  | MatchKind { members = members; _ },
    MatchKind { members = members'; _ } ->
      eq_list eq_text members members'
  | Enum { name = name; members = members; _ },
    Enum { name = name'; members = members'; _ } ->
      eq_text name name' &&
      eq_list eq_text members members'
  | SerializableEnum { typ = typ; name = name; members = members; _ },
    SerializableEnum { typ = typ'; name = name'; members = members'; _ } ->
      let eq_member (member: (Text.t * Expression.t)) (member': (Text.t * Expression.t)) =
        let (text, expr) = member in
        let (text', expr') = member' in
        eq_text text text' &&
        eq_expr expr expr'
      in
      eq_type typ typ' &&
      eq_text name name' &&
      eq_list eq_member members members'
  | ExternObject { name = name; type_params = type_params; methods = methods; _ },
    ExternObject { name = name'; type_params = type_params'; methods = methods'; _ } ->
      eq_text name name' &&
      eq_list eq_text type_params type_params' &&
      eq_list eq_method_prototype methods methods'
  | TypeDef { name = name; typ_or_decl = typ_or_decl; _ },
    TypeDef { name = name'; typ_or_decl = typ_or_decl'; _ } ->
      eq_text name name' &&
      eq_alternative eq_type eq_decl typ_or_decl typ_or_decl'
  | NewType { name = name; typ_or_decl = typ_or_decl; _ },
    NewType { name = name'; typ_or_decl = typ_or_decl'; _ } ->
      eq_text name name' &&
      eq_alternative eq_type eq_decl typ_or_decl typ_or_decl'
  | ControlType { name = name; type_params = type_params; params = params; _ },
    ControlType { name = name'; type_params = type_params'; params = params'; _ } ->
      eq_text name name' &&
      eq_list eq_text type_params type_params' &&
      eq_list eq_param params params'
  | ParserType { name = name; type_params = type_params; params = params; _ },
    ParserType { name = name'; type_params = type_params'; params = params'; _ } ->
      eq_text name name' &&
      eq_list eq_text type_params type_params' &&
      eq_list eq_param params params'
  | PackageType { name = name; type_params = type_params; params = params; _ },
    PackageType { name = name'; type_params = type_params'; params = params'; _ } ->
      eq_text name name' &&
      eq_list eq_text type_params type_params' &&
      eq_list eq_param params params'
  | _ ->
      Printf.sprintf "eq_decl: %s does not equal %s"
        (print_decl 0 decl) (print_decl 0 decl')
      |> print_endline;
      false


(* Program *)

and eq_program (program: program) (program': program) =
  let Program decls = program in
  let Program decls' = program' in
  eq_list eq_decl decls decls'
