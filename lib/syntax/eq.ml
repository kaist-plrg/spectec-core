open Ast
open Utils
open Alternative

(* Utils *)

let eq_option (eq: 'a -> 'a -> bool) (opt1: 'a option) (opt2: 'a option) =
  match opt1, opt2 with
  | None, None -> true
  | Some x, Some y -> eq x y
  | _ -> false

let eq_list (eq: 'a -> 'a -> bool) (lst1: 'a list) (lst2: 'a list) =
  List.length lst1 = List.length lst2 &&
  List.for_all2 eq lst1 lst2

let eq_alternative (eq_a: 'a -> 'a -> bool) (eq_b: 'b -> 'b -> bool) (alt1: ('a, 'b) alternative) (alt2: ('a, 'b) alternative) =
  match alt1, alt2 with
  | Left x, Left y -> eq_a x y
  | Right x, Right y -> eq_b x y
  | _ -> false


let eq_text (text1: Text.t) (text2: Text.t) =
  text1.str = text2.str

let eq_number (number1: Number.t) (number2: Number.t) =
  number1.value = number2.value &&
  match number1.width_signed, number2.width_signed with
  | None, None -> true
  | Some (width1, signed1), Some (width2, signed2) ->
      width1 = width2 && signed1 = signed2
  | _ -> false

let eq_name (name1: Name.t) (name2: Name.t) =
  match name1, name2 with
  | BareName name1, BareName name2 ->
      eq_text name1 name2
  | QualifiedName (prefix1, name1), QualifiedName (prefix2, name2) ->
      eq_list eq_text prefix1 prefix2 &&
      eq_text name1 name2
  | _ -> false

let eq_direction (dir1: Direction.t) (dir2: Direction.t) =
  match dir1, dir2 with
  | In _, In _
  | Out _, Out _
  | InOut _, InOut _ -> true
  | _ -> false


(* Operators *)

let eq_unop (unop1: Op.un) (unop2: Op.un) =
  match unop1, unop2 with
  | Not _ , Not _ -> true
  | BitNot _, BitNot _ -> true
  | UMinus _, UMinus _ -> true
  | _ -> false

let eq_binop (binop1: Op.bin) (binop2: Op.bin) =
  match binop1, binop2 with
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
  | _ -> false


(* Types *)

let rec eq_type (typ1: Type.t) (typ2: Type.t) =
  match typ1, typ2 with
  | Bool _, Bool _ -> true
  | Error _, Error _ -> true
  | Integer _, Integer _ -> true
  | IntType { expr = expr1; _ },
    IntType { expr = expr2; _ } ->
      eq_expr expr1 expr2
  | BitType { expr = expr1; _ },
    BitType { expr = expr2; _ } ->
      eq_expr expr1 expr2
  | VarBit { expr = expr1; _ },
    VarBit { expr = expr2; _ } ->
      eq_expr expr1 expr2
  | TypeName { name = name1; _ },
    TypeName { name = name2; _ } ->
      eq_name name1 name2
  | HeaderStack { header = header1; size = size1; _ },
    HeaderStack { header = header2; size = size2; _ } ->
      eq_type header1 header2 &&
      eq_expr size1 size2
  | Tuple { args = args1; _ },
    Tuple { args = args2; _ } ->
      eq_list eq_type args1 args2
  | String _, String _ -> true
  | Void _, Void _ -> true
  | DontCare _, DontCare _ -> true
  | _ -> false


(* Arguments and Prameters *)

and eq_arg (arg1: Argument.t) (arg2: Argument.t) =
  match arg1, arg2 with
  | Expression { value = value1; _ },
    Expression { value = value2; _ } ->
      eq_expr value1 value2
  | KeyValue { key = key1; value = value1; _ },
    KeyValue { key = key2; value = value2; _ } ->
      eq_text key1 key2 &&
      eq_expr value1 value2
  | Missing _ , Missing _ -> true
  | _ -> false

and eq_param (param1: Parameter.t) (param2: Parameter.t) =
  eq_option eq_direction param1.direction param2.direction &&
  eq_type param1.typ param2.typ &&
  eq_text param1.variable param2.variable &&
  eq_option eq_expr param1.opt_value param2.opt_value


(* Expressions *)

and eq_key_value (key_value1: KeyValue.t) (key_value2: KeyValue.t) =
  eq_text key_value1.key key_value2.key &&
  eq_expr key_value1.value key_value2.value

and eq_expr (expr1: Expression.t) (expr2: Expression.t) =
  match expr1, expr2 with
  | True _, True _ -> true
  | False _, False _ -> true
  | Int { i = i1; _ },
    Int { i = i2; _ } ->
      eq_number i1 i2
  | String { text = text1; _ },
    String { text = text2; _ } ->
      eq_text text1 text2
  | Name { name = name1; _ },
    Name { name = name2; _ } ->
      eq_name name1 name2
  | ArrayAccess { array = array1; index = index1; _ },
    ArrayAccess { array = array2; index = index2; _ } ->
      eq_expr array1 array2 &&
      eq_expr index1 index2
  | BitStringAccess { bits = bits1; lo = lo1; hi = hi1; _ },
    BitStringAccess { bits = bits2; lo = lo2; hi = hi2; _ } ->
      eq_expr bits1 bits2 &&
      eq_expr lo1 lo2 &&
      eq_expr hi1 hi2
  | List { values = values1; _ },
    List { values = values2; _ } ->
      eq_list eq_expr values1 values2
  | Record { entries = entries1; _ },
    Record { entries = entries2; _ } ->
      eq_list eq_key_value entries1 entries2
  | UnaryOp { op = op1; arg = arg1; _ },
    UnaryOp { op = op2; arg = arg2; _ } ->
      eq_unop op1 op2 &&
      eq_expr arg1 arg2
  | BinaryOp { op = op1; args = (args11, args12); _ },
    BinaryOp { op = op2; args = (args21, args22); _ } ->
      eq_binop op1 op2 &&
      eq_expr args11 args21 &&
      eq_expr args12 args22
  | Cast { typ = typ1; expr = expr1; _ },
    Cast { typ = typ2; expr = expr2; _ } ->
      eq_type typ1 typ2 &&
      eq_expr expr1 expr2
  | TypeMember { typ = typ1; name = name1; _ },
    TypeMember { typ = typ2; name = name2; _ } ->
      eq_name typ1 typ2 &&
      eq_text name1 name2
  | ErrorMember { err = err1; _ },
    ErrorMember { err = err2; _ } ->
      eq_text err1 err2
  | ExpressionMember { expr = expr1; name = name1; _ },
    ExpressionMember { expr = expr2; name = name2; _ } ->
      eq_expr expr1 expr2 &&
      eq_text name1 name2
  | Ternary { cond = cond1; tru = tru1; fls = fls1; _ },
    Ternary { cond = cond2; tru = tru2; fls = fls2; _ } ->
      eq_expr cond1 cond2 &&
      eq_expr tru1 tru2 &&
      eq_expr fls1 fls2
  | FunctionCall { func = func1; type_args = type_args1; args = args1; _ },
    FunctionCall { func = func2; type_args = type_args2; args = args2; _ } ->
      eq_expr func1 func2 &&
      eq_list eq_type type_args1 type_args2 &&
      eq_list eq_arg args1 args2
  | NamelessInstantiation { typ = typ1; args = args1; _ },
    NamelessInstantiation { typ = typ2; args = args2; _ } ->
      eq_type typ1 typ2 &&
      eq_list eq_arg args1 args2
  | Mask { expr = expr1; mask = mask1; _ },
    Mask { expr = expr2; mask = mask2; _ } ->
      eq_expr expr1 expr2 &&
      eq_expr mask1 mask2
  | Range { lo = lo1; hi = hi1; _ },
    Range { lo = lo2; hi = hi2; _ } ->
      eq_expr lo1 lo2 &&
      eq_expr hi1 hi2
  | _ -> false


(* Statements *)

and eq_switch_label (label1: Statement.switch_label) (label2: Statement.switch_label) =
  match label1, label2 with
  | Default _, Default _ -> true
  | Name { name = name1; _ },
    Name { name = name2; _ } ->
      eq_text name1 name2
  | _ -> false

and eq_switch_case (case1: Statement.switch_case) (case2: Statement.switch_case) =
  match case1, case2 with
  | Action { label = label1; code = code1; _ },
    Action { label = label2; code = code2; _ } ->
      eq_switch_label label1 label2 &&
      eq_block code1 code2
  | FallThrough { label = label1; _ },
    FallThrough { label = label2; _ } ->
      eq_switch_label label1 label2
  | _ -> false

and eq_block (block1: Block.t) (block2: Block.t) =
  eq_list eq_stmt block1.statements block2.statements

and eq_stmt (stmt1: Statement.t) (stmt2: Statement.t) =
  match stmt1, stmt2 with
  | MethodCall { func = func1; type_args = type_args1; args = args1; _ },
    MethodCall { func = func2; type_args = type_args2; args = args2; _ } ->
      eq_expr func1 func2 &&
      eq_list eq_type type_args1 type_args2 &&
      eq_list eq_arg args1 args2
  | Assignment { lhs = lhs1; rhs = rhs1; _ },
    Assignment { lhs = lhs2; rhs = rhs2; _ } ->
      eq_expr lhs1 lhs2 &&
      eq_expr rhs1 rhs2
  | DirectApplication { typ = typ1; args = args1; _ },
    DirectApplication { typ = typ2; args = args2; _ } ->
      eq_type typ1 typ2 &&
      eq_list eq_arg args1 args2
  | Conditional { cond = cond1; tru = tru1; fls = fls1; _ },
    Conditional { cond = cond2; tru = tru2; fls = fls2; _ } ->
      eq_expr cond1 cond2 &&
      eq_stmt tru1 tru2 &&
      eq_option eq_stmt fls1 fls2
  | BlockStatement { block = block1; _ },
    BlockStatement { block = block2; _ } ->
      eq_block block1 block2
  | Exit _ , Exit _ -> true
  | EmptyStatement _, EmptyStatement _ -> true
  | Return { expr = expr1; _ },
    Return { expr = expr2; _ } ->
      eq_option eq_expr expr1 expr2
  | Switch { expr = expr1; cases = cases1; _ },
    Switch { expr = expr2; cases = cases2; _ } ->
      eq_expr expr1 expr2 &&
      eq_list eq_switch_case cases1 cases2
  | DeclarationStatement { decl = decl1; _ },
    DeclarationStatement { decl = decl2; _ } ->
      eq_decl decl1 decl2
  | _ -> false


(* Matches *)

and eq_match (mtch1: Match.t) (mtch2: Match.t) =
  match mtch1, mtch2 with
  | Default _, Default _ -> true
  | DontCare _, DontCare _ -> true
  | Expression { expr = expr1; _ },
    Expression { expr = expr2; _ } ->
      eq_expr expr1 expr2
  | _ -> false


(* Parsers *)

and eq_parser_case (case1: Parser.case) (case2: Parser.case) =
  eq_list eq_match case1.matches case2.matches &&
  eq_text case1.next case2.next

and eq_parser_transition (transition1: Parser.transition) (transition2: Parser.transition) =
  match transition1, transition2 with
  | Direct { next = next1; _ },
    Direct { next = next2; _ } ->
      eq_text next1 next2
  | Select { exprs = exprs1; cases = cases1; _ },
    Select { exprs = exprs2; cases = cases2; _ } ->
      eq_list eq_expr exprs1 exprs2 &&
      eq_list eq_parser_case cases1 cases2
  | _ -> false

and eq_parser_state (state1: Parser.state) (state2: Parser.state) =
  eq_text state1.name state2.name &&
  eq_list eq_stmt state1.statements state2.statements &&
  eq_parser_transition state1.transition state2.transition


(* Tables *)

and eq_table_action_ref (action_ref1: Table.action_ref) (action_ref2: Table.action_ref) =
  eq_name action_ref1.name action_ref2.name &&
  eq_list eq_arg action_ref1.args action_ref2.args

and eq_table_key (key1: Table.key) (key2: Table.key) =
  eq_expr key1.key key2.key &&
  eq_text key1.match_kind key2.match_kind

and eq_table_entry (entry1: Table.entry) (entry2: Table.entry) =
  eq_list eq_match entry1.matches entry2.matches &&
  eq_table_action_ref entry1.action entry2.action

and eq_table_property (property1: Table.property) (property2: Table.property) =
  match property1, property2 with
  | Key { keys = keys1; _ },
    Key { keys = keys2; _ } ->
      eq_list eq_table_key keys1 keys2
  | Actions { actions = actions1; _ },
    Actions { actions = actions2; _ } ->
      eq_list eq_table_action_ref actions1 actions2
  | Entries { entries = entries1; _ },
    Entries { entries = entries2; _ } ->
      eq_list eq_table_entry entries1 entries2
  | DefaultAction { action = action1; const = const1; _ },
    DefaultAction { action = action2; const = const2; _ } ->
      eq_table_action_ref action1 action2 &&
      const1 = const2
  | Custom { const = const1; name = name1; value = value1; _ },
    Custom { const = const2; name = name2; value = value2; _ } ->
      const1 = const2 &&
      eq_text name1 name2 &&
      eq_expr value1 value2
  | _ -> false


(* Methods *)

and eq_method_prototype (methodproto1: MethodPrototype.t) (methodproto2: MethodPrototype.t) =
  match methodproto1, methodproto2 with
  | Constructor { name = name1; params = params1; _ },
    Constructor { name = name2; params = params2; _ } ->
      eq_text name1 name2 &&
      eq_list eq_param params1 params2
  | AbstractMethod { return = return1; name = name1; type_params = type_params1; params = params1; _ },
    AbstractMethod { return = return2; name = name2; type_params = type_params2; params = params2; _ } ->
      eq_type return1 return2 &&
      eq_text name1 name2 &&
      eq_list eq_text type_params1 type_params2 &&
      eq_list eq_param params1 params2
  | Method { return = return1; name = name1; type_params = type_params1; params = params1; _ },
    Method { return = return2; name = name2; type_params = type_params2; params = params2; _ } ->
      eq_type return1 return2 &&
      eq_text name1 name2 &&
      eq_list eq_text type_params1 type_params2 &&
      eq_list eq_param params1 params2
  | _ -> false


(* Declarations *)

and eq_decl_field (field1: Declaration.field) (field2: Declaration.field) =
  eq_type field1.typ field2.typ &&
  eq_text field1.name field2.name

and eq_decl (decl1: Declaration.t) (decl2: Declaration.t) =
  match decl1, decl2 with
  | Constant { typ = typ1; name = name1; value = value1; _ },
    Constant { typ = typ2; name = name2; value = value2; _ } ->
      eq_type typ1 typ2 &&
      eq_text name1 name2 &&
      eq_expr value1 value2
  | Instantiation { typ = typ1; args = args1; name = name1; init = init1; _ },
    Instantiation { typ = typ2; args = args2; name = name2; init = init2; _ } ->
      eq_type typ1 typ2 &&
      eq_list eq_arg args1 args2 &&
      eq_text name1 name2 &&
      eq_option eq_block init1 init2
  | Parser { name = name1; type_params = type_params1; params = params1; constructor_params = cons_params1; locals = locals1; states = states1; _ },
    Parser { name = name2; type_params = type_params2; params = params2; constructor_params = cons_params2; locals = locals2; states = states2; _ } ->
      eq_text name1 name2 &&
      eq_list eq_text type_params1 type_params2 &&
      eq_list eq_param params1 params2 &&
      eq_list eq_param cons_params1 cons_params2 &&
      eq_list eq_decl locals1 locals2 &&
      eq_list eq_parser_state states1 states2
  | Control { name = name1; type_params = type_params1; params = params1; constructor_params = cons_params1; locals = locals1; apply = apply1; _ },
    Control { name = name2; type_params = type_params2; params = params2; constructor_params = cons_params2; locals = locals2; apply = apply2; _ } ->
      eq_text name1 name2 &&
      eq_list eq_text type_params1 type_params2 &&
      eq_list eq_param params1 params2 &&
      eq_list eq_param cons_params1 cons_params2 &&
      eq_list eq_decl locals1 locals2 &&
      eq_block apply1 apply2
  | Function { return = return1; name = name1; type_params = type_params1; params = params1; body = body1; _ },
    Function { return = return2; name = name2; type_params = type_params2; params = params2; body = body2; _ } ->
      eq_type return1 return2 &&
      eq_text name1 name2 &&
      eq_list eq_text type_params1 type_params2 &&
      eq_list eq_param params1 params2 &&
      eq_block body1 body2
  | ExternFunction { return = return1; name = name1; type_params = type_params1; params = params1; _ },
    ExternFunction { return = return2; name = name2; type_params = type_params2; params = params2; _ } ->
      eq_type return1 return2 &&
      eq_text name1 name2 &&
      eq_list eq_text type_params1 type_params2 &&
      eq_list eq_param params1 params2
  | Variable { typ = typ1; name = name1; init = init1; _ },
    Variable { typ = typ2; name = name2; init = init2; _ } ->
      eq_type typ1 typ2 &&
      eq_text name1 name2 &&
      eq_option eq_expr init1 init2
  | ValueSet { typ = typ1; size = size1; name = name1; _ },
    ValueSet { typ = typ2; size = size2; name = name2; _ } ->
      eq_type typ1 typ2 &&
      eq_expr size1 size2 &&
      eq_text name1 name2
  | Action { name = name1; params = params1; body = body1; _ },
    Action { name = name2; params = params2; body = body2; _ } ->
      eq_text name1 name2 &&
      eq_list eq_param params1 params2 &&
      eq_block body1 body2
  | Table { name = name1; properties = properties1; _ },
    Table { name = name2; properties = properties2; _ } ->
      eq_text name1 name2 &&
      eq_list eq_table_property properties1 properties2
  | Header { name = name1; fields = fields1; _ },
    Header { name = name2; fields = fields2; _ } ->
      eq_text name1 name2 &&
      eq_list eq_decl_field fields1 fields2
  | HeaderUnion { name = name1; fields = fields1; _ },
    HeaderUnion { name = name2; fields = fields2; _ } ->
      eq_text name1 name2 &&
      eq_list eq_decl_field fields1 fields2
  | Struct { name = name1; fields = fields1; _ },
    Struct { name = name2; fields = fields2; _ } ->
      eq_text name1 name2 &&
      eq_list eq_decl_field fields1 fields2
  | Error { members = members1; _ },
    Error { members = members2; _ } ->
      eq_list eq_text members1 members2
  | MatchKind { members = members1; _ },
    MatchKind { members = members2; _ } ->
      eq_list eq_text members1 members2
  | Enum { name = name1; members = members1; _ },
    Enum { name = name2; members = members2; _ } ->
      eq_text name1 name2 &&
      eq_list eq_text members1 members2
  | SerializableEnum { typ = typ1; name = name1; members = members1; _ },
    SerializableEnum { typ = typ2; name = name2; members = members2; _ } ->
      let eq_member (member1: (Text.t * Expression.t)) (member2: (Text.t * Expression.t)) =
        let (text1, expr1) = member1 in
        let (text2, expr2) = member2 in
        eq_text text1 text2 &&
        eq_expr expr1 expr2
      in
      eq_type typ1 typ2 &&
      eq_text name1 name2 &&
      eq_list eq_member members1 members2
  | ExternObject { name = name1; type_params = type_params1; methods = methods1; _ },
    ExternObject { name = name2; type_params = type_params2; methods = methods2; _ } ->
      eq_text name1 name2 &&
      eq_list eq_text type_params1 type_params2 &&
      eq_list eq_method_prototype methods1 methods2
  | TypeDef { name = name1; typ_or_decl = typ_or_decl1; _ },
    TypeDef { name = name2; typ_or_decl = typ_or_decl2; _ } ->
      eq_text name1 name2 &&
      eq_alternative eq_type eq_decl typ_or_decl1 typ_or_decl2
  | NewType { name = name1; typ_or_decl = typ_or_decl1; _ },
    NewType { name = name2; typ_or_decl = typ_or_decl2; _ } ->
      eq_text name1 name2 &&
      eq_alternative eq_type eq_decl typ_or_decl1 typ_or_decl2
  | ControlType { name = name1; type_params = type_params1; params = params1; _ },
    ControlType { name = name2; type_params = type_params2; params = params2; _ } ->
      eq_text name1 name2 &&
      eq_list eq_text type_params1 type_params2 &&
      eq_list eq_param params1 params2
  | ParserType { name = name1; type_params = type_params1; params = params1; _ },
    ParserType { name = name2; type_params = type_params2; params = params2; _ } ->
      eq_text name1 name2 &&
      eq_list eq_text type_params1 type_params2 &&
      eq_list eq_param params1 params2
  | PackageType { name = name1; type_params = type_params1; params = params1; _ },
    PackageType { name = name2; type_params = type_params2; params = params2; _ } ->
      eq_text name1 name2 &&
      eq_list eq_text type_params1 type_params2 &&
      eq_list eq_param params1 params2
  | _ -> false


(* Program *)

and eq_program (program1: program) (program2: program) =
  let Program decls1 = program1 in
  let Program decls2 = program2 in
  eq_list eq_decl decls1 decls2
