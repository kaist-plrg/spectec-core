open Ast
open Print

(* Utils *)

let check (category : string) (eq : 'a -> 'a -> bool) (print : 'a -> string)
    (a : 'a) (b : 'a) =
  let same = eq a b in
  if not same then
    Format.eprintf "eq_%s: %s does not equal %s\n" category (print a) (print b);
  same

let eq_option (eq : 'a -> 'a -> bool) (opt : 'a option) (opt' : 'a option) =
  match (opt, opt') with
  | None, None -> true
  | Some x, Some y -> eq x y
  | _ -> false

let eq_list (eq : 'a -> 'a -> bool) (l : 'a list) (l' : 'a list) =
  List.length l = List.length l' && List.for_all2 eq l l'

let eq_alternative (eq_a : 'a -> 'a -> bool) (eq_b : 'b -> 'b -> bool)
    (alt : ('a, 'b) alternative) (alt' : ('a, 'b) alternative) =
  match (alt, alt') with
  | Left x, Left y -> eq_a x y
  | Right x, Right y -> eq_b x y
  | _ -> false

(* Basics *)

let rec eq_text (text : Text.t) (text' : Text.t) =
  check "text" eq_text' print_text text text'

and eq_text' (text : Text.t) (text' : Text.t) = text.str = text'.str

let rec eq_number (number : Number.t) (number' : Number.t) =
  check "number" eq_number' print_number number number'

and eq_number' (number : Number.t) (number' : Number.t) =
  number.value = number'.value
  &&
  match (number.width_signed, number'.width_signed) with
  | None, None -> true
  | Some (width, signed), Some (width', signed') ->
      width = width' && signed = signed'
  | _ -> false

let rec eq_name (name : Name.t) (name' : Name.t) =
  check "name" eq_name' print_name name name'

and eq_name' (name : Name.t) (name' : Name.t) =
  match (name, name') with
  | BareName name, BareName name' -> eq_text name name'
  | QualifiedName (prefix, name), QualifiedName (prefix', name') ->
      eq_list eq_text prefix prefix' && eq_text name name'
  | _ -> false

let rec eq_direction (dir : Direction.t) (dir' : Direction.t) =
  check "direction" eq_direction' print_direction dir dir'

and eq_direction' (dir : Direction.t) (dir' : Direction.t) =
  match (dir, dir') with
  | In _, In _ | Out _, Out _ | InOut _, InOut _ -> true
  | _ -> false

(* Operators *)

let rec eq_unop (unop : Op.un) (unop' : Op.un) =
  check "unop" eq_unop' print_unop unop unop'

and eq_unop' (unop : Op.un) (unop' : Op.un) =
  match (unop, unop') with
  | Not _, Not _ -> true
  | BitNot _, BitNot _ -> true
  | UPlus _, UPlus _ -> true
  | UMinus _, UMinus _ -> true
  | _ -> false

let rec eq_binop (binop : Op.bin) (binop' : Op.bin) =
  check "binop" eq_binop' print_binop binop binop'

and eq_binop' (binop : Op.bin) (binop' : Op.bin) =
  match (binop, binop') with
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

let rec eq_type (typ : Type.t) (typ' : Type.t) =
  check "type" eq_type' print_type typ typ'

and eq_type' (typ : Type.t) (typ' : Type.t) =
  match (typ, typ') with
  | Bool _, Bool _ -> true
  | Error _, Error _ -> true
  | Integer _, Integer _ -> true
  | IntType { expr; _ }, IntType { expr = expr'; _ } -> eq_expr expr expr'
  | BitType { expr; _ }, BitType { expr = expr'; _ } -> eq_expr expr expr'
  | VarBit { expr; _ }, VarBit { expr = expr'; _ } -> eq_expr expr expr'
  | TypeName { name; _ }, TypeName { name = name'; _ } -> eq_name name name'
  | ( SpecializedType { base; args; _ },
      SpecializedType { base = base'; args = args'; _ } ) ->
      eq_type base base' && eq_list eq_type args args'
  | ( HeaderStack { header; size; _ },
      HeaderStack { header = header'; size = size'; _ } ) ->
      eq_type header header' && eq_expr size size'
  | List { typ; _ }, List { typ = typ'; _ } -> eq_type typ typ'
  | Tuple { args; _ }, Tuple { args = args'; _ } -> eq_list eq_type args args'
  | String _, String _ -> true
  | Void _, Void _ -> true
  | DontCare _, DontCare _ -> true
  | _ -> false

(* Arguments and Prameters *)

and eq_arg (arg : Argument.t) (arg' : Argument.t) =
  check "arg" eq_arg' print_arg arg arg'

and eq_arg' (arg : Argument.t) (arg' : Argument.t) =
  match (arg, arg') with
  | Expression { value; _ }, Expression { value = value'; _ } ->
      eq_expr value value'
  | KeyValue { key; value; _ }, KeyValue { key = key'; value = value'; _ } ->
      eq_text key key' && eq_option eq_expr value value'
  | Missing _, Missing _ -> true
  | _ -> false

and eq_param (param : Parameter.t) (param' : Parameter.t) =
  check "param" eq_param' print_param param param'

and eq_param' (param : Parameter.t) (param' : Parameter.t) =
  eq_option eq_direction param.direction param'.direction
  && eq_type param.typ param'.typ
  && eq_text param.variable param'.variable
  && eq_option eq_expr param.opt_value param'.opt_value

(* Expressions *)

and eq_key_value (key_value : KeyValue.t) (key_value' : KeyValue.t) =
  check "key_value" eq_key_value' print_key_value key_value key_value'

and eq_key_value' (key_value : KeyValue.t) (key_value' : KeyValue.t) =
  eq_text key_value.key key_value'.key
  && eq_expr key_value.value key_value'.value

and eq_expr (expr : Expression.t) (expr' : Expression.t) =
  check "expr" eq_expr' print_expr expr expr'

and eq_expr' (expr : Expression.t) (expr' : Expression.t) =
  match (expr, expr') with
  | True _, True _ -> true
  | False _, False _ -> true
  | Int { i; _ }, Int { i = i'; _ } -> eq_number i i'
  | String { text; _ }, String { text = text'; _ } -> eq_text text text'
  | Name { name; _ }, Name { name = name'; _ } -> eq_name name name'
  | Dots _, Dots _ -> true
  | ( ArrayAccess { array; index; _ },
      ArrayAccess { array = array'; index = index'; _ } ) ->
      eq_expr array array' && eq_expr index index'
  | ( BitStringAccess { bits; lo; hi; _ },
      BitStringAccess { bits = bits'; lo = lo'; hi = hi'; _ } ) ->
      eq_expr bits bits' && eq_expr lo lo' && eq_expr hi hi'
  | List { values; _ }, List { values = values'; _ } ->
      eq_list eq_expr values values'
  | ListDots { values; _ }, ListDots { values = values'; _ } ->
      eq_list eq_expr values values'
  | Record { entries; _ }, Record { entries = entries'; _ } ->
      eq_list eq_key_value entries entries'
  | RecordDots { entries; _ }, RecordDots { entries = entries'; _ } ->
      eq_list eq_key_value entries entries'
  | Invalid _, Invalid _ -> true
  | UnaryOp { op; arg; _ }, UnaryOp { op = op'; arg = arg'; _ } ->
      eq_unop op op' && eq_expr arg arg'
  | ( BinaryOp { op; args = arg0, arg1; _ },
      BinaryOp { op = op'; args = arg0', arg1'; _ } ) ->
      eq_binop op op' && eq_expr arg0 arg0' && eq_expr arg1 arg1'
  | Cast { typ; expr; _ }, Cast { typ = typ'; expr = expr'; _ } ->
      eq_type typ typ' && eq_expr expr expr'
  | TypeMember { typ; name; _ }, TypeMember { typ = typ'; name = name'; _ } ->
      eq_name typ typ' && eq_text name name'
  | ErrorMember { err; _ }, ErrorMember { err = err'; _ } -> eq_text err err'
  | ( ExpressionMember { expr; name; _ },
      ExpressionMember { expr = expr'; name = name'; _ } ) ->
      eq_expr expr expr' && eq_text name name'
  | ( Ternary { cond; tru; fls; _ },
      Ternary { cond = cond'; tru = tru'; fls = fls'; _ } ) ->
      eq_expr cond cond' && eq_expr tru tru' && eq_expr fls fls'
  | ( FunctionCall { func; type_args; args; _ },
      FunctionCall { func = func'; type_args = type_args'; args = args'; _ } )
    ->
      eq_expr func func'
      && eq_list eq_type type_args type_args'
      && eq_list eq_arg args args'
  | ( NamelessInstantiation { typ; args; _ },
      NamelessInstantiation { typ = typ'; args = args'; _ } ) ->
      eq_type typ typ' && eq_list eq_arg args args'
  | Mask { expr; mask; _ }, Mask { expr = expr'; mask = mask'; _ } ->
      eq_expr expr expr' && eq_expr mask mask'
  | Range { lo; hi; _ }, Range { lo = lo'; hi = hi'; _ } ->
      eq_expr lo lo' && eq_expr hi hi'
  | _ -> false

(* Statements *)

and eq_block (block : Block.t) (block' : Block.t) =
  check "block" eq_block' (print_block 0) block block'

and eq_block' (block : Block.t) (block' : Block.t) =
  eq_list eq_stmt block.statements block'.statements

and eq_switch_label (label : Statement.switch_label)
    (label' : Statement.switch_label) =
  check "switch_label" eq_switch_label' print_switch_label label label'

and eq_switch_label' (label : Statement.switch_label)
    (label' : Statement.switch_label) =
  match (label, label') with
  | Default _, Default _ -> true
  | Expression { expr; _ }, Expression { expr = expr'; _ } -> eq_expr expr expr'
  | _ -> false

and eq_switch_case (case : Statement.switch_case)
    (case' : Statement.switch_case) =
  check "switch_case" eq_switch_case' (print_switch_case 0) case case'

and eq_switch_case' (case : Statement.switch_case)
    (case' : Statement.switch_case) =
  match (case, case') with
  | Action { label; code; _ }, Action { label = label'; code = code'; _ } ->
      eq_switch_label label label' && eq_block code code'
  | FallThrough { label; _ }, FallThrough { label = label'; _ } ->
      eq_switch_label label label'
  | _ -> false

and eq_stmt (stmt : Statement.t) (stmt' : Statement.t) =
  check "stmt" eq_stmt' (print_stmt 0) stmt stmt'

and eq_stmt' (stmt : Statement.t) (stmt' : Statement.t) =
  match (stmt, stmt') with
  | ( MethodCall { func; type_args; args; _ },
      MethodCall { func = func'; type_args = type_args'; args = args'; _ } ) ->
      eq_expr func func'
      && eq_list eq_type type_args type_args'
      && eq_list eq_arg args args'
  | Assignment { lhs; rhs; _ }, Assignment { lhs = lhs'; rhs = rhs'; _ } ->
      eq_expr lhs lhs' && eq_expr rhs rhs'
  | ( DirectApplication { typ; args; _ },
      DirectApplication { typ = typ'; args = args'; _ } ) ->
      eq_type typ typ' && eq_list eq_arg args args'
  | ( Conditional { cond; tru; fls; _ },
      Conditional { cond = cond'; tru = tru'; fls = fls'; _ } ) ->
      eq_expr cond cond' && eq_stmt tru tru' && eq_option eq_stmt fls fls'
  | BlockStatement { block; _ }, BlockStatement { block = block'; _ } ->
      eq_block block block'
  | Exit _, Exit _ -> true
  | EmptyStatement _, EmptyStatement _ -> true
  | Return { expr; _ }, Return { expr = expr'; _ } ->
      eq_option eq_expr expr expr'
  | Switch { expr; cases; _ }, Switch { expr = expr'; cases = cases'; _ } ->
      eq_expr expr expr' && eq_list eq_switch_case cases cases'
  | DeclarationStatement { decl; _ }, DeclarationStatement { decl = decl'; _ }
    ->
      eq_decl decl decl'
  | _ -> false

(* Matches *)

and eq_match (mtch : Match.t) (mtch' : Match.t) =
  check "match" eq_match' print_match mtch mtch'

and eq_match' (mtch : Match.t) (mtch' : Match.t) =
  match (mtch, mtch') with
  | Default _, Default _ -> true
  | DontCare _, DontCare _ -> true
  | Expression { expr; _ }, Expression { expr = expr'; _ } -> eq_expr expr expr'
  | _ -> false

(* Parsers *)

and eq_parser_case (case : Parser.case) (case' : Parser.case) =
  check "parser_case" eq_parser_case' (print_parser_case 0) case case'

and eq_parser_case' (case : Parser.case) (case' : Parser.case) =
  eq_list eq_match case.matches case'.matches && eq_text case.next case'.next

and eq_parser_transition (transition : Parser.transition)
    (transition' : Parser.transition) =
  check "parser_transition" eq_parser_transition'
    (print_parser_transition 0)
    transition transition'

and eq_parser_transition' (transition : Parser.transition)
    (transition' : Parser.transition) =
  match (transition, transition') with
  | Direct { next; _ }, Direct { next = next'; _ } -> eq_text next next'
  | Select { exprs; cases; _ }, Select { exprs = exprs'; cases = cases'; _ } ->
      eq_list eq_expr exprs exprs' && eq_list eq_parser_case cases cases'
  | _ -> false

and eq_parser_state (state : Parser.state) (state' : Parser.state) =
  check "parser_state" eq_parser_state' (print_parser_state 0) state state'

and eq_parser_state' (state : Parser.state) (state' : Parser.state) =
  eq_text state.name state'.name
  && eq_list eq_stmt state.statements state'.statements
  && eq_parser_transition state.transition state'.transition

(* Tables *)

and eq_table_action_ref (action_ref : Table.action_ref)
    (action_ref' : Table.action_ref) =
  check "table_action_ref" eq_table_action_ref' print_table_action_ref
    action_ref action_ref'

and eq_table_action_ref' (action_ref : Table.action_ref)
    (action_ref' : Table.action_ref) =
  eq_name action_ref.name action_ref'.name
  && eq_list eq_arg action_ref.args action_ref'.args

and eq_table_key (key : Table.key) (key' : Table.key) =
  check "table_key" eq_table_key' print_table_key key key'

and eq_table_key' (key : Table.key) (key' : Table.key) =
  eq_expr key.key key'.key && eq_text key.match_kind key'.match_kind

and eq_table_entry (entry : Table.entry) (entry' : Table.entry) =
  check "table_entry" eq_table_entry' print_table_entry entry entry'

and eq_table_entry' (entry : Table.entry) (entry' : Table.entry) =
  eq_list eq_match entry.matches entry'.matches
  && eq_table_action_ref entry.action entry'.action

and eq_table_property (property : Table.property) (property' : Table.property) =
  check "table_property" eq_table_property' (print_table_property 0) property
    property'

and eq_table_property' (property : Table.property) (property' : Table.property)
    =
  match (property, property') with
  | Key { keys; _ }, Key { keys = keys'; _ } -> eq_list eq_table_key keys keys'
  | Actions { actions; _ }, Actions { actions = actions'; _ } ->
      eq_list eq_table_action_ref actions actions'
  | Entries { entries; _ }, Entries { entries = entries'; _ } ->
      eq_list eq_table_entry entries entries'
  | ( DefaultAction { action; const; _ },
      DefaultAction { action = action'; const = const'; _ } ) ->
      eq_table_action_ref action action' && const = const'
  | ( Custom { const; name; value; _ },
      Custom { const = const'; name = name'; value = value'; _ } ) ->
      const = const' && eq_text name name' && eq_expr value value'
  | _ -> false

(* Methods *)

and eq_method_prototype (proto : MethodPrototype.t) (proto' : MethodPrototype.t)
    =
  check "methodproto" eq_method_prototype' (print_method_prototype 0) proto
    proto'

and eq_method_prototype' (proto : MethodPrototype.t)
    (proto' : MethodPrototype.t) =
  match (proto, proto') with
  | ( Constructor { name; params; _ },
      Constructor { name = name'; params = params'; _ } ) ->
      eq_text name name' && eq_list eq_param params params'
  | ( AbstractMethod { return; name; type_params; params; _ },
      AbstractMethod
        {
          return = return';
          name = name';
          type_params = type_params';
          params = params';
          _;
        } ) ->
      eq_type return return' && eq_text name name'
      && eq_list eq_text type_params type_params'
      && eq_list eq_param params params'
  | ( Method { return; name; type_params; params; _ },
      Method
        {
          return = return';
          name = name';
          type_params = type_params';
          params = params';
          _;
        } ) ->
      eq_type return return' && eq_text name name'
      && eq_list eq_text type_params type_params'
      && eq_list eq_param params params'
  | _ -> false

(* Declarations *)

and eq_decl_field (field : Declaration.field) (field' : Declaration.field) =
  check "decl_field" eq_decl_field' (print_decl_field 0) field field'

and eq_decl_field' (field : Declaration.field) (field' : Declaration.field) =
  eq_type field.typ field'.typ && eq_text field.name field'.name

and eq_decl (decl : Declaration.t) (decl' : Declaration.t) =
  check "decl" eq_decl' (print_decl 0) decl decl'

and eq_decl' (decl : Declaration.t) (decl' : Declaration.t) =
  match (decl, decl') with
  | ( Constant { typ; name; value; _ },
      Constant { typ = typ'; name = name'; value = value'; _ } ) ->
      eq_type typ typ' && eq_text name name' && eq_expr value value'
  | ( Instantiation { typ; args; name; init; _ },
      Instantiation { typ = typ'; args = args'; name = name'; init = init'; _ }
    ) ->
      eq_type typ typ' && eq_list eq_arg args args' && eq_text name name'
      && eq_list eq_decl init init'
  | ( Parser
        {
          name;
          type_params;
          params;
          constructor_params = cons_params;
          locals;
          states;
          _;
        },
      Parser
        {
          name = name';
          type_params = type_params';
          params = params';
          constructor_params = cons_params';
          locals = locals';
          states = states';
          _;
        } ) ->
      eq_text name name'
      && eq_list eq_text type_params type_params'
      && eq_list eq_param params params'
      && eq_list eq_param cons_params cons_params'
      && eq_list eq_decl locals locals'
      && eq_list eq_parser_state states states'
  | ( Control
        {
          name;
          type_params;
          params;
          constructor_params = cons_params;
          locals;
          apply;
          _;
        },
      Control
        {
          name = name';
          type_params = type_params';
          params = params';
          constructor_params = cons_params';
          locals = locals';
          apply = apply';
          _;
        } ) ->
      eq_text name name'
      && eq_list eq_text type_params type_params'
      && eq_list eq_param params params'
      && eq_list eq_param cons_params cons_params'
      && eq_list eq_decl locals locals'
      && eq_block apply apply'
  | ( Function { return; name; type_params; params; body; _ },
      Function
        {
          return = return';
          name = name';
          type_params = type_params';
          params = params';
          body = body';
          _;
        } ) ->
      eq_type return return' && eq_text name name'
      && eq_list eq_text type_params type_params'
      && eq_list eq_param params params'
      && eq_block body body'
  | ( ExternFunction { return; name; type_params; params; _ },
      ExternFunction
        {
          return = return';
          name = name';
          type_params = type_params';
          params = params';
          _;
        } ) ->
      eq_type return return' && eq_text name name'
      && eq_list eq_text type_params type_params'
      && eq_list eq_param params params'
  | ( Variable { typ; name; init; _ },
      Variable { typ = typ'; name = name'; init = init'; _ } ) ->
      eq_type typ typ' && eq_text name name' && eq_option eq_expr init init'
  | ( ValueSet { typ; size; name; _ },
      ValueSet { typ = typ'; size = size'; name = name'; _ } ) ->
      eq_type typ typ' && eq_expr size size' && eq_text name name'
  | ( Action { name; params; body; _ },
      Action { name = name'; params = params'; body = body'; _ } ) ->
      eq_text name name'
      && eq_list eq_param params params'
      && eq_block body body'
  | ( Table { name; properties; _ },
      Table { name = name'; properties = properties'; _ } ) ->
      eq_text name name' && eq_list eq_table_property properties properties'
  | ( Header { name; type_params; fields; _ },
      Header { name = name'; type_params = type_params'; fields = fields'; _ } )
    ->
      eq_text name name'
      && eq_list eq_text type_params type_params'
      && eq_list eq_decl_field fields fields'
  | ( HeaderUnion { name; type_params; fields; _ },
      HeaderUnion
        { name = name'; type_params = type_params'; fields = fields'; _ } ) ->
      eq_text name name'
      && eq_list eq_text type_params type_params'
      && eq_list eq_decl_field fields fields'
  | ( Struct { name; type_params; fields; _ },
      Struct { name = name'; type_params = type_params'; fields = fields'; _ } )
    ->
      eq_text name name'
      && eq_list eq_text type_params type_params'
      && eq_list eq_decl_field fields fields'
  | Error { members; _ }, Error { members = members'; _ } ->
      eq_list eq_text members members'
  | MatchKind { members; _ }, MatchKind { members = members'; _ } ->
      eq_list eq_text members members'
  | Enum { name; members; _ }, Enum { name = name'; members = members'; _ } ->
      eq_text name name' && eq_list eq_text members members'
  | ( SerializableEnum { typ; name; members; _ },
      SerializableEnum { typ = typ'; name = name'; members = members'; _ } ) ->
      let eq_member (member : Text.t * Expression.t)
          (member' : Text.t * Expression.t) =
        let text, expr = member in
        let text', expr' = member' in
        eq_text text text' && eq_expr expr expr'
      in
      eq_type typ typ' && eq_text name name'
      && eq_list eq_member members members'
  | ( ExternObject { name; type_params; methods; _ },
      ExternObject
        { name = name'; type_params = type_params'; methods = methods'; _ } ) ->
      eq_text name name'
      && eq_list eq_text type_params type_params'
      && eq_list eq_method_prototype methods methods'
  | ( TypeDef { name; typ_or_decl; _ },
      TypeDef { name = name'; typ_or_decl = typ_or_decl'; _ } ) ->
      eq_text name name'
      && eq_alternative eq_type eq_decl typ_or_decl typ_or_decl'
  | ( NewType { name; typ_or_decl; _ },
      NewType { name = name'; typ_or_decl = typ_or_decl'; _ } ) ->
      eq_text name name'
      && eq_alternative eq_type eq_decl typ_or_decl typ_or_decl'
  | ( ControlType { name; type_params; params; _ },
      ControlType
        { name = name'; type_params = type_params'; params = params'; _ } ) ->
      eq_text name name'
      && eq_list eq_text type_params type_params'
      && eq_list eq_param params params'
  | ( ParserType { name; type_params; params; _ },
      ParserType
        { name = name'; type_params = type_params'; params = params'; _ } ) ->
      eq_text name name'
      && eq_list eq_text type_params type_params'
      && eq_list eq_param params params'
  | ( PackageType { name; type_params; params; _ },
      PackageType
        { name = name'; type_params = type_params'; params = params'; _ } ) ->
      eq_text name name'
      && eq_list eq_text type_params type_params'
      && eq_list eq_param params params'
  | _ -> false

(* Program *)

and eq_program (program : p4program) (program' : p4program) =
  let (Program decls) = program in
  let (Program decls') = program' in
  eq_list eq_decl decls decls'
