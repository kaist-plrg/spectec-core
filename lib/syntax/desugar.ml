open Ast
open Surface.Ast

(* Numbers *)

let desugar_num (n : Number.t) : num = (n.value, n.width_signed)

(* Names *)

let desugar_name (name : Name.t) : var =
  match name with
  | BareName name -> Bare name.str
  | QualifiedName (prefix, name) ->
      assert (prefix = []);
      Top name.str

(* Unary and binary operators *)

let desugar_unop (unop : Op.un) : unop =
  match unop with BitNot _ -> BNotOp | Not _ -> LNotOp | UMinus _ -> UMinusOp

let desugar_binop (binop : Op.bin) : binop =
  match binop with
  | Plus _ -> PlusOp
  | PlusSat _ -> SPlusOp
  | Minus _ -> MinusOp
  | MinusSat _ -> SMinusOp
  | Mul _ -> MulOp
  | Div _ -> DivOp
  | Mod _ -> ModOp
  | Shl _ -> ShlOp
  | Shr _ -> ShrOp
  | Le _ -> LeOp
  | Ge _ -> GeOp
  | Lt _ -> LtOp
  | Gt _ -> GtOp
  | Eq _ -> EqOp
  | NotEq _ -> NeOp
  | BitAnd _ -> BAndOp
  | BitXor _ -> BXorOp
  | BitOr _ -> BOrOp
  | PlusPlus _ -> ConcatOp
  | And _ -> LAndOp
  | Or _ -> LOrOp

(* Types *)

let rec desugar_type (typ : Type.t) : typ =
  match typ with
  | Void _ -> VoidT
  | Bool _ -> BoolT
  | Error _ -> ErrT
  | String _ -> StrT
  | Integer _ -> AIntT
  | IntType { expr; _ } -> IntT (desugar_expr expr)
  | BitType { expr; _ } -> BitT (desugar_expr expr)
  | VarBit { expr; _ } -> VBitT (desugar_expr expr)
  | TypeName { name; _ } -> NameT (desugar_name name)
  | SpecializedType { base; args; _ } ->
      let var =
        match desugar_type base with NameT name -> name | _ -> assert false
      in
      let targs = List.map desugar_type args in
      SpecT (var, targs)
  | HeaderStack { header; size; _ } ->
      let typ = desugar_type header in
      let size = desugar_expr size in
      StackT (typ, size)
  | Tuple { args; _ } ->
      let targs = List.map desugar_type args in
      TupleT targs
  | DontCare _ -> AnyT

and desugar_types (typs : Type.t list) : typ list = List.map desugar_type typs

(* Parameters and Arguments *)

and desugar_dir (dir : Direction.t option) : dir =
  match dir with
  | Some dir -> ( match dir with In _ -> In | Out _ -> Out | InOut _ -> InOut)
  | None -> No

and desugar_param (param : Parameter.t) : param =
  let name = param.variable.str in
  let dir = desugar_dir param.direction in
  let typ = desugar_type param.typ in
  let default = Option.map desugar_expr param.opt_value in
  (name, dir, typ, default)

and desugar_params (params : Parameter.t list) : param list =
  List.map desugar_param params

and desugar_tparams (tparams : Text.t list) : string list =
  List.map (fun (tparam : Text.t) -> tparam.str) tparams

and desugar_cparams (cparams : Parameter.t list) : param list =
  List.map desugar_param cparams

and desugar_arg (arg : Argument.t) : arg =
  match arg with
  | Expression { value; _ } -> ExprA (desugar_expr value)
  | KeyValue { key; value; _ } ->
      let key = key.str in
      let value = desugar_expr value in
      NameA (key, value)
  | Missing _ -> AnyA

and desugar_args (args : Argument.t list) : arg list = List.map desugar_arg args

(* Expressions *)

and desugar_record (entries : KeyValue.t list) : (string * expr) list =
  List.map
    (fun (entry : KeyValue.t) ->
      let KeyValue.{ key; value; _ } = entry in
      (key.str, desugar_expr value))
    entries

and desugar_expr (expr : Expression.t) : expr =
  match expr with
  | True _ -> BoolE true
  | False _ -> BoolE false
  | Int { i; _ } -> NumE (desugar_num i)
  | String { text; _ } -> StrE text.str
  | Name { name; _ } -> VarE (desugar_name name)
  | List { values; _ } -> ListE (desugar_exprs values)
  | Record { entries; _ } ->
      let record = desugar_record entries in
      RecordE record
  | UnaryOp { op; arg; _ } ->
      let op = desugar_unop op in
      let arg = desugar_expr arg in
      UnE (op, arg)
  | BinaryOp { op; args; _ } ->
      let op = desugar_binop op in
      let larg, rarg = args in
      let larg = desugar_expr larg in
      let rarg = desugar_expr rarg in
      BinE (op, larg, rarg)
  | Ternary { cond; tru; fls; _ } ->
      let cond = desugar_expr cond in
      let tru = desugar_expr tru in
      let fls = desugar_expr fls in
      TernE (cond, tru, fls)
  | Cast { typ; expr; _ } ->
      let typ = desugar_type typ in
      let expr = desugar_expr expr in
      CastE (typ, expr)
  | Mask { expr; mask; _ } ->
      let expr = desugar_expr expr in
      let mask = desugar_expr mask in
      MaskE (expr, mask)
  | Range { lo; hi; _ } ->
      let lo = desugar_expr lo in
      let hi = desugar_expr hi in
      RangeE (lo, hi)
  | ArrayAccess { array; index; _ } ->
      let arr = desugar_expr array in
      let idx = desugar_expr index in
      ArrAccE (arr, idx)
  | BitStringAccess { bits; lo; hi; _ } ->
      let bits = desugar_expr bits in
      let lidx = desugar_expr lo in
      let hidx = desugar_expr hi in
      BitAccE (bits, lidx, hidx)
  | TypeMember { typ; name; _ } ->
      let typ = desugar_name typ in
      let name = name.str in
      TypeAccE (typ, name)
  | ErrorMember { err; _ } ->
      let err = err.str in
      ErrAccE err
  | ExpressionMember { expr; name; _ } ->
      let expr = desugar_expr expr in
      let name = name.str in
      ExprAccE (expr, name)
  | FunctionCall { func; type_args; args; _ } ->
      let func = desugar_expr func in
      let targs = desugar_types type_args in
      let args = desugar_args args in
      CallE (func, targs, args)
  | NamelessInstantiation { typ; args; _ } ->
      let typ = desugar_type typ in
      let args = desugar_args args in
      InstE (typ, args)

and desugar_exprs (exprs : Expression.t list) : expr list =
  List.map desugar_expr exprs

(* Match-cases *)

and desugar_match (mtch : Match.t) : mtch =
  match mtch with
  | Expression { expr; _ } -> ExprM (desugar_expr expr)
  | Default _ -> DefaultM
  | DontCare _ -> AnyM

and desugar_matches (mtchs : Match.t list) : mtch list =
  List.map desugar_match mtchs

(* Statements *)

and desugar_switch_case (case : Statement.switch_case) : switch_case =
  match case with
  | Action { label; code; _ } -> (
      let block = desugar_stmts code.statements in
      match label with
      | Name { name; _ } -> (CaseC name.str, block)
      | Default _ -> (DefaultC, block))
  | FallThrough { label; _ } -> (
      match label with
      | Name { name; _ } -> (CaseC name.str, [])
      | Default _ -> assert false)

and desugar_switch_cases (cases : Statement.switch_case list) : switch_case list
    =
  List.map desugar_switch_case cases

and desugar_stmt (stmt : Statement.t) : stmt =
  match stmt with
  | EmptyStatement _ -> EmptyI
  | Assignment { lhs; rhs; _ } ->
      let lhs = desugar_expr lhs in
      let rhs = desugar_expr rhs in
      AssignI (lhs, rhs)
  | Switch { expr; cases; _ } ->
      let expr = desugar_expr expr in
      let cases = desugar_switch_cases cases in
      SwitchI (expr, cases)
  | Conditional { cond; tru; fls; _ } ->
      let cond = desugar_expr cond in
      let tru = desugar_stmt tru in
      let fls =
        match fls with Some fls -> desugar_stmt fls | None -> EmptyI
      in
      IfI (cond, tru, fls)
  | BlockStatement { block; _ } ->
      let stmts = desugar_stmts block.statements in
      BlockI stmts
  | Exit _ -> ExitI
  | Return { expr; _ } ->
      let expr = Option.map desugar_expr expr in
      RetI expr
  | MethodCall { func; type_args; args; _ } ->
      let func = desugar_expr func in
      let targs = desugar_types type_args in
      let args = desugar_args args in
      CallI (func, targs, args)
  | DeclarationStatement { decl; _ } ->
      let decl = desugar_decl decl in
      DeclI decl
  | _ -> failwith "(TODO: desugar_stmt)"

and desugar_stmts (stmts : Statement.t list) : stmt list =
  List.map desugar_stmt stmts

(* Declarations *)

and desugar_fields (fields : Declaration.field list) : (string * typ) list =
  List.map
    (fun (field : Declaration.field) ->
      let Declaration.{ name; typ; _ } = field in
      (name.str, desugar_type typ))
    fields

and desugar_members (members : Text.t list) : string list =
  List.map (fun (member : Text.t) -> member.str) members

and desugar_serial_members (members : (Text.t * Expression.t) list) :
    (string * expr) list =
  List.map
    (fun (member : Text.t * Expression.t) ->
      let field, value = member in
      (field.str, desugar_expr value))
    members

and desugar_parser_case (case : Parser.case) : select_case =
  let Parser.{ matches; next; _ } = case in
  let mtchs = desugar_matches matches in
  let next = next.str in
  (mtchs, next)

and desugar_parser_cases (cases : Parser.case list) : select_case list =
  List.map desugar_parser_case cases

and desugar_parser_transition (trans : Parser.transition) : stmt =
  match trans with
  | Direct { next; _ } -> TransI next.str
  | Select { exprs; cases; _ } ->
      let exprs = desugar_exprs exprs in
      let cases = desugar_parser_cases cases in
      SelectI (exprs, cases)

and desugar_parser_state (state : Parser.state) : parser_state =
  let Parser.{ name; statements; transition; _ } = state in
  let name = name.str in
  let stmts =
    desugar_stmts statements @ [ desugar_parser_transition transition ]
  in
  (name, stmts)

and desugar_parser_states (states : Parser.state list) : parser_state list =
  List.map desugar_parser_state states

and desugar_table_keys (keys : Table.key list) : table_key list =
  List.map
    (fun (key : Table.key) ->
      let Table.{ key; match_kind; _ } = key in
      (desugar_expr key, match_kind.str))
    keys

and desugar_table_action (action : Table.action_ref) : table_action =
  let Table.{ name; args; _ } = action in
  let var = desugar_name name in
  let args = desugar_args args in
  (var, args)

and desugar_table_actions (actions : Table.action_ref list) : table_action list
    =
  List.map desugar_table_action actions

and desugar_table_entry (entry : Table.entry) : table_entry =
  let Table.{ matches; action; _ } = entry in
  let mtchs = desugar_matches matches in
  let action = desugar_table_action action in
  (mtchs, action)

and desugar_table_entries (entries : Table.entry list) : table_entry list =
  List.map desugar_table_entry entries

and desugar_table_properties (properties : Table.property list) :
    table_key list
    * table_action list
    * table_entry list
    * table_default option
    * table_custom list =
  List.fold_left
    (fun (keys, actions, entries, default, customs) (property : Table.property) ->
      match property with
      | Key { keys; _ } ->
          let keys = desugar_table_keys keys in
          (keys, actions, entries, default, customs)
      | Actions { actions; _ } ->
          let actions = desugar_table_actions actions in
          (keys, actions, entries, default, customs)
      | Entries { entries; _ } ->
          let entries = desugar_table_entries entries in
          (keys, actions, entries, default, customs)
      | DefaultAction { action; const; _ } ->
          let action = desugar_table_action action in
          let default = Some (action, const) in
          (keys, actions, entries, default, customs)
      | Custom { name; value; const; _ } ->
          let name = name.str in
          let value = desugar_expr value in
          let custom = (name, value, const) in
          (keys, actions, entries, default, customs @ [ custom ]))
    ([], [], [], None, []) properties

and desugar_method (mthd : MethodPrototype.t) : decl =
  match mthd with
  | Constructor { name; params; _ } ->
      let name = name.str in
      let params = desugar_params params in
      ConsD { name; params }
  | AbstractMethod { name; return; type_params; params; _ } ->
      let name = name.str in
      let rettyp = desugar_type return in
      let tparams = desugar_tparams type_params in
      let params = desugar_params params in
      AbstractD { name; rettyp; tparams; params }
  | Method { name; return; type_params; params; _ } ->
      let name = name.str in
      let rettyp = desugar_type return in
      let tparams = desugar_tparams type_params in
      let params = desugar_params params in
      MethodD { name; rettyp; tparams; params }

and desugar_methods (mthds : MethodPrototype.t list) : decl list =
  List.map desugar_method mthds

and desugar_decl (decl : Declaration.t) : decl =
  match decl with
  | Constant { name; typ; value; _ } ->
      let name = name.str in
      let typ = desugar_type typ in
      let value = desugar_expr value in
      ConstD { name; typ; value }
  | Variable { name; typ; init; _ } ->
      let name = name.str in
      let typ = desugar_type typ in
      let init = Option.map desugar_expr init in
      VarD { name; typ; init }
  | Instantiation { name; typ; args; init; _ } ->
      let name = name.str in
      let typ = desugar_type typ in
      let args = desugar_args args in
      let init =
        Option.map (fun (init : Block.t) -> desugar_stmts init.statements) init
      in
      InstD { name; typ; args; init }
  | Error { members; _ } ->
      let members = desugar_members members in
      ErrD { members }
  | MatchKind { members; _ } ->
      let members = desugar_members members in
      MatchKindD { members }
  | Struct { name; fields; _ } ->
      let name = name.str in
      let fields = desugar_fields fields in
      StructD { name; fields }
  | Header { name; fields; _ } ->
      let name = name.str in
      let fields = desugar_fields fields in
      HeaderD { name; fields }
  | HeaderUnion { name; fields; _ } ->
      let name = name.str in
      let fields = desugar_fields fields in
      UnionD { name; fields }
  | Enum { name; members; _ } ->
      let name = name.str in
      let members = desugar_members members in
      EnumD { name; members }
  | SerializableEnum { name; typ; members; _ } ->
      let name = name.str in
      let typ = desugar_type typ in
      let members = desugar_serial_members members in
      SEnumD { name; typ; members }
  | NewType { name; typ_or_decl; _ } ->
      let name = name.str in
      let typ, decl =
        match typ_or_decl with
        | Utils.Alternative.Left typ -> (Some (desugar_type typ), None)
        | Utils.Alternative.Right decl -> (None, Some (desugar_decl decl))
      in
      NewTypeD { name; typ; decl }
  | TypeDef { name; typ_or_decl; _ } ->
      let name = name.str in
      let typ, decl =
        match typ_or_decl with
        | Utils.Alternative.Left typ -> (Some (desugar_type typ), None)
        | Utils.Alternative.Right decl -> (None, Some (desugar_decl decl))
      in
      TypeDefD { name; typ; decl }
  | ValueSet { name; typ; size; _ } ->
      let name = name.str in
      let typ = desugar_type typ in
      let size = desugar_expr size in
      ValueSetD { name; typ; size }
  | ParserType { name; type_params; params; _ } ->
      let name = name.str in
      let tparams = desugar_tparams type_params in
      let params = desugar_params params in
      ParserTypeD { name; tparams; params }
  | Parser { name; type_params; params; constructor_params; locals; states; _ }
    ->
      let name = name.str in
      let tparams = desugar_tparams type_params in
      let params = desugar_params params in
      let cparams = desugar_cparams constructor_params in
      let locals = desugar_decls locals in
      let states = desugar_parser_states states in
      ParserD { name; tparams; params; cparams; locals; states }
  | Action { name; params; body; _ } ->
      let name = name.str in
      let params = desugar_params params in
      let body = desugar_stmts body.statements in
      ActionD { name; params; body }
  | Table { name; properties; _ } ->
      let name = name.str in
      let key, actions, entries, default, custom =
        desugar_table_properties properties
      in
      TableD { name; key; actions; entries; default; custom }
  | ControlType { name; type_params; params; _ } ->
      let name = name.str in
      let tparams = desugar_tparams type_params in
      let params = desugar_params params in
      ControlTypeD { name; tparams; params }
  | Control { name; type_params; params; constructor_params; locals; apply; _ }
    ->
      let name = name.str in
      let tparams = desugar_tparams type_params in
      let params = desugar_params params in
      let cparams = desugar_cparams constructor_params in
      let locals = desugar_decls locals in
      let body = desugar_stmts apply.statements in
      ControlD { name; tparams; params; cparams; locals; body }
  | Function { name; return; type_params; params; body; _ } ->
      let name = name.str in
      let rettyp = desugar_type return in
      let tparams = desugar_tparams type_params in
      let params = desugar_params params in
      let body = desugar_stmts body.statements in
      FuncD { name; rettyp; tparams; params; body }
  | ExternFunction { name; return; type_params; params; _ } ->
      let name = name.str in
      let rettyp = desugar_type return in
      let tparams = desugar_tparams type_params in
      let params = desugar_params params in
      ExternFuncD { name; rettyp; tparams; params }
  | ExternObject { name; type_params; methods; _ } ->
      let name = name.str in
      let tparams = desugar_tparams type_params in
      let methods = desugar_methods methods in
      ExternObjectD { name; tparams; methods }
  | PackageType { name; type_params; params; _ } ->
      let name = name.str in
      let tparams = desugar_tparams type_params in
      let params = desugar_params params in
      PackageTypeD { name; tparams; params }

and desugar_decls (decls : Declaration.t list) : decl list =
  List.map desugar_decl decls

let desugar (program : p4program) : program =
  let (Program decls) = program in
  desugar_decls decls
