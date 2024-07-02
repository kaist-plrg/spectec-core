open Syntax.Ast
open Surface.Ast
open Util.Source

(* Numbers *)

let desugar_num (num : Number.t) : num =
  (num.value, num.width_signed) $ Number.tags num

(* Names *)

let desugar_text (text : Text.t) : id = text.str $ Text.tags text

let desugar_name (name : Name.t) : var =
  let at = Name.tags name in
  match name with
  | BareName id -> Bare (desugar_text id) $ at
  | QualifiedName (prefix, id) ->
      assert (prefix = []);
      Top (desugar_text id) $ at

(* Unary and binary operators *)

let desugar_unop (unop : Op.un) : unop =
  let at = Op.tags_un unop in
  match unop with
  | BitNot _ -> BNotOp $ at
  | Not _ -> LNotOp $ at
  | UMinus _ -> UMinusOp $ at

let desugar_binop (binop : Op.bin) : binop =
  let at = Op.tags_bin binop in
  match binop with
  | Plus _ -> PlusOp $ at
  | PlusSat _ -> SPlusOp $ at
  | Minus _ -> MinusOp $ at
  | MinusSat _ -> SMinusOp $ at
  | Mul _ -> MulOp $ at
  | Div _ -> DivOp $ at
  | Mod _ -> ModOp $ at
  | Shl _ -> ShlOp $ at
  | Shr _ -> ShrOp $ at
  | Le _ -> LeOp $ at
  | Ge _ -> GeOp $ at
  | Lt _ -> LtOp $ at
  | Gt _ -> GtOp $ at
  | Eq _ -> EqOp $ at
  | NotEq _ -> NeOp $ at
  | BitAnd _ -> BAndOp $ at
  | BitXor _ -> BXorOp $ at
  | BitOr _ -> BOrOp $ at
  | PlusPlus _ -> ConcatOp $ at
  | And _ -> LAndOp $ at
  | Or _ -> LOrOp $ at

(* Types *)

let rec desugar_type (typ : Type.t) : typ =
  let at = Type.tags typ in
  match typ with
  | Void _ -> VoidT $ at
  | Bool _ -> BoolT $ at
  | Error _ -> ErrT $ at
  | String _ -> StrT $ at
  | Integer _ -> AIntT $ at
  | IntType { expr; _ } -> IntT (desugar_expr expr) $ at
  | BitType { expr; _ } -> BitT (desugar_expr expr) $ at
  | VarBit { expr; _ } -> VBitT (desugar_expr expr) $ at
  | TypeName { name; _ } -> NameT (desugar_name name) $ at
  | SpecializedType { base; args; _ } ->
      let var =
        let typ = desugar_type base in
        match typ.it with NameT name -> name | _ -> assert false
      in
      let targs = List.map desugar_type args in
      SpecT (var, targs) $ at
  | HeaderStack { header; size; _ } ->
      let typ = desugar_type header in
      let size = desugar_expr size in
      StackT (typ, size) $ at
  | Tuple { args; _ } ->
      let targs = List.map desugar_type args in
      TupleT targs $ at
  | DontCare _ -> AnyT $ at

and desugar_types (typs : Type.t list) : typ list = List.map desugar_type typs

(* Parameters and Arguments *)

and desugar_dir (dir : Direction.t option) : dir =
  match dir with
  | None -> No $ no_info
  | Some (In { tags = at }) -> In $ at
  | Some (Out { tags = at }) -> Out $ at
  | Some (InOut { tags = at }) -> InOut $ at

and desugar_param (param : Parameter.t) : param =
  let at = param.tags in
  let id = desugar_text param.variable in
  let dir = desugar_dir param.direction in
  let typ = desugar_type param.typ in
  let default = Option.map desugar_expr param.opt_value in
  (id, dir, typ, default) $ at

and desugar_params (params : Parameter.t list) : param list =
  List.map desugar_param params

and desugar_tparams (tparams : Text.t list) : tparam list =
  List.map desugar_text tparams

and desugar_cparams (cparams : Parameter.t list) : cparam list =
  List.map desugar_param cparams

and desugar_arg (arg : Argument.t) : arg =
  match arg with
  | Expression { value; tags = at } -> ExprA (desugar_expr value) $ at
  | KeyValue { key; value; tags = at } ->
      let key = desugar_text key in
      let value = desugar_expr value in
      NameA (key, value) $ at
  | Missing { tags = at } -> AnyA $ at

and desugar_args (args : Argument.t list) : arg list = List.map desugar_arg args

(* Expressions *)

and desugar_record (fields : KeyValue.t list) : (field * expr) list =
  List.map
    (fun (field : KeyValue.t) ->
      let KeyValue.{ key; value; _ } = field in
      (desugar_text key, desugar_expr value))
    fields

and desugar_expr (expr : Expression.t) : expr =
  let at = Expression.tags expr in
  match expr with
  | True _ -> BoolE true $ at
  | False _ -> BoolE false $ at
  | Int { i; _ } -> NumE (desugar_num i) $ at
  | String { text; _ } -> StrE text.str $ at
  | Name { name; _ } -> VarE (desugar_name name) $ at
  | List { values; _ } -> ListE (desugar_exprs values) $ at
  | Record { entries; _ } ->
      let record = desugar_record entries in
      RecordE record $ at
  | UnaryOp { op; arg; _ } ->
      let op = desugar_unop op in
      let arg = desugar_expr arg in
      UnE (op, arg) $ at
  | BinaryOp { op; args; _ } ->
      let op = desugar_binop op in
      let larg, rarg = args in
      let larg = desugar_expr larg in
      let rarg = desugar_expr rarg in
      BinE (op, larg, rarg) $ at
  | Ternary { cond; tru; fls; _ } ->
      let cond = desugar_expr cond in
      let tru = desugar_expr tru in
      let fls = desugar_expr fls in
      TernE (cond, tru, fls) $ at
  | Cast { typ; expr; _ } ->
      let typ = desugar_type typ in
      let expr = desugar_expr expr in
      CastE (typ, expr) $ at
  | Mask { expr; mask; _ } ->
      let expr = desugar_expr expr in
      let mask = desugar_expr mask in
      MaskE (expr, mask) $ at
  | Range { lo; hi; _ } ->
      let lo = desugar_expr lo in
      let hi = desugar_expr hi in
      RangeE (lo, hi) $ at
  | ArrayAccess { array; index; _ } ->
      let arr = desugar_expr array in
      let idx = desugar_expr index in
      ArrAccE (arr, idx) $ at
  | BitStringAccess { bits; lo; hi; _ } ->
      let bits = desugar_expr bits in
      let lidx = desugar_expr lo in
      let hidx = desugar_expr hi in
      BitAccE (bits, lidx, hidx) $ at
  | TypeMember { typ; name; _ } ->
      let typ = desugar_name typ in
      let field = desugar_text name in
      TypeAccE (typ, field) $ at
  | ErrorMember { err; _ } ->
      let err = desugar_text err in
      ErrAccE err $ at
  | ExpressionMember { expr; name; _ } ->
      let expr = desugar_expr expr in
      let field = desugar_text name in
      ExprAccE (expr, field) $ at
  | FunctionCall { func; type_args; args; _ } ->
      let func = desugar_expr func in
      let targs = desugar_types type_args in
      let args = desugar_args args in
      CallE (func, targs, args) $ at
  | NamelessInstantiation { typ; args; _ } ->
      let typ = desugar_type typ in
      let args = desugar_args args in
      InstE (typ, args) $ at

and desugar_exprs (exprs : Expression.t list) : expr list =
  List.map desugar_expr exprs

(* Match-cases *)

and desugar_match (mtch : Match.t) : mtch =
  let at = Match.tags mtch in
  match mtch with
  | Expression { expr; _ } -> ExprM (desugar_expr expr) $ at
  | Default _ -> DefaultM $ at
  | DontCare _ -> AnyM $ at

and desugar_matches (mtchs : Match.t list) : mtch list =
  List.map desugar_match mtchs

(* Statements *)

(* (TODO) hack with info *)
and desugar_switch_case (case : Statement.switch_case) : switch_case =
  let at = Statement.tags_case case in
  match case with
  | Action { label; code; _ } -> (
      let block = desugar_block code in
      match label with
      | Name { name; tags = at_case } -> (CaseC name.str $ at_case, block) $ at
      | Default { tags = at_case } -> (DefaultC $ at_case, block) $ no_info)
  | FallThrough { label; _ } -> (
      let block = [] $ no_info in
      match label with
      | Name { name; tags = at_case } -> (FallC name.str $ at_case, block) $ at
      | Default _ -> assert false)

and desugar_switch_cases (cases : Statement.switch_case list) : switch_case list
    =
  List.map desugar_switch_case cases

and desugar_stmt (stmt : Statement.t) : stmt =
  match stmt with
  | EmptyStatement { tags = at } -> EmptyI $ at
  | Assignment { lhs; rhs; tags = at } ->
      let lhs = desugar_expr lhs in
      let rhs = desugar_expr rhs in
      AssignI (lhs, rhs) $ at
  | Switch { expr; cases; tags = at } ->
      let expr = desugar_expr expr in
      let cases = desugar_switch_cases cases in
      SwitchI (expr, cases) $ at
  | Conditional { cond; tru; fls; tags = at } ->
      let cond = desugar_expr cond in
      let tru = desugar_stmt tru in
      let fls =
        match fls with Some fls -> desugar_stmt fls | None -> EmptyI $ no_info
      in
      IfI (cond, tru, fls) $ at
  | BlockStatement { block; tags = at } ->
      let block = desugar_block block in
      BlockI block $ at
  | Exit { tags = at } -> ExitI $ at
  | Return { expr; tags = at } ->
      let expr = Option.map desugar_expr expr in
      RetI expr $ at
  | MethodCall { func; type_args; args; tags = at } ->
      let func = desugar_expr func in
      let targs = desugar_types type_args in
      let args = desugar_args args in
      CallI (func, targs, args) $ at
  | DeclarationStatement { decl; tags = at } ->
      let decl = desugar_decl decl in
      DeclI decl $ at
  | _ -> failwith "(TODO: desugar_stmt)"

and desugar_stmts (stmts : Statement.t list) : stmt list =
  List.map desugar_stmt stmts

and desugar_block (block : Block.t) : block =
  let at = block.tags in
  let stmts = desugar_stmts block.statements in
  stmts $ at

(* Declarations *)

and desugar_struct_fields (fields : Declaration.field list) : (field * typ) list
    =
  List.map
    (fun (field : Declaration.field) ->
      let Declaration.{ name; typ; _ } = field in
      (desugar_text name, desugar_type typ))
    fields

and desugar_fields (fields : Text.t list) : field list =
  List.map desugar_text fields

and desugar_serial_fields (fields : (Text.t * Expression.t) list) :
    (field * expr) list =
  List.map
    (fun (field : Text.t * Expression.t) ->
      let field, value = field in
      (desugar_text field, desugar_expr value))
    fields

and desugar_parser_case (case : Parser.case) : select_case =
  let Parser.{ matches; next; tags = at } = case in
  let mtchs = desugar_matches matches in
  let next = desugar_text next in
  (mtchs, next) $ at

and desugar_parser_cases (cases : Parser.case list) : select_case list =
  List.map desugar_parser_case cases

and desugar_parser_transition (trans : Parser.transition) : stmt =
  match trans with
  | Direct { next; tags = at } -> TransI (desugar_text next) $ at
  | Select { exprs; cases; tags = at } ->
      let exprs = desugar_exprs exprs in
      let cases = desugar_parser_cases cases in
      SelectI (exprs, cases) $ at

and desugar_parser_state (state : Parser.state) : parser_state =
  let Parser.{ name; statements; transition; tags = at; _ } = state in
  let id = desugar_text name in
  let stmts =
    desugar_stmts statements @ [ desugar_parser_transition transition ]
  in
  let block = stmts $ no_info in
  (id, block) $ at

and desugar_parser_states (states : Parser.state list) : parser_state list =
  List.map desugar_parser_state states

and desugar_table_keys (keys : Table.key list) : table_key list =
  List.map
    (fun (key : Table.key) ->
      let Table.{ key; match_kind; tags = at; _ } = key in
      (desugar_expr key, desugar_text match_kind) $ at)
    keys

and desugar_table_action (action : Table.action_ref) : table_action =
  let Table.{ name; args; tags = at; _ } = action in
  let var = desugar_name name in
  let args = desugar_args args in
  (var, args) $ at

and desugar_table_actions (actions : Table.action_ref list) : table_action list
    =
  List.map desugar_table_action actions

and desugar_table_entry (entry : Table.entry) : table_entry =
  let Table.{ matches; action; tags = at; _ } = entry in
  let mtchs = desugar_matches matches in
  let action = desugar_table_action action in
  (mtchs, action) $ at

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
      | DefaultAction { action; const; tags = at; _ } ->
          assert (default = None);
          let action = desugar_table_action action in
          let default = Some ((action, const) $ at) in
          (keys, actions, entries, default, customs)
      | Custom { name; value; const; tags = at; _ } ->
          let id = desugar_text name in
          let value = desugar_expr value in
          let custom = (id, value, const) $ at in
          (keys, actions, entries, default, customs @ [ custom ]))
    ([], [], [], None, []) properties

and desugar_method (mthd : MethodPrototype.t) : decl =
  match mthd with
  | Constructor { name; params; tags = at; _ } ->
      let id = desugar_text name in
      let cparams = desugar_params params in
      ConsD { id; cparams } $ at
  | AbstractMethod { name; return; type_params; params; tags = at; _ } ->
      let id = desugar_text name in
      let rettyp = desugar_type return in
      let tparams = desugar_tparams type_params in
      let params = desugar_params params in
      AbstractD { id; rettyp; tparams; params } $ at
  | Method { name; return; type_params; params; tags = at; _ } ->
      let id = desugar_text name in
      let rettyp = desugar_type return in
      let tparams = desugar_tparams type_params in
      let params = desugar_params params in
      MethodD { id; rettyp; tparams; params } $ at

and desugar_methods (mthds : MethodPrototype.t list) : decl list =
  List.map desugar_method mthds

and desugar_decl (decl : Declaration.t) : decl =
  match decl with
  | Constant { name; typ; value; tags = at; _ } ->
      let id = desugar_text name in
      let typ = desugar_type typ in
      let value = desugar_expr value in
      ConstD { id; typ; value } $ at
  | Variable { name; typ; init; tags = at; _ } ->
      let id = desugar_text name in
      let typ = desugar_type typ in
      let init = Option.map desugar_expr init in
      VarD { id; typ; init } $ at
  | Instantiation { name; typ; args; init; tags = at; _ } ->
      let id = desugar_text name in
      let typ = desugar_type typ in
      let args = desugar_args args in
      let init = Option.map desugar_block init in
      InstD { id; typ; args; init } $ at
  | Error { members = fields; tags = at; _ } ->
      let fields = desugar_fields fields in
      ErrD { fields } $ at
  | MatchKind { members = fields; tags = at; _ } ->
      let fields = desugar_fields fields in
      MatchKindD { fields } $ at
  | Struct { name; fields; tags = at; _ } ->
      let id = desugar_text name in
      let fields = desugar_struct_fields fields in
      StructD { id; fields } $ at
  | Header { name; fields; tags = at; _ } ->
      let id = desugar_text name in
      let fields = desugar_struct_fields fields in
      HeaderD { id; fields } $ at
  | HeaderUnion { name; fields; tags = at; _ } ->
      let id = desugar_text name in
      let fields = desugar_struct_fields fields in
      UnionD { id; fields } $ at
  | Enum { name; members = fields; tags = at; _ } ->
      let id = desugar_text name in
      let fields = desugar_fields fields in
      EnumD { id; fields } $ at
  | SerializableEnum { name; typ; members = fields; tags = at; _ } ->
      let id = desugar_text name in
      let typ = desugar_type typ in
      let fields = desugar_serial_fields fields in
      SEnumD { id; typ; fields } $ at
  | NewType { name; typ_or_decl; tags = at; _ } ->
      let id = desugar_text name in
      let typ =
        match typ_or_decl with
        | Left typ -> (Left (desugar_type typ) : (typ, decl) alt)
        | Right decl -> (Right (desugar_decl decl) : (typ, decl) alt)
      in
      NewTypeD { id; typ } $ at
  | TypeDef { name; typ_or_decl; tags = at; _ } ->
      let id = desugar_text name in
      let typ =
        match typ_or_decl with
        | Left typ -> (Left (desugar_type typ) : (typ, decl) alt)
        | Right decl -> (Right (desugar_decl decl) : (typ, decl) alt)
      in
      TypeDefD { id; typ } $ at
  | ValueSet { name; typ; size; tags = at; _ } ->
      let id = desugar_text name in
      let typ = desugar_type typ in
      let size = desugar_expr size in
      ValueSetD { id; typ; size } $ at
  | ParserType { name; type_params; params; tags = at; _ } ->
      let id = desugar_text name in
      let tparams = desugar_tparams type_params in
      let params = desugar_params params in
      ParserTypeD { id; tparams; params } $ at
  | Parser
      {
        name;
        type_params;
        params;
        constructor_params;
        locals;
        states;
        tags = at;
        _;
      } ->
      let id = desugar_text name in
      let tparams = desugar_tparams type_params in
      let params = desugar_params params in
      let cparams = desugar_cparams constructor_params in
      let locals = desugar_decls locals in
      let states = desugar_parser_states states in
      ParserD { id; tparams; params; cparams; locals; states } $ at
  | Action { name; params; body; tags = at; _ } ->
      let id = desugar_text name in
      let params = desugar_params params in
      let body = desugar_block body in
      ActionD { id; params; body } $ at
  | Table { name; properties; tags = at; _ } ->
      let id = desugar_text name in
      let table = desugar_table_properties properties in
      TableD { id; table } $ at
  | ControlType { name; type_params; params; tags = at; _ } ->
      let id = desugar_text name in
      let tparams = desugar_tparams type_params in
      let params = desugar_params params in
      ControlTypeD { id; tparams; params } $ at
  | Control
      {
        name;
        type_params;
        params;
        constructor_params;
        locals;
        apply;
        tags = at;
        _;
      } ->
      let id = desugar_text name in
      let tparams = desugar_tparams type_params in
      let params = desugar_params params in
      let cparams = desugar_cparams constructor_params in
      let locals = desugar_decls locals in
      let body = desugar_block apply in
      ControlD { id; tparams; params; cparams; locals; body } $ at
  | Function { name; return; type_params; params; body; tags = at; _ } ->
      let id = desugar_text name in
      let rettyp = desugar_type return in
      let tparams = desugar_tparams type_params in
      let params = desugar_params params in
      let body = desugar_block body in
      FuncD { id; rettyp; tparams; params; body } $ at
  | ExternFunction { name; return; type_params; params; tags = at; _ } ->
      let id = desugar_text name in
      let rettyp = desugar_type return in
      let tparams = desugar_tparams type_params in
      let params = desugar_params params in
      ExternFuncD { id; rettyp; tparams; params } $ at
  | ExternObject { name; type_params; methods; tags = at; _ } ->
      let id = desugar_text name in
      let tparams = desugar_tparams type_params in
      let mthds = desugar_methods methods in
      ExternObjectD { id; tparams; mthds } $ at
  | PackageType { name; type_params; params; tags = at; _ } ->
      let id = desugar_text name in
      let tparams = desugar_tparams type_params in
      let cparams = desugar_params params in
      PackageTypeD { id; tparams; cparams } $ at

and desugar_decls (decls : Declaration.t list) : decl list =
  List.map desugar_decl decls

let desugar_program (program : p4program) : program =
  let (Program decls) = program in
  let program = desugar_decls decls in
  program
