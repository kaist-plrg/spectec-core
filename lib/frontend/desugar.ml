open Syntax.Ast
open Surface.Ast
open Util.Source

(* Numbers *)

let desugar_num (num : Number.t) : num =
  (num.value, num.width_signed) $ Number.tags num

(* Names *)

let desugar_text (text : Text.t) : id = text.str $ Text.tags text
let desugar_id (text : Text.t) : id = desugar_text text

let desugar_var (name : Name.t) : var =
  let at = Name.tags name in
  match name with
  | BareName id -> Bare (desugar_text id) $ at
  | QualifiedName (prefix, id) ->
      assert (prefix = []);
      Top (desugar_text id) $ at

let desugar_member (text : Text.t) : member = desugar_text text
let desugar_label (text : Text.t) : label = desugar_text text
let desugar_mtch_kind (text : Text.t) : mtch_kind = desugar_text text

(* Unary and binary operators *)

let desugar_unop (unop : Op.un) : unop =
  match unop with
  | BitNot { tags = at } -> BNotOp $ at
  | Not { tags = at } -> LNotOp $ at
  | UMinus { tags = at } -> UMinusOp $ at

let desugar_binop (binop : Op.bin) : binop =
  match binop with
  | Plus { tags = at } -> PlusOp $ at
  | PlusSat { tags = at } -> SPlusOp $ at
  | Minus { tags = at } -> MinusOp $ at
  | MinusSat { tags = at } -> SMinusOp $ at
  | Mul { tags = at } -> MulOp $ at
  | Div { tags = at } -> DivOp $ at
  | Mod { tags = at } -> ModOp $ at
  | Shl { tags = at } -> ShlOp $ at
  | Shr { tags = at } -> ShrOp $ at
  | Le { tags = at } -> LeOp $ at
  | Ge { tags = at } -> GeOp $ at
  | Lt { tags = at } -> LtOp $ at
  | Gt { tags = at } -> GtOp $ at
  | Eq { tags = at } -> EqOp $ at
  | NotEq { tags = at } -> NeOp $ at
  | BitAnd { tags = at } -> BAndOp $ at
  | BitXor { tags = at } -> BXorOp $ at
  | BitOr { tags = at } -> BOrOp $ at
  | PlusPlus { tags = at } -> ConcatOp $ at
  | And { tags = at } -> LAndOp $ at
  | Or { tags = at } -> LOrOp $ at

(* Types *)

let rec desugar_type (typ : Type.t) : typ =
  match typ with
  | Void { tags = at } -> VoidT $ at
  | Bool { tags = at } -> BoolT $ at
  | Error { tags = at } -> ErrT $ at
  | String { tags = at } -> StrT $ at
  | Integer { tags = at } -> AIntT $ at
  | IntType { expr; tags = at } -> IntT (desugar_expr expr) $ at
  | BitType { expr; tags = at } -> BitT (desugar_expr expr) $ at
  | VarBit { expr; tags = at } -> VBitT (desugar_expr expr) $ at
  | TypeName { name; tags = at } -> NameT (desugar_var name) $ at
  | SpecializedType { base; args; tags = at } ->
      let var =
        let typ = desugar_type base in
        match typ.it with NameT name -> name | _ -> assert false
      in
      let targs = List.map desugar_type args in
      SpecT (var, targs) $ at
  | HeaderStack { header; size; tags = at } ->
      let typ = desugar_type header in
      let size = desugar_expr size in
      StackT (typ, size) $ at
  | Tuple { args; tags = at } ->
      let targs = List.map desugar_type args in
      TupleT targs $ at
  | DontCare { tags = at } -> AnyT $ at

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
  let id = desugar_id param.variable in
  let dir = desugar_dir param.direction in
  let typ = desugar_type param.typ in
  let default = Option.map desugar_expr param.opt_value in
  (id, dir, typ, default) $ at

and desugar_params (params : Parameter.t list) : param list =
  List.map desugar_param params

and desugar_tparam (tparam : Text.t) : tparam = desugar_text tparam

and desugar_tparams (tparams : Text.t list) : tparam list =
  List.map desugar_tparam tparams

and desugar_cparams (cparams : Parameter.t list) : cparam list =
  List.map desugar_param cparams

and desugar_arg (arg : Argument.t) : arg =
  match arg with
  | Expression { value; tags = at } -> ExprA (desugar_expr value) $ at
  | KeyValue { key; value; tags = at } ->
      let key = desugar_id key in
      let value = desugar_expr value in
      NameA (key, value) $ at
  | Missing { tags = at } -> AnyA $ at

and desugar_args (args : Argument.t list) : arg list = List.map desugar_arg args

(* Expressions *)

and desugar_record (fields : KeyValue.t list) : (member * expr) list =
  List.map
    (fun (field : KeyValue.t) ->
      let KeyValue.{ key; value; _ } = field in
      (desugar_member key, desugar_expr value))
    fields

and desugar_expr (expr : Expression.t) : expr =
  match expr with
  | True { tags = at } -> BoolE true $ at
  | False { tags = at } -> BoolE false $ at
  | Int { i; tags = at } -> NumE (desugar_num i) $ at
  | String { text; tags = at } -> StrE text.str $ at
  | Name { name; tags = at } -> VarE (desugar_var name) $ at
  | List { values; tags = at } -> ListE (desugar_exprs values) $ at
  | Record { entries = fields; tags = at } ->
      let record = desugar_record fields in
      RecordE record $ at
  | UnaryOp { op; arg; tags = at } ->
      let op = desugar_unop op in
      let arg = desugar_expr arg in
      UnE (op, arg) $ at
  | BinaryOp { op; args; tags = at } ->
      let op = desugar_binop op in
      let larg, rarg = args in
      let larg = desugar_expr larg in
      let rarg = desugar_expr rarg in
      BinE (op, larg, rarg) $ at
  | Ternary { cond; tru; fls; tags = at } ->
      let cond = desugar_expr cond in
      let tru = desugar_expr tru in
      let fls = desugar_expr fls in
      TernE (cond, tru, fls) $ at
  | Cast { typ; expr; tags = at } ->
      let typ = desugar_type typ in
      let expr = desugar_expr expr in
      CastE (typ, expr) $ at
  | Mask { expr; mask; tags = at } ->
      let expr = desugar_expr expr in
      let mask = desugar_expr mask in
      MaskE (expr, mask) $ at
  | Range { lo; hi; tags = at } ->
      let lo = desugar_expr lo in
      let hi = desugar_expr hi in
      RangeE (lo, hi) $ at
  | ArrayAccess { array; index; tags = at } ->
      let arr = desugar_expr array in
      let idx = desugar_expr index in
      ArrAccE (arr, idx) $ at
  | BitStringAccess { bits; lo; hi; tags = at } ->
      let bits = desugar_expr bits in
      let lidx = desugar_expr lo in
      let hidx = desugar_expr hi in
      BitAccE (bits, lidx, hidx) $ at
  | TypeMember { typ; name; tags = at } ->
      let typ = desugar_var typ in
      let member = desugar_member name in
      TypeAccE (typ, member) $ at
  | ErrorMember { err; tags = at } ->
      let err = desugar_member err in
      ErrAccE err $ at
  | ExpressionMember { expr; name; tags = at } ->
      let expr = desugar_expr expr in
      let member = desugar_member name in
      ExprAccE (expr, member) $ at
  | FunctionCall { func; type_args; args; tags = at } ->
      let func = desugar_expr func in
      let targs = desugar_types type_args in
      let args = desugar_args args in
      CallE (func, targs, args) $ at
  | NamelessInstantiation { typ; args; tags = at } ->
      let typ = desugar_type typ in
      let args = desugar_args args in
      InstE (typ, args) $ at

and desugar_exprs (exprs : Expression.t list) : expr list =
  List.map desugar_expr exprs

(* Match-cases *)

and desugar_match (mtch : Match.t) : mtch =
  match mtch with
  | Expression { expr; tags = at } -> ExprM (desugar_expr expr) $ at
  | Default { tags = at } -> DefaultM $ at
  | DontCare { tags = at } -> AnyM $ at

and desugar_matches (mtchs : Match.t list) : mtch list =
  List.map desugar_match mtchs

(* Statements *)

(* (TODO) hack with info *)
and desugar_switch_case (case : Statement.switch_case) : switch_case =
  match case with
  | Action { label; code; tags = at } -> (
      let block = desugar_block code in
      match label with
      | Name { name; tags = at_case } -> (CaseC name.str $ at_case, block) $ at
      | Default { tags = at_case } -> (DefaultC $ at_case, block) $ at)
  | FallThrough { label; tags = at } -> (
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

and desugar_struct_fields (fields : Declaration.field list) :
    (member * typ) list =
  List.map
    (fun (field : Declaration.field) ->
      let Declaration.{ name; typ; _ } = field in
      (desugar_member name, desugar_type typ))
    fields

and desugar_serial_fields (fields : (Text.t * Expression.t) list) :
    (member * expr) list =
  List.map
    (fun (field : Text.t * Expression.t) ->
      let field, value = field in
      (desugar_member field, desugar_expr value))
    fields

and desugar_members (members : Text.t list) : member list =
  List.map desugar_member members

and desugar_parser_case (case : Parser.case) : select_case =
  let Parser.{ matches; next; tags = at } = case in
  let mtchs = desugar_matches matches in
  let next = desugar_label next in
  (mtchs, next) $ at

and desugar_parser_cases (cases : Parser.case list) : select_case list =
  List.map desugar_parser_case cases

and desugar_parser_transition (trans : Parser.transition) : stmt =
  match trans with
  | Direct { next; tags = at } -> TransI (desugar_label next) $ at
  | Select { exprs; cases; tags = at } ->
      let exprs = desugar_exprs exprs in
      let cases = desugar_parser_cases cases in
      SelectI (exprs, cases) $ at

and desugar_parser_state (state : Parser.state) : parser_state =
  let Parser.{ name; statements; transition; tags = at; _ } = state in
  let id = desugar_id name in
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
      (desugar_expr key, desugar_mtch_kind match_kind) $ at)
    keys

and desugar_table_action (action : Table.action_ref) : table_action =
  let Table.{ name; args; tags = at; _ } = action in
  let var = desugar_var name in
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
      let id = desugar_id name in
      let cparams = desugar_params params in
      ConsD { id; cparams } $ at
  | AbstractMethod { name; return; type_params; params; tags = at; _ } ->
      let id = desugar_id name in
      let rettyp = desugar_type return in
      let tparams = desugar_tparams type_params in
      let params = desugar_params params in
      AbstractD { id; rettyp; tparams; params } $ at
  | Method { name; return; type_params; params; tags = at; _ } ->
      let id = desugar_id name in
      let rettyp = desugar_type return in
      let tparams = desugar_tparams type_params in
      let params = desugar_params params in
      MethodD { id; rettyp; tparams; params } $ at

and desugar_methods (mthds : MethodPrototype.t list) : decl list =
  List.map desugar_method mthds

and desugar_decl (decl : Declaration.t) : decl =
  match decl with
  | Constant { name; typ; value; tags = at; _ } ->
      let id = desugar_id name in
      let typ = desugar_type typ in
      let value = desugar_expr value in
      ConstD { id; typ; value } $ at
  | Variable { name; typ; init; tags = at; _ } ->
      let id = desugar_id name in
      let typ = desugar_type typ in
      let init = Option.map desugar_expr init in
      VarD { id; typ; init } $ at
  | Instantiation { name; typ; args; init; tags = at; _ } ->
      let id = desugar_id name in
      let typ = desugar_type typ in
      let args = desugar_args args in
      let init = Option.map desugar_block init in
      InstD { id; typ; args; init } $ at
  | Error { members; tags = at; _ } ->
      let members = desugar_members members in
      ErrD { members } $ at
  | MatchKind { members; tags = at; _ } ->
      let members = desugar_members members in
      MatchKindD { members } $ at
  | Struct { name; fields; tags = at; _ } ->
      let id = desugar_id name in
      let fields = desugar_struct_fields fields in
      StructD { id; fields } $ at
  | Header { name; fields; tags = at; _ } ->
      let id = desugar_id name in
      let fields = desugar_struct_fields fields in
      HeaderD { id; fields } $ at
  | HeaderUnion { name; fields; tags = at; _ } ->
      let id = desugar_id name in
      let fields = desugar_struct_fields fields in
      UnionD { id; fields } $ at
  | Enum { name; members; tags = at; _ } ->
      let id = desugar_id name in
      let members = desugar_members members in
      EnumD { id; members } $ at
  | SerializableEnum { name; typ; members = fields; tags = at; _ } ->
      let id = desugar_id name in
      let typ = desugar_type typ in
      let fields = desugar_serial_fields fields in
      SEnumD { id; typ; fields } $ at
  | NewType { name; typ_or_decl; tags = at; _ } ->
      let id = desugar_id name in
      let typ =
        match typ_or_decl with
        | Left typ -> (Left (desugar_type typ) : (typ, decl) alt)
        | Right decl -> (Right (desugar_decl decl) : (typ, decl) alt)
      in
      NewTypeD { id; typ } $ at
  | TypeDef { name; typ_or_decl; tags = at; _ } ->
      let id = desugar_id name in
      let typ =
        match typ_or_decl with
        | Left typ -> (Left (desugar_type typ) : (typ, decl) alt)
        | Right decl -> (Right (desugar_decl decl) : (typ, decl) alt)
      in
      TypeDefD { id; typ } $ at
  | ValueSet { name; typ; size; tags = at; _ } ->
      let id = desugar_id name in
      let typ = desugar_type typ in
      let size = desugar_expr size in
      ValueSetD { id; typ; size } $ at
  | ParserType { name; type_params; params; tags = at; _ } ->
      let id = desugar_id name in
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
      let id = desugar_id name in
      let tparams = desugar_tparams type_params in
      let params = desugar_params params in
      let cparams = desugar_cparams constructor_params in
      let locals = desugar_decls locals in
      let states = desugar_parser_states states in
      ParserD { id; tparams; params; cparams; locals; states } $ at
  | Action { name; params; body; tags = at; _ } ->
      let id = desugar_id name in
      let params = desugar_params params in
      let body = desugar_block body in
      ActionD { id; params; body } $ at
  | Table { name; properties; tags = at; _ } ->
      let id = desugar_id name in
      let table = desugar_table_properties properties in
      TableD { id; table } $ at
  | ControlType { name; type_params; params; tags = at; _ } ->
      let id = desugar_id name in
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
      let id = desugar_id name in
      let tparams = desugar_tparams type_params in
      let params = desugar_params params in
      let cparams = desugar_cparams constructor_params in
      let locals = desugar_decls locals in
      let body = desugar_block apply in
      ControlD { id; tparams; params; cparams; locals; body } $ at
  | Function { name; return; type_params; params; body; tags = at; _ } ->
      let id = desugar_id name in
      let rettyp = desugar_type return in
      let tparams = desugar_tparams type_params in
      let params = desugar_params params in
      let body = desugar_block body in
      FuncD { id; rettyp; tparams; params; body } $ at
  | ExternFunction { name; return; type_params; params; tags = at; _ } ->
      let id = desugar_id name in
      let rettyp = desugar_type return in
      let tparams = desugar_tparams type_params in
      let params = desugar_params params in
      ExternFuncD { id; rettyp; tparams; params } $ at
  | ExternObject { name; type_params; methods; tags = at; _ } ->
      let id = desugar_id name in
      let tparams = desugar_tparams type_params in
      let mthds = desugar_methods methods in
      ExternObjectD { id; tparams; mthds } $ at
  | PackageType { name; type_params; params; tags = at; _ } ->
      let id = desugar_id name in
      let tparams = desugar_tparams type_params in
      let cparams = desugar_params params in
      PackageTypeD { id; tparams; cparams } $ at

and desugar_decls (decls : Declaration.t list) : decl list =
  List.map desugar_decl decls

let desugar_program (program : p4program) : program =
  let (Program decls) = program in
  let program = desugar_decls decls in
  program
