open Syntax.Ast
open Surface.Ast
open Util.Source

(* Numbers *)

let desugar_num (num : Number.t) : num =
  (num.value, num.width_signed) $ Number.tags num

(* Texts *)

let desugar_text (text : Text.t) : text = text.str $ Text.tags text

(* Identifiers *)

let desugar_id (text : Text.t) : id = desugar_text text

(* Variables (scoped identifiers) *)

let desugar_var (name : Name.t) : var =
  let at = Name.tags name in
  match name with
  | BareName id -> Current (desugar_text id) $ at
  | QualifiedName (prefix, id) ->
      assert (prefix = []);
      Top (desugar_text id) $ at

(* Members *)

let desugar_member (text : Text.t) : member = desugar_text text

let desugar_members (members : Text.t list) : member list =
  List.map desugar_member members

(* State labels *)

let desugar_state_label (text : Text.t) : state_label = desugar_text text

(* Match kinds *)

let desugar_match_kind (text : Text.t) : match_kind = desugar_text text

(* Unary operators *)

let desugar_unop (unop : Op.un) : unop =
  match unop with
  | BitNot { tags = at } -> BNotOp $ at
  | Not { tags = at } -> LNotOp $ at
  | UMinus { tags = at } -> UMinusOp $ at

(* Binary operators *)

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

(* Annotations *)

let rec desugar_anno (anno : Annotation.t) : anno =
  let text_name = desugar_text anno.name in
  let at = anno.tags in
  match anno.body with
  | Empty _ -> EmptyN text_name $ at
  | Unparsed { str = texts; _ } ->
      let texts = List.map desugar_text texts in
      TextN (text_name, texts) $ at
  | Expression { exprs; _ } ->
      let exprs = List.map desugar_expr exprs in
      ExprN (text_name, exprs) $ at
  | KeyValue { key_values = fields; _ } ->
      let fields =
        List.map
          (fun (field : KeyValue.t) ->
            let KeyValue.{ key; value; _ } = field in
            (desugar_member key, desugar_expr value))
          fields
      in
      RecordN (text_name, fields) $ at

and desugar_annos (annos : Annotation.t list) : anno list =
  List.map desugar_anno annos

(* Types *)

and desugar_type (typ : Type.t) : typ =
  match typ with
  | Void { tags = at } -> VoidT $ at
  | Bool { tags = at } -> BoolT $ at
  | Error { tags = at } -> ErrT $ at
  | String { tags = at } -> StrT $ at
  | Integer { tags = at } -> IntT $ at
  | IntType { expr; tags = at } -> FIntT (desugar_expr expr) $ at
  | BitType { expr; tags = at } -> FBitT (desugar_expr expr) $ at
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

(* Directions *)

and desugar_dir (dir : Direction.t option) : dir =
  match dir with
  | None -> No $ no_info
  | Some (In { tags = at }) -> In $ at
  | Some (Out { tags = at }) -> Out $ at
  | Some (InOut { tags = at }) -> InOut $ at

(* Type parameters *)

and desugar_tparam (tparam : Text.t) : tparam = desugar_text tparam

and desugar_tparams (tparams : Text.t list) : tparam list =
  List.map desugar_tparam tparams

(* Parameters *)

and desugar_param (param : Parameter.t) : param =
  let at = param.tags in
  let id = desugar_id param.variable in
  let dir = desugar_dir param.direction in
  let typ = desugar_type param.typ in
  let expr_default = Option.map desugar_expr param.opt_value in
  let annos = desugar_annos param.annotations in
  (id, dir, typ, expr_default, annos) $ at

and desugar_params (params : Parameter.t list) : param list =
  List.map desugar_param params

(* Constructor parameters *)

and desugar_cparam (cparam : Parameter.t) : cparam = desugar_param cparam

and desugar_cparams (cparams : Parameter.t list) : cparam list =
  List.map desugar_cparam cparams

(* Type arguments *)

and desugar_targ (targ : Type.t) : targ = desugar_type targ

and desugar_targs (targs : Type.t list) : targ list =
  List.map desugar_targ targs

(* Arguments *)

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

and desugar_expr (expr : Expression.t) : expr =
  match expr with
  | True { tags = at } -> BoolE true $ at
  | False { tags = at } -> BoolE false $ at
  | Int { i; tags = at } -> NumE (desugar_num i) $ at
  | String { text; tags = at } -> StrE (desugar_text text) $ at
  | Name { name; tags = at } -> VarE (desugar_var name) $ at
  | List { values; tags = at } -> ListE (desugar_exprs values) $ at
  | Record { entries = fields; tags = at } ->
      let record =
        List.map
          (fun (field : KeyValue.t) ->
            let KeyValue.{ key; value; _ } = field in
            (desugar_member key, desugar_expr value))
          fields
      in
      RecordE record $ at
  | UnaryOp { op; arg; tags = at } ->
      let unop = desugar_unop op in
      let expr = desugar_expr arg in
      UnE (unop, expr) $ at
  | BinaryOp { op; args; tags = at } ->
      let binop = desugar_binop op in
      let larg, rarg = args in
      let expr_l = desugar_expr larg in
      let expr_r = desugar_expr rarg in
      BinE (binop, expr_l, expr_r) $ at
  | Ternary { cond; tru; fls; tags = at } ->
      let expr_cond = desugar_expr cond in
      let expr_then = desugar_expr tru in
      let expr_else = desugar_expr fls in
      TernE (expr_cond, expr_then, expr_else) $ at
  | Cast { typ; expr; tags = at } ->
      let typ = desugar_type typ in
      let expr = desugar_expr expr in
      CastE (typ, expr) $ at
  | Mask { expr; mask; tags = at } ->
      let expr_base = desugar_expr expr in
      let expr_mask = desugar_expr mask in
      MaskE (expr_base, expr_mask) $ at
  | Range { lo; hi; tags = at } ->
      let expr_lb = desugar_expr lo in
      let expr_ub = desugar_expr hi in
      RangeE (expr_lb, expr_ub) $ at
  | ArrayAccess { array; index; tags = at } ->
      let expr_base = desugar_expr array in
      let expr_idx = desugar_expr index in
      ArrAccE (expr_base, expr_idx) $ at
  | BitStringAccess { bits; lo; hi; tags = at } ->
      let expr_base = desugar_expr bits in
      let expr_lidx = desugar_expr lo in
      let expr_hidx = desugar_expr hi in
      BitAccE (expr_base, expr_lidx, expr_hidx) $ at
  | ErrorMember { err; tags = at } ->
      let err = desugar_member err in
      ErrAccE err $ at
  | TypeMember { typ; name; tags = at } ->
      let var = desugar_var typ in
      let member = desugar_member name in
      TypeAccE (var, member) $ at
  | ExpressionMember { expr; name; tags = at } ->
      let expr_base = desugar_expr expr in
      let member = desugar_member name in
      ExprAccE (expr_base, member) $ at
  | FunctionCall { func; type_args; args; tags = at } ->
      let expr_func = desugar_expr func in
      let targs = desugar_targs type_args in
      let args = desugar_args args in
      CallE (expr_func, targs, args) $ at
  | NamelessInstantiation { typ; args; tags = at } ->
      let typ = desugar_type typ in
      let args = desugar_args args in
      InstE (typ, args) $ at

and desugar_exprs (exprs : Expression.t list) : expr list =
  List.map desugar_expr exprs

(* Keyset expressions *)

and desugar_keyset (mtch : Match.t) : keyset =
  match mtch with
  | Default { tags = at } -> DefaultK $ at
  | DontCare { tags = at } -> AnyK $ at
  | Expression { expr; tags = at } -> ExprK (desugar_expr expr) $ at

and desugar_keysets (mtchs : Match.t list) : keyset list =
  List.map desugar_keyset mtchs

(* Statements *)

and desugar_stmt (stmt : Statement.t) : stmt =
  match stmt with
  | EmptyStatement { tags = at } -> EmptyS $ at
  | Assignment { lhs; rhs; tags = at } ->
      let expr_lhs = desugar_expr lhs in
      let expr_rhs = desugar_expr rhs in
      AssignS (expr_lhs, expr_rhs) $ at
  | Switch { expr; cases; tags = at } ->
      let expr = desugar_expr expr in
      let switch_cases = desugar_switch_cases cases in
      SwitchS (expr, switch_cases) $ at
  | Conditional { cond; tru; fls; tags = at } ->
      let expr_cond = desugar_expr cond in
      let stmt_then = desugar_stmt tru in
      let stmt_else =
        match fls with Some fls -> desugar_stmt fls | None -> EmptyS $ no_info
      in
      IfS (expr_cond, stmt_then, stmt_else) $ at
  | BlockStatement { block; tags = at } ->
      let block = desugar_block block in
      BlockS block $ at
  | Exit { tags = at } -> ExitS $ at
  | Return { expr; tags = at } ->
      let expr = Option.map desugar_expr expr in
      RetS expr $ at
  | MethodCall { func; type_args; args; tags = at } ->
      let expr_func = desugar_expr func in
      let targs = desugar_targs type_args in
      let args = desugar_args args in
      CallS (expr_func, targs, args) $ at
  | DeclarationStatement { decl; tags = at } ->
      let decl = desugar_decl decl in
      DeclS decl $ at
  | _ ->
      Format.eprintf "(TODO: desugar_stmt) %s\n"
        (Surface.Print.print_stmt 0 stmt);
      assert false

and desugar_stmts (stmts : Statement.t list) : stmt list =
  List.map desugar_stmt stmts

(* Blocks (sequence of statements *)

and desugar_block (block : Block.t) : block =
  let at = block.tags in
  let stmts = desugar_stmts block.statements in
  let annos = desugar_annos block.annotations in
  (stmts, annos) $ at

(* Match-cases for switch *)

and desugar_switch_label (label : Statement.switch_label) : switch_label =
  match label with
  | Default { tags = at } -> DefaultL $ at
  | Name { name; tags = at } -> NameL (desugar_text name) $ at

and desugar_switch_case (case : Statement.switch_case) : switch_case =
  match case with
  | Action { label; code; tags = at } ->
      let switch_label = desugar_switch_label label in
      let block = desugar_block code in
      MatchC (switch_label, block) $ at
  | FallThrough { label; tags = at } ->
      let switch_label = desugar_switch_label label in
      FallC switch_label $ at

and desugar_switch_cases (cases : Statement.switch_case list) : switch_case list
    =
  List.map desugar_switch_case cases

(* Select-cases for select *)

(* Declarations *)

and desugar_decl (decl : Declaration.t) : decl =
  match decl with
  | Constant { name; typ; value; tags = at; annotations } ->
      let id = desugar_id name in
      let typ = desugar_type typ in
      let value = desugar_expr value in
      let annos = desugar_annos annotations in
      ConstD { id; typ; value; annos } $ at
  | Variable { name; typ; init; tags = at; annotations } ->
      let id = desugar_id name in
      let typ = desugar_type typ in
      let init = Option.map desugar_expr init in
      let annos = desugar_annos annotations in
      VarD { id; typ; init; annos } $ at
  | Instantiation { name; typ; args; init; tags = at; annotations } ->
      let id = desugar_id name in
      let typ = desugar_type typ in
      let args = desugar_args args in
      let init = Option.map desugar_block init in
      let annos = desugar_annos annotations in
      InstD { id; typ; args; init; annos } $ at
  | Error { members; tags = at } ->
      let members = desugar_members members in
      ErrD { members } $ at
  | MatchKind { members; tags = at } ->
      let members = desugar_members members in
      MatchKindD { members } $ at
  | Struct { name; fields; tags = at; annotations } ->
      let id = desugar_id name in
      let fields = desugar_record_fields fields in
      let annos = desugar_annos annotations in
      StructD { id; fields; annos } $ at
  | Header { name; fields; tags = at; annotations } ->
      let id = desugar_id name in
      let fields = desugar_record_fields fields in
      let annos = desugar_annos annotations in
      HeaderD { id; fields; annos } $ at
  | HeaderUnion { name; fields; tags = at; annotations } ->
      let id = desugar_id name in
      let fields = desugar_record_fields fields in
      let annos = desugar_annos annotations in
      UnionD { id; fields; annos } $ at
  | Enum { name; members; tags = at; annotations } ->
      let id = desugar_id name in
      let members = desugar_members members in
      let annos = desugar_annos annotations in
      EnumD { id; members; annos } $ at
  | SerializableEnum { name; typ; members = fields; tags = at; annotations } ->
      let id = desugar_id name in
      let typ = desugar_type typ in
      let fields = desugar_serial_fields fields in
      let annos = desugar_annos annotations in
      SEnumD { id; typ; fields; annos } $ at
  | NewType { name; typ_or_decl; tags = at; annotations } ->
      let id = desugar_id name in
      let typdef =
        match typ_or_decl with
        | Left typ -> (Left (desugar_type typ) : (typ, decl) alt)
        | Right decl -> (Right (desugar_decl decl) : (typ, decl) alt)
      in
      let annos = desugar_annos annotations in
      NewTypeD { id; typdef; annos } $ at
  | TypeDef { name; typ_or_decl; tags = at; annotations } ->
      let id = desugar_id name in
      let typdef =
        match typ_or_decl with
        | Left typ -> (Left (desugar_type typ) : (typ, decl) alt)
        | Right decl -> (Right (desugar_decl decl) : (typ, decl) alt)
      in
      let annos = desugar_annos annotations in
      TypeDefD { id; typdef; annos } $ at
  | ValueSet { name; typ; size; tags = at; annotations } ->
      let id = desugar_id name in
      let typ = desugar_type typ in
      let size = desugar_expr size in
      let annos = desugar_annos annotations in
      ValueSetD { id; typ; size; annos } $ at
  | ParserType { name; type_params; params; tags = at; annotations } ->
      let id = desugar_id name in
      let tparams = desugar_tparams type_params in
      let params = desugar_params params in
      let annos = desugar_annos annotations in
      ParserTypeD { id; tparams; params; annos } $ at
  | Parser
      {
        name;
        type_params;
        params;
        constructor_params;
        locals;
        states;
        tags = at;
        annotations;
      } ->
      let id = desugar_id name in
      let tparams = desugar_tparams type_params in
      let params = desugar_params params in
      let cparams = desugar_cparams constructor_params in
      let locals = desugar_decls locals in
      let states = desugar_parser_states states in
      let annos = desugar_annos annotations in
      ParserD { id; tparams; params; cparams; locals; states; annos } $ at
  | Action { name; params; body; tags = at; annotations } ->
      let id = desugar_id name in
      let params = desugar_params params in
      let body = desugar_block body in
      let annos = desugar_annos annotations in
      ActionD { id; params; body; annos } $ at
  | Table { name; properties; tags = at; annotations } ->
      let id = desugar_id name in
      let table = desugar_table_properties properties in
      let annos = desugar_annos annotations in
      TableD { id; table; annos } $ at
  | ControlType { name; type_params; params; tags = at; annotations } ->
      let id = desugar_id name in
      let tparams = desugar_tparams type_params in
      let params = desugar_params params in
      let annos = desugar_annos annotations in
      ControlTypeD { id; tparams; params; annos } $ at
  | Control
      {
        name;
        type_params;
        params;
        constructor_params;
        locals;
        apply;
        tags = at;
        annotations;
      } ->
      let id = desugar_id name in
      let tparams = desugar_tparams type_params in
      let params = desugar_params params in
      let cparams = desugar_cparams constructor_params in
      let locals = desugar_decls locals in
      let body = desugar_block apply in
      let annos = desugar_annos annotations in
      ControlD { id; tparams; params; cparams; locals; body; annos } $ at
  | Function { name; return; type_params; params; body; tags = at } ->
      let id = desugar_id name in
      let typ_ret = desugar_type return in
      let tparams = desugar_tparams type_params in
      let params = desugar_params params in
      let body = desugar_block body in
      FuncD { id; typ_ret; tparams; params; body } $ at
  | ExternFunction { name; return; type_params; params; tags = at; annotations }
    ->
      let id = desugar_id name in
      let typ_ret = desugar_type return in
      let tparams = desugar_tparams type_params in
      let params = desugar_params params in
      let annos = desugar_annos annotations in
      ExtFuncD { id; typ_ret; tparams; params; annos } $ at
  | ExternObject { name; type_params; methods; tags = at; annotations } ->
      let id = desugar_id name in
      let tparams = desugar_tparams type_params in
      let mthds = desugar_methods methods in
      let annos = desugar_annos annotations in
      ExtObjectD { id; tparams; mthds; annos } $ at
  | PackageType { name; type_params; params; tags = at; annotations } ->
      let id = desugar_id name in
      let tparams = desugar_tparams type_params in
      let cparams = desugar_params params in
      let annos = desugar_annos annotations in
      PackageTypeD { id; tparams; cparams; annos } $ at

and desugar_decls (decls : Declaration.t list) : decl list =
  List.map desugar_decl decls

and desugar_record_fields (fields : Declaration.field list) :
    (member * typ * anno list) list =
  List.map
    (fun (field : Declaration.field) ->
      let Declaration.{ name; typ; annotations; _ } = field in
      (desugar_member name, desugar_type typ, desugar_annos annotations))
    fields

and desugar_serial_fields (fields : (Text.t * Expression.t) list) :
    (member * expr) list =
  List.map
    (fun (field : Text.t * Expression.t) ->
      let field, value = field in
      (desugar_member field, desugar_expr value))
    fields

and desugar_method (mthd : MethodPrototype.t) : decl =
  match mthd with
  | Constructor { name; params; tags = at; annotations } ->
      let id = desugar_id name in
      let cparams = desugar_params params in
      let annos = desugar_annos annotations in
      ExtConstructorD { id; cparams; annos } $ at
  | AbstractMethod { name; return; type_params; params; tags = at; annotations }
    ->
      let id = desugar_id name in
      let typ_ret = desugar_type return in
      let tparams = desugar_tparams type_params in
      let params = desugar_params params in
      let annos = desugar_annos annotations in
      ExtAbstractMethodD { id; typ_ret; tparams; params; annos } $ at
  | Method { name; return; type_params; params; tags = at; annotations } ->
      let id = desugar_id name in
      let typ_ret = desugar_type return in
      let tparams = desugar_tparams type_params in
      let params = desugar_params params in
      let annos = desugar_annos annotations in
      ExtMethodD { id; typ_ret; tparams; params; annos } $ at

and desugar_methods (mthds : MethodPrototype.t list) : decl list =
  List.map desugar_method mthds

(* Parser state machine *)

and desugar_parser_case (case : Parser.case) : select_case =
  let Parser.{ matches; next; tags = at } = case in
  let keysets = desugar_keysets matches in
  let state_label = desugar_state_label next in
  (keysets, state_label) $ at

and desugar_parser_cases (cases : Parser.case list) : select_case list =
  List.map desugar_parser_case cases

and desugar_parser_transition (trans : Parser.transition) : stmt =
  let expr_trans =
    match trans with
    | Direct { next; tags = at } ->
        VarE (Current (desugar_id next) $ no_info) $ at
    | Select { exprs; cases; tags = at } ->
        let exprs = desugar_exprs exprs in
        let select_cases = desugar_parser_cases cases in
        SelectE (exprs, select_cases) $ at
  in
  TransS expr_trans $ expr_trans.at

and desugar_parser_state (state : Parser.state) : parser_state =
  let Parser.{ name; statements; transition; tags = at; annotations } = state in
  let state_label = desugar_state_label name in
  let stmts =
    desugar_stmts statements @ [ desugar_parser_transition transition ]
  in
  let block = (stmts, []) $ no_info in
  let annos = desugar_annos annotations in
  (state_label, block, annos) $ at

and desugar_parser_states (states : Parser.state list) : parser_state list =
  List.map desugar_parser_state states

(* Tables *)

and desugar_table_properties (properties : Table.property list) :
    table_key list
    * table_action list
    * table_entry list
    * table_default option
    * table_custom list =
  List.fold_left
    (fun (table_keys, table_actions, table_entries, table_default, table_customs)
         (property : Table.property) ->
      match property with
      | Key { keys; _ } ->
          let table_keys = desugar_table_keys keys in
          ( table_keys,
            table_actions,
            table_entries,
            table_default,
            table_customs )
      | Actions { actions; _ } ->
          let table_actions = desugar_table_actions actions in
          ( table_keys,
            table_actions,
            table_entries,
            table_default,
            table_customs )
      | Entries { entries; _ } ->
          let table_entries = desugar_table_entries entries in
          ( table_keys,
            table_actions,
            table_entries,
            table_default,
            table_customs )
      | DefaultAction { action; const; tags = at; _ } ->
          assert (table_default = None);
          let table_action = desugar_table_action action in
          let table_default = Some ((table_action, const) $ at) in
          ( table_keys,
            table_actions,
            table_entries,
            table_default,
            table_customs )
      | Custom { name; value; const; tags = at; annotations } ->
          let text_name = desugar_text name in
          let expr = desugar_expr value in
          let annos = desugar_annos annotations in
          let table_custom = (text_name, expr, const, annos) $ at in
          ( table_keys,
            table_actions,
            table_entries,
            table_default,
            table_customs @ [ table_custom ] ))
    ([], [], [], None, []) properties

(* Table keys *)

and desugar_table_keys (keys : Table.key list) : table_key list =
  List.map
    (fun (key : Table.key) ->
      let Table.{ key; match_kind; tags = at; annotations } = key in
      ( desugar_expr key,
        desugar_match_kind match_kind,
        desugar_annos annotations )
      $ at)
    keys

(* Table actions *)

and desugar_table_action (action : Table.action_ref) : table_action =
  let Table.{ name; args; tags = at; annotations } = action in
  let var = desugar_var name in
  let args = desugar_args args in
  let annos = desugar_annos annotations in
  (var, args, annos) $ at

and desugar_table_actions (actions : Table.action_ref list) : table_action list
    =
  List.map desugar_table_action actions

(* Table entries *)

and desugar_table_entry (entry : Table.entry) : table_entry =
  let Table.{ matches; action; tags = at; annotations } = entry in
  let keysets = desugar_keysets matches in
  let table_action = desugar_table_action action in
  let annos = desugar_annos annotations in
  (keysets, table_action, annos) $ at

and desugar_table_entries (entries : Table.entry list) : table_entry list =
  List.map desugar_table_entry entries

(* Program *)

let desugar_program (program : p4program) : program =
  let (Program decls) = program in
  let program = desugar_decls decls in
  program
