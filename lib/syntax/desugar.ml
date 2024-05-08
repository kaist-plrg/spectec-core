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
  match unop with BitNot _ -> BNot | Not _ -> LNot | UMinus _ -> UMinus

let desugar_binop (binop : Op.bin) : binop =
  match binop with
  | Plus _ -> Plus
  | PlusSat _ -> SPlus
  | Minus _ -> Minus
  | MinusSat _ -> SMinus
  | Mul _ -> Mul
  | Div _ -> Div
  | Mod _ -> Mod
  | Shl _ -> Shl
  | Shr _ -> Shr
  | Le _ -> Le
  | Ge _ -> Ge
  | Lt _ -> Lt
  | Gt _ -> Gt
  | Eq _ -> Eq
  | NotEq _ -> Ne
  | BitAnd _ -> BAnd
  | BitXor _ -> BXor
  | BitOr _ -> BOr
  | PlusPlus _ -> Concat
  | And _ -> LAnd
  | Or _ -> LOr

(* Types *)

let rec desugar_type (typ : Type.t) : typ =
  match typ with
  | Void _ -> TVoid
  | Bool _ -> TBool
  | Error _ -> TErr
  | String _ -> TStr
  | Integer _ -> TAInt
  | IntType { expr; _ } -> TInt (desugar_expr expr)
  | BitType { expr; _ } -> TBit (desugar_expr expr)
  | VarBit { expr; _ } -> TVBit (desugar_expr expr)
  | TypeName { name; _ } -> TName (desugar_name name)
  | SpecializedType { base; args; _ } ->
      let var =
        match desugar_type base with TName name -> name | _ -> assert false
      in
      let targs = List.map desugar_type args in
      TSpec (var, targs)
  | HeaderStack { header; size; _ } ->
      let typ = desugar_type header in
      let size = desugar_expr size in
      TStack (typ, size)
  | Tuple { args; _ } ->
      let targs = List.map desugar_type args in
      TTuple targs
  | DontCare _ -> TAny

and desugar_types (typs : Type.t list) : typ list =
  List.map desugar_type typs

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
  | Expression { value; _ } -> AExpr (desugar_expr value)
  | KeyValue { key; value; _ } ->
      let key = key.str in
      let value = desugar_expr value in
      AName (key, value)
  | Missing _ -> AAny

and desugar_args (args : Argument.t list) : arg list =
  List.map desugar_arg args

(* Expressions *)

and desugar_record (entries : KeyValue.t list) : (string * expr) list =
  List.map
    (fun (entry : KeyValue.t) ->
      let KeyValue.{ key; value; _ } = entry in
      (key.str, desugar_expr value))
    entries

and desugar_expr (expr : Expression.t) : expr =
  match expr with
  | True _ -> EBool true
  | False _ -> EBool false
  | Int { i; _ } -> ENum (desugar_num i)
  | String { text; _ } -> EStr text.str
  | Name { name; _ } -> EVar (desugar_name name)
  | List { values; _ } -> EList (desugar_exprs values)
  | Record { entries; _ } ->
      let record = desugar_record entries in
      ERecord record
  | UnaryOp { op; arg; _ } ->
      let op = desugar_unop op in
      let arg = desugar_expr arg in
      EUnop (op, arg)
  | BinaryOp { op; args; _ } ->
      let op = desugar_binop op in
      let larg, rarg = args in
      let larg = desugar_expr larg in
      let rarg = desugar_expr rarg in
      EBinop (op, larg, rarg)
  | Ternary { cond; tru; fls; _ } ->
      let cond = desugar_expr cond in
      let tru = desugar_expr tru in
      let fls = desugar_expr fls in
      ETern (cond, tru, fls)
  | Cast { typ; expr; _ } ->
      let typ = desugar_type typ in
      let expr = desugar_expr expr in
      ECast (typ, expr)
  | Mask { expr; mask; _ } ->
      let expr = desugar_expr expr in
      let mask = desugar_expr mask in
      EMask (expr, mask)
  | Range { lo; hi; _ } ->
      let lo = desugar_expr lo in
      let hi = desugar_expr hi in
      ERange (lo, hi)
  | ArrayAccess { array; index; _ } ->
      let arr = desugar_expr array in
      let idx = desugar_expr index in
      EAccArr (arr, idx)
  | BitStringAccess { bits; lo; hi; _ } ->
      let bits = desugar_expr bits in
      let lidx = desugar_expr lo in
      let hidx = desugar_expr hi in
      EAccBit (bits, lidx, hidx)
  | TypeMember { typ; name; _ } ->
      let typ = desugar_name typ in
      let name = name.str in
      EMemTyp (typ, name)
  | ErrorMember { err; _ } ->
      let err = err.str in
      EMemErr err
  | ExpressionMember { expr; name; _ } ->
      let expr = desugar_expr expr in
      let name = name.str in
      EMemExpr (expr, name)
  | FunctionCall { func; type_args; args; _ } ->
      let func = desugar_expr func in
      let targs = desugar_types type_args in
      let args = desugar_args args in
      ECall (func, targs, args)
  | NamelessInstantiation { typ; args; _ } ->
      let typ = desugar_type typ in
      let args = desugar_args args in
      EInst (typ, args)

and desugar_exprs (exprs : Expression.t list) : expr list =
  List.map desugar_expr exprs

(* Match-cases *)

and desugar_match (mtch : Match.t) : mtch =
  match mtch with
  | Expression { expr; _ } -> MExpr (desugar_expr expr)
  | Default _ -> MDefault
  | DontCare _ -> MAny

and desugar_matches (mtchs : Match.t list) : mtch list =
  List.map desugar_match mtchs

(* Statements *)

and desugar_switch_case (case : Statement.switch_case) : switch_case =
  match case with
  | Action { label; code; _ } -> (
      let block = desugar_stmts code.statements in
      match label with
      | Name { name; _ } -> (CCase name.str, block)
      | Default _ -> (CDefault, block))
  | FallThrough { label; _ } -> (
      match label with
      | Name { name; _ } -> (CCase name.str, [])
      | Default _ -> assert false)

and desugar_switch_cases (cases : Statement.switch_case list) :
    switch_case list =
  List.map desugar_switch_case cases

and desugar_stmt (stmt : Statement.t) : stmt =
  match stmt with
  | EmptyStatement _ -> SEmpty
  | Assignment { lhs; rhs; _ } ->
      let lhs = desugar_expr lhs in
      let rhs = desugar_expr rhs in
      SAssign (lhs, rhs)
  | Switch { expr; cases; _ } ->
      let expr = desugar_expr expr in
      let cases = desugar_switch_cases cases in
      SSwitch (expr, cases)
  | Conditional { cond; tru; fls; _ } ->
      let cond = desugar_expr cond in
      let tru = desugar_stmt tru in
      let fls =
        match fls with Some fls -> desugar_stmt fls | None -> SEmpty
      in
      SCond (cond, tru, fls)
  | BlockStatement { block; _ } ->
      let stmts = desugar_stmts block.statements in
      SBlock stmts
  | Exit _ -> SExit
  | Return { expr; _ } ->
      let expr = Option.map desugar_expr expr in
      SReturn expr
  | MethodCall { func; type_args; args; _ } ->
      let func = desugar_expr func in
      let targs = desugar_types type_args in
      let args = desugar_args args in
      SCall (func, targs, args)
  | DeclarationStatement { decl; _ } ->
      let decl = desugar_decl decl in
      SDecl decl
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
  | Direct { next; _ } -> STrans next.str
  | Select { exprs; cases; _ } ->
      let exprs = desugar_exprs exprs in
      let cases = desugar_parser_cases cases in
      SSelect (exprs, cases)

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

and desugar_table_actions (actions : Table.action_ref list) :
    table_action list =
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
    (fun (keys, actions, entries, default, customs)
         (property : Table.property) ->
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
      DCons { name; params }
  | AbstractMethod { name; return; type_params; params; _ } ->
      let name = name.str in
      let rettyp = desugar_type return in
      let tparams = desugar_tparams type_params in
      let params = desugar_params params in
      DAbstract { name; rettyp; tparams; params }
  | Method { name; return; type_params; params; _ } ->
      let name = name.str in
      let rettyp = desugar_type return in
      let tparams = desugar_tparams type_params in
      let params = desugar_params params in
      DMethod { name; rettyp; tparams; params }

and desugar_methods (mthds : MethodPrototype.t list) : decl list =
  List.map desugar_method mthds

and desugar_decl (decl : Declaration.t) : decl =
  match decl with
  | Constant { name; typ; value; _ } ->
      let name = name.str in
      let typ = desugar_type typ in
      let value = desugar_expr value in
      DConst { name; typ; value }
  | Variable { name; typ; init; _ } ->
      let name = name.str in
      let typ = desugar_type typ in
      let init = Option.map desugar_expr init in
      DVar { name; typ; init }
  | Instantiation { name; typ; args; init; _ } ->
      let name = name.str in
      let typ = desugar_type typ in
      let args = desugar_args args in
      let init =
        Option.map
          (fun (init : Block.t) -> desugar_stmts init.statements)
          init
      in
      DInst { name; typ; args; init }
  | Error { members; _ } ->
      let members = desugar_members members in
      DErr { members }
  | MatchKind { members; _ } ->
      let members = desugar_members members in
      DMatchKind { members }
  | Struct { name; fields; _ } ->
      let name = name.str in
      let fields = desugar_fields fields in
      DStruct { name; fields }
  | Header { name; fields; _ } ->
      let name = name.str in
      let fields = desugar_fields fields in
      DHeader { name; fields }
  | HeaderUnion { name; fields; _ } ->
      let name = name.str in
      let fields = desugar_fields fields in
      DUnion { name; fields }
  | Enum { name; members; _ } ->
      let name = name.str in
      let members = desugar_members members in
      DEnum { name; members }
  | SerializableEnum { name; typ; members; _ } ->
      let name = name.str in
      let typ = desugar_type typ in
      let members = desugar_serial_members members in
      DSEnum { name; typ; members }
  | NewType { name; typ_or_decl; _ } ->
      let name = name.str in
      let typ, decl =
        match typ_or_decl with
        | Utils.Alternative.Left typ -> (Some (desugar_type typ), None)
        | Utils.Alternative.Right decl -> (None, Some (desugar_decl decl))
      in
      DNewTyp { name; typ; decl }
  | TypeDef { name; typ_or_decl; _ } ->
      let name = name.str in
      let typ, decl =
        match typ_or_decl with
        | Utils.Alternative.Left typ -> (Some (desugar_type typ), None)
        | Utils.Alternative.Right decl -> (None, Some (desugar_decl decl))
      in
      DDefTyp { name; typ; decl }
  | ValueSet { name; typ; size; _ } ->
      let name = name.str in
      let typ = desugar_type typ in
      let size = desugar_expr size in
      DVSet { name; typ; size }
  | ParserType { name; type_params; params; _ } ->
      let name = name.str in
      let tparams = desugar_tparams type_params in
      let params = desugar_params params in
      DParserTyp { name; tparams; params }
  | Parser { name; type_params; params; constructor_params; locals; states; _ }
    ->
      let name = name.str in
      let tparams = desugar_tparams type_params in
      let params = desugar_params params in
      let cparams = desugar_cparams constructor_params in
      let locals = desugar_decls locals in
      let states = desugar_parser_states states in
      DParser { name; tparams; params; cparams; locals; states }
  | Action { name; params; body; _ } ->
      let name = name.str in
      let params = desugar_params params in
      let body = desugar_stmts body.statements in
      DAction { name; params; body }
  | Table { name; properties; _ } ->
      let name = name.str in
      let key, actions, entries, default, custom =
        desugar_table_properties properties
      in
      DTable { name; key; actions; entries; default; custom }
  | ControlType { name; type_params; params; _ } ->
      let name = name.str in
      let tparams = desugar_tparams type_params in
      let params = desugar_params params in
      DControlTyp { name; tparams; params }
  | Control { name; type_params; params; constructor_params; locals; apply; _ }
    ->
      let name = name.str in
      let tparams = desugar_tparams type_params in
      let params = desugar_params params in
      let cparams = desugar_cparams constructor_params in
      let locals = desugar_decls locals in
      let body = desugar_stmts apply.statements in
      DControl { name; tparams; params; cparams; locals; body }
  | Function { name; return; type_params; params; body; _ } ->
      let name = name.str in
      let rettyp = desugar_type return in
      let tparams = desugar_tparams type_params in
      let params = desugar_params params in
      let body = desugar_stmts body.statements in
      DFunc { name; rettyp; tparams; params; body }
  | ExternFunction { name; return; type_params; params; _ } ->
      let name = name.str in
      let rettyp = desugar_type return in
      let tparams = desugar_tparams type_params in
      let params = desugar_params params in
      DExtFunc { name; rettyp; tparams; params }
  | ExternObject { name; type_params; methods; _ } ->
      let name = name.str in
      let tparams = desugar_tparams type_params in
      let methods = desugar_methods methods in
      DExtObj { name; tparams; methods }
  | PackageType { name; type_params; params; _ } ->
      let name = name.str in
      let tparams = desugar_tparams type_params in
      let params = desugar_params params in
      DPkgTyp { name; tparams; params }

and desugar_decls (decls : Declaration.t list) : decl list =
  List.map desugar_decl decls

let desugar (program : p4program) : program =
  let (Program decls) = program in
  desugar_decls decls
