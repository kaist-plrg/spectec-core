module L = P4lang.Ast
module Elpp = P4el.Pp
module El = P4el.Ast
open P4surface.Ast
open P4util.Source

(* Numbers *)

let transform_num (num : Number.t) : El.num =
  (num.value, num.width_signed) $ Number.tags num

(* Texts *)

let transform_text (text : Text.t) : El.text = text.str $ Text.tags text

(* Identifiers *)

let transform_id (text : Text.t) : El.id = transform_text text

(* Variables (scoped identifiers) *)

let transform_var (name : Name.t) : El.var =
  let at = Name.tags name in
  match name with
  | BareName id -> L.Current (transform_text id) $ at
  | QualifiedName (prefix, id) ->
      assert (prefix = []);
      L.Top (transform_text id) $ at

(* Members *)

let transform_member (text : Text.t) : El.member = transform_text text

let transform_members (members : Text.t list) : El.member list =
  List.map transform_member members

(* State labels *)

let transform_state_label (text : Text.t) : El.state_label = transform_text text

(* Match kinds *)

let transform_match_kind (text : Text.t) : El.match_kind = transform_text text

(* Unary operators *)

let transform_unop (unop : Op.un) : El.unop =
  match unop with
  | BitNot { tags = at } -> L.BNotOp $ at
  | Not { tags = at } -> L.LNotOp $ at
  | UPlus { tags = at } -> L.UPlusOp $ at
  | UMinus { tags = at } -> L.UMinusOp $ at

(* Binary operators *)

let transform_binop (binop : Op.bin) : El.binop =
  match binop with
  | Plus { tags = at } -> L.PlusOp $ at
  | PlusSat { tags = at } -> L.SPlusOp $ at
  | Minus { tags = at } -> L.MinusOp $ at
  | MinusSat { tags = at } -> L.SMinusOp $ at
  | Mul { tags = at } -> L.MulOp $ at
  | Div { tags = at } -> L.DivOp $ at
  | Mod { tags = at } -> L.ModOp $ at
  | Shl { tags = at } -> L.ShlOp $ at
  | Shr { tags = at } -> L.ShrOp $ at
  | Le { tags = at } -> L.LeOp $ at
  | Ge { tags = at } -> L.GeOp $ at
  | Lt { tags = at } -> L.LtOp $ at
  | Gt { tags = at } -> L.GtOp $ at
  | Eq { tags = at } -> L.EqOp $ at
  | NotEq { tags = at } -> L.NeOp $ at
  | BitAnd { tags = at } -> L.BAndOp $ at
  | BitXor { tags = at } -> L.BXorOp $ at
  | BitOr { tags = at } -> L.BOrOp $ at
  | PlusPlus { tags = at } -> L.ConcatOp $ at
  | And { tags = at } -> L.LAndOp $ at
  | Or { tags = at } -> L.LOrOp $ at

(* Annotations *)

let rec transform_anno (anno : Annotation.t) : El.anno =
  let text_name = transform_text anno.name in
  let at = anno.tags in
  match anno.body with
  | Empty _ -> L.EmptyN text_name $ at
  | Unparsed { str = texts; _ } ->
      let texts = List.map transform_text texts in
      L.TextN (text_name, texts) $ at
  | Expression { exprs; _ } ->
      let exprs = List.map transform_expr exprs in
      L.ExprN (text_name, exprs) $ at
  | KeyValue { key_values = fields; _ } ->
      let fields =
        List.map
          (fun (field : KeyValue.t) ->
            let KeyValue.{ key; value; _ } = field in
            (transform_member key, transform_expr value))
          fields
      in
      L.RecordN (text_name, fields) $ at

and transform_annos (annos : Annotation.t list) : El.anno list =
  List.map transform_anno annos

(* Types *)

and transform_type (typ : Type.t) : El.typ =
  match typ with
  | Void { tags = at } -> El.VoidT $ at
  | MatchKind { tags = at } -> El.MatchKindT $ at
  | Bool { tags = at } -> El.BoolT $ at
  | Error { tags = at } -> El.ErrT $ at
  | String { tags = at } -> El.StrT $ at
  | Integer { tags = at } -> El.IntT $ at
  | IntType { expr; tags = at } -> El.FIntT (transform_expr expr) $ at
  | BitType { expr; tags = at } -> El.FBitT (transform_expr expr) $ at
  | VarBit { expr; tags = at } -> El.VBitT (transform_expr expr) $ at
  | TypeName { name; tags = at } -> El.NameT (transform_var name) $ at
  | SpecializedType { base; args; tags = at } ->
      let var =
        let typ = transform_type base in
        match typ.it with NameT name -> name | _ -> assert false
      in
      let targs = List.map transform_type args in
      El.SpecT (var, targs) $ at
  | HeaderStack { header; size; tags = at } ->
      let typ = transform_type header in
      let size = transform_expr size in
      El.StackT (typ, size) $ at
  | List { typ; tags = at } ->
      let typ = transform_type typ in
      El.ListT typ $ at
  | Tuple { args; tags = at } ->
      let targs = List.map transform_type args in
      El.TupleT targs $ at
  | DontCare { tags = at } -> El.AnyT $ at

and transform_types (typs : Type.t list) : El.typ list =
  List.map transform_type typs

(* Directions *)

and transform_dir (dir : Direction.t option) : El.dir =
  match dir with
  | None -> L.No $ no_info
  | Some (In { tags = at }) -> L.In $ at
  | Some (Out { tags = at }) -> L.Out $ at
  | Some (InOut { tags = at }) -> L.InOut $ at

(* Type parameters *)

and transform_tparam (tparam : Text.t) : El.tparam = transform_text tparam

and transform_tparams (tparams : Text.t list) : El.tparam list =
  List.map transform_tparam tparams

(* Parameters *)

and transform_param (param : Parameter.t) : El.param =
  let at = param.tags in
  let id = transform_id param.variable in
  let dir = transform_dir param.direction in
  let typ = transform_type param.typ in
  let expr_default = Option.map transform_expr param.opt_value in
  let annos = transform_annos param.annotations in
  (id, dir, typ, expr_default, annos) $ at

and transform_params (params : Parameter.t list) : El.param list =
  List.map transform_param params

(* Constructor parameters *)

and transform_cparam (cparam : Parameter.t) : El.cparam = transform_param cparam

and transform_cparams (cparams : Parameter.t list) : El.cparam list =
  List.map transform_cparam cparams

(* Type arguments *)

and transform_targ (targ : Type.t) : El.targ = transform_type targ

and transform_targs (targs : Type.t list) : El.targ list =
  List.map transform_targ targs

(* Arguments *)

and transform_arg (arg : Argument.t) : El.arg =
  match arg with
  | Expression { value; tags = at } -> L.ExprA (transform_expr value) $ at
  | KeyValue { key; value; tags = at } ->
      let key = transform_id key in
      let value = Option.map transform_expr value in
      L.NameA (key, value) $ at
  | Missing { tags = at } -> L.AnyA $ at

and transform_args (args : Argument.t list) : El.arg list =
  List.map transform_arg args

(* Expressions *)

and transform_expr (expr : Expression.t) : El.expr =
  match expr with
  | True { tags = at } -> El.BoolE { boolean = true } $ at
  | False { tags = at } -> El.BoolE { boolean = false } $ at
  | String { text; tags = at } -> El.StrE { text = transform_text text } $ at
  | Int { i; tags = at } -> El.NumE { num = transform_num i } $ at
  | Name { name; tags = at } -> El.VarE { var = transform_var name } $ at
  | List { values; tags = at } ->
      El.SeqE { exprs = transform_exprs values } $ at
  | ListDots { values; tags = at } ->
      El.SeqDefaultE { exprs = transform_exprs values } $ at
  | Record { entries = fields; tags = at } ->
      let fields =
        List.map
          (fun (field : KeyValue.t) ->
            let KeyValue.{ key; value; _ } = field in
            (transform_member key, transform_expr value))
          fields
      in
      El.RecordE { fields } $ at
  | RecordDots { entries = fields; tags = at } ->
      let fields =
        List.map
          (fun (field : KeyValue.t) ->
            let KeyValue.{ key; value; _ } = field in
            (transform_member key, transform_expr value))
          fields
      in
      El.RecordDefaultE { fields } $ at
  | Dots { tags = at } -> El.DefaultE $ at
  | Invalid { tags = at } -> El.InvalidE $ at
  | UnaryOp { op; arg; tags = at } ->
      let unop = transform_unop op in
      let expr = transform_expr arg in
      El.UnE { unop; expr } $ at
  | BinaryOp { op; args; tags = at } ->
      let binop = transform_binop op in
      let larg, rarg = args in
      let expr_l = transform_expr larg in
      let expr_r = transform_expr rarg in
      El.BinE { binop; expr_l; expr_r } $ at
  | Ternary { cond; tru; fls; tags = at } ->
      let expr_cond = transform_expr cond in
      let expr_then = transform_expr tru in
      let expr_else = transform_expr fls in
      El.TernE { expr_cond; expr_then; expr_else } $ at
  | Cast { typ; expr; tags = at } ->
      let typ = transform_type typ in
      let expr = transform_expr expr in
      El.CastE { typ; expr } $ at
  | Mask { expr; mask; tags = at } ->
      let expr_base = transform_expr expr in
      let expr_mask = transform_expr mask in
      El.MaskE { expr_base; expr_mask } $ at
  | Range { lo; hi; tags = at } ->
      let expr_lb = transform_expr lo in
      let expr_ub = transform_expr hi in
      El.RangeE { expr_lb; expr_ub } $ at
  | ArrayAccess { array; index; tags = at } ->
      let expr_base = transform_expr array in
      let expr_idx = transform_expr index in
      El.ArrAccE { expr_base; expr_idx } $ at
  | BitStringAccess { bits; lo; hi; tags = at } ->
      let expr_base = transform_expr bits in
      let expr_lo = transform_expr lo in
      let expr_hi = transform_expr hi in
      El.BitAccE { expr_base; expr_lo; expr_hi } $ at
  | ErrorMember { err; tags = at } ->
      let member = transform_member err in
      El.ErrAccE { member } $ at
  | TypeMember { typ; name; tags = at } ->
      let var_base = transform_var typ in
      let member = transform_member name in
      El.TypeAccE { var_base; member } $ at
  | ExpressionMember { expr; name; tags = at } ->
      let expr_base = transform_expr expr in
      let member = transform_member name in
      El.ExprAccE { expr_base; member } $ at
  | FunctionCall { func; type_args; args; tags = at } -> (
      let targs = transform_targs type_args in
      let args = transform_args args in
      match func with
      | Name { name; _ } ->
          let var_func = transform_var name in
          El.CallFuncE { var_func; targs; args } $ at
      | TypeMember { typ; name; _ } ->
          let var_typ = transform_var typ in
          let member = transform_member name in
          El.CallTypeE { var_typ; member; targs; args } $ at
      | ExpressionMember { expr; name; _ } ->
          let expr_base = transform_expr expr in
          let member = transform_member name in
          El.CallMethodE { expr_base; member; targs; args } $ at
      | _ -> assert false)
  | NamelessInstantiation { typ; args; tags = at } ->
      let typ = transform_type typ in
      let var_inst, targs =
        match (typ.it : El.typ') with
        | NameT var_inst -> (var_inst, [])
        | SpecT (var_inst, targs) -> (var_inst, targs)
        | _ -> assert false
      in
      let args = transform_args args in
      El.InstE { var_inst; targs; args } $ at

and transform_exprs (exprs : Expression.t list) : El.expr list =
  List.map transform_expr exprs

(* Keyset expressions *)

and transform_keyset (mtch : Match.t) : El.keyset =
  match mtch with
  | Default { tags = at } -> L.DefaultK $ at
  | DontCare { tags = at } -> L.AnyK $ at
  | Expression { expr; tags = at } -> L.ExprK (transform_expr expr) $ at

and transform_keysets (mtchs : Match.t list) : El.keyset list =
  List.map transform_keyset mtchs

(* Statements *)

and transform_stmt (stmt : Statement.t) : El.stmt =
  match stmt with
  | EmptyStatement { tags = at } -> El.EmptyS $ at
  | Assignment { lhs; rhs; tags = at } ->
      let expr_l = transform_expr lhs in
      let expr_r = transform_expr rhs in
      El.AssignS { expr_l; expr_r } $ at
  | Switch { expr; cases; tags = at } ->
      let expr_switch = transform_expr expr in
      let cases = transform_switch_cases cases in
      El.SwitchS { expr_switch; cases } $ at
  | Conditional { cond; tru; fls; tags = at } ->
      let expr_cond = transform_expr cond in
      let stmt_then = transform_stmt tru in
      let stmt_else =
        match fls with
        | Some fls -> transform_stmt fls
        | None -> El.EmptyS $ no_info
      in
      El.IfS { expr_cond; stmt_then; stmt_else } $ at
  | BlockStatement { block; tags = at } ->
      let block = transform_block block in
      El.BlockS { block } $ at
  | Exit { tags = at } -> El.ExitS $ at
  | Return { expr; tags = at } ->
      let expr_ret = Option.map transform_expr expr in
      El.RetS { expr_ret } $ at
  | MethodCall { func; type_args; args; tags = at } -> (
      let targs = transform_targs type_args in
      let args = transform_args args in
      match func with
      | Name { name; _ } ->
          let var_func = transform_var name in
          El.CallFuncS { var_func; targs; args } $ at
      | ExpressionMember { expr; name; _ } ->
          let expr_base = transform_expr expr in
          let member = transform_member name in
          El.CallMethodS { expr_base; member; targs; args } $ at
      | _ -> assert false)
  | DirectApplication { typ; args; tags = at } ->
      let var_inst, targs =
        match typ with
        | TypeName { name; _ } -> (transform_var name, [])
        | SpecializedType { base = TypeName { name; _ }; args; _ } ->
            let var_base = transform_var name in
            (var_base, transform_targs args)
        | _ -> assert false
      in
      let args = transform_args args in
      El.CallInstS { var_inst; targs; args } $ at
  | DeclarationStatement { decl; tags = at } ->
      let decl = transform_decl decl in
      El.DeclS { decl } $ at

and transform_stmts (stmts : Statement.t list) : El.stmt list =
  List.map transform_stmt stmts

(* Blocks (sequence of statements *)

and transform_block (block : Block.t) : El.block =
  let at = block.tags in
  let stmts = transform_stmts block.statements in
  let annos = transform_annos block.annotations in
  (stmts, annos) $ at

(* Match-cases for switch *)

and transform_switch_label (label : Statement.switch_label) : El.switch_label =
  match label with
  | Expression { expr; tags = at } -> L.ExprL (transform_expr expr) $ at
  | Default { tags = at } -> L.DefaultL $ at

and transform_switch_case (case : Statement.switch_case) : El.switch_case =
  match case with
  | Action { label; code; tags = at } ->
      let switch_label = transform_switch_label label in
      let block = transform_block code in
      L.MatchC (switch_label, block) $ at
  | FallThrough { label; tags = at } ->
      let switch_label = transform_switch_label label in
      L.FallC switch_label $ at

and transform_switch_cases (cases : Statement.switch_case list) :
    El.switch_case list =
  List.map transform_switch_case cases

(* Select-cases for select *)

(* Declarations *)

and transform_decl (decl : Declaration.t) : El.decl =
  match decl with
  | Constant { name; typ; value; tags = at; annotations } ->
      let id = transform_id name in
      let typ = transform_type typ in
      let value = transform_expr value in
      let annos = transform_annos annotations in
      El.ConstD { id; typ; value; annos } $ at
  | Variable { name; typ; init; tags = at; annotations } ->
      let id = transform_id name in
      let typ = transform_type typ in
      let init = Option.map transform_expr init in
      let annos = transform_annos annotations in
      El.VarD { id; typ; init; annos } $ at
  | Instantiation { name; typ; args; init; tags = at; annotations } ->
      let id = transform_id name in
      let typ = transform_type typ in
      let var_inst, targs =
        match (typ.it : El.typ') with
        | NameT var_inst -> (var_inst, [])
        | SpecT (var_inst, targs) -> (var_inst, targs)
        | _ -> assert false
      in
      let args = transform_args args in
      let init = transform_decls init in
      let annos = transform_annos annotations in
      El.InstD { id; var_inst; targs; args; init; annos } $ at
  | Error { members; tags = at } ->
      let members = transform_members members in
      El.ErrD { members } $ at
  | MatchKind { members; tags = at } ->
      let members = transform_members members in
      El.MatchKindD { members } $ at
  | Struct { name; type_params; fields; tags = at; annotations } ->
      let id = transform_id name in
      let tparams = transform_tparams type_params in
      let fields = transform_record_fields fields in
      let annos = transform_annos annotations in
      El.StructD { id; tparams; fields; annos } $ at
  | Header { name; type_params; fields; tags = at; annotations } ->
      let id = transform_id name in
      let tparams = transform_tparams type_params in
      let fields = transform_record_fields fields in
      let annos = transform_annos annotations in
      El.HeaderD { id; tparams; fields; annos } $ at
  | HeaderUnion { name; type_params; fields; tags = at; annotations } ->
      let id = transform_id name in
      let tparams = transform_tparams type_params in
      let fields = transform_record_fields fields in
      let annos = transform_annos annotations in
      El.UnionD { id; tparams; fields; annos } $ at
  | Enum { name; members; tags = at; annotations } ->
      let id = transform_id name in
      let members = transform_members members in
      let annos = transform_annos annotations in
      El.EnumD { id; members; annos } $ at
  | SerializableEnum { name; typ; members = fields; tags = at; annotations } ->
      let id = transform_id name in
      let typ = transform_type typ in
      let fields = transform_serial_fields fields in
      let annos = transform_annos annotations in
      El.SEnumD { id; typ; fields; annos } $ at
  | NewType { name; typ_or_decl; tags = at; annotations } ->
      let id = transform_id name in
      let typdef =
        match typ_or_decl with
        | Left typ -> (Left (transform_type typ) : (El.typ, El.decl) L.alt)
        | Right decl -> (Right (transform_decl decl) : (El.typ, El.decl) L.alt)
      in
      let annos = transform_annos annotations in
      El.NewTypeD { id; typdef; annos } $ at
  | TypeDef { name; typ_or_decl; tags = at; annotations } ->
      let id = transform_id name in
      let typdef =
        match typ_or_decl with
        | Left typ -> (Left (transform_type typ) : (El.typ, El.decl) L.alt)
        | Right decl -> (Right (transform_decl decl) : (El.typ, El.decl) L.alt)
      in
      let annos = transform_annos annotations in
      El.TypeDefD { id; typdef; annos } $ at
  | ValueSet { name; typ; size; tags = at; annotations } ->
      let id = transform_id name in
      let typ = transform_type typ in
      let size = transform_expr size in
      let annos = transform_annos annotations in
      El.ValueSetD { id; typ; size; annos } $ at
  | ParserType { name; type_params; params; tags = at; annotations } ->
      let id = transform_id name in
      let tparams = transform_tparams type_params in
      let params = transform_params params in
      let annos = transform_annos annotations in
      El.ParserTypeD { id; tparams; params; annos } $ at
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
      let id = transform_id name in
      let tparams = transform_tparams type_params in
      let params = transform_params params in
      let cparams = transform_cparams constructor_params in
      let locals = transform_decls locals in
      let states = transform_parser_states states in
      let annos = transform_annos annotations in
      El.ParserD { id; tparams; params; cparams; locals; states; annos } $ at
  | Action { name; params; body; tags = at; annotations } ->
      let id = transform_id name in
      let params = transform_params params in
      let body = transform_block body in
      let annos = transform_annos annotations in
      El.ActionD { id; params; body; annos } $ at
  | Table { name; properties; tags = at; annotations } ->
      let id = transform_id name in
      let table = transform_table_properties properties in
      let annos = transform_annos annotations in
      El.TableD { id; table; annos } $ at
  | ControlType { name; type_params; params; tags = at; annotations } ->
      let id = transform_id name in
      let tparams = transform_tparams type_params in
      let params = transform_params params in
      let annos = transform_annos annotations in
      El.ControlTypeD { id; tparams; params; annos } $ at
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
      let id = transform_id name in
      let tparams = transform_tparams type_params in
      let params = transform_params params in
      let cparams = transform_cparams constructor_params in
      let locals = transform_decls locals in
      let body = transform_block apply in
      let annos = transform_annos annotations in
      El.ControlD { id; tparams; params; cparams; locals; body; annos } $ at
  | Function
      {
        name;
        annotations = _annotations;
        return;
        type_params;
        params;
        body;
        tags = at;
      } ->
      let id = transform_id name in
      let typ_ret = transform_type return in
      let tparams = transform_tparams type_params in
      let params = transform_params params in
      let body = transform_block body in
      El.FuncD { id; typ_ret; tparams; params; body } $ at
  | ExternFunction { name; return; type_params; params; tags = at; annotations }
    ->
      let id = transform_id name in
      let typ_ret = transform_type return in
      let tparams = transform_tparams type_params in
      let params = transform_params params in
      let annos = transform_annos annotations in
      El.ExternFuncD { id; typ_ret; tparams; params; annos } $ at
  | ExternObject { name; type_params; methods; tags = at; annotations } ->
      let id = transform_id name in
      let tparams = transform_tparams type_params in
      let mthds = transform_methods methods in
      let annos = transform_annos annotations in
      El.ExternObjectD { id; tparams; mthds; annos } $ at
  | PackageType { name; type_params; params; tags = at; annotations } ->
      let id = transform_id name in
      let tparams = transform_tparams type_params in
      let cparams = transform_params params in
      let annos = transform_annos annotations in
      El.PackageTypeD { id; tparams; cparams; annos } $ at

and transform_decls (decls : Declaration.t list) : El.decl list =
  List.map transform_decl decls

and transform_record_fields (fields : Declaration.field list) :
    (El.member * El.typ * El.anno list) list =
  List.map
    (fun (field : Declaration.field) ->
      let Declaration.{ name; typ; annotations; _ } = field in
      (transform_member name, transform_type typ, transform_annos annotations))
    fields

and transform_serial_fields (fields : (Text.t * Expression.t) list) :
    (El.member * El.expr) list =
  List.map
    (fun (field : Text.t * Expression.t) ->
      let field, value = field in
      (transform_member field, transform_expr value))
    fields

and transform_method (mthd : MethodPrototype.t) : El.mthd =
  match mthd with
  | Constructor { name; params; tags = at; annotations } ->
      let id = transform_id name in
      let cparams = transform_params params in
      let annos = transform_annos annotations in
      El.ExternConsM { id; cparams; annos } $ at
  | AbstractMethod { name; return; type_params; params; tags = at; annotations }
    ->
      let id = transform_id name in
      let typ_ret = transform_type return in
      let tparams = transform_tparams type_params in
      let params = transform_params params in
      let annos = transform_annos annotations in
      El.ExternAbstractM { id; typ_ret; tparams; params; annos } $ at
  | Method { name; return; type_params; params; tags = at; annotations } ->
      let id = transform_id name in
      let typ_ret = transform_type return in
      let tparams = transform_tparams type_params in
      let params = transform_params params in
      let annos = transform_annos annotations in
      El.ExternM { id; typ_ret; tparams; params; annos } $ at

and transform_methods (mthds : MethodPrototype.t list) : El.mthd list =
  List.map transform_method mthds

(* Parser state machine *)

and transform_parser_case (case : Parser.case) : El.select_case =
  let Parser.{ matches; next; tags = at } = case in
  let keysets = transform_keysets matches in
  let state_label = transform_state_label next in
  (keysets, state_label) $ at

and transform_parser_cases (cases : Parser.case list) : El.select_case list =
  List.map transform_parser_case cases

and transform_parser_transition (trans : Parser.transition) : El.stmt =
  let expr_label =
    match trans with
    | Direct { next; tags = at } ->
        El.VarE { var = L.Current (transform_id next) $ no_info } $ at
    | Select { exprs; cases; tags = at } ->
        let exprs_select = transform_exprs exprs in
        let cases = transform_parser_cases cases in
        El.SelectE { exprs_select; cases } $ at
  in
  El.TransS { expr_label } $ expr_label.at

and transform_parser_state (state : Parser.state) : El.parser_state =
  let Parser.{ name; statements; transition; tags = at; annotations } = state in
  let state_label = transform_state_label name in
  let stmts =
    transform_stmts statements @ [ transform_parser_transition transition ]
  in
  let block = (stmts, []) $ no_info in
  let annos = transform_annos annotations in
  (state_label, block, annos) $ at

and transform_parser_states (states : Parser.state list) : El.parser_state list
    =
  List.map transform_parser_state states

(* Tables *)

and transform_table_property (property : Table.property) : El.table_property =
  match property with
  | Key { keys; tags = at } -> L.KeyP (transform_table_keys keys $ at)
  | Actions { actions; tags = at } ->
      L.ActionP (transform_table_actions actions $ at)
  | Entries { entries; const; tags = at; _ } ->
      L.EntryP (transform_table_entries entries const $ at)
  | DefaultAction { action; const; tags = at } ->
      L.DefaultP (transform_table_default action const $ at)
  | Custom { name; value; const; annotations; tags = at } ->
      L.CustomP (transform_table_custom name value const annotations $ at)

and transform_table_properties (properties : Table.property list) :
    El.table_property list =
  List.map transform_table_property properties

(* Table keys *)

and transform_table_keys (keys : Table.key list) : El.table_keys' =
  List.map
    (fun (key : Table.key) ->
      let Table.{ key; match_kind; tags = at; annotations } = key in
      ( transform_expr key,
        transform_match_kind match_kind,
        transform_annos annotations )
      $ at)
    keys

(* Table action references *)

and transform_table_action (action : Table.action_ref) : El.table_action =
  let Table.{ name; args; tags = at; annotations } = action in
  let var = transform_var name in
  let args = transform_args args in
  let annos = transform_annos annotations in
  (var, args, annos) $ at

and transform_table_actions (actions : Table.action_ref list) :
    El.table_actions' =
  List.map transform_table_action actions

(* Table entries *)
and transform_table_entry (entry : Table.entry) : El.table_entry =
  let Table.{ matches; action; tags = at; annotations; priority; const } =
    entry
  in
  let keysets = transform_keysets matches in
  let table_action = transform_table_action action in
  let table_entry_priority = Option.map transform_expr priority in
  let annos = transform_annos annotations in
  (const, keysets, table_action, table_entry_priority, annos) $ at

and transform_table_entries (entries : Table.entry list) (const : bool) :
    El.table_entries' =
  let table_entries = List.map transform_table_entry entries in
  (const, table_entries)

(* Table default properties *)

and transform_table_default (action : Table.action_ref) (const : bool) :
    El.table_default' =
  let table_action = transform_table_action action in
  (const, table_action)

(* Table custom properties *)

and transform_table_custom (name : Text.t) (value : Expression.t) (const : bool)
    (annotations : Annotation.t list) : El.table_custom' =
  let text_name = transform_text name in
  let expr = transform_expr value in
  let annos = transform_annos annotations in
  (const, text_name, expr, annos)

(* Program declarations *)

(* Program *)

let transform_program (program : p4program) : El.program =
  let (Program decls) = program in
  let program = transform_decls decls in
  program
