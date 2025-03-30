open Xl.Atom
module P4 = P4el.Ast
open Il.Ast
open Util.Source

(* Helpers *)

let in_opt (do_in : 'a -> value) (opt : 'a option) : value =
  let vopt = Option.map do_in opt in
  OptV vopt

let in_list (do_in : 'a -> value) (lst : 'a list) : value =
  let vlst = List.map do_in lst in
  ListV vlst

let in_pair (do_in_a : 'a -> value) (do_in_b : 'b -> value) ((a, b) : 'a * 'b) :
    value =
  TupleV [ do_in_a a; do_in_b b ]

let atom (s : string) : atom = Atom s $ no_region

(* Booleans *)

let in_bool (boolean : bool) : value = BoolV boolean

(* Numbers *)

let in_num (num : P4.num) : value =
  match num.it with
  | i, Some (width, signed) ->
      let mixop =
        if signed then [ [ atom "FINT" ]; []; [] ]
        else [ [ atom "FBIT" ]; []; [] ]
      in
      let vwidth = NumV (`Nat width) in
      let vint = NumV (`Int i) in
      CaseV (mixop, [ vwidth; vint ])
  | i, None ->
      let mixop = [ [ atom "INT" ]; [] ] in
      let vint = NumV (`Int i) in
      CaseV (mixop, [ vint ])

(* Texts *)

let in_text (text : P4.text) : value = TextV text.it

(* Identifiers *)

let in_id (id : P4.id) : value = TextV id.it

(* Variables (scoped identifiers) *)

let in_var (var : P4.var) : value =
  match var.it with
  | Top id ->
      let mixop = [ [ atom "TOP" ]; [] ] in
      let vid = in_id id in
      CaseV (mixop, [ vid ])
  | Current id ->
      let mixop = [ [ atom "CURRENT" ]; [] ] in
      let vid = in_id id in
      CaseV (mixop, [ vid ])

(* Members *)

let rec in_member (member : P4.member) : value = TextV member.it
and in_members (members : P4.member list) : value = in_list in_member members

(* Match kinds *)

let in_match_kind (match_kind : P4.match_kind) : value = TextV match_kind.it

(* State labels *)

let in_state_label (state_label : P4.state_label) : value = TextV state_label.it

(* Unary operators *)

let in_unop (unop : P4.unop) : value =
  match unop.it with
  | BNotOp ->
      let mixop = [ [ atom "BNOT" ] ] in
      CaseV (mixop, [])
  | LNotOp ->
      let mixop = [ [ atom "LNOT" ] ] in
      CaseV (mixop, [])
  | UPlusOp ->
      let mixop = [ [ atom "UPLUS" ] ] in
      CaseV (mixop, [])
  | UMinusOp ->
      let mixop = [ [ atom "UMINUS" ] ] in
      CaseV (mixop, [])

(* Binary operators *)

let in_binop (binop : P4.binop) : value =
  match binop.it with
  | PlusOp ->
      let mixop = [ [ atom "PLUS" ] ] in
      CaseV (mixop, [])
  | SPlusOp ->
      let mixop = [ [ atom "SPLUS" ] ] in
      CaseV (mixop, [])
  | MinusOp ->
      let mixop = [ [ atom "MINUS" ] ] in
      CaseV (mixop, [])
  | SMinusOp ->
      let mixop = [ [ atom "SMINUS" ] ] in
      CaseV (mixop, [])
  | MulOp ->
      let mixop = [ [ atom "MUL" ] ] in
      CaseV (mixop, [])
  | DivOp ->
      let mixop = [ [ atom "DIV" ] ] in
      CaseV (mixop, [])
  | ModOp ->
      let mixop = [ [ atom "MOD" ] ] in
      CaseV (mixop, [])
  | ShlOp ->
      let mixop = [ [ atom "SHL" ] ] in
      CaseV (mixop, [])
  | ShrOp ->
      let mixop = [ [ atom "SHR" ] ] in
      CaseV (mixop, [])
  | LeOp ->
      let mixop = [ [ atom "LE" ] ] in
      CaseV (mixop, [])
  | GeOp ->
      let mixop = [ [ atom "GE" ] ] in
      CaseV (mixop, [])
  | LtOp ->
      let mixop = [ [ atom "LT" ] ] in
      CaseV (mixop, [])
  | GtOp ->
      let mixop = [ [ atom "GT" ] ] in
      CaseV (mixop, [])
  | EqOp ->
      let mixop = [ [ atom "EQ" ] ] in
      CaseV (mixop, [])
  | NeOp ->
      let mixop = [ [ atom "NE" ] ] in
      CaseV (mixop, [])
  | BAndOp ->
      let mixop = [ [ atom "BAND" ] ] in
      CaseV (mixop, [])
  | BXorOp ->
      let mixop = [ [ atom "BXOR" ] ] in
      CaseV (mixop, [])
  | BOrOp ->
      let mixop = [ [ atom "BOR" ] ] in
      CaseV (mixop, [])
  | ConcatOp ->
      let mixop = [ [ atom "CONCAT" ] ] in
      CaseV (mixop, [])
  | LAndOp ->
      let mixop = [ [ atom "LAND" ] ] in
      CaseV (mixop, [])
  | LOrOp ->
      let mixop = [ [ atom "LOR" ] ] in
      CaseV (mixop, [])

(* Directions *)

let in_dir (dir : P4.dir) : value =
  match dir.it with
  | No ->
      let mixop = [ [ atom "NO" ] ] in
      CaseV (mixop, [])
  | In ->
      let mixop = [ [ atom "IN" ] ] in
      CaseV (mixop, [])
  | Out ->
      let mixop = [ [ atom "OUT" ] ] in
      CaseV (mixop, [])
  | InOut ->
      let mixop = [ [ atom "INOUT" ] ] in
      CaseV (mixop, [])

(* Types *)

let rec in_typ (typ : P4.typ) : value =
  match typ.it with
  | VoidT ->
      let mixop = [ [ atom "VoidT" ] ] in
      CaseV (mixop, [])
  | ErrT ->
      let mixop = [ [ atom "ErrT" ] ] in
      CaseV (mixop, [])
  | MatchKindT ->
      let mixop = [ [ atom "MatchKindT" ] ] in
      CaseV (mixop, [])
  | StrT ->
      let mixop = [ [ atom "StrT" ] ] in
      CaseV (mixop, [])
  | BoolT ->
      let mixop = [ [ atom "BoolT" ] ] in
      CaseV (mixop, [])
  | IntT ->
      let mixop = [ [ atom "IntT" ] ] in
      CaseV (mixop, [])
  | FIntT expr ->
      let mixop = [ [ atom "FIntT" ]; [] ] in
      let vexpr = in_expr expr in
      CaseV (mixop, [ vexpr ])
  | FBitT expr ->
      let mixop = [ [ atom "FBitT" ]; [] ] in
      let vexpr = in_expr expr in
      CaseV (mixop, [ vexpr ])
  | VBitT expr ->
      let mixop = [ [ atom "VBitT" ]; [] ] in
      let vexpr = in_expr expr in
      CaseV (mixop, [ vexpr ])
  | StackT (typ, expr) ->
      let mixop = [ [ atom "StackT" ]; []; [] ] in
      let vtyp = in_typ typ in
      let vexpr = in_expr expr in
      CaseV (mixop, [ vtyp; vexpr ])
  | ListT typ ->
      let mixop = [ [ atom "ListT" ]; [] ] in
      let vtyp = in_typ typ in
      CaseV (mixop, [ vtyp ])
  | TupleT typs ->
      let mixop = [ [ atom "TupleT" ]; [] ] in
      let vtyps = in_typs typs in
      CaseV (mixop, [ vtyps ])
  | NameT var ->
      let mixop = [ [ atom "NameT" ]; [] ] in
      let vvar = in_var var in
      CaseV (mixop, [ vvar ])
  | SpecT (var, targs) ->
      let mixop = [ [ atom "SpecT" ]; []; [] ] in
      let vvar = in_var var in
      let vtargs = in_targs targs in
      CaseV (mixop, [ vvar; vtargs ])
  | AnyT ->
      let mixop = [ [ atom "AnyT" ] ] in
      CaseV (mixop, [])

and in_typs (typs : P4.typ list) : value = in_list in_typ typs

(* Type parameters *)

and in_tparam (tparam : P4.tparam) : value = TextV tparam.it
and in_tparams (tparams : P4.tparam list) : value = in_list in_tparam tparams

(* Parameters *)

and in_param (param : P4.param) : value =
  let id, dir, typ, expr_opt, _ = param.it in
  let mixop = [ []; []; []; []; [] ] in
  let vid = in_id id in
  let vdir = in_dir dir in
  let vtyp = in_typ typ in
  let vexpr_opt = in_opt in_expr expr_opt in
  CaseV (mixop, [ vid; vdir; vtyp; vexpr_opt ])

and in_params (params : P4.param list) : value = in_list in_param params

(* Constructor parameters *)

and in_cparam (cparam : P4.cparam) : value = in_param cparam
and in_cparams (cparams : P4.cparam list) : value = in_list in_cparam cparams

(* Type arguments *)

and in_targ (targ : P4.targ) : value = in_typ targ
and in_targs (targs : P4.targ list) : value = in_list in_targ targs

(* Arguments *)

and in_arg (arg : P4.arg) : value =
  match arg.it with
  | ExprA expr ->
      let mixop = [ [ atom "ExprA" ]; [] ] in
      let vexpr = in_expr expr in
      CaseV (mixop, [ vexpr ])
  | NameA (id, expr_opt) ->
      let mixop = [ [ atom "NameA" ]; []; [] ] in
      let vid = in_id id in
      let vexpr_opt = in_opt in_expr expr_opt in
      CaseV (mixop, [ vid; vexpr_opt ])
  | AnyA ->
      let mixop = [ [ atom "AnyA" ] ] in
      CaseV (mixop, [])

and in_args (args : P4.arg list) : value = in_list in_arg args

(* Expressions *)

and in_expr (expr : P4.expr) : value =
  match expr.it with
  | BoolE { boolean } ->
      let mixop = [ [ atom "BoolE" ]; [] ] in
      let vboolean = in_bool boolean in
      CaseV (mixop, [ vboolean ])
  | StrE { text } ->
      let mixop = [ [ atom "StrE" ]; [] ] in
      let vtext = in_text text in
      CaseV (mixop, [ vtext ])
  | NumE { num } ->
      let mixop = [ [ atom "NumE" ]; [] ] in
      let vnum = in_num num in
      CaseV (mixop, [ vnum ])
  | VarE { var } ->
      let mixop = [ [ atom "NameE" ]; [] ] in
      let vvar = in_var var in
      CaseV (mixop, [ vvar ])
  | SeqE { exprs } ->
      let mixop = [ [ atom "SeqE" ]; [] ] in
      let vexprs = in_exprs exprs in
      CaseV (mixop, [ vexprs ])
  | SeqDefaultE { exprs } ->
      let mixop = [ [ atom "SeqDefaultE" ]; [] ] in
      let vexprs = in_exprs exprs in
      CaseV (mixop, [ vexprs ])
  | RecordE { fields } ->
      let mixop = [ [ atom "RecordE" ]; [] ] in
      let vfields = in_list (in_pair in_member in_expr) fields in
      CaseV (mixop, [ vfields ])
  | RecordDefaultE { fields } ->
      let mixop = [ [ atom "RecordDefaultE" ]; [] ] in
      let vfields = in_list (in_pair in_member in_expr) fields in
      CaseV (mixop, [ vfields ])
  | DefaultE ->
      let mixop = [ [ atom "DefaultE" ] ] in
      CaseV (mixop, [])
  | InvalidE ->
      let mixop = [ [ atom "InvalidE" ] ] in
      CaseV (mixop, [])
  | UnE { unop; expr } ->
      let mixop = [ [ atom "UnE" ]; []; [] ] in
      let vunop = in_unop unop in
      let vexpr = in_expr expr in
      CaseV (mixop, [ vunop; vexpr ])
  | BinE { binop; expr_l; expr_r } ->
      let mixop = [ [ atom "BinE" ]; []; []; [] ] in
      let vbinop = in_binop binop in
      let vexpr_l = in_expr expr_l in
      let vexpr_r = in_expr expr_r in
      CaseV (mixop, [ vbinop; vexpr_l; vexpr_r ])
  | TernE { expr_cond; expr_then; expr_else } ->
      let mixop = [ [ atom "TernE" ]; []; []; [] ] in
      let vexpr_cond = in_expr expr_cond in
      let vexpr_then = in_expr expr_then in
      let vexpr_else = in_expr expr_else in
      CaseV (mixop, [ vexpr_cond; vexpr_then; vexpr_else ])
  | CastE { typ; expr } ->
      let mixop = [ [ atom "CastE" ]; []; [] ] in
      let vtyp = in_typ typ in
      let vexpr = in_expr expr in
      CaseV (mixop, [ vtyp; vexpr ])
  | MaskE { expr_base; expr_mask } ->
      let mixop = [ [ atom "MaskE" ]; []; [] ] in
      let vexpr_base = in_expr expr_base in
      let vexpr_mask = in_expr expr_mask in
      CaseV (mixop, [ vexpr_base; vexpr_mask ])
  | RangeE { expr_lb; expr_ub } ->
      let mixop = [ [ atom "RangeE" ]; []; [] ] in
      let vexpr_lb = in_expr expr_lb in
      let vexpr_ub = in_expr expr_ub in
      CaseV (mixop, [ vexpr_lb; vexpr_ub ])
  | SelectE { exprs_select; cases } ->
      let mixop = [ [ atom "SelectE" ]; []; [] ] in
      let vexprs_select = in_exprs exprs_select in
      let vcases = in_select_cases cases in
      CaseV (mixop, [ vexprs_select; vcases ])
  | ArrAccE { expr_base; expr_idx } ->
      let mixop = [ [ atom "ArrAccE" ]; []; [] ] in
      let vexpr_base = in_expr expr_base in
      let vexpr_idx = in_expr expr_idx in
      CaseV (mixop, [ vexpr_base; vexpr_idx ])
  | BitAccE { expr_base; expr_lo; expr_hi } ->
      let mixop = [ [ atom "BitAccE" ]; []; []; [] ] in
      let vexpr_base = in_expr expr_base in
      let vexpr_lo = in_expr expr_lo in
      let vexpr_hi = in_expr expr_hi in
      CaseV (mixop, [ vexpr_base; vexpr_lo; vexpr_hi ])
  | ErrAccE { member } ->
      let mixop = [ [ atom "ErrAccE" ]; [] ] in
      let vmember = in_member member in
      CaseV (mixop, [ vmember ])
  | TypeAccE { var_base; member } ->
      let mixop = [ [ atom "TypeAccE" ]; []; [] ] in
      let vvar_base = in_var var_base in
      let vmember = in_member member in
      CaseV (mixop, [ vvar_base; vmember ])
  | ExprAccE { expr_base; member } ->
      let mixop = [ [ atom "ExprAccE" ]; []; [] ] in
      let vexpr_base = in_expr expr_base in
      let vmember = in_member member in
      CaseV (mixop, [ vexpr_base; vmember ])
  | CallFuncE { var_func; targs; args } ->
      let mixop = [ [ atom "CallFuncE" ]; []; []; [] ] in
      let vvar_func = in_var var_func in
      let vtargs = in_targs targs in
      let vargs = in_args args in
      CaseV (mixop, [ vvar_func; vtargs; vargs ])
  | CallMethodE { expr_base; member; targs; args } ->
      let mixop = [ [ atom "CallMethodE" ]; []; []; []; [] ] in
      let vexpr_base = in_expr expr_base in
      let vmember = in_member member in
      let vtargs = in_targs targs in
      let vargs = in_args args in
      CaseV (mixop, [ vexpr_base; vmember; vtargs; vargs ])
  | CallTypeE { var_typ; member; targs; args } ->
      let mixop = [ [ atom "CallTypeE" ]; []; []; []; [] ] in
      let vvar_typ = in_var var_typ in
      let vmember = in_member member in
      let vtargs = in_targs targs in
      let vargs = in_args args in
      CaseV (mixop, [ vvar_typ; vmember; vtargs; vargs ])
  | InstE { var_inst; targs; args } ->
      let mixop = [ [ atom "InstE" ]; []; []; [] ] in
      let vvar_inst = in_var var_inst in
      let vtargs = in_targs targs in
      let vargs = in_args args in
      CaseV (mixop, [ vvar_inst; vtargs; vargs ])

and in_exprs (exprs : P4.expr list) : value = in_list in_expr exprs

(* Keyset expressions *)

and in_keyset (keyset : P4.keyset) : value =
  match keyset.it with
  | ExprK expr ->
      let mixop = [ [ atom "ExprK" ]; [] ] in
      let vexpr = in_expr expr in
      CaseV (mixop, [ vexpr ])
  | DefaultK ->
      let mixop = [ [ atom "DefaultK" ] ] in
      CaseV (mixop, [])
  | AnyK ->
      let mixop = [ [ atom "AnyK" ] ] in
      CaseV (mixop, [])

and in_keysets (keysets : P4.keyset list) : value = in_list in_keyset keysets

(* Select-cases for select *)

and in_select_case (select_case : P4.select_case) : value =
  let keysets, state_label = select_case.it in
  let mixop = [ []; []; [] ] in
  let vkeysets = in_keysets keysets in
  let vstate_label = in_state_label state_label in
  CaseV (mixop, [ vkeysets; vstate_label ])

and in_select_cases (select_cases : P4.select_case list) : value =
  in_list in_select_case select_cases

(* Statements *)

and in_stmt (stmt : P4.stmt) : value =
  match stmt.it with
  | EmptyS ->
      let mixop = [ [ atom "EmptyS" ] ] in
      CaseV (mixop, [])
  | AssignS { expr_l; expr_r } ->
      let mixop = [ [ atom "AssignS" ]; []; [] ] in
      let vexpr_l = in_expr expr_l in
      let vexpr_r = in_expr expr_r in
      CaseV (mixop, [ vexpr_l; vexpr_r ])
  | SwitchS { expr_switch; cases } ->
      let mixop = [ [ atom "SwitchS" ]; []; [] ] in
      let vexpr_switch = in_expr expr_switch in
      let vcases = in_switch_cases cases in
      CaseV (mixop, [ vexpr_switch; vcases ])
  | IfS { expr_cond; stmt_then; stmt_else } ->
      let mixop = [ [ atom "IfS" ]; []; []; [] ] in
      let vexpr_cond = in_expr expr_cond in
      let vstmt_then = in_stmt stmt_then in
      let vstmt_else = in_stmt stmt_else in
      CaseV (mixop, [ vexpr_cond; vstmt_then; vstmt_else ])
  | BlockS { block } ->
      let mixop = [ [ atom "BlockS" ]; [] ] in
      let vblock = in_block block in
      CaseV (mixop, [ vblock ])
  | ExitS ->
      let mixop = [ [ atom "ExitS" ] ] in
      CaseV (mixop, [])
  | RetS { expr_ret } ->
      let mixop = [ [ atom "RetS" ]; [] ] in
      let vexpr_ret = in_opt in_expr expr_ret in
      CaseV (mixop, [ vexpr_ret ])
  | CallFuncS { var_func; targs; args } ->
      let mixop = [ [ atom "CallFuncS" ]; []; []; [] ] in
      let vvar_func = in_var var_func in
      let vtargs = in_targs targs in
      let vargs = in_args args in
      CaseV (mixop, [ vvar_func; vtargs; vargs ])
  | CallMethodS { expr_base; member; targs; args } ->
      let mixop = [ [ atom "CallMethodS" ]; []; []; []; [] ] in
      let vexpr_base = in_expr expr_base in
      let vmember = in_member member in
      let vtargs = in_targs targs in
      let vargs = in_args args in
      CaseV (mixop, [ vexpr_base; vmember; vtargs; vargs ])
  | CallInstS { var_inst; targs; args } ->
      let mixop = [ [ atom "CallInstS" ]; []; []; [] ] in
      let vvar_inst = in_var var_inst in
      let vtargs = in_targs targs in
      let vargs = in_args args in
      CaseV (mixop, [ vvar_inst; vtargs; vargs ])
  | TransS { expr_label } ->
      let mixop = [ [ atom "TransS" ]; [] ] in
      let vexpr_label = in_expr expr_label in
      CaseV (mixop, [ vexpr_label ])
  | DeclS { decl } ->
      let mixop = [ [ atom "DeclS" ]; [] ] in
      let vdecl = in_decl decl in
      CaseV (mixop, [ vdecl ])

and in_stmts (stmts : P4.stmt list) : value = in_list in_stmt stmts

(* Blocks (sequence of statements) *)

and in_block (block : P4.block) : value =
  let stmts, _ = block.it in
  let mixop = [ [ atom "BlockB" ]; [] ] in
  let vstmts = in_stmts stmts in
  CaseV (mixop, [ vstmts ])

(* Match-cases for switch *)

and in_switch_label (switch_label : P4.switch_label) : value =
  match switch_label.it with
  | ExprL expr ->
      let mixop = [ [ atom "ExprL" ]; [] ] in
      let vexpr = in_expr expr in
      CaseV (mixop, [ vexpr ])
  | DefaultL ->
      let mixop = [ [ atom "DefaultL" ] ] in
      CaseV (mixop, [])

and in_switch_case (switch_case : P4.switch_case) : value =
  match switch_case.it with
  | MatchC (switch_label, block) ->
      let mixop = [ [ atom "MatchC" ]; []; [] ] in
      let vswitch_label = in_switch_label switch_label in
      let vblock = in_block block in
      CaseV (mixop, [ vswitch_label; vblock ])
  | FallC switch_label ->
      let mixop = [ [ atom "FallC" ]; [] ] in
      let vswitch_label = in_switch_label switch_label in
      CaseV (mixop, [ vswitch_label ])

and in_switch_cases (switch_cases : P4.switch_case list) : value =
  in_list in_switch_case switch_cases

(* Declarations *)

and in_typdef (typdef : (P4.typ, P4.decl) P4.alt) : value =
  match typdef with
  | Left typ ->
      let mixop = [ [ atom "TypeD" ]; [] ] in
      let vtyp = in_typ typ in
      CaseV (mixop, [ vtyp ])
  | Right decl ->
      let mixop = [ [ atom "DeclD" ]; [] ] in
      let vdecl = in_decl decl in
      CaseV (mixop, [ vdecl ])

and in_decl (decl : P4.decl) : value =
  match decl.it with
  | ConstD { id; typ; value; _ } ->
      let mixop = [ [ atom "ConstD" ]; []; []; [] ] in
      let vid = in_id id in
      let vtyp = in_typ typ in
      let vvalue = in_expr value in
      CaseV (mixop, [ vid; vtyp; vvalue ])
  | VarD { id; typ; init; _ } ->
      let mixop = [ [ atom "VarD" ]; []; []; [] ] in
      let vid = in_id id in
      let vtyp = in_typ typ in
      let vinit = in_opt in_expr init in
      CaseV (mixop, [ vid; vtyp; vinit ])
  | ErrD { members } ->
      let mixop = [ [ atom "ErrD" ]; [] ] in
      let vmembers = in_members members in
      CaseV (mixop, [ vmembers ])
  | MatchKindD { members } ->
      let mixop = [ [ atom "MatchKindD" ]; [] ] in
      let vmembers = in_members members in
      CaseV (mixop, [ vmembers ])
  | InstD { id; var_inst; targs; args; init; _ } ->
      let mixop = [ [ atom "InstD" ]; []; []; []; []; [] ] in
      let vid = in_id id in
      let vvar_inst = in_var var_inst in
      let vtargs = in_targs targs in
      let vargs = in_args args in
      let vinit = in_decls init in
      CaseV (mixop, [ vid; vvar_inst; vtargs; vargs; vinit ])
  | StructD { id; tparams; fields; _ } ->
      let mixop = [ [ atom "StructD" ]; []; []; [] ] in
      let vid = in_id id in
      let vtparams = in_tparams tparams in
      let vfields =
        let fields = List.map (fun (member, typ, _) -> (member, typ)) fields in
        in_list (in_pair in_member in_typ) fields
      in
      CaseV (mixop, [ vid; vtparams; vfields ])
  | HeaderD { id; tparams; fields; _ } ->
      let mixop = [ [ atom "HeaderD" ]; []; []; [] ] in
      let vid = in_id id in
      let vtparams = in_tparams tparams in
      let vfields =
        let fields = List.map (fun (member, typ, _) -> (member, typ)) fields in
        in_list (in_pair in_member in_typ) fields
      in
      CaseV (mixop, [ vid; vtparams; vfields ])
  | UnionD { id; tparams; fields; _ } ->
      let mixop = [ [ atom "UnionD" ]; []; []; [] ] in
      let vid = in_id id in
      let vtparams = in_tparams tparams in
      let vfields =
        let fields = List.map (fun (member, typ, _) -> (member, typ)) fields in
        in_list (in_pair in_member in_typ) fields
      in
      CaseV (mixop, [ vid; vtparams; vfields ])
  | EnumD { id; members; _ } ->
      let mixop = [ [ atom "EnumD" ]; []; [] ] in
      let vid = in_id id in
      let vmembers = in_members members in
      CaseV (mixop, [ vid; vmembers ])
  | SEnumD { id; typ; fields; _ } ->
      let mixop = [ [ atom "SEnumD" ]; []; []; [] ] in
      let vid = in_id id in
      let vtyp = in_typ typ in
      let vfields = in_list (in_pair in_member in_expr) fields in
      CaseV (mixop, [ vid; vtyp; vfields ])
  | NewTypeD { id; typdef; _ } ->
      let mixop = [ [ atom "NewTypeD" ]; []; [] ] in
      let vid = in_id id in
      let vtypdef = in_typdef typdef in
      CaseV (mixop, [ vid; vtypdef ])
  | TypeDefD { id; typdef; _ } ->
      let mixop = [ [ atom "TypeDefD" ]; []; [] ] in
      let vid = in_id id in
      let vtypdef = in_typdef typdef in
      CaseV (mixop, [ vid; vtypdef ])
  | ValueSetD { id; typ; size; _ } ->
      let mixop = [ [ atom "ValueSetD" ]; []; []; [] ] in
      let vid = in_id id in
      let vtyp = in_typ typ in
      let vsize = in_expr size in
      CaseV (mixop, [ vid; vtyp; vsize ])
  | ParserTypeD { id; tparams; params; _ } ->
      let mixop = [ [ atom "ParserTypeD" ]; []; []; [] ] in
      let vid = in_id id in
      let vtparams = in_tparams tparams in
      let vparams = in_params params in
      CaseV (mixop, [ vid; vtparams; vparams ])
  | ParserD { id; params; cparams; locals; states; _ } ->
      let mixop = [ [ atom "ParserD" ]; []; []; []; []; [] ] in
      let vid = in_id id in
      let vparams = in_params params in
      let vcparams = in_cparams cparams in
      let vlocals = in_decls locals in
      let vstates = in_parser_states states in
      CaseV (mixop, [ vid; vparams; vcparams; vlocals; vstates ])
  | TableD { id; table; _ } ->
      let mixop = [ [ atom "TableD" ]; []; [] ] in
      let vid = in_id id in
      let vtable = in_table table in
      CaseV (mixop, [ vid; vtable ])
  | ControlTypeD { id; tparams; params; _ } ->
      let mixop = [ [ atom "ControlTypeD" ]; []; []; [] ] in
      let vid = in_id id in
      let vtparams = in_tparams tparams in
      let vparams = in_params params in
      CaseV (mixop, [ vid; vtparams; vparams ])
  | ControlD { id; params; cparams; locals; body; _ } ->
      let mixop = [ [ atom "ControlD" ]; []; []; []; []; [] ] in
      let vid = in_id id in
      let vparams = in_params params in
      let vcparams = in_cparams cparams in
      let vlocals = in_decls locals in
      let vbody = in_block body in
      CaseV (mixop, [ vid; vparams; vcparams; vlocals; vbody ])
  | ActionD { id; params; body; _ } ->
      let mixop = [ [ atom "ActionD" ]; []; []; [] ] in
      let vid = in_id id in
      let vparams = in_params params in
      let vbody = in_block body in
      CaseV (mixop, [ vid; vparams; vbody ])
  | FuncD { id; typ_ret; tparams; params; body; _ } ->
      let mixop = [ [ atom "FuncD" ]; []; []; []; []; [] ] in
      let vid = in_id id in
      let vtyp_ret = in_typ typ_ret in
      let vtparams = in_tparams tparams in
      let vparams = in_params params in
      let vbody = in_block body in
      CaseV (mixop, [ vid; vtyp_ret; vtparams; vparams; vbody ])
  | ExternFuncD { id; typ_ret; tparams; params; _ } ->
      let mixop = [ [ atom "ExternFuncD" ]; []; []; []; [] ] in
      let vid = in_id id in
      let vtyp_ret = in_typ typ_ret in
      let vtparams = in_tparams tparams in
      let vparams = in_params params in
      CaseV (mixop, [ vid; vtyp_ret; vtparams; vparams ])
  | ExternObjectD { id; tparams; mthds; _ } ->
      let mixop = [ [ atom "ExternObjectD" ]; []; []; [] ] in
      let vid = in_id id in
      let vtparams = in_tparams tparams in
      let vmthds = in_mthds mthds in
      CaseV (mixop, [ vid; vtparams; vmthds ])
  | PackageTypeD { id; tparams; cparams; _ } ->
      let mixop = [ [ atom "PackageTypeD" ]; []; []; [] ] in
      let vid = in_id id in
      let vtparams = in_tparams tparams in
      let vcparams = in_cparams cparams in
      CaseV (mixop, [ vid; vtparams; vcparams ])

and in_decls (decls : P4.decl list) : value = in_list in_decl decls

(* Parser state machine *)

and in_parser_state (parser_state : P4.parser_state) : value =
  let state_label, block, _ = parser_state.it in
  let mixop = [ []; []; [] ] in
  let vstate_label = in_state_label state_label in
  let vblock = in_block block in
  CaseV (mixop, [ vstate_label; vblock ])

and in_parser_states (parser_states : P4.parser_state list) : value =
  in_list in_parser_state parser_states

(* Tables *)

and in_table (table : P4.table) : value = in_list in_table_property table

(* Table properties *)

and in_table_property (table_property : P4.table_property) : value =
  match table_property with
  | KeyP table_keys ->
      let mixop = [ [ atom "KeyP" ]; [] ] in
      let vtable_keys = in_table_keys table_keys in
      CaseV (mixop, [ vtable_keys ])
  | ActionP table_actions ->
      let mixop = [ [ atom "ActionP" ]; [] ] in
      let vtable_actions = in_table_actions table_actions in
      CaseV (mixop, [ vtable_actions ])
  | EntryP table_entries ->
      let mixop = [ [ atom "EntryP" ]; [] ] in
      let vtable_entries = in_table_entries table_entries in
      CaseV (mixop, [ vtable_entries ])
  | DefaultP table_default ->
      let mixop = [ [ atom "DefaultP" ]; [] ] in
      let vtable_default = in_table_default table_default in
      CaseV (mixop, [ vtable_default ])
  | CustomP table_custom ->
      let mixop = [ [ atom "CustomP" ]; [] ] in
      let vtable_custom = in_table_custom table_custom in
      CaseV (mixop, [ vtable_custom ])

(* Table keys *)

and in_table_key (table_key : P4.table_key) : value =
  let expr, match_kind, _ = table_key.it in
  let mixop = [ []; []; [] ] in
  let vexpr = in_expr expr in
  let vmatch_kind = in_match_kind match_kind in
  CaseV (mixop, [ vexpr; vmatch_kind ])

and in_table_keys (table_keys : P4.table_keys) : value =
  in_list in_table_key table_keys.it

(* Table action references *)

and in_table_action (table_action : P4.table_action) : value =
  let var, args, _ = table_action.it in
  let mixop = [ []; []; [] ] in
  let vvar = in_var var in
  let vargs = in_args args in
  CaseV (mixop, [ vvar; vargs ])

and in_table_actions (table_actions : P4.table_actions) : value =
  in_list in_table_action table_actions.it

(* Table entries *)

and in_table_entry (table_entry : P4.table_entry) : value =
  let table_entry_const, keysets, table_action, expr_opt, _ = table_entry.it in
  let mixop = [ []; []; []; []; [] ] in
  let vtable_entry_const = in_bool table_entry_const in
  let vkeysets = in_keysets keysets in
  let vtable_action = in_table_action table_action in
  let vexpr_opt = in_opt in_expr expr_opt in
  CaseV (mixop, [ vtable_entry_const; vkeysets; vtable_action; vexpr_opt ])

and in_table_entries (table_entries : P4.table_entries) : value =
  let table_entries_const, table_entries = table_entries.it in
  let mixop = [ []; []; [] ] in
  let vtable_entries_const = in_bool table_entries_const in
  let vtable_entries = in_list in_table_entry table_entries in
  CaseV (mixop, [ vtable_entries_const; vtable_entries ])

(* Table default properties *)

and in_table_default (table_default : P4.table_default) : value =
  let table_default_const, table_action = table_default.it in
  let mixop = [ []; []; [] ] in
  let vtable_default_const = in_bool table_default_const in
  let vtable_action = in_table_action table_action in
  CaseV (mixop, [ vtable_default_const; vtable_action ])

(* Table custom properties *)

and in_table_custom (table_custom : P4.table_custom) : value =
  let table_custom_const, member, expr, _ = table_custom.it in
  let mixop = [ []; []; []; [] ] in
  let vtable_custom_const = in_bool table_custom_const in
  let vmember = in_member member in
  let vexpr = in_expr expr in
  CaseV (mixop, [ vtable_custom_const; vmember; vexpr ])

(* Methods *)

and in_mthd (mthd : P4.mthd) : value =
  match mthd.it with
  | ExternConsM { id; cparams; _ } ->
      let mixop = [ [ atom "ExternConsM" ]; []; [] ] in
      let vid = in_id id in
      let vcparams = in_cparams cparams in
      CaseV (mixop, [ vid; vcparams ])
  | ExternAbstractM { id; typ_ret; tparams; params; _ } ->
      let mixop = [ [ atom "ExternAbstractM" ]; []; []; []; [] ] in
      let vid = in_id id in
      let vtyp_ret = in_typ typ_ret in
      let vtparams = in_tparams tparams in
      let vparams = in_params params in
      CaseV (mixop, [ vid; vtyp_ret; vtparams; vparams ])
  | ExternM { id; typ_ret; tparams; params; _ } ->
      let mixop = [ [ atom "ExternM" ]; []; []; []; [] ] in
      let vid = in_id id in
      let vtyp_ret = in_typ typ_ret in
      let vtparams = in_tparams tparams in
      let vparams = in_params params in
      CaseV (mixop, [ vid; vtyp_ret; vtparams; vparams ])

and in_mthds (mthds : P4.mthd list) : value = in_list in_mthd mthds

(* Program *)

let in_program (program : P4.program) : value = in_list in_decl program
