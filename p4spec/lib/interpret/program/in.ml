open Xl.Atom
module Value = Runtime_dynamic.Value

(* module L = P4lang.Ast *)
open P4el.Ast
open P4util.Source

(* Helpers *)

let in_opt (do_in : 'a -> Value.t) (opt : 'a option) : Value.t =
  let vopt = Option.map do_in opt in
  OptV vopt

let in_list (do_in : 'a -> Value.t) (lst : 'a list) : Value.t =
  let vlst = List.map do_in lst in
  ListV vlst

let in_pair (do_in_a : 'a -> Value.t) (do_in_b : 'b -> Value.t)
    ((a, b) : 'a * 'b) : Value.t =
  TupleV [ do_in_a a; do_in_b b ]

(* Booleans *)

let in_bool (boolean : bool) : Value.t = BoolV boolean

(* Numbers *)

let in_num (num : num) : Value.t =
  match num.it with
  | i, Some (width, signed) ->
      let mixop =
        if signed then [ [ Atom "FINT" ]; []; [] ]
        else [ [ Atom "FBIT" ]; []; [] ]
      in
      let vi = Value.NumV (`Int (i |> Bigint.to_int_exn |> Z.of_int)) in
      let vwidth = Value.NumV (`Nat (width |> Bigint.to_int_exn |> Z.of_int)) in
      CaseV (mixop, [ vi; vwidth ])
  | i, None ->
      let mixop = [ [ Atom "INT" ]; [] ] in
      let vi = Value.NumV (`Int (i |> Bigint.to_int_exn |> Z.of_int)) in
      CaseV (mixop, [ vi ])

(* Texts *)

let in_text (text : text) : Value.t = TextV text.it

(* Identifiers *)

let in_id (id : id) : Value.t = TextV id.it

(* Variables (scoped identifiers) *)

let in_var (var : var) : Value.t =
  match var.it with
  | Top id ->
      let mixop = [ [ Atom "TOP" ]; [] ] in
      let vid = in_id id in
      CaseV (mixop, [ vid ])
  | Current id ->
      let mixop = [ [ Atom "CURRENT" ]; [] ] in
      let vid = in_id id in
      CaseV (mixop, [ vid ])

(* Members *)

let in_member (member : member) : Value.t = TextV member.it

(* Match kinds *)

let in_match_kind (match_kind : match_kind) : Value.t = TextV match_kind.it

(* State labels *)

let in_state_label (state_label : state_label) : Value.t = TextV state_label.it

(* Unary operators *)

let in_unop (unop : unop) : Value.t =
  match unop.it with
  | BNotOp ->
      let mixop = [ [ Atom "BNOT" ] ] in
      CaseV (mixop, [])
  | LNotOp ->
      let mixop = [ [ Atom "LNOT" ] ] in
      CaseV (mixop, [])
  | UPlusOp ->
      let mixop = [ [ Atom "UPLUS" ] ] in
      CaseV (mixop, [])
  | UMinusOp ->
      let mixop = [ [ Atom "UMINUS" ] ] in
      CaseV (mixop, [])

(* Binary operators *)

let in_binop (binop : binop) : Value.t =
  match binop.it with
  | PlusOp ->
      let mixop = [ [ Atom "PLUS" ] ] in
      CaseV (mixop, [])
  | SPlusOp ->
      let mixop = [ [ Atom "SPLUS" ] ] in
      CaseV (mixop, [])
  | MinusOp ->
      let mixop = [ [ Atom "MINUS" ] ] in
      CaseV (mixop, [])
  | SMinusOp ->
      let mixop = [ [ Atom "SMINUS" ] ] in
      CaseV (mixop, [])
  | MulOp ->
      let mixop = [ [ Atom "MUL" ] ] in
      CaseV (mixop, [])
  | DivOp ->
      let mixop = [ [ Atom "DIV" ] ] in
      CaseV (mixop, [])
  | ModOp ->
      let mixop = [ [ Atom "MOD" ] ] in
      CaseV (mixop, [])
  | ShlOp ->
      let mixop = [ [ Atom "SHL" ] ] in
      CaseV (mixop, [])
  | ShrOp ->
      let mixop = [ [ Atom "SHR" ] ] in
      CaseV (mixop, [])
  | LeOp ->
      let mixop = [ [ Atom "LE" ] ] in
      CaseV (mixop, [])
  | GeOp ->
      let mixop = [ [ Atom "GE" ] ] in
      CaseV (mixop, [])
  | LtOp ->
      let mixop = [ [ Atom "LT" ] ] in
      CaseV (mixop, [])
  | GtOp ->
      let mixop = [ [ Atom "GT" ] ] in
      CaseV (mixop, [])
  | EqOp ->
      let mixop = [ [ Atom "EQ" ] ] in
      CaseV (mixop, [])
  | NeOp ->
      let mixop = [ [ Atom "NE" ] ] in
      CaseV (mixop, [])
  | BAndOp ->
      let mixop = [ [ Atom "BAND" ] ] in
      CaseV (mixop, [])
  | BXorOp ->
      let mixop = [ [ Atom "BXOR" ] ] in
      CaseV (mixop, [])
  | BOrOp ->
      let mixop = [ [ Atom "BOR" ] ] in
      CaseV (mixop, [])
  | ConcatOp ->
      let mixop = [ [ Atom "CONCAT" ] ] in
      CaseV (mixop, [])
  | LAndOp ->
      let mixop = [ [ Atom "LAND" ] ] in
      CaseV (mixop, [])
  | LOrOp ->
      let mixop = [ [ Atom "LOR" ] ] in
      CaseV (mixop, [])

(* Directions *)

let in_dir (dir : dir) : Value.t =
  match dir.it with
  | No ->
      let mixop = [ [ Atom "NO" ] ] in
      CaseV (mixop, [])
  | In ->
      let mixop = [ [ Atom "IN" ] ] in
      CaseV (mixop, [])
  | Out ->
      let mixop = [ [ Atom "OUT" ] ] in
      CaseV (mixop, [])
  | InOut ->
      let mixop = [ [ Atom "INOUT" ] ] in
      CaseV (mixop, [])

(* Types *)

let rec in_typ (typ : typ) : Value.t =
  match typ.it with
  | VoidT ->
      let mixop = [ [ Atom "VoidT" ] ] in
      CaseV (mixop, [])
  | ErrT ->
      let mixop = [ [ Atom "ErrT" ] ] in
      CaseV (mixop, [])
  | MatchKindT ->
      let mixop = [ [ Atom "MatchKindT" ] ] in
      CaseV (mixop, [])
  | StrT ->
      let mixop = [ [ Atom "StrT" ] ] in
      CaseV (mixop, [])
  | BoolT ->
      let mixop = [ [ Atom "BoolT" ] ] in
      CaseV (mixop, [])
  | IntT ->
      let mixop = [ [ Atom "IntT" ] ] in
      CaseV (mixop, [])
  | FIntT expr ->
      let mixop = [ [ Atom "FIntT" ]; [] ] in
      let vexpr = in_expr expr in
      CaseV (mixop, [ vexpr ])
  | FBitT expr ->
      let mixop = [ [ Atom "FBitT" ]; [] ] in
      let vexpr = in_expr expr in
      CaseV (mixop, [ vexpr ])
  | VBitT expr ->
      let mixop = [ [ Atom "VBitT" ]; [] ] in
      let vexpr = in_expr expr in
      CaseV (mixop, [ vexpr ])
  | StackT (typ, expr) ->
      let mixop = [ [ Atom "StackT" ]; []; [] ] in
      let vtyp = in_typ typ in
      let vexpr = in_expr expr in
      CaseV (mixop, [ vtyp; vexpr ])
  | ListT typ ->
      let mixop = [ [ Atom "ListT" ]; [] ] in
      let vtyp = in_typ typ in
      CaseV (mixop, [ vtyp ])
  | TupleT typs ->
      let mixop = [ [ Atom "TupleT" ]; [] ] in
      let vtyps = in_typs typs in
      CaseV (mixop, [ vtyps ])
  | NameT var ->
      let mixop = [ [ Atom "NameT" ]; [] ] in
      let vvar = in_var var in
      CaseV (mixop, [ vvar ])
  | SpecT (var, targs) ->
      let mixop = [ [ Atom "SpecT" ]; []; [] ] in
      let vvar = in_var var in
      let vtargs = in_targs targs in
      CaseV (mixop, [ vvar; vtargs ])
  | AnyT ->
      let mixop = [ [ Atom "AnyT" ] ] in
      CaseV (mixop, [])

and in_typs (typs : typ list) : Value.t = in_list in_typ typs

(* Type parameters *)

and in_tparam (tparam : tparam) : Value.t = in_id tparam
and in_tparams (tparams : tparam list) : Value.t = in_list in_tparam tparams

(* Parameters *)

and in_param (param : param) : Value.t =
  let id, dir, typ, expr_opt, _ = param.it in
  let mixop = [ []; []; []; []; [] ] in
  let vid = in_id id in
  let vdir = in_dir dir in
  let vtyp = in_typ typ in
  let vexpr_opt = in_opt in_expr expr_opt in
  CaseV (mixop, [ vid; vdir; vtyp; vexpr_opt ])

and in_params (params : param list) : Value.t = in_list in_param params

(* Constructor parameters *)

and in_cparam (cparam : cparam) : Value.t = in_param cparam
and in_cparams (cparams : cparam list) : Value.t = in_list in_cparam cparams

(* Type arguments *)

and in_targ (targ : targ) : Value.t = in_typ targ

and in_targs (targs : targ list) : Value.t =
  let vtargs = List.map in_targ targs in
  ListV vtargs

(* Arguments *)

and in_arg (arg : arg) : Value.t =
  match arg.it with
  | ExprA expr ->
      let mixop = [ [ Atom "ExprA" ]; [] ] in
      let vexpr = in_expr expr in
      CaseV (mixop, [ vexpr ])
  | NameA (id, expr_opt) ->
      let mixop = [ [ Atom "NameA" ]; []; [] ] in
      let vid = in_id id in
      let vexpr_opt = in_opt in_expr expr_opt in
      CaseV (mixop, [ vid; vexpr_opt ])
  | AnyA ->
      let mixop = [ [ Atom "AnyA" ] ] in
      CaseV (mixop, [])

and in_args (args : arg list) : Value.t = in_list in_arg args

(* Expressions *)

and in_expr (expr : expr) : Value.t =
  match expr.it with
  | BoolE { boolean } ->
      let mixop = [ [ Atom "BoolE" ]; [] ] in
      let vboolean = in_bool boolean in
      CaseV (mixop, [ vboolean ])
  | StrE { text } ->
      let mixop = [ [ Atom "StrE" ]; [] ] in
      let vtext = in_text text in
      CaseV (mixop, [ vtext ])
  | NumE { num } ->
      let mixop = [ [ Atom "NumE" ]; [] ] in
      let vnum = in_num num in
      CaseV (mixop, [ vnum ])
  | VarE { var } ->
      let mixop = [ [ Atom "NameE" ]; [] ] in
      let vvar = in_var var in
      CaseV (mixop, [ vvar ])
  | SeqE { exprs } ->
      let mixop = [ [ Atom "SeqE" ]; [] ] in
      let vexprs = in_exprs exprs in
      CaseV (mixop, [ vexprs ])
  | SeqDefaultE { exprs } ->
      let mixop = [ [ Atom "SeqDefaultE" ]; [] ] in
      let vexprs = in_exprs exprs in
      CaseV (mixop, [ vexprs ])
  | RecordE { fields } ->
      let mixop = [ [ Atom "RecordE" ]; [] ] in
      let vfields = in_list (in_pair in_member in_expr) fields in
      CaseV (mixop, [ vfields ])
  | RecordDefaultE { fields } ->
      let mixop = [ [ Atom "RecordDefaultE" ]; [] ] in
      let vfields = in_list (in_pair in_member in_expr) fields in
      CaseV (mixop, [ vfields ])
  | DefaultE ->
      let mixop = [ [ Atom "DefaultE" ] ] in
      CaseV (mixop, [])
  | InvalidE ->
      let mixop = [ [ Atom "InvalidE" ] ] in
      CaseV (mixop, [])
  | UnE { unop; expr } ->
      let mixop = [ [ Atom "UnE" ]; []; [] ] in
      let vunop = in_unop unop in
      let vexpr = in_expr expr in
      CaseV (mixop, [ vunop; vexpr ])
  | BinE { binop; expr_l; expr_r } ->
      let mixop = [ [ Atom "BinE" ]; []; []; [] ] in
      let vbinop = in_binop binop in
      let vexpr_l = in_expr expr_l in
      let vexpr_r = in_expr expr_r in
      CaseV (mixop, [ vbinop; vexpr_l; vexpr_r ])
  | TernE { expr_cond; expr_then; expr_else } ->
      let mixop = [ [ Atom "TernE" ]; []; []; [] ] in
      let vexpr_cond = in_expr expr_cond in
      let vexpr_then = in_expr expr_then in
      let vexpr_else = in_expr expr_else in
      CaseV (mixop, [ vexpr_cond; vexpr_then; vexpr_else ])
  | CastE { typ; expr } ->
      let mixop = [ [ Atom "CastE" ]; []; [] ] in
      let vtyp = in_typ typ in
      let vexpr = in_expr expr in
      CaseV (mixop, [ vtyp; vexpr ])
  | MaskE { expr_base; expr_mask } ->
      let mixop = [ [ Atom "MaskE" ]; []; [] ] in
      let vexpr_base = in_expr expr_base in
      let vexpr_mask = in_expr expr_mask in
      CaseV (mixop, [ vexpr_base; vexpr_mask ])
  | RangeE { expr_lb; expr_ub } ->
      let mixop = [ [ Atom "RangeE" ]; []; [] ] in
      let vexpr_lb = in_expr expr_lb in
      let vexpr_ub = in_expr expr_ub in
      CaseV (mixop, [ vexpr_lb; vexpr_ub ])
  | SelectE { exprs_select; cases } ->
      let mixop = [ [ Atom "SelectE" ]; []; [] ] in
      let vexprs_select = in_exprs exprs_select in
      let vcases = in_select_cases cases in
      CaseV (mixop, [ vexprs_select; vcases ])
  | ArrAccE { expr_base; expr_idx } ->
      let mixop = [ [ Atom "ArrAccE" ]; []; [] ] in
      let vexpr_base = in_expr expr_base in
      let vexpr_idx = in_expr expr_idx in
      CaseV (mixop, [ vexpr_base; vexpr_idx ])
  | BitAccE { expr_base; expr_lo; expr_hi } ->
      let mixop = [ [ Atom "BitAccE" ]; []; []; [] ] in
      let vexpr_base = in_expr expr_base in
      let vexpr_lo = in_expr expr_lo in
      let vexpr_hi = in_expr expr_hi in
      CaseV (mixop, [ vexpr_base; vexpr_lo; vexpr_hi ])
  | ErrAccE { member } ->
      let mixop = [ [ Atom "ErrAccE" ]; [] ] in
      let vmember = in_member member in
      CaseV (mixop, [ vmember ])
  | TypeAccE { var_base; member } ->
      let mixop = [ [ Atom "TypeAccE" ]; []; [] ] in
      let vvar_base = in_var var_base in
      let vmember = in_member member in
      CaseV (mixop, [ vvar_base; vmember ])
  | ExprAccE { expr_base; member } ->
      let mixop = [ [ Atom "ExprAccE" ]; []; [] ] in
      let vexpr_base = in_expr expr_base in
      let vmember = in_member member in
      CaseV (mixop, [ vexpr_base; vmember ])
  | CallFuncE { var_func; targs; args } ->
      let mixop = [ [ Atom "CallFuncE" ]; []; []; [] ] in
      let vvar_func = in_var var_func in
      let vtargs = in_targs targs in
      let vargs = in_args args in
      CaseV (mixop, [ vvar_func; vtargs; vargs ])
  | CallMethodE { expr_base; member; targs; args } ->
      let mixop = [ [ Atom "CallMethodE" ]; []; []; []; [] ] in
      let vexpr_base = in_expr expr_base in
      let vmember = in_member member in
      let vtargs = in_targs targs in
      let vargs = in_args args in
      CaseV (mixop, [ vexpr_base; vmember; vtargs; vargs ])
  | CallTypeE { var_typ; member; targs; args } ->
      let mixop = [ [ Atom "CallTypeE" ]; []; []; []; [] ] in
      let vvar_typ = in_var var_typ in
      let vmember = in_member member in
      let vtargs = in_targs targs in
      let vargs = in_args args in
      CaseV (mixop, [ vvar_typ; vmember; vtargs; vargs ])
  | InstE { var_inst; targs; args } ->
      let mixop = [ [ Atom "InstE" ]; []; []; [] ] in
      let vvar_inst = in_var var_inst in
      let vtargs = in_targs targs in
      let vargs = in_args args in
      CaseV (mixop, [ vvar_inst; vtargs; vargs ])

and in_exprs (exprs : expr list) : Value.t = in_list in_expr exprs

(* Keyset expressions *)

and in_keyset (keyset : keyset) : Value.t =
  match keyset.it with
  | ExprK expr ->
      let mixop = [ [ Atom "ExprK" ]; [] ] in
      let vexpr = in_expr expr in
      CaseV (mixop, [ vexpr ])
  | DefaultK ->
      let mixop = [ [ Atom "DefaultK" ] ] in
      CaseV (mixop, [])
  | AnyK ->
      let mixop = [ [ Atom "AnyK" ] ] in
      CaseV (mixop, [])

and in_keysets (keysets : keyset list) : Value.t = in_list in_keyset keysets

(* Select-cases for select *)

and in_select_case (select_case : select_case) : Value.t =
  let keysets, state_label = select_case.it in
  let mixop = [ []; []; [] ] in
  let vkeysets = in_keysets keysets in
  let vstate_label = in_state_label state_label in
  CaseV (mixop, [ vkeysets; vstate_label ])

and in_select_cases (select_cases : select_case list) : Value.t =
  let vselect_cases = List.map in_select_case select_cases in
  ListV vselect_cases

(* Statements *)

and in_stmt (stmt : stmt) : Value.t =
  match stmt.it with
  | EmptyS ->
      let mixop = [ [ Atom "EmptyS" ] ] in
      CaseV (mixop, [])
  | AssignS { expr_l; expr_r } ->
      let mixop = [ [ Atom "AssignS" ]; []; [] ] in
      let vexpr_l = in_expr expr_l in
      let vexpr_r = in_expr expr_r in
      CaseV (mixop, [ vexpr_l; vexpr_r ])
  | SwitchS { expr_switch; cases } ->
      let mixop = [ [ Atom "SwitchS" ]; []; [] ] in
      let vexpr_switch = in_expr expr_switch in
      let vcases = in_switch_cases cases in
      CaseV (mixop, [ vexpr_switch; vcases ])
  | IfS { expr_cond; stmt_then; stmt_else } ->
      let mixop = [ [ Atom "IfS" ]; []; []; [] ] in
      let vexpr_cond = in_expr expr_cond in
      let vstmt_then = in_stmt stmt_then in
      let vstmt_else = in_stmt stmt_else in
      CaseV (mixop, [ vexpr_cond; vstmt_then; vstmt_else ])
  | BlockS { block } ->
      let mixop = [ [ Atom "BlockS" ]; [] ] in
      let vblock = in_block block in
      CaseV (mixop, [ vblock ])
  | ExitS ->
      let mixop = [ [ Atom "ExitS" ] ] in
      CaseV (mixop, [])
  | RetS { expr_ret } ->
      let mixop = [ [ Atom "RetS" ]; [] ] in
      let vexpr_ret = in_opt in_expr expr_ret in
      CaseV (mixop, [ vexpr_ret ])
  | CallFuncS { var_func; targs; args } ->
      let mixop = [ [ Atom "CallFuncS" ]; []; []; [] ] in
      let vvar_func = in_var var_func in
      let vtargs = in_targs targs in
      let vargs = in_args args in
      CaseV (mixop, [ vvar_func; vtargs; vargs ])
  | CallMethodS { expr_base; member; targs; args } ->
      let mixop = [ [ Atom "CallMethodS" ]; []; []; []; [] ] in
      let vexpr_base = in_expr expr_base in
      let vmember = in_member member in
      let vtargs = in_targs targs in
      let vargs = in_args args in
      CaseV (mixop, [ vexpr_base; vmember; vtargs; vargs ])
  | CallInstS { var_inst; targs; args } ->
      let mixop = [ [ Atom "CallInstS" ]; []; []; [] ] in
      let vvar_inst = in_var var_inst in
      let vtargs = in_targs targs in
      let vargs = in_args args in
      CaseV (mixop, [ vvar_inst; vtargs; vargs ])
  | TransS { expr_label } ->
      let mixop = [ [ Atom "TransS" ]; [] ] in
      let vexpr_label = in_expr expr_label in
      CaseV (mixop, [ vexpr_label ])
  | DeclS { decl } ->
      let mixop = [ [ Atom "DeclS" ]; [] ] in
      let vdecl = in_decl decl in
      CaseV (mixop, [ vdecl ])

and in_stmts (stmts : stmt list) : Value.t = in_list in_stmt stmts

(* Blocks (sequence of statements) *)

and in_block (block : block) : Value.t =
  let stmts, _ = block.it in
  let mixop = [ [ Atom "BlockB" ]; [] ] in
  let vstmts = in_stmts stmts in
  CaseV (mixop, [ vstmts ])

(* Match-cases for switch *)

and in_switch_label (switch_label : switch_label) : Value.t =
  match switch_label.it with
  | ExprL expr ->
      let mixop = [ [ Atom "ExprL" ]; [] ] in
      let vexpr = in_expr expr in
      CaseV (mixop, [ vexpr ])
  | DefaultL ->
      let mixop = [ [ Atom "DefaultL" ] ] in
      CaseV (mixop, [])

and in_switch_case (switch_case : switch_case) : Value.t =
  match switch_case.it with
  | MatchC (switch_label, block) ->
      let mixop = [ [ Atom "MatchC" ]; []; [] ] in
      let vswitch_label = in_switch_label switch_label in
      let vblock = in_block block in
      CaseV (mixop, [ vswitch_label; vblock ])
  | FallC switch_label ->
      let mixop = [ [ Atom "FallC" ]; [] ] in
      let vswitch_label = in_switch_label switch_label in
      CaseV (mixop, [ vswitch_label ])

and in_switch_cases (switch_cases : switch_case list) : Value.t =
  in_list in_switch_case switch_cases

(* Declarations *)

and in_decl (decl : decl) : Value.t =
  match decl.it with
  | ConstD { id; typ; value; _ } ->
      let mixop = [ [ Atom "ConstD" ]; []; []; [] ] in
      let vid = in_id id in
      let vtyp = in_typ typ in
      let vvalue = in_expr value in
      CaseV (mixop, [ vid; vtyp; vvalue ])
  | VarD { id; typ; init; _ } ->
      let mixop = [ [ Atom "VarD" ]; []; []; [] ] in
      let vid = in_id id in
      let vtyp = in_typ typ in
      let vinit = in_opt in_expr init in
      CaseV (mixop, [ vid; vtyp; vinit ])
  | ErrD { members } ->
      let mixop = [ [ Atom "ErrD" ]; [] ] in
      let vmembers = in_list in_member members in
      CaseV (mixop, [ vmembers ])
  | MatchKindD { members } ->
      let mixop = [ [ Atom "MatchKindD" ]; [] ] in
      let vmembers = in_list in_member members in
      CaseV (mixop, [ vmembers ])
  | InstD { id; var_inst; targs; args; init; _ } ->
      let mixop = [ [ Atom "InstD" ]; []; []; []; []; [] ] in
      let vid = in_id id in
      let vvar_inst = in_var var_inst in
      let vtargs = in_targs targs in
      let vargs = in_args args in
      let vinit = in_decls init in
      CaseV (mixop, [ vid; vvar_inst; vtargs; vargs; vinit ])
  | StructD { id; tparams; fields; _ } ->
      let mixop = [ [ Atom "StructD" ]; []; []; [] ] in
      let vid = in_id id in
      let vtparams = in_tparams tparams in
      let vfields =
        let fields = List.map (fun (member, typ, _) -> (member, typ)) fields in
        in_list (in_pair in_member in_typ) fields
      in
      CaseV (mixop, [ vid; vtparams; vfields ])
  | HeaderD { id; tparams; fields; _ } ->
      let mixop = [ [ Atom "HeaderD" ]; []; []; [] ] in
      let vid = in_id id in
      let vtparams = in_tparams tparams in
      let vfields =
        let fields = List.map (fun (member, typ, _) -> (member, typ)) fields in
        in_list (in_pair in_member in_typ) fields
      in
      CaseV (mixop, [ vid; vtparams; vfields ])
  | UnionD { id; tparams; fields; _ } ->
      let mixop = [ [ Atom "UnionD" ]; []; []; [] ] in
      let vid = in_id id in
      let vtparams = in_tparams tparams in
      let vfields =
        let fields = List.map (fun (member, typ, _) -> (member, typ)) fields in
        in_list (in_pair in_member in_typ) fields
      in
      CaseV (mixop, [ vid; vtparams; vfields ])
  | EnumD { id; members; _ } ->
      let mixop = [ [ Atom "EnumD" ]; []; [] ] in
      let vid = in_id id in
      let vmembers = in_list in_member members in
      CaseV (mixop, [ vid; vmembers ])
  | SEnumD { id; typ; fields; _ } ->
      let mixop = [ [ Atom "SEnumD" ]; []; []; [] ] in
      let vid = in_id id in
      let vtyp = in_typ typ in
      let vfields = in_list (in_pair in_member in_expr) fields in
      CaseV (mixop, [ vid; vtyp; vfields ])
  | NewTypeD { id; typdef; _ } -> (
      match typdef with
      | Left typ ->
          let mixop = [ [ Atom "NewTypeD" ]; []; [] ] in
          let vid = in_id id in
          let vtyp = in_typ typ in
          CaseV (mixop, [ vid; vtyp ])
      | _ -> failwith "(TODO) NewTypeD")
  | TypeDefD { id; typdef; _ } -> (
      match typdef with
      | Left typ ->
          let mixop = [ [ Atom "TypeDefD" ]; []; [] ] in
          let vid = in_id id in
          let vtyp = in_typ typ in
          CaseV (mixop, [ vid; vtyp ])
      | _ -> failwith "(TODO) TypeDefD")
  | ValueSetD { id; typ; size; _ } ->
      let mixop = [ [ Atom "ValueSetD" ]; []; []; [] ] in
      let vid = in_id id in
      let vtyp = in_typ typ in
      let vsize = in_expr size in
      CaseV (mixop, [ vid; vtyp; vsize ])
  | ParserTypeD { id; tparams; params; _ } ->
      let mixop = [ [ Atom "ParserTypeD" ]; []; []; [] ] in
      let vid = in_id id in
      let vtparams = in_tparams tparams in
      let vparams = in_params params in
      CaseV (mixop, [ vid; vtparams; vparams ])
  | ParserD { id; params; cparams; locals; states; _ } ->
      let mixop = [ [ Atom "ParserD" ]; []; []; []; []; [] ] in
      let vid = in_id id in
      let vparams = in_params params in
      let vcparams = in_cparams cparams in
      let vlocals = in_decls locals in
      let vstates = in_parser_states states in
      CaseV (mixop, [ vid; vparams; vcparams; vlocals; vstates ])
  | TableD { id; table; _ } ->
      let mixop = [ [ Atom "TableD" ]; []; [] ] in
      let vid = in_id id in
      let vtable = in_table table in
      CaseV (mixop, [ vid; vtable ])
  | ControlTypeD { id; tparams; params; _ } ->
      let mixop = [ [ Atom "ControlTypeD" ]; []; []; [] ] in
      let vid = in_id id in
      let vtparams = in_tparams tparams in
      let vparams = in_params params in
      CaseV (mixop, [ vid; vtparams; vparams ])
  | ControlD { id; params; cparams; locals; body; _ } ->
      let mixop = [ [ Atom "ControlD" ]; []; []; []; []; [] ] in
      let vid = in_id id in
      let vparams = in_params params in
      let vcparams = in_cparams cparams in
      let vlocals = in_decls locals in
      let vbody = in_block body in
      CaseV (mixop, [ vid; vparams; vcparams; vlocals; vbody ])
  | ActionD { id; params; body; _ } ->
      let mixop = [ [ Atom "ActionD" ]; []; []; [] ] in
      let vid = in_id id in
      let vparams = in_params params in
      let vbody = in_block body in
      CaseV (mixop, [ vid; vparams; vbody ])
  | FuncD { id; typ_ret; tparams; params; body; _ } ->
      let mixop = [ [ Atom "FuncD" ]; []; []; []; []; [] ] in
      let vid = in_id id in
      let vtyp_ret = in_typ typ_ret in
      let vtparams = in_tparams tparams in
      let vparams = in_params params in
      let vbody = in_block body in
      CaseV (mixop, [ vid; vtyp_ret; vtparams; vparams; vbody ])
  | ExternFuncD { id; typ_ret; tparams; params; _ } ->
      let mixop = [ [ Atom "ExternFuncD" ]; []; []; []; [] ] in
      let vid = in_id id in
      let vtyp_ret = in_typ typ_ret in
      let vtparams = in_tparams tparams in
      let vparams = in_params params in
      CaseV (mixop, [ vid; vtyp_ret; vtparams; vparams ])
  | ExternObjectD { id; tparams; mthds; _ } ->
      let mixop = [ [ Atom "ExternObjectD" ]; []; []; [] ] in
      let vid = in_id id in
      let vtparams = in_tparams tparams in
      let vmthds = in_mthds mthds in
      CaseV (mixop, [ vid; vtparams; vmthds ])
  | PackageTypeD { id; tparams; cparams; _ } ->
      let mixop = [ [ Atom "PackageTypeD" ]; []; []; [] ] in
      let vid = in_id id in
      let vtparams = in_tparams tparams in
      let vcparams = in_cparams cparams in
      CaseV (mixop, [ vid; vtparams; vcparams ])

and in_decls (decls : decl list) : Value.t =
  let vdecls = List.map in_decl decls in
  ListV vdecls

(* Parser state machine *)

and in_parser_state (parser_state : parser_state) : Value.t =
  let state_label, block, _ = parser_state.it in
  let mixop = [ []; []; [] ] in
  let vstate_label = in_state_label state_label in
  let vblock = in_block block in
  CaseV (mixop, [ vstate_label; vblock ])

and in_parser_states (parser_states : parser_state list) : Value.t =
  in_list in_parser_state parser_states

(* Tables *)

and in_table (table : table) : Value.t = in_table_properties table

(* Table properties *)

and in_table_property (table_property : table_property) : Value.t =
  match table_property with
  | KeyP table_keys ->
      let mixop = [ [ Atom "KeyP" ]; [] ] in
      let vtable_keys = in_table_keys table_keys in
      CaseV (mixop, [ vtable_keys ])
  | ActionP table_actions ->
      let mixop = [ [ Atom "ActionP" ]; [] ] in
      let vtable_actions = in_table_actions table_actions in
      CaseV (mixop, [ vtable_actions ])
  | EntryP table_entries ->
      let mixop = [ [ Atom "EntryP" ]; [] ] in
      let vtable_entries = in_table_entries table_entries in
      CaseV (mixop, [ vtable_entries ])
  | DefaultP table_default ->
      let mixop = [ [ Atom "DefaultP" ]; [] ] in
      let vtable_default = in_table_default table_default in
      CaseV (mixop, [ vtable_default ])
  | CustomP table_custom ->
      let mixop = [ [ Atom "CustomP" ]; [] ] in
      let vtable_custom = in_table_custom table_custom in
      CaseV (mixop, [ vtable_custom ])

and in_table_properties (table_properties : table_property list) : Value.t =
  in_list in_table_property table_properties

(* Table keys *)

and in_table_key (table_key : table_key) : Value.t =
  let expr, match_kind, _ = table_key.it in
  let mixop = [ []; []; [] ] in
  let vexpr = in_expr expr in
  let vmatch_kind = in_match_kind match_kind in
  CaseV (mixop, [ vexpr; vmatch_kind ])

and in_table_keys (table_keys : table_keys) : Value.t =
  in_list in_table_key table_keys.it

(* Table action references *)

and in_table_action (table_action : table_action) : Value.t =
  let var, args, _ = table_action.it in
  let mixop = [ []; []; [] ] in
  let vvar = in_var var in
  let vargs = in_args args in
  CaseV (mixop, [ vvar; vargs ])

and in_table_actions (table_actions : table_actions) : Value.t =
  in_list in_table_action table_actions.it

(* Table entries *)

and in_table_entry (table_entry : table_entry) : Value.t =
  let table_entry_const, keysets, table_action, expr_opt, _ = table_entry.it in
  let mixop = [ []; []; []; []; [] ] in
  let vtable_entry_const = in_bool table_entry_const in
  let vkeysets = in_keysets keysets in
  let vtable_action = in_table_action table_action in
  let vexpr_opt = in_opt in_expr expr_opt in
  CaseV (mixop, [ vtable_entry_const; vkeysets; vtable_action; vexpr_opt ])

and in_table_entries (table_entries : table_entries) : Value.t =
  let table_entries_const, table_entries = table_entries.it in
  let mixop = [ []; []; [] ] in
  let vtable_entries_const = in_bool table_entries_const in
  let vtable_entries = in_list in_table_entry table_entries in
  CaseV (mixop, [ vtable_entries_const; vtable_entries ])

(* Table default properties *)

and in_table_default (table_default : table_default) : Value.t =
  let table_default_const, table_action = table_default.it in
  let mixop = [ []; []; [] ] in
  let vtable_default_const = in_bool table_default_const in
  let vtable_action = in_table_action table_action in
  CaseV (mixop, [ vtable_default_const; vtable_action ])

(* Table custom properties *)

and in_table_custom (table_custom : table_custom) : Value.t =
  let table_custom_const, member, expr, _ = table_custom.it in
  let mixop = [ []; []; []; [] ] in
  let vtable_custom_const = in_bool table_custom_const in
  let vmember = in_member member in
  let vexpr = in_expr expr in
  CaseV (mixop, [ vtable_custom_const; vmember; vexpr ])

(* Methods *)

and in_mthd (mthd : mthd) : Value.t =
  match mthd.it with
  | ExternConsM { id; cparams; _ } ->
      let mixop = [ [ Atom "ExternConsM" ]; []; [] ] in
      let vid = in_id id in
      let vcparams = in_cparams cparams in
      CaseV (mixop, [ vid; vcparams ])
  | ExternAbstractM { id; typ_ret; tparams; params; _ } ->
      let mixop = [ [ Atom "ExternAbstractM" ]; []; []; []; [] ] in
      let vid = in_id id in
      let vtyp_ret = in_typ typ_ret in
      let vtparams = in_tparams tparams in
      let vparams = in_params params in
      CaseV (mixop, [ vid; vtyp_ret; vtparams; vparams ])
  | ExternM { id; typ_ret; tparams; params; _ } ->
      let mixop = [ [ Atom "ExternM" ]; []; []; []; [] ] in
      let vid = in_id id in
      let vtyp_ret = in_typ typ_ret in
      let vtparams = in_tparams tparams in
      let vparams = in_params params in
      CaseV (mixop, [ vid; vtyp_ret; vtparams; vparams ])

and in_mthds (mthds : mthd list) : Value.t = in_list in_mthd mthds

(* Program *)

let in_program (program : program) : Value.t = in_decls program
