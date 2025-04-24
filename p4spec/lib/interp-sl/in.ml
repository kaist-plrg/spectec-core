open Xl.Atom
module P4 = P4el.Ast
open Sl.Ast
module Dep = Runtime_testgen.Dep
open Util.Source

(* Helpers *)

let in_opt (do_in : Ctx.t -> 'a -> value) (ctx : Ctx.t) (opt : 'a option) :
    value =
  let vopt = Option.map (do_in ctx) opt in
  let value = OptV vopt $$$ Dep.Graph.fresh () in
  Ctx.add_node ~taint:true ctx value;
  value

let in_list (do_in : Ctx.t -> 'a -> value) (ctx : Ctx.t) (lst : 'a list) : value
    =
  let vlst = List.map (do_in ctx) lst in
  let value = ListV vlst $$$ Dep.Graph.fresh () in
  Ctx.add_node ~taint:true ctx value;
  value

let in_pair (do_in_a : Ctx.t -> 'a -> value) (do_in_b : Ctx.t -> 'b -> value)
    (ctx : Ctx.t) ((a, b) : 'a * 'b) : value =
  let va = do_in_a ctx a in
  let vb = do_in_b ctx b in
  let value = TupleV [ va; vb ] $$$ Dep.Graph.fresh () in
  Ctx.add_node ~taint:true ctx value;
  value

let atom (s : string) : atom = Atom s $ no_region

(* Booleans *)

let in_bool (ctx : Ctx.t) (boolean : bool) : value =
  let value = BoolV boolean $$$ Dep.Graph.fresh () in
  Ctx.add_node ~taint:true ctx value;
  value

(* Numbers *)

let in_num (ctx : Ctx.t) (num : P4.num) : value =
  match num.it with
  | i, Some (width, signed) ->
      let mixop =
        if signed then [ [ atom "FINT" ]; []; [] ]
        else [ [ atom "FBIT" ]; []; [] ]
      in
      let vwidth =
        let vwidth = NumV (`Nat width) $$$ Dep.Graph.fresh () in
        Ctx.add_node ~taint:true ctx vwidth;
        vwidth
      in
      let vint =
        let vint = NumV (`Int i) $$$ Dep.Graph.fresh () in
        Ctx.add_node ~taint:true ctx vint;
        vint
      in
      let value = CaseV (mixop, [ vwidth; vint ]) $$$ Dep.Graph.fresh () in
      Ctx.add_node ~taint:true ctx value;
      value
  | i, None ->
      let mixop = [ [ atom "INT" ]; [] ] in
      let vint =
        let vint = NumV (`Int i) $$$ Dep.Graph.fresh () in
        Ctx.add_node ~taint:true ctx vint;
        vint
      in
      let value = CaseV (mixop, [ vint ]) $$$ Dep.Graph.fresh () in
      Ctx.add_node ~taint:true ctx value;
      value

(* Texts *)

let in_text (ctx : Ctx.t) (text : P4.text) : value =
  let value = TextV text.it $$$ Dep.Graph.fresh () in
  Ctx.add_node ~taint:true ctx value;
  value

(* Identifiers *)

let in_id (ctx : Ctx.t) (id : P4.id) : value =
  let value = TextV id.it $$$ Dep.Graph.fresh () in
  Ctx.add_node ~taint:true ctx value;
  value

(* Variables (scoped identifiers) *)

let in_var (ctx : Ctx.t) (var : P4.var) : value =
  match var.it with
  | Top id ->
      let mixop = [ [ atom "TOP" ]; [] ] in
      let vid = in_id ctx id in
      let value = CaseV (mixop, [ vid ]) $$$ Dep.Graph.fresh () in
      Ctx.add_node ~taint:true ctx value;
      value
  | Current id ->
      let mixop = [ [ atom "CURRENT" ]; [] ] in
      let vid = in_id ctx id in
      let value = CaseV (mixop, [ vid ]) $$$ Dep.Graph.fresh () in
      Ctx.add_node ~taint:true ctx value;
      value

(* Members *)

let rec in_member (ctx : Ctx.t) (member : P4.member) : value =
  let value = TextV member.it $$$ Dep.Graph.fresh () in
  Ctx.add_node ~taint:true ctx value;
  value

and in_members (ctx : Ctx.t) (members : P4.member list) : value =
  in_list in_member ctx members

(* Match kinds *)

let in_match_kind (ctx : Ctx.t) (match_kind : P4.match_kind) : value =
  let value = TextV match_kind.it $$$ Dep.Graph.fresh () in
  Ctx.add_node ~taint:true ctx value;
  value

(* State labels *)

let in_state_label (ctx : Ctx.t) (state_label : P4.state_label) : value =
  let value = TextV state_label.it $$$ Dep.Graph.fresh () in
  Ctx.add_node ~taint:true ctx value;
  value

(* Unary operators *)

let in_unop (ctx : Ctx.t) (unop : P4.unop) : value =
  let mixop =
    match unop.it with
    | BNotOp -> [ [ atom "BNOT" ] ]
    | LNotOp -> [ [ atom "LNOT" ] ]
    | UPlusOp -> [ [ atom "UPLUS" ] ]
    | UMinusOp -> [ [ atom "UMINUS" ] ]
  in
  let value = CaseV (mixop, []) $$$ Dep.Graph.fresh () in
  Ctx.add_node ~taint:true ctx value;
  value

(* Binary operators *)

let in_binop (ctx : Ctx.t) (binop : P4.binop) : value =
  let mixop =
    match binop.it with
    | PlusOp -> [ [ atom "PLUS" ] ]
    | SPlusOp -> [ [ atom "SPLUS" ] ]
    | MinusOp -> [ [ atom "MINUS" ] ]
    | SMinusOp -> [ [ atom "SMINUS" ] ]
    | MulOp -> [ [ atom "MUL" ] ]
    | DivOp -> [ [ atom "DIV" ] ]
    | ModOp -> [ [ atom "MOD" ] ]
    | ShlOp -> [ [ atom "SHL" ] ]
    | ShrOp -> [ [ atom "SHR" ] ]
    | LeOp -> [ [ atom "LE" ] ]
    | GeOp -> [ [ atom "GE" ] ]
    | LtOp -> [ [ atom "LT" ] ]
    | GtOp -> [ [ atom "GT" ] ]
    | EqOp -> [ [ atom "EQ" ] ]
    | NeOp -> [ [ atom "NE" ] ]
    | BAndOp -> [ [ atom "BAND" ] ]
    | BXorOp -> [ [ atom "BXOR" ] ]
    | BOrOp -> [ [ atom "BOR" ] ]
    | ConcatOp -> [ [ atom "CONCAT" ] ]
    | LAndOp -> [ [ atom "LAND" ] ]
    | LOrOp -> [ [ atom "LOR" ] ]
  in
  let value = CaseV (mixop, []) $$$ Dep.Graph.fresh () in
  Ctx.add_node ~taint:true ctx value;
  value

(* Directions *)

let in_dir (ctx : Ctx.t) (dir : P4.dir) : value =
  let mixop =
    match dir.it with
    | No -> [ [ atom "NO" ] ]
    | In -> [ [ atom "IN" ] ]
    | Out -> [ [ atom "OUT" ] ]
    | InOut -> [ [ atom "INOUT" ] ]
  in
  let value = CaseV (mixop, []) $$$ Dep.Graph.fresh () in
  Ctx.add_node ~taint:true ctx value;
  value

(* Types *)

let rec in_typ (ctx : Ctx.t) (typ : P4.typ) : value =
  match typ.it with
  | VoidT ->
      let mixop = [ [ atom "VoidT" ] ] in
      let value = CaseV (mixop, []) $$$ Dep.Graph.fresh () in
      Ctx.add_node ~taint:true ctx value;
      value
  | ErrT ->
      let mixop = [ [ atom "ErrT" ] ] in
      let value = CaseV (mixop, []) $$$ Dep.Graph.fresh () in
      Ctx.add_node ~taint:true ctx value;
      value
  | MatchKindT ->
      let mixop = [ [ atom "MatchKindT" ] ] in
      let value = CaseV (mixop, []) $$$ Dep.Graph.fresh () in
      Ctx.add_node ~taint:true ctx value;
      value
  | StrT ->
      let mixop = [ [ atom "StrT" ] ] in
      let value = CaseV (mixop, []) $$$ Dep.Graph.fresh () in
      Ctx.add_node ~taint:true ctx value;
      value
  | BoolT ->
      let mixop = [ [ atom "BoolT" ] ] in
      let value = CaseV (mixop, []) $$$ Dep.Graph.fresh () in
      Ctx.add_node ~taint:true ctx value;
      value
  | IntT ->
      let mixop = [ [ atom "IntT" ] ] in
      let value = CaseV (mixop, []) $$$ Dep.Graph.fresh () in
      Ctx.add_node ~taint:true ctx value;
      value
  | FIntT expr ->
      let mixop = [ [ atom "FIntT" ]; [] ] in
      let vexpr = in_expr ctx expr in
      let value = CaseV (mixop, [ vexpr ]) $$$ Dep.Graph.fresh () in
      Ctx.add_node ~taint:true ctx value;
      value
  | FBitT expr ->
      let mixop = [ [ atom "FBitT" ]; [] ] in
      let vexpr = in_expr ctx expr in
      let value = CaseV (mixop, [ vexpr ]) $$$ Dep.Graph.fresh () in
      Ctx.add_node ~taint:true ctx value;
      value
  | VBitT expr ->
      let mixop = [ [ atom "VBitT" ]; [] ] in
      let vexpr = in_expr ctx expr in
      let value = CaseV (mixop, [ vexpr ]) $$$ Dep.Graph.fresh () in
      Ctx.add_node ~taint:true ctx value;
      value
  | StackT (typ, expr) ->
      let mixop = [ [ atom "StackT" ]; []; [] ] in
      let vtyp = in_typ ctx typ in
      let vexpr = in_expr ctx expr in
      let value = CaseV (mixop, [ vtyp; vexpr ]) $$$ Dep.Graph.fresh () in
      Ctx.add_node ~taint:true ctx value;
      value
  | ListT typ ->
      let mixop = [ [ atom "ListT" ]; [] ] in
      let vtyp = in_typ ctx typ in
      let value = CaseV (mixop, [ vtyp ]) $$$ Dep.Graph.fresh () in
      Ctx.add_node ~taint:true ctx value;
      value
  | TupleT typs ->
      let mixop = [ [ atom "TupleT" ]; [] ] in
      let vtyps = in_typs ctx typs in
      let value = CaseV (mixop, [ vtyps ]) $$$ Dep.Graph.fresh () in
      Ctx.add_node ~taint:true ctx value;
      value
  | NameT var ->
      let mixop = [ [ atom "NameT" ]; [] ] in
      let vvar = in_var ctx var in
      let value = CaseV (mixop, [ vvar ]) $$$ Dep.Graph.fresh () in
      Ctx.add_node ~taint:true ctx value;
      value
  | SpecT (var, targs) ->
      let mixop = [ [ atom "SpecT" ]; []; [] ] in
      let vvar = in_var ctx var in
      let vtargs = in_targs ctx targs in
      let value = CaseV (mixop, [ vvar; vtargs ]) $$$ Dep.Graph.fresh () in
      Ctx.add_node ~taint:true ctx value;
      value
  | AnyT ->
      let mixop = [ [ atom "AnyT" ] ] in
      let value = CaseV (mixop, []) $$$ Dep.Graph.fresh () in
      Ctx.add_node ~taint:true ctx value;
      value

and in_typs (ctx : Ctx.t) (typs : P4.typ list) : value = in_list in_typ ctx typs

(* Type parameters *)

and in_tparam (ctx : Ctx.t) (tparam : P4.tparam) : value =
  let value = TextV tparam.it $$$ Dep.Graph.fresh () in
  Ctx.add_node ~taint:true ctx value;
  value

and in_tparams (ctx : Ctx.t) (tparams : P4.tparam list) : value =
  in_list in_tparam ctx tparams

(* Parameters *)

and in_param (ctx : Ctx.t) (param : P4.param) : value =
  let id, dir, typ, expr_opt, _ = param.it in
  let mixop = [ []; []; []; []; [] ] in
  let vid = in_id ctx id in
  let vdir = in_dir ctx dir in
  let vtyp = in_typ ctx typ in
  let vexpr_opt = in_opt in_expr ctx expr_opt in
  let value =
    CaseV (mixop, [ vid; vdir; vtyp; vexpr_opt ]) $$$ Dep.Graph.fresh ()
  in
  Ctx.add_node ~taint:true ctx value;
  value

and in_params (ctx : Ctx.t) (params : P4.param list) : value =
  in_list in_param ctx params

(* Constructor parameters *)

and in_cparam (ctx : Ctx.t) (cparam : P4.cparam) : value = in_param ctx cparam

and in_cparams (ctx : Ctx.t) (cparams : P4.cparam list) : value =
  in_list in_cparam ctx cparams

(* Type arguments *)

and in_targ (ctx : Ctx.t) (targ : P4.targ) : value = in_typ ctx targ

and in_targs (ctx : Ctx.t) (targs : P4.targ list) : value =
  in_list in_targ ctx targs

(* Arguments *)

and in_arg (ctx : Ctx.t) (arg : P4.arg) : value =
  match arg.it with
  | ExprA expr ->
      let mixop = [ [ atom "ExprA" ]; [] ] in
      let vexpr = in_expr ctx expr in
      let value = CaseV (mixop, [ vexpr ]) $$$ Dep.Graph.fresh () in
      Ctx.add_node ~taint:true ctx value;
      value
  | NameA (id, expr_opt) ->
      let mixop = [ [ atom "NameA" ]; []; [] ] in
      let vid = in_id ctx id in
      let vexpr_opt = in_opt in_expr ctx expr_opt in
      let value = CaseV (mixop, [ vid; vexpr_opt ]) $$$ Dep.Graph.fresh () in
      Ctx.add_node ~taint:true ctx value;
      value
  | AnyA ->
      let mixop = [ [ atom "AnyA" ] ] in
      let value = CaseV (mixop, []) $$$ Dep.Graph.fresh () in
      Ctx.add_node ~taint:true ctx value;
      value

and in_args (ctx : Ctx.t) (args : P4.arg list) : value = in_list in_arg ctx args

(* Expressions *)

and in_expr (ctx : Ctx.t) (expr : P4.expr) : value =
  match expr.it with
  | BoolE { boolean } ->
      let mixop = [ [ atom "BoolE" ]; [] ] in
      let vboolean = in_bool ctx boolean in
      let value = CaseV (mixop, [ vboolean ]) $$$ Dep.Graph.fresh () in
      Ctx.add_node ~taint:true ctx value;
      value
  | StrE { text } ->
      let mixop = [ [ atom "StrE" ]; [] ] in
      let vtext = in_text ctx text in
      let value = CaseV (mixop, [ vtext ]) $$$ Dep.Graph.fresh () in
      Ctx.add_node ~taint:true ctx value;
      value
  | NumE { num } ->
      let mixop = [ [ atom "NumE" ]; [] ] in
      let vnum = in_num ctx num in
      let value = CaseV (mixop, [ vnum ]) $$$ Dep.Graph.fresh () in
      Ctx.add_node ~taint:true ctx value;
      value
  | VarE { var } ->
      let mixop = [ [ atom "NameE" ]; [] ] in
      let vvar = in_var ctx var in
      let value = CaseV (mixop, [ vvar ]) $$$ Dep.Graph.fresh () in
      Ctx.add_node ~taint:true ctx value;
      value
  | SeqE { exprs } ->
      let mixop = [ [ atom "SeqE" ]; [] ] in
      let vexprs = in_exprs ctx exprs in
      let value = CaseV (mixop, [ vexprs ]) $$$ Dep.Graph.fresh () in
      Ctx.add_node ~taint:true ctx value;
      value
  | SeqDefaultE { exprs } ->
      let mixop = [ [ atom "SeqDefaultE" ]; [] ] in
      let vexprs = in_exprs ctx exprs in
      let value = CaseV (mixop, [ vexprs ]) $$$ Dep.Graph.fresh () in
      Ctx.add_node ~taint:true ctx value;
      value
  | RecordE { fields } ->
      let mixop = [ [ atom "RecordE" ]; [] ] in
      let vfields = in_list (in_pair in_member in_expr) ctx fields in
      let value = CaseV (mixop, [ vfields ]) $$$ Dep.Graph.fresh () in
      Ctx.add_node ~taint:true ctx value;
      value
  | RecordDefaultE { fields } ->
      let mixop = [ [ atom "RecordDefaultE" ]; [] ] in
      let vfields = in_list (in_pair in_member in_expr) ctx fields in
      let value = CaseV (mixop, [ vfields ]) $$$ Dep.Graph.fresh () in
      Ctx.add_node ~taint:true ctx value;
      value
  | DefaultE ->
      let mixop = [ [ atom "DefaultE" ] ] in
      let value = CaseV (mixop, []) $$$ Dep.Graph.fresh () in
      Ctx.add_node ~taint:true ctx value;
      value
  | InvalidE ->
      let mixop = [ [ atom "InvalidE" ] ] in
      let value = CaseV (mixop, []) $$$ Dep.Graph.fresh () in
      Ctx.add_node ~taint:true ctx value;
      value
  | UnE { unop; expr } ->
      let mixop = [ [ atom "UnE" ]; []; [] ] in
      let vunop = in_unop ctx unop in
      let vexpr = in_expr ctx expr in
      let value = CaseV (mixop, [ vunop; vexpr ]) $$$ Dep.Graph.fresh () in
      Ctx.add_node ~taint:true ctx value;
      value
  | BinE { binop; expr_l; expr_r } ->
      let mixop = [ [ atom "BinE" ]; []; []; [] ] in
      let vbinop = in_binop ctx binop in
      let vexpr_l = in_expr ctx expr_l in
      let vexpr_r = in_expr ctx expr_r in
      let value =
        CaseV (mixop, [ vbinop; vexpr_l; vexpr_r ]) $$$ Dep.Graph.fresh ()
      in
      Ctx.add_node ~taint:true ctx value;
      value
  | TernE { expr_cond; expr_then; expr_else } ->
      let mixop = [ [ atom "TernE" ]; []; []; [] ] in
      let vexpr_cond = in_expr ctx expr_cond in
      let vexpr_then = in_expr ctx expr_then in
      let vexpr_else = in_expr ctx expr_else in
      let value =
        CaseV (mixop, [ vexpr_cond; vexpr_then; vexpr_else ])
        $$$ Dep.Graph.fresh ()
      in
      Ctx.add_node ~taint:true ctx value;
      value
  | CastE { typ; expr } ->
      let mixop = [ [ atom "CastE" ]; []; [] ] in
      let vtyp = in_typ ctx typ in
      let vexpr = in_expr ctx expr in
      let value = CaseV (mixop, [ vtyp; vexpr ]) $$$ Dep.Graph.fresh () in
      Ctx.add_node ~taint:true ctx value;
      value
  | MaskE { expr_base; expr_mask } ->
      let mixop = [ [ atom "MaskE" ]; []; [] ] in
      let vexpr_base = in_expr ctx expr_base in
      let vexpr_mask = in_expr ctx expr_mask in
      let value =
        CaseV (mixop, [ vexpr_base; vexpr_mask ]) $$$ Dep.Graph.fresh ()
      in
      Ctx.add_node ~taint:true ctx value;
      value
  | RangeE { expr_lb; expr_ub } ->
      let mixop = [ [ atom "RangeE" ]; []; [] ] in
      let vexpr_lb = in_expr ctx expr_lb in
      let vexpr_ub = in_expr ctx expr_ub in
      let value =
        CaseV (mixop, [ vexpr_lb; vexpr_ub ]) $$$ Dep.Graph.fresh ()
      in
      Ctx.add_node ~taint:true ctx value;
      value
  | SelectE { exprs_select; cases } ->
      let mixop = [ [ atom "SelectE" ]; []; [] ] in
      let vexprs_select = in_exprs ctx exprs_select in
      let vcases = in_select_cases ctx cases in
      let value =
        CaseV (mixop, [ vexprs_select; vcases ]) $$$ Dep.Graph.fresh ()
      in
      Ctx.add_node ~taint:true ctx value;
      value
  | ArrAccE { expr_base; expr_idx } ->
      let mixop = [ [ atom "ArrAccE" ]; []; [] ] in
      let vexpr_base = in_expr ctx expr_base in
      let vexpr_idx = in_expr ctx expr_idx in
      let value =
        CaseV (mixop, [ vexpr_base; vexpr_idx ]) $$$ Dep.Graph.fresh ()
      in
      Ctx.add_node ~taint:true ctx value;
      value
  | BitAccE { expr_base; expr_lo; expr_hi } ->
      let mixop = [ [ atom "BitAccE" ]; []; []; [] ] in
      let vexpr_base = in_expr ctx expr_base in
      let vexpr_lo = in_expr ctx expr_lo in
      let vexpr_hi = in_expr ctx expr_hi in
      let value =
        CaseV (mixop, [ vexpr_base; vexpr_lo; vexpr_hi ]) $$$ Dep.Graph.fresh ()
      in
      Ctx.add_node ~taint:true ctx value;
      value
  | ErrAccE { member } ->
      let mixop = [ [ atom "ErrAccE" ]; [] ] in
      let vmember = in_member ctx member in
      let value = CaseV (mixop, [ vmember ]) $$$ Dep.Graph.fresh () in
      Ctx.add_node ~taint:true ctx value;
      value
  | TypeAccE { var_base; member } ->
      let mixop = [ [ atom "TypeAccE" ]; []; [] ] in
      let vvar_base = in_var ctx var_base in
      let vmember = in_member ctx member in
      let value =
        CaseV (mixop, [ vvar_base; vmember ]) $$$ Dep.Graph.fresh ()
      in
      Ctx.add_node ~taint:true ctx value;
      value
  | ExprAccE { expr_base; member } ->
      let mixop = [ [ atom "ExprAccE" ]; []; [] ] in
      let vexpr_base = in_expr ctx expr_base in
      let vmember = in_member ctx member in
      let value =
        CaseV (mixop, [ vexpr_base; vmember ]) $$$ Dep.Graph.fresh ()
      in
      Ctx.add_node ~taint:true ctx value;
      value
  | CallFuncE { var_func; targs; args } ->
      let mixop = [ [ atom "CallFuncE" ]; []; []; [] ] in
      let vvar_func = in_var ctx var_func in
      let vtargs = in_targs ctx targs in
      let vargs = in_args ctx args in
      let value =
        CaseV (mixop, [ vvar_func; vtargs; vargs ]) $$$ Dep.Graph.fresh ()
      in
      Ctx.add_node ~taint:true ctx value;
      value
  | CallMethodE { expr_base; member; targs; args } ->
      let mixop = [ [ atom "CallMethodE" ]; []; []; []; [] ] in
      let vexpr_base = in_expr ctx expr_base in
      let vmember = in_member ctx member in
      let vtargs = in_targs ctx targs in
      let vargs = in_args ctx args in
      let value =
        CaseV (mixop, [ vexpr_base; vmember; vtargs; vargs ])
        $$$ Dep.Graph.fresh ()
      in
      Ctx.add_node ~taint:true ctx value;
      value
  | CallTypeE { var_typ; member; targs; args } ->
      let mixop = [ [ atom "CallTypeE" ]; []; []; []; [] ] in
      let vvar_typ = in_var ctx var_typ in
      let vmember = in_member ctx member in
      let vtargs = in_targs ctx targs in
      let vargs = in_args ctx args in
      let value =
        CaseV (mixop, [ vvar_typ; vmember; vtargs; vargs ])
        $$$ Dep.Graph.fresh ()
      in
      Ctx.add_node ~taint:true ctx value;
      value
  | InstE { var_inst; targs; args } ->
      let mixop = [ [ atom "InstE" ]; []; []; [] ] in
      let vvar_inst = in_var ctx var_inst in
      let vtargs = in_targs ctx targs in
      let vargs = in_args ctx args in
      let value =
        CaseV (mixop, [ vvar_inst; vtargs; vargs ]) $$$ Dep.Graph.fresh ()
      in
      Ctx.add_node ~taint:true ctx value;
      value

and in_exprs (ctx : Ctx.t) (exprs : P4.expr list) : value =
  in_list in_expr ctx exprs

(* Keyset expressions *)

and in_keyset (ctx : Ctx.t) (keyset : P4.keyset) : value =
  match keyset.it with
  | ExprK expr ->
      let mixop = [ [ atom "ExprK" ]; [] ] in
      let vexpr = in_expr ctx expr in
      let value = CaseV (mixop, [ vexpr ]) $$$ Dep.Graph.fresh () in
      Ctx.add_node ~taint:true ctx value;
      value
  | DefaultK ->
      let mixop = [ [ atom "DefaultK" ] ] in
      let value = CaseV (mixop, []) $$$ Dep.Graph.fresh () in
      Ctx.add_node ~taint:true ctx value;
      value
  | AnyK ->
      let mixop = [ [ atom "AnyK" ] ] in
      let value = CaseV (mixop, []) $$$ Dep.Graph.fresh () in
      Ctx.add_node ~taint:true ctx value;
      value

and in_keysets (ctx : Ctx.t) (keysets : P4.keyset list) : value =
  in_list in_keyset ctx keysets

(* Select-cases for select *)

and in_select_case (ctx : Ctx.t) (select_case : P4.select_case) : value =
  let keysets, state_label = select_case.it in
  let mixop = [ []; []; [] ] in
  let vkeysets = in_keysets ctx keysets in
  let vstate_label = in_state_label ctx state_label in
  let value =
    CaseV (mixop, [ vkeysets; vstate_label ]) $$$ Dep.Graph.fresh ()
  in
  Ctx.add_node ~taint:true ctx value;
  value

and in_select_cases (ctx : Ctx.t) (select_cases : P4.select_case list) : value =
  in_list in_select_case ctx select_cases

(* Statements *)

and in_stmt (ctx : Ctx.t) (stmt : P4.stmt) : value =
  match stmt.it with
  | EmptyS ->
      let mixop = [ [ atom "EmptyS" ] ] in
      let value = CaseV (mixop, []) $$$ Dep.Graph.fresh () in
      Ctx.add_node ~taint:true ctx value;
      value
  | AssignS { expr_l; expr_r } ->
      let mixop = [ [ atom "AssignS" ]; []; [] ] in
      let vexpr_l = in_expr ctx expr_l in
      let vexpr_r = in_expr ctx expr_r in
      let value = CaseV (mixop, [ vexpr_l; vexpr_r ]) $$$ Dep.Graph.fresh () in
      Ctx.add_node ~taint:true ctx value;
      value
  | SwitchS { expr_switch; cases } ->
      let mixop = [ [ atom "SwitchS" ]; []; [] ] in
      let vexpr_switch = in_expr ctx expr_switch in
      let vcases = in_switch_cases ctx cases in
      let value =
        CaseV (mixop, [ vexpr_switch; vcases ]) $$$ Dep.Graph.fresh ()
      in
      Ctx.add_node ~taint:true ctx value;
      value
  | IfS { expr_cond; stmt_then; stmt_else } ->
      let mixop = [ [ atom "IfS" ]; []; []; [] ] in
      let vexpr_cond = in_expr ctx expr_cond in
      let vstmt_then = in_stmt ctx stmt_then in
      let vstmt_else = in_stmt ctx stmt_else in
      let value =
        CaseV (mixop, [ vexpr_cond; vstmt_then; vstmt_else ])
        $$$ Dep.Graph.fresh ()
      in
      Ctx.add_node ~taint:true ctx value;
      value
  | BlockS { block } ->
      let mixop = [ [ atom "BlockS" ]; [] ] in
      let vblock = in_block ctx block in
      let value = CaseV (mixop, [ vblock ]) $$$ Dep.Graph.fresh () in
      Ctx.add_node ~taint:true ctx value;
      value
  | ExitS ->
      let mixop = [ [ atom "ExitS" ] ] in
      let value = CaseV (mixop, []) $$$ Dep.Graph.fresh () in
      Ctx.add_node ~taint:true ctx value;
      value
  | RetS { expr_ret } ->
      let mixop = [ [ atom "RetS" ]; [] ] in
      let vexpr_ret = in_opt in_expr ctx expr_ret in
      let value = CaseV (mixop, [ vexpr_ret ]) $$$ Dep.Graph.fresh () in
      Ctx.add_node ~taint:true ctx value;
      value
  | CallFuncS { var_func; targs; args } ->
      let mixop = [ [ atom "CallFuncS" ]; []; []; [] ] in
      let vvar_func = in_var ctx var_func in
      let vtargs = in_targs ctx targs in
      let vargs = in_args ctx args in
      let value =
        CaseV (mixop, [ vvar_func; vtargs; vargs ]) $$$ Dep.Graph.fresh ()
      in
      Ctx.add_node ~taint:true ctx value;
      value
  | CallMethodS { expr_base; member; targs; args } ->
      let mixop = [ [ atom "CallMethodS" ]; []; []; []; [] ] in
      let vexpr_base = in_expr ctx expr_base in
      let vmember = in_member ctx member in
      let vtargs = in_targs ctx targs in
      let vargs = in_args ctx args in
      let value =
        CaseV (mixop, [ vexpr_base; vmember; vtargs; vargs ])
        $$$ Dep.Graph.fresh ()
      in
      Ctx.add_node ~taint:true ctx value;
      value
  | CallInstS { var_inst; targs; args } ->
      let mixop = [ [ atom "CallInstS" ]; []; []; [] ] in
      let vvar_inst = in_var ctx var_inst in
      let vtargs = in_targs ctx targs in
      let vargs = in_args ctx args in
      let value =
        CaseV (mixop, [ vvar_inst; vtargs; vargs ]) $$$ Dep.Graph.fresh ()
      in
      Ctx.add_node ~taint:true ctx value;
      value
  | TransS { expr_label } ->
      let mixop = [ [ atom "TransS" ]; [] ] in
      let vexpr_label = in_expr ctx expr_label in
      let value = CaseV (mixop, [ vexpr_label ]) $$$ Dep.Graph.fresh () in
      Ctx.add_node ~taint:true ctx value;
      value
  | DeclS { decl } ->
      let mixop = [ [ atom "DeclS" ]; [] ] in
      let vdecl = in_decl ctx decl in
      let value = CaseV (mixop, [ vdecl ]) $$$ Dep.Graph.fresh () in
      Ctx.add_node ~taint:true ctx value;
      value

and in_stmts (ctx : Ctx.t) (stmts : P4.stmt list) : value =
  in_list in_stmt ctx stmts

(* Blocks (sequence of statements) *)

and in_block (ctx : Ctx.t) (block : P4.block) : value =
  let stmts, _ = block.it in
  let mixop = [ [ atom "BlockB" ]; [] ] in
  let vstmts = in_stmts ctx stmts in
  let value = CaseV (mixop, [ vstmts ]) $$$ Dep.Graph.fresh () in
  Ctx.add_node ~taint:true ctx value;
  value

(* Match-cases for switch *)

and in_switch_label (ctx : Ctx.t) (switch_label : P4.switch_label) : value =
  match switch_label.it with
  | ExprL expr ->
      let mixop = [ [ atom "ExprL" ]; [] ] in
      let vexpr = in_expr ctx expr in
      let value = CaseV (mixop, [ vexpr ]) $$$ Dep.Graph.fresh () in
      Ctx.add_node ~taint:true ctx value;
      value
  | DefaultL ->
      let mixop = [ [ atom "DefaultL" ] ] in
      let value = CaseV (mixop, []) $$$ Dep.Graph.fresh () in
      Ctx.add_node ~taint:true ctx value;
      value

and in_switch_case (ctx : Ctx.t) (switch_case : P4.switch_case) : value =
  match switch_case.it with
  | MatchC (switch_label, block) ->
      let mixop = [ [ atom "MatchC" ]; []; [] ] in
      let vswitch_label = in_switch_label ctx switch_label in
      let vblock = in_block ctx block in
      let value =
        CaseV (mixop, [ vswitch_label; vblock ]) $$$ Dep.Graph.fresh ()
      in
      Ctx.add_node ~taint:true ctx value;
      value
  | FallC switch_label ->
      let mixop = [ [ atom "FallC" ]; [] ] in
      let vswitch_label = in_switch_label ctx switch_label in
      let value = CaseV (mixop, [ vswitch_label ]) $$$ Dep.Graph.fresh () in
      Ctx.add_node ~taint:true ctx value;
      value

and in_switch_cases (ctx : Ctx.t) (switch_cases : P4.switch_case list) : value =
  in_list in_switch_case ctx switch_cases

(* Declarations *)

and in_typdef (ctx : Ctx.t) (typdef : (P4.typ, P4.decl) P4.alt) : value =
  match typdef with
  | Left typ ->
      let mixop = [ [ atom "TypeD" ]; [] ] in
      let vtyp = in_typ ctx typ in
      let value = CaseV (mixop, [ vtyp ]) $$$ Dep.Graph.fresh () in
      Ctx.add_node ~taint:true ctx value;
      value
  | Right decl ->
      let mixop = [ [ atom "DeclD" ]; [] ] in
      let vdecl = in_decl ctx decl in
      let value = CaseV (mixop, [ vdecl ]) $$$ Dep.Graph.fresh () in
      Ctx.add_node ~taint:true ctx value;
      value

and in_decl (ctx : Ctx.t) (decl : P4.decl) : value =
  match decl.it with
  | ConstD { id; typ; value; _ } ->
      let mixop = [ [ atom "ConstD" ]; []; []; [] ] in
      let vid = in_id ctx id in
      let vtyp = in_typ ctx typ in
      let vvalue = in_expr ctx value in
      let value = CaseV (mixop, [ vid; vtyp; vvalue ]) $$$ Dep.Graph.fresh () in
      Ctx.add_node ~taint:true ctx value;
      value
  | VarD { id; typ; init; _ } ->
      let mixop = [ [ atom "VarD" ]; []; []; [] ] in
      let vid = in_id ctx id in
      let vtyp = in_typ ctx typ in
      let vinit = in_opt in_expr ctx init in
      let value = CaseV (mixop, [ vid; vtyp; vinit ]) $$$ Dep.Graph.fresh () in
      Ctx.add_node ~taint:true ctx value;
      value
  | ErrD { members } ->
      let mixop = [ [ atom "ErrD" ]; [] ] in
      let vmembers = in_members ctx members in
      let value = CaseV (mixop, [ vmembers ]) $$$ Dep.Graph.fresh () in
      Ctx.add_node ~taint:true ctx value;
      value
  | MatchKindD { members } ->
      let mixop = [ [ atom "MatchKindD" ]; [] ] in
      let vmembers = in_members ctx members in
      let value = CaseV (mixop, [ vmembers ]) $$$ Dep.Graph.fresh () in
      Ctx.add_node ~taint:true ctx value;
      value
  | InstD { id; var_inst; targs; args; init; _ } ->
      let mixop = [ [ atom "InstD" ]; []; []; []; []; [] ] in
      let vid = in_id ctx id in
      let vvar_inst = in_var ctx var_inst in
      let vtargs = in_targs ctx targs in
      let vargs = in_args ctx args in
      let vinit = in_decls ctx init in
      let value =
        CaseV (mixop, [ vid; vvar_inst; vtargs; vargs; vinit ])
        $$$ Dep.Graph.fresh ()
      in
      Ctx.add_node ~taint:true ctx value;
      value
  | StructD { id; tparams; fields; _ } ->
      let mixop = [ [ atom "StructD" ]; []; []; [] ] in
      let vid = in_id ctx id in
      let vtparams = in_tparams ctx tparams in
      let vfields =
        let fields = List.map (fun (member, typ, _) -> (member, typ)) fields in
        in_list (in_pair in_member in_typ) ctx fields
      in
      let value =
        CaseV (mixop, [ vid; vtparams; vfields ]) $$$ Dep.Graph.fresh ()
      in
      Ctx.add_node ~taint:true ctx value;
      value
  | HeaderD { id; tparams; fields; _ } ->
      let mixop = [ [ atom "HeaderD" ]; []; []; [] ] in
      let vid = in_id ctx id in
      let vtparams = in_tparams ctx tparams in
      let vfields =
        let fields = List.map (fun (member, typ, _) -> (member, typ)) fields in
        in_list (in_pair in_member in_typ) ctx fields
      in
      let value =
        CaseV (mixop, [ vid; vtparams; vfields ]) $$$ Dep.Graph.fresh ()
      in
      Ctx.add_node ~taint:true ctx value;
      value
  | UnionD { id; tparams; fields; _ } ->
      let mixop = [ [ atom "UnionD" ]; []; []; [] ] in
      let vid = in_id ctx id in
      let vtparams = in_tparams ctx tparams in
      let vfields =
        let fields = List.map (fun (member, typ, _) -> (member, typ)) fields in
        in_list (in_pair in_member in_typ) ctx fields
      in
      let value =
        CaseV (mixop, [ vid; vtparams; vfields ]) $$$ Dep.Graph.fresh ()
      in
      Ctx.add_node ~taint:true ctx value;
      value
  | EnumD { id; members; _ } ->
      let mixop = [ [ atom "EnumD" ]; []; [] ] in
      let vid = in_id ctx id in
      let vmembers = in_members ctx members in
      let value = CaseV (mixop, [ vid; vmembers ]) $$$ Dep.Graph.fresh () in
      Ctx.add_node ~taint:true ctx value;
      value
  | SEnumD { id; typ; fields; _ } ->
      let mixop = [ [ atom "SEnumD" ]; []; []; [] ] in
      let vid = in_id ctx id in
      let vtyp = in_typ ctx typ in
      let vfields = in_list (in_pair in_member in_expr) ctx fields in
      let value =
        CaseV (mixop, [ vid; vtyp; vfields ]) $$$ Dep.Graph.fresh ()
      in
      Ctx.add_node ~taint:true ctx value;
      value
  | NewTypeD { id; typdef; _ } ->
      let mixop = [ [ atom "NewTypeD" ]; []; [] ] in
      let vid = in_id ctx id in
      let vtypdef = in_typdef ctx typdef in
      let value = CaseV (mixop, [ vid; vtypdef ]) $$$ Dep.Graph.fresh () in
      Ctx.add_node ~taint:true ctx value;
      value
  | TypeDefD { id; typdef; _ } ->
      let mixop = [ [ atom "TypeDefD" ]; []; [] ] in
      let vid = in_id ctx id in
      let vtypdef = in_typdef ctx typdef in
      let value = CaseV (mixop, [ vid; vtypdef ]) $$$ Dep.Graph.fresh () in
      Ctx.add_node ~taint:true ctx value;
      value
  | ValueSetD { id; typ; size; _ } ->
      let mixop = [ [ atom "ValueSetD" ]; []; []; [] ] in
      let vid = in_id ctx id in
      let vtyp = in_typ ctx typ in
      let vsize = in_expr ctx size in
      let value = CaseV (mixop, [ vid; vtyp; vsize ]) $$$ Dep.Graph.fresh () in
      Ctx.add_node ~taint:true ctx value;
      value
  | ParserTypeD { id; tparams; params; _ } ->
      let mixop = [ [ atom "ParserTypeD" ]; []; []; [] ] in
      let vid = in_id ctx id in
      let vtparams = in_tparams ctx tparams in
      let vparams = in_params ctx params in
      let value =
        CaseV (mixop, [ vid; vtparams; vparams ]) $$$ Dep.Graph.fresh ()
      in
      Ctx.add_node ~taint:true ctx value;
      value
  | ParserD { id; params; cparams; locals; states; _ } ->
      let mixop = [ [ atom "ParserD" ]; []; []; []; []; [] ] in
      let vid = in_id ctx id in
      let vparams = in_params ctx params in
      let vcparams = in_cparams ctx cparams in
      let vlocals = in_decls ctx locals in
      let vstates = in_parser_states ctx states in
      let value =
        CaseV (mixop, [ vid; vparams; vcparams; vlocals; vstates ])
        $$$ Dep.Graph.fresh ()
      in
      Ctx.add_node ~taint:true ctx value;
      value
  | TableD { id; table; _ } ->
      let mixop = [ [ atom "TableD" ]; []; [] ] in
      let vid = in_id ctx id in
      let vtable = in_table ctx table in
      let value = CaseV (mixop, [ vid; vtable ]) $$$ Dep.Graph.fresh () in
      Ctx.add_node ~taint:true ctx value;
      value
  | ControlTypeD { id; tparams; params; _ } ->
      let mixop = [ [ atom "ControlTypeD" ]; []; []; [] ] in
      let vid = in_id ctx id in
      let vtparams = in_tparams ctx tparams in
      let vparams = in_params ctx params in
      let value =
        CaseV (mixop, [ vid; vtparams; vparams ]) $$$ Dep.Graph.fresh ()
      in
      Ctx.add_node ~taint:true ctx value;
      value
  | ControlD { id; params; cparams; locals; body; _ } ->
      let mixop = [ [ atom "ControlD" ]; []; []; []; []; [] ] in
      let vid = in_id ctx id in
      let vparams = in_params ctx params in
      let vcparams = in_cparams ctx cparams in
      let vlocals = in_decls ctx locals in
      let vbody = in_block ctx body in
      let value =
        CaseV (mixop, [ vid; vparams; vcparams; vlocals; vbody ])
        $$$ Dep.Graph.fresh ()
      in
      Ctx.add_node ~taint:true ctx value;
      value
  | ActionD { id; params; body; _ } ->
      let mixop = [ [ atom "ActionD" ]; []; []; [] ] in
      let vid = in_id ctx id in
      let vparams = in_params ctx params in
      let vbody = in_block ctx body in
      let value =
        CaseV (mixop, [ vid; vparams; vbody ]) $$$ Dep.Graph.fresh ()
      in
      Ctx.add_node ~taint:true ctx value;
      value
  | FuncD { id; typ_ret; tparams; params; body; _ } ->
      let mixop = [ [ atom "FuncD" ]; []; []; []; []; [] ] in
      let vid = in_id ctx id in
      let vtyp_ret = in_typ ctx typ_ret in
      let vtparams = in_tparams ctx tparams in
      let vparams = in_params ctx params in
      let vbody = in_block ctx body in
      let value =
        CaseV (mixop, [ vid; vtyp_ret; vtparams; vparams; vbody ])
        $$$ Dep.Graph.fresh ()
      in
      Ctx.add_node ~taint:true ctx value;
      value
  | ExternFuncD { id; typ_ret; tparams; params; _ } ->
      let mixop = [ [ atom "ExternFuncD" ]; []; []; []; [] ] in
      let vid = in_id ctx id in
      let vtyp_ret = in_typ ctx typ_ret in
      let vtparams = in_tparams ctx tparams in
      let vparams = in_params ctx params in
      let value =
        CaseV (mixop, [ vid; vtyp_ret; vtparams; vparams ])
        $$$ Dep.Graph.fresh ()
      in
      Ctx.add_node ~taint:true ctx value;
      value
  | ExternObjectD { id; tparams; mthds; _ } ->
      let mixop = [ [ atom "ExternObjectD" ]; []; []; [] ] in
      let vid = in_id ctx id in
      let vtparams = in_tparams ctx tparams in
      let vmthds = in_mthds ctx mthds in
      let value =
        CaseV (mixop, [ vid; vtparams; vmthds ]) $$$ Dep.Graph.fresh ()
      in
      Ctx.add_node ~taint:true ctx value;
      value
  | PackageTypeD { id; tparams; cparams; _ } ->
      let mixop = [ [ atom "PackageTypeD" ]; []; []; [] ] in
      let vid = in_id ctx id in
      let vtparams = in_tparams ctx tparams in
      let vcparams = in_cparams ctx cparams in
      let value =
        CaseV (mixop, [ vid; vtparams; vcparams ]) $$$ Dep.Graph.fresh ()
      in
      Ctx.add_node ~taint:true ctx value;
      value

and in_decls (ctx : Ctx.t) (decls : P4.decl list) : value =
  in_list in_decl ctx decls

(* Parser state machine *)

and in_parser_state (ctx : Ctx.t) (parser_state : P4.parser_state) : value =
  let state_label, block, _ = parser_state.it in
  let mixop = [ []; []; [] ] in
  let vstate_label = in_state_label ctx state_label in
  let vblock = in_block ctx block in
  let value = CaseV (mixop, [ vstate_label; vblock ]) $$$ Dep.Graph.fresh () in
  Ctx.add_node ~taint:true ctx value;
  value

and in_parser_states (ctx : Ctx.t) (parser_states : P4.parser_state list) :
    value =
  in_list in_parser_state ctx parser_states

(* Tables *)

and in_table (ctx : Ctx.t) (table : P4.table) : value =
  in_list in_table_property ctx table

(* Table properties *)

and in_table_property (ctx : Ctx.t) (table_property : P4.table_property) : value
    =
  match table_property with
  | KeyP table_keys ->
      let mixop = [ [ atom "KeyP" ]; [] ] in
      let vtable_keys = in_table_keys ctx table_keys in
      let value = CaseV (mixop, [ vtable_keys ]) $$$ Dep.Graph.fresh () in
      Ctx.add_node ~taint:true ctx value;
      value
  | ActionP table_actions ->
      let mixop = [ [ atom "ActionP" ]; [] ] in
      let vtable_actions = in_table_actions ctx table_actions in
      let value = CaseV (mixop, [ vtable_actions ]) $$$ Dep.Graph.fresh () in
      Ctx.add_node ~taint:true ctx value;
      value
  | EntryP table_entries ->
      let mixop = [ [ atom "EntryP" ]; [] ] in
      let vtable_entries = in_table_entries ctx table_entries in
      let value = CaseV (mixop, [ vtable_entries ]) $$$ Dep.Graph.fresh () in
      Ctx.add_node ~taint:true ctx value;
      value
  | DefaultP table_default ->
      let mixop = [ [ atom "DefaultP" ]; [] ] in
      let vtable_default = in_table_default ctx table_default in
      let value = CaseV (mixop, [ vtable_default ]) $$$ Dep.Graph.fresh () in
      Ctx.add_node ~taint:true ctx value;
      value
  | CustomP table_custom ->
      let mixop = [ [ atom "CustomP" ]; [] ] in
      let vtable_custom = in_table_custom ctx table_custom in
      let value = CaseV (mixop, [ vtable_custom ]) $$$ Dep.Graph.fresh () in
      Ctx.add_node ~taint:true ctx value;
      value

(* Table keys *)

and in_table_key (ctx : Ctx.t) (table_key : P4.table_key) : value =
  let expr, match_kind, _ = table_key.it in
  let mixop = [ []; []; [] ] in
  let vexpr = in_expr ctx expr in
  let vmatch_kind = in_match_kind ctx match_kind in
  let value = CaseV (mixop, [ vexpr; vmatch_kind ]) $$$ Dep.Graph.fresh () in
  Ctx.add_node ~taint:true ctx value;
  value

and in_table_keys (ctx : Ctx.t) (table_keys : P4.table_keys) : value =
  in_list in_table_key ctx table_keys.it

(* Table action references *)

and in_table_action (ctx : Ctx.t) (table_action : P4.table_action) : value =
  let var, args, _ = table_action.it in
  let mixop = [ []; []; [] ] in
  let vvar = in_var ctx var in
  let vargs = in_args ctx args in
  let value = CaseV (mixop, [ vvar; vargs ]) $$$ Dep.Graph.fresh () in
  Ctx.add_node ~taint:true ctx value;
  value

and in_table_actions (ctx : Ctx.t) (table_actions : P4.table_actions) : value =
  in_list in_table_action ctx table_actions.it

(* Table entries *)

and in_table_entry (ctx : Ctx.t) (table_entry : P4.table_entry) : value =
  let table_entry_const, keysets, table_action, expr_opt, _ = table_entry.it in
  let mixop = [ []; []; []; []; [] ] in
  let vtable_entry_const = in_bool ctx table_entry_const in
  let vkeysets = in_keysets ctx keysets in
  let vtable_action = in_table_action ctx table_action in
  let vexpr_opt = in_opt in_expr ctx expr_opt in
  let value =
    CaseV (mixop, [ vtable_entry_const; vkeysets; vtable_action; vexpr_opt ])
    $$$ Dep.Graph.fresh ()
  in
  Ctx.add_node ~taint:true ctx value;
  value

and in_table_entries (ctx : Ctx.t) (table_entries : P4.table_entries) : value =
  let table_entries_const, table_entries = table_entries.it in
  let mixop = [ []; []; [] ] in
  let vtable_entries_const = in_bool ctx table_entries_const in
  let vtable_entries = in_list in_table_entry ctx table_entries in
  let value =
    CaseV (mixop, [ vtable_entries_const; vtable_entries ])
    $$$ Dep.Graph.fresh ()
  in
  Ctx.add_node ~taint:true ctx value;
  value

(* Table default properties *)

and in_table_default (ctx : Ctx.t) (table_default : P4.table_default) : value =
  let table_default_const, table_action = table_default.it in
  let mixop = [ []; []; [] ] in
  let vtable_default_const = in_bool ctx table_default_const in
  let vtable_action = in_table_action ctx table_action in
  let value =
    CaseV (mixop, [ vtable_default_const; vtable_action ])
    $$$ Dep.Graph.fresh ()
  in
  Ctx.add_node ~taint:true ctx value;
  value

(* Table custom properties *)

and in_table_custom (ctx : Ctx.t) (table_custom : P4.table_custom) : value =
  let table_custom_const, member, expr, _ = table_custom.it in
  let mixop = [ []; []; []; [] ] in
  let vtable_custom_const = in_bool ctx table_custom_const in
  let vmember = in_member ctx member in
  let vexpr = in_expr ctx expr in
  let value =
    CaseV (mixop, [ vtable_custom_const; vmember; vexpr ])
    $$$ Dep.Graph.fresh ()
  in
  Ctx.add_node ~taint:true ctx value;
  value

(* Methods *)

and in_mthd (ctx : Ctx.t) (mthd : P4.mthd) : value =
  match mthd.it with
  | ExternConsM { id; cparams; _ } ->
      let mixop = [ [ atom "ExternConsM" ]; []; [] ] in
      let vid = in_id ctx id in
      let vcparams = in_cparams ctx cparams in
      let value = CaseV (mixop, [ vid; vcparams ]) $$$ Dep.Graph.fresh () in
      Ctx.add_node ~taint:true ctx value;
      value
  | ExternAbstractM { id; typ_ret; tparams; params; _ } ->
      let mixop = [ [ atom "ExternAbstractM" ]; []; []; []; [] ] in
      let vid = in_id ctx id in
      let vtyp_ret = in_typ ctx typ_ret in
      let vtparams = in_tparams ctx tparams in
      let vparams = in_params ctx params in
      let value =
        CaseV (mixop, [ vid; vtyp_ret; vtparams; vparams ])
        $$$ Dep.Graph.fresh ()
      in
      Ctx.add_node ~taint:true ctx value;
      value
  | ExternM { id; typ_ret; tparams; params; _ } ->
      let mixop = [ [ atom "ExternM" ]; []; []; []; [] ] in
      let vid = in_id ctx id in
      let vtyp_ret = in_typ ctx typ_ret in
      let vtparams = in_tparams ctx tparams in
      let vparams = in_params ctx params in
      let value =
        CaseV (mixop, [ vid; vtyp_ret; vtparams; vparams ])
        $$$ Dep.Graph.fresh ()
      in
      Ctx.add_node ~taint:true ctx value;
      value

and in_mthds (ctx : Ctx.t) (mthds : P4.mthd list) : value =
  in_list in_mthd ctx mthds

(* Program *)

let in_program (ctx : Ctx.t) (includes_p4 : string list) (filename_p4 : string)
    : value =
  P4frontend.Parse.parse_file includes_p4 filename_p4 |> in_list in_decl ctx
