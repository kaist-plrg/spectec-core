open Xl.Atom
module P4 = P4el.Ast
open Sl.Ast
module Dep = Runtime_testgen.Dep
open Util.Error
open Util.Source

(* Conversion from p4cherry AST to IL value *)

(* Helpers *)

let in_typ_var (s : string) : typ' = Il.Ast.VarT (s $ no_region, [])

let in_opt (do_in : Dep.Graph.t -> 'a -> value) (typ : typ)
    (graph : Dep.Graph.t) (opt : 'a option) : value =
  let value_opt = Option.map (do_in graph) opt in
  let value =
    let vid = Dep.Graph.fresh () in
    let typ = Il.Ast.IterT (typ, Il.Ast.Opt) in
    OptV value_opt $$$ { vid; typ }
  in
  Dep.Graph.add_node ~taint:true graph value;
  value

let in_list (do_in : Dep.Graph.t -> 'a -> value) (typ : typ)
    (graph : Dep.Graph.t) (lst : 'a list) : value =
  let vlst = List.map (do_in graph) lst in
  let value =
    let vid = Dep.Graph.fresh () in
    let typ = Il.Ast.IterT (typ, Il.Ast.List) in
    ListV vlst $$$ { vid; typ }
  in
  Dep.Graph.add_node ~taint:true graph value;
  value

let in_pair (do_in_a : Dep.Graph.t -> 'a -> value)
    (do_in_b : Dep.Graph.t -> 'b -> value) (typ_a : typ) (typ_b : typ)
    (graph : Dep.Graph.t) ((a, b) : 'a * 'b) : value =
  let value_a = do_in_a graph a in
  let value_b = do_in_b graph b in
  let value =
    let vid = Dep.Graph.fresh () in
    let typ = Il.Ast.TupleT [ typ_a; typ_b ] in
    TupleV [ value_a; value_b ] $$$ { vid; typ }
  in
  Dep.Graph.add_node ~taint:true graph value;
  value

(* Atoms *)

let in_atom (s : string) : atom = Atom s $ no_region

(* Booleans *)

let in_bool (graph : Dep.Graph.t) (boolean : bool) : value =
  let value =
    let vid = Dep.Graph.fresh () in
    let typ = Il.Ast.BoolT in
    BoolV boolean $$$ { vid; typ }
  in
  Dep.Graph.add_node ~taint:true graph value;
  value

(* Numbers *)

let in_num (graph : Dep.Graph.t) (num : P4.num) : value =
  let mixop, values =
    match num.it with
    | i, Some (width, signed) ->
        let mixop =
          if signed then [ [ in_atom "FINT" ]; []; [] ]
          else [ [ in_atom "FBIT" ]; []; [] ]
        in
        let value_width =
          let vid = Dep.Graph.fresh () in
          let typ = Il.Ast.NumT `NatT in
          NumV (`Nat width) $$$ { vid; typ }
        in
        Dep.Graph.add_node ~taint:true graph value_width;
        let value_int =
          let vid = Dep.Graph.fresh () in
          let typ = Il.Ast.NumT `IntT in
          NumV (`Int i) $$$ { vid; typ }
        in
        Dep.Graph.add_node ~taint:true graph value_int;
        (mixop, [ value_width; value_int ])
    | i, None ->
        let mixop = [ [ in_atom "INT" ]; [] ] in
        let value_int =
          let vid = Dep.Graph.fresh () in
          let typ = Il.Ast.NumT `IntT in
          NumV (`Int i) $$$ { vid; typ }
        in
        Dep.Graph.add_node ~taint:true graph value_int;
        (mixop, [ value_int ])
  in
  let value =
    let vid = Dep.Graph.fresh () in
    let typ = in_typ_var "num" in
    CaseV (mixop, values) $$$ { vid; typ }
  in
  Dep.Graph.add_node ~taint:true graph value;
  value

(* Texts *)

let in_text (graph : Dep.Graph.t) (text : P4.text) : value =
  let value =
    let vid = Dep.Graph.fresh () in
    let typ = Il.Ast.TextT in
    TextV text.it $$$ { vid; typ }
  in
  Dep.Graph.add_node ~taint:true graph value;
  value

(* Identifiers *)

let in_id (graph : Dep.Graph.t) (id : P4.id) : value =
  let value =
    let vid = Dep.Graph.fresh () in
    let typ = in_typ_var "id" in
    TextV id.it $$$ { vid; typ }
  in
  Dep.Graph.add_node ~taint:true graph value;
  value

(* Variables (scoped identifiers) *)

let in_var (graph : Dep.Graph.t) (var : P4.var) : value =
  let mixop, values =
    match var.it with
    | Top id ->
        let mixop = [ [ in_atom "TOP" ]; [] ] in
        let value_id = in_id graph id in
        (mixop, [ value_id ])
    | Current id ->
        let mixop = [ [ in_atom "CURRENT" ]; [] ] in
        let value_id = in_id graph id in
        (mixop, [ value_id ])
  in
  let value =
    let vid = Dep.Graph.fresh () in
    let typ = in_typ_var "name" in
    CaseV (mixop, values) $$$ { vid; typ }
  in
  Dep.Graph.add_node ~taint:true graph value;
  value

(* Members *)

let rec in_member (graph : Dep.Graph.t) (member : P4.member) : value =
  let value =
    let vid = Dep.Graph.fresh () in
    let typ = in_typ_var "member" in
    TextV member.it $$$ { vid; typ }
  in
  Dep.Graph.add_node ~taint:true graph value;
  value

and in_members (graph : Dep.Graph.t) (members : P4.member list) : value =
  in_list in_member (in_typ_var "member" $ no_region) graph members

(* Match kinds *)

let in_match_kind (graph : Dep.Graph.t) (match_kind : P4.match_kind) : value =
  let value =
    let vid = Dep.Graph.fresh () in
    let typ = in_typ_var "matchkind" in
    TextV match_kind.it $$$ { vid; typ }
  in
  Dep.Graph.add_node ~taint:true graph value;
  value

(* State labels *)

let in_state_label (graph : Dep.Graph.t) (state_label : P4.state_label) : value
    =
  let value =
    let vid = Dep.Graph.fresh () in
    let typ = in_typ_var "statelabel" in
    TextV state_label.it $$$ { vid; typ }
  in
  Dep.Graph.add_node ~taint:true graph value;
  value

(* Unary operators *)

let in_unop (graph : Dep.Graph.t) (unop : P4.unop) : value =
  let mixop =
    match unop.it with
    | BNotOp -> [ [ in_atom "BNOT" ] ]
    | LNotOp -> [ [ in_atom "LNOT" ] ]
    | UPlusOp -> [ [ in_atom "UPLUS" ] ]
    | UMinusOp -> [ [ in_atom "UMINUS" ] ]
  in
  let value =
    let vid = Dep.Graph.fresh () in
    let typ = in_typ_var "unop" in
    CaseV (mixop, []) $$$ { vid; typ }
  in
  Dep.Graph.add_node ~taint:true graph value;
  value

(* Binary operators *)

let in_binop (graph : Dep.Graph.t) (binop : P4.binop) : value =
  let mixop =
    match binop.it with
    | PlusOp -> [ [ in_atom "PLUS" ] ]
    | SPlusOp -> [ [ in_atom "SPLUS" ] ]
    | MinusOp -> [ [ in_atom "MINUS" ] ]
    | SMinusOp -> [ [ in_atom "SMINUS" ] ]
    | MulOp -> [ [ in_atom "MUL" ] ]
    | DivOp -> [ [ in_atom "DIV" ] ]
    | ModOp -> [ [ in_atom "MOD" ] ]
    | ShlOp -> [ [ in_atom "SHL" ] ]
    | ShrOp -> [ [ in_atom "SHR" ] ]
    | LeOp -> [ [ in_atom "LE" ] ]
    | GeOp -> [ [ in_atom "GE" ] ]
    | LtOp -> [ [ in_atom "LT" ] ]
    | GtOp -> [ [ in_atom "GT" ] ]
    | EqOp -> [ [ in_atom "EQ" ] ]
    | NeOp -> [ [ in_atom "NE" ] ]
    | BAndOp -> [ [ in_atom "BAND" ] ]
    | BXorOp -> [ [ in_atom "BXOR" ] ]
    | BOrOp -> [ [ in_atom "BOR" ] ]
    | ConcatOp -> [ [ in_atom "CONCAT" ] ]
    | LAndOp -> [ [ in_atom "LAND" ] ]
    | LOrOp -> [ [ in_atom "LOR" ] ]
  in
  let value =
    let vid = Dep.Graph.fresh () in
    let typ = in_typ_var "binop" in
    CaseV (mixop, []) $$$ { vid; typ }
  in
  Dep.Graph.add_node ~taint:true graph value;
  value

(* Directions *)

let in_dir (graph : Dep.Graph.t) (dir : P4.dir) : value =
  let mixop =
    match dir.it with
    | No -> [ [ in_atom "NO" ] ]
    | In -> [ [ in_atom "IN" ] ]
    | Out -> [ [ in_atom "OUT" ] ]
    | InOut -> [ [ in_atom "INOUT" ] ]
  in
  let value =
    let vid = Dep.Graph.fresh () in
    let typ = in_typ_var "dir" in
    CaseV (mixop, []) $$$ { vid; typ }
  in
  Dep.Graph.add_node ~taint:true graph value;
  value

(* Types *)

let rec in_typ (graph : Dep.Graph.t) (typ : P4.typ) : value =
  let mixop, values =
    match typ.it with
    | VoidT ->
        let mixop = [ [ in_atom "VoidT" ] ] in
        (mixop, [])
    | ErrT ->
        let mixop = [ [ in_atom "ErrT" ] ] in
        (mixop, [])
    | MatchKindT ->
        let mixop = [ [ in_atom "MatchKindT" ] ] in
        (mixop, [])
    | StrT ->
        let mixop = [ [ in_atom "StrT" ] ] in
        (mixop, [])
    | BoolT ->
        let mixop = [ [ in_atom "BoolT" ] ] in
        (mixop, [])
    | IntT ->
        let mixop = [ [ in_atom "IntT" ] ] in
        (mixop, [])
    | FIntT expr ->
        let mixop = [ [ in_atom "FIntT" ]; [] ] in
        let value_expr = in_expr graph expr in
        (mixop, [ value_expr ])
    | FBitT expr ->
        let mixop = [ [ in_atom "FBitT" ]; [] ] in
        let value_expr = in_expr graph expr in
        (mixop, [ value_expr ])
    | VBitT expr ->
        let mixop = [ [ in_atom "VBitT" ]; [] ] in
        let value_expr = in_expr graph expr in
        (mixop, [ value_expr ])
    | StackT (typ, expr) ->
        let mixop = [ [ in_atom "StackT" ]; []; [] ] in
        let value_typ = in_typ graph typ in
        let value_expr = in_expr graph expr in
        (mixop, [ value_typ; value_expr ])
    | ListT typ ->
        let mixop = [ [ in_atom "ListT" ]; [] ] in
        let value_typ = in_typ graph typ in
        (mixop, [ value_typ ])
    | TupleT typs ->
        let mixop = [ [ in_atom "TupleT" ]; [] ] in
        let value_typs = in_typs graph typs in
        (mixop, [ value_typs ])
    | NameT var ->
        let mixop = [ [ in_atom "NameT" ]; [] ] in
        let value_var = in_var graph var in
        (mixop, [ value_var ])
    | SpecT (var, targs) ->
        let mixop = [ [ in_atom "SpecT" ]; []; [] ] in
        let value_var = in_var graph var in
        let value_targs = in_targs graph targs in
        (mixop, [ value_var; value_targs ])
    | AnyT ->
        let mixop = [ [ in_atom "AnyT" ] ] in
        (mixop, [])
  in
  let value =
    let vid = Dep.Graph.fresh () in
    let typ = in_typ_var "type" in
    CaseV (mixop, values) $$$ { vid; typ }
  in
  Dep.Graph.add_node ~taint:true graph value;
  value

and in_typs (graph : Dep.Graph.t) (typs : P4.typ list) : value =
  in_list in_typ (in_typ_var "type" $ no_region) graph typs

(* Type parameters *)

and in_tparam (graph : Dep.Graph.t) (tparam : P4.tparam) : value =
  let value =
    let vid = Dep.Graph.fresh () in
    let typ = in_typ_var "tparam" in
    TextV tparam.it $$$ { vid; typ }
  in
  Dep.Graph.add_node ~taint:true graph value;
  value

and in_tparams (graph : Dep.Graph.t) (tparams : P4.tparam list) : value =
  in_list in_tparam (in_typ_var "tparam" $ no_region) graph tparams

(* Parameters *)

and in_param (graph : Dep.Graph.t) (param : P4.param) : value =
  let id, dir, typ, expr_opt, _ = param.it in
  let mixop = [ []; []; []; []; [] ] in
  let value_id = in_id graph id in
  let value_dir = in_dir graph dir in
  let value_typ = in_typ graph typ in
  let value_expr_opt =
    in_opt in_expr (in_typ_var "expr" $ no_region) graph expr_opt
  in
  let value =
    let vid = Dep.Graph.fresh () in
    let typ = in_typ_var "param" in
    CaseV (mixop, [ value_id; value_dir; value_typ; value_expr_opt ])
    $$$ { vid; typ }
  in
  Dep.Graph.add_node ~taint:true graph value;
  value

and in_params (graph : Dep.Graph.t) (params : P4.param list) : value =
  in_list in_param (in_typ_var "param" $ no_region) graph params

(* Constructor parameters *)

and in_cparam (graph : Dep.Graph.t) (cparam : P4.cparam) : value =
  let id, dir, typ, expr_opt, _ = cparam.it in
  let mixop = [ []; []; []; []; [] ] in
  let value_id = in_id graph id in
  let value_dir = in_dir graph dir in
  let value_typ = in_typ graph typ in
  let value_expr_opt =
    in_opt in_expr (in_typ_var "expr" $ no_region) graph expr_opt
  in
  let value =
    let vid = Dep.Graph.fresh () in
    let typ = in_typ_var "cparam" in
    CaseV (mixop, [ value_id; value_dir; value_typ; value_expr_opt ])
    $$$ { vid; typ }
  in
  Dep.Graph.add_node ~taint:true graph value;
  value

and in_cparams (graph : Dep.Graph.t) (cparams : P4.cparam list) : value =
  in_list in_cparam (in_typ_var "cparam" $ no_region) graph cparams

(* Type arguments *)

and in_targ (graph : Dep.Graph.t) (targ : P4.targ) : value =
  let mixop, values =
    match targ.it with
    | VoidT ->
        let mixop = [ [ in_atom "VoidT" ] ] in
        (mixop, [])
    | ErrT ->
        let mixop = [ [ in_atom "ErrT" ] ] in
        (mixop, [])
    | MatchKindT ->
        let mixop = [ [ in_atom "MatchKindT" ] ] in
        (mixop, [])
    | StrT ->
        let mixop = [ [ in_atom "StrT" ] ] in
        (mixop, [])
    | BoolT ->
        let mixop = [ [ in_atom "BoolT" ] ] in
        (mixop, [])
    | IntT ->
        let mixop = [ [ in_atom "IntT" ] ] in
        (mixop, [])
    | FIntT expr ->
        let mixop = [ [ in_atom "FIntT" ]; [] ] in
        let value_expr = in_expr graph expr in
        (mixop, [ value_expr ])
    | FBitT expr ->
        let mixop = [ [ in_atom "FBitT" ]; [] ] in
        let value_expr = in_expr graph expr in
        (mixop, [ value_expr ])
    | VBitT expr ->
        let mixop = [ [ in_atom "VBitT" ]; [] ] in
        let value_expr = in_expr graph expr in
        (mixop, [ value_expr ])
    | StackT (typ, expr) ->
        let mixop = [ [ in_atom "StackT" ]; []; [] ] in
        let value_typ = in_targ graph typ in
        let value_expr = in_expr graph expr in
        (mixop, [ value_typ; value_expr ])
    | ListT typ ->
        let mixop = [ [ in_atom "ListT" ]; [] ] in
        let value_typ = in_targ graph typ in
        (mixop, [ value_typ ])
    | TupleT typs ->
        let mixop = [ [ in_atom "TupleT" ]; [] ] in
        let value_typs = in_targs graph typs in
        (mixop, [ value_typs ])
    | NameT var ->
        let mixop = [ [ in_atom "NameT" ]; [] ] in
        let value_var = in_var graph var in
        (mixop, [ value_var ])
    | SpecT (var, targs) ->
        let mixop = [ [ in_atom "SpecT" ]; []; [] ] in
        let value_var = in_var graph var in
        let value_targs = in_targs graph targs in
        (mixop, [ value_var; value_targs ])
    | AnyT ->
        let mixop = [ [ in_atom "AnyT" ] ] in
        (mixop, [])
  in
  let value =
    let vid = Dep.Graph.fresh () in
    let typ = in_typ_var "targ" in
    CaseV (mixop, values) $$$ { vid; typ }
  in
  Dep.Graph.add_node ~taint:true graph value;
  value

and in_targs (graph : Dep.Graph.t) (targs : P4.targ list) : value =
  in_list in_targ (in_typ_var "targ" $ no_region) graph targs

(* Arguments *)

and in_arg (graph : Dep.Graph.t) (arg : P4.arg) : value =
  let mixop, values =
    match arg.it with
    | ExprA expr ->
        let mixop = [ [ in_atom "ExprA" ]; [] ] in
        let value_expr = in_expr graph expr in
        (mixop, [ value_expr ])
    | NameA (id, expr_opt) ->
        let mixop = [ [ in_atom "NameA" ]; []; [] ] in
        let value_id = in_id graph id in
        let value_expr_opt =
          in_opt in_expr (in_typ_var "expr" $ no_region) graph expr_opt
        in
        (mixop, [ value_id; value_expr_opt ])
    | AnyA ->
        let mixop = [ [ in_atom "AnyA" ] ] in
        (mixop, [])
  in
  let value =
    let vid = Dep.Graph.fresh () in
    let typ = in_typ_var "arg" in
    CaseV (mixop, values) $$$ { vid; typ }
  in
  Dep.Graph.add_node ~taint:true graph value;
  value

and in_args (graph : Dep.Graph.t) (args : P4.arg list) : value =
  in_list in_arg (in_typ_var "arg" $ no_region) graph args

(* Expressions *)

and in_expr (graph : Dep.Graph.t) (expr : P4.expr) : value =
  let mixop, values =
    match expr.it with
    | BoolE { boolean } ->
        let mixop = [ [ in_atom "BoolE" ]; [] ] in
        let value_boolean = in_bool graph boolean in
        (mixop, [ value_boolean ])
    | StrE { text } ->
        let mixop = [ [ in_atom "StrE" ]; [] ] in
        let value_text = in_text graph text in
        (mixop, [ value_text ])
    | NumE { num } ->
        let mixop = [ [ in_atom "NumE" ]; [] ] in
        let value_num = in_num graph num in
        (mixop, [ value_num ])
    | VarE { var } ->
        let mixop = [ [ in_atom "NameE" ]; [] ] in
        let value_var = in_var graph var in
        (mixop, [ value_var ])
    | SeqE { exprs } ->
        let mixop = [ [ in_atom "SeqE" ]; [] ] in
        let value_exprs = in_exprs graph exprs in
        (mixop, [ value_exprs ])
    | SeqDefaultE { exprs } ->
        let mixop = [ [ in_atom "SeqDefaultE" ]; [] ] in
        let value_exprs = in_exprs graph exprs in
        (mixop, [ value_exprs ])
    | RecordE { fields } ->
        let mixop = [ [ in_atom "RecordE" ]; [] ] in
        let value_fields =
          let typ_member = in_typ_var "member" $ no_region in
          let typ_expr = in_typ_var "expr" $ no_region in
          let typ_pair = Il.Ast.TupleT [ typ_member; typ_expr ] $ no_region in
          in_list
            (in_pair in_member in_expr typ_member typ_expr)
            typ_pair graph fields
        in
        (mixop, [ value_fields ])
    | RecordDefaultE { fields } ->
        let mixop = [ [ in_atom "RecordDefaultE" ]; [] ] in
        let value_fields =
          let typ_member = in_typ_var "member" $ no_region in
          let typ_expr = in_typ_var "expr" $ no_region in
          let typ_pair = Il.Ast.TupleT [ typ_member; typ_expr ] $ no_region in
          in_list
            (in_pair in_member in_expr typ_member typ_expr)
            typ_pair graph fields
        in
        (mixop, [ value_fields ])
    | DefaultE ->
        let mixop = [ [ in_atom "DefaultE" ] ] in
        (mixop, [])
    | InvalidE ->
        let mixop = [ [ in_atom "InvalidE" ] ] in
        (mixop, [])
    | UnE { unop; expr } ->
        let mixop = [ [ in_atom "UnE" ]; []; [] ] in
        let value_unop = in_unop graph unop in
        let value_expr = in_expr graph expr in
        (mixop, [ value_unop; value_expr ])
    | BinE { binop; expr_l; expr_r } ->
        let mixop = [ [ in_atom "BinE" ]; []; []; [] ] in
        let value_binop = in_binop graph binop in
        let value_expr_l = in_expr graph expr_l in
        let value_expr_r = in_expr graph expr_r in
        (mixop, [ value_binop; value_expr_l; value_expr_r ])
    | TernE { expr_cond; expr_then; expr_else } ->
        let mixop = [ [ in_atom "TernE" ]; []; []; [] ] in
        let value_expr_cond = in_expr graph expr_cond in
        let value_expr_then = in_expr graph expr_then in
        let value_expr_else = in_expr graph expr_else in
        (mixop, [ value_expr_cond; value_expr_then; value_expr_else ])
    | CastE { typ; expr } ->
        let mixop = [ [ in_atom "CastE" ]; []; [] ] in
        let value_typ = in_typ graph typ in
        let value_expr = in_expr graph expr in
        (mixop, [ value_typ; value_expr ])
    | MaskE { expr_base; expr_mask } ->
        let mixop = [ [ in_atom "MaskE" ]; []; [] ] in
        let value_expr_base = in_expr graph expr_base in
        let value_expr_mask = in_expr graph expr_mask in
        (mixop, [ value_expr_base; value_expr_mask ])
    | RangeE { expr_lb; expr_ub } ->
        let mixop = [ [ in_atom "RangeE" ]; []; [] ] in
        let value_expr_lb = in_expr graph expr_lb in
        let value_expr_ub = in_expr graph expr_ub in
        (mixop, [ value_expr_lb; value_expr_ub ])
    | SelectE { exprs_select; cases } ->
        let mixop = [ [ in_atom "SelectE" ]; []; [] ] in
        let value_exprs_select = in_exprs graph exprs_select in
        let value_cases = in_select_cases graph cases in
        (mixop, [ value_exprs_select; value_cases ])
    | ArrAccE { expr_base; expr_idx } ->
        let mixop = [ [ in_atom "ArrAccE" ]; []; [] ] in
        let value_expr_base = in_expr graph expr_base in
        let value_expr_idx = in_expr graph expr_idx in
        (mixop, [ value_expr_base; value_expr_idx ])
    | BitAccE { expr_base; expr_lo; expr_hi } ->
        let mixop = [ [ in_atom "BitAccE" ]; []; []; [] ] in
        let value_expr_base = in_expr graph expr_base in
        let value_expr_lo = in_expr graph expr_lo in
        let value_expr_hi = in_expr graph expr_hi in
        (mixop, [ value_expr_base; value_expr_lo; value_expr_hi ])
    | ErrAccE { member } ->
        let mixop = [ [ in_atom "ErrAccE" ]; [] ] in
        let value_member = in_member graph member in
        (mixop, [ value_member ])
    | TypeAccE { var_base; member } ->
        let mixop = [ [ in_atom "TypeAccE" ]; []; [] ] in
        let value_var_base = in_var graph var_base in
        let value_member = in_member graph member in
        (mixop, [ value_var_base; value_member ])
    | ExprAccE { expr_base; member } ->
        let mixop = [ [ in_atom "ExprAccE" ]; []; [] ] in
        let value_expr_base = in_expr graph expr_base in
        let value_member = in_member graph member in
        (mixop, [ value_expr_base; value_member ])
    | CallFuncE { var_func; targs; args } ->
        let mixop = [ [ in_atom "CallFuncE" ]; []; []; [] ] in
        let value_var_func = in_var graph var_func in
        let value_targs = in_targs graph targs in
        let value_args = in_args graph args in
        (mixop, [ value_var_func; value_targs; value_args ])
    | CallMethodE { expr_base; member; targs; args } ->
        let mixop = [ [ in_atom "CallMethodE" ]; []; []; []; [] ] in
        let value_expr_base = in_expr graph expr_base in
        let value_member = in_member graph member in
        let value_targs = in_targs graph targs in
        let value_args = in_args graph args in
        (mixop, [ value_expr_base; value_member; value_targs; value_args ])
    | CallTypeE { var_typ; member; targs; args } ->
        let mixop = [ [ in_atom "CallTypeE" ]; []; []; []; [] ] in
        let value_var_typ = in_var graph var_typ in
        let value_member = in_member graph member in
        let value_targs = in_targs graph targs in
        let value_args = in_args graph args in
        (mixop, [ value_var_typ; value_member; value_targs; value_args ])
    | InstE { var_inst; targs; args } ->
        let mixop = [ [ in_atom "InstE" ]; []; []; [] ] in
        let value_var_inst = in_var graph var_inst in
        let value_targs = in_targs graph targs in
        let value_args = in_args graph args in
        (mixop, [ value_var_inst; value_targs; value_args ])
  in
  let value =
    let vid = Dep.Graph.fresh () in
    let typ = in_typ_var "expr" in
    CaseV (mixop, values) $$$ { vid; typ }
  in
  Dep.Graph.add_node ~taint:true graph value;
  value

and in_exprs (graph : Dep.Graph.t) (exprs : P4.expr list) : value =
  in_list in_expr (in_typ_var "expr" $ no_region) graph exprs

(* Keyset expressions *)

and in_keyset (graph : Dep.Graph.t) (keyset : P4.keyset) : value =
  let mixop, values =
    match keyset.it with
    | ExprK expr ->
        let mixop = [ [ in_atom "ExprK" ]; [] ] in
        let value_expr = in_expr graph expr in
        (mixop, [ value_expr ])
    | DefaultK ->
        let mixop = [ [ in_atom "DefaultK" ] ] in
        (mixop, [])
    | AnyK ->
        let mixop = [ [ in_atom "AnyK" ] ] in
        (mixop, [])
  in
  let value =
    let vid = Dep.Graph.fresh () in
    let typ = in_typ_var "keyset" in
    CaseV (mixop, values) $$$ { vid; typ }
  in
  Dep.Graph.add_node ~taint:true graph value;
  value

and in_keysets (graph : Dep.Graph.t) (keysets : P4.keyset list) : value =
  in_list in_keyset (in_typ_var "keyset" $ no_region) graph keysets

(* Select-cases for select *)

and in_select_case (graph : Dep.Graph.t) (select_case : P4.select_case) : value
    =
  let keysets, state_label = select_case.it in
  let mixop = [ []; []; [] ] in
  let value_keysets = in_keysets graph keysets in
  let value_state_label = in_state_label graph state_label in
  let value =
    let vid = Dep.Graph.fresh () in
    let typ = in_typ_var "selectcase" in
    CaseV (mixop, [ value_keysets; value_state_label ]) $$$ { vid; typ }
  in
  Dep.Graph.add_node ~taint:true graph value;
  value

and in_select_cases (graph : Dep.Graph.t) (select_cases : P4.select_case list) :
    value =
  in_list in_select_case
    (in_typ_var "selectcase" $ no_region)
    graph select_cases

(* Statements *)

and in_stmt (graph : Dep.Graph.t) (stmt : P4.stmt) : value =
  let mixop, values =
    match stmt.it with
    | EmptyS ->
        let mixop = [ [ in_atom "EmptyS" ] ] in
        (mixop, [])
    | AssignS { expr_l; expr_r } ->
        let mixop = [ [ in_atom "AssignS" ]; []; [] ] in
        let value_expr_l = in_expr graph expr_l in
        let value_expr_r = in_expr graph expr_r in
        (mixop, [ value_expr_l; value_expr_r ])
    | SwitchS { expr_switch; cases } ->
        let mixop = [ [ in_atom "SwitchS" ]; []; [] ] in
        let value_expr_switch = in_expr graph expr_switch in
        let value_cases = in_switch_cases graph cases in
        (mixop, [ value_expr_switch; value_cases ])
    | IfS { expr_cond; stmt_then; stmt_else } ->
        let mixop = [ [ in_atom "IfS" ]; []; []; [] ] in
        let value_expr_cond = in_expr graph expr_cond in
        let value_stmt_then = in_stmt graph stmt_then in
        let value_stmt_else = in_stmt graph stmt_else in
        (mixop, [ value_expr_cond; value_stmt_then; value_stmt_else ])
    | BlockS { block } ->
        let mixop = [ [ in_atom "BlockS" ]; [] ] in
        let value_block = in_block graph block in
        (mixop, [ value_block ])
    | ExitS ->
        let mixop = [ [ in_atom "ExitS" ] ] in
        (mixop, [])
    | RetS { expr_ret } ->
        let mixop = [ [ in_atom "RetS" ]; [] ] in
        let value_expr_ret =
          in_opt in_expr (in_typ_var "expr" $ no_region) graph expr_ret
        in
        (mixop, [ value_expr_ret ])
    | CallFuncS { var_func; targs; args } ->
        let mixop = [ [ in_atom "CallFuncS" ]; []; []; [] ] in
        let value_var_func = in_var graph var_func in
        let value_targs = in_targs graph targs in
        let value_args = in_args graph args in
        (mixop, [ value_var_func; value_targs; value_args ])
    | CallMethodS { expr_base; member; targs; args } ->
        let mixop = [ [ in_atom "CallMethodS" ]; []; []; []; [] ] in
        let value_expr_base = in_expr graph expr_base in
        let value_member = in_member graph member in
        let value_targs = in_targs graph targs in
        let value_args = in_args graph args in
        (mixop, [ value_expr_base; value_member; value_targs; value_args ])
    | CallInstS { var_inst; targs; args } ->
        let mixop = [ [ in_atom "CallInstS" ]; []; []; [] ] in
        let value_var_inst = in_var graph var_inst in
        let value_targs = in_targs graph targs in
        let value_args = in_args graph args in
        (mixop, [ value_var_inst; value_targs; value_args ])
    | TransS { expr_label } ->
        let mixop = [ [ in_atom "TransS" ]; [] ] in
        let value_expr_label = in_expr graph expr_label in
        (mixop, [ value_expr_label ])
    | DeclS { decl } ->
        let mixop = [ [ in_atom "DeclS" ]; [] ] in
        let value_decl = in_decl graph decl in
        (mixop, [ value_decl ])
  in
  let value =
    let vid = Dep.Graph.fresh () in
    let typ = in_typ_var "stmt" in
    CaseV (mixop, values) $$$ { vid; typ }
  in
  Dep.Graph.add_node ~taint:true graph value;
  value

and in_stmts (graph : Dep.Graph.t) (stmts : P4.stmt list) : value =
  in_list in_stmt (in_typ_var "stmt" $ no_region) graph stmts

(* Blocks (sequence of statements) *)

and in_block (graph : Dep.Graph.t) (block : P4.block) : value =
  let stmts, _ = block.it in
  let mixop = [ [ in_atom "BlockB" ]; [] ] in
  let value_stmts = in_stmts graph stmts in
  let value =
    let vid = Dep.Graph.fresh () in
    let typ = in_typ_var "block" in
    CaseV (mixop, [ value_stmts ]) $$$ { vid; typ }
  in
  Dep.Graph.add_node ~taint:true graph value;
  value

(* Match-cases for switch *)

and in_switch_label (graph : Dep.Graph.t) (switch_label : P4.switch_label) :
    value =
  let mixop, values =
    match switch_label.it with
    | ExprL expr ->
        let mixop = [ [ in_atom "ExprL" ]; [] ] in
        let value_expr = in_expr graph expr in
        (mixop, [ value_expr ])
    | DefaultL ->
        let mixop = [ [ in_atom "DefaultL" ] ] in
        (mixop, [])
  in
  let value =
    let vid = Dep.Graph.fresh () in
    let typ = in_typ_var "switchlabel" in
    CaseV (mixop, values) $$$ { vid; typ }
  in
  Dep.Graph.add_node ~taint:true graph value;
  value

and in_switch_case (graph : Dep.Graph.t) (switch_case : P4.switch_case) : value
    =
  let mixop, values =
    match switch_case.it with
    | MatchC (switch_label, block) ->
        let mixop = [ [ in_atom "MatchC" ]; []; [] ] in
        let value_switch_label = in_switch_label graph switch_label in
        let value_block = in_block graph block in
        (mixop, [ value_switch_label; value_block ])
    | FallC switch_label ->
        let mixop = [ [ in_atom "FallC" ]; [] ] in
        let value_switch_label = in_switch_label graph switch_label in
        (mixop, [ value_switch_label ])
  in
  let value =
    let vid = Dep.Graph.fresh () in
    let typ = in_typ_var "switchcase" in
    CaseV (mixop, values) $$$ { vid; typ }
  in
  Dep.Graph.add_node ~taint:true graph value;
  value

and in_switch_cases (graph : Dep.Graph.t) (switch_cases : P4.switch_case list) :
    value =
  in_list in_switch_case
    (in_typ_var "switchcase" $ no_region)
    graph switch_cases

(* Declarations *)

and in_typdef (graph : Dep.Graph.t) (typdef : (P4.typ, P4.decl) P4.alt) : value
    =
  let mixop, values =
    match typdef with
    | Left typ ->
        let mixop = [ [ in_atom "TypeD" ]; [] ] in
        let value_typ = in_typ graph typ in
        (mixop, [ value_typ ])
    | Right decl ->
        let mixop = [ [ in_atom "DeclD" ]; [] ] in
        let value_decl = in_decl graph decl in
        (mixop, [ value_decl ])
  in
  let value =
    let vid = Dep.Graph.fresh () in
    let typ = in_typ_var "typedef" in
    CaseV (mixop, values) $$$ { vid; typ }
  in
  Dep.Graph.add_node ~taint:true graph value;
  value

and in_decl (graph : Dep.Graph.t) (decl : P4.decl) : value =
  let mixop, values =
    match decl.it with
    | ConstD { id; typ; value; _ } ->
        let mixop = [ [ in_atom "ConstD" ]; []; []; [] ] in
        let value_id = in_id graph id in
        let value_typ = in_typ graph typ in
        let value_value = in_expr graph value in
        (mixop, [ value_id; value_typ; value_value ])
    | VarD { id; typ; init; _ } ->
        let mixop = [ [ in_atom "VarD" ]; []; []; [] ] in
        let value_id = in_id graph id in
        let value_typ = in_typ graph typ in
        let value_init =
          in_opt in_expr (in_typ_var "expr" $ no_region) graph init
        in
        (mixop, [ value_id; value_typ; value_init ])
    | ErrD { members } ->
        let mixop = [ [ in_atom "ErrD" ]; [] ] in
        let value_members = in_members graph members in
        (mixop, [ value_members ])
    | MatchKindD { members } ->
        let mixop = [ [ in_atom "MatchKindD" ]; [] ] in
        let value_members = in_members graph members in
        (mixop, [ value_members ])
    | InstD { id; var_inst; targs; args; init; _ } ->
        let mixop = [ [ in_atom "InstD" ]; []; []; []; []; [] ] in
        let value_id = in_id graph id in
        let value_var_inst = in_var graph var_inst in
        let value_targs = in_targs graph targs in
        let value_args = in_args graph args in
        let value_init = in_decls graph init in
        ( mixop,
          [ value_id; value_var_inst; value_targs; value_args; value_init ] )
    | StructD { id; tparams; fields; _ } ->
        let mixop = [ [ in_atom "StructD" ]; []; []; [] ] in
        let value_id = in_id graph id in
        let value_tparams = in_tparams graph tparams in
        let value_fields =
          let fields =
            List.map (fun (member, typ, _) -> (member, typ)) fields
          in
          let typ_member = in_typ_var "member" $ no_region in
          let typ_typ = in_typ_var "type" $ no_region in
          let typ_pair = Il.Ast.TupleT [ typ_member; typ_typ ] $ no_region in
          in_list
            (in_pair in_member in_typ typ_member typ_typ)
            typ_pair graph fields
        in
        (mixop, [ value_id; value_tparams; value_fields ])
    | HeaderD { id; tparams; fields; _ } ->
        let mixop = [ [ in_atom "HeaderD" ]; []; []; [] ] in
        let value_id = in_id graph id in
        let value_tparams = in_tparams graph tparams in
        let value_fields =
          let fields =
            List.map (fun (member, typ, _) -> (member, typ)) fields
          in
          let typ_member = in_typ_var "member" $ no_region in
          let typ_typ = in_typ_var "type" $ no_region in
          let typ_pair = Il.Ast.TupleT [ typ_member; typ_typ ] $ no_region in
          in_list
            (in_pair in_member in_typ typ_member typ_typ)
            typ_pair graph fields
        in
        (mixop, [ value_id; value_tparams; value_fields ])
    | UnionD { id; tparams; fields; _ } ->
        let mixop = [ [ in_atom "UnionD" ]; []; []; [] ] in
        let value_id = in_id graph id in
        let value_tparams = in_tparams graph tparams in
        let value_fields =
          let fields =
            List.map (fun (member, typ, _) -> (member, typ)) fields
          in
          let typ_member = in_typ_var "member" $ no_region in
          let typ_typ = in_typ_var "type" $ no_region in
          let typ_pair = Il.Ast.TupleT [ typ_member; typ_typ ] $ no_region in
          in_list
            (in_pair in_member in_typ typ_member typ_typ)
            typ_pair graph fields
        in
        (mixop, [ value_id; value_tparams; value_fields ])
    | EnumD { id; members; _ } ->
        let mixop = [ [ in_atom "EnumD" ]; []; [] ] in
        let value_id = in_id graph id in
        let value_members = in_members graph members in
        (mixop, [ value_id; value_members ])
    | SEnumD { id; typ; fields; _ } ->
        let mixop = [ [ in_atom "SEnumD" ]; []; []; [] ] in
        let value_id = in_id graph id in
        let value_typ = in_typ graph typ in
        let value_fields =
          let typ_member = in_typ_var "member" $ no_region in
          let typ_expr = in_typ_var "expr" $ no_region in
          let typ_pair = Il.Ast.TupleT [ typ_member; typ_expr ] $ no_region in
          in_list
            (in_pair in_member in_expr typ_member typ_expr)
            typ_pair graph fields
        in
        (mixop, [ value_id; value_typ; value_fields ])
    | NewTypeD { id; typdef; _ } ->
        let mixop = [ [ in_atom "NewTypeD" ]; []; [] ] in
        let value_id = in_id graph id in
        let value_typdef = in_typdef graph typdef in
        (mixop, [ value_id; value_typdef ])
    | TypeDefD { id; typdef; _ } ->
        let mixop = [ [ in_atom "TypeDefD" ]; []; [] ] in
        let value_id = in_id graph id in
        let value_typdef = in_typdef graph typdef in
        (mixop, [ value_id; value_typdef ])
    | ValueSetD { id; typ; size; _ } ->
        let mixop = [ [ in_atom "ValueSetD" ]; []; []; [] ] in
        let value_id = in_id graph id in
        let value_typ = in_typ graph typ in
        let value_size = in_expr graph size in
        (mixop, [ value_id; value_typ; value_size ])
    | ParserTypeD { id; tparams; params; _ } ->
        let mixop = [ [ in_atom "ParserTypeD" ]; []; []; [] ] in
        let value_id = in_id graph id in
        let value_tparams = in_tparams graph tparams in
        let value_params = in_params graph params in
        (mixop, [ value_id; value_tparams; value_params ])
    | ParserD { id; params; cparams; locals; states; _ } ->
        let mixop = [ [ in_atom "ParserD" ]; []; []; []; []; [] ] in
        let value_id = in_id graph id in
        let value_params = in_params graph params in
        let value_cparams = in_cparams graph cparams in
        let value_locals = in_decls graph locals in
        let value_states = in_parser_states graph states in
        ( mixop,
          [ value_id; value_params; value_cparams; value_locals; value_states ]
        )
    | TableD { id; table; _ } ->
        let mixop = [ [ in_atom "TableD" ]; []; [] ] in
        let value_id = in_id graph id in
        let value_table = in_table graph table in
        (mixop, [ value_id; value_table ])
    | ControlTypeD { id; tparams; params; _ } ->
        let mixop = [ [ in_atom "ControlTypeD" ]; []; []; [] ] in
        let value_id = in_id graph id in
        let value_tparams = in_tparams graph tparams in
        let value_params = in_params graph params in
        (mixop, [ value_id; value_tparams; value_params ])
    | ControlD { id; params; cparams; locals; body; _ } ->
        let mixop = [ [ in_atom "ControlD" ]; []; []; []; []; [] ] in
        let value_id = in_id graph id in
        let value_params = in_params graph params in
        let value_cparams = in_cparams graph cparams in
        let value_locals = in_decls graph locals in
        let value_body = in_block graph body in
        ( mixop,
          [ value_id; value_params; value_cparams; value_locals; value_body ] )
    | ActionD { id; params; body; _ } ->
        let mixop = [ [ in_atom "ActionD" ]; []; []; [] ] in
        let value_id = in_id graph id in
        let value_params = in_params graph params in
        let value_body = in_block graph body in
        (mixop, [ value_id; value_params; value_body ])
    | FuncD { id; typ_ret; tparams; params; body; _ } ->
        let mixop = [ [ in_atom "FuncD" ]; []; []; []; []; [] ] in
        let value_id = in_id graph id in
        let value_typ_ret = in_typ graph typ_ret in
        let value_tparams = in_tparams graph tparams in
        let value_params = in_params graph params in
        let value_body = in_block graph body in
        ( mixop,
          [ value_id; value_typ_ret; value_tparams; value_params; value_body ]
        )
    | ExternFuncD { id; typ_ret; tparams; params; _ } ->
        let mixop = [ [ in_atom "ExternFuncD" ]; []; []; []; [] ] in
        let value_id = in_id graph id in
        let value_typ_ret = in_typ graph typ_ret in
        let value_tparams = in_tparams graph tparams in
        let value_params = in_params graph params in
        (mixop, [ value_id; value_typ_ret; value_tparams; value_params ])
    | ExternObjectD { id; tparams; mthds; _ } ->
        let mixop = [ [ in_atom "ExternObjectD" ]; []; []; [] ] in
        let value_id = in_id graph id in
        let value_tparams = in_tparams graph tparams in
        let value_mthds = in_mthds graph mthds in
        (mixop, [ value_id; value_tparams; value_mthds ])
    | PackageTypeD { id; tparams; cparams; _ } ->
        let mixop = [ [ in_atom "PackageTypeD" ]; []; []; [] ] in
        let value_id = in_id graph id in
        let value_tparams = in_tparams graph tparams in
        let value_cparams = in_cparams graph cparams in
        (mixop, [ value_id; value_tparams; value_cparams ])
  in
  let value =
    let vid = Dep.Graph.fresh () in
    let typ = in_typ_var "decl" in
    CaseV (mixop, values) $$$ { vid; typ }
  in
  Dep.Graph.add_node ~taint:true graph value;
  value

and in_decls (graph : Dep.Graph.t) (decls : P4.decl list) : value =
  in_list in_decl (in_typ_var "decl" $ no_region) graph decls

(* Parser state machine *)

and in_parser_state (graph : Dep.Graph.t) (parser_state : P4.parser_state) :
    value =
  let state_label, block, _ = parser_state.it in
  let mixop = [ []; []; [] ] in
  let value_state_label = in_state_label graph state_label in
  let value_block = in_block graph block in
  let value =
    let vid = Dep.Graph.fresh () in
    let typ = in_typ_var "parserstate" in
    CaseV (mixop, [ value_state_label; value_block ]) $$$ { vid; typ }
  in
  Dep.Graph.add_node ~taint:true graph value;
  value

and in_parser_states (graph : Dep.Graph.t)
    (parser_states : P4.parser_state list) : value =
  in_list in_parser_state
    (in_typ_var "parserstate" $ no_region)
    graph parser_states

(* Tables *)

and in_table (graph : Dep.Graph.t) (table : P4.table) : value =
  in_list in_table_property (in_typ_var "tblprop" $ no_region) graph table

(* Table properties *)

and in_table_property (graph : Dep.Graph.t) (table_property : P4.table_property)
    : value =
  let mixop, values =
    match table_property with
    | KeyP table_keys ->
        let mixop = [ [ in_atom "KeyP" ]; [] ] in
        let value_table_keys = in_table_keys graph table_keys in
        (mixop, [ value_table_keys ])
    | ActionP table_actions ->
        let mixop = [ [ in_atom "ActionP" ]; [] ] in
        let value_table_actions = in_table_actions graph table_actions in
        (mixop, [ value_table_actions ])
    | EntryP table_entries ->
        let mixop = [ [ in_atom "EntryP" ]; [] ] in
        let value_table_entries = in_table_entries graph table_entries in
        (mixop, [ value_table_entries ])
    | DefaultP table_default ->
        let mixop = [ [ in_atom "DefaultP" ]; [] ] in
        let value_table_default = in_table_default graph table_default in
        (mixop, [ value_table_default ])
    | CustomP table_custom ->
        let mixop = [ [ in_atom "CustomP" ]; [] ] in
        let value_table_custom = in_table_custom graph table_custom in
        (mixop, [ value_table_custom ])
  in
  let value =
    let vid = Dep.Graph.fresh () in
    let typ = in_typ_var "tblprop" in
    CaseV (mixop, values) $$$ { vid; typ }
  in
  Dep.Graph.add_node ~taint:true graph value;
  value

(* Table keys *)

and in_table_key (graph : Dep.Graph.t) (table_key : P4.table_key) : value =
  let expr, match_kind, _ = table_key.it in
  let mixop = [ []; []; [] ] in
  let value_expr = in_expr graph expr in
  let value_match_kind = in_match_kind graph match_kind in
  let value =
    let vid = Dep.Graph.fresh () in
    let typ = in_typ_var "tblkey" in
    CaseV (mixop, [ value_expr; value_match_kind ]) $$$ { vid; typ }
  in
  Dep.Graph.add_node ~taint:true graph value;
  value

and in_table_keys (graph : Dep.Graph.t) (table_keys : P4.table_keys) : value =
  in_list in_table_key (in_typ_var "tblkey" $ no_region) graph table_keys.it

(* Table action references *)

and in_table_action (graph : Dep.Graph.t) (table_action : P4.table_action) :
    value =
  let var, args, _ = table_action.it in
  let mixop = [ []; []; [] ] in
  let value_var = in_var graph var in
  let value_args = in_args graph args in
  let value =
    let vid = Dep.Graph.fresh () in
    let typ = in_typ_var "tblaction" in
    CaseV (mixop, [ value_var; value_args ]) $$$ { vid; typ }
  in
  Dep.Graph.add_node ~taint:true graph value;
  value

and in_table_actions (graph : Dep.Graph.t) (table_actions : P4.table_actions) :
    value =
  in_list in_table_action
    (in_typ_var "tblaction" $ no_region)
    graph table_actions.it

(* Table entries *)

and in_table_entry (graph : Dep.Graph.t) (table_entry : P4.table_entry) : value
    =
  let table_entry_const, keysets, table_action, expr_opt, _ = table_entry.it in
  let mixop = [ []; []; []; []; [] ] in
  let value_table_entry_const = in_bool graph table_entry_const in
  let value_keysets = in_keysets graph keysets in
  let value_table_action = in_table_action graph table_action in
  let value_expr_opt =
    in_opt in_expr (in_typ_var "expr" $ no_region) graph expr_opt
  in
  let value =
    let vid = Dep.Graph.fresh () in
    let typ = in_typ_var "tblentry" in
    CaseV
      ( mixop,
        [
          value_table_entry_const;
          value_keysets;
          value_table_action;
          value_expr_opt;
        ] )
    $$$ { vid; typ }
  in
  Dep.Graph.add_node ~taint:true graph value;
  value

and in_table_entries (graph : Dep.Graph.t) (table_entries : P4.table_entries) :
    value =
  let table_entries_const, table_entries = table_entries.it in
  let mixop = [ []; []; [] ] in
  let value_table_entries_const = in_bool graph table_entries_const in
  let value_table_entries =
    in_list in_table_entry
      (in_typ_var "tblentry" $ no_region)
      graph table_entries
  in
  let value =
    let vid = Dep.Graph.fresh () in
    let typ = in_typ_var "tblentryprop" in
    CaseV (mixop, [ value_table_entries_const; value_table_entries ])
    $$$ { vid; typ }
  in
  Dep.Graph.add_node ~taint:true graph value;
  value

(* Table default properties *)

and in_table_default (graph : Dep.Graph.t) (table_default : P4.table_default) :
    value =
  let table_default_const, table_action = table_default.it in
  let mixop = [ []; []; [] ] in
  let value_table_default_const = in_bool graph table_default_const in
  let value_table_action = in_table_action graph table_action in
  let value =
    let vid = Dep.Graph.fresh () in
    let typ = in_typ_var "tbldefaultprop" in
    CaseV (mixop, [ value_table_default_const; value_table_action ])
    $$$ { vid; typ }
  in
  Dep.Graph.add_node ~taint:true graph value;
  value

(* Table custom properties *)

and in_table_custom (graph : Dep.Graph.t) (table_custom : P4.table_custom) :
    value =
  let table_custom_const, member, expr, _ = table_custom.it in
  let mixop = [ []; []; []; [] ] in
  let value_table_custom_const = in_bool graph table_custom_const in
  let value_member = in_member graph member in
  let value_expr = in_expr graph expr in
  let value =
    let vid = Dep.Graph.fresh () in
    let typ = in_typ_var "tblcustomprop" in
    CaseV (mixop, [ value_table_custom_const; value_member; value_expr ])
    $$$ { vid; typ }
  in
  Dep.Graph.add_node ~taint:true graph value;
  value

(* Methods *)

and in_mthd (graph : Dep.Graph.t) (mthd : P4.mthd) : value =
  let mixop, values =
    match mthd.it with
    | ExternConsM { id; cparams; _ } ->
        let mixop = [ [ in_atom "ExternConsM" ]; []; [] ] in
        let value_id = in_id graph id in
        let value_cparams = in_cparams graph cparams in
        (mixop, [ value_id; value_cparams ])
    | ExternAbstractM { id; typ_ret; tparams; params; _ } ->
        let mixop = [ [ in_atom "ExternAbstractM" ]; []; []; []; [] ] in
        let value_id = in_id graph id in
        let value_typ_ret = in_typ graph typ_ret in
        let value_tparams = in_tparams graph tparams in
        let value_params = in_params graph params in
        (mixop, [ value_id; value_typ_ret; value_tparams; value_params ])
    | ExternM { id; typ_ret; tparams; params; _ } ->
        let mixop = [ [ in_atom "ExternM" ]; []; []; []; [] ] in
        let value_id = in_id graph id in
        let value_typ_ret = in_typ graph typ_ret in
        let value_tparams = in_tparams graph tparams in
        let value_params = in_params graph params in
        (mixop, [ value_id; value_typ_ret; value_tparams; value_params ])
  in
  let value =
    let vid = Dep.Graph.fresh () in
    let typ = in_typ_var "method" in
    CaseV (mixop, values) $$$ { vid; typ }
  in
  Dep.Graph.add_node ~taint:true graph value;
  value

and in_mthds (graph : Dep.Graph.t) (mthds : P4.mthd list) : value =
  in_list in_mthd (in_typ_var "method" $ no_region) graph mthds

(* Program *)

let in_program (graph : Dep.Graph.t) (includes_p4 : string list)
    (filename_p4 : string) : value =
  try
    P4frontend.Parse.parse_file includes_p4 filename_p4
    |> in_list in_decl (in_typ_var "decl" $ no_region) graph
  with P4util.Error.ParseErr (msg, _) -> error_convert_in msg
