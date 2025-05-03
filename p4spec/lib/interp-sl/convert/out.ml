module P4Lang = P4lang.Ast
module P4El = P4el.Ast
open Sl.Ast
module F = Format
open Util.Error
open Util.Source

(* Conversion from IL value to p4cherry AST *)

(* Error handling *)

let error (category : string) (value : value) =
  F.asprintf "expected a(an) %s, but got %s" category
    (Sl.Print.string_of_value value)
  |> error_convert_out

(* Helpers *)

let no_info = P4util.Source.no_info
let ( $ ) = P4util.Source.( $ )

let out_opt (do_out : value -> 'a) (value_opt : value) : 'a option =
  match value_opt.it with
  | OptV (Some value) -> Some (do_out value)
  | OptV None -> None
  | _ -> error "option" value_opt

let out_list (do_out : value -> 'a) (value_list : value) : 'a list =
  match value_list.it with
  | ListV values -> List.map do_out values
  | _ -> error "list" value_list

let out_pair (do_out_a : value -> 'a) (do_out_b : value -> 'b)
    (value_pair : value) : 'a * 'b =
  match value_pair.it with
  | TupleV [ value_a; value_b ] -> (do_out_a value_a, do_out_b value_b)
  | _ -> error "tuple" value_pair

(* Booleans *)

let out_bool (value_boolean : value) : bool =
  match value_boolean.it with
  | BoolV boolean -> boolean
  | _ -> error "bool" value_boolean

(* Numbers *)

let out_num (value_num : value) : P4El.num =
  match value_num.it with
  | CaseV ([ [ { it = Atom "FINT"; _ } ]; []; [] ], [ value_width; value_int ])
    ->
      let width =
        match value_width.it with
        | NumV (`Nat width) -> width
        | _ -> error "nat" value_width
      in
      let i =
        match value_int.it with
        | NumV (`Int i) -> i
        | _ -> error "int" value_int
      in
      (i, Some (width, true)) $ no_info
  | CaseV ([ [ { it = Atom "FBIT"; _ } ]; []; [] ], [ value_width; value_int ])
    ->
      let width =
        match value_width.it with
        | NumV (`Nat width) -> width
        | _ -> error "nat" value_width
      in
      let i =
        match value_int.it with
        | NumV (`Int i) -> i
        | _ -> error "int" value_int
      in
      (i, Some (width, false)) $ no_info
  | CaseV ([ [ { it = Atom "INT"; _ } ]; [] ], [ value_int ]) ->
      let i =
        match value_int.it with
        | NumV (`Int i) -> i
        | _ -> error "int" value_int
      in
      (i, None) $ no_info
  | _ -> error "num" value_num

(* Texts *)

let out_text (value_text : value) : P4El.text =
  match value_text.it with
  | TextV s -> s $ no_info
  | _ -> error "text" value_text

(* Identifiers *)

let out_id (value_id : value) : P4El.id =
  match value_id.it with TextV s -> s $ no_info | _ -> error "id" value_id

(* Variables (scoped identifiers) *)

let out_var (value_var : value) : P4El.var =
  match value_var.it with
  | CaseV ([ [ { it = Atom "TOP"; _ } ]; [] ], [ value_id ]) ->
      let id = out_id value_id in
      P4Lang.Top id $ no_info
  | CaseV ([ [ { it = Atom "CURRENT"; _ } ]; [] ], [ value_id ]) ->
      let id = out_id value_id in
      P4Lang.Current id $ no_info
  | _ -> error "var" value_var

(* Members *)

let out_member (value_member : value) : P4El.member =
  match value_member.it with
  | TextV s -> s $ no_info
  | _ -> error "member" value_member

let out_members (value_members : value) : P4El.member list =
  out_list out_member value_members

(* Match kinds *)

let out_match_kind (value_match_kind : value) : P4El.match_kind =
  match value_match_kind.it with
  | TextV s -> s $ no_info
  | _ -> error "match_kind" value_match_kind

(* State labels *)

let out_state_label (value_state_label : value) : P4El.state_label =
  match value_state_label.it with
  | TextV s -> s $ no_info
  | _ -> error "state_label" value_state_label

(* Unary operators *)

let out_unop (value_unop : value) : P4El.unop =
  match value_unop.it with
  | CaseV ([ [ { it = Atom "BNOT"; _ } ] ], []) -> P4Lang.BNotOp $ no_info
  | CaseV ([ [ { it = Atom "LNOT"; _ } ] ], []) -> P4Lang.LNotOp $ no_info
  | CaseV ([ [ { it = Atom "UPLUS"; _ } ] ], []) -> P4Lang.UPlusOp $ no_info
  | CaseV ([ [ { it = Atom "UMINUS"; _ } ] ], []) -> P4Lang.UMinusOp $ no_info
  | _ -> error "unop" value_unop

(* Binary operators *)

let out_binop (value_binop : value) : P4El.binop =
  match value_binop.it with
  | CaseV ([ [ { it = Atom "PLUS"; _ } ] ], []) -> P4Lang.PlusOp $ no_info
  | CaseV ([ [ { it = Atom "SPLUS"; _ } ] ], []) -> P4Lang.SPlusOp $ no_info
  | CaseV ([ [ { it = Atom "MINUS"; _ } ] ], []) -> P4Lang.MinusOp $ no_info
  | CaseV ([ [ { it = Atom "SMINUS"; _ } ] ], []) -> P4Lang.SMinusOp $ no_info
  | CaseV ([ [ { it = Atom "MUL"; _ } ] ], []) -> P4Lang.MulOp $ no_info
  | CaseV ([ [ { it = Atom "DIV"; _ } ] ], []) -> P4Lang.DivOp $ no_info
  | CaseV ([ [ { it = Atom "MOD"; _ } ] ], []) -> P4Lang.ModOp $ no_info
  | CaseV ([ [ { it = Atom "SHL"; _ } ] ], []) -> P4Lang.ShlOp $ no_info
  | CaseV ([ [ { it = Atom "SHR"; _ } ] ], []) -> P4Lang.ShrOp $ no_info
  | CaseV ([ [ { it = Atom "LE"; _ } ] ], []) -> P4Lang.LeOp $ no_info
  | CaseV ([ [ { it = Atom "GE"; _ } ] ], []) -> P4Lang.GeOp $ no_info
  | CaseV ([ [ { it = Atom "LT"; _ } ] ], []) -> P4Lang.LtOp $ no_info
  | CaseV ([ [ { it = Atom "GT"; _ } ] ], []) -> P4Lang.GtOp $ no_info
  | CaseV ([ [ { it = Atom "EQ"; _ } ] ], []) -> P4Lang.EqOp $ no_info
  | CaseV ([ [ { it = Atom "NE"; _ } ] ], []) -> P4Lang.NeOp $ no_info
  | CaseV ([ [ { it = Atom "BAND"; _ } ] ], []) -> P4Lang.BAndOp $ no_info
  | CaseV ([ [ { it = Atom "BXOR"; _ } ] ], []) -> P4Lang.BXorOp $ no_info
  | CaseV ([ [ { it = Atom "BOR"; _ } ] ], []) -> P4Lang.BOrOp $ no_info
  | CaseV ([ [ { it = Atom "CONCAT"; _ } ] ], []) -> P4Lang.ConcatOp $ no_info
  | CaseV ([ [ { it = Atom "LAND"; _ } ] ], []) -> P4Lang.LAndOp $ no_info
  | CaseV ([ [ { it = Atom "LOR"; _ } ] ], []) -> P4Lang.LOrOp $ no_info
  | _ -> error "binop" value_binop

(* Directions *)

let out_dir (value_dir : value) : P4El.dir =
  match value_dir.it with
  | CaseV ([ [ { it = Atom "NO"; _ } ] ], []) -> P4Lang.No $ no_info
  | CaseV ([ [ { it = Atom "IN"; _ } ] ], []) -> P4Lang.In $ no_info
  | CaseV ([ [ { it = Atom "OUT"; _ } ] ], []) -> P4Lang.Out $ no_info
  | CaseV ([ [ { it = Atom "INOUT"; _ } ] ], []) -> P4Lang.InOut $ no_info
  | _ -> error "dir" value_dir

(* Types *)

let rec out_typ (value_typ : value) : P4El.typ =
  match value_typ.it with
  | CaseV ([ [ { it = Atom "VoidT"; _ } ] ], []) -> P4El.VoidT $ no_info
  | CaseV ([ [ { it = Atom "ErrT"; _ } ] ], []) -> P4El.ErrT $ no_info
  | CaseV ([ [ { it = Atom "MatchKindT"; _ } ] ], []) ->
      P4El.MatchKindT $ no_info
  | CaseV ([ [ { it = Atom "StrT"; _ } ] ], []) -> P4El.StrT $ no_info
  | CaseV ([ [ { it = Atom "BoolT"; _ } ] ], []) -> P4El.BoolT $ no_info
  | CaseV ([ [ { it = Atom "IntT"; _ } ] ], []) -> P4El.IntT $ no_info
  | CaseV ([ [ { it = Atom "FIntT"; _ } ]; [] ], [ value_expr ]) ->
      let expr = out_expr value_expr in
      P4El.FIntT expr $ no_info
  | CaseV ([ [ { it = Atom "FBitT"; _ } ]; [] ], [ value_expr ]) ->
      let expr = out_expr value_expr in
      P4El.FBitT expr $ no_info
  | CaseV ([ [ { it = Atom "VBitT"; _ } ]; [] ], [ value_expr ]) ->
      let expr = out_expr value_expr in
      P4El.VBitT expr $ no_info
  | CaseV ([ [ { it = Atom "StackT"; _ } ]; []; [] ], [ value_typ; value_expr ])
    ->
      let typ = out_typ value_typ in
      let expr = out_expr value_expr in
      P4El.StackT (typ, expr) $ no_info
  | CaseV ([ [ { it = Atom "ListT"; _ } ]; [] ], [ value_typ ]) ->
      let typ = out_typ value_typ in
      P4El.ListT typ $ no_info
  | CaseV ([ [ { it = Atom "TupleT"; _ } ]; [] ], [ value_typs ]) ->
      let typs = out_typs value_typs in
      P4El.TupleT typs $ no_info
  | CaseV ([ [ { it = Atom "NameT"; _ } ]; [] ], [ value_var ]) ->
      let var = out_var value_var in
      P4El.NameT var $ no_info
  | CaseV ([ [ { it = Atom "SpecT"; _ } ]; []; [] ], [ value_var; value_targs ])
    ->
      let var = out_var value_var in
      let targs = out_targs value_targs in
      P4El.SpecT (var, targs) $ no_info
  | CaseV ([ [ { it = Atom "AnyT"; _ } ] ], []) -> P4El.AnyT $ no_info
  | _ -> error "typ" value_typ

and out_typs (value_typs : value) : P4El.typ list = out_list out_typ value_typs

(* Type parameters *)

and out_tparam (value_tparam : value) : P4El.tparam =
  match value_tparam.it with
  | TextV s -> s $ no_info
  | _ -> error "tparam" value_tparam

and out_tparams (value_tparams : value) : P4El.tparam list =
  out_list out_tparam value_tparams

(* Parameters *)

and out_param (value_param : value) : P4El.param =
  match value_param.it with
  | CaseV
      ( [ []; []; []; []; [] ],
        [ value_id; value_dir; value_typ; value_expr_opt ] ) ->
      let id = out_id value_id in
      let dir = out_dir value_dir in
      let typ = out_typ value_typ in
      let expr_opt = out_opt out_expr value_expr_opt in
      (id, dir, typ, expr_opt, []) $ no_info
  | _ -> error "param" value_param

and out_params (value_params : value) : P4El.param list =
  out_list out_param value_params

(* Constructor parameters *)

and out_cparam (value_cparam : value) : P4El.cparam = out_param value_cparam

and out_cparams (value_cparams : value) : P4El.cparam list =
  out_list out_cparam value_cparams

(* Type arguments *)

and out_targ (value_targ : value) : P4El.targ = out_typ value_targ

and out_targs (value_targs : value) : P4El.targ list =
  out_list out_targ value_targs

(* Arguments *)

and out_arg (value_arg : value) : P4El.arg =
  match value_arg.it with
  | CaseV ([ [ { it = Atom "ExprA"; _ } ]; [] ], [ value_expr ]) ->
      let expr = out_expr value_expr in
      P4Lang.ExprA expr $ no_info
  | CaseV
      ([ [ { it = Atom "NameA"; _ } ]; []; [] ], [ value_id; value_expr_opt ])
    ->
      let id = out_id value_id in
      let expr_opt = out_opt out_expr value_expr_opt in
      P4Lang.NameA (id, expr_opt) $ no_info
  | CaseV ([ [ { it = Atom "AnyA"; _ } ] ], []) -> P4Lang.AnyA $ no_info
  | _ -> error "arg" value_arg

and out_args (value_args : value) : P4El.arg list = out_list out_arg value_args

(* Expressions *)

and out_expr (value_expr : value) : P4El.expr =
  match value_expr.it with
  | CaseV ([ [ { it = Atom "BoolE"; _ } ]; [] ], [ value_boolean ]) ->
      let boolean = out_bool value_boolean in
      P4El.BoolE { boolean } $ no_info
  | CaseV ([ [ { it = Atom "StrE"; _ } ]; [] ], [ value_text ]) ->
      let text = out_text value_text in
      P4El.StrE { text } $ no_info
  | CaseV ([ [ { it = Atom "NumE"; _ } ]; [] ], [ value_num ]) ->
      let num = out_num value_num in
      P4El.NumE { num } $ no_info
  | CaseV ([ [ { it = Atom "NameE"; _ } ]; [] ], [ value_var ]) ->
      let var = out_var value_var in
      P4El.VarE { var } $ no_info
  | CaseV ([ [ { it = Atom "SeqE"; _ } ]; [] ], [ value_exprs ]) ->
      let exprs = out_exprs value_exprs in
      P4El.SeqE { exprs } $ no_info
  | CaseV ([ [ { it = Atom "SeqDefaultE"; _ } ]; [] ], [ value_exprs ]) ->
      let exprs = out_exprs value_exprs in
      P4El.SeqDefaultE { exprs } $ no_info
  | CaseV ([ [ { it = Atom "RecordE"; _ } ]; [] ], [ value_fields ]) ->
      let fields = out_list (out_pair out_member out_expr) value_fields in
      P4El.RecordE { fields } $ no_info
  | CaseV ([ [ { it = Atom "RecordDefaultE"; _ } ]; [] ], [ value_fields ]) ->
      let fields = out_list (out_pair out_member out_expr) value_fields in
      P4El.RecordDefaultE { fields } $ no_info
  | CaseV ([ [ { it = Atom "DefaultE"; _ } ] ], []) -> P4El.DefaultE $ no_info
  | CaseV ([ [ { it = Atom "InvalidE"; _ } ] ], []) -> P4El.InvalidE $ no_info
  | CaseV ([ [ { it = Atom "UnE"; _ } ]; []; [] ], [ value_unop; value_expr ])
    ->
      let unop = out_unop value_unop in
      let expr = out_expr value_expr in
      P4El.UnE { unop; expr } $ no_info
  | CaseV
      ( [ [ { it = Atom "BinE"; _ } ]; []; []; [] ],
        [ value_binop; value_expr_l; value_expr_r ] ) ->
      let binop = out_binop value_binop in
      let expr_l = out_expr value_expr_l in
      let expr_r = out_expr value_expr_r in
      P4El.BinE { binop; expr_l; expr_r } $ no_info
  | CaseV
      ( [ [ { it = Atom "TernE"; _ } ]; []; []; [] ],
        [ value_expr_cond; value_expr_then; value_expr_else ] ) ->
      let expr_cond = out_expr value_expr_cond in
      let expr_then = out_expr value_expr_then in
      let expr_else = out_expr value_expr_else in
      P4El.TernE { expr_cond; expr_then; expr_else } $ no_info
  | CaseV ([ [ { it = Atom "CastE"; _ } ]; []; [] ], [ value_typ; value_expr ])
    ->
      let typ = out_typ value_typ in
      let expr = out_expr value_expr in
      P4El.CastE { typ; expr } $ no_info
  | CaseV
      ( [ [ { it = Atom "MaskE"; _ } ]; []; [] ],
        [ value_expr_base; value_expr_mask ] ) ->
      let expr_base = out_expr value_expr_base in
      let expr_mask = out_expr value_expr_mask in
      P4El.MaskE { expr_base; expr_mask } $ no_info
  | CaseV
      ( [ [ { it = Atom "RangeE"; _ } ]; []; [] ],
        [ value_expr_lb; value_expr_ub ] ) ->
      let expr_lb = out_expr value_expr_lb in
      let expr_ub = out_expr value_expr_ub in
      P4El.RangeE { expr_lb; expr_ub } $ no_info
  | CaseV
      ( [ [ { it = Atom "SelectE"; _ } ]; []; [] ],
        [ value_exprs_select; value_cases ] ) ->
      let exprs_select = out_exprs value_exprs_select in
      let cases = out_select_cases value_cases in
      P4El.SelectE { exprs_select; cases } $ no_info
  | CaseV
      ( [ [ { it = Atom "ArrAccE"; _ } ]; []; [] ],
        [ value_expr_base; value_expr_idx ] ) ->
      let expr_base = out_expr value_expr_base in
      let expr_idx = out_expr value_expr_idx in
      P4El.ArrAccE { expr_base; expr_idx } $ no_info
  | CaseV
      ( [ [ { it = Atom "BitAccE"; _ } ]; []; []; [] ],
        [ value_expr_base; value_expr_lo; value_expr_hi ] ) ->
      let expr_base = out_expr value_expr_base in
      let expr_lo = out_expr value_expr_lo in
      let expr_hi = out_expr value_expr_hi in
      P4El.BitAccE { expr_base; expr_lo; expr_hi } $ no_info
  | CaseV ([ [ { it = Atom "ErrAccE"; _ } ]; [] ], [ value_member ]) ->
      let member = out_member value_member in
      P4El.ErrAccE { member } $ no_info
  | CaseV
      ( [ [ { it = Atom "TypeAccE"; _ } ]; []; [] ],
        [ value_var_base; value_member ] ) ->
      let var_base = out_var value_var_base in
      let member = out_member value_member in
      P4El.TypeAccE { var_base; member } $ no_info
  | CaseV
      ( [ [ { it = Atom "ExprAccE"; _ } ]; []; [] ],
        [ value_expr_base; value_member ] ) ->
      let expr_base = out_expr value_expr_base in
      let member = out_member value_member in
      P4El.ExprAccE { expr_base; member } $ no_info
  | CaseV
      ( [ [ { it = Atom "CallFuncE"; _ } ]; []; []; [] ],
        [ value_var_func; value_targs; value_args ] ) ->
      let var_func = out_var value_var_func in
      let targs = out_targs value_targs in
      let args = out_args value_args in
      P4El.CallFuncE { var_func; targs; args } $ no_info
  | CaseV
      ( [ [ { it = Atom "CallMethodE"; _ } ]; []; []; []; [] ],
        [ value_expr_base; value_member; value_targs; value_args ] ) ->
      let expr_base = out_expr value_expr_base in
      let member = out_member value_member in
      let targs = out_targs value_targs in
      let args = out_args value_args in
      P4El.CallMethodE { expr_base; member; targs; args } $ no_info
  | CaseV
      ( [ [ { it = Atom "CallTypeE"; _ } ]; []; []; []; [] ],
        [ value_var_typ; value_member; value_targs; value_args ] ) ->
      let var_typ = out_var value_var_typ in
      let member = out_member value_member in
      let targs = out_targs value_targs in
      let args = out_args value_args in
      P4El.CallTypeE { var_typ; member; targs; args } $ no_info
  | CaseV
      ( [ [ { it = Atom "InstE"; _ } ]; []; []; [] ],
        [ value_var_inst; value_targs; valur_args ] ) ->
      let var_inst = out_var value_var_inst in
      let targs = out_targs value_targs in
      let args = out_args valur_args in
      P4El.InstE { var_inst; targs; args } $ no_info
  | _ -> error "expr" value_expr

and out_exprs (value_exprs : value) : P4El.expr list =
  out_list out_expr value_exprs

(* Keyset expressions *)

and out_keyset (value_keyset : value) : P4El.keyset =
  match value_keyset.it with
  | CaseV ([ [ { it = Atom "ExprK"; _ } ]; [] ], [ value_expr ]) ->
      let expr = out_expr value_expr in
      P4Lang.ExprK expr $ no_info
  | CaseV ([ [ { it = Atom "DefaultK"; _ } ] ], []) -> P4Lang.DefaultK $ no_info
  | CaseV ([ [ { it = Atom "AnyK"; _ } ] ], []) -> P4Lang.AnyK $ no_info
  | _ -> error "keyset" value_keyset

and out_keysets (value_keysets : value) : P4El.keyset list =
  out_list out_keyset value_keysets

(* Select-cases for select *)

and out_select_case (value_select_case : value) : P4El.select_case =
  match value_select_case.it with
  | CaseV ([ []; []; [] ], [ value_keysets; value_state_label ]) ->
      let keysets = out_keysets value_keysets in
      let state_label = out_state_label value_state_label in
      (keysets, state_label) $ no_info
  | _ -> error "select_case" value_select_case

and out_select_cases (value_select_cases : value) : P4El.select_case list =
  out_list out_select_case value_select_cases

(* Statements *)

and out_stmt (value_stmt : value) : P4El.stmt =
  match value_stmt.it with
  | CaseV ([ [ { it = Atom "EmptyS"; _ } ] ], []) -> P4El.EmptyS $ no_info
  | CaseV
      ( [ [ { it = Atom "AssignS"; _ } ]; []; [] ],
        [ value_expr_l; value_expr_r ] ) ->
      let expr_l = out_expr value_expr_l in
      let expr_r = out_expr value_expr_r in
      P4El.AssignS { expr_l; expr_r } $ no_info
  | CaseV
      ( [ [ { it = Atom "SwitchS"; _ } ]; []; [] ],
        [ value_expr_switch; value_cases ] ) ->
      let expr_switch = out_expr value_expr_switch in
      let cases = out_switch_cases value_cases in
      P4El.SwitchS { expr_switch; cases } $ no_info
  | CaseV
      ( [ [ { it = Atom "IfS"; _ } ]; []; []; [] ],
        [ value_expr_cond; value_stmt_then; value_stmt_else ] ) ->
      let expr_cond = out_expr value_expr_cond in
      let stmt_then = out_stmt value_stmt_then in
      let stmt_else = out_stmt value_stmt_else in
      P4El.IfS { expr_cond; stmt_then; stmt_else } $ no_info
  | CaseV ([ [ { it = Atom "BlockS"; _ } ]; [] ], [ value_block ]) ->
      let block = out_block value_block in
      P4El.BlockS { block } $ no_info
  | CaseV ([ [ { it = Atom "ExitS"; _ } ] ], []) -> P4El.ExitS $ no_info
  | CaseV ([ [ { it = Atom "RetS"; _ } ]; [] ], [ value_expr_ret ]) ->
      let expr_ret = out_opt out_expr value_expr_ret in
      P4El.RetS { expr_ret } $ no_info
  | CaseV
      ( [ [ { it = Atom "CallFuncS"; _ } ]; []; []; [] ],
        [ value_var_func; value_targs; value_args ] ) ->
      let var_func = out_var value_var_func in
      let targs = out_targs value_targs in
      let args = out_args value_args in
      P4El.CallFuncS { var_func; targs; args } $ no_info
  | CaseV
      ( [ [ { it = Atom "CallMethodS"; _ } ]; []; []; []; [] ],
        [ value_expr_base; value_member; value_targs; value_args ] ) ->
      let expr_base = out_expr value_expr_base in
      let member = out_member value_member in
      let targs = out_targs value_targs in
      let args = out_args value_args in
      P4El.CallMethodS { expr_base; member; targs; args } $ no_info
  | CaseV
      ( [ [ { it = Atom "CallInstS"; _ } ]; []; []; [] ],
        [ value_var_inst; value_targs; value_args ] ) ->
      let var_inst = out_var value_var_inst in
      let targs = out_targs value_targs in
      let args = out_args value_args in
      P4El.CallInstS { var_inst; targs; args } $ no_info
  | CaseV ([ [ { it = Atom "TransS"; _ } ]; [] ], [ value_expr_label ]) ->
      let expr_label = out_expr value_expr_label in
      P4El.TransS { expr_label } $ no_info
  | CaseV ([ [ { it = Atom "DeclS"; _ } ]; [] ], [ value_decl ]) ->
      let decl = out_decl value_decl in
      P4El.DeclS { decl } $ no_info
  | _ -> error "stmt" value_stmt

and out_stmts (value_stmts : value) : P4El.stmt list =
  out_list out_stmt value_stmts

(* Blocks (sequence of statements) *)

and out_block (value_block : value) : P4El.block =
  match value_block.it with
  | CaseV ([ [ { it = Atom "BlockB"; _ } ]; [] ], [ value_stmts ]) ->
      let stmts = out_stmts value_stmts in
      (stmts, []) $ no_info
  | _ -> error "block" value_block

(* Match-cases for switch *)

and out_switch_label (value_switch_label : value) : P4El.switch_label =
  match value_switch_label.it with
  | CaseV ([ [ { it = Atom "ExprL"; _ } ]; [] ], [ value_expr ]) ->
      let expr = out_expr value_expr in
      P4Lang.ExprL expr $ no_info
  | CaseV ([ [ { it = Atom "DefaultL"; _ } ] ], []) -> P4Lang.DefaultL $ no_info
  | _ -> error "switch_label" value_switch_label

and out_switch_case (value_switch_case : value) : P4El.switch_case =
  match value_switch_case.it with
  | CaseV
      ( [ [ { it = Atom "MatchC"; _ } ]; []; [] ],
        [ value_switch_label; value_block ] ) ->
      let switch_label = out_switch_label value_switch_label in
      let block = out_block value_block in
      P4Lang.MatchC (switch_label, block) $ no_info
  | CaseV ([ [ { it = Atom "FallC"; _ } ]; [] ], [ value_switch_label ]) ->
      let switch_label = out_switch_label value_switch_label in
      P4Lang.FallC switch_label $ no_info
  | _ -> error "switch_case" value_switch_case

and out_switch_cases (value_switch_cases : value) : P4El.switch_case list =
  out_list out_switch_case value_switch_cases

(* Declarations *)

and out_typdef (value_typdef : value) : (P4El.typ, P4El.decl) P4El.alt =
  match value_typdef.it with
  | CaseV ([ [ { it = Atom "TypeD"; _ } ]; [] ], [ value_typ ]) ->
      let typ = out_typ value_typ in
      P4Lang.Left typ
  | CaseV ([ [ { it = Atom "DeclD"; _ } ]; [] ], [ value_decl ]) ->
      let decl = out_decl value_decl in
      P4Lang.Right decl
  | _ -> error "typdef" value_typdef

and out_decl (value_decl : value) : P4El.decl =
  match value_decl.it with
  | CaseV
      ( [ [ { it = Atom "ConstD"; _ } ]; []; []; [] ],
        [ value_id; value_typ; value_value ] ) ->
      let id = out_id value_id in
      let typ = out_typ value_typ in
      let value = out_expr value_value in
      P4El.ConstD { id; typ; value; annos = [] } $ no_info
  | CaseV
      ( [ [ { it = Atom "VarD"; _ } ]; []; []; [] ],
        [ value_id; value_typ; value_init ] ) ->
      let id = out_id value_id in
      let typ = out_typ value_typ in
      let init = out_opt out_expr value_init in
      P4El.VarD { id; typ; init; annos = [] } $ no_info
  | CaseV ([ [ { it = Atom "ErrD"; _ } ]; [] ], [ value_members ]) ->
      let members = out_members value_members in
      P4El.ErrD { members } $ no_info
  | CaseV ([ [ { it = Atom "MatchKindD"; _ } ]; [] ], [ value_members ]) ->
      let members = out_members value_members in
      P4El.MatchKindD { members } $ no_info
  | CaseV
      ( [ [ { it = Atom "InstD"; _ } ]; []; []; []; []; [] ],
        [ value_id; value_var_inst; value_targs; value_args; value_init ] ) ->
      let id = out_id value_id in
      let var_inst = out_var value_var_inst in
      let targs = out_targs value_targs in
      let args = out_args value_args in
      let init = out_decls value_init in
      P4El.InstD { id; var_inst; targs; args; init; annos = [] } $ no_info
  | CaseV
      ( [ [ { it = Atom "StructD"; _ } ]; []; []; [] ],
        [ value_id; value_tparams; value_fields ] ) ->
      let id = out_id value_id in
      let tparams = out_tparams value_tparams in
      let fields = out_list (out_pair out_member out_typ) value_fields in
      let fields = List.map (fun (member, typ) -> (member, typ, [])) fields in
      P4El.StructD { id; tparams; fields; annos = [] } $ no_info
  | CaseV
      ( [ [ { it = Atom "HeaderD"; _ } ]; []; []; [] ],
        [ value_id; value_tparams; value_fields ] ) ->
      let id = out_id value_id in
      let tparams = out_tparams value_tparams in
      let fields = out_list (out_pair out_member out_typ) value_fields in
      let fields = List.map (fun (member, typ) -> (member, typ, [])) fields in
      P4El.HeaderD { id; tparams; fields; annos = [] } $ no_info
  | CaseV
      ( [ [ { it = Atom "UnionD"; _ } ]; []; []; [] ],
        [ value_id; value_tparams; value_fields ] ) ->
      let id = out_id value_id in
      let tparams = out_tparams value_tparams in
      let fields = out_list (out_pair out_member out_typ) value_fields in
      let fields = List.map (fun (member, typ) -> (member, typ, [])) fields in
      P4El.UnionD { id; tparams; fields; annos = [] } $ no_info
  | CaseV ([ [ { it = Atom "EnumD"; _ } ]; []; [] ], [ value_id; value_members ])
    ->
      let id = out_id value_id in
      let members = out_members value_members in
      P4El.EnumD { id; members; annos = [] } $ no_info
  | CaseV
      ( [ [ { it = Atom "SEnumD"; _ } ]; []; []; [] ],
        [ value_id; value_typ; value_fields ] ) ->
      let id = out_id value_id in
      let typ = out_typ value_typ in
      let fields = out_list (out_pair out_member out_expr) value_fields in
      P4El.SEnumD { id; typ; fields; annos = [] } $ no_info
  | CaseV
      ([ [ { it = Atom "NewTypeD"; _ } ]; []; [] ], [ value_id; value_typdef ])
    ->
      let id = out_id value_id in
      let typdef = out_typdef value_typdef in
      P4El.NewTypeD { id; typdef; annos = [] } $ no_info
  | CaseV
      ([ [ { it = Atom "TypeDefD"; _ } ]; []; [] ], [ value_id; value_typdef ])
    ->
      let id = out_id value_id in
      let typdef = out_typdef value_typdef in
      P4El.TypeDefD { id; typdef; annos = [] } $ no_info
  | CaseV
      ( [ [ { it = Atom "ValueSetD"; _ } ]; []; []; [] ],
        [ value_id; value_typ; value_size ] ) ->
      let id = out_id value_id in
      let typ = out_typ value_typ in
      let size = out_expr value_size in
      P4El.ValueSetD { id; typ; size; annos = [] } $ no_info
  | CaseV
      ( [ [ { it = Atom "ParserTypeD"; _ } ]; []; []; [] ],
        [ value_id; value_tparams; value_params ] ) ->
      let id = out_id value_id in
      let tparams = out_tparams value_tparams in
      let params = out_params value_params in
      P4El.ParserTypeD { id; tparams; params; annos = [] } $ no_info
  | CaseV
      ( [ [ { it = Atom "ParserD"; _ } ]; []; []; []; []; [] ],
        [ value_id; value_params; value_cparams; value_locals; value_states ] )
    ->
      let id = out_id value_id in
      let params = out_params value_params in
      let cparams = out_cparams value_cparams in
      let locals = out_decls value_locals in
      let states = out_parser_states value_states in
      P4El.ParserD
        { id; tparams = []; params; cparams; locals; states; annos = [] }
      $ no_info
  | CaseV ([ [ { it = Atom "TableD"; _ } ]; []; [] ], [ value_id; value_table ])
    ->
      let id = out_id value_id in
      let table = out_table value_table in
      P4El.TableD { id; table; annos = [] } $ no_info
  | CaseV
      ( [ [ { it = Atom "ControlTypeD"; _ } ]; []; []; [] ],
        [ value_id; value_tparams; value_params ] ) ->
      let id = out_id value_id in
      let tparams = out_tparams value_tparams in
      let params = out_params value_params in
      P4El.ControlTypeD { id; tparams; params; annos = [] } $ no_info
  | CaseV
      ( [ [ { it = Atom "ControlD"; _ } ]; []; []; []; []; [] ],
        [ value_id; value_params; value_cparams; value_locals; value_body ] ) ->
      let id = out_id value_id in
      let params = out_params value_params in
      let cparams = out_cparams value_cparams in
      let locals = out_decls value_locals in
      let body = out_block value_body in
      P4El.ControlD
        { id; tparams = []; params; cparams; locals; body; annos = [] }
      $ no_info
  | CaseV
      ( [ [ { it = Atom "ActionD"; _ } ]; []; []; [] ],
        [ value_id; value_params; value_body ] ) ->
      let id = out_id value_id in
      let params = out_params value_params in
      let body = out_block value_body in
      P4El.ActionD { id; params; body; annos = [] } $ no_info
  | CaseV
      ( [ [ { it = Atom "FuncD"; _ } ]; []; []; []; []; [] ],
        [ value_id; value_typ_ret; value_tparams; value_params; value_body ] )
    ->
      let id = out_id value_id in
      let typ_ret = out_typ value_typ_ret in
      let tparams = out_tparams value_tparams in
      let params = out_params value_params in
      let body = out_block value_body in
      P4El.FuncD { id; typ_ret; tparams; params; body } $ no_info
  | CaseV
      ( [ [ { it = Atom "ExternFuncD"; _ } ]; []; []; []; [] ],
        [ value_id; value_typ_ret; value_tparams; value_params ] ) ->
      let id = out_id value_id in
      let typ_ret = out_typ value_typ_ret in
      let tparams = out_tparams value_tparams in
      let params = out_params value_params in
      P4El.ExternFuncD { id; typ_ret; tparams; params; annos = [] } $ no_info
  | CaseV
      ( [ [ { it = Atom "ExternObjectD"; _ } ]; []; []; [] ],
        [ value_id; value_tparams; value_mthds ] ) ->
      let id = out_id value_id in
      let tparams = out_tparams value_tparams in
      let mthds = out_mthds value_mthds in
      P4El.ExternObjectD { id; tparams; mthds; annos = [] } $ no_info
  | CaseV
      ( [ [ { it = Atom "PackageTypeD"; _ } ]; []; []; [] ],
        [ value_id; value_tparams; value_cparams ] ) ->
      let id = out_id value_id in
      let tparams = out_tparams value_tparams in
      let cparams = out_cparams value_cparams in
      P4El.PackageTypeD { id; tparams; cparams; annos = [] } $ no_info
  | _ -> error "decl" value_decl

and out_decls (value_decls : value) : P4El.decl list =
  out_list out_decl value_decls

(* Parser state machine *)

and out_parser_state (value_parser_state : value) : P4El.parser_state =
  match value_parser_state.it with
  | CaseV ([ []; []; [] ], [ value_state_label; value_block ]) ->
      let state_label = out_state_label value_state_label in
      let block = out_block value_block in
      (state_label, block, []) $ no_info
  | _ -> error "parser_state" value_parser_state

and out_parser_states (value_parser_states : value) : P4El.parser_state list =
  out_list out_parser_state value_parser_states

(* Table *)

and out_table (value_table : value) : P4El.table =
  out_list out_table_property value_table

(* Table properties *)

and out_table_property (value_table_property : value) : P4El.table_property =
  match value_table_property.it with
  | CaseV ([ [ { it = Atom "KeyP"; _ } ]; [] ], [ value_table_keys ]) ->
      let table_keys = out_table_keys value_table_keys in
      P4Lang.KeyP table_keys
  | CaseV ([ [ { it = Atom "ActionP"; _ } ]; [] ], [ value_table_actions ]) ->
      let table_actions = out_table_actions value_table_actions in
      P4Lang.ActionP table_actions
  | CaseV ([ [ { it = Atom "EntryP"; _ } ]; [] ], [ value_table_entries ]) ->
      let table_entries = out_table_entries value_table_entries in
      P4Lang.EntryP table_entries
  | CaseV ([ [ { it = Atom "DefaultP"; _ } ]; [] ], [ value_table_default ]) ->
      let table_default = out_table_default value_table_default in
      P4Lang.DefaultP table_default
  | CaseV ([ [ { it = Atom "CustomP"; _ } ]; [] ], [ value_table_custom ]) ->
      let table_custom = out_table_custom value_table_custom in
      P4Lang.CustomP table_custom
  | _ -> error "table_property" value_table_property

(* Table keys  *)

and out_table_key (value_table_key : value) : P4El.table_key =
  match value_table_key.it with
  | CaseV ([ []; []; [] ], [ value_expr; value_match_kind ]) ->
      let expr = out_expr value_expr in
      let match_kind = out_match_kind value_match_kind in
      (expr, match_kind, []) $ no_info
  | _ -> error "table_key" value_table_key

and out_table_keys (value_table_keys : value) : P4El.table_keys =
  let table_keys = out_list out_table_key value_table_keys in
  table_keys $ no_info

(* Table action references *)

and out_table_action (value_table_action : value) : P4El.table_action =
  match value_table_action.it with
  | CaseV ([ []; []; [] ], [ value_var; value_args ]) ->
      let var = out_var value_var in
      let args = out_args value_args in
      (var, args, []) $ no_info
  | _ -> error "table_action" value_table_action

and out_table_actions (value_table_actions : value) : P4El.table_actions =
  let table_actions = out_list out_table_action value_table_actions in
  table_actions $ no_info

(* Table entries *)

and out_table_entry (value_table_entry : value) : P4El.table_entry =
  match value_table_entry.it with
  | CaseV
      ( [ []; []; []; []; [] ],
        [
          value_table_entry_const;
          value_keysets;
          value_table_action;
          value_expr_opt;
        ] ) ->
      let table_entry_const = out_bool value_table_entry_const in
      let keysets = out_keysets value_keysets in
      let table_action = out_table_action value_table_action in
      let expr_opt = out_opt out_expr value_expr_opt in
      (table_entry_const, keysets, table_action, expr_opt, []) $ no_info
  | _ -> error "table_entry" value_table_entry

and out_table_entries (value_table_entries : value) : P4El.table_entries =
  match value_table_entries.it with
  | CaseV ([ []; []; [] ], [ value_table_entries_const; value_table_entries ])
    ->
      let table_entries_const = out_bool value_table_entries_const in
      let table_entries = out_list out_table_entry value_table_entries in
      (table_entries_const, table_entries) $ no_info
  | _ -> error "table_entries" value_table_entries

(* Table default properties *)

and out_table_default (value_table_default : value) : P4El.table_default =
  match value_table_default.it with
  | CaseV ([ []; []; [] ], [ value_table_default_const; value_table_action ]) ->
      let table_default_const = out_bool value_table_default_const in
      let table_action = out_table_action value_table_action in
      (table_default_const, table_action) $ no_info
  | _ -> error "table_default" value_table_default

(* Table custom properties *)

and out_table_custom (value_table_custom : value) : P4El.table_custom =
  match value_table_custom.it with
  | CaseV
      ( [ []; []; []; [] ],
        [ value_table_custom_const; value_member; value_expr ] ) ->
      let table_custom_const = out_bool value_table_custom_const in
      let member = out_member value_member in
      let expr = out_expr value_expr in
      (table_custom_const, member, expr, []) $ no_info
  | _ -> error "table_custom" value_table_custom

(* Methods *)

and out_mthd (value_mthd : value) : P4El.mthd =
  match value_mthd.it with
  | CaseV
      ( [ [ { it = Atom "ExternConsM"; _ } ]; []; [] ],
        [ value_id; value_cparams ] ) ->
      let id = out_id value_id in
      let cparams = out_cparams value_cparams in
      P4El.ExternConsM { id; cparams; annos = [] } $ no_info
  | CaseV
      ( [ [ { it = Atom "ExternAbstractM"; _ } ]; []; []; []; [] ],
        [ value_id; value_typ_ret; value_tparams; value_params ] ) ->
      let id = out_id value_id in
      let typ_ret = out_typ value_typ_ret in
      let tparams = out_tparams value_tparams in
      let params = out_params value_params in
      P4El.ExternAbstractM { id; typ_ret; tparams; params; annos = [] }
      $ no_info
  | CaseV
      ( [ [ { it = Atom "ExternM"; _ } ]; []; []; []; [] ],
        [ value_id; value_typ_ret; value_tparams; value_params ] ) ->
      let id = out_id value_id in
      let typ_ret = out_typ value_typ_ret in
      let tparams = out_tparams value_tparams in
      let params = out_params value_params in
      P4El.ExternM { id; typ_ret; tparams; params; annos = [] } $ no_info
  | _ -> error "mthd" value_mthd

and out_mthds (value_mthds : value) : P4El.mthd list =
  out_list out_mthd value_mthds

(* Program *)

let out_program (value_program : value) : P4El.program = out_decls value_program
