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

(* Texts *)

let out_text (value_text : value) : P4El.text =
  match value_text.it with
  | TextV s -> s $ no_info
  | _ -> error "text" value_text

(* Identifiers *)

let out_id (value_id : value) : P4El.id =
  match value_id.it with TextV s -> s $ no_info | _ -> error "id" value_id

(* Members *)

let out_member (value_member : value) : P4El.member =
  match value_member.it with
  | TextV s -> s $ no_info
  | _ -> error "member" value_member

let out_members (value_members : value) : P4El.member list =
  out_list out_member value_members

(* Binary operators *)

let out_binop (value_binop : value) : P4El.binop =
  match value_binop.it with
  | CaseV ([ [ { it = Atom "PLUS"; _ } ] ], []) -> P4Lang.PlusOp $ no_info
  | CaseV ([ [ { it = Atom "MINUS"; _ } ] ], []) -> P4Lang.MinusOp $ no_info
  | CaseV ([ [ { it = Atom "SHL"; _ } ] ], []) -> P4Lang.ShlOp $ no_info
  | CaseV ([ [ { it = Atom "SHR"; _ } ] ], []) -> P4Lang.ShrOp $ no_info
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
  | CaseV ([ [ { it = Atom "FIntT"; _ } ]; [] ], [ value_expr ]) ->
      let expr = out_expr value_expr in
      P4El.FIntT expr $ no_info
  | CaseV ([ [ { it = Atom "FBitT"; _ } ]; [] ], [ value_expr ]) ->
      let expr = out_expr value_expr in
      P4El.FBitT expr $ no_info
  | CaseV ([ [ { it = Atom "NameT"; _ } ]; [] ], [ value_id ]) ->
      let id = out_id value_id in
      let var = P4Lang.Current id $ no_info in
      P4El.NameT var $ no_info
  | _ -> error "typ" value_typ

and out_typs (value_typs : value) : P4El.typ list = out_list out_typ value_typs

(* Parameters *)

and out_param (value_param : value) : P4El.param =
  match value_param.it with
  | CaseV ([ []; []; []; [] ], [ value_id; value_dir; value_typ ]) ->
      let id = out_id value_id in
      let dir = out_dir value_dir in
      let typ = out_typ value_typ in
      (id, dir, typ, None, []) $ no_info
  | _ -> error "param" value_param

and out_params (value_params : value) : P4El.param list =
  out_list out_param value_params

(* Expressions *)

and out_expr (value_expr : value) : P4El.expr =
  match value_expr.it with
  | CaseV ([ [ { it = Atom "IntE"; _ } ]; [] ], [ value_int ]) ->
      let i =
        match value_int.it with
        | NumV (`Int i) -> i
        | _ -> error "int" value_int
      in
      let num = (i, None) $ no_info in
      P4El.NumE { num } $ no_info
  | CaseV ([ [ { it = Atom "FIntE"; _ } ]; []; [] ], [ value_width; value_int ])
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
      let num = (i, Some (width, true)) $ no_info in
      P4El.NumE { num } $ no_info
  | CaseV ([ [ { it = Atom "FBitE"; _ } ]; []; [] ], [ value_width; value_int ])
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
      let num = (i, Some (width, true)) $ no_info in
      P4El.NumE { num } $ no_info
  | CaseV ([ [ { it = Atom "NameE"; _ } ]; [] ], [ value_id ]) ->
      let id = out_id value_id in
      let var = P4Lang.Current id $ no_info in
      P4El.VarE { var } $ no_info
  | CaseV
      ( [ [ { it = Atom "BinE"; _ } ]; []; []; [] ],
        [ value_binop; value_expr_l; value_expr_r ] ) ->
      let binop = out_binop value_binop in
      let expr_l = out_expr value_expr_l in
      let expr_r = out_expr value_expr_r in
      P4El.BinE { binop; expr_l; expr_r } $ no_info
  | CaseV
      ( [ [ { it = Atom "ExprAccE"; _ } ]; []; [] ],
        [ value_expr_base; value_member ] ) ->
      let expr_base = out_expr value_expr_base in
      let member = out_member value_member in
      P4El.ExprAccE { expr_base; member } $ no_info
  | _ -> error "expr" value_expr

and out_exprs (value_exprs : value) : P4El.expr list =
  out_list out_expr value_exprs

(* Statements *)

and out_stmt (value_stmt : value) : P4El.stmt =
  match value_stmt.it with
  | CaseV ([ [ { it = Atom "RetS"; _ } ]; [] ], [ value_expr_ret ]) ->
      let expr_ret = out_opt out_expr value_expr_ret in
      P4El.RetS { expr_ret } $ no_info
  | _ -> error "stmt" value_stmt

and out_stmts (value_stmts : value) : P4El.stmt list =
  out_list out_stmt value_stmts

(* Declarations *)

and out_decl (value_decl : value) : P4El.decl =
  match value_decl.it with
  | CaseV
      ([ [ { it = Atom "HeaderD"; _ } ]; []; [] ], [ value_id; value_fields ])
    ->
      let id = out_id value_id in
      let tparams = [] in
      let fields = out_list (out_pair out_member out_typ) value_fields in
      let fields = List.map (fun (member, typ) -> (member, typ, [])) fields in
      P4El.HeaderD { id; tparams; fields; annos = [] } $ no_info
  | CaseV
      ( [ [ { it = Atom "FuncD"; _ } ]; []; []; []; [] ],
        [ value_id; value_typ_ret; value_params; value_body ] ) ->
      let id = out_id value_id in
      let typ_ret = out_typ value_typ_ret in
      let tparams = [] in
      let params = out_params value_params in
      let stmts = out_stmts value_body in
      let body = (stmts, []) $ no_info in
      P4El.FuncD { id; typ_ret; tparams; params; body } $ no_info
  | _ -> error "decl" value_decl

and out_decls (value_decls : value) : P4El.decl list =
  out_list out_decl value_decls

(* Program *)

let out_program (value_program : value) : P4El.program = out_decls value_program
