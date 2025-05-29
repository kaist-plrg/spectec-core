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

(* Binary operators *)

let in_binop (graph : Dep.Graph.t) (binop : P4.binop) : value =
  let mixop =
    match binop.it with
    | PlusOp -> [ [ in_atom "PLUS" ] ]
    | MinusOp -> [ [ in_atom "MINUS" ] ]
    | ShlOp -> [ [ in_atom "SHL" ] ]
    | ShrOp -> [ [ in_atom "SHR" ] ]
    | _ -> error_convert_in "minified binop"
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
    | In -> [ [ in_atom "IN" ] ]
    | Out -> [ [ in_atom "OUT" ] ]
    | InOut -> [ [ in_atom "INOUT" ] ]
    | _ -> error_convert_in "minified dir"
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
    | FIntT expr ->
        let mixop = [ [ in_atom "FIntT" ]; [] ] in
        let value_expr = in_expr graph expr in
        (mixop, [ value_expr ])
    | FBitT expr ->
        let mixop = [ [ in_atom "FBitT" ]; [] ] in
        let value_expr = in_expr graph expr in
        (mixop, [ value_expr ])
    | NameT { it = Current id; _ } ->
        let mixop = [ [ in_atom "NameT" ]; [] ] in
        let value_id = in_id graph id in
        (mixop, [ value_id ])
    | _ -> error_convert_in "minified typ"
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

(* Parameters *)

and in_param (graph : Dep.Graph.t) (param : P4.param) : value =
  let id, dir, typ, _, _ = param.it in
  let mixop = [ []; []; []; [] ] in
  let value_id = in_id graph id in
  let value_dir = in_dir graph dir in
  let value_typ = in_typ graph typ in
  let value =
    let vid = Dep.Graph.fresh () in
    let typ = in_typ_var "param" in
    CaseV (mixop, [ value_id; value_dir; value_typ ]) $$$ { vid; typ }
  in
  Dep.Graph.add_node ~taint:true graph value;
  value

and in_params (graph : Dep.Graph.t) (params : P4.param list) : value =
  in_list in_param (in_typ_var "param" $ no_region) graph params

(* Expressions *)

and in_expr (graph : Dep.Graph.t) (expr : P4.expr) : value =
  let mixop, values =
    match expr.it with
    | NumE { num } -> (
        match num.it with
        | i, Some (width, signed) ->
            let mixop =
              if signed then [ [ in_atom "FIntE" ]; []; [] ]
              else [ [ in_atom "FBitE" ]; []; [] ]
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
            let mixop = [ [ in_atom "IntE" ]; [] ] in
            let value_int =
              let vid = Dep.Graph.fresh () in
              let typ = Il.Ast.NumT `IntT in
              NumV (`Int i) $$$ { vid; typ }
            in
            Dep.Graph.add_node ~taint:true graph value_int;
            (mixop, [ value_int ]))
    | VarE { var = { it = Current id; _ } } ->
        let mixop = [ [ in_atom "NameE" ]; [] ] in
        let value_id = in_id graph id in
        (mixop, [ value_id ])
    | BinE { binop; expr_l; expr_r } ->
        let mixop = [ [ in_atom "BinE" ]; []; []; [] ] in
        let value_binop = in_binop graph binop in
        let value_expr_l = in_expr graph expr_l in
        let value_expr_r = in_expr graph expr_r in
        (mixop, [ value_binop; value_expr_l; value_expr_r ])
    | ExprAccE { expr_base; member } ->
        let mixop = [ [ in_atom "ExprAccE" ]; []; [] ] in
        let value_expr_base = in_expr graph expr_base in
        let value_member = in_member graph member in
        (mixop, [ value_expr_base; value_member ])
    | _ -> error_convert_in "minified expr"
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

(* Statements *)

and in_stmt (graph : Dep.Graph.t) (stmt : P4.stmt) : value =
  let mixop, values =
    match stmt.it with
    | RetS { expr_ret } ->
        let mixop = [ [ in_atom "RetS" ]; [] ] in
        let value_expr_ret =
          in_opt in_expr (in_typ_var "expr" $ no_region) graph expr_ret
        in
        (mixop, [ value_expr_ret ])
    | _ -> error_convert_in "minified stmt"
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

(* Declarations *)

and in_decl (graph : Dep.Graph.t) (decl : P4.decl) : value =
  let mixop, values =
    match decl.it with
    | HeaderD { id; fields; _ } ->
        let mixop = [ [ in_atom "HeaderD" ]; []; [] ] in
        let value_id = in_id graph id in
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
        (mixop, [ value_id; value_fields ])
    | FuncD { id; typ_ret; params; body; _ } ->
        let mixop = [ [ in_atom "FuncD" ]; []; []; []; [] ] in
        let value_id = in_id graph id in
        let value_typ_ret = in_typ graph typ_ret in
        let value_params = in_params graph params in
        let stmts, _ = body.it in
        let value_body = in_stmts graph stmts in
        (mixop, [ value_id; value_typ_ret; value_params; value_body ])
    | _ -> error_convert_in "minified decl"
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

(* Program *)

let in_program (graph : Dep.Graph.t) (includes_p4 : string list)
    (filename_p4 : string) : value =
  try
    P4frontend.Parse.parse_file includes_p4 filename_p4
    |> in_list in_decl (in_typ_var "decl" $ no_region) graph
  with P4util.Error.ParseErr (msg, _) -> error_convert_in msg
