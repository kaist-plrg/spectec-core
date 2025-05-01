open Xl
open Ast

(* Identifiers *)

let eq_id (id_a : id) (id_b : id) : bool = id_a.it = id_b.it

(* Atoms *)

let eq_atom (atom_a : atom) (atom_b : atom) : bool = Atom.eq atom_a.it atom_b.it

let eq_atoms (atoms_a : atom list) (atoms_b : atom list) : bool =
  List.length atoms_a = List.length atoms_b
  && List.for_all2 eq_atom atoms_a atoms_b

(* Mixfix operators *)

let eq_mixop (mixop_a : mixop) (mixop_b : mixop) : bool =
  Mixop.eq mixop_a mixop_b

(* Iterators *)

let eq_iter (iter_a : iter) (iter_b : iter) : bool = iter_a = iter_b

let eq_iters (iters_a : iter list) (iters_b : iter list) : bool =
  List.length iters_a = List.length iters_b
  && List.for_all2 eq_iter iters_a iters_b

(* Variables *)

let rec eq_var (var_a : var) (var_b : var) : bool =
  let id_a, _typ_a, iters_a = var_a in
  let id_b, _typ_b, iters_b = var_b in
  eq_id id_a id_b && eq_iters iters_a iters_b

and eq_vars (vars_a : var list) (vars_b : var list) : bool =
  let compare_id (id_a : id) (id_b : id) : int = compare id_a.it id_b.it in
  let compare_iters (iters_a : iter list) (iters_b : iter list) : int =
    let compare_iter (iter_a : iter) (iter_b : iter) : int =
      match (iter_a, iter_b) with
      | Opt, Opt -> 0
      | Opt, List -> -1
      | List, Opt -> 1
      | List, List -> 0
    in
    List.compare compare_iter iters_a iters_b
  in
  let compare_var ((id_a, _typ_a, iters_a) : var)
      ((id_b, _typ_b, iters_b) : var) : int =
    match compare_id id_a id_b with
    | 0 -> compare_iters iters_a iters_b
    | n -> n
  in
  let vars_a = List.sort compare_var vars_a in
  let vars_b = List.sort compare_var vars_b in
  List.length vars_a = List.length vars_b && List.for_all2 eq_var vars_a vars_b

(* Types *)

and eq_typ (typ_a : typ) (typ_b : typ) : bool =
  match (typ_a.it, typ_b.it) with
  | BoolT, BoolT -> true
  | NumT numtyp_a, NumT numtyp_b -> Num.equiv numtyp_a numtyp_b
  | TextT, TextT -> true
  | VarT (id_a, targs_a), VarT (id_b, targs_b) ->
      eq_id id_a id_b && eq_targs targs_a targs_b
  | TupleT typs_a, TupleT typs_b -> eq_typs typs_a typs_b
  | IterT (typ_a, iter_a), IterT (typ_b, iter_b) ->
      eq_typ typ_a typ_b && eq_iter iter_a iter_b
  | FuncT, FuncT -> true
  | _ -> false

and eq_typs (typs_a : typ list) (typs_b : typ list) : bool =
  List.length typs_a = List.length typs_b && List.for_all2 eq_typ typs_a typs_b

(* Expressions *)

and eq_exp (exp_a : exp) (exp_b : exp) : bool =
  match (exp_a.it, exp_b.it) with
  | BoolE b_a, BoolE b_b -> b_a = b_b
  | NumE n_a, NumE n_b -> Num.eq n_a n_b
  | TextE t_a, TextE t_b -> t_a = t_b
  | VarE id_a, VarE id_b -> eq_id id_a id_b
  | UnE (unop_a, optyp_a, exp_a), UnE (unop_b, optyp_b, exp_b) ->
      unop_a = unop_b && optyp_a = optyp_b && eq_exp exp_a exp_b
  | ( BinE (binop_a, optyp_a, exp_l_a, exp_r_a),
      BinE (binop_b, optyp_b, exp_l_b, exp_r_b) ) ->
      binop_a = binop_b && optyp_a = optyp_b && eq_exp exp_l_a exp_l_b
      && eq_exp exp_r_a exp_r_b
  | ( CmpE (cmpop_a, optyp_a, exp_l_a, exp_r_a),
      CmpE (cmpop_b, optyp_b, exp_l_b, exp_r_b) ) ->
      cmpop_a = cmpop_b && optyp_a = optyp_b && eq_exp exp_l_a exp_l_b
      && eq_exp exp_r_a exp_r_b
  | UpCastE (typ_a, exp_a), UpCastE (typ_b, exp_b)
  | DownCastE (typ_a, exp_a), DownCastE (typ_b, exp_b)
  | SubE (exp_a, typ_a), SubE (exp_b, typ_b) ->
      eq_exp exp_a exp_b && eq_typ typ_a typ_b
  | MatchE (exp_a, pattern_a), MatchE (exp_b, pattern_b) ->
      eq_exp exp_a exp_b && eq_pattern pattern_a pattern_b
  | TupleE exps_a, TupleE exps_b -> eq_exps exps_a exps_b
  | CaseE (mixop_a, exps_a), CaseE (mixop_b, exps_b) ->
      eq_mixop mixop_a mixop_b && eq_exps exps_a exps_b
  | StrE expfields_a, StrE expfields_b ->
      List.length expfields_a = List.length expfields_b
      && List.for_all2
           (fun (atom_a, exp_a) (atom_b, exp_b) ->
             eq_atom atom_a atom_b && eq_exp exp_a exp_b)
           expfields_a expfields_b
  | OptE (Some exp_a), OptE (Some exp_b) -> eq_exp exp_a exp_b
  | OptE None, OptE None -> true
  | ListE exps_a, ListE exps_b -> eq_exps exps_a exps_b
  | ConsE (exp_h_a, exp_t_a), ConsE (exp_h_b, exp_t_b) ->
      eq_exp exp_h_a exp_h_b && eq_exp exp_t_a exp_t_b
  | CatE (exp_l_a, exp_r_a), CatE (exp_l_b, exp_r_b) ->
      eq_exp exp_l_a exp_l_b && eq_exp exp_r_a exp_r_b
  | MemE (exp_e_a, exp_s_a), MemE (exp_e_b, exp_s_b) ->
      eq_exp exp_e_a exp_e_b && eq_exp exp_s_a exp_s_b
  | LenE exp_a, LenE exp_b -> eq_exp exp_a exp_b
  | DotE (exp_a, atom_a), DotE (exp_b, atom_b) ->
      eq_exp exp_a exp_b && eq_atom atom_a atom_b
  | IdxE (exp_b_a, exp_i_a), IdxE (exp_b_b, exp_i_b) ->
      eq_exp exp_b_a exp_b_b && eq_exp exp_i_a exp_i_b
  | SliceE (exp_b_a, exp_l_a, exp_h_a), SliceE (exp_b_b, exp_l_b, exp_h_b) ->
      eq_exp exp_b_a exp_b_b && eq_exp exp_l_a exp_l_b && eq_exp exp_h_a exp_h_b
  | UpdE (exp_b_a, path_a, exp_f_a), UpdE (exp_b_b, path_b, exp_f_b) ->
      eq_exp exp_b_a exp_b_b && eq_path path_a path_b && eq_exp exp_f_a exp_f_b
  | CallE (id_a, targs_a, args_a), CallE (id_b, targs_b, args_b) ->
      eq_id id_a id_b && eq_targs targs_a targs_b && eq_args args_a args_b
  | HoldE (id_a, (mixop_a, exps_a)), HoldE (id_b, (mixop_b, exps_b)) ->
      eq_id id_a id_b && eq_mixop mixop_a mixop_b && eq_exps exps_a exps_b
  | IterE (exp_a, iterexp_a), IterE (exp_b, iterexp_b) ->
      eq_exp exp_a exp_b && eq_iterexp iterexp_a iterexp_b
  | _ -> false

and eq_exps (exps_a : exp list) (exps_b : exp list) : bool =
  List.length exps_a = List.length exps_b && List.for_all2 eq_exp exps_a exps_b

and eq_iterexp (iterexp_a : iterexp) (iterexp_b : iterexp) : bool =
  let iter_a, vars_a = iterexp_a in
  let iter_b, vars_b = iterexp_b in
  eq_iter iter_a iter_b && eq_vars vars_a vars_b

and eq_iterexps (iterexps_a : iterexp list) (iterexps_b : iterexp list) : bool =
  List.length iterexps_a = List.length iterexps_b
  && List.for_all2 eq_iterexp iterexps_a iterexps_b

(* Patterns *)

and eq_pattern (pattern_a : pattern) (pattern_b : pattern) : bool =
  match (pattern_a, pattern_b) with
  | CaseP mixop_a, CaseP mixop_b -> eq_mixop mixop_a mixop_b
  | ListP `Cons, ListP `Cons -> true
  | ListP (`Fixed len_a), ListP (`Fixed len_b) -> len_a = len_b
  | ListP `Nil, ListP `Nil -> true
  | OptP `Some, OptP `Some -> true
  | OptP `None, OptP `None -> true
  | _ -> false

(* Paths *)

and eq_path (path_a : path) (path_b : path) : bool =
  match (path_a.it, path_b.it) with
  | RootP, RootP -> true
  | IdxP (path_a, exp_a), IdxP (path_b, exp_b) ->
      eq_path path_a path_b && eq_exp exp_a exp_b
  | SliceP (path_a, exp_l_a, exp_h_a), SliceP (path_b, exp_l_b, exp_h_b) ->
      eq_path path_a path_b && eq_exp exp_l_a exp_l_b && eq_exp exp_h_a exp_h_b
  | DotP (path_a, atom_a), DotP (path_b, atom_b) ->
      eq_path path_a path_b && Atom.eq atom_a atom_b
  | _ -> false

(* Arguments *)

and eq_arg (arg_a : arg) (arg_b : arg) : bool =
  match (arg_a.it, arg_b.it) with
  | ExpA exp_a, ExpA exp_b -> eq_exp exp_a exp_b
  | DefA id_a, DefA id_b -> eq_id id_a id_b
  | _ -> false

and eq_args (args_a : arg list) (args_b : arg list) : bool =
  List.length args_a = List.length args_b && List.for_all2 eq_arg args_a args_b

(* Type arguments *)

and eq_targ (targ_a : targ) (targ_b : targ) : bool = eq_typ targ_a targ_b

and eq_targs (targs_a : targ list) (targs_b : targ list) : bool =
  List.length targs_a = List.length targs_b
  && List.for_all2 eq_targ targs_a targs_b
