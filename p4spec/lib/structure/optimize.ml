open Domain.Lib
open Xl
open Sl.Ast
module TDEnv = Runtime_dynamic.Envs.TDEnv
open Util.Source

(* Renamer *)

module Rename = MakeIdEnv (Id)

let rename_iterexp (rename : Rename.t) (iterexp : iterexp) : iterexp =
  let iter, vars = iterexp in
  let vars =
    List.map
      (fun (id, iters) ->
        match Rename.find_opt id rename with
        | Some id_renamed -> (id_renamed, iters)
        | None -> (id, iters))
      vars
  in
  (iter, vars)

let rec rename_exp (rename : Rename.t) (exp : exp) : exp =
  let at, note = (exp.at, exp.note) in
  match exp.it with
  | BoolE _ | NumE _ | TextE _ -> exp
  | VarE id when Rename.mem id rename ->
      let id_renamed = Rename.find id rename in
      Il.Ast.VarE id_renamed $$ (at, note)
  | VarE _ -> exp
  | UnE (unop, optyp, exp) ->
      let exp = rename_exp rename exp in
      Il.Ast.UnE (unop, optyp, exp) $$ (at, note)
  | BinE (binop, optyp, exp_l, exp_r) ->
      let exp_l = rename_exp rename exp_l in
      let exp_r = rename_exp rename exp_r in
      Il.Ast.BinE (binop, optyp, exp_l, exp_r) $$ (at, note)
  | CmpE (cmpop, optyp, exp_l, exp_r) ->
      let exp_l = rename_exp rename exp_l in
      let exp_r = rename_exp rename exp_r in
      Il.Ast.CmpE (cmpop, optyp, exp_l, exp_r) $$ (at, note)
  | UpCastE (typ, exp) ->
      let exp = rename_exp rename exp in
      Il.Ast.UpCastE (typ, exp) $$ (at, note)
  | DownCastE (typ, exp) ->
      let exp = rename_exp rename exp in
      Il.Ast.DownCastE (typ, exp) $$ (at, note)
  | SubE (exp, typ) ->
      let exp = rename_exp rename exp in
      Il.Ast.SubE (exp, typ) $$ (at, note)
  | MatchE (exp, pattern) ->
      let exp = rename_exp rename exp in
      Il.Ast.MatchE (exp, pattern) $$ (at, note)
  | TupleE exps ->
      let exps = List.map (rename_exp rename) exps in
      Il.Ast.TupleE exps $$ (at, note)
  | CaseE (mixop, exps) ->
      let exps = List.map (rename_exp rename) exps in
      Il.Ast.CaseE (mixop, exps) $$ (at, note)
  | StrE expfields ->
      let atoms, exps = List.split expfields in
      let exps = List.map (rename_exp rename) exps in
      let expfields = List.combine atoms exps in
      Il.Ast.StrE expfields $$ (at, note)
  | OptE exp_opt ->
      let exp_opt = Option.map (rename_exp rename) exp_opt in
      Il.Ast.OptE exp_opt $$ (at, note)
  | ListE exps ->
      let exps = List.map (rename_exp rename) exps in
      Il.Ast.ListE exps $$ (at, note)
  | ConsE (exp_h, exp_t) ->
      let exp_h = rename_exp rename exp_h in
      let exp_t = rename_exp rename exp_t in
      Il.Ast.ConsE (exp_h, exp_t) $$ (at, note)
  | CatE (exp_l, exp_r) ->
      let exp_l = rename_exp rename exp_l in
      let exp_r = rename_exp rename exp_r in
      Il.Ast.CatE (exp_l, exp_r) $$ (at, note)
  | MemE (exp_e, exp_s) ->
      let exp_e = rename_exp rename exp_e in
      let exp_s = rename_exp rename exp_s in
      Il.Ast.MemE (exp_e, exp_s) $$ (at, note)
  | LenE exp ->
      let exp = rename_exp rename exp in
      Il.Ast.LenE exp $$ (at, note)
  | DotE (exp, atom) ->
      let exp = rename_exp rename exp in
      Il.Ast.DotE (exp, atom) $$ (at, note)
  | IdxE (exp_b, exp_i) ->
      let exp_b = rename_exp rename exp_b in
      let exp_i = rename_exp rename exp_i in
      Il.Ast.IdxE (exp_b, exp_i) $$ (at, note)
  | SliceE (exp_b, exp_l, exp_h) ->
      let exp_b = rename_exp rename exp_b in
      let exp_l = rename_exp rename exp_l in
      let exp_h = rename_exp rename exp_h in
      Il.Ast.SliceE (exp_b, exp_l, exp_h) $$ (at, note)
  | UpdE (exp_b, path, exp_f) ->
      let exp_b = rename_exp rename exp_b in
      let path = rename_path rename path in
      let exp_f = rename_exp rename exp_f in
      Il.Ast.UpdE (exp_b, path, exp_f) $$ (at, note)
  | CallE (id, targs, args) ->
      let args = List.map (rename_arg rename) args in
      Il.Ast.CallE (id, targs, args) $$ (at, note)
  | HoldE (id, (mixop, exps)) ->
      let exps = List.map (rename_exp rename) exps in
      Il.Ast.HoldE (id, (mixop, exps)) $$ (at, note)
  | IterE (exp, iterexp) ->
      let exp = rename_exp rename exp in
      let iterexp = rename_iterexp rename iterexp in
      Il.Ast.IterE (exp, iterexp) $$ (at, note)

and rename_path (rename : Rename.t) (path : path) : path =
  let at, note = (path.at, path.note) in
  match path.it with
  | RootP -> path
  | IdxP (path, exp) ->
      let path = rename_path rename path in
      let exp = rename_exp rename exp in
      Il.Ast.IdxP (path, exp) $$ (at, note)
  | SliceP (path, exp_l, exp_h) ->
      let path = rename_path rename path in
      let exp_l = rename_exp rename exp_l in
      let exp_h = rename_exp rename exp_h in
      Il.Ast.SliceP (path, exp_l, exp_h) $$ (at, note)
  | DotP (path, atom) ->
      let path = rename_path rename path in
      Il.Ast.DotP (path, atom) $$ (at, note)

and rename_arg (rename : Rename.t) (arg : arg) : arg =
  let at = arg.at in
  match arg.it with
  | ExpA exp ->
      let exp = rename_exp rename exp in
      Il.Ast.ExpA exp $ at
  | DefA _ -> arg

and rename_instr (rename : Rename.t) (instr : instr) : instr =
  let at = instr.at in
  match instr.it with
  | RuleI (id_rel, (mixop, exps), iterexps) ->
      let exps = List.map (rename_exp rename) exps in
      let iterexps = List.map (rename_iterexp rename) iterexps in
      RuleI (id_rel, (mixop, exps), iterexps) $ at
  | IfI (exp_cond, iterexps, instrs_then, instrs_else) ->
      let exp_cond = rename_exp rename exp_cond in
      let iterexps = List.map (rename_iterexp rename) iterexps in
      let instrs_then = List.map (rename_instr rename) instrs_then in
      let instrs_else = List.map (rename_instr rename) instrs_else in
      IfI (exp_cond, iterexps, instrs_then, instrs_else) $ at
  | OtherwiseI instr ->
      let instr = rename_instr rename instr in
      OtherwiseI instr $ at
  | LetI (exp_l, exp_r, iterexps) ->
      let exp_l = rename_exp rename exp_l in
      let exp_r = rename_exp rename exp_r in
      let iterexps = List.map (rename_iterexp rename) iterexps in
      LetI (exp_l, exp_r, iterexps) $ at
  | ResultI exps ->
      let exps = List.map (rename_exp rename) exps in
      ResultI exps $ at
  | ReturnI exp ->
      let exp = rename_exp rename exp in
      ReturnI exp $ at

(* Remove redundant, trivial let aliases from the code,

   let y = x; if (y == 0) then { let z = y + y; let y = 1; let k = y + y; ... }

    will be transformed into

    if (x == 0) then { let z = x + x; let y = 1; let k = y + y; ... }

    Notice the stop condition when we meet a shadowing let binding *)

let rec rename_let_alias (rename : Rename.t) (instrs : instr list) : instr list
    =
  match instrs with
  | [] -> []
  | instr_h :: instrs_t -> (
      match instr_h.it with
      | LetI ({ it = VarE id_l; _ }, _, _) when Rename.mem id_l rename ->
          instr_h :: instrs_t
      | _ ->
          let instr_h = rename_instr rename instr_h in
          let instrs_t = rename_let_alias rename instrs_t in
          instr_h :: instrs_t)

let rec remove_let_alias (instrs : instr list) : instr list =
  match instrs with
  | [] -> []
  | instr_h :: instrs_t -> (
      match instr_h.it with
      | LetI ({ it = VarE id_l; _ }, { it = VarE id_r; _ }, []) ->
          let rename = Rename.singleton id_l id_r in
          instrs_t |> rename_let_alias rename |> remove_let_alias
      | _ ->
          let instrs_t = remove_let_alias instrs_t in
          instr_h :: instrs_t)

(* Remove redundant let and rule bindings from the code,
   which appears due to the concatenation of multiple rules and clauses
   This operation is safe because IL is already in SSA form *)

let rec remove_redundant_binding_instr (instrs_seen : instr list)
    (instr : instr) : instr list * instr option =
  match instr.it with
  | RuleI _ | LetI _ ->
      if List.exists (Sl.Eq.eq_instr instr) instrs_seen then (instrs_seen, None)
      else (instrs_seen @ [ instr ], Some instr)
  | IfI (exp_cond, iterexps, instrs_then, instrs_else) ->
      let instrs_then =
        remove_redundant_binding_instrs ~instrs_seen instrs_then
      in
      let instrs_else =
        remove_redundant_binding_instrs ~instrs_seen instrs_else
      in
      let instr =
        IfI (exp_cond, iterexps, instrs_then, instrs_else) $ instr.at
      in
      (instrs_seen, Some instr)
  | _ -> (instrs_seen, Some instr)

and remove_redundant_binding_instrs ?(instrs_seen : instr list = [])
    (instrs : instr list) : instr list =
  List.fold_left
    (fun (instrs_seen, instrs) instr ->
      let instrs_seen, instr_opt =
        remove_redundant_binding_instr instrs_seen instr
      in
      let instrs =
        match instr_opt with Some instr -> instrs @ [ instr ] | None -> instrs
      in
      (instrs_seen, instrs))
    (instrs_seen, []) instrs
  |> snd

(* Syntactic analysis of conditions

   Note that this is best-effort analysis,
   since even semantic analysis cannot guarantee completeness of the analysis *)

type overlap = Identical | Disjoint | Mutex | Fuzzy

let overlap_pattern (pattern_a : pattern) (pattern_b : pattern) : overlap =
  match (pattern_a, pattern_b) with
  | CaseP mixop_a, CaseP mixop_b ->
      if Sl.Eq.eq_mixop mixop_a mixop_b then Identical else Disjoint
  | ListP `Cons, ListP (`Fixed n) | ListP (`Fixed n), ListP `Cons ->
      if n = 0 then Mutex else Disjoint
  | ListP `Cons, ListP `Nil | ListP `Nil, ListP `Cons -> Mutex
  | ListP (`Fixed n_a), ListP (`Fixed n_b) ->
      if n_a = n_b then Identical else Disjoint
  | ListP (`Fixed n), ListP `Nil | ListP `Nil, ListP (`Fixed n) ->
      if n = 0 then Identical else Disjoint
  | ListP `Nil, ListP `Nil -> Identical
  | OptP `Some, OptP `Some -> Identical
  | OptP `Some, OptP `None | OptP `None, OptP `Some -> Mutex
  | OptP `None, OptP `None -> Identical
  | _ -> assert false

let rec distinct_exp_literal (exp_a : exp) (exp_b : exp) : bool =
  match (exp_a.it, exp_b.it) with
  | BoolE b_a, BoolE b_b -> b_a <> b_b
  | NumE n_a, NumE n_b -> not (Num.eq n_a n_b)
  | TextE t_a, TextE t_b -> t_a <> t_b
  | TupleE exps_a, TupleE exps_b ->
      assert (List.length exps_a = List.length exps_b);
      List.exists2 distinct_exp_literal exps_a exps_b
  | ListE exps_a, ListE exps_b when List.length exps_a = List.length exps_b ->
      List.exists2 distinct_exp_literal exps_a exps_b
  | ListE _, ListE _ -> true
  | _ -> false

let overlap_exp (exp_a : exp) (exp_b : exp) : overlap =
  match (exp_a.it, exp_b.it) with
  (* Negation *)
  | UnE (`NotOp, _, exp_a), _ when Sl.Eq.eq_exp exp_a exp_b -> Mutex
  | _, UnE (`NotOp, _, exp_b) when Sl.Eq.eq_exp exp_a exp_b -> Mutex
  (* Equals *)
  | ( CmpE (`EqOp, optyp_a, exp_a_l, exp_a_r),
      CmpE (`EqOp, optyp_b, exp_b_l, exp_b_r) )
    when optyp_a = optyp_b
         && (Sl.Eq.eq_exp exp_a_l exp_b_l
             && distinct_exp_literal exp_a_r exp_b_r
            || Sl.Eq.eq_exp exp_a_l exp_b_r
               && distinct_exp_literal exp_a_r exp_b_l) ->
      Disjoint
  (* Equals and Not Equals *)
  | ( CmpE (`EqOp, optyp_a, exp_a_l, exp_a_r),
      CmpE (`NeOp, optyp_b, exp_b_l, exp_b_r) )
  | ( CmpE (`NeOp, optyp_a, exp_a_l, exp_a_r),
      CmpE (`EqOp, optyp_b, exp_b_l, exp_b_r) )
    when optyp_a = optyp_b
         && ((Sl.Eq.eq_exp exp_a_l exp_b_l && Sl.Eq.eq_exp exp_a_r exp_b_r)
            || (Sl.Eq.eq_exp exp_a_l exp_b_r && Sl.Eq.eq_exp exp_a_r exp_b_l))
    ->
      Mutex
  (* Match on patterns *)
  | MatchE (exp_a, pattern_a), MatchE (exp_b, pattern_b)
    when Sl.Eq.eq_exp exp_a exp_b ->
      overlap_pattern pattern_a pattern_b
  | _ -> Fuzzy

(* Merge if statements with the same condition *)

let rec merge_block (instrs_a : instr list) (instrs_b : instr list) : instr list
    =
  match (instrs_a, instrs_b) with
  | instr_a :: instrs_a, instr_b :: instrs_b when Sl.Eq.eq_instr instr_a instr_b
    ->
      let instrs = merge_block instrs_a instrs_b in
      instr_a :: instrs
  | _ -> instrs_a @ instrs_b

let rec find_identical_if ?(instrs_unmergeable : instr list = [])
    (exp_cond_target : exp) (iterexps_target : iterexp list)
    (instrs : instr list) : (instr list * instr list * instr list) option =
  match instrs with
  | { it = IfI (exp_cond, iterexps, instrs_then, instrs_else); _ } :: instrs_t
    when Sl.Eq.eq_exp exp_cond exp_cond_target
         && Sl.Eq.eq_iterexps iterexps iterexps_target ->
      let instrs_unmergeable = instrs_unmergeable @ instrs_t in
      Some (instrs_unmergeable, instrs_then, instrs_else)
  | ({ it = IfI _; _ } as instr_h) :: instrs_t ->
      let instrs_unmergeable = instrs_unmergeable @ [ instr_h ] in
      find_identical_if ~instrs_unmergeable exp_cond_target iterexps_target
        instrs_t
  | _ -> None

let rec merge_if (instrs : instr list) : instr list =
  match instrs with
  | [] -> []
  | ({ it = IfI (exp_cond, iterexps, instrs_then, instrs_else); _ } as instr_h)
    :: instrs_t -> (
      match find_identical_if exp_cond iterexps instrs_t with
      | Some (instrs_unmergeable, instrs_then_matched, instrs_else_matched) ->
          let instrs_then = merge_block instrs_then instrs_then_matched in
          let instrs_else = merge_block instrs_else instrs_else_matched in
          let instr_h =
            IfI (exp_cond, iterexps, instrs_then, instrs_else) $ instr_h.at
          in
          let instrs = instr_h :: instrs_unmergeable in
          merge_if instrs
      | None ->
          let instrs_then = merge_if instrs_then in
          let instrs_else = merge_if instrs_else in
          let instr_h =
            IfI (exp_cond, iterexps, instrs_then, instrs_else) $ instr_h.at
          in
          let instrs_t = merge_if instrs_t in
          instr_h :: instrs_t)
  | instr_h :: instrs_t ->
      let instrs_t = merge_if instrs_t in
      instr_h :: instrs_t

(* Insert else branches for disjoint if conditions

   (1) Disjoint conditions

    - if (i >= 0) then (... a ...) else (... b ...)
    - if (i = 0) then (... c ...) else (empty)

    is merged into,

    - if (i >= 0) then (... a ...)
      else
        (... b ...)
        if (i = 0) then (... c ...)

   (2) Mutex conditions

    - if (i >= 0) then (... a ...) else (... b ...)
    - if (i < 0) then (... c ...) else (... d ...)

    is merged into,

    - if (i >= 0) then
        (... a ...)
        (... d ...)
      else
        (... b ...)
        (... c ...) *)

type if_couple =
  | DisjointC of instr list * instr
  | MutexC of instr list * instr list * instr list

let rec find_disjoint_if ?(instrs_unmergeable : instr list = [])
    (exp_cond_target : exp) (iterexps_target : iterexp list)
    (instrs : instr list) : if_couple option =
  match instrs with
  | ({ it = IfI (exp_cond, iterexps, instrs_then, instrs_else); _ } as instr_h)
    :: instrs_t -> (
      let eq_iterexps = Sl.Eq.eq_iterexps iterexps iterexps_target in
      let overlap_exp_cond = overlap_exp exp_cond exp_cond_target in
      match (eq_iterexps, overlap_exp_cond) with
      | true, Disjoint when instrs_else = [] ->
          let instrs_unmergeable = instrs_unmergeable @ instrs_t in
          let if_couple = DisjointC (instrs_unmergeable, instr_h) in
          Some if_couple
      | true, Mutex ->
          let instrs_unmergeable = instrs_unmergeable @ instrs_t in
          let if_couple =
            MutexC (instrs_unmergeable, instrs_then, instrs_else)
          in
          Some if_couple
      | _ ->
          let instrs_unmergeable = instrs_unmergeable @ [ instr_h ] in
          find_disjoint_if ~instrs_unmergeable exp_cond_target iterexps_target
            instrs_t)
  | _ -> None

let rec merge_else (instrs : instr list) : instr list =
  match instrs with
  | [] -> []
  | ({ it = IfI (exp_cond, iterexps, instrs_then, instrs_else); _ } as instr_h)
    :: instrs_t -> (
      match find_disjoint_if exp_cond iterexps instrs_t with
      | Some (DisjointC (instrs_unmergeable, instr_matched)) ->
          let instrs_else = merge_block instrs_else [ instr_matched ] in
          let instr_h =
            IfI (exp_cond, iterexps, instrs_then, instrs_else) $ instr_h.at
          in
          let instrs = instr_h :: instrs_unmergeable in
          merge_else instrs
      | Some
          (MutexC
            (instrs_unmergeable, instrs_then_matched, instrs_else_matched)) ->
          let instrs_then = merge_block instrs_then instrs_else_matched in
          let instrs_else = merge_block instrs_else instrs_then_matched in
          let instr_h =
            IfI (exp_cond, iterexps, instrs_then, instrs_else) $ instr_h.at
          in
          let instrs = instr_h :: instrs_unmergeable in
          merge_else instrs
      | None ->
          let instrs_then = merge_else instrs_then in
          let instrs_else = merge_else instrs_else in
          let instr_h =
            IfI (exp_cond, iterexps, instrs_then, instrs_else) $ instr_h.at
          in
          let instrs_t = merge_else instrs_t in
          instr_h :: instrs_t)
  | instr_h :: instrs_t ->
      let instrs_t = merge_else instrs_t in
      instr_h :: instrs_t

(* Convert last else if in an if chain into an else branch
   when the branch conditions are total

   For now, it only takes case matches into account

   syntax foo = | AAA | BBB | CCC

   if (foo matches AAA)
   else if (foo matches BBB)
   else if (foo matches CCC)

   is converted into

   if (foo matches AAA)
   else if (foo matches BBB)
   else *)

let rec find_chain_if ?(exps_cond : exp list = []) (instr : instr) : exp list =
  match instr.it with
  | IfI (exp_cond, [], _, [ ({ it = IfI _; _ } as instr_else) ]) ->
      let exps_cond = exps_cond @ [ exp_cond ] in
      find_chain_if ~exps_cond instr_else
  | IfI (exp_cond, [], _, []) -> exps_cond @ [ exp_cond ]
  | _ -> []

let find_case_analysis_if (exp_cond : exp) : (exp * mixop) option =
  match exp_cond.it with
  | MatchE (exp, CaseP mixop) -> Some (exp, mixop)
  | _ -> None

let find_case_analysis_chain_if (exp_cond_h : exp) (exps_cond_t : exp list) :
    (typ * mixop list) option =
  match find_case_analysis_if exp_cond_h with
  | Some (exp_h, mixop_h) ->
      let exps_t, mixops_t =
        List.map find_case_analysis_if exps_cond_t
        |> List.filter_map Fun.id |> List.split
      in
      if
        List.length exps_cond_t = List.length exps_t
        && List.for_all (Sl.Eq.eq_exp exp_h) exps_t
      then
        let typ = exp_h.note $ exp_h.at in
        let mixops = mixop_h :: mixops_t in
        Some (typ, mixops)
      else None
  | None -> None

let rec find_case_analysis_typ (tdenv : TDEnv.t) (typ : typ) : mixop list =
  match typ.it with
  | VarT (tid, _) -> (
      let _, deftyp = TDEnv.find tid tdenv in
      match deftyp.it with
      | PlainT typ -> find_case_analysis_typ tdenv typ
      | VariantT typcases -> typcases |> List.map it |> List.map fst
      | _ -> assert false)
  | _ -> assert false

let is_total_chain_if (tdenv : TDEnv.t) (exps_cond : exp list) : bool =
  assert (List.length exps_cond > 1);
  let exp_cond_h, exps_cond_t = (List.hd exps_cond, List.tl exps_cond) in
  match find_case_analysis_chain_if exp_cond_h exps_cond_t with
  | Some (typ, mixops) ->
      let mixops_typ = find_case_analysis_typ tdenv typ in
      let module Set = Set.Make (Mixop) in
      let mixops = Set.of_list mixops in
      let mixops_typ = Set.of_list mixops_typ in
      Set.equal mixops mixops_typ
  | None -> false

let rec totalize_chain_if' (instr : instr) : instr =
  let at = instr.at in
  match instr.it with
  | IfI
      ( exp_cond,
        [],
        instrs_then,
        [ { it = IfI (_, [], instrs_else_then, []); _ } ] ) ->
      IfI (exp_cond, [], instrs_then, instrs_else_then) $ at
  | IfI (exp_cond, [], instrs_then, [ ({ it = IfI _; _ } as instr_else) ]) ->
      let instr_else = totalize_chain_if' instr_else in
      IfI (exp_cond, [], instrs_then, [ instr_else ]) $ at
  | _ -> assert false

let rec totalize_chain_if (tdenv : TDEnv.t) (instrs : instr list) : instr list =
  match instrs with
  | [] -> []
  | ({ it = IfI (exp_cond, iterexps, instrs_then, instrs_else); _ } as instr_h)
    :: instrs_t -> (
      match find_chain_if instr_h with
      | exps_cond
        when List.length exps_cond > 1 && is_total_chain_if tdenv exps_cond ->
          let instr_h = totalize_chain_if' instr_h in
          let instrs_t = totalize_chain_if tdenv instrs_t in
          let instrs = instr_h :: instrs_t in
          totalize_chain_if tdenv instrs
      | _ ->
          let instrs_then = totalize_chain_if tdenv instrs_then in
          let instrs_else = totalize_chain_if tdenv instrs_else in
          let instr_h =
            IfI (exp_cond, iterexps, instrs_then, instrs_else) $ instr_h.at
          in
          let instrs_t = totalize_chain_if tdenv instrs_t in
          instr_h :: instrs_t)
  | instr_h :: instrs_t ->
      let instrs_t = totalize_chain_if tdenv instrs_t in
      instr_h :: instrs_t

(* Apply optimizations until it reaches a fixed point *)

let rec optimize' (tdenv : TDEnv.t) (instrs : instr list) : instr list =
  let instrs_optimized =
    instrs |> remove_redundant_binding_instrs |> merge_if |> merge_else
    |> totalize_chain_if tdenv
  in
  if Sl.Eq.eq_instrs instrs instrs_optimized then instrs
  else optimize' tdenv instrs_optimized

let optimize (tdenv : TDEnv.t) (instrs : instr list) : instr list =
  instrs |> remove_let_alias |> optimize' tdenv
