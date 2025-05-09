open Domain.Lib
open Xl
open Ol.Ast
module Hint = Runtime_static.Rel.Hint
module HEnv = Runtime_static.Envs.HEnv
module TDEnv = Runtime_dynamic_sl.Envs.TDEnv
open Util.Source

(* [1] Remove redundant, trivial let aliases from the code,

   let y = x; if (y == 0) then { let z = y + y; let y = 1; let k = y + y; ... }

   will be transformed into

   if (x == 0) then { let z = x + x; let y = 1; let k = y + y; ... }

   Notice the stop condition when we meet a shadowing let binding *)

let rec rename_let_alias (rename : Renamer.t) (instrs : instr list) : instr list
    =
  match instrs with
  | [] -> []
  | instr_h :: instrs_t -> (
      match instr_h.it with
      | LetI ({ it = VarE id_l; _ }, _, _) when Renamer.Rename.mem id_l rename
        ->
          instr_h :: instrs_t
      | _ ->
          let instr_h = Renamer.rename_instr rename instr_h in
          let instrs_t = rename_let_alias rename instrs_t in
          instr_h :: instrs_t)

let rec remove_let_alias (instrs : instr list) : instr list =
  match instrs with
  | [] -> []
  | instr_h :: instrs_t -> (
      match instr_h.it with
      | LetI ({ it = VarE id_l; _ }, { it = VarE id_r; _ }, _) ->
          let rename = Renamer.Rename.singleton id_l id_r in
          instrs_t |> rename_let_alias rename |> remove_let_alias
      | _ ->
          let instrs_t = remove_let_alias instrs_t in
          instr_h :: instrs_t)

(* [2] Parallelize if conditions in logical or *)

let rec parallelize_exp_disjunction (iterexps : iterexp list) (exp : exp) :
    exp list option =
  match iterexps with [] -> parallelize_exp_disjunction' exp | _ -> None

and parallelize_exp_disjunction' (exp : exp) : exp list option =
  match exp.it with
  | BinE (`OrOp, _, exp_l, exp_r) -> (
      let exps_l = parallelize_exp_disjunction' exp_l in
      let exps_r = parallelize_exp_disjunction' exp_r in
      match (exps_l, exps_r) with
      | Some exps_l, Some exps_r -> Some (exps_l @ exps_r)
      | Some exps_l, None -> Some (exps_l @ [ exp_r ])
      | None, Some exps_r -> Some (exps_r @ [ exp_l ])
      | None, None -> Some [ exp_l; exp_r ])
  | _ -> None

let rec parallelize_if_disjunction (instr : instr) : instr list =
  let at = instr.at in
  match instr.it with
  | IfI (exp_cond, iterexps, instrs_then) -> (
      let instrs_then = parallelize_if_disjunctions instrs_then in
      match parallelize_exp_disjunction iterexps exp_cond with
      | Some exps_cond ->
          List.map
            (fun exp_cond -> IfI (exp_cond, iterexps, instrs_then) $ at)
            exps_cond
      | None -> [ instr ])
  | _ -> [ instr ]

and parallelize_if_disjunctions (instrs : instr list) : instr list =
  List.concat_map parallelize_if_disjunction instrs

(* [3] Matchify equals terminal *)

let matchify_exp_eq_terminal (exp : exp) : exp =
  let at, note = (exp.at, exp.note) in
  match exp.it with
  | CmpE (`EqOp, _, exp_l, { it = CaseE (mixop, []); _ }) ->
      Il.Ast.MatchE (exp_l, CaseP mixop) $$ (at, note)
  | CmpE (`NeOp, _, exp_l, { it = CaseE (mixop, []); _ }) ->
      let exp = Il.Ast.MatchE (exp_l, CaseP mixop) $$ (at, note) in
      Il.Ast.UnE (`NotOp, `BoolT, exp) $$ (at, note)
  | _ -> exp

let rec matchify_if_eq_terminal (instr : instr) : instr =
  let at = instr.at in
  match instr.it with
  | IfI (exp_cond, iterexps, instrs_then) ->
      let exp_cond = matchify_exp_eq_terminal exp_cond in
      let instrs_then = matchify_if_eq_terminals instrs_then in
      IfI (exp_cond, iterexps, instrs_then) $ at
  | _ -> instr

and matchify_if_eq_terminals (instrs : instr list) : instr list =
  List.map matchify_if_eq_terminal instrs

(* [4] Remove redundant let and rule bindings from the code,
   which appears due to the concatenation of multiple rules and clauses
   This operation is safe because IL is already in SSA form *)

module Bind = struct
  (* Expression unit *)

  type expunit = exp * iterexp list

  let string_of_expunit (expunit : expunit) : string =
    let exp, iterexps = expunit in
    Format.asprintf "(%s)%s"
      (Sl.Print.string_of_exp exp)
      (Sl.Print.string_of_iterexps iterexps)

  let string_of_expunits (expunits : expunit list) : string =
    String.concat ", " (List.map string_of_expunit expunits)

  let eq_expunit (expunit_a : expunit) (expunit_b : expunit) : bool =
    let exp_a, iterexps_a = expunit_a in
    let exp_b, iterexps_b = expunit_b in
    Sl.Eq.eq_exp exp_a exp_b && Sl.Eq.eq_iterexps iterexps_a iterexps_b

  let eq_expunits (expunits_a : expunit list) (expunits_b : expunit list) : bool
      =
    List.length expunits_a = List.length expunits_b
    && List.for_all2 eq_expunit expunits_a expunits_b

  (* Binding *)

  type t =
    | LetBind of expunit * expunit
    | RuleBind of id * expunit list * expunit list

  (* Constructors *)

  let init_expunit (exp : exp) (iterexps : iterexp list) : expunit =
    let ids = Il.Free.free_exp exp in
    let iterexps =
      List.map
        (fun (iter, vars) ->
          let vars = List.filter (fun (id, _, _) -> IdSet.mem id ids) vars in
          (iter, vars))
        iterexps
    in
    (exp, iterexps)

  let init_let_bind (exp_l : exp) (exp_r : exp) (iterexps : iterexp list) : t =
    let expunit_l = init_expunit exp_l iterexps in
    let expunit_r = init_expunit exp_r iterexps in
    LetBind (expunit_l, expunit_r)

  let init_rule_bind (henv : HEnv.t) (id : id) (notexp : notexp)
      (iterexps : iterexp list) : t =
    let exps_l, exps_r =
      let _, exps = notexp in
      let inputs = HEnv.find id henv in
      Hint.split_exps_without_idx inputs exps
    in
    let expunits_l =
      List.map (fun exp_l -> init_expunit exp_l iterexps) exps_l
    in
    let expunits_r =
      List.map (fun exp_r -> init_expunit exp_r iterexps) exps_r
    in
    RuleBind (id, expunits_l, expunits_r)

  (* Collapsing two bindings,
     if two bindings have syntactically equal right-hand sides,
     and the left-hand sides are equal up to renaming,
     then we can collapse them into a single binding *)

  let rec collapse_exp (rename : Renamer.t) (exp : exp) (exp_target : exp) :
      Renamer.t option =
    match (exp.it, exp_target.it) with
    | VarE id, VarE id_target ->
        let rename =
          if Sl.Eq.eq_id id id_target then rename
          else Renamer.Rename.add id_target id rename
        in
        Some rename
    | TupleE exps, TupleE exps_target -> collapse_exps rename exps exps_target
    | CaseE (mixop, exps), CaseE (mixop_target, exps_target)
      when Sl.Eq.eq_mixop mixop mixop_target ->
        collapse_exps rename exps exps_target
    | StrE expfields, StrE expfields_target ->
        let atoms, exps = List.split expfields in
        let atoms_target, exps_target = List.split expfields_target in
        if Sl.Eq.eq_atoms atoms atoms_target then
          collapse_exps rename exps exps_target
        else None
    | IterE (exp, iterexp), IterE (exp_target, iterexp_target) -> (
        match collapse_exp rename exp exp_target with
        | Some rename ->
            let iterexp_target_renamed =
              Renamer.rename_iterexp rename iterexp_target
            in
            if Sl.Eq.eq_iterexp iterexp iterexp_target_renamed then Some rename
            else None
        | None -> None)
    | _ -> None

  and collapse_exps (rename : Renamer.t) (exps : exp list)
      (exps_target : exp list) : Renamer.t option =
    match (exps, exps_target) with
    | [], [] -> Some rename
    | exp_h :: exps_t, exp_target_h :: exps_target_t -> (
        match collapse_exp rename exp_h exp_target_h with
        | Some rename -> collapse_exps rename exps_t exps_target_t
        | None -> None)
    | _ -> None

  let collapse_expunit (rename : Renamer.t) (expunit : expunit)
      (expunit_target : expunit) : Renamer.t option =
    let exp, iterexps = expunit in
    let exp_target, iterexps_target = expunit_target in
    let rename_opt = collapse_exp rename exp exp_target in
    match rename_opt with
    | Some rename ->
        let iterexps_target_renamed =
          Renamer.rename_iterexps rename iterexps_target
        in
        if Sl.Eq.eq_iterexps iterexps iterexps_target_renamed then Some rename
        else None
    | None -> None

  let rec collapse_expunits (rename : Renamer.t) (expunits : expunit list)
      (expunits_target : expunit list) : Renamer.t option =
    match (expunits, expunits_target) with
    | [], [] -> Some Renamer.Rename.empty
    | expunit_h :: expunits_t, expunit_target_h :: expunits_target_t -> (
        match collapse_expunit rename expunit_h expunit_target_h with
        | Some rename -> collapse_expunits rename expunits_t expunits_target_t
        | None -> None)
    | _ -> None

  let collapse_bind (bind : t) (bind_target : t) : Renamer.t option =
    match (bind, bind_target) with
    | ( LetBind (expunit_l, expunit_r),
        LetBind (expunit_target_l, expunit_target_r) )
      when eq_expunit expunit_r expunit_target_r ->
        collapse_expunit Renamer.Rename.empty expunit_l expunit_target_l
    | ( RuleBind (id, expunits_l, expunits_r),
        RuleBind (id_target, expunits_target_l, expunits_target_r) )
      when Sl.Eq.eq_id id id_target && eq_expunits expunits_r expunits_target_r
      ->
        collapse_expunits Renamer.Rename.empty expunits_l expunits_target_l
    | _ -> None
end

let rec remove_redundant_bindings' (henv : HEnv.t) (bind : Bind.t)
    (instrs : instr list) : instr list =
  match instrs with
  | [] -> []
  | { it = IfI (exp_cond, iterexps, instrs_then); at; _ } :: instrs_t ->
      let instrs_then = instrs_then |> remove_redundant_bindings' henv bind in
      let instr_h = IfI (exp_cond, iterexps, instrs_then) $ at in
      let instrs_t = remove_redundant_bindings' henv bind instrs_t in
      instr_h :: instrs_t
  | { it = CaseI (exp, cases, total); at; _ } :: instrs_t ->
      let cases =
        let guards, blocks = List.split cases in
        let blocks = List.map (remove_redundant_bindings' henv bind) blocks in
        List.combine guards blocks
      in
      let instr_h = CaseI (exp, cases, total) $ at in
      let instrs_t = remove_redundant_bindings' henv bind instrs_t in
      instr_h :: instrs_t
  | ({ it = LetI (exp_l, exp_r, iterexps); _ } as instr_h) :: instrs_t -> (
      let bind_target = Bind.init_let_bind exp_l exp_r iterexps in
      let rename_opt = Bind.collapse_bind bind bind_target in
      match rename_opt with
      | Some rename ->
          instrs_t
          |> Renamer.rename_instrs rename
          |> remove_redundant_bindings' henv bind
      | None ->
          let instrs_t = remove_redundant_bindings' henv bind instrs_t in
          instr_h :: instrs_t)
  | ({ it = RuleI (id, notexp, iterexps); _ } as instr_h) :: instrs_t -> (
      let bind_target = Bind.init_rule_bind henv id notexp iterexps in
      let rename_opt = Bind.collapse_bind bind bind_target in
      match rename_opt with
      | Some rename ->
          instrs_t
          |> Renamer.rename_instrs rename
          |> remove_redundant_bindings' henv bind
      | None ->
          let instrs_t = remove_redundant_bindings' henv bind instrs_t in
          instr_h :: instrs_t)
  | instr_h :: instrs_t ->
      let instrs_t = remove_redundant_bindings' henv bind instrs_t in
      instr_h :: instrs_t

let rec remove_redundant_bindings (henv : HEnv.t) (instrs : instr list) :
    instr list =
  match instrs with
  | [] -> []
  | { it = IfI (exp_cond, iterexps, instrs_then); at; _ } :: instrs_t ->
      let instrs_then = instrs_then |> remove_redundant_bindings henv in
      let instr_h = IfI (exp_cond, iterexps, instrs_then) $ at in
      let instrs_t = remove_redundant_bindings henv instrs_t in
      instr_h :: instrs_t
  | { it = CaseI (exp, cases, total); at; _ } :: instrs_t ->
      let cases =
        let guards, blocks = List.split cases in
        let blocks = List.map (remove_redundant_bindings henv) blocks in
        List.combine guards blocks
      in
      let instr_h = CaseI (exp, cases, total) $ at in
      let instrs_t = remove_redundant_bindings henv instrs_t in
      instr_h :: instrs_t
  | ({ it = LetI (exp_l, exp_r, iterexps); _ } as instr_h) :: instrs_t ->
      let bind = Bind.init_let_bind exp_l exp_r iterexps in
      let instrs_t =
        instrs_t
        |> remove_redundant_bindings' henv bind
        |> remove_redundant_bindings henv
      in
      instr_h :: instrs_t
  | ({ it = RuleI (id, notexp, iterexps); _ } as instr_h) :: instrs_t ->
      let bind = Bind.init_rule_bind henv id notexp iterexps in
      let instrs_t =
        instrs_t
        |> remove_redundant_bindings' henv bind
        |> remove_redundant_bindings henv
      in
      instr_h :: instrs_t
  | instr_h :: instrs_t ->
      let instrs_t = instrs_t |> remove_redundant_bindings henv in
      instr_h :: instrs_t

(* [5] Condition analysis and case analysis insertion *)

let rec merge_block (instrs_a : instr list) (instrs_b : instr list) : instr list
    =
  match (instrs_a, instrs_b) with
  | instr_a :: instrs_a, instr_b :: instrs_b when Ol.Eq.eq_instr instr_a instr_b
    ->
      let instrs = merge_block instrs_a instrs_b in
      instr_a :: instrs
  | _ -> instrs_a @ instrs_b

(* Syntactic analysis of conditions

   Note that this is best-effort analysis,
   since even semantic analysis cannot guarantee completeness of the analysis *)

(* Conversion between guard and conditional expression *)

let exp_as_guard (exp_target : exp) (exp_cond : exp) : guard option =
  match exp_cond.it with
  | UnE (`NotOp, _, exp) when Sl.Eq.eq_exp exp_target exp -> Some (BoolG false)
  | CmpE (`EqOp, optyp, exp_l, exp_r) when Sl.Eq.eq_exp exp_target exp_l ->
      Some (CmpG (`EqOp, optyp, exp_r))
  | CmpE (`EqOp, optyp, exp_l, exp_r) when Sl.Eq.eq_exp exp_target exp_r ->
      Some (CmpG (`EqOp, optyp, exp_l))
  | CmpE (`NeOp, optyp, exp_l, exp_r) when Sl.Eq.eq_exp exp_target exp_l ->
      Some (CmpG (`NeOp, optyp, exp_r))
  | CmpE (`NeOp, optyp, exp_l, exp_r) when Sl.Eq.eq_exp exp_target exp_r ->
      Some (CmpG (`NeOp, optyp, exp_l))
  | SubE (exp, typ) when Sl.Eq.eq_exp exp_target exp -> Some (SubG typ)
  | MatchE (exp, pattern) when Sl.Eq.eq_exp exp_target exp ->
      Some (MatchG pattern)
  | MemE (exp_e, exp_s) when Sl.Eq.eq_exp exp_target exp_e -> Some (MemG exp_s)
  | _ -> None

let guard_as_exp (exp_target : exp) (guard : guard) : exp =
  match guard with
  | BoolG true -> exp_target
  | BoolG false ->
      Il.Ast.UnE (`NotOp, `BoolT, exp_target) $$ (exp_target.at, Il.Ast.BoolT)
  | CmpG (cmpop, optyp, exp) ->
      Il.Ast.CmpE (cmpop, optyp, exp_target, exp)
      $$ (exp_target.at, Il.Ast.BoolT)
  | SubG typ -> Il.Ast.SubE (exp_target, typ) $$ (exp_target.at, Il.Ast.BoolT)
  | MatchG pattern ->
      Il.Ast.MatchE (exp_target, pattern) $$ (exp_target.at, Il.Ast.BoolT)
  | MemG exp -> Il.Ast.MemE (exp_target, exp) $$ (exp_target.at, Il.Ast.BoolT)

(* Conversion from type to its variants *)

let rec typ_as_variant (tdenv : TDEnv.t) (typ : typ) : mixop list option =
  match typ.it with
  | VarT (tid, _) -> (
      let _, deftyp = TDEnv.find tid tdenv in
      match deftyp.it with
      | PlainT typ -> typ_as_variant tdenv typ
      | VariantT typcases ->
          let mixops = typcases |> List.map it |> List.map fst in
          Some mixops
      | _ -> None)
  | _ -> None

(* Determine the overlapping guard of two conditions *)

type overlap =
  | Identical
  | Disjoint of exp * guard * guard
  | Partition of exp * guard * guard
  | Fuzzy

let rec distinct_exp_literal (exp_a : exp) (exp_b : exp) : bool =
  match (exp_a.it, exp_b.it) with
  | BoolE b_a, BoolE b_b -> b_a <> b_b
  | NumE n_a, NumE n_b -> not (Num.eq n_a n_b)
  | TextE t_a, TextE t_b -> t_a <> t_b
  | TupleE exps_a, TupleE exps_b ->
      assert (List.length exps_a = List.length exps_b);
      List.exists2 distinct_exp_literal exps_a exps_b
  | CaseE (mixop_a, []), CaseE (mixop_b, []) -> not (Mixop.eq mixop_a mixop_b)
  | ListE exps_a, ListE exps_b when List.length exps_a = List.length exps_b ->
      List.exists2 distinct_exp_literal exps_a exps_b
  | ListE _, ListE _ -> true
  | _ -> false

let overlap_typ (tdenv : TDEnv.t) (exp : exp) (typ_a : typ) (typ_b : typ) :
    overlap =
  let guard_a = SubG typ_a in
  let guard_b = SubG typ_b in
  match (typ_as_variant tdenv typ_a, typ_as_variant tdenv typ_b) with
  | Some mixops_a, Some mixops_b ->
      let module Set = Set.Make (Mixop) in
      let mixops_a = Set.of_list mixops_a in
      let mixops_b = Set.of_list mixops_b in
      if Set.equal mixops_a mixops_b then Identical
      else if Set.inter mixops_a mixops_b |> Set.is_empty then
        Disjoint (exp, guard_a, guard_b)
      else Fuzzy
  | _ -> Fuzzy

let rec overlap_exp (tdenv : TDEnv.t) (exp_a : exp) (exp_b : exp) : overlap =
  let overlap_exp_unequal () : overlap =
    match (exp_a.it, exp_b.it) with
    (* Negation *)
    | UnE (`NotOp, _, exp_a), _ when Sl.Eq.eq_exp exp_a exp_b ->
        Partition (exp_a, BoolG false, BoolG true)
    | _, UnE (`NotOp, _, exp_b) when Sl.Eq.eq_exp exp_a exp_b ->
        Partition (exp_b, BoolG true, BoolG false)
    (* Equals literal *)
    | ( CmpE (`EqOp, optyp_a, exp_a_l, exp_a_r),
        CmpE (`EqOp, optyp_b, exp_b_l, exp_b_r) )
      when optyp_a = optyp_b
           && Sl.Eq.eq_exp exp_a_l exp_b_l
           && distinct_exp_literal exp_a_r exp_b_r ->
        Disjoint
          ( exp_a_l,
            CmpG (`EqOp, optyp_a, exp_a_r),
            CmpG (`EqOp, optyp_b, exp_b_r) )
    | ( CmpE (`EqOp, optyp_a, exp_a_l, exp_a_r),
        CmpE (`EqOp, optyp_b, exp_b_l, exp_b_r) )
      when optyp_a = optyp_b
           && Sl.Eq.eq_exp exp_a_l exp_b_r
           && distinct_exp_literal exp_a_r exp_b_l ->
        Disjoint
          ( exp_a_l,
            CmpG (`EqOp, optyp_a, exp_a_r),
            CmpG (`EqOp, optyp_b, exp_b_l) )
    (* Equals and not equals *)
    | ( CmpE (`EqOp, optyp_a, exp_a_l, exp_a_r),
        CmpE (`NeOp, optyp_b, exp_b_l, exp_b_r) )
      when optyp_a = optyp_b
           && Sl.Eq.eq_exp exp_a_l exp_b_l
           && Sl.Eq.eq_exp exp_a_r exp_b_r ->
        Partition
          ( exp_a_l,
            CmpG (`EqOp, optyp_a, exp_a_r),
            CmpG (`NeOp, optyp_b, exp_b_r) )
    | ( CmpE (`EqOp, optyp_a, exp_a_l, exp_a_r),
        CmpE (`NeOp, optyp_b, exp_b_l, exp_b_r) )
      when optyp_a = optyp_b
           && Sl.Eq.eq_exp exp_a_l exp_b_r
           && Sl.Eq.eq_exp exp_a_r exp_b_l ->
        Partition
          ( exp_a_l,
            CmpG (`EqOp, optyp_a, exp_a_r),
            CmpG (`NeOp, optyp_b, exp_b_l) )
    (* Subtyping *)
    | SubE (exp_a, typ_a), SubE (exp_b, typ_b) when Sl.Eq.eq_exp exp_a exp_b ->
        overlap_typ tdenv exp_a typ_a typ_b
    (* Match on patterns *)
    | MatchE (exp_a, pattern_a), MatchE (exp_b, pattern_b)
      when Sl.Eq.eq_exp exp_a exp_b ->
        overlap_pattern exp_a pattern_a pattern_b
    (* Membership on literals *)
    | ( MemE (exp_e_a, ({ it = ListE exps_s_a; _ } as exp_s_a)),
        MemE (exp_e_b, ({ it = ListE exps_s_b; _ } as exp_s_b)) )
      when Sl.Eq.eq_exp exp_e_a exp_e_b
           && List.for_all
                (fun exp_s_a ->
                  List.for_all (distinct_exp_literal exp_s_a) exps_s_b)
                exps_s_a ->
        Disjoint (exp_e_a, MemG exp_s_a, MemG exp_s_b)
    | _ -> Fuzzy
  in
  if Sl.Eq.eq_exp exp_a exp_b then Identical else overlap_exp_unequal ()

and overlap_pattern (exp : exp) (pattern_a : pattern) (pattern_b : pattern) :
    overlap =
  let guard_a = MatchG pattern_a in
  let guard_b = MatchG pattern_b in
  let overlap_pattern_unequal () : overlap =
    match (pattern_a, pattern_b) with
    | CaseP _, CaseP _ -> Disjoint (exp, guard_a, guard_b)
    | ListP `Cons, ListP (`Fixed n) | ListP (`Fixed n), ListP `Cons ->
        if n = 0 then Partition (exp, guard_a, guard_b)
        else Disjoint (exp, guard_a, guard_b)
    | ListP `Cons, ListP `Nil | ListP `Nil, ListP `Cons ->
        Partition (exp, guard_a, guard_b)
    | ListP (`Fixed _), ListP (`Fixed _) -> Disjoint (exp, guard_a, guard_b)
    | ListP (`Fixed n), ListP `Nil | ListP `Nil, ListP (`Fixed n) ->
        if n = 0 then Identical else Disjoint (exp, guard_a, guard_b)
    | OptP `Some, OptP `None | OptP `None, OptP `Some ->
        Partition (exp, guard_a, guard_b)
    | _ -> Fuzzy
  in
  if Sl.Eq.eq_pattern pattern_a pattern_b then Identical
  else overlap_pattern_unequal ()

let overlap_guard (tdenv : TDEnv.t) (exp : exp) (guard_a : guard)
    (guard_b : guard) : overlap =
  let exp_a = guard_as_exp exp guard_a in
  let exp_b = guard_as_exp exp guard_b in
  overlap_exp tdenv exp_a exp_b

(* [5-1] Merge consecutive if statements with the same condition

   This handles if statements that are not categorized as case analysis,
   either because the condition itself is complex or because it is iterated *)

let rec merge_identical_if (tdenv : TDEnv.t) (at : region)
    (exp_cond_target : exp) (iterexps_target : iterexp list)
    (instrs_then_target : instr list) (instrs : instr list) : instr list option
    =
  merge_identical_if' tdenv exp_cond_target iterexps_target [] instrs
  |> Option.map (fun (instrs_then, instrs_leftover) ->
         let instr =
           let instrs_then = merge_block instrs_then_target instrs_then in
           IfI (exp_cond_target, iterexps_target, instrs_then) $ at
         in
         instr :: instrs_leftover)

and merge_identical_if' (tdenv : TDEnv.t) (exp_cond_target : exp)
    (iterexps_target : iterexp list) (instrs_leftover : instr list)
    (instrs : instr list) : (instr list * instr list) option =
  match instrs with
  | ({ it = IfI (exp_cond, iterexps, instrs_then); _ } as instr_h) :: instrs_t
    -> (
      let eq_iterexps = Sl.Eq.eq_iterexps iterexps iterexps_target in
      let overlap_exp_cond = overlap_exp tdenv exp_cond_target exp_cond in
      match (eq_iterexps, overlap_exp_cond) with
      | true, Identical ->
          let instrs_leftover = instrs_leftover @ instrs_t in
          Some (instrs_then, instrs_leftover)
      | _ ->
          let instrs_leftover = instrs_leftover @ [ instr_h ] in
          merge_identical_if' tdenv exp_cond_target iterexps_target
            instrs_leftover instrs_t)
  | _ -> None

let rec merge_if (tdenv : TDEnv.t) (instrs : instr list) : instr list =
  match instrs with
  | [] -> []
  | { it = IfI (exp_cond, iterexps, instrs_then); at; _ } :: instrs_t -> (
      match
        merge_identical_if tdenv at exp_cond iterexps instrs_then instrs_t
      with
      | Some instrs -> merge_if tdenv instrs
      | None ->
          let instr_h =
            let instrs_then = merge_if tdenv instrs_then in
            IfI (exp_cond, iterexps, instrs_then) $ at
          in
          let instrs_t = merge_if tdenv instrs_t in
          instr_h :: instrs_t)
  | { it = CaseI (exp, cases, total); at; _ } :: instrs_t ->
      let instr_h =
        let guards, blocks = List.split cases in
        let blocks = List.map (merge_if tdenv) blocks in
        let cases = List.combine guards blocks in
        CaseI (exp, cases, total) $ at
      in
      let instrs_t = merge_if tdenv instrs_t in
      instr_h :: instrs_t
  | instr_h :: instrs_t ->
      let instrs_t = merge_if tdenv instrs_t in
      instr_h :: instrs_t

(* [5-2-a] if-and-if to case analysis *)

let casify_if_if (tdenv : TDEnv.t) (at : region) (exp_cond_target : exp)
    (instrs_then_target : instr list) (exp_cond : exp)
    (instrs_then : instr list) : instr option =
  let overlap_exp_cond = overlap_exp tdenv exp_cond_target exp_cond in
  match overlap_exp_cond with
  | Disjoint (exp, guard_target, guard) ->
      let cases =
        [ (guard_target, instrs_then_target); (guard, instrs_then) ]
      in
      let instr = CaseI (exp, cases, false) $ at in
      Some instr
  | Partition (exp, guard_target, guard) ->
      let cases =
        [ (guard_target, instrs_then_target); (guard, instrs_then) ]
      in
      let instr = CaseI (exp, cases, true) $ at in
      Some instr
  | _ -> None

(* [5-2-b] if-and-case to case analysis *)

let rec merge_if_case (tdenv : TDEnv.t) (exp_cond_target : exp)
    (instrs_then_target : instr list) (exp : exp) (cases : case list)
    (total : bool) : case list option =
  match exp_as_guard exp exp_cond_target with
  | Some guard_target ->
      merge_if_case' tdenv exp cases total [] guard_target instrs_then_target
  | None -> None

and merge_if_case' (tdenv : TDEnv.t) (exp : exp) (cases : case list)
    (total : bool) (cases_leftover : case list) (guard_target : guard)
    (instrs_then_target : instr list) : case list option =
  match cases with
  | [] when total -> assert false
  | [] ->
      let cases = cases_leftover @ [ (guard_target, instrs_then_target) ] in
      Some cases
  | case_h :: cases_t -> (
      let guard_h, instrs_h = case_h in
      let overlap_guard = overlap_guard tdenv exp guard_target guard_h in
      match overlap_guard with
      | Identical ->
          let instrs_h = merge_block instrs_then_target instrs_h in
          let case_h = (guard_h, instrs_h) in
          Some (case_h :: cases_t)
      | Disjoint _ | Partition _ ->
          let cases_leftover = cases_leftover @ [ case_h ] in
          merge_if_case' tdenv exp cases_t total cases_leftover guard_target
            instrs_then_target
      | _ -> None)

let casify_if_case (tdenv : TDEnv.t) (at : region) (exp_cond_target : exp)
    (instrs_then_target : instr list) (exp : exp) (cases : case list)
    (total : bool) : instr option =
  let cases_opt =
    merge_if_case tdenv exp_cond_target instrs_then_target exp cases total
  in
  match cases_opt with
  | Some cases ->
      let instr = CaseI (exp, cases, total) $ at in
      Some instr
  | None -> None

(* [5-2-c] case-and-if to case analysis *)

let rec merge_case_if (tdenv : TDEnv.t) (exp_target : exp)
    (cases_target : case list) (total_target : bool) (exp_cond : exp)
    (instrs : instr list) : case list option =
  match exp_as_guard exp_target exp_cond with
  | Some guard ->
      merge_case_if' tdenv exp_target cases_target [] total_target guard instrs
  | None -> None

and merge_case_if' (tdenv : TDEnv.t) (exp_target : exp)
    (cases_target : case list) (cases_target_leftover : case list)
    (total_target : bool) (guard : guard) (instrs : instr list) :
    case list option =
  match cases_target with
  | [] when total_target -> assert false
  | [] ->
      let cases = cases_target_leftover @ [ (guard, instrs) ] in
      Some cases
  | case_target_h :: cases_target_t -> (
      let guard_target_h, instrs_target_h = case_target_h in
      let overlap_guard = overlap_guard tdenv exp_target guard_target_h guard in
      match overlap_guard with
      | Identical ->
          let instrs_target_h = merge_block instrs_target_h instrs in
          let case_target_h = (guard_target_h, instrs_target_h) in
          Some (case_target_h :: cases_target_t)
      | Disjoint _ | Partition _ ->
          let cases_target_leftover =
            cases_target_leftover @ [ case_target_h ]
          in
          merge_case_if' tdenv exp_target cases_target_t cases_target_leftover
            total_target guard instrs
      | _ -> None)

let casify_case_if (tdenv : TDEnv.t) (at : region) (exp_target : exp)
    (cases_target : case list) (total_target : bool) (exp_cond : exp)
    (instrs_then : instr list) : instr option =
  let cases_opt =
    merge_case_if tdenv exp_target cases_target total_target exp_cond
      instrs_then
  in
  match cases_opt with
  | Some cases ->
      let instr = CaseI (exp_target, cases, false) $ at in
      Some instr
  | None -> None

(* [5-2-d] case-and-case to case analysis *)

let merge_case_case (tdenv : TDEnv.t) (exp_target : exp)
    (cases_target : case list) (total_target : bool) (exp : exp)
    (cases : case list) : case list option =
  if Sl.Eq.eq_exp exp_target exp then
    List.fold_left
      (fun cases_target_opt (guard, instrs) ->
        match cases_target_opt with
        | Some cases_target ->
            merge_case_if' tdenv exp_target cases_target [] total_target guard
              instrs
        | None -> None)
      (Some cases_target) cases
  else None

let casify_case_case (tdenv : TDEnv.t) (at : region) (exp_target : exp)
    (cases_target : case list) (total_target : bool) (exp : exp)
    (cases : case list) : instr option =
  let cases_opt =
    merge_case_case tdenv exp_target cases_target total_target exp cases
  in
  match cases_opt with
  | Some cases ->
      let instr = CaseI (exp_target, cases, total_target) $ at in
      Some instr
  | None -> None

(* [5-2-a/b] Casifying from an if statement *)

let rec casify_from_if (tdenv : TDEnv.t) (at : region) (exp_cond_target : exp)
    (iterexps_target : iterexp list) (instrs_then_target : instr list)
    (instrs : instr list) : instr list option =
  match iterexps_target with
  | [] -> casify_from_if' tdenv at exp_cond_target instrs_then_target [] instrs
  | _ -> None

and casify_from_if' (tdenv : TDEnv.t) (at : region) (exp_cond_target : exp)
    (instrs_then_target : instr list) (instrs_leftover : instr list)
    (instrs : instr list) : instr list option =
  match instrs with
  | ({ it = IfI (exp_cond, [], instrs_then); _ } as instr_h) :: instrs_t -> (
      let instr_h_opt =
        casify_if_if tdenv at exp_cond_target instrs_then_target exp_cond
          instrs_then
      in
      match instr_h_opt with
      | Some instr_h -> Some ([ instr_h ] @ instrs_leftover @ instrs_t)
      | None ->
          let instrs_leftover = instrs_leftover @ [ instr_h ] in
          casify_from_if' tdenv at exp_cond_target instrs_then_target
            instrs_leftover instrs_t)
  | ({ it = CaseI (exp, cases, total); _ } as instr_h) :: instrs_t -> (
      let instr_h_opt =
        casify_if_case tdenv at exp_cond_target instrs_then_target exp cases
          total
      in
      match instr_h_opt with
      | Some instr_h -> Some ([ instr_h ] @ instrs_leftover @ instrs_t)
      | None ->
          let instrs_leftover = instrs_leftover @ [ instr_h ] in
          casify_from_if' tdenv at exp_cond_target instrs_then_target
            instrs_leftover instrs_t)
  | _ -> None

(* [5-2-c] Casifying from a case statement *)

let rec casify_from_case (tdenv : TDEnv.t) (at : region) (exp_target : exp)
    (cases_target : case list) (total_target : bool) (instrs : instr list) :
    instr list option =
  casify_from_case' tdenv at exp_target cases_target total_target [] instrs

and casify_from_case' (tdenv : TDEnv.t) (at : region) (exp_target : exp)
    (cases_target : case list) (total_target : bool)
    (instrs_leftover : instr list) (instrs : instr list) : instr list option =
  match instrs with
  | ({ it = IfI (exp_cond, [], instrs_then); _ } as instr_h) :: instrs_t -> (
      let instr_h_opt =
        casify_case_if tdenv at exp_target cases_target total_target exp_cond
          instrs_then
      in
      match instr_h_opt with
      | Some instr_h -> Some ([ instr_h ] @ instrs_leftover @ instrs_t)
      | None ->
          let instrs_leftover = instrs_leftover @ [ instr_h ] in
          casify_from_case' tdenv at exp_target cases_target total_target
            instrs_leftover instrs_t)
  | ({ it = CaseI (exp, cases, _total); _ } as instr_h) :: instrs_t -> (
      let instr_h_opt =
        casify_case_case tdenv at exp_target cases_target total_target exp cases
      in
      match instr_h_opt with
      | Some instr_h -> Some ([ instr_h ] @ instrs_leftover @ instrs_t)
      | None ->
          let instrs_leftover = instrs_leftover @ [ instr_h ] in
          casify_from_case' tdenv at exp_target cases_target total_target
            instrs_leftover instrs_t)
  | _ -> None

let rec casify (tdenv : TDEnv.t) (instrs : instr list) : instr list =
  match instrs with
  | [] -> []
  | { it = IfI (exp_cond, iterexps, instrs_then); at; _ } :: instrs_t -> (
      match casify_from_if tdenv at exp_cond iterexps instrs_then instrs_t with
      | Some instrs -> casify tdenv instrs
      | None ->
          let instr_h =
            let instrs_then = casify tdenv instrs_then in
            IfI (exp_cond, iterexps, instrs_then) $ at
          in
          let instrs_t = casify tdenv instrs_t in
          instr_h :: instrs_t)
  | { it = CaseI (exp, cases, total); at; _ } :: instrs_t -> (
      match casify_from_case tdenv at exp cases total instrs_t with
      | Some instrs -> casify tdenv instrs
      | None ->
          let instr_h =
            let guards, blocks = List.split cases in
            let blocks = List.map (casify tdenv) blocks in
            let cases = List.combine guards blocks in
            CaseI (exp, cases, total) $ at
          in
          let instrs_t = casify tdenv instrs_t in
          instr_h :: instrs_t)
  | instr_h :: instrs_t ->
      let instrs_t = casify tdenv instrs_t in
      instr_h :: instrs_t

(* [5-3] Totalize case analysis of variant matches *)

let find_variant_case_analysis (tdenv : TDEnv.t) (cases : case list) :
    mixop list option =
  List.fold_left
    (fun mixops_opt (guard, _) ->
      match mixops_opt with
      | Some mixops -> (
          match guard with
          | SubG typ ->
              Some (mixops @ (typ |> typ_as_variant tdenv |> Option.get))
          | MatchG (CaseP mixop) -> Some (mixops @ [ mixop ])
          | _ -> None)
      | None -> None)
    (Some []) cases

let rec totalize_case_analysis (tdenv : TDEnv.t) (instrs : instr list) :
    instr list =
  List.map (totalize_case_analysis' tdenv) instrs

and totalize_case_analysis' (tdenv : TDEnv.t) (instr : instr) : instr =
  let at = instr.at in
  match instr.it with
  | IfI (exp_cond, iterexps, instrs_then) ->
      let instrs_then = totalize_case_analysis tdenv instrs_then in
      IfI (exp_cond, iterexps, instrs_then) $ at
  | CaseI (exp, cases, false) -> (
      let cases =
        let guards, blocks = List.split cases in
        let blocks = List.map (totalize_case_analysis tdenv) blocks in
        List.combine guards blocks
      in
      match find_variant_case_analysis tdenv cases with
      | Some mixops_case ->
          let module Set = Set.Make (Mixop) in
          let mixops_total =
            let typ = exp.note $ exp.at in
            typ |> typ_as_variant tdenv |> Option.get
          in
          let mixops_total = Set.of_list mixops_total in
          let mixops_case = Set.of_list mixops_case in
          let total = Set.equal mixops_case mixops_total in
          CaseI (exp, cases, total) $ at
      | None -> CaseI (exp, cases, false) $ at)
  | _ -> instr

(* Apply optimizations until it reaches a fixed point *)

let optimize_pre (instrs : instr list) : instr list =
  instrs |> remove_let_alias |> parallelize_if_disjunctions
  |> matchify_if_eq_terminals

let rec optimize_loop (henv : HEnv.t) (tdenv : TDEnv.t) (instrs : instr list) :
    instr list =
  let instrs_optimized =
    instrs |> remove_redundant_bindings henv |> merge_if tdenv |> casify tdenv
  in
  if Ol.Eq.eq_instrs instrs instrs_optimized then instrs
  else optimize_loop henv tdenv instrs_optimized

let optimize_post (tdenv : TDEnv.t) (instrs : instr list) : instr list =
  instrs |> totalize_case_analysis tdenv

let optimize (henv : HEnv.t) (tdenv : TDEnv.t) (instrs : instr list) :
    instr list =
  instrs |> optimize_pre |> optimize_loop henv tdenv |> optimize_post tdenv
