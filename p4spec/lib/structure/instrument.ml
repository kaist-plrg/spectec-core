open Ol.Ast
module TDEnv = Runtime_dynamic_il.Envs.TDEnv
open Util.Source

(* Insert phantom instructions at dangling else branches,
   with the path condition necessary to reach the else branch

   Note that this does not take fall-through into account,
   so the path condition is not precise

   Fall-through may happen due to the heuristic-driven syntactic optimization of SL,

   (i) Good case

   -- if i >= 0   and   -- if i < 0
   -- if j >= 0         -- if j >= 0

   are nicely merged into

   if i >= 0 then
     if j >= 0 then ...
     else Phantom: i >= 0 && j < 0
   else
     if j >= 0 then ...
     else Phantom: i < 0 && j < 0

   (ii) Bad case

   -- if j >= 0   and  -- if i < 0
   -- if i >= 0        -- if j >= 0

   are merged into

   if j >= 0 then
     if i >= 0 then ...
     else Phantom: j >= 0 && i < 0
   else Phantom: j < 0

   ... if i = -1, j = 3 is given as input, it falls through

   if i < 0 then
      if j >= 0 then ...
      else Phantom: i < 0 && j < 0
   else Phantom: i >= 0 *)

(* Phantom id generator *)

let tick = ref 0

let pid () =
  let pid = !tick in
  tick := !tick + 1;
  pid

(* Path condition *)

let negate_exp (exp : exp) : exp =
  Il.Ast.UnE (`NotOp, `BoolT, exp) $$ (exp.at, exp.note)

let negate_pathcond (pathcond : pathcond) : pathcond =
  match pathcond with
  | ForallC (exp_cond, iterexps) -> ExistsC (negate_exp exp_cond, iterexps)
  | ExistsC (exp_cond, iterexps) -> ForallC (negate_exp exp_cond, iterexps)
  | PlainC exp_cond -> PlainC (negate_exp exp_cond)

(* Phantom insertion *)

let rec insert_phantom (tdenv : TDEnv.t) (pathconds : pathcond list)
    (instrs : instr list) : Sl.Ast.instr list =
  List.map (insert_phantom' tdenv pathconds) instrs

and insert_phantom' (tdenv : TDEnv.t) (pathconds : pathcond list)
    (instr : instr) : Sl.Ast.instr =
  let at = instr.at in
  match instr.it with
  | IfI (exp_cond, iterexps, instrs_then) ->
      let pathcond =
        if iterexps = [] then PlainC exp_cond else ForallC (exp_cond, iterexps)
      in
      let instrs_then =
        let pathconds = pathconds @ [ pathcond ] in
        insert_phantom tdenv pathconds instrs_then
      in
      let instrs_else =
        let phantom =
          let pid = pid () in
          let pathconds = pathconds @ [ negate_pathcond pathcond ] in
          (pid, pathconds)
        in
        [ Sl.Ast.PhantomI phantom $ at ]
      in
      Sl.Ast.IfI (exp_cond, iterexps, instrs_then, instrs_else) $ at
  | CaseI (exp, cases, total) ->
      let pathconds =
        List.map
          (fun (guard, _) ->
            let exp_cond = Optimize.guard_as_exp exp guard in
            PlainC exp_cond)
          cases
      in
      let cases = List.rev cases in
      let case_h, cases_t = (List.hd cases, List.tl cases) in
      let instr_h =
        let guard_h, instrs_then_h = case_h in
        let exp_cond_h = Optimize.guard_as_exp exp guard_h in
        let instrs_then_h = insert_phantom tdenv pathconds instrs_then_h in
        let instrs_else_h =
          if total then []
          else
            let phantom =
              let pid = pid () in
              let pathconds = pathconds @ List.map negate_pathcond pathconds in
              (pid, pathconds)
            in
            [ Sl.Ast.PhantomI phantom $ at ]
        in
        Sl.Ast.IfI (exp_cond_h, [], instrs_then_h, instrs_else_h) $ at
      in
      List.fold_left
        (fun instr (guard, instrs) ->
          let exp_cond = Optimize.guard_as_exp exp guard in
          let instrs = insert_phantom tdenv pathconds instrs in
          Sl.Ast.IfI (exp_cond, [], instrs, [ instr ]) $ at)
        instr_h cases_t
  | OtherwiseI instr ->
      let instr = insert_phantom' tdenv pathconds instr in
      Sl.Ast.OtherwiseI instr $ at
  | LetI (exp_l, exp_r, iterexps) -> Sl.Ast.LetI (exp_l, exp_r, iterexps) $ at
  | RuleI (id, notexp, iterexps) -> Sl.Ast.RuleI (id, notexp, iterexps) $ at
  | ResultI exps -> Sl.Ast.ResultI exps $ at
  | ReturnI exp -> Sl.Ast.ReturnI exp $ at

(* Nop pass *)

let rec insert_nothing (instrs : instr list) : Sl.Ast.instr list =
  List.map insert_nothing' instrs

and insert_nothing' (instr : instr) : Sl.Ast.instr =
  let at = instr.at in
  match instr.it with
  | IfI (exp_cond, iterexps, instrs_then) ->
      let instrs_then = insert_nothing instrs_then in
      Sl.Ast.IfI (exp_cond, iterexps, instrs_then, []) $ at
  | CaseI (exp, cases, _total) ->
      let cases = List.rev cases in
      let case_h, cases_t = (List.hd cases, List.tl cases) in
      let instr_h =
        let guard_h, instrs_then_h = case_h in
        let exp_cond_h = Optimize.guard_as_exp exp guard_h in
        let instrs_then_h = insert_nothing instrs_then_h in
        Sl.Ast.IfI (exp_cond_h, [], instrs_then_h, []) $ at
      in
      List.fold_left
        (fun instr (guard, instrs) ->
          let exp_cond = Optimize.guard_as_exp exp guard in
          let instrs = insert_nothing instrs in
          Sl.Ast.IfI (exp_cond, [], instrs, [ instr ]) $ at)
        instr_h cases_t
  | OtherwiseI instr ->
      let instr = insert_nothing' instr in
      Sl.Ast.OtherwiseI instr $ at
  | LetI (exp_l, exp_r, iterexps) -> Sl.Ast.LetI (exp_l, exp_r, iterexps) $ at
  | RuleI (id, notexp, iterexps) -> Sl.Ast.RuleI (id, notexp, iterexps) $ at
  | ResultI exps -> Sl.Ast.ResultI exps $ at
  | ReturnI exp -> Sl.Ast.ReturnI exp $ at

(* Instrumentation *)

let instrument (tdenv : TDEnv.t) (instrs : instr list) : Sl.Ast.instr list =
  if
    List.exists
      (fun instr -> match instr.it with OtherwiseI _ -> true | _ -> false)
      instrs
  then insert_nothing instrs
  else insert_phantom tdenv [] instrs
