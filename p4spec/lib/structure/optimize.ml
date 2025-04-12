open Domain.Lib
open Sl.Ast
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
  | ElseI instr ->
      let instr = rename_instr rename instr in
      ElseI instr $ at
  | LetI (exp_l, exp_r, iterexps) ->
      let exp_l = rename_exp rename exp_l in
      let exp_r = rename_exp rename exp_r in
      let iterexps = List.map (rename_iterexp rename) iterexps in
      LetI (exp_l, exp_r, iterexps) $ at
  | RetRelI exps ->
      let exps = List.map (rename_exp rename) exps in
      RetRelI exps $ at
  | RetDecI exp ->
      let exp = rename_exp rename exp in
      RetDecI exp $ at

(* Remove redundant let bindings from the code,

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

(* Merge if statements with the same condition *)

let rec merge_block (instrs_a : instr list) (instrs_b : instr list) : instr list
    =
  match (instrs_a, instrs_b) with
  | instr_a :: instrs_a, instr_b :: instrs_b when Sl.Eq.eq_instr instr_a instr_b
    ->
      let instrs = merge_block instrs_a instrs_b in
      instr_a :: instrs
  | _ -> instrs_a @ instrs_b

let rec find_mergeable_if ?(instrs_unmergeable : instr list = [])
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
      find_mergeable_if ~instrs_unmergeable exp_cond_target iterexps_target
        instrs_t
  | _ -> None

let rec merge_if (instrs : instr list) : instr list =
  match instrs with
  | [] -> []
  | ({ it = IfI (exp_cond, iterexps, instrs_then, instrs_else); _ } as instr_h)
    :: instrs_t -> (
      match find_mergeable_if exp_cond iterexps instrs_t with
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
