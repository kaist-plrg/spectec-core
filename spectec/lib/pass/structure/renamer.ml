open Common.Domain
open Common.Source
module Il = Lang.Il
open Ol.Ast
open Envs.Make

(* Helper for renaming identifiers in expressions and instructions. *)

module Rename = MakeIdMap (Id)

type t = Rename.t

let rename_iterexp (rename : t) (iterexp : iterexp) : iterexp =
  let iter, vars = iterexp in
  let vars =
    List.map
      (fun var ->
        match Rename.find_opt var.Il.varid rename with
        | Some id_renamed -> { var with Il.varid = id_renamed }
        | None -> var)
      vars
  in
  (iter, vars)

let rename_iterexps (rename : t) (iterexps : iterexp list) : iterexp list =
  List.map (rename_iterexp rename) iterexps

let rec rename_exp (rename : t) (exp : exp) : exp =
  let at, note = (exp.at, exp.note) in
  match exp.it with
  | BoolE _ | NumE _ | TextE _ -> exp
  | VarE id when Rename.mem id rename ->
      let id_renamed = Rename.find id rename in
      Il.VarE id_renamed $$ (at, note)
  | VarE _ -> exp
  | UnE (unop, optyp, exp) ->
      let exp = rename_exp rename exp in
      Il.UnE (unop, optyp, exp) $$ (at, note)
  | BinE (binop, optyp, exp_l, exp_r) ->
      let exp_l = rename_exp rename exp_l in
      let exp_r = rename_exp rename exp_r in
      Il.BinE (binop, optyp, exp_l, exp_r) $$ (at, note)
  | CmpE (cmpop, optyp, exp_l, exp_r) ->
      let exp_l = rename_exp rename exp_l in
      let exp_r = rename_exp rename exp_r in
      Il.CmpE (cmpop, optyp, exp_l, exp_r) $$ (at, note)
  | UpCastE (typ, exp) ->
      let exp = rename_exp rename exp in
      Il.UpCastE (typ, exp) $$ (at, note)
  | DownCastE (typ, exp) ->
      let exp = rename_exp rename exp in
      Il.DownCastE (typ, exp) $$ (at, note)
  | SubE (exp, typ) ->
      let exp = rename_exp rename exp in
      Il.SubE (exp, typ) $$ (at, note)
  | MatchE (exp, pattern) ->
      let exp = rename_exp rename exp in
      Il.MatchE (exp, pattern) $$ (at, note)
  | TupleE exps ->
      let exps = List.map (rename_exp rename) exps in
      Il.TupleE exps $$ (at, note)
  | CaseE notexp ->
      let notexp = Il.Mixfix.map (rename_exp rename) notexp in
      Il.CaseE notexp $$ (at, note)
  | StrE expfields ->
      let atoms, exps = List.split expfields in
      let exps = List.map (rename_exp rename) exps in
      let expfields = List.combine atoms exps in
      Il.StrE expfields $$ (at, note)
  | OptE exp_opt ->
      let exp_opt = Option.map (rename_exp rename) exp_opt in
      Il.OptE exp_opt $$ (at, note)
  | ListE exps ->
      let exps = List.map (rename_exp rename) exps in
      Il.ListE exps $$ (at, note)
  | ConsE (exp_h, exp_t) ->
      let exp_h = rename_exp rename exp_h in
      let exp_t = rename_exp rename exp_t in
      Il.ConsE (exp_h, exp_t) $$ (at, note)
  | CatE (exp_l, exp_r) ->
      let exp_l = rename_exp rename exp_l in
      let exp_r = rename_exp rename exp_r in
      Il.CatE (exp_l, exp_r) $$ (at, note)
  | MemE (exp_e, exp_s) ->
      let exp_e = rename_exp rename exp_e in
      let exp_s = rename_exp rename exp_s in
      Il.MemE (exp_e, exp_s) $$ (at, note)
  | LenE exp ->
      let exp = rename_exp rename exp in
      Il.LenE exp $$ (at, note)
  | DotE (exp, atom) ->
      let exp = rename_exp rename exp in
      Il.DotE (exp, atom) $$ (at, note)
  | IdxE (exp_b, exp_i) ->
      let exp_b = rename_exp rename exp_b in
      let exp_i = rename_exp rename exp_i in
      Il.IdxE (exp_b, exp_i) $$ (at, note)
  | SliceE (exp_b, exp_l, exp_h) ->
      let exp_b = rename_exp rename exp_b in
      let exp_l = rename_exp rename exp_l in
      let exp_h = rename_exp rename exp_h in
      Il.SliceE (exp_b, exp_l, exp_h) $$ (at, note)
  | UpdE (exp_b, path, exp_f) ->
      let exp_b = rename_exp rename exp_b in
      let path = rename_path rename path in
      let exp_f = rename_exp rename exp_f in
      Il.UpdE (exp_b, path, exp_f) $$ (at, note)
  | CallE (id, targs, args) ->
      let args = List.map (rename_arg rename) args in
      Il.CallE (id, targs, args) $$ (at, note)
  | IterE (exp, iterexp) ->
      let exp = rename_exp rename exp in
      let iterexp = rename_iterexp rename iterexp in
      Il.IterE (exp, iterexp) $$ (at, note)

and rename_path (rename : t) (path : path) : path =
  let at, note = (path.at, path.note) in
  match path.it with
  | RootP -> path
  | IdxP (path, exp) ->
      let path = rename_path rename path in
      let exp = rename_exp rename exp in
      Il.IdxP (path, exp) $$ (at, note)
  | SliceP (path, exp_l, exp_h) ->
      let path = rename_path rename path in
      let exp_l = rename_exp rename exp_l in
      let exp_h = rename_exp rename exp_h in
      Il.SliceP (path, exp_l, exp_h) $$ (at, note)
  | DotP (path, atom) ->
      let path = rename_path rename path in
      Il.DotP (path, atom) $$ (at, note)

and rename_arg (rename : t) (arg : arg) : arg =
  let at = arg.at in
  match arg.it with
  | ExpA exp ->
      let exp = rename_exp rename exp in
      Il.ExpA exp $ at
  | DefA _ -> arg

and rename_case (rename : t) (case : case) : case =
  let guard, instrs = case in
  let guard = rename_guard rename guard in
  let instrs = List.map (rename_instr rename) instrs in
  (guard, instrs)

and rename_guard (rename : t) (guard : guard) : guard =
  match guard with
  | BoolG _ -> guard
  | CmpG (cmpop, optyp, exp) ->
      let exp = rename_exp rename exp in
      CmpG (cmpop, optyp, exp)
  | SubG _ | MatchG _ -> guard
  | MemG exp ->
      let exp = rename_exp rename exp in
      MemG exp

and rename_instr (rename : t) (instr : instr) : instr =
  let at = instr.at in
  match instr.it with
  | IfI (exp_cond, iterexps, instrs_then) ->
      let exp_cond = rename_exp rename exp_cond in
      let iterexps = List.map (rename_iterexp rename) iterexps in
      let instrs_then = List.map (rename_instr rename) instrs_then in
      IfI (exp_cond, iterexps, instrs_then) $ at
  | RelAssertI { call; expect; iterexps; block } ->
      let notexp = Il.Mixfix.map (rename_exp rename) call.notexp in
      let iterexps = List.map (rename_iterexp rename) iterexps in
      let block = List.map (rename_instr rename) block in
      RelAssertI
        { call = { relid = call.relid; notexp }; expect; iterexps; block }
      $ at
  | CaseI (exp, cases, total) ->
      let exp = rename_exp rename exp in
      let cases = List.map (rename_case rename) cases in
      CaseI (exp, cases, total) $ at
  | OtherwiseI instr ->
      let instr = rename_instr rename instr in
      OtherwiseI instr $ at
  | LetI (exp_l, exp_r, iterexps, block) ->
      let exp_l = rename_exp rename exp_l in
      let exp_r = rename_exp rename exp_r in
      let iterexps = List.map (rename_iterexp rename) iterexps in
      let block = List.map (rename_instr rename) block in
      LetI (exp_l, exp_r, iterexps, block) $ at
  | RelI { call; iterexps; block } ->
      let notexp = Il.Mixfix.map (rename_exp rename) call.notexp in
      let iterexps = List.map (rename_iterexp rename) iterexps in
      let block = List.map (rename_instr rename) block in
      RelI { call = { relid = call.relid; notexp }; iterexps; block } $ at
  | ResultI exps ->
      let exps = List.map (rename_exp rename) exps in
      ResultI exps $ at
  | ReturnI exp ->
      let exp = rename_exp rename exp in
      ReturnI exp $ at
  | DebugI (exp, instr_body) ->
      let exp = rename_exp rename exp in
      let instr_body = rename_instr rename instr_body in
      DebugI (exp, instr_body) $ at

and rename_instrs (rename : t) (instrs : instr list) : instr list =
  List.map (rename_instr rename) instrs
