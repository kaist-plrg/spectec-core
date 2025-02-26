open Domain.Dom
open Il.Ast
open Attempt
open Util.Source

(* Renaming environment *)

module Ids = struct
  type t = IdSet.t

  let to_string = IdSet.to_string ~with_braces:false
end

module REnv = MakeIdEnv (Ids)

(* Rename multiple bindings, leaving only the leftmost occurrence intact *)

(* Expressions *)

let rec rename_exp (ids : IdSet.t) (renv : REnv.t) (exp : exp) :
    IdSet.t * REnv.t * exp =
  let at, note = (exp.at, exp.note) in
  match exp.it with
  | BoolE _ | NumE _ | TextE _ -> (ids, renv, exp)
  | VarE id -> (
      match REnv.find_opt id renv with
      | Some renames when IdSet.is_empty renames ->
          let exp = VarE id $$ (at, note) in
          let renames = IdSet.singleton id in
          let renv = REnv.add id renames renv in
          (ids, renv, exp)
      | Some renames ->
          let id_rename = Fresh.fresh_id ids id in
          let ids = IdSet.add id_rename ids in
          let renames = IdSet.add id_rename renames in
          let renv = REnv.add id renames renv in
          let exp = VarE id_rename $$ (at, note) in
          (ids, renv, exp)
      | None -> (ids, renv, exp))
  | UnE (unop, optyp, exp) ->
      let ids, renv, exp = rename_exp ids renv exp in
      let exp = UnE (unop, optyp, exp) $$ (at, note) in
      (ids, renv, exp)
  | BinE (binop, optyp, exp_l, exp_r) ->
      let ids, renv, exp_l = rename_exp ids renv exp_l in
      let ids, renv, exp_r = rename_exp ids renv exp_r in
      let exp = BinE (binop, optyp, exp_l, exp_r) $$ (at, note) in
      (ids, renv, exp)
  | CmpE (cmpop, optyp, exp_l, exp_r) ->
      let ids, renv, exp_l = rename_exp ids renv exp_l in
      let ids, renv, exp_r = rename_exp ids renv exp_r in
      let exp = CmpE (cmpop, optyp, exp_l, exp_r) $$ (at, note) in
      (ids, renv, exp)
  | TupleE exps ->
      let ids, renv, exps = rename_exps ids renv exps in
      let exp = TupleE exps $$ (at, note) in
      (ids, renv, exp)
  | CaseE (mixop, exps) ->
      let ids, renv, exps = rename_exps ids renv exps in
      let exp = CaseE (mixop, exps) $$ (at, note) in
      (ids, renv, exp)
  | OptE (Some exp) ->
      let ids, renv, exp = rename_exp ids renv exp in
      let exp = OptE (Some exp) $$ (at, note) in
      (ids, renv, exp)
  | OptE None -> (ids, renv, exp)
  | StrE expfields ->
      let atoms, exps = List.split expfields in
      let ids, renv, exps = rename_exps ids renv exps in
      let expfields = List.combine atoms exps in
      let exp = StrE expfields $$ (at, note) in
      (ids, renv, exp)
  | DotE (exp, atom) ->
      let ids, renv, exp = rename_exp ids renv exp in
      let exp = DotE (exp, atom) $$ (at, note) in
      (ids, renv, exp)
  | ListE exps ->
      let ids, renv, exps = rename_exps ids renv exps in
      let exp = ListE exps $$ (at, note) in
      (ids, renv, exp)
  | ConsE (exp_l, exp_r) ->
      let ids, renv, exp_l = rename_exp ids renv exp_l in
      let ids, renv, exp_r = rename_exp ids renv exp_r in
      let exp = ConsE (exp_l, exp_r) $$ (at, note) in
      (ids, renv, exp)
  | CatE (exp_l, exp_r) ->
      let ids, renv, exp_l = rename_exp ids renv exp_l in
      let ids, renv, exp_r = rename_exp ids renv exp_r in
      let exp = CatE (exp_l, exp_r) $$ (at, note) in
      (ids, renv, exp)
  | MemE (exp_l, exp_r) ->
      let ids, renv, exp_l = rename_exp ids renv exp_l in
      let ids, renv, exp_r = rename_exp ids renv exp_r in
      let exp = MemE (exp_l, exp_r) $$ (at, note) in
      (ids, renv, exp)
  | LenE exp ->
      let ids, renv, exp = rename_exp ids renv exp in
      let exp = LenE exp $$ (at, note) in
      (ids, renv, exp)
  | IdxE (exp_b, exp_i) ->
      let ids, renv, exp_b = rename_exp ids renv exp_b in
      let ids, renv, exp_i = rename_exp ids renv exp_i in
      let exp = IdxE (exp_b, exp_i) $$ (at, note) in
      (ids, renv, exp)
  | SliceE (exp_b, exp_l, exp_h) ->
      let ids, renv, exp_b = rename_exp ids renv exp_b in
      let ids, renv, exp_l = rename_exp ids renv exp_l in
      let ids, renv, exp_h = rename_exp ids renv exp_h in
      let exp = SliceE (exp_b, exp_l, exp_h) $$ (at, note) in
      (ids, renv, exp)
  | UpdE (exp_b, path, exp_f) ->
      let ids, renv, exp_b = rename_exp ids renv exp_b in
      let ids, renv, exp_f = rename_exp ids renv exp_f in
      let ids, renv, path = rename_path ids renv path in
      let exp = UpdE (exp_b, path, exp_f) $$ (at, note) in
      (ids, renv, exp)
  | CallE (id, targs, args) ->
      let ids, renv, args = rename_args ids renv args in
      let exp = CallE (id, targs, args) $$ (at, note) in
      (ids, renv, exp)
  | IterE (_, ((_, _ :: _) as iterexp)) ->
      error at
        (Format.asprintf
           "iterated expression should initially have no annotations, but got \
            %s"
           (Il.Print.string_of_iterexp iterexp))
  | IterE (exp, (iter, [])) ->
      let ids, renv, exp = rename_exp ids renv exp in
      let exp = IterE (exp, (iter, [])) $$ (at, note) in
      (ids, renv, exp)
  | CastE (exp, typ) ->
      let ids, renv, exp = rename_exp ids renv exp in
      let exp = CastE (exp, typ) $$ (at, note) in
      (ids, renv, exp)

and rename_exps (ids : IdSet.t) (renv : REnv.t) (exps : exp list) :
    IdSet.t * REnv.t * exp list =
  List.fold_left
    (fun (ids, renv, exps) exp ->
      let ids, renv, exp = rename_exp ids renv exp in
      (ids, renv, exps @ [ exp ]))
    (ids, renv, []) exps

(* Paths *)

and rename_path (ids : IdSet.t) (renv : REnv.t) (path : path) :
    IdSet.t * REnv.t * path =
  let at, note = (path.at, path.note) in
  match path.it with
  | RootP -> (ids, renv, path)
  | IdxP (path, exp) ->
      let ids, renv, path = rename_path ids renv path in
      let ids, renv, exp = rename_exp ids renv exp in
      let path = IdxP (path, exp) $$ (at, note) in
      (ids, renv, path)
  | SliceP (path, exp_l, exp_h) ->
      let ids, renv, path = rename_path ids renv path in
      let ids, renv, exp_l = rename_exp ids renv exp_l in
      let ids, renv, exp_h = rename_exp ids renv exp_h in
      let path = SliceP (path, exp_l, exp_h) $$ (at, note) in
      (ids, renv, path)
  | DotP (path, atom) ->
      let ids, renv, path = rename_path ids renv path in
      let path = DotP (path, atom) $$ (at, note) in
      (ids, renv, path)

(* Arguments *)

and rename_arg (ids : IdSet.t) (renv : REnv.t) (arg : arg) :
    IdSet.t * REnv.t * arg =
  let at = arg.at in
  match arg.it with
  | ExpA exp ->
      let ids, renv, exp = rename_exp ids renv exp in
      let arg = ExpA exp $ at in
      (ids, renv, arg)
  | DefA _ -> (ids, renv, arg)

and rename_args (ids : IdSet.t) (renv : REnv.t) (args : arg list) :
    IdSet.t * REnv.t * arg list =
  List.fold_left
    (fun (ids, renv, args) arg ->
      let ids, renv, arg = rename_arg ids renv arg in
      (ids, renv, args @ [ arg ]))
    (ids, renv, []) args

(* Premise *)

let rec rename_prem (ids : IdSet.t) (renv : REnv.t) (prem : prem) :
    IdSet.t * REnv.t * prem =
  let at = prem.at in
  match prem.it with
  | RulePr (id, (mixop, exps)) ->
      let ids, renv, exps = rename_exps ids renv exps in
      let prem = RulePr (id, (mixop, exps)) $ at in
      (ids, renv, prem)
  | IfPr exp ->
      let ids, renv, exp = rename_exp ids renv exp in
      let prem = IfPr exp $ at in
      (ids, renv, prem)
  | ElsePr -> (ids, renv, prem)
  | LetPr (exp_l, exp_r) ->
      let ids, renv, exp_l = rename_exp ids renv exp_l in
      let ids, renv, exp_r = rename_exp ids renv exp_r in
      let prem = LetPr (exp_l, exp_r) $ at in
      (ids, renv, prem)
  | IterPr (_, ((_, _ :: _) as iterexp)) ->
      error at
        (Format.asprintf
           "iterated premise should initially have no annotations, but got %s"
           (Il.Print.string_of_iterexp iterexp))
  | IterPr (prem, (iter, [])) ->
      let ids, renv, prem = rename_prem ids renv prem in
      let prem = IterPr (prem, (iter, [])) $ at in
      (ids, renv, prem)
