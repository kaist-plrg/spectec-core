open Domain.Dom
open Il.Ast
open Attempt
open Util.Source

(* Renaming environment *)

module Exp = struct
  type t = exp * Dom.Dim.t

  let to_string (exp, dim) = Il.Print.string_of_exp exp ^ Dom.Dim.to_string dim
end

module REnv = struct
  include MakeIdEnv (Exp)

  let add_dim (iter : iter) (renv_pre : t) (renv_post : t) : t =
    let ids_updated = IdSet.diff (dom renv_post) (dom renv_pre) in
    mapi
      (fun id (exp, dim) ->
        if IdSet.mem id ids_updated then (exp, dim @ [ iter ]) else (exp, dim))
      renv_post
end

(* Expressions *)

let rec rename_exp (ids : IdSet.t) (binds : IdSet.t) (renv : REnv.t) (exp : exp)
    : IdSet.t * REnv.t * exp =
  let frees = Il.Free.free_exp exp in
  (* If the expression contains no bindings, rename it *)
  if IdSet.inter binds frees |> IdSet.is_empty then rename_exp_base ids renv exp
  else rename_exp_rec ids binds renv exp

and rename_exps (ids : IdSet.t) (binds : IdSet.t) (renv : REnv.t)
    (exps : exp list) : IdSet.t * REnv.t * exp list =
  List.fold_left
    (fun (ids, renv, exps) exp ->
      let ids, renv, exp = rename_exp ids binds renv exp in
      (ids, renv, exps @ [ exp ]))
    (ids, renv, []) exps

and rename_exp_base (ids : IdSet.t) (renv : REnv.t) (exp : exp) :
    IdSet.t * REnv.t * exp =
  let id_rename = Fresh.fresh_exp ids exp in
  let ids = IdSet.add id_rename ids in
  let renv = REnv.add id_rename (exp, []) renv in
  let exp = VarE id_rename $$ (exp.at, exp.note) in
  (ids, renv, exp)

and rename_exp_rec (ids : IdSet.t) (binds : IdSet.t) (renv : REnv.t) (exp : exp)
    : IdSet.t * REnv.t * exp =
  let at, note = (exp.at, exp.note) in
  match exp.it with
  | BoolE _ | NumE _ | TextE _ | VarE _ -> (ids, renv, exp)
  | TupleE exps ->
      let ids, renv, exps = rename_exps ids binds renv exps in
      let exp = TupleE exps $$ (at, note) in
      (ids, renv, exp)
  | CaseE (mixop, exps) ->
      let ids, renv, exps = rename_exps ids binds renv exps in
      let exp = CaseE (mixop, exps) $$ (at, note) in
      (ids, renv, exp)
  | OptE (Some exp) ->
      let ids, renv, exp = rename_exp ids binds renv exp in
      let exp = OptE (Some exp) $$ (at, note) in
      (ids, renv, exp)
  | OptE None -> (ids, renv, exp)
  | StrE expfields ->
      let atoms, exps = List.split expfields in
      let ids, renv, exps = rename_exps ids binds renv exps in
      let expfields = List.combine atoms exps in
      let exp = StrE expfields $$ (at, note) in
      (ids, renv, exp)
  | ListE exps ->
      let ids, renv, exps = rename_exps ids binds renv exps in
      let exp = ListE exps $$ (at, note) in
      (ids, renv, exp)
  | ConsE (exp_h, exp_t) ->
      let ids, renv, exp_h = rename_exp ids binds renv exp_h in
      let ids, renv, exp_t = rename_exp ids binds renv exp_t in
      let exp = ConsE (exp_h, exp_t) $$ (at, note) in
      (ids, renv, exp)
  | IterE (_, ((_, _ :: _) as iterexp)) ->
      error at
        (Format.asprintf
           "iterated expression should initially have no annotations, but got \
            %s"
           (Il.Print.string_of_iterexp iterexp))
  | IterE (exp, (iter, [])) ->
      let renv_pre = renv in
      let ids, renv_post, exp = rename_exp ids binds renv_pre exp in
      let renv = REnv.add_dim iter renv_pre renv_post in
      let exp = IterE (exp, (iter, [])) $$ (at, note) in
      (ids, renv, exp)
  | CastE (exp, typ) ->
      let ids, renv, exp = rename_exp ids binds renv exp in
      let exp = CastE (exp, typ) $$ (at, note) in
      (ids, renv, exp)
  | _ -> (ids, renv, exp)

(* Arguments *)

and rename_arg (ids : IdSet.t) (binds : IdSet.t) (renv : REnv.t) (arg : arg) :
    IdSet.t * REnv.t * arg =
  let at = arg.at in
  match arg.it with
  | ExpA exp ->
      let ids, renv, exp = rename_exp ids binds renv exp in
      let arg = ExpA exp $ at in
      (ids, renv, arg)
  | _ -> (ids, renv, arg)

and rename_args (ids : IdSet.t) (binds : IdSet.t) (renv : REnv.t)
    (args : arg list) : IdSet.t * REnv.t * arg list =
  List.fold_left
    (fun (ids, renv, args) arg ->
      let ids, renv, arg = rename_arg ids binds renv arg in
      (ids, renv, args @ [ arg ]))
    (ids, renv, []) args

(* Premises *)

module Hint = struct
  type t = int list

  let to_string t =
    Format.asprintf "hint(input %s)"
      (String.concat " " (List.map (fun idx -> "%" ^ string_of_int idx) t))
end

module HEnv = MakeIdEnv (Hint)

let rec rename_prem (hints : HEnv.t) (ids : IdSet.t) (binds : IdSet.t)
    (renv : REnv.t) (prem : prem) : IdSet.t * REnv.t * prem =
  let at = prem.at in
  match prem.it with
  | RulePr (id, (mixop, exps)) ->
      let inputs = HEnv.find id hints in
      let exps_input, exps_output =
        exps
        |> List.mapi (fun idx exp -> (idx, exp))
        |> List.partition (fun (idx, _) -> List.mem idx inputs)
      in
      let ids, renv, exps_output =
        let idxs, exps_output = List.split exps_output in
        let ids, renv, exps_output = rename_exps ids binds renv exps_output in
        let exps_output = List.combine idxs exps_output in
        (ids, renv, exps_output)
      in
      let notexp =
        let exps =
          exps_input @ exps_output
          |> List.sort (fun (idx_a, _) (idx_b, _) -> compare idx_a idx_b)
          |> List.map snd
        in
        (mixop, exps)
      in
      let prem = RulePr (id, notexp) $ at in
      (ids, renv, prem)
  | LetPr (exp_l, exp_r) ->
      let ids, renv, exp_l = rename_exp ids binds renv exp_l in
      let prem = LetPr (exp_l, exp_r) $ at in
      (ids, renv, prem)
  | IterPr (_, ((_, _ :: _) as iterexp)) ->
      error at
        (Format.asprintf
           "iterated premise should initially have no annotations, but got %s"
           (Il.Print.string_of_iterexp iterexp))
  | IterPr (prem, (iter, [])) ->
      let renv_pre = renv in
      let ids, renv_post, prem = rename_prem hints ids binds renv_pre prem in
      let renv = REnv.add_dim iter renv_pre renv_post in
      let prem = IterPr (prem, (iter, [])) $ at in
      (ids, renv, prem)
  | _ -> (ids, renv, prem)
