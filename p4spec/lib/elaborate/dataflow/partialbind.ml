open Domain.Lib
open Il.Ast
open Runtime_static
open Error
module DCtx = Dctx
open Util.Source

(* Rename for an expression *)

module Exp = struct
  type t = exp * Dim.t

  let to_string (exp, dim) = Il.Print.string_of_exp exp ^ Dim.to_string dim
end

module REnv = struct
  include MakeIdEnv (Exp)

  let gen_sidecondition (id : Id.t) (exp : exp) (dim : Dim.t) =
    let exp =
      let exp_l = VarE id $$ (id.at, exp.note) in
      let exp_r = exp in
      CmpE (`EqOp, `BoolT, exp_l, exp_r) $$ (exp.at, BoolT)
    in
    let sidecondition = IfPr exp $ exp.at in
    List.fold_left
      (fun sidecondition iter -> IterPr (sidecondition, (iter, [])) $ exp.at)
      sidecondition dim

  let gen_sideconditions (renv : t) : prem list =
    fold
      (fun id (exp, dim) sideconditions ->
        let sidecondition = gen_sidecondition id exp dim in
        sideconditions @ [ sidecondition ])
      renv []

  let update_dim (iter : iter) (renv_pre : t) (renv_post : t) : t =
    let ids_updated = IdSet.diff (dom renv_post) (dom renv_pre) in
    mapi
      (fun id (exp, dim) ->
        if IdSet.mem id ids_updated then (exp, dim @ [ iter ]) else (exp, dim))
      renv_post
end

(* Expressions *)

let rec rename_exp (dctx : DCtx.t) (binds : IdSet.t) (renv : REnv.t) (exp : exp)
    : DCtx.t * REnv.t * exp =
  let frees = Il.Free.free_exp exp in
  (* If the expression contains no bindings, rename it *)
  if IdSet.inter binds frees |> IdSet.is_empty then
    rename_exp_base dctx renv exp
  else rename_exp_rec dctx binds renv exp

and rename_exps (dctx : DCtx.t) (binds : IdSet.t) (renv : REnv.t)
    (exps : exp list) : DCtx.t * REnv.t * exp list =
  List.fold_left
    (fun (dctx, renv, exps) exp ->
      let dctx, renv, exp = rename_exp dctx binds renv exp in
      (dctx, renv, exps @ [ exp ]))
    (dctx, renv, []) exps

and rename_exp_base (dctx : DCtx.t) (renv : REnv.t) (exp : exp) :
    DCtx.t * REnv.t * exp =
  let id_rename = Fresh.fresh_from_exp dctx.frees exp in
  let dctx = DCtx.add_free dctx id_rename in
  let renv = REnv.add id_rename (exp, []) renv in
  let exp = VarE id_rename $$ (exp.at, exp.note) in
  (dctx, renv, exp)

and rename_exp_rec (dctx : DCtx.t) (binds : IdSet.t) (renv : REnv.t) (exp : exp)
    : DCtx.t * REnv.t * exp =
  let at, note = (exp.at, exp.note) in
  match exp.it with
  | BoolE _ | NumE _ | TextE _ | VarE _ -> (dctx, renv, exp)
  | TupleE exps ->
      let dctx, renv, exps = rename_exps dctx binds renv exps in
      let exp = TupleE exps $$ (at, note) in
      (dctx, renv, exp)
  | CaseE (mixop, exps) ->
      let dctx, renv, exps = rename_exps dctx binds renv exps in
      let exp = CaseE (mixop, exps) $$ (at, note) in
      (dctx, renv, exp)
  | OptE (Some exp) ->
      let dctx, renv, exp = rename_exp dctx binds renv exp in
      let exp = OptE (Some exp) $$ (at, note) in
      (dctx, renv, exp)
  | OptE None -> (dctx, renv, exp)
  | StrE expfields ->
      let atoms, exps = List.split expfields in
      let dctx, renv, exps = rename_exps dctx binds renv exps in
      let expfields = List.combine atoms exps in
      let exp = StrE expfields $$ (at, note) in
      (dctx, renv, exp)
  | ListE exps ->
      let dctx, renv, exps = rename_exps dctx binds renv exps in
      let exp = ListE exps $$ (at, note) in
      (dctx, renv, exp)
  | ConsE (exp_h, exp_t) ->
      let dctx, renv, exp_h = rename_exp dctx binds renv exp_h in
      let dctx, renv, exp_t = rename_exp dctx binds renv exp_t in
      let exp = ConsE (exp_h, exp_t) $$ (at, note) in
      (dctx, renv, exp)
  | IterE (_, ((_, _ :: _) as iterexp)) ->
      error at
        (Format.asprintf
           "iterated expression should initially have no annotations, but got \
            %s"
           (Il.Print.string_of_iterexp iterexp))
  | IterE (exp, (iter, [])) ->
      let renv_pre = renv in
      let dctx, renv_post, exp = rename_exp dctx binds renv_pre exp in
      let renv = REnv.update_dim iter renv_pre renv_post in
      let exp = IterE (exp, (iter, [])) $$ (at, note) in
      (dctx, renv, exp)
  | CastE (exp, typ) ->
      let dctx, renv, exp = rename_exp dctx binds renv exp in
      let exp = CastE (exp, typ) $$ (at, note) in
      (dctx, renv, exp)
  | _ -> (dctx, renv, exp)

(* Arguments *)

and rename_arg (dctx : DCtx.t) (binds : IdSet.t) (renv : REnv.t) (arg : arg) :
    DCtx.t * REnv.t * arg =
  let at = arg.at in
  match arg.it with
  | ExpA exp ->
      let dctx, renv, exp = rename_exp dctx binds renv exp in
      let arg = ExpA exp $ at in
      (dctx, renv, arg)
  | _ -> (dctx, renv, arg)

and rename_args (dctx : DCtx.t) (binds : IdSet.t) (renv : REnv.t)
    (args : arg list) : DCtx.t * REnv.t * arg list =
  List.fold_left
    (fun (dctx, renv, args) arg ->
      let dctx, renv, arg = rename_arg dctx binds renv arg in
      (dctx, renv, args @ [ arg ]))
    (dctx, renv, []) args
