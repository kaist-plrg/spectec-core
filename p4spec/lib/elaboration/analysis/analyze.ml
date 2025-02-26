open Domain.Dom
open Il.Ast
open Attempt
open Envs
open Binding
module Multi = Multibinds
module Partial = Partialbinds
open Util.Source

(* Analysis *)

let generate_sidecondition_multi (benv : BEnv.t) (id : Id.t) (renames : IdSet.t)
    =
  let typ = BEnv.find id benv |> Bind.get_typ in
  let id_rename, ids_rename =
    renames |> IdSet.elements |> fun ids -> (List.hd ids, List.tl ids)
  in
  let exp =
    let exp =
      let exp_l = VarE id $$ (id.at, typ) in
      let exp_r = VarE id_rename $$ (id_rename.at, typ) in
      CmpE (`EqOp, `BoolT, exp_l, exp_r) $$ (no_region, BoolT)
    in
    List.fold_left
      (fun exp_l id_rename ->
        let exp_r =
          let exp_l = VarE id $$ (id.at, typ) in
          let exp_r = VarE id_rename $$ (id_rename.at, typ) in
          CmpE (`EqOp, `BoolT, exp_l, exp_r) $$ (no_region, BoolT)
        in
        BinE (`AndOp, `BoolT, exp_l, exp_r) $$ (no_region, BoolT))
      exp ids_rename
  in
  let sidecondition = IfPr exp $ no_region in
  let iters = BEnv.find id benv |> Bind.get_dim in
  List.fold_left
    (fun sidecondition iter -> IterPr (sidecondition, (iter, [])) $ no_region)
    sidecondition iters

let generate_sidecondition_partial (id : Id.t) (exp : exp) (dim : Dom.Dim.t) =
  let exp =
    let exp_l = VarE id $$ (id.at, exp.note) in
    let exp_r = exp in
    CmpE (`EqOp, `BoolT, exp_l, exp_r) $$ (no_region, BoolT)
  in
  let sidecondition = IfPr exp $ no_region in
  List.fold_left
    (fun sidecondition iter -> IterPr (sidecondition, (iter, [])) $ no_region)
    sidecondition dim

let analyze ~(expect_bind : bool) (ctx : Ctx.t)
    (binder : Ctx.t -> 'a -> ('a * BEnv.t) attempt)
    (renamer_multi :
      IdSet.t -> Multi.REnv.t -> 'a -> IdSet.t * Multi.REnv.t * 'a)
    (renamer_partial :
      IdSet.t ->
      IdSet.t ->
      Partial.REnv.t ->
      'a ->
      IdSet.t * Partial.REnv.t * 'a) (construct : 'a) :
    ('a * IdSet.t * VEnv.t * prem list) attempt =
  (* Identify binding identifiers *)
  let* construct, benv = binder ctx construct in
  (* Rename multiple bindings *)
  let renv_multi =
    benv |> BEnv.bindings
    |> List.filter_map (fun (id, bind) ->
           match bind with Bind.Multi _ -> Some id | Bind.Single _ -> None)
    |> List.fold_left
         (fun renv_multi id -> Multi.REnv.add id IdSet.empty renv_multi)
         Multi.REnv.empty
  in
  let ids_free, renv_multi, construct =
    renamer_multi ctx.frees renv_multi construct
  in
  let renv_multi =
    Multi.REnv.mapi (fun id renames -> IdSet.remove id renames) renv_multi
  in
  (* Update binding identifiers, taking renamed identifiers into account *)
  let venv =
    benv |> BEnv.map Bind.get_dim
    |> Multi.REnv.fold
         (fun id renames venv ->
           let ids_rename = IdSet.elements renames in
           let iters = BEnv.find id benv |> Bind.get_dim in
           List.fold_left
             (fun venv id_rename -> VEnv.add id_rename iters venv)
             venv ids_rename)
         renv_multi
  in
  (* Rename partial bindings *)
  let construct, ids_free, venv, sideconditions_partial =
    if expect_bind then
      let ids_free, renv_partial, construct =
        renamer_partial ids_free (VEnv.dom venv) REnv.empty construct
      in
      (* Update binding identifiers, taking renamed identifiers into account *)
      let venv =
        Partial.REnv.fold
          (fun id (_, dim) venv -> VEnv.add id dim venv)
          renv_partial venv
      in
      let sideconditions_partial =
        Partial.REnv.fold
          (fun id (exp, dim) sideconditions ->
            let sidecondition = generate_sidecondition_partial id exp dim in
            sideconditions @ [ sidecondition ])
          renv_partial []
      in
      (construct, ids_free, venv, sideconditions_partial)
    else (construct, ids_free, venv, [])
  in
  (* Generate sideconditions *)
  let sideconditions_multi =
    Multi.REnv.fold
      (fun id renames sideconditions ->
        if IdSet.is_empty renames then sideconditions
        else
          let sidecondition = generate_sidecondition_multi benv id renames in
          sideconditions @ [ sidecondition ])
      renv_multi []
  in
  let sideconditions = sideconditions_multi @ sideconditions_partial in
  let ids_free = IdSet.diff ids_free ctx.frees in
  Ok (construct, ids_free, venv, sideconditions)

let analyze_exp ~(expect_bind : bool) (ctx : Ctx.t) (exp : exp) :
    (exp * IdSet.t * VEnv.t * prem list) attempt =
  analyze ~expect_bind ctx bind_exp Multi.rename_exp Partial.rename_exp exp

let analyze_exps ~(expect_bind : bool) (ctx : Ctx.t) (exps : exp list) :
    (exp list * IdSet.t * VEnv.t * prem list) attempt =
  analyze ~expect_bind ctx bind_exps Multi.rename_exps Partial.rename_exps exps

let analyze_args ~(expect_bind : bool) (ctx : Ctx.t) (args : arg list) :
    (arg list * IdSet.t * VEnv.t * prem list) attempt =
  analyze ~expect_bind ctx bind_args Multi.rename_args Partial.rename_args args

let analyze_prem (ctx : Ctx.t) (prem : prem) :
    (prem * IdSet.t * VEnv.t * prem list) attempt =
  let henv = REnv.map (fun (_, inputs, _) -> inputs) ctx.renv in
  analyze ~expect_bind:true ctx bind_prem Multi.rename_prem
    (Partial.rename_prem henv) prem
