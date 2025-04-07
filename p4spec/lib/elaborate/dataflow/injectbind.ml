open Domain.Lib
open Il.Ast
open Runtime_static
open Error
open Util.Source

(* Rename for an expression *)

module Inject = struct
  type t = Match of pattern | Sub of exp * typ
end

module Placeholder = struct
  type t = exp * Inject.t * iter list
end

module REnv = struct
  type t = (Id.t * Placeholder.t) list

  let gen_prem (id : Id.t) (exp : exp) (inject : Inject.t) (iters : iter list) :
      prem list =
    let exp_placeholder = VarE id $$ (id.at, exp.note) in
    let prems =
      match inject with
      | Match pattern ->
          let exp_match =
            MatchE (exp_placeholder, pattern) $$ (exp.at, BoolT)
          in
          let sidecondition_match = IfPr exp_match $ exp.at in
          let prem_bind = LetPr (exp, exp_placeholder) $ exp.at in
          [ sidecondition_match; prem_bind ]
      | Sub (exp_bind, typ) ->
          let exp_placeholder = VarE id $$ (id.at, exp.note) in
          let exp_sub = SubE (exp_placeholder, typ) $$ (exp.at, BoolT) in
          let sidecondition_sub = IfPr exp_sub $ exp.at in
          let exp_downcast =
            DownCastE (typ, exp_placeholder) $$ (exp.at, typ.it)
          in
          let prem_bind = LetPr (exp_bind, exp_downcast) $ exp_bind.at in
          [ sidecondition_sub; prem_bind ]
    in
    List.map
      (fun prem ->
        List.fold_left
          (fun prem iter -> IterPr (prem, (iter, [])) $ exp.at)
          prem iters)
      prems

  let gen_prems (renv : t) : prem list =
    List.concat_map
      (fun (id, (exp, inject, iters)) -> gen_prem id exp inject iters)
      renv

  let empty : t = []
  let add renv id_rename placeholder = (id_rename, placeholder) :: renv

  let update_dim (iter : iter) (renv_pre : t) (renv_post : t) : t =
    let ids_updated =
      IdSet.diff
        (renv_post |> List.map fst |> IdSet.of_list)
        (renv_pre |> List.map fst |> IdSet.of_list)
    in
    List.map
      (fun (id, (exp, inject, iters)) ->
        if IdSet.mem id ids_updated then (id, (exp, inject, iters @ [ iter ]))
        else (id, (exp, inject, iters)))
      renv_post
end

(* Invariant : Binder patterns contain binding identifiers only *)

let rename (dctx : Dctx.t) (renv : REnv.t) (exp : exp) (inject : Inject.t) :
    Dctx.t * REnv.t * id =
  let id_rename = Fresh.fresh_from_exp dctx.frees exp in
  let dctx = Dctx.add_free dctx id_rename in
  let renv = REnv.add renv id_rename (exp, inject, []) in
  (dctx, renv, id_rename)

(* Expressions *)

let rec is_singleton_case (dctx : Dctx.t) (typ : typ) : bool =
  typ |> Plaintyp.of_internal_typ |> is_singleton_case' dctx

and is_singleton_case' (dctx : Dctx.t) (plaintyp : El.Ast.plaintyp) : bool =
  match plaintyp.it with
  | VarT (tid, targs) -> (
      let td = Dctx.find_typdef dctx tid in
      match td with
      | Defined (tparams, typdef) -> (
          match typdef with
          | `Plain plaintyp ->
              let theta = List.combine tparams targs |> TIdMap.of_list in
              let plaintyp = Plaintyp.subst_plaintyp theta plaintyp in
              is_singleton_case' dctx plaintyp
          | `Struct _ -> false
          | `Variant cases -> List.length cases = 1)
      | _ -> false)
  | _ -> false

let rec rename_exp (dctx : Dctx.t) (renv : REnv.t) (exp : exp) :
    Dctx.t * REnv.t * exp =
  let at, note = (exp.at, exp.note) in
  match exp.it with
  | UpCastE (typ, exp) ->
      let dctx, renv, exp = rename_exp dctx renv exp in
      let exp_renamed = UpCastE (typ, exp) $$ (at, note) in
      let dctx, renv, id_rename =
        rename dctx renv exp_renamed (Inject.Sub (exp, exp.note $ at))
      in
      let exp = VarE id_rename $$ (at, note) in
      (dctx, renv, exp)
  | TupleE exps ->
      let dctx, renv, exps = rename_exps dctx renv exps in
      let exp = TupleE exps $$ (at, note) in
      (dctx, renv, exp)
  | CaseE (mixop, exps) when is_singleton_case dctx (note $ at) ->
      let dctx, renv, exps = rename_exps dctx renv exps in
      let exp = CaseE (mixop, exps) $$ (at, note) in
      (dctx, renv, exp)
  | CaseE (mixop, exps) ->
      let dctx, renv, exps = rename_exps dctx renv exps in
      let exp_renamed = CaseE (mixop, exps) $$ (at, note) in
      let dctx, renv, id_rename =
        rename dctx renv exp_renamed (Inject.Match (CaseP mixop))
      in
      let exp = VarE id_rename $$ (at, note) in
      (dctx, renv, exp)
  | StrE expfields ->
      let atoms, exps = List.split expfields in
      let dctx, renv, exps = rename_exps dctx renv exps in
      let expfields = List.combine atoms exps in
      let exp = StrE expfields $$ (at, note) in
      (dctx, renv, exp)
  | OptE (Some exp) ->
      let dctx, renv, exp = rename_exp dctx renv exp in
      let exp_renamed = OptE (Some exp) $$ (at, note) in
      let dctx, renv, id_rename =
        rename dctx renv exp_renamed (Inject.Match (OptP `Some))
      in
      let exp = VarE id_rename $$ (at, note) in
      (dctx, renv, exp)
  | OptE None ->
      let dctx, renv, id_rename =
        rename dctx renv exp (Inject.Match (OptP `None))
      in
      let exp = VarE id_rename $$ (at, note) in
      (dctx, renv, exp)
  | ListE exps ->
      let dctx, renv, exps = rename_exps dctx renv exps in
      let exp_renamed = ListE exps $$ (at, note) in
      let dctx, renv, id_rename =
        rename dctx renv exp_renamed
          (Inject.Match (ListP (`Fixed (List.length exps))))
      in
      let exp = VarE id_rename $$ (at, note) in
      (dctx, renv, exp)
  | ConsE (exp_h, exp_t) ->
      let dctx, renv, exp_h = rename_exp dctx renv exp_h in
      let dctx, renv, exp_t = rename_exp dctx renv exp_t in
      let exp_renamed = ConsE (exp_h, exp_t) $$ (at, note) in
      let dctx, renv, id_rename =
        rename dctx renv exp_renamed (Inject.Match (ListP `Cons))
      in
      let exp = VarE id_rename $$ (at, note) in
      (dctx, renv, exp)
  | IterE (_, ((_, _ :: _) as iterexp)) ->
      error at
        (Format.asprintf
           "iterated expression should initially have no annotations, but got \
            %s"
           (Il.Print.string_of_iterexp iterexp))
  | IterE (exp, (iter, [])) ->
      let renv_pre = renv in
      let dctx, renv_post, exp = rename_exp dctx renv exp in
      let renv = REnv.update_dim iter renv_pre renv_post in
      let exp = IterE (exp, (iter, [])) $$ (at, note) in
      (dctx, renv, exp)
  | _ -> (dctx, renv, exp)

and rename_exps (dctx : Dctx.t) (renv : REnv.t) (exps : exp list) :
    Dctx.t * REnv.t * exp list =
  List.fold_left
    (fun (dctx, renv, exps) exp ->
      let dctx, renv, exp = rename_exp dctx renv exp in
      (dctx, renv, exps @ [ exp ]))
    (dctx, renv, []) exps

(* Arguments *)

and rename_arg (dctx : Dctx.t) (renv : REnv.t) (arg : arg) :
    Dctx.t * REnv.t * arg =
  let at = arg.at in
  match arg.it with
  | ExpA exp ->
      let dctx, renv, exp = rename_exp dctx renv exp in
      let arg = ExpA exp $ at in
      (dctx, renv, arg)
  | _ -> (dctx, renv, arg)

and rename_args (dctx : Dctx.t) (renv : REnv.t) (args : arg list) :
    Dctx.t * REnv.t * arg list =
  List.fold_left
    (fun (dctx, renv, args) arg ->
      let dctx, renv, arg = rename_arg dctx renv arg in
      (dctx, renv, args @ [ arg ]))
    (dctx, renv, []) args
