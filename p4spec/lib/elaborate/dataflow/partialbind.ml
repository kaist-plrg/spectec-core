open Domain.Lib
open Il.Ast
open Runtime_static
open Error
open Util.Source

(* Rename for an expression *)

module Placeholder = struct
  type t =
    | Bound of { exp_orig : exp; iters : iter list }
    | Bindmatch of { pattern : pattern; exp_orig : exp; iters : iter list }
    | Bindsub of {
        typ_sub : typ;
        exp_sub : exp;
        exp_orig : exp;
        iters : iter list;
      }

  let add_iter (placeholder : t) (iter : iter) : t =
    match placeholder with
    | Bound { exp_orig; iters } -> Bound { exp_orig; iters = iters @ [ iter ] }
    | Bindmatch { pattern; exp_orig; iters } ->
        Bindmatch { pattern; exp_orig; iters = iters @ [ iter ] }
    | Bindsub { typ_sub; exp_sub; exp_orig; iters } ->
        Bindsub { typ_sub; exp_sub; exp_orig; iters = iters @ [ iter ] }
end

module REnv = struct
  type t = (Id.t * Placeholder.t) list

  (* Construstor *)

  let empty : t = []

  (* Adder and updater *)

  let add renv id_rename placeholder = (id_rename, placeholder) :: renv

  let update_dim (iter : iter) (renv_pre : t) (renv_post : t) : t =
    let ids_updated =
      IdSet.diff
        (renv_post |> List.map fst |> IdSet.of_list)
        (renv_pre |> List.map fst |> IdSet.of_list)
    in
    List.map
      (fun (id, placeholder) ->
        let placeholder =
          if IdSet.mem id ids_updated then Placeholder.add_iter placeholder iter
          else placeholder
        in
        (id, placeholder))
      renv_post

  (* Generate premises *)

  let gen_prem_bound (id : Id.t) (exp_orig : exp) (iters : iter list) : prem =
    let exp_cond =
      let exp_l = VarE id $$ (id.at, exp_orig.note) in
      match exp_orig.it with
      | OptE (Some _) -> MatchE (exp_l, OptP `Some) $$ (exp_orig.at, BoolT)
      | OptE None -> MatchE (exp_l, OptP `None) $$ (exp_orig.at, BoolT)
      | ListE [] -> MatchE (exp_l, ListP `Nil) $$ (exp_orig.at, BoolT)
      | ListE exps ->
          MatchE (exp_l, ListP (`Fixed (List.length exps)))
          $$ (exp_orig.at, BoolT)
      | _ ->
          let exp_r = exp_orig in
          CmpE (`EqOp, `BoolT, exp_l, exp_r) $$ (exp_orig.at, BoolT)
    in
    let sidecondition = IfPr exp_cond $ exp_orig.at in
    List.fold_left
      (fun sidecondition iter ->
        IterPr (sidecondition, (iter, [])) $ exp_orig.at)
      sidecondition iters

  let gen_prem_bind_match (id : Id.t) (pattern : pattern) (exp_orig : exp)
      (iters : iter list) : prem list =
    let exp_placeholder = VarE id $$ (id.at, exp_orig.note) in
    let prems =
      let exp_guard_match =
        MatchE (exp_placeholder, pattern) $$ (exp_orig.at, BoolT)
      in
      let sidecondition_guard_match = IfPr exp_guard_match $ exp_orig.at in
      let prem_bind = LetPr (exp_orig, exp_placeholder) $ exp_orig.at in
      [ sidecondition_guard_match; prem_bind ]
    in
    List.map
      (fun prem ->
        List.fold_left
          (fun prem iter -> IterPr (prem, (iter, [])) $ exp_orig.at)
          prem iters)
      prems

  let gen_prem_bind_sub (id : Id.t) (typ_sub : typ) (exp_sub : exp)
      (exp_orig : exp) (iters : iter list) : prem list =
    let exp_placeholder = VarE id $$ (id.at, exp_orig.note) in
    let prems =
      let exp_guard_sub =
        SubE (exp_placeholder, typ_sub) $$ (exp_orig.at, BoolT)
      in
      let sidecondition_guard_sub = IfPr exp_guard_sub $ exp_orig.at in
      let exp_downcast =
        DownCastE (typ_sub, exp_placeholder) $$ (exp_orig.at, typ_sub.it)
      in
      let prem_bind = LetPr (exp_sub, exp_downcast) $ exp_orig.at in
      [ sidecondition_guard_sub; prem_bind ]
    in
    List.map
      (fun prem ->
        List.fold_left
          (fun prem iter -> IterPr (prem, (iter, [])) $ exp_orig.at)
          prem iters)
      prems

  let gen_prem (id : Id.t) (placeholder : Placeholder.t) : prem list =
    match placeholder with
    | Bound { exp_orig; iters } -> [ gen_prem_bound id exp_orig iters ]
    | Bindmatch { pattern; exp_orig; iters } ->
        gen_prem_bind_match id pattern exp_orig iters
    | Bindsub { typ_sub; exp_sub; exp_orig; iters } ->
        gen_prem_bind_sub id typ_sub exp_sub exp_orig iters

  let gen_prems (renv : t) : prem list =
    List.concat_map (fun (id, placeholder) -> gen_prem id placeholder) renv
end

(* Desugar partial bindings, occuring as either:

   (1) Bound values occurring inside binder patterns

   -- let PATTERN (a, 1 + 2) = pat

   becomes,

   -- let PATTERN (a, b) = pat
   -- if b == 1 + 2

   (2) Injection of a variant case

   -- let PATTERN (a, b) = pat

   becomes,

   -- if pat matches PATTERN
   -- let PATTERN (a, b) = pat

   (3) Injection of a subtype case

   -- let ((int) n) = $foo()

   becomes,

   -- let i = $foo()
   -- if i <: nat
   -- let n = i as nat *)

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

let rename_exp_bind_match (dctx : Dctx.t) (renv : REnv.t) (pattern : pattern)
    (exp_orig : exp) : Dctx.t * REnv.t * id =
  let id_rename = Fresh.fresh_from_exp dctx.frees exp_orig in
  let dctx = Dctx.add_free dctx id_rename in
  let placeholder = Placeholder.Bindmatch { pattern; exp_orig; iters = [] } in
  let renv = REnv.add renv id_rename placeholder in
  (dctx, renv, id_rename)

let rename_exp_bind_sub (dctx : Dctx.t) (renv : REnv.t) (typ_sub : typ)
    (exp_sub : exp) (exp_orig : exp) : Dctx.t * REnv.t * id =
  let id_rename = Fresh.fresh_from_exp dctx.frees exp_orig in
  let dctx = Dctx.add_free dctx id_rename in
  let placeholder =
    Placeholder.Bindsub { typ_sub; exp_sub; exp_orig; iters = [] }
  in
  let renv = REnv.add renv id_rename placeholder in
  (dctx, renv, id_rename)

let rec rename_exp (dctx : Dctx.t) (binds : IdSet.t) (renv : REnv.t) (exp : exp)
    : Dctx.t * REnv.t * exp =
  let frees = Il.Free.free_exp exp in
  (* If the expression contains no bindings, rename it *)
  if IdSet.inter binds frees |> IdSet.is_empty then
    rename_exp_bound dctx renv exp
  else rename_exp_bind dctx binds renv exp

and rename_exp_bound (dctx : Dctx.t) (renv : REnv.t) (exp : exp) :
    Dctx.t * REnv.t * exp =
  let id_rename = Fresh.fresh_from_exp dctx.frees exp in
  let dctx = Dctx.add_free dctx id_rename in
  let placeholder = Placeholder.Bound { exp_orig = exp; iters = [] } in
  let renv = REnv.add renv id_rename placeholder in
  let exp = VarE id_rename $$ (exp.at, exp.note) in
  (dctx, renv, exp)

and rename_exp_bind (dctx : Dctx.t) (binds : IdSet.t) (renv : REnv.t)
    (exp : exp) : Dctx.t * REnv.t * exp =
  let at, note = (exp.at, exp.note) in
  match exp.it with
  | UpCastE (typ, exp) ->
      let dctx, renv, exp = rename_exp dctx binds renv exp in
      let exp_renamed = UpCastE (typ, exp) $$ (at, note) in
      let dctx, renv, id_rename =
        rename_exp_bind_sub dctx renv (exp.note $ at) exp exp_renamed
      in
      let exp = VarE id_rename $$ (at, note) in
      (dctx, renv, exp)
  | TupleE exps ->
      let dctx, renv, exps = rename_exps dctx binds renv exps in
      let exp = TupleE exps $$ (at, note) in
      (dctx, renv, exp)
  | CaseE (mixop, exps) when is_singleton_case dctx (note $ at) ->
      let dctx, renv, exps = rename_exps dctx binds renv exps in
      let exp = CaseE (mixop, exps) $$ (at, note) in
      (dctx, renv, exp)
  | CaseE (mixop, exps) ->
      let dctx, renv, exps = rename_exps dctx binds renv exps in
      let exp_renamed = CaseE (mixop, exps) $$ (at, note) in
      let dctx, renv, id_rename =
        rename_exp_bind_match dctx renv (CaseP mixop) exp_renamed
      in
      let exp = VarE id_rename $$ (at, note) in
      (dctx, renv, exp)
  | StrE expfields ->
      let atoms, exps = List.split expfields in
      let dctx, renv, exps = rename_exps dctx binds renv exps in
      let expfields = List.combine atoms exps in
      let exp = StrE expfields $$ (at, note) in
      (dctx, renv, exp)
  | OptE (Some exp) ->
      let dctx, renv, exp = rename_exp dctx binds renv exp in
      let exp_renamed = OptE (Some exp) $$ (at, note) in
      let dctx, renv, id_rename =
        rename_exp_bind_match dctx renv (OptP `Some) exp_renamed
      in
      let exp = VarE id_rename $$ (at, note) in
      (dctx, renv, exp)
  | OptE None ->
      let dctx, renv, id_rename =
        rename_exp_bind_match dctx renv (OptP `None) exp
      in
      let exp = VarE id_rename $$ (at, note) in
      (dctx, renv, exp)
  | ListE exps ->
      let dctx, renv, exps = rename_exps dctx binds renv exps in
      let exp_renamed = ListE exps $$ (at, note) in
      let dctx, renv, id_rename =
        let pattern =
          if List.length exps = 0 then ListP `Nil
          else ListP (`Fixed (List.length exps))
        in
        rename_exp_bind_match dctx renv pattern exp_renamed
      in
      let exp = VarE id_rename $$ (at, note) in
      (dctx, renv, exp)
  | ConsE (exp_h, exp_t) ->
      let dctx, renv, exp_h = rename_exp dctx binds renv exp_h in
      let dctx, renv, exp_t = rename_exp dctx binds renv exp_t in
      let exp_renamed = ConsE (exp_h, exp_t) $$ (at, note) in
      let dctx, renv, id_rename =
        rename_exp_bind_match dctx renv (ListP `Cons) exp_renamed
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
      let dctx, renv_post, exp = rename_exp dctx binds renv_pre exp in
      let renv = REnv.update_dim iter renv_pre renv_post in
      let exp = IterE (exp, (iter, [])) $$ (at, note) in
      (dctx, renv, exp)
  | _ -> (dctx, renv, exp)

and rename_exps (dctx : Dctx.t) (binds : IdSet.t) (renv : REnv.t)
    (exps : exp list) : Dctx.t * REnv.t * exp list =
  List.fold_left
    (fun (dctx, renv_pre, exps) exp ->
      let dctx, renv_post, exp = rename_exp dctx binds REnv.empty exp in
      (dctx, renv_pre @ renv_post, exps @ [ exp ]))
    (dctx, renv, []) exps

(* Arguments *)

and rename_arg (dctx : Dctx.t) (binds : IdSet.t) (renv : REnv.t) (arg : arg) :
    Dctx.t * REnv.t * arg =
  let at = arg.at in
  match arg.it with
  | ExpA exp ->
      let dctx, renv, exp = rename_exp dctx binds renv exp in
      let arg = ExpA exp $ at in
      (dctx, renv, arg)
  | _ -> (dctx, renv, arg)

and rename_args (dctx : Dctx.t) (binds : IdSet.t) (renv : REnv.t)
    (args : arg list) : Dctx.t * REnv.t * arg list =
  List.fold_left
    (fun (dctx, renv_pre, args) arg ->
      let dctx, renv_post, arg = rename_arg dctx binds REnv.empty arg in
      (dctx, renv_pre @ renv_post, args @ [ arg ]))
    (dctx, renv, []) args
