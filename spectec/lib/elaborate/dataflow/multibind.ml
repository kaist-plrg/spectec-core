open Domain.Lib
open Il.Ast
open Error
open Util.Source

(* Renames for an identifier *)

module Ids = struct
  include IdSet

  let to_string = to_string ~with_braces:false
end

(* Renaming environment *)

module REnv = struct
  include MakeIdEnv (Ids)

  let init (benv : Bind.BEnv.t) : t =
    Bind.BEnv.fold
      (fun id bind renv ->
        match bind with
        | Bind.Occ.Multi _ -> add id Ids.empty renv
        | Bind.Occ.Single _ -> renv)
      benv empty
end

(* Generate sideconditions *)

let gen_sidecondition (benv : Bind.BEnv.t) (id : Id.t) (ids_rename : Ids.t) :
    prem =
  let typ, iters = Bind.BEnv.find id benv |> Bind.Occ.strip in
  let id_rename, ids_rename =
    ids_rename |> IdSet.elements |> fun ids -> (List.hd ids, List.tl ids)
  in
  let exp =
    let exp =
      let exp_l = VarE id $$ (id.at, typ.it) in
      let exp_r = VarE id_rename $$ (id_rename.at, typ.it) in
      CmpE (`EqOp, `BoolT, exp_l, exp_r) $$ (id_rename.at, BoolT)
    in
    List.fold_left
      (fun exp_l id_rename ->
        let exp_r =
          let exp_l = VarE id $$ (id.at, typ.it) in
          let exp_r = VarE id_rename $$ (id_rename.at, typ.it) in
          CmpE (`EqOp, `BoolT, exp_l, exp_r) $$ (id_rename.at, BoolT)
        in
        BinE (`AndOp, `BoolT, exp_l, exp_r) $$ (id_rename.at, BoolT))
      exp ids_rename
  in
  let sidecondition = IfPr exp $ id.at in
  List.fold_left
    (fun sidecondition iter -> IterPr (sidecondition, (iter, [])) $ id.at)
    sidecondition iters

let gen_sideconditions (benv : Bind.BEnv.t) (renv : REnv.t) : prem list =
  let renv = REnv.mapi Ids.remove renv in
  REnv.fold
    (fun id ids_rename sideconditions ->
      if Ids.is_empty ids_rename then sideconditions
      else
        let sidecondition = gen_sidecondition benv id ids_rename in
        sideconditions @ [ sidecondition ])
    renv []

(* Rename multiple bindings, leaving the leftmost binding occurrence intact *)

(* Expressions *)

let rec rename_exp (dctx : Dctx.t) (renv : REnv.t) (exp : exp) :
    Dctx.t * REnv.t * exp =
  let at, note = (exp.at, exp.note) in
  match exp.it with
  | VarE id -> (
      match REnv.find_opt id renv with
      (* Leftmost binding occurrence *)
      | Some ids_rename when IdSet.is_empty ids_rename ->
          let exp = VarE id $$ (at, note) in
          let renv =
            let ids_rename = IdSet.singleton id in
            REnv.add id ids_rename renv
          in
          (dctx, renv, exp)
      (* Parallel binding occurrences *)
      | Some ids_rename ->
          let id_rename = Fresh.fresh_id dctx.frees id in
          let dctx = Dctx.add_free dctx id_rename in
          let renv =
            let ids_rename = IdSet.add id_rename ids_rename in
            REnv.add id ids_rename renv
          in
          let exp = VarE id_rename $$ (at, note) in
          (dctx, renv, exp)
      | None -> (dctx, renv, exp))
  | UpCastE (typ, exp) ->
      let dctx, renv, exp = rename_exp dctx renv exp in
      let exp = UpCastE (typ, exp) $$ (at, note) in
      (dctx, renv, exp)
  | TupleE exps ->
      let dctx, renv, exps = rename_exps dctx renv exps in
      let exp = TupleE exps $$ (at, note) in
      (dctx, renv, exp)
  | CaseE (mixop, exps) ->
      let dctx, renv, exps = rename_exps dctx renv exps in
      let exp = CaseE (mixop, exps) $$ (at, note) in
      (dctx, renv, exp)
  | StrE expfields ->
      let atoms, exps = List.split expfields in
      let dctx, renv, exps = rename_exps dctx renv exps in
      let expfields = List.combine atoms exps in
      let exp = StrE expfields $$ (at, note) in
      (dctx, renv, exp)
  | OptE (Some exp) ->
      let dctx, renv, exp = rename_exp dctx renv exp in
      let exp = OptE (Some exp) $$ (at, note) in
      (dctx, renv, exp)
  | OptE None -> (dctx, renv, exp)
  | ListE exps ->
      let dctx, renv, exps = rename_exps dctx renv exps in
      let exp = ListE exps $$ (at, note) in
      (dctx, renv, exp)
  | ConsE (exp_h, exp_t) ->
      let dctx, renv, exp_h = rename_exp dctx renv exp_h in
      let dctx, renv, exp_t = rename_exp dctx renv exp_t in
      let exp = ConsE (exp_h, exp_t) $$ (at, note) in
      (dctx, renv, exp)
  | IterE (_, ((_, _ :: _) as iterexp)) ->
      error at
        (Format.asprintf
           "iterated expression should initially have no annotations, but got \
            %s"
           (Il.Print.string_of_iterexp iterexp))
  | IterE (exp, (iter, [])) ->
      let dctx, renv, exp = rename_exp dctx renv exp in
      let exp = IterE (exp, (iter, [])) $$ (at, note) in
      (dctx, renv, exp)
  (* Unnecessary to handle non-invertible constructs *)
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
  | DefA _ -> (dctx, renv, arg)

and rename_args (dctx : Dctx.t) (renv : REnv.t) (args : arg list) :
    Dctx.t * REnv.t * arg list =
  List.fold_left
    (fun (dctx, renv, args) arg ->
      let dctx, renv, arg = rename_arg dctx renv arg in
      (dctx, renv, args @ [ arg ]))
    (dctx, renv, []) args
