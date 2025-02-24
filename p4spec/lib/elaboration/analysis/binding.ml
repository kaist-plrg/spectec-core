open Domain.Dom
open Il.Ast
open Attempt
open Envs
open Util.Source

(* Binding environment *)

module Bind = struct
  type t = Single of typ' * Dom.Dim.t | Multi of typ' * Dom.Dim.t

  let get_typ = function Single (typ, _) -> typ | Multi (typ, _) -> typ
  let get_dim = function Single (_, dim) -> dim | Multi (_, dim) -> dim
  let to_string t = t |> get_dim |> Dom.Dim.to_string

  let add_dim (iter : iter) = function
    | Single (typ, iters) -> Single (typ, iters @ [ iter ])
    | Multi (typ, iters) -> Multi (typ, iters @ [ iter ])
end

module BEnv = struct
  include MakeIdEnv (Bind)

  let singleton id typ = add id (Bind.Single (typ, [])) empty

  let union (benv_a : t) (benv_b : t) : t attempt =
    let ids =
      (benv_a |> bindings |> List.map fst) @ (benv_b |> bindings |> List.map fst)
      |> IdSet.of_list
    in
    IdSet.fold
      (fun id benv ->
        let* benv = benv in
        let bind_a = find_opt id benv_a in
        let bind_b = find_opt id benv_b in
        match (bind_a, bind_b) with
        | Some bind_a, Some bind_b ->
            let typ_a = Bind.get_typ bind_a in
            let dim_a = Bind.get_dim bind_a in
            let _typ_b = Bind.get_typ bind_b in
            let dim_b = Bind.get_dim bind_b in
            if Dom.Dim.equiv dim_a dim_b then
              Ok (add id (Bind.Multi (typ_a, dim_a)) benv)
            else
              fail id.at
                (Format.asprintf
                   "inconsistent dimensions for multiple benv of %s: (left) \
                    %s, (right) %s"
                   (Id.to_string id) (Bind.to_string bind_a)
                   (Bind.to_string bind_b))
        | Some bind_a, None -> Ok (add id bind_a benv)
        | None, Some bind_b -> Ok (add id bind_b benv)
        | None, None -> assert false)
      ids (Ok empty)
end

(* Renaming environment *)

module Rename = struct
  type t = IdSet.t

  let to_string = IdSet.to_string ~with_braces:false
end

module REnv = MakeIdEnv (Rename)

(* Collect binding identifiers,
   while enforcing the invariant that binding identifiers can only occur in
   trivially invertible constructs, or binder patterns *)

(* Expressions *)

let bind_nontrivial (at : region) (construct : string) (benv : BEnv.t) :
    BEnv.t attempt =
  if BEnv.is_empty benv then Ok BEnv.empty
  else
    fail at
      (Format.asprintf "invalid binding position(s) for %s in non-invertible %s"
         (BEnv.to_string benv) construct)

let rec bind_exp (ctx : Ctx.t) (exp : exp) : (exp * BEnv.t) attempt =
  let* benv = bind_exp' ctx.venv exp in
  Ok (exp, benv)

and bind_exp' (bounds : VEnv.t) (exp : exp) : BEnv.t attempt =
  match exp.it with
  | BoolE _ | NumE _ | TextE _ -> Ok BEnv.empty
  | VarE id ->
      if VEnv.mem id bounds then Ok BEnv.empty
      else Ok (BEnv.singleton id exp.note)
  | UnE (_, _, exp) ->
      let* benv = bind_exp' bounds exp in
      bind_nontrivial exp.at "unary operator" benv
  | BinE (_, _, exp_l, exp_r) ->
      let* bind_l = bind_exp' bounds exp_l in
      let* bind_r = bind_exp' bounds exp_r in
      let* benv = BEnv.union bind_l bind_r in
      bind_nontrivial exp.at "binary operator" benv
  | CmpE (_, _, exp_l, exp_r) ->
      let* bind_l = bind_exp' bounds exp_l in
      let* bind_r = bind_exp' bounds exp_r in
      let* benv = BEnv.union bind_l bind_r in
      bind_nontrivial exp.at "comparison operator" benv
  | TupleE exps -> bind_exps' bounds exps
  | CaseE notexp -> notexp |> snd |> bind_exps' bounds
  | OptE exp_opt ->
      exp_opt
      |> Option.map (bind_exp' bounds)
      |> Option.value ~default:(Ok BEnv.empty)
  | StrE expfields -> expfields |> List.map snd |> bind_exps' bounds
  | DotE (exp, _) ->
      let* benv = bind_exp' bounds exp in
      bind_nontrivial exp.at "dot operator" benv
  | ListE exps -> bind_exps' bounds exps
  | ConsE (exp_l, exp_r) ->
      let* bind_l = bind_exp' bounds exp_l in
      let* bind_r = bind_exp' bounds exp_r in
      BEnv.union bind_l bind_r
  | CatE (exp_l, exp_r) ->
      let* bind_l = bind_exp' bounds exp_l in
      let* bind_r = bind_exp' bounds exp_r in
      let* benv = BEnv.union bind_l bind_r in
      bind_nontrivial exp.at "concatenation operator" benv
  | MemE (exp_l, exp_r) ->
      let* bind_l = bind_exp' bounds exp_l in
      let* bind_r = bind_exp' bounds exp_r in
      let* benv = BEnv.union bind_l bind_r in
      bind_nontrivial exp.at "set membership operator" benv
  | LenE exp ->
      let* benv = bind_exp' bounds exp in
      bind_nontrivial exp.at "length operator" benv
  | IdxE (exp_b, exp_i) ->
      let* bind_b = bind_exp' bounds exp_b in
      let* bind_i = bind_exp' bounds exp_i in
      let* benv = BEnv.union bind_b bind_i in
      bind_nontrivial exp.at "indexing operator" benv
  | SliceE (exp_b, exp_l, exp_h) ->
      let* bind_b = bind_exp' bounds exp_b in
      let* bind_l = bind_exp' bounds exp_l in
      let* bind_h = bind_exp' bounds exp_h in
      let* benv = BEnv.union bind_b bind_l in
      let* benv = BEnv.union benv bind_h in
      bind_nontrivial exp.at "slicing operator" benv
  | UpdE (exp_b, path, exp_f) ->
      let* bind_b = bind_exp' bounds exp_b in
      let* bind_f = bind_exp' bounds exp_f in
      let* bind_p = bind_path' bounds path in
      let* benv = BEnv.union bind_b bind_f in
      let* benv = BEnv.union benv bind_p in
      bind_nontrivial exp.at "update operator" benv
  | CallE (_, _, args) ->
      let* benv = bind_args' bounds args in
      bind_nontrivial exp.at "call operator" benv
  | IterE (_, (_, (_ :: _ as benv))) ->
      fail exp.at
        ("iterated expression should initially have no annotations, but got "
        ^ String.concat ", " (List.map Il.Print.string_of_var benv))
  | IterE (exp, (iter, [])) ->
      let* benv = bind_exp' bounds exp in
      let benv = BEnv.map (Bind.add_dim iter) benv in
      Ok benv
  | CastE (exp, _) -> bind_exp' bounds exp

and bind_exps (ctx : Ctx.t) (exps : exp list) : (exp list * BEnv.t) attempt =
  let* benv = bind_exps' ctx.venv exps in
  Ok (exps, benv)

and bind_exps' (bounds : VEnv.t) (exps : exp list) : BEnv.t attempt =
  match exps with
  | [] -> Ok BEnv.empty
  | exp :: exps ->
      let* bind_h = bind_exp' bounds exp in
      let* bind_t = bind_exps' bounds exps in
      BEnv.union bind_h bind_t

(* Paths *)

and bind_path (ctx : Ctx.t) (path : path) : (path * BEnv.t) attempt =
  let* benv = bind_path' ctx.venv path in
  Ok (path, benv)

and bind_path' (bounds : VEnv.t) (path : path) : BEnv.t attempt =
  match path.it with
  | RootP -> Ok BEnv.empty
  | IdxP (path, exp) ->
      let* bind_p = bind_path' bounds path in
      let* bind_e = bind_exp' bounds exp in
      let* benv = BEnv.union bind_p bind_e in
      bind_nontrivial path.at "indexing operator" benv
  | SliceP (path, exp_l, exp_h) ->
      let* bind_p = bind_path' bounds path in
      let* bind_l = bind_exp' bounds exp_l in
      let* bind_h = bind_exp' bounds exp_h in
      let* benv = BEnv.union bind_p bind_l in
      let* benv = BEnv.union benv bind_h in
      bind_nontrivial path.at "slice operator" benv
  | DotP (path, _) ->
      let* benv = bind_path' bounds path in
      bind_nontrivial path.at "dot operator" benv

(* Arguments *)

and bind_arg (ctx : Ctx.t) (arg : arg) : (arg * BEnv.t) attempt =
  let* benv = bind_arg' ctx.venv arg in
  Ok (arg, benv)

and bind_arg' (bounds : VEnv.t) (arg : arg) : BEnv.t attempt =
  match arg.it with ExpA exp -> bind_exp' bounds exp | DefA _ -> Ok BEnv.empty

and bind_args (ctx : Ctx.t) (args : arg list) : (arg list * BEnv.t) attempt =
  let* benv = bind_args' ctx.venv args in
  Ok (args, benv)

and bind_args' (bounds : VEnv.t) (args : arg list) : BEnv.t attempt =
  match args with
  | [] -> Ok BEnv.empty
  | arg :: args ->
      let* bind_h = bind_arg' bounds arg in
      let* bind_t = bind_args' bounds args in
      BEnv.union bind_h bind_t

(* Collect binding identifiers,
   while enforcing the invariant that relation inputs should always be bound,
   and also disambiguate `=` in if premises, of whether it is a comparison or a binding *)

let rec bind_prem (ctx : Ctx.t) (prem : prem) : (prem * BEnv.t) attempt =
  match prem.it with
  | RulePr (id, notexp) ->
      let* benv = bind_rule_prem ctx prem.at id notexp in
      Ok (prem, benv)
  | IfPr exp -> bind_if_prem ctx prem.at exp
  | ElsePr -> Ok (prem, BEnv.empty)
  | LetPr _ -> fail prem.at "let premise should appear only after bind analysis"
  | IterPr (_, (_, (_ :: _ as benv))) ->
      fail prem.at
        ("iterated premise should initially have no annotations, but got "
        ^ (List.map Il.Print.string_of_var benv |> String.concat ", "))
  | IterPr (prem, (iter, [])) -> bind_iter_prem ctx prem.at prem iter

and bind_rule_prem (ctx : Ctx.t) (at : region) (id : id) (notexp : notexp) :
    BEnv.t attempt =
  let inputs = Ctx.find_rel ctx id |> snd in
  let exps_input, exps_output =
    notexp |> snd
    |> List.mapi (fun idx exp -> (idx, exp))
    |> List.partition (fun (idx, _) -> List.mem idx inputs)
    |> fun (exps_input, exps_output) ->
    (List.map snd exps_input, List.map snd exps_output)
  in
  let* benv_input = bind_exps' ctx.venv exps_input in
  if not (BEnv.is_empty benv_input) then
    fail at ("rule input has free variable(s): " ^ BEnv.to_string benv_input)
  else bind_exps' ctx.venv exps_output

and bind_if_eq_prem (ctx : Ctx.t) (at : region) (note : region * typ')
    (optyp : optyp) (exp_l : exp) (exp_r : exp) : (prem * BEnv.t) attempt =
  let* bind_l = bind_exp' ctx.venv exp_l in
  let* bind_r = bind_exp' ctx.venv exp_r in
  match (BEnv.is_empty bind_l, BEnv.is_empty bind_r) with
  | true, true ->
      let prem = IfPr (CmpE (`EqOp, optyp, exp_l, exp_r) $$ note) $ at in
      Ok (prem, BEnv.empty)
  | false, true ->
      let prem = LetPr (exp_l, exp_r) $ at in
      Ok (prem, bind_l)
  | true, false ->
      let prem = LetPr (exp_r, exp_l) $ at in
      Ok (prem, bind_r)
  | false, false ->
      fail at
        (Format.asprintf
           "cannot bind on both sides of an equality: (left) %s, (right) %s"
           (BEnv.to_string bind_l) (BEnv.to_string bind_r))

and bind_if_cond_prem (ctx : Ctx.t) (at : region) (exp : exp) : unit attempt =
  let* benv = bind_exp' ctx.venv exp in
  if not (BEnv.is_empty benv) then
    fail at ("condition has free variable(s): " ^ BEnv.to_string benv)
  else Ok ()

and bind_if_prem (ctx : Ctx.t) (at : region) (exp : exp) :
    (prem * BEnv.t) attempt =
  match exp.it with
  | CmpE (`EqOp, optyp, exp_l, exp_r) ->
      bind_if_eq_prem ctx at (exp.at, exp.note) optyp exp_l exp_r
  | _ ->
      let* _ = bind_if_cond_prem ctx at exp in
      let prem = IfPr exp $ at in
      Ok (prem, BEnv.empty)

and bind_iter_prem (ctx : Ctx.t) (at : region) (prem : prem) (iter : iter) :
    (prem * BEnv.t) attempt =
  let* prem, benv = bind_prem ctx prem in
  let benv = BEnv.map (Bind.add_dim iter) benv in
  let prem = IterPr (prem, (iter, [])) $ at in
  Ok (prem, benv)

(* Rename multiple benv, leaving only the leftmost occurrence intact *)

(* Expressions *)

let rec rename_exp (ids : IdSet.t) (ids_multi : IdSet.t) (renv : REnv.t)
    (exp : exp) : IdSet.t * REnv.t * exp =
  let at, note = (exp.at, exp.note) in
  match exp.it with
  | BoolE _ | NumE _ | TextE _ -> (ids, renv, exp)
  | VarE id -> (
      match (IdSet.mem id ids_multi, REnv.find_opt id renv) with
      | false, Some _ -> assert false
      | true, Some renames ->
          let id_rename = Fresh.fresh ids id in
          let ids = IdSet.add id_rename ids in
          let renv =
            let renames = IdSet.add id_rename renames in
            REnv.add id renames renv
          in
          let exp = VarE id_rename $$ (at, note) in
          (ids, renv, exp)
      | true, None -> (ids, REnv.add id IdSet.empty renv, exp)
      | false, None -> (ids, renv, exp))
  | UnE (unop, optyp, exp) ->
      let ids, renv, exp = rename_exp ids ids_multi renv exp in
      let exp = UnE (unop, optyp, exp) $$ (at, note) in
      (ids, renv, exp)
  | BinE (binop, optyp, exp_l, exp_r) ->
      let ids, renv, exp_l = rename_exp ids ids_multi renv exp_l in
      let ids, renv, exp_r = rename_exp ids ids_multi renv exp_r in
      let exp = BinE (binop, optyp, exp_l, exp_r) $$ (at, note) in
      (ids, renv, exp)
  | CmpE (cmpop, optyp, exp_l, exp_r) ->
      let ids, renv, exp_l = rename_exp ids ids_multi renv exp_l in
      let ids, renv, exp_r = rename_exp ids ids_multi renv exp_r in
      let exp = CmpE (cmpop, optyp, exp_l, exp_r) $$ (at, note) in
      (ids, renv, exp)
  | TupleE exps ->
      let ids, renv, exps = rename_exps ids ids_multi renv exps in
      let exp = TupleE exps $$ (at, note) in
      (ids, renv, exp)
  | CaseE (mixop, exps) ->
      let ids, renv, exps = rename_exps ids ids_multi renv exps in
      let exp = CaseE (mixop, exps) $$ (at, note) in
      (ids, renv, exp)
  | OptE exp_opt -> (
      match exp_opt with
      | Some exp ->
          let ids, renv, exp = rename_exp ids ids_multi renv exp in
          let exp = OptE (Some exp) $$ (at, note) in
          (ids, renv, exp)
      | None -> (ids, renv, exp))
  | StrE expfields ->
      let atoms, exps = List.split expfields in
      let ids, renv, exps = rename_exps ids ids_multi renv exps in
      let expfields = List.combine atoms exps in
      let exp = StrE expfields $$ (at, note) in
      (ids, renv, exp)
  | DotE (exp, atom) ->
      let ids, renv, exp = rename_exp ids ids_multi renv exp in
      let exp = DotE (exp, atom) $$ (at, note) in
      (ids, renv, exp)
  | ListE exps ->
      let ids, renv, exps = rename_exps ids ids_multi renv exps in
      let exp = ListE exps $$ (at, note) in
      (ids, renv, exp)
  | ConsE (exp_l, exp_r) ->
      let ids, renv, exp_l = rename_exp ids ids_multi renv exp_l in
      let ids, renv, exp_r = rename_exp ids ids_multi renv exp_r in
      let exp = ConsE (exp_l, exp_r) $$ (at, note) in
      (ids, renv, exp)
  | CatE (exp_l, exp_r) ->
      let ids, renv, exp_l = rename_exp ids ids_multi renv exp_l in
      let ids, renv, exp_r = rename_exp ids ids_multi renv exp_r in
      let exp = CatE (exp_l, exp_r) $$ (at, note) in
      (ids, renv, exp)
  | MemE (exp_l, exp_r) ->
      let ids, renv, exp_l = rename_exp ids ids_multi renv exp_l in
      let ids, renv, exp_r = rename_exp ids ids_multi renv exp_r in
      let exp = MemE (exp_l, exp_r) $$ (at, note) in
      (ids, renv, exp)
  | LenE exp ->
      let ids, renv, exp = rename_exp ids ids_multi renv exp in
      let exp = LenE exp $$ (at, note) in
      (ids, renv, exp)
  | IdxE (exp_b, exp_i) ->
      let ids, renv, exp_b = rename_exp ids ids_multi renv exp_b in
      let ids, renv, exp_i = rename_exp ids ids_multi renv exp_i in
      let exp = IdxE (exp_b, exp_i) $$ (at, note) in
      (ids, renv, exp)
  | SliceE (exp_b, exp_l, exp_h) ->
      let ids, renv, exp_b = rename_exp ids ids_multi renv exp_b in
      let ids, renv, exp_l = rename_exp ids ids_multi renv exp_l in
      let ids, renv, exp_h = rename_exp ids ids_multi renv exp_h in
      let exp = SliceE (exp_b, exp_l, exp_h) $$ (at, note) in
      (ids, renv, exp)
  | UpdE (exp_b, path, exp_f) ->
      let ids, renv, exp_b = rename_exp ids ids_multi renv exp_b in
      let ids, renv, exp_f = rename_exp ids ids_multi renv exp_f in
      let ids, renv, path = rename_path ids ids_multi renv path in
      let exp = UpdE (exp_b, path, exp_f) $$ (at, note) in
      (ids, renv, exp)
  | CallE (id, targs, args) ->
      let ids, renv, args = rename_args ids ids_multi renv args in
      let exp = CallE (id, targs, args) $$ (at, note) in
      (ids, renv, exp)
  | IterE (_, (_, (_ :: _ as benv))) ->
      error at
        ("iterated expression should initially have no annotations, but got "
        ^ String.concat ", " (List.map Il.Print.string_of_var benv))
  | IterE (exp, (iter, [])) ->
      let ids, renv, exp = rename_exp ids ids_multi renv exp in
      let exp = IterE (exp, (iter, [])) $$ (at, note) in
      (ids, renv, exp)
  | CastE (exp, typ) ->
      let ids, renv, exp = rename_exp ids ids_multi renv exp in
      let exp = CastE (exp, typ) $$ (at, note) in
      (ids, renv, exp)

and rename_exps (ids : IdSet.t) (ids_multi : IdSet.t) (renv : REnv.t)
    (exps : exp list) : IdSet.t * REnv.t * exp list =
  List.fold_left
    (fun (ids, renv, exps) exp ->
      let ids, renv, exp = rename_exp ids ids_multi renv exp in
      (ids, renv, exps @ [ exp ]))
    (ids, renv, []) exps

(* Paths *)

and rename_path (ids : IdSet.t) (ids_multi : IdSet.t) (renv : REnv.t)
    (path : path) : IdSet.t * REnv.t * path =
  let at, note = (path.at, path.note) in
  match path.it with
  | RootP -> (ids, renv, path)
  | IdxP (path, exp) ->
      let ids, renv, path = rename_path ids ids_multi renv path in
      let ids, renv, exp = rename_exp ids ids_multi renv exp in
      let path = IdxP (path, exp) $$ (at, note) in
      (ids, renv, path)
  | SliceP (path, exp_l, exp_h) ->
      let ids, renv, path = rename_path ids ids_multi renv path in
      let ids, renv, exp_l = rename_exp ids ids_multi renv exp_l in
      let ids, renv, exp_h = rename_exp ids ids_multi renv exp_h in
      let path = SliceP (path, exp_l, exp_h) $$ (at, note) in
      (ids, renv, path)
  | DotP (path, atom) ->
      let ids, renv, path = rename_path ids ids_multi renv path in
      let path = DotP (path, atom) $$ (at, note) in
      (ids, renv, path)

(* Arguments *)

and rename_arg (ids : IdSet.t) (ids_multi : IdSet.t) (renv : REnv.t) (arg : arg)
    : IdSet.t * REnv.t * arg =
  let at = arg.at in
  match arg.it with
  | ExpA exp ->
      let ids, renv, exp = rename_exp ids ids_multi renv exp in
      let arg = ExpA exp $ at in
      (ids, renv, arg)
  | DefA _ -> (ids, renv, arg)

and rename_args (ids : IdSet.t) (ids_multi : IdSet.t) (renv : REnv.t)
    (args : arg list) : IdSet.t * REnv.t * arg list =
  List.fold_left
    (fun (ids, renv, args) arg ->
      let ids, renv, arg = rename_arg ids ids_multi renv arg in
      (ids, renv, args @ [ arg ]))
    (ids, renv, []) args

(* Premise *)

let rec rename_prem (ids : IdSet.t) (ids_multi : IdSet.t) (renv : REnv.t)
    (prem : prem) : IdSet.t * REnv.t * prem =
  let at = prem.at in
  match prem.it with
  | RulePr (id, (mixop, exps)) ->
      let ids, renv, exps = rename_exps ids ids_multi renv exps in
      let prem = RulePr (id, (mixop, exps)) $ at in
      (ids, renv, prem)
  | IfPr exp ->
      let ids, renv, exp = rename_exp ids ids_multi renv exp in
      let prem = IfPr exp $ at in
      (ids, renv, prem)
  | ElsePr -> (ids, renv, prem)
  | LetPr (exp_l, exp_r) ->
      let ids, renv, exp_l = rename_exp ids ids_multi renv exp_l in
      let ids, renv, exp_r = rename_exp ids ids_multi renv exp_r in
      let prem = LetPr (exp_l, exp_r) $ at in
      (ids, renv, prem)
  | IterPr (_, (_, (_ :: _ as benv))) ->
      error at
        ("iterated premise should initially have no annotations, but got "
        ^ String.concat ", " (List.map Il.Print.string_of_var benv))
  | IterPr (prem, (iter, [])) ->
      let ids, renv, prem = rename_prem ids ids_multi renv prem in
      let prem = IterPr (prem, (iter, [])) $ at in
      (ids, renv, prem)

(* Analysis *)

let generate_sidecondition (benv : BEnv.t) (id : Id.t) (renames : IdSet.t) =
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

let analyze (ctx : Ctx.t) (binder : Ctx.t -> 'a -> ('a * BEnv.t) attempt)
    (renamer : IdSet.t -> IdSet.t -> REnv.t -> 'a -> IdSet.t * REnv.t * 'a)
    (construct : 'a) : ('a * IdSet.t * VEnv.t * prem list) attempt =
  (* Identify binding identifiers *)
  let* construct, benv = binder ctx construct in
  (* Rename multiple benv *)
  let ids_free = ctx.frees in
  let ids_multi =
    benv |> BEnv.bindings
    |> List.filter_map (fun (id, bind) ->
           match bind with Bind.Multi _ -> Some id | Bind.Single _ -> None)
    |> IdSet.of_list
  in
  let ids_free, renv, construct =
    renamer ids_free ids_multi REnv.empty construct
  in
  let ids_free = IdSet.diff ids_free ctx.frees in
  (* Update binding identifiers, taking renamed identifiers into account *)
  let venv =
    benv |> BEnv.map Bind.get_dim
    |> REnv.fold
         (fun id renames venv ->
           let ids_rename = IdSet.elements renames in
           let iters = BEnv.find id benv |> Bind.get_dim in
           List.fold_left
             (fun venv id_rename -> VEnv.add id_rename iters venv)
             venv ids_rename)
         renv
  in
  (* Generate sideconditions *)
  let sideconditions =
    REnv.fold
      (fun id renames sideconditions ->
        let sidecondition = generate_sidecondition benv id renames in
        sideconditions @ [ sidecondition ])
      renv []
  in
  Ok (construct, ids_free, venv, sideconditions)

let analyze_exp (ctx : Ctx.t) (exp : exp) :
    (exp * IdSet.t * VEnv.t * prem list) attempt =
  analyze ctx bind_exp rename_exp exp

let analyze_exps (ctx : Ctx.t) (exps : exp list) :
    (exp list * IdSet.t * VEnv.t * prem list) attempt =
  analyze ctx bind_exps rename_exps exps

let analyze_args (ctx : Ctx.t) (args : arg list) :
    (arg list * IdSet.t * VEnv.t * prem list) attempt =
  analyze ctx bind_args rename_args args

let analyze_prem (ctx : Ctx.t) (prem : prem) :
    (prem * IdSet.t * VEnv.t * prem list) attempt =
  analyze ctx bind_prem rename_prem prem
