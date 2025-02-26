open Domain.Dom
open Il.Ast
open Attempt
open Envs
open Util.Source

(* Environment for identifier occurrence and bindings *)

module Bind = struct
  type t = Single of typ' * Dom.Dim.t | Multi of typ' * Dom.Dim.t

  let get_typ = function Single (typ, _) -> typ | Multi (typ, _) -> typ
  let get_dim = function Single (_, dim) -> dim | Multi (_, dim) -> dim

  let to_string t =
    Format.asprintf "(%s)%s"
      (get_typ t $ no_region |> Il.Print.string_of_typ)
      (get_dim t |> Dom.Dim.to_string)

  let add_dim (iter : iter) = function
    | Single (typ, iters) -> Single (typ, iters @ [ iter ])
    | Multi (typ, iters) -> Multi (typ, iters @ [ iter ])
end

module BEnv = struct
  include MakeIdEnv (Bind)

  let singleton id typ = add id (Bind.Single (typ, [])) empty

  let union (benv_a : t) (benv_b : t) : t attempt =
    let ids =
      IdSet.union
        (benv_a |> bindings |> List.map fst |> IdSet.of_list)
        (benv_b |> bindings |> List.map fst |> IdSet.of_list)
    in
    IdSet.fold
      (fun id benv ->
        let* benv = benv in
        let bind_a = find_opt id benv_a in
        let bind_b = find_opt id benv_b in
        match (bind_a, bind_b) with
        | Some bind_a, Some bind_b ->
            let typ_a, dim_a = (Bind.get_typ bind_a, Bind.get_dim bind_a) in
            let _typ_b, dim_b = (Bind.get_typ bind_b, Bind.get_dim bind_b) in
            if Dom.Dim.equiv dim_a dim_b then
              Ok (add id (Bind.Multi (typ_a, dim_a)) benv)
            else
              fail id.at
                (Format.asprintf
                   "inconsistent dimensions for multiple bindings of %s: \
                    (left) %s, (right) %s"
                   (Id.to_string id) (Bind.to_string bind_a)
                   (Bind.to_string bind_b))
        | Some bind_a, None -> Ok (add id bind_a benv)
        | None, Some bind_b -> Ok (add id bind_b benv)
        | None, None -> assert false)
      ids (Ok empty)
end

(* Collect binding identifiers,
   while enforcing the invariant that binding identifiers
   can only occur in invertible constructs *)

(* Expressions *)

let bind_noninvertible (at : region) (construct : string) (benv : BEnv.t) :
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
      bind_noninvertible exp.at "unary operator" benv
  | BinE (_, _, exp_l, exp_r) ->
      let* bind_l = bind_exp' bounds exp_l in
      let* bind_r = bind_exp' bounds exp_r in
      let* benv = BEnv.union bind_l bind_r in
      bind_noninvertible exp.at "binary operator" benv
  | CmpE (_, _, exp_l, exp_r) ->
      let* bind_l = bind_exp' bounds exp_l in
      let* bind_r = bind_exp' bounds exp_r in
      let* benv = BEnv.union bind_l bind_r in
      bind_noninvertible exp.at "comparison operator" benv
  | TupleE exps -> bind_exps' bounds exps
  | CaseE notexp -> notexp |> snd |> bind_exps' bounds
  | OptE exp_opt ->
      exp_opt
      |> Option.map (bind_exp' bounds)
      |> Option.value ~default:(Ok BEnv.empty)
  | StrE expfields -> expfields |> List.map snd |> bind_exps' bounds
  | DotE (exp, _) ->
      let* benv = bind_exp' bounds exp in
      bind_noninvertible exp.at "dot operator" benv
  | ListE exps -> bind_exps' bounds exps
  | ConsE (exp_l, exp_r) ->
      let* bind_l = bind_exp' bounds exp_l in
      let* bind_r = bind_exp' bounds exp_r in
      BEnv.union bind_l bind_r
  | CatE (exp_l, exp_r) ->
      let* bind_l = bind_exp' bounds exp_l in
      let* bind_r = bind_exp' bounds exp_r in
      let* benv = BEnv.union bind_l bind_r in
      bind_noninvertible exp.at "concatenation operator" benv
  | MemE (exp_l, exp_r) ->
      let* bind_l = bind_exp' bounds exp_l in
      let* bind_r = bind_exp' bounds exp_r in
      let* benv = BEnv.union bind_l bind_r in
      bind_noninvertible exp.at "set membership operator" benv
  | LenE exp ->
      let* benv = bind_exp' bounds exp in
      bind_noninvertible exp.at "length operator" benv
  | IdxE (exp_b, exp_i) ->
      let* bind_b = bind_exp' bounds exp_b in
      let* bind_i = bind_exp' bounds exp_i in
      let* benv = BEnv.union bind_b bind_i in
      bind_noninvertible exp.at "indexing operator" benv
  | SliceE (exp_b, exp_l, exp_h) ->
      let* bind_b = bind_exp' bounds exp_b in
      let* bind_l = bind_exp' bounds exp_l in
      let* bind_h = bind_exp' bounds exp_h in
      let* benv = BEnv.union bind_b bind_l in
      let* benv = BEnv.union benv bind_h in
      bind_noninvertible exp.at "slicing operator" benv
  | UpdE (exp_b, path, exp_f) ->
      let* bind_b = bind_exp' bounds exp_b in
      let* bind_f = bind_exp' bounds exp_f in
      let* bind_p = bind_path' bounds path in
      let* benv = BEnv.union bind_b bind_f in
      let* benv = BEnv.union benv bind_p in
      bind_noninvertible exp.at "update operator" benv
  | CallE (_, _, args) ->
      let* benv = bind_args' bounds args in
      bind_noninvertible exp.at "call operator" benv
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
      bind_noninvertible path.at "indexing operator" benv
  | SliceP (path, exp_l, exp_h) ->
      let* bind_p = bind_path' bounds path in
      let* bind_l = bind_exp' bounds exp_l in
      let* bind_h = bind_exp' bounds exp_h in
      let* benv = BEnv.union bind_p bind_l in
      let* benv = BEnv.union benv bind_h in
      bind_noninvertible path.at "slice operator" benv
  | DotP (path, _) ->
      let* benv = bind_path' bounds path in
      bind_noninvertible path.at "dot operator" benv

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
