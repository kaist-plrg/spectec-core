open Domain.Dom
open Il.Ast
open Attempt
open Envs
open Util.Source

type t = VEnv.t

(* Constructors *)

let empty = VEnv.empty
let singleton id = VEnv.add id [] VEnv.empty
let ( + ) = VEnv.union

let union (at : region) (bind_a : t) (bind_b : t) : t attempt =
  let ids_a = VEnv.bindings bind_a |> List.map fst |> IdSet.of_list in
  let ids_b = VEnv.bindings bind_b |> List.map fst |> IdSet.of_list in
  let ids = IdSet.inter ids_a ids_b in
  if IdSet.is_empty ids then
    Ok (VEnv.union (fun _ -> assert false) bind_a bind_b)
  else
    fail at
      ("multiple bindings for the same identifier(s): "
      ^ (ids |> IdSet.elements |> List.map Id.to_string |> String.concat ", "))

(* Collect binding identifiers,
   while enforcing the invariant that binding identifiers can only occur in
   trivially invertible constructs, or binder patterns *)

let bind_nontrivial (at : region) (construct : string) (binds : t) : t attempt =
  if VEnv.is_empty binds then Ok empty
  else
    fail at
      (Format.asprintf
         "invalid binding position(s) for %s in non-invertible %s construct"
         (VEnv.to_string binds) construct)

(* Expressions *)

let rec bind_exp (bounds : t) (exp : exp) : t attempt =
  match exp.it with
  | BoolE _ | NumE _ | TextE _ -> Ok empty
  | VarE id -> if VEnv.mem id bounds then Ok empty else Ok (singleton id)
  | UnE (_, _, exp) ->
      let* binds = bind_exp bounds exp in
      bind_nontrivial exp.at "unary operator" binds
  | BinE (_, _, exp_l, exp_r) ->
      let* bind_l = bind_exp bounds exp_l in
      let* bind_r = bind_exp bounds exp_r in
      let* binds = union exp.at bind_l bind_r in
      bind_nontrivial exp.at "binary operator" binds
  | CmpE (_, _, exp_l, exp_r) ->
      let* bind_l = bind_exp bounds exp_l in
      let* bind_r = bind_exp bounds exp_r in
      let* binds = union exp.at bind_l bind_r in
      bind_nontrivial exp.at "comparison operator" binds
  | TupleE exps -> bind_exps bounds exps
  | CaseE notexp -> notexp |> snd |> bind_exps bounds
  | OptE exp_opt ->
      exp_opt
      |> Option.map (bind_exp bounds)
      |> Option.value ~default:(Ok empty)
  | StrE expfields -> expfields |> List.map snd |> bind_exps bounds
  | DotE (exp, _) ->
      let* binds = bind_exp bounds exp in
      bind_nontrivial exp.at "dot operator" binds
  | ListE exps -> bind_exps bounds exps
  | ConsE (exp_l, exp_r) ->
      let* bind_l = bind_exp bounds exp_l in
      let* bind_r = bind_exp bounds exp_r in
      union exp.at bind_l bind_r
  | CatE (exp_l, exp_r) ->
      let* bind_l = bind_exp bounds exp_l in
      let* bind_r = bind_exp bounds exp_r in
      let* binds = union exp.at bind_l bind_r in
      bind_nontrivial exp.at "concatenation operator" binds
  | MemE (exp_l, exp_r) ->
      let* bind_l = bind_exp bounds exp_l in
      let* bind_r = bind_exp bounds exp_r in
      let* binds = union exp.at bind_l bind_r in
      bind_nontrivial exp.at "set membership operator" binds
  | LenE exp ->
      let* binds = bind_exp bounds exp in
      bind_nontrivial exp.at "length operator" binds
  | IdxE (exp_b, exp_i) ->
      let* bind_b = bind_exp bounds exp_b in
      let* bind_i = bind_exp bounds exp_i in
      let* binds = union exp.at bind_b bind_i in
      bind_nontrivial exp.at "indexing operator" binds
  | SliceE (exp_b, exp_l, exp_h) ->
      let* bind_b = bind_exp bounds exp_b in
      let* bind_l = bind_exp bounds exp_l in
      let* bind_h = bind_exp bounds exp_h in
      let* binds = union exp.at bind_b bind_l in
      let* binds = union exp.at binds bind_h in
      bind_nontrivial exp.at "slicing operator" binds
  | UpdE (exp_b, path, exp_f) ->
      let* bind_b = bind_exp bounds exp_b in
      let* bind_f = bind_exp bounds exp_f in
      let* bind_p = bind_path bounds path in
      let* binds = union exp.at bind_b bind_f in
      let* binds = union exp.at binds bind_p in
      bind_nontrivial exp.at "update operator" binds
  | CallE (_, _, args) ->
      let* binds = bind_args bounds args in
      bind_nontrivial exp.at "call operator" binds
  | IterE (_, (_, (_ :: _ as binds))) ->
      fail exp.at
        ("iterated expression should initially have no annotations, but got "
        ^ String.concat ", " (List.map Il.Print.string_of_var binds))
  | IterE (exp, (iter, [])) ->
      let* binds = bind_exp bounds exp in
      let binds = VEnv.map (fun iters -> iters @ [ iter ]) binds in
      Ok binds
  | CastE (exp, _) -> bind_exp bounds exp

and bind_exps (bounds : t) (exps : exp list) : t attempt =
  match exps with
  | [] -> Ok empty
  | exp :: exps ->
      let* bind_h = bind_exp bounds exp in
      let* bind_t = bind_exps bounds exps in
      union exp.at bind_h bind_t

(* Path *)

and bind_path (bounds : t) (path : path) : t attempt =
  match path.it with
  | RootP -> Ok empty
  | IdxP (path, exp) ->
      let* bind_p = bind_path bounds path in
      let* bind_e = bind_exp bounds exp in
      let* binds = union path.at bind_p bind_e in
      bind_nontrivial path.at "indexing operator" binds
  | SliceP (path, exp_l, exp_h) ->
      let* bind_p = bind_path bounds path in
      let* bind_l = bind_exp bounds exp_l in
      let* bind_h = bind_exp bounds exp_h in
      let* binds = union path.at bind_p bind_l in
      let* binds = union path.at binds bind_h in
      bind_nontrivial path.at "slice operator" binds
  | DotP (path, _) ->
      let* binds = bind_path bounds path in
      bind_nontrivial path.at "dot operator" binds

(* Arguments *)

and bind_arg (bounds : t) (arg : arg) : t attempt =
  match arg.it with ExpA exp -> bind_exp bounds exp | DefA _ -> Ok empty

and bind_args (bounds : t) (args : arg list) : t attempt =
  match args with
  | [] -> Ok empty
  | arg :: args ->
      let* bind_h = bind_arg bounds arg in
      let* bind_t = bind_args bounds args in
      union arg.at bind_h bind_t

(* Collect binding identifiers,
   while enforcing the invariant that relation inputs should always be bound,
   and also disambiguate `=` in if premises, of whether it is a comparison or a binding *)

let rec bind_prem (ctx : Ctx.t) (prem : prem) : (prem * t) attempt =
  match prem.it with
  | RulePr (id, notexp) ->
      let* binds = bind_rule_prem ctx prem.at id notexp in
      Ok (prem, binds)
  | IfPr exp -> bind_if_prem ctx prem.at exp
  | ElsePr -> Ok (prem, empty)
  | LetPr _ -> fail prem.at "let premise should appear only after bind analysis"
  | IterPr (_, (_, (_ :: _ as binds))) ->
      fail prem.at
        ("iterated premise should initially have no annotations, but got "
        ^ (List.map Il.Print.string_of_var binds |> String.concat ", "))
  | IterPr (prem, (iter, [])) -> bind_iter_prem ctx prem.at prem iter

and bind_rule_prem (ctx : Ctx.t) (at : region) (id : id) (notexp : notexp) :
    t attempt =
  let inputs = Ctx.find_rel ctx id |> snd in
  let exps_input, exps_output =
    notexp |> snd
    |> List.mapi (fun idx exp -> (idx, exp))
    |> List.partition (fun (idx, _) -> List.mem idx inputs)
    |> fun (exps_input, exps_output) ->
    (List.map snd exps_input, List.map snd exps_output)
  in
  let* binds_input = bind_exps ctx.venv exps_input in
  if not (VEnv.is_empty binds_input) then
    fail at ("rule input has free variable(s): " ^ VEnv.to_string binds_input)
  else bind_exps ctx.venv exps_output

and bind_if_eq_prem (ctx : Ctx.t) (at : region) (note : region * typ')
    (optyp : optyp) (exp_l : exp) (exp_r : exp) : (prem * t) attempt =
  let* bind_l = bind_exp ctx.venv exp_l in
  let* bind_r = bind_exp ctx.venv exp_r in
  if VEnv.is_empty bind_l && VEnv.is_empty bind_r then
    let prem = IfPr (CmpE (`EqOp, optyp, exp_l, exp_r) $$ note) $ at in
    Ok (prem, empty)
  else if VEnv.is_empty bind_r then
    let prem = LetPr (exp_l, exp_r) $ at in
    Ok (prem, bind_l)
  else if VEnv.is_empty bind_l then
    let prem = LetPr (exp_r, exp_l) $ at in
    Ok (prem, bind_r)
  else
    fail at
      (Format.asprintf
         "cannot bind both sides of an equality: (left) %s, (right) %s"
         (VEnv.to_string bind_l) (VEnv.to_string bind_r))

and bind_if_cond_prem (ctx : Ctx.t) (at : region) (exp : exp) : unit attempt =
  let* binds = bind_exp ctx.venv exp in
  if not (VEnv.is_empty binds) then
    fail at ("condition has free variable(s): " ^ VEnv.to_string binds)
  else Ok ()

and bind_if_prem (ctx : Ctx.t) (at : region) (exp : exp) : (prem * t) attempt =
  match exp.it with
  | CmpE (`EqOp, optyp, exp_l, exp_r) ->
      bind_if_eq_prem ctx at (exp.at, exp.note) optyp exp_l exp_r
  | _ ->
      let* _ = bind_if_cond_prem ctx at exp in
      let prem = IfPr exp $ at in
      Ok (prem, empty)

and bind_iter_prem (ctx : Ctx.t) (at : region) (prem : prem) (iter : iter) :
    (prem * t) attempt =
  let* prem, binds = bind_prem ctx prem in
  let binds = VEnv.map (fun iters -> iters @ [ iter ]) binds in
  let prem = IterPr (prem, (iter, [])) $ at in
  Ok (prem, binds)
