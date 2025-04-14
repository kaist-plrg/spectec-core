open Domain.Lib
open Ast
open Util.Source

(* Collect free type identifiers *)

let rec free_tid_plaintyp (plaintyp : plaintyp) : TIdSet.t =
  match plaintyp.it with
  | BoolT | NumT _ | TextT -> TIdSet.empty
  | VarT (id, targs) ->
      TIdSet.union (TIdSet.singleton id) (free_tid_plaintyps targs)
  | ParenT plaintyp -> free_tid_plaintyp plaintyp
  | TupleT plaintyps -> free_tid_plaintyps plaintyps
  | IterT (plaintyp, _) -> free_tid_plaintyp plaintyp

and free_tid_plaintyps (plaintyps : plaintyp list) : TIdSet.t =
  List.fold_left
    (fun frees plaintyp -> free_tid_plaintyp plaintyp |> TIdSet.union frees)
    TIdSet.empty plaintyps

let rec free_tid_param (param : param) : TIdSet.t =
  match param.it with
  | ExpP plaintyp -> free_tid_plaintyp plaintyp
  | DefP (_, tparams, params, plaintyp) ->
      TIdSet.of_list tparams
      |> TIdSet.union (free_tid_params params)
      |> TIdSet.union (free_tid_plaintyp plaintyp)

and free_tid_params (params : param list) : TIdSet.t =
  params |> List.map free_tid_param |> List.fold_left TIdSet.union TIdSet.empty

(* Collect free identifiers *)

(* Expressions *)

let rec free_id_exp (exp : exp) : IdSet.t =
  match exp.it with
  | BoolE _ | NumE _ | TextE _ -> IdSet.empty
  | VarE id -> IdSet.singleton id
  | UnE (_, exp) -> free_id_exp exp
  | BinE (exp_l, _, exp_r) ->
      free_id_exp exp_l |> IdSet.union (free_id_exp exp_r)
  | CmpE (exp_l, _, exp_r) ->
      free_id_exp exp_l |> IdSet.union (free_id_exp exp_r)
  | ArithE exp -> free_id_exp exp
  | EpsE -> IdSet.empty
  | ListE exps -> free_id_exps exps
  | ConsE (exp_h, exp_t) -> free_id_exp exp_h |> IdSet.union (free_id_exp exp_t)
  | CatE (exp_l, exp_r) -> free_id_exp exp_l |> IdSet.union (free_id_exp exp_r)
  | IdxE (exp_b, exp_i) -> free_id_exp exp_b |> IdSet.union (free_id_exp exp_i)
  | SliceE (exp_b, exp_l, exp_h) ->
      free_id_exp exp_b
      |> IdSet.union (free_id_exp exp_l)
      |> IdSet.union (free_id_exp exp_h)
  | LenE exp -> free_id_exp exp
  | MemE (exp_e, exp_s) -> free_id_exp exp_e |> IdSet.union (free_id_exp exp_s)
  | StrE expfields -> expfields |> List.map snd |> free_id_exps
  | DotE (exp, _) -> free_id_exp exp
  | UpdE (exp_b, path, exp_f) ->
      free_id_exp exp_b
      |> IdSet.union (free_id_path path)
      |> IdSet.union (free_id_exp exp_f)
  | ParenE exp -> free_id_exp exp
  | TupleE exps -> free_id_exps exps
  | CallE (_, _, args) -> free_id_args args
  | IterE (exp, _) -> free_id_exp exp
  | TypE (exp, _) -> free_id_exp exp
  | AtomE _ -> IdSet.empty
  | SeqE exps -> free_id_exps exps
  | InfixE (exp_l, _, exp_r) ->
      free_id_exp exp_l |> IdSet.union (free_id_exp exp_r)
  | BrackE (_, exp, _) -> free_id_exp exp
  | HoleE _ -> IdSet.empty
  | FuseE (exp_l, exp_r) -> free_id_exp exp_l |> IdSet.union (free_id_exp exp_r)
  | UnparenE exp -> free_id_exp exp
  | LatexE _ -> IdSet.empty

and free_id_exps (exps : exp list) : IdSet.t =
  exps |> List.map free_id_exp |> List.fold_left IdSet.union IdSet.empty

(* Paths *)

and free_id_path (path : path) : IdSet.t =
  match path.it with
  | RootP -> IdSet.empty
  | IdxP (path, exp) -> free_id_path path |> IdSet.union (free_id_exp exp)
  | SliceP (path, exp_l, exp_h) ->
      free_id_path path
      |> IdSet.union (free_id_exp exp_l)
      |> IdSet.union (free_id_exp exp_h)
  | DotP (path, _) -> free_id_path path

(* Arguments *)

and free_id_arg (arg : arg) : IdSet.t =
  match arg.it with ExpA exp -> free_id_exp exp | DefA _ -> IdSet.empty

and free_id_args (args : arg list) : IdSet.t =
  args |> List.map free_id_arg |> List.fold_left IdSet.union IdSet.empty

(* Premises *)

let rec free_id_prem (prem : prem) : IdSet.t =
  match prem.it with
  | VarPr (id, _) -> IdSet.singleton id
  | RulePr (_, exp) -> free_id_exp exp
  | RuleNotPr (_, exp) -> free_id_exp exp
  | IfPr exp -> free_id_exp exp
  | ElsePr -> IdSet.empty
  | IterPr (prem, _) -> free_id_prem prem

and free_id_prems (prems : prem list) : IdSet.t =
  prems |> List.map free_id_prem |> List.fold_left IdSet.union IdSet.empty

(* Definitions *)

let free_id_def (def : def) : IdSet.t =
  match def.it with
  | RuleD (_, _, exp, prems) ->
      free_id_exp exp |> IdSet.union (free_id_prems prems)
  | DefD (_, _, args, exp, prems) ->
      free_id_args args
      |> IdSet.union (free_id_exp exp)
      |> IdSet.union (free_id_prems prems)
  | _ -> IdSet.empty
