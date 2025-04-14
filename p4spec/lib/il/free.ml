open Domain.Lib
open Ast
open Util.Source

(* Identifier set *)

type t = IdSet.t

let empty = IdSet.empty
let singleton = IdSet.singleton
let ( + ) = IdSet.union

(* Collect free identifiers *)

(* Expressions *)

let rec free_exp (exp : exp) : t =
  match exp.it with
  | BoolE _ | NumE _ | TextE _ -> empty
  | VarE id -> singleton id
  | UnE (_, _, exp) -> free_exp exp
  | BinE (_, _, exp_l, exp_r) -> free_exp exp_l + free_exp exp_r
  | CmpE (_, _, exp_l, exp_r) -> free_exp exp_l + free_exp exp_r
  | UpCastE (_, exp) -> free_exp exp
  | DownCastE (_, exp) -> free_exp exp
  | SubE (exp, _) -> free_exp exp
  | MatchE (exp, _) -> free_exp exp
  | TupleE exps -> free_exps exps
  | CaseE (_, exps) -> free_exps exps
  | StrE expfields -> expfields |> List.map snd |> free_exps
  | OptE (Some exp) -> free_exp exp
  | OptE None -> empty
  | ListE exps -> free_exps exps
  | ConsE (exp_h, exp_t) -> free_exp exp_h + free_exp exp_t
  | CatE (exp_l, exp_r) -> free_exp exp_l + free_exp exp_r
  | MemE (exp_e, exp_s) -> free_exp exp_e + free_exp exp_s
  | LenE exp -> free_exp exp
  | DotE (exp, _) -> free_exp exp
  | IdxE (exp_b, exp_i) -> free_exp exp_b + free_exp exp_i
  | SliceE (exp_b, exp_l, exp_h) ->
      free_exp exp_b + free_exp exp_l + free_exp exp_h
  | UpdE (exp_b, path, exp_f) ->
      free_exp exp_b + free_path path + free_exp exp_f
  | CallE (_, _, args) -> free_args args
  | HoldE (_, (_, exps)) -> free_exps exps
  | IterE (exp, _) -> free_exp exp

and free_exps (exps : exp list) : t =
  exps |> List.map free_exp |> List.fold_left ( + ) empty

(* Paths *)

and free_path (path : path) : t =
  match path.it with
  | RootP -> empty
  | IdxP (path, exp) -> free_path path + free_exp exp
  | SliceP (path, exp_l, exp_h) ->
      free_path path + free_exp exp_l + free_exp exp_h
  | DotP (path, _) -> free_path path

(* Arguments *)

and free_arg (arg : arg) : t =
  match arg.it with ExpA exp -> free_exp exp | DefA _ -> empty

and free_args (args : arg list) : t =
  args |> List.map free_arg |> List.fold_left ( + ) empty

(* Premises *)

let rec free_prem (prem : prem) : t =
  match prem.it with
  | RulePr (_, (_, exps)) -> free_exps exps
  | IfPr exp -> free_exp exp
  | LetPr (exp_l, exp_r) -> free_exp exp_l + free_exp exp_r
  | ElsePr -> empty
  | IterPr (prem, _) -> free_prem prem

and free_prems (prems : prem list) : t =
  prems |> List.map free_prem |> List.fold_left ( + ) empty

(* Definitions *)

let free_rule (rule : rule) : t =
  let _, (_, exps), prems = rule.it in
  free_exps exps + free_prems prems

let free_rules (rules : rule list) : t =
  rules |> List.map free_rule |> List.fold_left ( + ) empty

let free_clause (clause : clause) : t =
  let args, exp, prems = clause.it in
  free_args args + free_exp exp + free_prems prems

let free_clauses (clauses : clause list) : t =
  clauses |> List.map free_clause |> List.fold_left ( + ) empty

let free_def (def : def) : t =
  match def.it with
  | RelD (_, _, _, rules) -> free_rules rules
  | DecD (_, _, _, _, clauses) -> free_clauses clauses
  | _ -> empty
