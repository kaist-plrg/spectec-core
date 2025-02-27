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
  | VarE (id, _) -> singleton id
  | UnE (_, exp) -> free_exp exp
  | BinE (exp_l, _, exp_r) -> free_exp exp_l + free_exp exp_r
  | CmpE (exp_l, _, exp_r) -> free_exp exp_l + free_exp exp_r
  | ArithE exp -> free_exp exp
  | EpsE -> empty
  | ListE exps -> free_exps exps
  | ConsE (exp_h, exp_t) -> free_exp exp_h + free_exp exp_t
  | CatE (exp_l, exp_r) -> free_exp exp_l + free_exp exp_r
  | IdxE (exp_b, exp_i) -> free_exp exp_b + free_exp exp_i
  | SliceE (exp_b, exp_l, exp_h) ->
      free_exp exp_b + free_exp exp_l + free_exp exp_h
  | LenE exp -> free_exp exp
  | MemE (exp_e, exp_s) -> free_exp exp_e + free_exp exp_s
  | StrE expfields -> expfields |> List.map snd |> free_exps
  | DotE (exp, _) -> free_exp exp
  | UpdE (exp_b, path, exp_f) ->
      free_exp exp_b + free_path path + free_exp exp_f
  | ParenE exp -> free_exp exp
  | TupleE exps -> free_exps exps
  | CallE (_, _, args) -> free_args args
  | IterE (exp, _) -> free_exp exp
  | TypE (exp, _) -> free_exp exp
  | AtomE _ -> empty
  | SeqE exps -> free_exps exps
  | InfixE (exp_l, _, exp_r) -> free_exp exp_l + free_exp exp_r
  | BrackE (_, exp, _) -> free_exp exp
  | HoleE _ -> empty
  | FuseE (exp_l, exp_r) -> free_exp exp_l + free_exp exp_r
  | UnparenE exp -> free_exp exp
  | LatexE _ -> empty

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
  | VarPr (id, _) -> singleton id
  | RulePr (_, exp) -> free_exp exp
  | IfPr exp -> free_exp exp
  | ElsePr -> empty
  | IterPr (prem, _) -> free_prem prem

and free_prems (prems : prem list) : t =
  prems |> List.map free_prem |> List.fold_left ( + ) empty

(* Definitions *)

let free_def (def : def) : t =
  match def.it with
  | RuleD (_, _, exp, prems) -> free_exp exp + free_prems prems
  | DefD (_, _, args, exp, prems) ->
      free_args args + free_exp exp + free_prems prems
  | _ -> empty
