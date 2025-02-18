open Domain.Dom
open Il.Ast
open Attempt
open Util.Source

type t = IdSet.t

(* Constructors *)

let empty = IdSet.empty
let singleton id = IdSet.singleton id
let ( + ) = IdSet.union

(* Collect free identifiers,
   while enforcing the invariant that free identifiers can only occur in
   trivially invertible constructs, or binder patterns *)

let free_nontrivial (at : region) (construct : string) (frees : t) : t attempt =
  if IdSet.is_empty frees then Ok empty
  else fail at ("invalid binding position for non-invertible " ^ construct)

(* Expressions *)

let rec free_exp (bounds : IdSet.t) (exp : exp) : t attempt =
  match exp.it with
  | BoolE _ | NumE _ | TextE _ -> Ok empty
  | VarE id -> if IdSet.mem id bounds then Ok empty else Ok (singleton id)
  | UnE (_, _, exp) ->
      let* frees = free_exp bounds exp in
      free_nontrivial exp.at "unary operator" frees
  | BinE (_, _, exp_l, exp_r) ->
      let* frees_l = free_exp bounds exp_l in
      let* frees_r = free_exp bounds exp_r in
      frees_l + frees_r |> free_nontrivial exp.at "binary operator"
  | CmpE (_, _, exp_l, exp_r) ->
      let* frees_l = free_exp bounds exp_l in
      let* frees_r = free_exp bounds exp_r in
      frees_l + frees_r |> free_nontrivial exp.at "comparison operator"
  | TupleE exps -> free_exps bounds exps
  | CaseE notexp -> notexp |> snd |> free_exps bounds
  | OptE exp_opt ->
      exp_opt
      |> Option.map (free_exp bounds)
      |> Option.value ~default:(Ok empty)
  | StrE expfields -> expfields |> List.map snd |> free_exps bounds
  | DotE (exp, _) ->
      let* frees = free_exp bounds exp in
      free_nontrivial exp.at "dot operator" frees
  | ListE exps -> free_exps bounds exps
  | ConsE (exp_l, exp_r) ->
      let* frees_l = free_exp bounds exp_l in
      let* frees_r = free_exp bounds exp_r in
      Ok (frees_l + frees_r)
  | CatE (exp_l, exp_r) ->
      let* frees_l = free_exp bounds exp_l in
      let* frees_r = free_exp bounds exp_r in
      frees_l + frees_r |> free_nontrivial exp.at "concatenation operator"
  | MemE (exp_l, exp_r) ->
      let* frees_l = free_exp bounds exp_l in
      let* frees_r = free_exp bounds exp_r in
      frees_l + frees_r |> free_nontrivial exp.at "set membership operator"
  | LenE exp ->
      let* frees = free_exp bounds exp in
      free_nontrivial exp.at "length operator" frees
  | IdxE (exp_b, exp_i) ->
      let* frees_b = free_exp bounds exp_b in
      let* frees_i = free_exp bounds exp_i in
      frees_b + frees_i |> free_nontrivial exp.at "indexing operator"
  | SliceE (exp_b, exp_l, exp_h) ->
      let* frees_b = free_exp bounds exp_b in
      let* frees_l = free_exp bounds exp_l in
      let* frees_h = free_exp bounds exp_h in
      frees_b + frees_l + frees_h |> free_nontrivial exp.at "slicing operator"
  | UpdE (exp_b, path, exp_f) ->
      let* frees_b = free_exp bounds exp_b in
      let* frees_f = free_exp bounds exp_f in
      let* frees_p = free_path bounds path in
      frees_b + frees_f + frees_p |> free_nontrivial exp.at "update operator"
  | CallE (_, _, args) ->
      let* frees = free_args bounds args in
      free_nontrivial exp.at "call operator" frees
  (* (TODO) How to handle iteration and casting? *)
  | IterE (exp, _) -> free_exp bounds exp
  | CastE (exp, _) -> free_exp bounds exp

and free_exps (bounds : IdSet.t) (exps : exp list) : t attempt =
  match exps with
  | [] -> Ok empty
  | exp :: exps ->
      let* frees = free_exp bounds exp in
      let* frees' = free_exps bounds exps in
      Ok (frees + frees')

(* Path *)

and free_path (bounds : IdSet.t) (path : path) : t attempt =
  match path.it with
  | RootP -> Ok empty
  | IdxP (path, exp) ->
      let* frees_p = free_path bounds path in
      let* frees_e = free_exp bounds exp in
      frees_p + frees_e |> free_nontrivial path.at "indexing operator"
  | SliceP (path, exp_l, exp_h) ->
      let* frees_p = free_path bounds path in
      let* frees_l = free_exp bounds exp_l in
      let* frees_h = free_exp bounds exp_h in
      frees_p + frees_l + frees_h |> free_nontrivial path.at "slice operator"
  | DotP (path, _) ->
      let* frees = free_path bounds path in
      free_nontrivial path.at "dot operator" frees

(* Arguments *)

and free_arg (bounds : IdSet.t) (arg : arg) : t attempt =
  match arg.it with ExpA exp -> free_exp bounds exp | DefA _ -> Ok empty

and free_args (bounds : IdSet.t) (args : arg list) : t attempt =
  match args with
  | [] -> Ok empty
  | arg :: args ->
      let* frees = free_arg bounds arg in
      let* frees' = free_args bounds args in
      Ok (frees + frees')

(* If premises *)

and free_if_prem (bounds : IdSet.t) (exp : exp) : t attempt =
  match exp.it with
  | CmpE (`EqOp, _, exp_l, exp_r) ->
      let* frees_l = free_exp bounds exp_l in
      let* frees_r = free_exp bounds exp_r in
      if IdSet.is_empty frees_l && IdSet.is_empty frees_r then Ok empty
      else if IdSet.is_empty frees_l then Ok frees_r
      else if IdSet.is_empty frees_r then Ok frees_l
      else fail exp.at "cannot bind both sides of an equality"
  | _ -> free_exp bounds exp
