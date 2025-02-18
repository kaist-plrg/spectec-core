open Domain.Dom
open Il.Ast
open Attempt
open Util.Source

type t = IdSet.t

(* Constructors *)

let empty = IdSet.empty
let singleton id = IdSet.singleton id
let ( + ) = IdSet.union

(* Collect binding identifiers,
   while enforcing the invariant that binding identifiers can only occur in
   trivially invertible constructs, or binder patterns *)

let binding_nontrivial (at : region) (construct : string) (binds : t) :
    t attempt =
  if IdSet.is_empty binds then Ok empty
  else fail at ("invalid binding position for non-invertible " ^ construct)

(* Expressions *)

let rec binding_exp (bounds : IdSet.t) (exp : exp) : t attempt =
  match exp.it with
  | BoolE _ | NumE _ | TextE _ -> Ok empty
  | VarE id -> if IdSet.mem id bounds then Ok empty else Ok (singleton id)
  | UnE (_, _, exp) ->
      let* binds = binding_exp bounds exp in
      binding_nontrivial exp.at "unary operator" binds
  | BinE (_, _, exp_l, exp_r) ->
      let* binds_l = binding_exp bounds exp_l in
      let* binds_r = binding_exp bounds exp_r in
      binds_l + binds_r |> binding_nontrivial exp.at "binary operator"
  | CmpE (_, _, exp_l, exp_r) ->
      let* binds_l = binding_exp bounds exp_l in
      let* binds_r = binding_exp bounds exp_r in
      binds_l + binds_r |> binding_nontrivial exp.at "comparison operator"
  | TupleE exps -> binding_exps bounds exps
  | CaseE notexp -> notexp |> snd |> binding_exps bounds
  | OptE exp_opt ->
      exp_opt
      |> Option.map (binding_exp bounds)
      |> Option.value ~default:(Ok empty)
  | StrE expfields -> expfields |> List.map snd |> binding_exps bounds
  | DotE (exp, _) ->
      let* binds = binding_exp bounds exp in
      binding_nontrivial exp.at "dot operator" binds
  | ListE exps -> binding_exps bounds exps
  | ConsE (exp_l, exp_r) ->
      let* binds_l = binding_exp bounds exp_l in
      let* binds_r = binding_exp bounds exp_r in
      Ok (binds_l + binds_r)
  | CatE (exp_l, exp_r) ->
      let* binds_l = binding_exp bounds exp_l in
      let* binds_r = binding_exp bounds exp_r in
      binds_l + binds_r |> binding_nontrivial exp.at "concatenation operator"
  | MemE (exp_l, exp_r) ->
      let* binds_l = binding_exp bounds exp_l in
      let* binds_r = binding_exp bounds exp_r in
      binds_l + binds_r |> binding_nontrivial exp.at "set membership operator"
  | LenE exp ->
      let* binds = binding_exp bounds exp in
      binding_nontrivial exp.at "length operator" binds
  | IdxE (exp_b, exp_i) ->
      let* binds_b = binding_exp bounds exp_b in
      let* binds_i = binding_exp bounds exp_i in
      binds_b + binds_i |> binding_nontrivial exp.at "indexing operator"
  | SliceE (exp_b, exp_l, exp_h) ->
      let* binds_b = binding_exp bounds exp_b in
      let* binds_l = binding_exp bounds exp_l in
      let* binds_h = binding_exp bounds exp_h in
      binds_b + binds_l + binds_h
      |> binding_nontrivial exp.at "slicing operator"
  | UpdE (exp_b, path, exp_f) ->
      let* binds_b = binding_exp bounds exp_b in
      let* binds_f = binding_exp bounds exp_f in
      let* binds_p = binding_path bounds path in
      binds_b + binds_f + binds_p |> binding_nontrivial exp.at "update operator"
  | CallE (_, _, args) ->
      let* binds = binding_args bounds args in
      binding_nontrivial exp.at "call operator" binds
  (* (TODO) How to handle iteration and casting? *)
  | IterE (exp, _) -> binding_exp bounds exp
  | CastE (exp, _) -> binding_exp bounds exp

and binding_exps (bounds : IdSet.t) (exps : exp list) : t attempt =
  match exps with
  | [] -> Ok empty
  | exp :: exps ->
      let* binds = binding_exp bounds exp in
      let* binds' = binding_exps bounds exps in
      Ok (binds + binds')

(* Path *)

and binding_path (bounds : IdSet.t) (path : path) : t attempt =
  match path.it with
  | RootP -> Ok empty
  | IdxP (path, exp) ->
      let* binds_p = binding_path bounds path in
      let* binds_e = binding_exp bounds exp in
      binds_p + binds_e |> binding_nontrivial path.at "indexing operator"
  | SliceP (path, exp_l, exp_h) ->
      let* binds_p = binding_path bounds path in
      let* binds_l = binding_exp bounds exp_l in
      let* binds_h = binding_exp bounds exp_h in
      binds_p + binds_l + binds_h |> binding_nontrivial path.at "slice operator"
  | DotP (path, _) ->
      let* binds = binding_path bounds path in
      binding_nontrivial path.at "dot operator" binds

(* Arguments *)

and binding_arg (bounds : IdSet.t) (arg : arg) : t attempt =
  match arg.it with ExpA exp -> binding_exp bounds exp | DefA _ -> Ok empty

and binding_args (bounds : IdSet.t) (args : arg list) : t attempt =
  match args with
  | [] -> Ok empty
  | arg :: args ->
      let* binds = binding_arg bounds arg in
      let* binds' = binding_args bounds args in
      Ok (binds + binds')

(* If premises *)

type kind = [ `Equality | `AssignL of t | `AssignR of t ]

let binding_if_prem (bounds : IdSet.t) (at : region) (exp_l : exp) (exp_r : exp)
    : kind attempt =
  let* binds_l = binding_exp bounds exp_l in
  let* binds_r = binding_exp bounds exp_r in
  if IdSet.is_empty binds_l && IdSet.is_empty binds_r then Ok `Equality
  else if IdSet.is_empty binds_r then Ok (`AssignL binds_l)
  else if IdSet.is_empty binds_l then Ok (`AssignR binds_r)
  else fail at "cannot bind both sides of an equality"
