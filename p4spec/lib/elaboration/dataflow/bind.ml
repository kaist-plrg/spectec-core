open Domain.Lib
open Il.Ast
open Attempt
open Dom
open Envs
module DCtx = Dctx
open Util.Source

(* Binding occurrences of identifiers, singular or multiple (parallel) *)

module Bind = struct
  type t = Single of typ' * Dim.t | Multi of typ' * Dim.t

  let to_string = function
    | Single (typ, dim) | Multi (typ, dim) ->
        Format.asprintf "(%s)%s"
          (Il.Print.string_of_typ (typ $ no_region))
          (Dim.to_string dim)

  let get_typ = function Single (typ, _) -> typ | Multi (typ, _) -> typ
  let get_dim = function Single (_, dim) -> dim | Multi (_, dim) -> dim

  let add_dim (iter : iter) = function
    | Single (typ, iters) -> Single (typ, iters @ [ iter ])
    | Multi (typ, iters) -> Multi (typ, iters @ [ iter ])
end

(* Environment for identifier bindings *)

module BEnv = struct
  include MakeIdEnv (Bind)

  let singleton id typ = add id (Bind.Single (typ, [])) empty
  let flatten (benv : t) : VEnv.t = map Bind.get_dim benv

  let union (benv_a : t) (benv_b : t) : t =
    let ids = IdSet.union (dom benv_a) (dom benv_b) in
    IdSet.fold
      (fun id benv ->
        let collect_a = find_opt id benv_a in
        let collect_b = find_opt id benv_b in
        match (collect_a, collect_b) with
        | Some collect_a, Some collect_b ->
            let typ_a, dim_a =
              (Bind.get_typ collect_a, Bind.get_dim collect_a)
            in
            let _typ_b, dim_b =
              (Bind.get_typ collect_b, Bind.get_dim collect_b)
            in
            if not (Dim.equiv dim_a dim_b) then
              error id.at
                (Format.asprintf
                   "inconsistent dimensions for multiple bindings of %s: \
                    (left) %s, (right) %s"
                   (Id.to_string id) (Bind.to_string collect_a)
                   (Bind.to_string collect_b));
            add id (Bind.Multi (typ_a, dim_a)) benv
        | Some collect, None | None, Some collect -> add id collect benv
        | None, None -> assert false)
      ids empty
end

(* Collect binding identifiers,
   while enforcing the invariant that binding identifiers
   can only occur in invertible constructs *)

let collect_noninvertible (at : region) (construct : string) (benv : BEnv.t) :
    unit =
  if not (BEnv.is_empty benv) then
    error at
      (Format.asprintf "invalid binding position(s) for %s in non-invertible %s"
         (BEnv.to_string benv) construct)

(* Expressions *)

let rec collect_exp (dctx : DCtx.t) (exp : exp) : BEnv.t =
  match exp.it with
  | BoolE _ | NumE _ | TextE _ -> BEnv.empty
  | VarE id ->
      if VEnv.mem id dctx.bounds then BEnv.empty else BEnv.singleton id exp.note
  | UnE (_, _, exp) ->
      let binds = collect_exp dctx exp in
      collect_noninvertible exp.at "unary operator" binds;
      BEnv.empty
  | BinE (_, _, exp_l, exp_r) ->
      let binds_l = collect_exp dctx exp_l in
      let binds_r = collect_exp dctx exp_r in
      let binds = BEnv.union binds_l binds_r in
      collect_noninvertible exp.at "binary operator" binds;
      BEnv.empty
  | CmpE (_, _, exp_l, exp_r) ->
      let binds_l = collect_exp dctx exp_l in
      let binds_r = collect_exp dctx exp_r in
      let binds = BEnv.union binds_l binds_r in
      collect_noninvertible exp.at "comparison operator" binds;
      BEnv.empty
  | TupleE exps -> collect_exps dctx exps
  | CaseE notexp -> notexp |> snd |> collect_exps dctx
  | OptE exp_opt ->
      exp_opt
      |> Option.map (collect_exp dctx)
      |> Option.value ~default:BEnv.empty
  | StrE expfields -> expfields |> List.map snd |> collect_exps dctx
  | DotE (exp, _) ->
      let binds = collect_exp dctx exp in
      collect_noninvertible exp.at "dot operator" binds;
      BEnv.empty
  | ListE exps -> collect_exps dctx exps
  | ConsE (exp_l, exp_r) ->
      let binds_l = collect_exp dctx exp_l in
      let binds_r = collect_exp dctx exp_r in
      BEnv.union binds_l binds_r
  | CatE (exp_l, exp_r) ->
      let binds_l = collect_exp dctx exp_l in
      let binds_r = collect_exp dctx exp_r in
      let binds = BEnv.union binds_l binds_r in
      collect_noninvertible exp.at "concatenation operator" binds;
      BEnv.empty
  | MemE (exp_l, exp_r) ->
      let binds_l = collect_exp dctx exp_l in
      let binds_r = collect_exp dctx exp_r in
      let binds = BEnv.union binds_l binds_r in
      collect_noninvertible exp.at "set membership operator" binds;
      BEnv.empty
  | LenE exp ->
      let binds = collect_exp dctx exp in
      collect_noninvertible exp.at "length operator" binds;
      BEnv.empty
  | IdxE (exp_b, exp_i) ->
      let binds_b = collect_exp dctx exp_b in
      let binds_i = collect_exp dctx exp_i in
      let binds = BEnv.union binds_b binds_i in
      collect_noninvertible exp.at "indexing operator" binds;
      BEnv.empty
  | SliceE (exp_b, exp_l, exp_h) ->
      let binds_b = collect_exp dctx exp_b in
      let binds_l = collect_exp dctx exp_l in
      let binds_h = collect_exp dctx exp_h in
      let binds = BEnv.union binds_b binds_l in
      let binds = BEnv.union binds binds_h in
      collect_noninvertible exp.at "slicing operator" binds;
      BEnv.empty
  | UpdE (exp_b, path, exp_f) ->
      let binds_b = collect_exp dctx exp_b in
      let binds_p = collect_path dctx path in
      let binds_f = collect_exp dctx exp_f in
      let binds = BEnv.union binds_b binds_f in
      let binds = BEnv.union binds binds_p in
      collect_noninvertible exp.at "update operator" binds;
      BEnv.empty
  | CallE (_, _, args) ->
      let binds = collect_args dctx args in
      collect_noninvertible exp.at "call operator" binds;
      BEnv.empty
  | IterE (_, ((_, _ :: _) as iterexp)) ->
      error exp.at
        (Format.asprintf
           "iterated expression should initially have no annotations, but got \
            %s"
           (Il.Print.string_of_iterexp iterexp))
  | IterE (exp, (iter, [])) ->
      let binds = collect_exp dctx exp in
      let binds = BEnv.map (Bind.add_dim iter) binds in
      binds
  | CastE (exp, _) -> collect_exp dctx exp

and collect_exps (dctx : DCtx.t) (exps : exp list) : BEnv.t =
  match exps with
  | [] -> BEnv.empty
  | exp :: exps ->
      let binds_h = collect_exp dctx exp in
      let binds_t = collect_exps dctx exps in
      BEnv.union binds_h binds_t

(* Paths *)

and collect_path (dctx : DCtx.t) (path : path) : BEnv.t =
  match path.it with
  | RootP -> BEnv.empty
  | IdxP (path, exp) ->
      let binds_p = collect_path dctx path in
      let binds_e = collect_exp dctx exp in
      BEnv.union binds_p binds_e
  | SliceP (path, exp_l, exp_h) ->
      let binds_p = collect_path dctx path in
      let binds_l = collect_exp dctx exp_l in
      let binds_h = collect_exp dctx exp_h in
      let binds = BEnv.union binds_p binds_l in
      BEnv.union binds binds_h
  | DotP (path, _) -> collect_path dctx path

(* Arguments *)

and collect_arg (dctx : DCtx.t) (arg : arg) : BEnv.t =
  match arg.it with ExpA exp -> collect_exp dctx exp | DefA _ -> BEnv.empty

and collect_args (dctx : DCtx.t) (args : arg list) : BEnv.t =
  match args with
  | [] -> BEnv.empty
  | arg :: args ->
      let binds_h = collect_arg dctx arg in
      let binds_t = collect_args dctx args in
      BEnv.union binds_h binds_t
