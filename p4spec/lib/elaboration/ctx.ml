open Domain.Dom
open El.Ast
open Envs
open Util.Source

type t = { tenv : TEnv.t; tdenv : TDEnv.t; renv : REnv.t }

(* Constructors *)

let empty = { tenv = TEnv.empty; tdenv = TDEnv.empty; renv = REnv.empty }

(* Adders *)

(* Adders for type definitions *)

let add_typdef (ctx : t) (tid : TId.t) (td : Typedef.t) : t =
  let tdenv = TDEnv.add_nodup tid td ctx.tdenv in
  { ctx with tdenv }

let add_tparams (ctx : t) (tparams : tparam list) : t =
  List.fold_left
    (fun ctx tparam -> add_typdef ctx tparam.it Typedef.Param)
    ctx tparams

(* Adders for rules *)

let add_rel (ctx : t) (rid : RId.t) (nottyp : nottyp) : t =
  let rel = (nottyp, []) in
  let renv = REnv.add_nodup rid rel ctx.renv in
  { ctx with renv }

let add_rule (ctx : t) (rid : RId.t) (rule : Il.Ast.rule) : t =
  let nottyp, rules = REnv.find rid ctx.renv in
  let rel = (nottyp, rules @ [ rule ]) in
  let renv = REnv.add rid rel ctx.renv in
  { ctx with renv }

(* Finders *)

(* Finders for type definitions *)

let find_typdef_opt (ctx : t) (tid : TId.t) : Typedef.t option =
  TDEnv.find_opt tid ctx.tdenv

(* Finders for rules *)

let find_rel_opt (ctx : t) (rid : RId.t) : nottyp option =
  REnv.find_opt rid ctx.renv |> Option.map fst

(* Updaters *)

let update_typdef (ctx : t) (tid : TId.t) (td : Typedef.t) : t =
  if find_typdef_opt ctx tid |> Option.is_none then assert false;
  { ctx with tdenv = TDEnv.add tid td ctx.tdenv }
