open Domain.Dom
open El.Ast
open Dom
open Envs
open Util.Source

type t = { tenv : TEnv.t; tdenv : TDEnv.t; renv : REnv.t; fenv : FEnv.t }

(* Constructors *)

let empty =
  {
    tenv = TEnv.empty;
    tdenv = TDEnv.empty;
    renv = REnv.empty;
    fenv = FEnv.empty;
  }

(* Adders *)

(* Adders for type definitions *)

let add_typdef (ctx : t) (tid : TId.t) (td : TypeDef.t) : t =
  let tdenv = TDEnv.add_nodup tid td ctx.tdenv in
  { ctx with tdenv }

let add_tparams (ctx : t) (tparams : tparam list) : t =
  List.fold_left
    (fun ctx tparam -> add_typdef ctx tparam.it TypeDef.Param)
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

(* Adders for definitions *)

let add_dec (ctx : t) (fid : FId.t) (tparams : tparam list)
    (params : param list) (plaintyp : plaintyp) : t =
  let func = (tparams, params, plaintyp, []) in
  let fenv = FEnv.add_nodup fid func ctx.fenv in
  { ctx with fenv }

let add_clause (ctx : t) (fid : FId.t) (clause : Il.Ast.clause) : t =
  let tparams, params, plaintyp, clauses = FEnv.find fid ctx.fenv in
  let func = (tparams, params, plaintyp, clauses @ [ clause ]) in
  let fenv = FEnv.add fid func ctx.fenv in
  { ctx with fenv }

(* Finders *)

(* Finders for type definitions *)

let find_typdef_opt (ctx : t) (tid : TId.t) : TypeDef.t option =
  TDEnv.find_opt tid ctx.tdenv

(* Finders for rules *)

let find_rel_opt (ctx : t) (rid : RId.t) : nottyp option =
  REnv.find_opt rid ctx.renv |> Option.map fst

let find_rules_opt (ctx : t) (rid : RId.t) : Il.Ast.rule list option =
  REnv.find_opt rid ctx.renv |> Option.map snd

(* Finders for definitions *)

let find_dec_opt (ctx : t) (fid : FId.t) :
    (tparam list * param list * plaintyp) option =
  FEnv.find_opt fid ctx.fenv
  |> Option.map (fun (tparams, params, plaintyp, _) ->
         (tparams, params, plaintyp))

let find_clauses_opt (ctx : t) (fid : FId.t) : Il.Ast.clause list option =
  FEnv.find_opt fid ctx.fenv |> Option.map (fun (_, _, _, clauses) -> clauses)

(* Getters *)

(* Updaters *)

let update_typdef (ctx : t) (tid : TId.t) (td : TypeDef.t) : t =
  if find_typdef_opt ctx tid |> Option.is_none then assert false;
  { ctx with tdenv = TDEnv.add tid td ctx.tdenv }
