open Domain.Dom
open El.Ast
open Dom
open Envs
open Util.Error
open Util.Source

(* Error *)

let error (at : region) (msg : string) = error at "elab" msg

(* Context *)

type t = {
  tenv : TEnv.t;
  tdenv : TDEnv.t;
  menv : TEnv.t;
  renv : REnv.t;
  fenv : FEnv.t;
}

(* Constructors *)

let empty : t =
  {
    tenv = TEnv.empty;
    tdenv = TDEnv.empty;
    menv = TEnv.empty;
    renv = REnv.empty;
    fenv = FEnv.empty;
  }

let init () : t =
  let menv =
    TEnv.empty
    |> TEnv.add_nodup "bool" (BoolT $ no_region)
    |> TEnv.add_nodup "nat" (NumT `NatT $ no_region)
    |> TEnv.add_nodup "int" (NumT `IntT $ no_region)
    |> TEnv.add_nodup "text" (TextT $ no_region)
  in
  { empty with menv }

(* Adders *)

(* Adders for type definitions *)

let add_typdef (ctx : t) (tid : TId.t) (td : TypeDef.t) : t =
  let tdenv = TDEnv.add_nodup tid td ctx.tdenv in
  { ctx with tdenv }

let add_tparams (ctx : t) (tparams : tparam list) : t =
  List.fold_left
    (fun ctx tparam -> add_typdef ctx tparam.it TypeDef.Param)
    ctx tparams

(* Adders for meta-variables *)

let add_metavar (ctx : t) (tid : TId.t) (typ : Type.t) : t =
  let menv = TEnv.add_nodup tid typ ctx.menv in
  { ctx with menv }

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

let find_typdef (ctx : t) (tid : TId.t phrase) : TypeDef.t =
  match find_typdef_opt ctx tid.it with
  | Some td -> td
  | None -> error tid.at "undefined type"

let bound_typdef (ctx : t) (tid : TId.t) : bool =
  find_typdef_opt ctx tid |> Option.is_some

(* Finders for meta-variables *)

let find_metavar_opt (ctx : t) (tid : TId.t) : Type.t option =
  TDEnv.find_opt tid ctx.menv

let find_metavar (ctx : t) (tid : TId.t phrase) : Type.t =
  match find_metavar_opt ctx tid.it with
  | Some typ -> typ
  | None -> error tid.at "undefined type"

(* Finders for rules *)

let find_rel_opt (ctx : t) (rid : RId.t) : nottyp option =
  REnv.find_opt rid ctx.renv |> Option.map fst

let find_rel (ctx : t) (rid : RId.t phrase) : nottyp =
  match find_rel_opt ctx rid.it with
  | Some nottyp -> nottyp
  | None -> error rid.at "undefined relation"

let find_rules_opt (ctx : t) (rid : RId.t) : Il.Ast.rule list option =
  REnv.find_opt rid ctx.renv |> Option.map snd

let find_rules (ctx : t) (rid : RId.t phrase) : Il.Ast.rule list =
  match find_rules_opt ctx rid.it with
  | Some rules -> rules
  | None -> error rid.at "undefined relation"

(* Finders for definitions *)

let find_dec_opt (ctx : t) (fid : FId.t) :
    (tparam list * param list * plaintyp) option =
  FEnv.find_opt fid ctx.fenv
  |> Option.map (fun (tparams, params, plaintyp, _) ->
         (tparams, params, plaintyp))

let find_dec (ctx : t) (fid : FId.t phrase) :
    tparam list * param list * plaintyp =
  match find_dec_opt ctx fid.it with
  | Some dec -> dec
  | None -> error fid.at "undefined function"

let find_clauses_opt (ctx : t) (fid : FId.t) : Il.Ast.clause list option =
  FEnv.find_opt fid ctx.fenv |> Option.map (fun (_, _, _, clauses) -> clauses)

let find_clauses (ctx : t) (fid : FId.t phrase) : Il.Ast.clause list =
  match find_clauses_opt ctx fid.it with
  | Some clauses -> clauses
  | None -> error fid.at "undefined function"

(* Getters *)

(* Updaters *)

let update_typdef (ctx : t) (tid : TId.t) (td : TypeDef.t) : t =
  if find_typdef_opt ctx tid |> Option.is_none then assert false;
  { ctx with tdenv = TDEnv.add tid td ctx.tdenv }
