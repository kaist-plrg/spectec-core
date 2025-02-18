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
  (* Set of bound identifiers *)
  venv : Bound.t;
  (* Map from syntax ids to type definitions *)
  tdenv : TDEnv.t;
  (* Map from meta-variable ids to types *)
  menv : TEnv.t;
  (* Map from relation ids to relations *)
  renv : REnv.t;
  (* Map from function ids to functions *)
  fenv : FEnv.t;
}

(* Constructors *)

let empty : t =
  {
    venv = Bound.empty;
    tdenv = TDEnv.empty;
    menv = TEnv.empty;
    renv = REnv.empty;
    fenv = FEnv.empty;
  }

let init () : t =
  let menv =
    TEnv.empty
    |> TEnv.add ("bool" $ no_region) (BoolT $ no_region)
    |> TEnv.add ("nat" $ no_region) (NumT `NatT $ no_region)
    |> TEnv.add ("int" $ no_region) (NumT `IntT $ no_region)
    |> TEnv.add ("text" $ no_region) (TextT $ no_region)
  in
  { empty with menv }

(* Finders *)

(* Finders for variables *)

let bound_var (ctx : t) (id : Id.t) : bool = Bound.mem id ctx.venv

(* Finders for type definitions *)

let find_typdef_opt (ctx : t) (tid : TId.t) : TypeDef.t option =
  TDEnv.find_opt tid ctx.tdenv

let find_typdef (ctx : t) (tid : TId.t) : TypeDef.t =
  match find_typdef_opt ctx tid with
  | Some td -> td
  | None -> error tid.at "undefined type"

let bound_typdef (ctx : t) (tid : TId.t) : bool =
  find_typdef_opt ctx tid |> Option.is_some

(* Finders for meta-variables *)

let find_metavar_opt (ctx : t) (tid : TId.t) : Type.t option =
  TDEnv.find_opt tid ctx.menv

let find_metavar (ctx : t) (tid : TId.t) : Type.t =
  match find_metavar_opt ctx tid with
  | Some typ -> typ
  | None -> error tid.at "undefined type"

let bound_metavar (ctx : t) (tid : TId.t) : bool =
  find_metavar_opt ctx tid |> Option.is_some

(* Finders for rules *)

let find_rel_opt (ctx : t) (rid : RId.t) : nottyp option =
  REnv.find_opt rid ctx.renv |> Option.map fst

let find_rel (ctx : t) (rid : RId.t) : nottyp =
  match find_rel_opt ctx rid with
  | Some nottyp -> nottyp
  | None -> error rid.at "undefined relation"

let bound_rel (ctx : t) (rid : RId.t) : bool =
  find_rel_opt ctx rid |> Option.is_some

let find_rules_opt (ctx : t) (rid : RId.t) : Il.Ast.rule list option =
  REnv.find_opt rid ctx.renv |> Option.map snd

let find_rules (ctx : t) (rid : RId.t) : Il.Ast.rule list =
  match find_rules_opt ctx rid with
  | Some rules -> rules
  | None -> error rid.at "undefined relation"

(* Finders for definitions *)

let find_dec_opt (ctx : t) (fid : FId.t) :
    (tparam list * param list * plaintyp) option =
  FEnv.find_opt fid ctx.fenv
  |> Option.map (fun (tparams, params, plaintyp, _) ->
         (tparams, params, plaintyp))

let find_dec (ctx : t) (fid : FId.t) : tparam list * param list * plaintyp =
  match find_dec_opt ctx fid with
  | Some dec -> dec
  | None -> error fid.at "undefined function"

let bound_dec (ctx : t) (fid : FId.t) : bool =
  find_dec_opt ctx fid |> Option.is_some

let find_clauses_opt (ctx : t) (fid : FId.t) : Il.Ast.clause list option =
  FEnv.find_opt fid ctx.fenv |> Option.map (fun (_, _, _, clauses) -> clauses)

let find_clauses (ctx : t) (fid : FId.t) : Il.Ast.clause list =
  match find_clauses_opt ctx fid with
  | Some clauses -> clauses
  | None -> error fid.at "undefined function"

(* Adders *)

(* Adders for variables *)

let add_var (ctx : t) (id : Id.t) : t =
  if bound_var ctx id then error id.at "variable already defined";
  let venv = Bound.add id ctx.venv in
  { ctx with venv }

let add_vars (ctx : t) (ids : Id.t list) : t =
  List.fold_left (fun ctx id -> add_var ctx id) ctx ids

(* Adders for type definitions *)

let add_typdef (ctx : t) (tid : TId.t) (td : TypeDef.t) : t =
  if bound_typdef ctx tid then error tid.at "type already defined";
  let tdenv = TDEnv.add tid td ctx.tdenv in
  { ctx with tdenv }

let add_tparams (ctx : t) (tparams : tparam list) : t =
  List.fold_left
    (fun ctx tparam -> add_typdef ctx tparam TypeDef.Param)
    ctx tparams

(* Adders for meta-variables *)

let add_metavar (ctx : t) (tid : TId.t) (typ : Type.t) : t =
  if bound_metavar ctx tid then error tid.at "type already defined";
  let menv = TEnv.add tid typ ctx.menv in
  { ctx with menv }

(* Adders for rules *)

let add_rel (ctx : t) (rid : RId.t) (nottyp : nottyp) : t =
  if bound_rel ctx rid then error rid.at "relation already defined";
  let rel = (nottyp, []) in
  let renv = REnv.add rid rel ctx.renv in
  { ctx with renv }

let add_rule (ctx : t) (rid : RId.t) (rule : Il.Ast.rule) : t =
  if not (bound_rel ctx rid) then error rid.at "undefined relation";
  let nottyp, rules = REnv.find rid ctx.renv in
  let rel = (nottyp, rules @ [ rule ]) in
  let renv = REnv.add rid rel ctx.renv in
  { ctx with renv }

(* Adders for definitions *)

let add_dec (ctx : t) (fid : FId.t) (tparams : tparam list)
    (params : param list) (plaintyp : plaintyp) : t =
  if bound_dec ctx fid then error fid.at "function already defined";
  let func = (tparams, params, plaintyp, []) in
  let fenv = FEnv.add fid func ctx.fenv in
  { ctx with fenv }

let add_clause (ctx : t) (fid : FId.t) (clause : Il.Ast.clause) : t =
  if not (bound_dec ctx fid) then error fid.at "undefined function";
  let tparams, params, plaintyp, clauses = FEnv.find fid ctx.fenv in
  let func = (tparams, params, plaintyp, clauses @ [ clause ]) in
  let fenv = FEnv.add fid func ctx.fenv in
  { ctx with fenv }

(* Updaters *)

let update_typdef (ctx : t) (tid : TId.t) (td : TypeDef.t) : t =
  if not (bound_typdef ctx tid) then error tid.at "undefined type";
  let tdenv = TDEnv.add tid td ctx.tdenv in
  { ctx with tdenv }
