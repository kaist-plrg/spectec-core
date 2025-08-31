open Domain.Lib
open El.Ast
open Runtime_static
open Envs
open Error
open Util.Source

(* Error *)

let error_undef (at : region) (kind : string) (id : string) =
  error at (Format.asprintf "%s `%s` is undefined" kind id)

let error_dup (at : region) (kind : string) (id : string) =
  error at (Format.asprintf "%s `%s` was already defined" kind id)

(* Global counter for unique identifiers *)

let tick = ref 0
let refresh () = tick := 0

let fresh () =
  let id = !tick in
  tick := !tick + 1;
  id

(* Context *)

type t = {
  (* Set of free ids, for unique id insertion *)
  frees : IdSet.t;
  (* Map from variable ids to dimensions *)
  venv : VEnv.t;
  (* Map from syntax ids to type definitions *)
  tdenv : TDEnv.t;
  (* Map from meta-variable ids to types *)
  menv : PTEnv.t;
  (* Map from relation ids to relations *)
  renv : REnv.t;
  (* Map from function ids to functions *)
  fenv : FEnv.t;
}

(* Constructors *)

let empty : t =
  {
    frees = IdSet.empty;
    venv = VEnv.empty;
    tdenv = TDEnv.empty;
    menv = PTEnv.empty;
    renv = REnv.empty;
    fenv = FEnv.empty;
  }

let init () : t =
  let menv =
    PTEnv.empty
    |> PTEnv.add ("bool" $ no_region) (BoolT $ no_region)
    |> PTEnv.add ("nat" $ no_region) (NumT `NatT $ no_region)
    |> PTEnv.add ("int" $ no_region) (NumT `IntT $ no_region)
    |> PTEnv.add ("text" $ no_region) (TextT $ no_region)
  in
  { empty with menv }

(* Finders *)

(* Finders for variables *)

let bound_var (ctx : t) (id : Id.t) : bool = VEnv.mem id ctx.venv

(* Finders for type definitions *)

let find_typdef_opt (ctx : t) (tid : TId.t) : Typdef.t option =
  TDEnv.find_opt tid ctx.tdenv

let find_typdef (ctx : t) (tid : TId.t) : Typdef.t =
  match find_typdef_opt ctx tid with
  | Some td -> td
  | None -> error_undef tid.at "type" tid.it

let bound_typdef (ctx : t) (tid : TId.t) : bool =
  find_typdef_opt ctx tid |> Option.is_some

(* Finders for meta-variables *)

let find_metavar_opt (ctx : t) (tid : TId.t) : Plaintyp.t option =
  TDEnv.find_opt tid ctx.menv

let find_metavar (ctx : t) (tid : TId.t) : Plaintyp.t =
  match find_metavar_opt ctx tid with
  | Some plaintyp -> plaintyp
  | None -> error_undef tid.at "meta-variable" tid.it

let bound_metavar (ctx : t) (tid : TId.t) : bool =
  find_metavar_opt ctx tid |> Option.is_some

(* Finders for rules *)

let find_rel_opt (ctx : t) (rid : RId.t) : (nottyp * int list) option =
  REnv.find_opt rid ctx.renv
  |> Option.map (fun (nottyp, inputs, _) -> (nottyp, inputs))

let find_rel (ctx : t) (rid : RId.t) : nottyp * int list =
  match find_rel_opt ctx rid with
  | Some (nottyp, inputs) -> (nottyp, inputs)
  | None -> error_undef rid.at "relation" rid.it

let bound_rel (ctx : t) (rid : RId.t) : bool =
  find_rel_opt ctx rid |> Option.is_some

let find_rules_opt (ctx : t) (rid : RId.t) : Il.Ast.rule list option =
  REnv.find_opt rid ctx.renv |> Option.map (fun (_, _, rules) -> rules)

let find_rules (ctx : t) (rid : RId.t) : Il.Ast.rule list =
  match find_rules_opt ctx rid with
  | Some rules -> rules
  | None -> error_undef rid.at "relation" rid.it

(* Finders for definitions *)

let find_dec_opt (ctx : t) (fid : FId.t) :
    (tparam list * param list * plaintyp) option =
  FEnv.find_opt fid ctx.fenv
  |> Option.map (fun (tparams, params, plaintyp, _) ->
         (tparams, params, plaintyp))

let find_dec (ctx : t) (fid : FId.t) : tparam list * param list * plaintyp =
  match find_dec_opt ctx fid with
  | Some dec -> dec
  | None -> error_undef fid.at "function" fid.it

let bound_dec (ctx : t) (fid : FId.t) : bool =
  find_dec_opt ctx fid |> Option.is_some

let find_clauses_opt (ctx : t) (fid : FId.t) : Il.Ast.clause list option =
  FEnv.find_opt fid ctx.fenv |> Option.map (fun (_, _, _, clauses) -> clauses)

let find_clauses (ctx : t) (fid : FId.t) : Il.Ast.clause list =
  match find_clauses_opt ctx fid with
  | Some clauses -> clauses
  | None -> error_undef fid.at "function" fid.it

(* Adders *)

(* Adders for free variables *)

let add_free (ctx : t) (id : Id.t) : t =
  let frees = IdSet.add id ctx.frees in
  { ctx with frees }

let add_frees (ctx : t) (ids : IdSet.t) : t =
  ids |> IdSet.elements |> List.fold_left (fun ctx id -> add_free ctx id) ctx

(* Adders for variables *)

let add_var (ctx : t) (var : Id.t * Typ.t) : t =
  let id, typ = var in
  if bound_var ctx id then error_dup id.at "variable" id.it;
  let venv = VEnv.add id typ ctx.venv in
  { ctx with venv }

let add_vars (ctx : t) (vars : (Id.t * Typ.t) list) : t =
  List.fold_left add_var ctx vars

(* Adders for meta-variables *)

let add_metavar (ctx : t) (tid : TId.t) (plaintyp : Plaintyp.t) : t =
  if bound_metavar ctx tid then error_dup tid.at "meta-variable" tid.it;
  let menv = PTEnv.add tid plaintyp ctx.menv in
  { ctx with menv }

(* Adders for type definitions *)

let add_typdef (ctx : t) (tid : TId.t) (td : Typdef.t) : t =
  if bound_typdef ctx tid then error_dup tid.at "type" tid.it;
  let tdenv = TDEnv.add tid td ctx.tdenv in
  { ctx with tdenv }

let add_tparam (ctx : t) (tparam : tparam) : t =
  let ctx = add_typdef ctx tparam Typdef.Param in
  add_metavar ctx tparam (VarT (tparam, []) $ tparam.at)

let add_tparams (ctx : t) (tparams : tparam list) : t =
  List.fold_left add_tparam ctx tparams

(* Adders for rules *)

let add_rel (ctx : t) (rid : RId.t) (nottyp : nottyp) (inputs : int list) : t =
  if bound_rel ctx rid then error_dup rid.at "relation" rid.it;
  let rel = (nottyp, inputs, []) in
  let renv = REnv.add rid rel ctx.renv in
  { ctx with renv }

let add_rule (ctx : t) (rid : RId.t) (rule : Il.Ast.rule) : t =
  if not (bound_rel ctx rid) then error_undef rid.at "relation" rid.it;
  let nottyp, inputs, rules = REnv.find rid ctx.renv in
  let rel = (nottyp, inputs, rules @ [ rule ]) in
  let renv = REnv.add rid rel ctx.renv in
  { ctx with renv }

(* Adders for definitions *)

let add_dec (ctx : t) (fid : FId.t) (tparams : tparam list)
    (params : param list) (plaintyp : plaintyp) : t =
  if bound_dec ctx fid then error_dup fid.at "function" fid.it;
  let func = (tparams, params, plaintyp, []) in
  let fenv = FEnv.add fid func ctx.fenv in
  { ctx with fenv }

let add_clause (ctx : t) (fid : FId.t) (clause : Il.Ast.clause) : t =
  if not (bound_dec ctx fid) then error_undef clause.at "function" fid.it;
  let tparams, params, plaintyp, clauses = FEnv.find fid ctx.fenv in
  let func = (tparams, params, plaintyp, clauses @ [ clause ]) in
  let fenv = FEnv.add fid func ctx.fenv in
  { ctx with fenv }

(* Updaters *)

let update_typdef (ctx : t) (tid : TId.t) (td : Typdef.t) : t =
  if not (bound_typdef ctx tid) then error_undef tid.at "type" tid.it;
  let tdenv = TDEnv.add tid td ctx.tdenv in
  { ctx with tdenv }
