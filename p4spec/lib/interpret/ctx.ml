open Domain.Lib
open Runtime_dynamic
open Envs
open Util.Error
open Util.Source

(* Error *)

let error (at : region) (msg : string) = error at "interp" msg

let error_undef (at : region) (kind : string) (id : string) =
  error at (Format.asprintf "%s `%s` is undefined" kind id)

let error_dup (at : region) (kind : string) (id : string) =
  error at (Format.asprintf "%s `%s` was already defined" kind id)

(* Context *)

type t = {
  (* Map from variables to values *)
  venv : VEnv.t;
  (* Map from syntax ids to type definitions *)
  tdenv : TDEnv.t;
  (* Map from relation ids to relations *)
  renv : REnv.t;
  (* Map from function ids to functions *)
  fenv : FEnv.t;
}

(* Constructors *)

let empty : t =
  {
    venv = VEnv.empty;
    tdenv = TDEnv.empty;
    renv = REnv.empty;
    fenv = FEnv.empty;
  }

let localize (ctx : t) : t = { ctx with venv = VEnv.empty }

(* Finders *)

(* Finders for values *)

let find_value_opt (ctx : t) (var : Var.t) : Value.t option =
  VEnv.find_opt var ctx.venv

let find_value (ctx : t) (var : Var.t) : Value.t =
  match find_value_opt ctx var with
  | Some value -> value
  | None ->
      let id, _ = var in
      error_undef id.at "value" id.it

let bound_value (ctx : t) (var : Var.t) : bool =
  find_value_opt ctx var |> Option.is_some

(* Finders for type definitions *)

let find_typdef_opt (ctx : t) (tid : TId.t) : Typdef.t option =
  TDEnv.find_opt tid ctx.tdenv

let find_typdef (ctx : t) (tid : TId.t) : Typdef.t =
  match find_typdef_opt ctx tid with
  | Some td -> td
  | None -> error_undef tid.at "type" tid.it

let bound_typdef (ctx : t) (tid : TId.t) : bool =
  find_typdef_opt ctx tid |> Option.is_some

(* Finders for rules *)

let find_rel_opt (ctx : t) (rid : RId.t) : Rel.t option =
  REnv.find_opt rid ctx.renv

let find_rel (ctx : t) (rid : RId.t) : Rel.t =
  match find_rel_opt ctx rid with
  | Some rel -> rel
  | None -> error_undef rid.at "relation" rid.it

let bound_rel (ctx : t) (rid : RId.t) : bool =
  find_rel_opt ctx rid |> Option.is_some

(* Finders for definitions *)

let find_func_opt (ctx : t) (fid : FId.t) : Func.t option =
  FEnv.find_opt fid ctx.fenv

let find_func (ctx : t) (fid : FId.t) : Func.t =
  match find_func_opt ctx fid with
  | Some func -> func
  | None -> error_undef fid.at "function" fid.it

let bound_func (ctx : t) (fid : FId.t) : bool =
  find_func_opt ctx fid |> Option.is_some

(* Adders *)

(* Adders for values *)

let add_value (ctx : t) (var : Var.t) (value : Value.t) : t =
  (if bound_value ctx var then
     let id, _ = var in
     error_dup id.at "value" id.it);
  let venv = VEnv.add var value ctx.venv in
  { ctx with venv }

(* Adders for type definitions *)

let add_typdef (ctx : t) (tid : TId.t) (td : Typdef.t) : t =
  if bound_typdef ctx tid then error_dup tid.at "type" tid.it;
  let tdenv = TDEnv.add tid td ctx.tdenv in
  { ctx with tdenv }

(* Adders for relations *)

let add_rel (ctx : t) (rid : RId.t) (rel : Rel.t) : t =
  if bound_rel ctx rid then error_dup rid.at "relation" rid.it;
  let renv = REnv.add rid rel ctx.renv in
  { ctx with renv }

(* Adders for functions *)

let add_func (ctx : t) (fid : FId.t) (func : Func.t) : t =
  if bound_func ctx fid then error_dup fid.at "function" fid.it;
  let fenv = FEnv.add fid func ctx.fenv in
  { ctx with fenv }
