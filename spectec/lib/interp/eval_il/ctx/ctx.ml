open Common.Source
open Common.Domain
open Envs.Make
open Envs.Il
open Lang.Il
open Envs.Interp
module Value = Lang.Il.Value
open Error
open Attempt

(* Error *)

let error_undef (at : region) (kind : string) (id : string) =
  error at (Format.asprintf "%s `%s` is undefined" kind id)

let error_dup (at : region) (kind : string) (id : string) =
  error at (Format.asprintf "%s `%s` was already defined" kind id)

(* Environment map types *)

module Func = Func
module TDEnv = TDEnv
module VEnv = VEnv

(* Global layer *)
module Global = struct
  (* Type definition environment *)
  module TDEnv = MakeFrozenTIdTbl (Typdef)

  (* Relation environment *)
  module REnv = MakeFrozenRIdTbl (Rel)

  (* Function environment *)
  module FEnv = MakeFrozenFIdTbl (Func)
end

(* Local layer *)
module Local = struct
  (* Type definition environment *)
  module TDEnv = TDEnv

  (* Function environment *)
  module FEnv = MakeFIdMap (Func)

  (* Value environment *)
  module VEnv = VEnv
end

(* Context *)

(* Cursor *)

type cursor = Global | Local

(* Global loader (mutable, used during load phase) *)
type global_loader = {
  tdenv : Global.TDEnv.loader;
  renv : Global.REnv.loader;
  fenv : Global.FEnv.loader;
}

(* Global layer *)
type global = {
  (* Frozen hashtable from syntax ids to type definitions *)
  tdenv : Global.TDEnv.t;
  (* Frozen hashtable from relation ids to relations *)
  renv : Global.REnv.t;
  (* Frozen hashtable from function ids to functions *)
  fenv : Global.FEnv.t;
}

(* Local layer *)
type local = {
  (* Map from syntax ids to type definitions *)
  tdenv : Local.TDEnv.t;
  (* Map from function ids to functions *)
  fenv : Local.FEnv.t;
  (* Map from variables to values *)
  venv : Local.VEnv.t;
}

type t = {
  (* Filename of the source file *)
  filename : string;
  (* Builtins *)
  builtins : Builtins.t;
  (* Cache *)
  cache : Cache.t;
  (* Global layer *)
  global : global;
  (* Local layer *)
  local : local;
}

(* Finders *)

(* Finders for values *)

let find_value_opt (ctx : t) (var : Var.t) : Value.t option =
  Local.VEnv.find_opt var ctx.local.venv

let find_value (ctx : t) (var : Var.t) : Value.t =
  match find_value_opt ctx var with
  | Some value -> value
  | None ->
      let id, _ = var in
      error_undef id.at "value" (Var.to_string var)

let bound_value (ctx : t) (var : Var.t) : bool =
  find_value_opt ctx var |> Option.is_some

(* Finders for type definitions *)

let find_typdef_opt (ctx : t) (tid : TId.t) : Typdef.t option =
  match Local.TDEnv.find_opt tid ctx.local.tdenv with
  | Some td -> Some td
  | None -> Global.TDEnv.find_opt tid ctx.global.tdenv

let find_typdef (ctx : t) (tid : TId.t) : Typdef.t =
  match find_typdef_opt ctx tid with
  | Some td -> td
  | None -> error_undef tid.at "type" tid.it

let bound_typdef (ctx : t) (tid : TId.t) : bool =
  find_typdef_opt ctx tid |> Option.is_some

(* Finders for rules *)

let find_rel_opt (ctx : t) (rid : RId.t) : Rel.t option =
  Global.REnv.find_opt rid ctx.global.renv

let find_rel (ctx : t) (rid : RId.t) : Rel.t =
  match find_rel_opt ctx rid with
  | Some rel -> rel
  | None -> error_undef rid.at "relation" rid.it

let bound_rel (ctx : t) (rid : RId.t) : bool =
  find_rel_opt ctx rid |> Option.is_some

(* Finders for definitions *)

let find_func_opt (ctx : t) (fid : FId.t) : (cursor * Func.t) option =
  match Local.FEnv.find_opt fid ctx.local.fenv with
  | Some func -> Some (Local, func)
  | None -> (
      match Global.FEnv.find_opt fid ctx.global.fenv with
      | Some func -> Some (Global, func)
      | None -> None)

let find_func (ctx : t) (fid : FId.t) : cursor * Func.t =
  match find_func_opt ctx fid with
  | Some func -> func
  | None -> error_undef fid.at "function" fid.it

let bound_func (ctx : t) (fid : FId.t) : bool =
  find_func_opt ctx fid |> Option.is_some

(* Adders *)

(* Adders for values *)

let add_value ?(shadow = false) (ctx : t) (var : Var.t) (value : Value.t) : t =
  (if (not shadow) && Local.VEnv.mem var ctx.local.venv then
     let id, _ = var in
     error_dup id.at "value" (Var.to_string var));
  let venv = Local.VEnv.add var value ctx.local.venv in
  if venv == ctx.local.venv then ctx
  else { ctx with local = { ctx.local with venv } }

(* Batch add multiple values efficiently *)
let add_values ?(shadow = false) (ctx : t) (bindings : (Var.t * Value.t) list) :
    t =
  (* Check for duplicates if not shadowing *)
  if not shadow then
    List.iter
      (fun (var, _) ->
        if bound_value ctx var then
          let id, _ = var in
          error_dup id.at "value" (Var.to_string var))
      bindings;
  (* Build venv in one pass *)
  let venv =
    List.fold_left
      (fun venv (var, value) -> Local.VEnv.add var value venv)
      ctx.local.venv bindings
  in
  (* Optimize: avoid creating new context if venv unchanged *)
  if venv == ctx.local.venv then ctx
  else { ctx with local = { ctx.local with venv } }

(* Adders for type definitions *)

let add_typdef (ctx : t) (tid : TId.t) (td : Typdef.t) : t =
  if bound_typdef ctx tid then error_dup tid.at "type" tid.it;
  let tdenv = Local.TDEnv.add tid td ctx.local.tdenv in
  { ctx with local = { ctx.local with tdenv } }

(* Batch add multiple type definitions efficiently *)
let add_typdefs (ctx : t) (bindings : (TId.t * Typdef.t) list) : t =
  (* Check for duplicates *)
  List.iter
    (fun (tid, _) ->
      if bound_typdef ctx tid then error_dup tid.at "type" tid.it)
    bindings;
  let tdenv =
    List.fold_left
      (fun tdenv (tid, td) -> Local.TDEnv.add tid td tdenv)
      ctx.local.tdenv bindings
  in
  { ctx with local = { ctx.local with tdenv } }

(* Adders for functions *)

let add_func (ctx : t) (fid : FId.t) (func : Func.t) : t =
  if bound_func ctx fid then error_dup fid.at "function" fid.it;
  let fenv = Local.FEnv.add fid func ctx.local.fenv in
  { ctx with local = { ctx.local with fenv } }

(* Constructors *)

(* Constructing an empty context *)

(* Cache empty environments to avoid recreating them *)
let empty_local () : local =
  {
    tdenv = Local.TDEnv.empty;
    fenv = Local.FEnv.empty;
    venv = Local.VEnv.empty;
  }

(* Constructing a loader *)
let create_loader () : global_loader =
  {
    tdenv = Global.TDEnv.create ();
    renv = Global.REnv.create ();
    fenv = Global.FEnv.create ();
  }

(* Loader operations *)

let load_typdef (l : global_loader) (tid : TId.t) (td : Typdef.t) : unit =
  Global.TDEnv.add l.tdenv tid td

let load_rel (l : global_loader) (rid : RId.t) (rel : Rel.t) : unit =
  Global.REnv.add l.renv rid rel

let load_func (l : global_loader) (fid : FId.t) (func : Func.t) : unit =
  Global.FEnv.add l.fenv fid func

(* Freezing a loader into global context *)
let freeze (l : global_loader) : global =
  {
    tdenv = Global.TDEnv.freeze l.tdenv;
    renv = Global.REnv.freeze l.renv;
    fenv = Global.FEnv.freeze l.fenv;
  }

let create ~filename builtins cache (global : global) : t =
  { filename; builtins; cache; global; local = empty_local () }

(* Constructing a local context *)
let localize (ctx : t) : t = { ctx with local = empty_local () }

(* Constructing sub-contexts *)

let sub_opt (ctx : t) (vars : var list) : t option attempt =
  (* First collect the values that are to be iterated over *)
  let values =
    List.map
      (fun { varid; iters; _ } ->
        find_value ctx (varid, iters @ [ Opt ]) |> Value.get_opt)
      vars
  in
  (* Iteration is valid when all variables agree on their optionality *)
  if List.for_all Option.is_some values then
    let values = List.map Option.get values in
    (* Build venv in one pass to avoid intermediate context creations *)
    let venv_sub =
      List.fold_left2
        (fun venv { varid; iters; _ } value ->
          Local.VEnv.add (varid, iters) value venv)
        ctx.local.venv vars values
    in
    let ctx_sub = { ctx with local = { ctx.local with venv = venv_sub } } in
    Ok (Some ctx_sub)
  else if List.for_all Option.is_none values then Ok None
  else fail no_region "mismatch in optionality of iterated variables"

(* Transpose a matrix of values, as a list of value batches
   that are to be each fed into an iterated expression *)

let transpose (value_matrix : value list list) : value list list attempt =
  match value_matrix with
  | [] -> Ok []
  | row :: rows ->
      let width = List.length (List.hd value_matrix) in
      let* () =
        guard
          (List.for_all
             (fun value_row -> List.length value_row = width)
             value_matrix)
          no_region "cannot transpose a matrix of value batches"
      in
      let columns_init = List.map (fun elem -> [ elem ]) row in
      let columns_rev =
        List.fold_left
          (fun columns_rev row ->
            List.map2
              (fun column_rev elem -> elem :: column_rev)
              columns_rev row)
          columns_init rows
      in
      Ok (List.map List.rev columns_rev)

let sub_list (ctx : t) (vars : var list) : t list attempt =
  (* First break the values that are to be iterated over,
     into a batch of values *)
  let* values_batch =
    List.map
      (fun { varid; iters; _ } ->
        find_value ctx (varid, iters @ [ List ]) |> Value.get_list)
      vars
    |> transpose
  in
  (* For each batch of values, create a sub-context *)
  (* Build venv in one pass per batch to avoid intermediate context creations *)
  let ctxs_sub =
    List.fold_left
      (fun ctxs_sub_rev value_batch ->
        let venv_sub =
          List.fold_left2
            (fun venv { varid; iters; _ } value ->
              Local.VEnv.add (varid, iters) value venv)
            ctx.local.venv vars value_batch
        in
        let ctx_sub = { ctx with local = { ctx.local with venv = venv_sub } } in
        ctx_sub :: ctxs_sub_rev)
      [] values_batch
    |> List.rev
  in
  Ok ctxs_sub
