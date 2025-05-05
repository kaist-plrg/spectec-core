open Domain.Lib
open Runtime_dynamic_sl
module Dep = Runtime_testgen.Dep
module Ignore = Runtime_testgen.Cov.Ignore
module SCov = Runtime_testgen.Cov.Single
open Envs
open Sl.Ast
open Error
open Util.Source

(* Error *)

let error_undef (at : region) (kind : string) (id : string) =
  error at (Format.asprintf "%s `%s` is undefined" kind id)

let error_dup (at : region) (kind : string) (id : string) =
  error at (Format.asprintf "%s `%s` was already defined" kind id)

(* Cursor *)

type cursor = Global | Local

(* Context *)

(* Global layer *)

type global = {
  (* Map from syntax ids to type definitions *)
  tdenv : TDEnv.t;
  (* Map from relation ids to relations *)
  renv : REnv.t;
  (* Map from function ids to functions *)
  fenv : FEnv.t;
}

(* Local layer *)

type local = {
  (* Input values *)
  values_input : value list;
  (* Map from syntax ids to type definitions *)
  tdenv : TDEnv.t;
  (* Map from function ids to functions *)
  fenv : FEnv.t;
  (* Map from variables to values *)
  venv : VEnv.t;
}

type t = {
  (* Filename of the source file *)
  filename : string;
  (* Value dependency graph *)
  derive : bool;
  graph : Dep.Graph.t;
  vid_program : vid;
  (* Branch coverage of phantoms *)
  cover : SCov.Cover.t ref;
  (* Global layer *)
  global : global;
  (* Local layer *)
  local : local;
}

(* Value dependencies *)

let add_node ?(taint = false) (ctx : t) (value : value) : unit =
  if ctx.derive then Dep.Graph.add_node ~taint ctx.graph value

let add_edge (ctx : t) (value_from : value) (value_to : value)
    (label : Dep.Edges.label) : unit =
  if ctx.derive then Dep.Graph.add_edge ctx.graph value_from value_to label

(* Cover *)

let cover (ctx : t) (hit : bool) (pid : pid) (vid : vid) : t =
  if hit then ctx.cover := SCov.hit !(ctx.cover) pid
  else ctx.cover := SCov.miss !(ctx.cover) pid vid;
  ctx

(* Finders *)

(* Finders for values *)

let find_value_opt (cursor : cursor) (ctx : t) (var : Var.t) : Value.t option =
  match cursor with Global -> None | Local -> VEnv.find_opt var ctx.local.venv

let find_value (cursor : cursor) (ctx : t) (var : Var.t) : Value.t =
  match find_value_opt cursor ctx var with
  | Some value -> value
  | None ->
      let id, _ = var in
      error_undef id.at "value" (Var.to_string var)

let bound_value (cursor : cursor) (ctx : t) (var : Var.t) : bool =
  find_value_opt cursor ctx var |> Option.is_some

(* Finders for type definitions *)

let rec find_typdef_opt (cursor : cursor) (ctx : t) (tid : TId.t) :
    Typdef.t option =
  match cursor with
  | Global -> TDEnv.find_opt tid ctx.global.tdenv
  | Local -> (
      match TDEnv.find_opt tid ctx.local.tdenv with
      | Some td -> Some td
      | None -> find_typdef_opt Global ctx tid)

let find_typdef (cursor : cursor) (ctx : t) (tid : TId.t) : Typdef.t =
  match find_typdef_opt cursor ctx tid with
  | Some td -> td
  | None -> error_undef tid.at "type" tid.it

let bound_typdef (cursor : cursor) (ctx : t) (tid : TId.t) : bool =
  find_typdef_opt cursor ctx tid |> Option.is_some

(* Finders for rules *)

let find_rel_opt (_cursor : cursor) (ctx : t) (rid : RId.t) : Rel.t option =
  REnv.find_opt rid ctx.global.renv

let find_rel (cursor : cursor) (ctx : t) (rid : RId.t) : Rel.t =
  match find_rel_opt cursor ctx rid with
  | Some rel -> rel
  | None -> error_undef rid.at "relation" rid.it

let bound_rel (cursor : cursor) (ctx : t) (rid : RId.t) : bool =
  find_rel_opt cursor ctx rid |> Option.is_some

(* Finders for definitions *)

let rec find_func_opt (cursor : cursor) (ctx : t) (fid : FId.t) : Func.t option
    =
  match cursor with
  | Global -> FEnv.find_opt fid ctx.global.fenv
  | Local -> (
      match FEnv.find_opt fid ctx.local.fenv with
      | Some func -> Some func
      | None -> find_func_opt Global ctx fid)

let find_func (cursor : cursor) (ctx : t) (fid : FId.t) : Func.t =
  match find_func_opt cursor ctx fid with
  | Some func -> func
  | None -> error_undef fid.at "function" fid.it

let bound_func (cursor : cursor) (ctx : t) (fid : FId.t) : bool =
  find_func_opt cursor ctx fid |> Option.is_some

(* Adders *)

(* Adders for values *)

let add_value (cursor : cursor) (ctx : t) (var : Var.t) (value : Value.t) : t =
  (if cursor = Global then
     let id, _ = var in
     error id.at "cannot add value to global context");
  let venv = VEnv.add var value ctx.local.venv in
  { ctx with local = { ctx.local with venv } }

(* Adders for type definitions *)

let add_typdef (cursor : cursor) (ctx : t) (tid : TId.t) (td : Typdef.t) : t =
  if bound_typdef cursor ctx tid then error_dup tid.at "type" tid.it;
  match cursor with
  | Global ->
      let tdenv = TDEnv.add tid td ctx.global.tdenv in
      { ctx with global = { ctx.global with tdenv } }
  | Local ->
      let tdenv = TDEnv.add tid td ctx.local.tdenv in
      { ctx with local = { ctx.local with tdenv } }

(* Adders for relations *)

let add_rel (cursor : cursor) (ctx : t) (rid : RId.t) (rel : Rel.t) : t =
  if cursor = Local then error rid.at "cannot add relation to local context";
  if bound_rel cursor ctx rid then error_dup rid.at "relation" rid.it;
  let renv = REnv.add rid rel ctx.global.renv in
  { ctx with global = { ctx.global with renv } }

(* Adders for functions *)

let add_func (cursor : cursor) (ctx : t) (fid : FId.t) (func : Func.t) : t =
  if bound_func cursor ctx fid then error_dup fid.at "function" fid.it;
  match cursor with
  | Global ->
      let fenv = FEnv.add fid func ctx.global.fenv in
      { ctx with global = { ctx.global with fenv } }
  | Local ->
      let fenv = FEnv.add fid func ctx.local.fenv in
      { ctx with local = { ctx.local with fenv } }

(* Constructors *)

(* Constructing an empty context *)

let empty_global () : global =
  { tdenv = TDEnv.empty; renv = REnv.empty; fenv = FEnv.empty }

let empty_local () : local =
  {
    values_input = [];
    tdenv = TDEnv.empty;
    fenv = FEnv.empty;
    venv = VEnv.empty;
  }

let empty ~(derive : bool) (filename : string) (graph : Dep.Graph.t)
    (vid_program : vid) (cover : SCov.Cover.t ref) : t =
  let global = empty_global () in
  let local = empty_local () in
  { filename; derive; graph; vid_program; cover; global; local }

(* Constructing a local context *)

let localize (ctx : t) : t =
  let local = empty_local () in
  { ctx with local }

let localize_inputs (ctx : t) (values_input : value list) : t =
  { ctx with local = { ctx.local with values_input } }

(* Constructing sub-contexts *)

let sub_opt (ctx : t) (vars : var list) : t option =
  (* First collect the values that are to be iterated over *)
  let values =
    List.map
      (fun (id, _typ, iters) ->
        find_value Local ctx (id, iters @ [ Il.Ast.Opt ]) |> Value.get_opt)
      vars
  in
  (* Iteration is valid when all variables agree on their optionality *)
  if List.for_all Option.is_some values then
    let values = List.map Option.get values in
    let ctx_sub =
      List.fold_left2
        (fun ctx_sub (id, _typ, iters) value ->
          add_value Local ctx_sub (id, iters) value)
        ctx vars values
    in
    Some ctx_sub
  else if List.for_all Option.is_none values then None
  else error no_region "mismatch in optionality of iterated variables"

(* Transpose a matrix of values, as a list of value batches
   that are to be each fed into an iterated expression *)

let transpose (value_matrix : value list list) : value list list =
  match value_matrix with
  | [] -> []
  | _ ->
      let width = List.length (List.hd value_matrix) in
      check
        (List.for_all
           (fun value_row -> List.length value_row = width)
           value_matrix)
        no_region "cannot transpose a matrix of value batches";
      List.init width (fun j ->
          List.init (List.length value_matrix) (fun i ->
              List.nth (List.nth value_matrix i) j))

let sub_list (ctx : t) (vars : var list) : t list =
  (* First break the values that are to be iterated over,
     into a batch of values *)
  let values_batch =
    List.map
      (fun (id, _typ, iters) ->
        find_value Local ctx (id, iters @ [ Il.Ast.List ]) |> Value.get_list)
      vars
    |> transpose
  in
  (* For each batch of values, create a sub-context *)
  List.fold_left
    (fun ctxs_sub value_batch ->
      let ctx_sub =
        List.fold_left2
          (fun ctx_sub (id, _typ, iters) value ->
            add_value Local ctx_sub (id, iters) value)
          ctx vars value_batch
      in
      ctxs_sub @ [ ctx_sub ])
    [] values_batch

(* Committing a sub-context *)

let commit (ctx : t) (ctx_sub : t) : t = { ctx with cover = ctx_sub.cover }
