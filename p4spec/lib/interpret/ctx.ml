open Domain.Lib
open Runtime_dynamic
open Envs
open Il.Ast
open Error
open Util.Source

(* Error *)

let error_undef (at : region) (kind : string) (id : string) =
  error at (Format.asprintf "%s `%s` is undefined" kind id)

let error_dup (at : region) (kind : string) (id : string) =
  error at (Format.asprintf "%s `%s` was already defined" kind id)

(* Context *)

type t = {
  (* Debug flag *)
  debug : bool;
  (* Profiling flag *)
  profile : bool;
  (* Execution trace *)
  trace : Trace.t;
  (* Map from variables to values *)
  venv : VEnv.t;
  (* Map from syntax ids to type definitions *)
  tdenv : TDEnv.t;
  (* Map from relation ids to relations *)
  renv : REnv.t;
  (* Map from function ids to functions *)
  fenv : FEnv.t;
}

(* Profiling *)

let profile (ctx : t) : unit = if ctx.profile then Trace.profile ctx.trace

(* Tracing *)

let trace_open_rel (ctx : t) (id_rel : id) (id_rule : id)
    (values_input : value list) : t =
  let trace = Trace.open_rel id_rel id_rule values_input in
  if ctx.debug then
    Format.asprintf
      "Opening rule %s/%s\n--- with input ---\n%s\n----------------\n" id_rel.it
      id_rule.it
      (values_input |> List.map Value.to_string |> String.concat "\n")
    |> print_endline;
  { ctx with trace }

let trace_open_dec (ctx : t) (id_func : id) (idx_clause : int)
    (values_input : value list) : t =
  let trace = Trace.open_dec id_func idx_clause values_input in
  if ctx.debug then
    Format.asprintf
      "Opening clause %s/%d\n--- with input ---\n%s\n----------------\n"
      id_func.it idx_clause
      (values_input |> List.map Value.to_string |> String.concat "\n")
    |> print_endline;
  { ctx with trace }

let trace_open_iter (ctx : t) (inner : string) : t =
  let trace = Trace.open_iter inner in
  { ctx with trace }

let trace_close (ctx : t) : t =
  let trace = Trace.close ctx.trace in
  (if ctx.debug then
     match trace with
     | Rel { id_rel; id_rule; _ } ->
         Format.asprintf "Closing rule %s/%s\n" id_rel.it id_rule.it
         |> print_endline
     | Dec { id_func; idx_clause; _ } ->
         Format.asprintf "Closing clause %s/%d\n" id_func.it idx_clause
         |> print_endline
     | Iter _ -> Format.asprintf "Closing iteration\n" |> print_endline
     | _ -> ());
  { ctx with trace }

let trace_extend (ctx : t) (prem : prem) : t =
  let trace = Trace.extend ctx.trace prem in
  if ctx.debug then
    Format.asprintf "Premise: %s\n" (prem |> Il.Print.string_of_prem)
    |> print_endline;
  { ctx with trace }

let trace_commit (ctx : t) (trace : Trace.t) : t =
  let trace = Trace.commit ctx.trace trace in
  { ctx with trace }

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
     error_dup id.at "value" (Var.to_string var));
  let venv = VEnv.add var value ctx.venv in
  { ctx with venv }

(* Adders for type definitions *)

let add_typdef (ctx : t) (tid : TId.t) (td : Typdef.t) : t =
  (* if bound_typdef ctx tid then error_dup tid.at "type" tid.it; *)
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

(* Constructors *)

(* Constructing an empty context *)

let empty (debug : bool) (profile : bool) : t =
  {
    debug;
    profile;
    trace = Trace.Empty;
    venv = VEnv.empty;
    tdenv = TDEnv.empty;
    renv = REnv.empty;
    fenv = FEnv.empty;
  }

(* Constructing a local context *)

let localize (ctx : t) : t = { ctx with trace = Trace.Empty; venv = VEnv.empty }

(* Constructing sub-contexts *)

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
        no_region "value matrix is not rectangular";
      List.init width (fun j ->
          List.init (List.length value_matrix) (fun i ->
              List.nth (List.nth value_matrix i) j))

let sub_list (ctx : t) (vars : var list) : t list =
  (* First break the values that are to be iterated over,
     into a batch of values *)
  let values_batch =
    List.map
      (fun var ->
        let id, _typ, iters = var in
        find_value ctx (id, iters @ [ List ]) |> Value.get_list)
      vars
    |> transpose
  in
  (* For each batch of values, create a sub-context *)
  List.fold_left
    (fun ctxs_sub value_batch ->
      let ctx_sub =
        List.fold_left2
          (fun ctx_sub (id, _typ, iters) value ->
            add_value ctx_sub (id, iters) value)
          ctx vars value_batch
      in
      ctxs_sub @ [ ctx_sub ])
    [] values_batch
