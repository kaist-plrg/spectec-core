open Domain.Lib
open Runtime_dynamic
open Envs
open Il.Ast
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
  (* Debug flag *)
  debug : bool;
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

(* Constructors *)

let empty (debug : bool) : t =
  {
    debug;
    trace = Trace.Empty;
    venv = VEnv.empty;
    tdenv = TDEnv.empty;
    renv = REnv.empty;
    fenv = FEnv.empty;
  }

let localize (ctx : t) : t = { ctx with venv = VEnv.empty }

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

let trace_prem (ctx : t) (prem : prem) : t =
  let trace = Trace.extend ctx.trace prem in
  if ctx.debug then
    Format.asprintf "Premise: %s\n" (prem |> Il.Print.string_of_prem)
    |> print_endline;
  { ctx with trace }

let trace_commit (ctx : t) (trace : Trace.t) : t =
  let trace = Trace.commit ctx.trace trace in
  (if ctx.debug then
     match trace with
     | Rel (id_rel, id_rule, _, _) ->
         Format.asprintf "Closing rule %s/%s\n" id_rel.it id_rule.it
         |> print_endline
     | Dec (id_func, idx_clause, _, _) ->
         Format.asprintf "Closing clause %s/%d\n" id_func.it idx_clause
         |> print_endline
     | _ -> ());
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
     error_dup id.at "value" id.it);
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
