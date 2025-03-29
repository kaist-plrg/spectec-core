open Domain.Lib
open Runtime_dynamic
open Envs
open Il.Ast
open Error
open Attempt
open Util.Source

(* Error *)

let error_undef (at : region) (kind : string) (id : string) =
  error at (Format.asprintf "%s `%s` is undefined" kind id)

let error_dup (at : region) (kind : string) (id : string) =
  error at (Format.asprintf "%s `%s` was already defined" kind id)

(* Cursor *)

type cursor = Global | Local

(* Context *)

(* Config *)

type config = { debug : bool; profile : bool }

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
  (* Map from syntax ids to type definitions *)
  tdenv : TDEnv.t;
  (* Map from function ids to functions *)
  fenv : FEnv.t;
  (* Map from variables to values *)
  venv : VEnv.t;
}

type t = {
  (* Config *)
  config : config;
  (* Execution trace *)
  trace : Trace.t;
  (* Global layer *)
  global : global;
  (* Local layer *)
  local : local;
}

(* Profiling *)

let profile (ctx : t) : unit =
  if ctx.config.profile then Trace.profile ctx.trace

(* Tracing *)

let trace_open_rel (ctx : t) (id_rel : id) (id_rule : id)
    (values_input : value list) : t =
  let trace = Trace.open_rel id_rel id_rule values_input in
  if ctx.config.debug then
    Format.asprintf
      "Opening rule %s/%s\n--- with input ---\n%s\n----------------\n" id_rel.it
      id_rule.it
      (values_input |> List.map Value.to_string |> String.concat "\n")
    |> print_endline;
  { ctx with trace }

let trace_open_dec (ctx : t) (id_func : id) (idx_clause : int)
    (values_input : value list) : t =
  let trace = Trace.open_dec id_func idx_clause values_input in
  if ctx.config.debug then
    Format.asprintf
      "Opening clause $%s/%d\n--- with input ---\n%s\n----------------\n"
      id_func.it idx_clause
      (values_input |> List.map Value.to_string |> String.concat "\n")
    |> print_endline;
  { ctx with trace }

let trace_open_iter (ctx : t) (inner : string) : t =
  let trace = Trace.open_iter inner in
  { ctx with trace }

let trace_close (ctx : t) : t =
  let trace = Trace.close ctx.trace in
  (if ctx.config.debug then
     match trace with
     | Rel { id_rel; id_rule; _ } ->
         Format.asprintf "Closing rule %s/%s\n" id_rel.it id_rule.it
         |> print_endline
     | Dec { id_func; idx_clause; _ } ->
         Format.asprintf "Closing clause $%s/%d\n" id_func.it idx_clause
         |> print_endline
     | Iter _ -> Format.asprintf "Closing iteration\n" |> print_endline
     | _ -> ());
  { ctx with trace }

let trace_extend (ctx : t) (prem : prem) : t =
  let trace = Trace.extend ctx.trace prem in
  if ctx.config.debug then
    Format.asprintf "Premise: %s\n" (prem |> Il.Print.string_of_prem)
    |> print_endline;
  { ctx with trace }

let trace_commit (ctx : t) (trace : Trace.t) : t =
  let trace = Trace.commit ctx.trace trace in
  { ctx with trace }

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

let add_value ?(shadow = false) (cursor : cursor) (ctx : t) (var : Var.t)
    (value : Value.t) : t =
  (if cursor = Global then
     let id, _ = var in
     error id.at "cannot add value to global context");
  (if (not shadow) && bound_value cursor ctx var then
     let id, _ = var in
     error_dup id.at "value" (Var.to_string var));
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
  { tdenv = TDEnv.empty; fenv = FEnv.empty; venv = VEnv.empty }

let empty (debug : bool) (profile : bool) : t =
  let config = { debug; profile } in
  let trace = Trace.Empty in
  let global = empty_global () in
  let local = empty_local () in
  { config; trace; global; local }

(* Constructing a local context *)

let localize (ctx : t) : t =
  let trace = Trace.Empty in
  let local = empty_local () in
  { ctx with trace; local }

(* Constructing sub-contexts *)

let sub_opt (ctx : t) (vars : var list) : t option attempt =
  (* First collect the values that are to be iterated over *)
  let values =
    List.map
      (fun var ->
        let id, _typ, iters = var in
        find_value Local ctx (id, iters @ [ Opt ]) |> Value.get_opt)
      vars
  in
  (* Iteration is valid when all variables agree on their optionality *)
  if List.for_all Option.is_some values then
    let values = List.map Option.get values in
    let ctx_sub =
      List.fold_left2
        (fun ctx_sub (id, _typ, iters) value ->
          add_value ~shadow:true Local ctx_sub (id, iters) value)
        ctx vars values
    in
    Ok (Some ctx_sub)
  else if List.for_all Option.is_none values then Ok None
  else fail no_region "mismatch in optionality of iterated variables"

(* Transpose a matrix of values, as a list of value batches
   that are to be each fed into an iterated expression *)

let transpose (value_matrix : value list list) : value list list attempt =
  match value_matrix with
  | [] -> Ok []
  | _ ->
      let width = List.length (List.hd value_matrix) in
      let* _ =
        check_fail
          (List.for_all
             (fun value_row -> List.length value_row = width)
             value_matrix)
          no_region "cannot transpose a matrix of value batches"
      in
      let value_matrix =
        List.init width (fun j ->
            List.init (List.length value_matrix) (fun i ->
                List.nth (List.nth value_matrix i) j))
      in
      Ok value_matrix

let sub_list (ctx : t) (vars : var list) : t list attempt =
  (* First break the values that are to be iterated over,
     into a batch of values *)
  let* values_batch =
    List.map
      (fun var ->
        let id, _typ, iters = var in
        find_value Local ctx (id, iters @ [ List ]) |> Value.get_list)
      vars
    |> transpose
  in
  (* For each batch of values, create a sub-context *)
  let ctxs_sub =
    List.fold_left
      (fun ctxs_sub value_batch ->
        let ctx_sub =
          List.fold_left2
            (fun ctx_sub (id, _typ, iters) value ->
              add_value ~shadow:true Local ctx_sub (id, iters) value)
            ctx vars value_batch
        in
        ctxs_sub @ [ ctx_sub ])
      [] values_batch
  in
  Ok ctxs_sub
