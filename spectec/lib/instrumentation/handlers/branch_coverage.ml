(** Branch coverage: records all rules and clauses at session init, then marks
    which ones execute. [Summary] reports only uncovered branches; [Full] emits
    every relation and function with coverage annotations. *)

open Common.Source
module Il = Lang.Il
module Sl = Lang.Sl
open Util

type level = Summary | Full
type config = { level : level; output : Instrumentation_api.Output.t }

let default_config =
  { level = Summary; output = Instrumentation_api.Output.stdout }

let config = ref default_config
let fmt = ref Format.std_formatter

(* Runtime state - changes during execution *)
module State = struct
  let all_rules : (string * string) list ref = ref []
  let all_clauses : (string * int) list ref = ref []
  let rules_hit : (string * string, int) Hashtbl.t = Hashtbl.create 64
  let clauses_hit : (string * int, int) Hashtbl.t = Hashtbl.create 64

  let reset () =
    all_rules := [];
    all_clauses := [];
    Hashtbl.clear rules_hit;
    Hashtbl.clear clauses_hit

  let incr tbl key =
    let count = Hashtbl.find_opt tbl key |> Option.value ~default:0 in
    Hashtbl.replace tbl key (count + 1)
end

(* Group items by their first component *)
let group_by items =
  List.fold_left
    (fun acc (k, v) ->
      let vs = List.assoc_opt k acc |> Option.value ~default:[] in
      (k, v :: vs) :: List.remove_assoc k acc)
    [] items
  |> List.sort compare

module M : Instrumentation_api.Handler.S = struct
  let static_dependencies = []

  let init ~spec =
    State.reset ();
    match spec with
    | Instrumentation_api.Handler.IlSpec il_spec ->
        List.iter
          (fun def ->
            match def.it with
            | Il.RelD { relid = id; rules; _ } ->
                List.iter
                  (fun rule ->
                    let { Il.ruleid = rule_id; _ } = rule.it in
                    State.all_rules := (id.it, rule_id.it) :: !State.all_rules)
                  rules
            | Il.DecD { defid = id; clauses; _ } ->
                List.iteri
                  (fun idx _ ->
                    State.all_clauses := (id.it, idx) :: !State.all_clauses)
                  clauses
            | _ -> ())
          il_spec
    | Instrumentation_api.Handler.SlSpec sl_spec ->
        List.iter
          (fun def ->
            match def.it with
            | Sl.RelD (id, _, _, _) ->
                State.all_rules := (id.it, "0") :: !State.all_rules
            | Sl.DecD (id, _, _, _, _) ->
                State.all_clauses := (id.it, 0) :: !State.all_clauses
            | _ -> ())
          sl_spec

  let handle : Instrumentation_api.Event.t -> unit = function
    | Rule_exit { id; rule_id; at = _; success } ->
        if success then State.incr State.rules_hit (id, rule_id)
    | Clause_exit { id; clause_idx; at = _; success } ->
        if success then State.incr State.clauses_hit (id, clause_idx)
    | _ -> ()

  (* --- Output: Summary mode (stats + uncovered only) --- *)

  let print_summary () =
    let rules_by_rel = group_by !State.all_rules in
    let clauses_by_func = group_by !State.all_clauses in

    (* Rules summary *)
    let total_rules = List.length !State.all_rules in
    if total_rules > 0 then (
      let hit =
        List.filter
          (fun key -> Hashtbl.mem State.rules_hit key)
          !State.all_rules
        |> List.length
      in
      Format.fprintf !fmt "Rules: %d/%d (%.2f%%)\n" hit total_rules
        (percentage hit total_rules);
      (* Uncovered rules *)
      let uncovered =
        List.filter_map
          (fun (rel, rules) ->
            let uncov =
              List.filter
                (fun r -> not (Hashtbl.mem State.rules_hit (rel, r)))
                rules
            in
            if uncov <> [] then Some (rel, uncov) else None)
          rules_by_rel
      in
      if uncovered <> [] then (
        Format.fprintf !fmt "\nUncovered rules:\n";
        List.iter
          (fun (rel, rules) ->
            List.iter (fun r -> Format.fprintf !fmt "  %s/%s\n" rel r) rules)
          uncovered));

    (* Clauses summary *)
    let total_clauses = List.length !State.all_clauses in
    if total_clauses > 0 then (
      let hit =
        List.filter
          (fun key -> Hashtbl.mem State.clauses_hit key)
          !State.all_clauses
        |> List.length
      in
      Format.fprintf !fmt "\nClauses: %d/%d (%.2f%%)\n" hit total_clauses
        (percentage hit total_clauses);
      (* Uncovered clauses *)
      let uncovered =
        List.filter_map
          (fun (func, idxs) ->
            let uncov =
              List.filter
                (fun i -> not (Hashtbl.mem State.clauses_hit (func, i)))
                idxs
            in
            if uncov <> [] then Some (func, uncov) else None)
          clauses_by_func
      in
      if uncovered <> [] then (
        Format.fprintf !fmt "\nUncovered clauses:\n";
        List.iter
          (fun (func, idxs) ->
            List.iter (fun i -> Format.fprintf !fmt "  $%s/%d\n" func i) idxs)
          uncovered))

  (* --- Output: Full mode (all branches with execution counts) --- *)

  let print_full () =
    let rules_by_rel = group_by !State.all_rules in
    let clauses_by_func = group_by !State.all_clauses in

    (* Relations *)
    if rules_by_rel <> [] then (
      Format.fprintf !fmt "-- Relations --\n\n";
      List.iter
        (fun (rel, rules) ->
          let rules = List.sort compare rules in
          let hit =
            List.filter (fun r -> Hashtbl.mem State.rules_hit (rel, r)) rules
            |> List.length
          in
          let total = List.length rules in
          Format.fprintf !fmt "relation %s: (%d/%d = %.2f%%)\n" rel hit total
            (percentage hit total);
          List.iter
            (fun r ->
              let count =
                Hashtbl.find_opt State.rules_hit (rel, r)
                |> Option.value ~default:0
              in
              Format.fprintf !fmt "  %s  rule %s\n" (format_count count) r)
            rules;
          Format.fprintf !fmt "\n")
        rules_by_rel);

    (* Functions *)
    if clauses_by_func <> [] then (
      Format.fprintf !fmt "-- Functions --\n\n";
      List.iter
        (fun (func, idxs) ->
          let idxs = List.sort compare idxs in
          let hit =
            List.filter (fun i -> Hashtbl.mem State.clauses_hit (func, i)) idxs
            |> List.length
          in
          let total = List.length idxs in
          Format.fprintf !fmt "def $%s: (%d/%d = %.2f%%)\n" func hit total
            (percentage hit total);
          List.iter
            (fun i ->
              let count =
                Hashtbl.find_opt State.clauses_hit (func, i)
                |> Option.value ~default:0
              in
              Format.fprintf !fmt "  %s  clause %d\n" (format_count count) i)
            idxs;
          Format.fprintf !fmt "\n")
        clauses_by_func)

  let finish () =
    Format.fprintf !fmt "\n=== Branch Coverage ===\n\n";
    match !config.level with
    | Summary -> print_summary ()
    | Full -> print_full ()
end

(* Result type for programmatic access and checkpoint restoration *)
type result = {
  all_rules : (string * string) list;
  all_clauses : (string * int) list;
  rules_hit : ((string * string) * int) list; (* key * count *)
  clauses_hit : ((string * int) * int) list; (* key * count *)
}

let get_result () =
  {
    all_rules = !State.all_rules;
    all_clauses = !State.all_clauses;
    rules_hit = State.rules_hit |> Hashtbl.to_seq |> List.of_seq;
    clauses_hit = State.clauses_hit |> Hashtbl.to_seq |> List.of_seq;
  }

(* Restore state from a previous result (for checkpoint resume) *)
let restore result =
  State.all_rules := result.all_rules;
  State.all_clauses := result.all_clauses;
  Hashtbl.clear State.rules_hit;
  Hashtbl.clear State.clauses_hit;
  List.iter (fun (k, v) -> Hashtbl.replace State.rules_hit k v) result.rules_hit;
  List.iter
    (fun (k, v) -> Hashtbl.replace State.clauses_hit k v)
    result.clauses_hit

(* Merge two results — used for checkpoint merging *)
let merge_results r1 r2 =
  let merge_counts counts1 counts2 =
    let tbl = Hashtbl.create 256 in
    let add k v =
      let existing = Hashtbl.find_opt tbl k |> Option.value ~default:0 in
      Hashtbl.replace tbl k (existing + v)
    in
    List.iter (fun (k, v) -> add k v) counts1;
    List.iter (fun (k, v) -> add k v) counts2;
    Hashtbl.to_seq tbl |> List.of_seq
  in
  {
    all_rules = r1.all_rules;
    all_clauses = r1.all_clauses;
    rules_hit = merge_counts r1.rules_hit r2.rules_hit;
    clauses_hit = merge_counts r1.clauses_hit r2.clauses_hit;
  }

(* Handler with data access - implements HANDLER_WITH_DATA signature *)
module HandlerWithData :
  Instrumentation_api.Handler.S_with_data with type result = result = struct
  include M

  type nonrec result = result

  let get_result = get_result
  let restore = restore
end

let make cfg =
  config := cfg;
  fmt := Instrumentation_api.Output.formatter cfg.output;
  (module M : Instrumentation_api.Handler.S)

(* Create handler with data getter for programmatic access.
   Usage:
     let handler, get_coverage = Branch_coverage.make_with_data cfg in
     Hooks.set_handlers [handler];
     (* ... run interpreter ... *)
     let data = get_coverage () in
*)
let make_with_data cfg =
  config := cfg;
  fmt := Instrumentation_api.Output.formatter cfg.output;
  ( (module HandlerWithData : Instrumentation_api.Handler.S_with_data
      with type result = result),
    get_result )

module Spec : Instrumentation_spec.Spec.S = struct
  let name = "branch-coverage"
  let modes = [ `IL; `SL ]

  let params =
    [
      Instrumentation_spec.Param_utils.level_param;
      Instrumentation_spec.Param_utils.output_param;
    ]

  let parse alist =
    match Instrumentation_spec.Param_utils.get alist "level" with
    | None -> None
    | Some s ->
        let output =
          Instrumentation_spec.Param_utils.output_of
            (Instrumentation_spec.Param_utils.get alist "output")
        in
        let cfg =
          {
            level =
              Instrumentation_spec.Param_utils.parse_level ~summary:Summary
                ~full:Full s;
            output;
          }
        in
        Some
          {
            Instrumentation_config.Handler_config.name;
            modes;
            handler = make cfg;
            output;
          }

  let checkpoint =
    Some
      Instrumentation_spec.Spec.
        {
          snapshot = (fun () -> Marshal.to_bytes (get_result ()) []);
          restore = (fun b -> restore (Marshal.from_bytes b 0));
          merge =
            (fun b1 b2 ->
              Marshal.to_bytes
                (merge_results (Marshal.from_bytes b1 0)
                   (Marshal.from_bytes b2 0))
                []);
        }
end

let spec : Instrumentation_spec.Spec.t = (module Spec)
