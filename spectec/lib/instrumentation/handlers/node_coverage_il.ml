(** IL node coverage: records premises at session init, tracks execution counts,
    reports at finish. [Summary] lists only uncovered premises; [Full] emits a
    GCOV-style annotated spec with per-premise counts. *)

open Common.Source
open Lang.Il
open Util
open Instrumentation_static.Premise_uid

type level = Summary | Full
type config = { level : level; output : Instrumentation_api.Output.t }

let default_config =
  { level = Summary; output = Instrumentation_api.Output.stdout }

let config = ref default_config
let fmt = ref Format.std_formatter

(* Runtime state - changes during execution *)
module State = struct
  let il_spec : spec ref = ref []
  let prems_attempted : (region * string, int) Hashtbl.t = Hashtbl.create 256
  let prems_succeeded : (region * string, int) Hashtbl.t = Hashtbl.create 256
  let prems_failed : (region * string, int) Hashtbl.t = Hashtbl.create 256

  let prem_to_test : (region * string, string list) Hashtbl.t =
    Hashtbl.create 256

  let current_test_case_id : string option ref = ref None
  let total_prems = ref 0
  let total_fallible_prems = ref 0
  let total_rel_prems = ref 0
  let total_if_prems = ref 0

  let reset () =
    il_spec := [];
    Hashtbl.clear prems_attempted;
    Hashtbl.clear prems_succeeded;
    Hashtbl.clear prems_failed;
    Hashtbl.clear prem_to_test;
    current_test_case_id := None;
    total_prems := 0;
    total_fallible_prems := 0;
    total_rel_prems := 0;
    total_if_prems := 0

  (* Set current test case ID (called by runner before each test) *)
  let set_test_case_id id = current_test_case_id := Some id
  let clear_test_case_id () = current_test_case_id := None

  (* Record that a premise was covered by the current test case *)
  let record_premise_coverage key =
    match !current_test_case_id with
    | Some test_id ->
        let existing =
          Hashtbl.find_opt prem_to_test key |> Option.value ~default:[]
        in
        if not (List.mem test_id existing) then
          Hashtbl.replace prem_to_test key (test_id :: existing)
    | None -> ()

  let incr_count tbl key =
    let count = Hashtbl.find_opt tbl key |> Option.value ~default:0 in
    Hashtbl.replace tbl key (count + 1)
end

let rec is_fallible prem =
  match prem.it with
  | LetPr _ | ElsePr | DebugPr _ | RelAssertPr _ -> false
  | IterPr (inner, _) -> is_fallible inner
  | IfPr _ | RelPr _ -> true

module M : Instrumentation_api.Handler.S = struct
  let static_dependencies =
    [
      (module Instrumentation_static.Premise_uid.Premise_uid
      : Instrumentation_static.Static.S);
    ]

  let rec count_prem prem =
    match prem.it with
    (* count IfPr, RelPr, and their iterations *)
    | LetPr _ | ElsePr | DebugPr _ | RelAssertPr _ ->
        State.total_prems := !State.total_prems + 1
    | IterPr (inner, _) -> count_prem inner
    | IfPr _ ->
        State.total_prems := !State.total_prems + 1;
        State.total_fallible_prems := !State.total_fallible_prems + 1;
        State.total_if_prems := !State.total_if_prems + 1
    | RelPr _ ->
        State.total_prems := !State.total_prems + 1;
        State.total_fallible_prems := !State.total_fallible_prems + 1;
        State.total_rel_prems := !State.total_rel_prems + 1

  let init ~spec =
    State.reset ();
    match spec with
    | Instrumentation_api.Handler.IlSpec il_spec ->
        State.il_spec := il_spec;
        List.iter
          (fun def ->
            match def.it with
            | RelD { rules; _ } ->
                List.iter
                  (fun rule ->
                    let ({ prems; _ } : rule') = rule.it in
                    List.iter (fun prem -> count_prem prem) prems)
                  rules
            | DecD { clauses; _ } ->
                List.iter
                  (fun clause ->
                    let { prems; _ } = clause.it in
                    List.iter (fun prem -> count_prem prem) prems)
                  clauses
            | _ -> ())
          il_spec
    | Instrumentation_api.Handler.SlSpec _ -> ()

  let handle : Instrumentation_api.Event.t -> unit = function
    | Test_start { test_case_id } -> State.set_test_case_id test_case_id
    | Test_end _ -> State.clear_test_case_id ()
    | Prem_enter { prem; at = _ } ->
        let key = prem_key prem in
        State.incr_count State.prems_attempted key;
        State.record_premise_coverage key
    | Prem_exit { prem; at = _; success; bindings = _ } ->
        let key = prem_key prem in
        if success then (
          State.incr_count State.prems_succeeded key;
          State.record_premise_coverage key)
        else
          let rec incr_failures prem =
            match prem.it with
            | LetPr _ | ElsePr | DebugPr _ | RelAssertPr _ -> ()
            | IterPr (inner, _) -> incr_failures inner
            | IfPr _ | RelPr _ ->
                let key = prem_key prem in
                State.incr_count State.prems_failed key
          in
          incr_failures prem
    | _ -> ()

  (* --- Output: Summary mode (stats + uncovered only) --- *)

  let is_if_prem_key ((_, content) : region * string) : bool =
    String.length content >= 3 && String.sub content 0 3 = "if "

  let print_stats () =
    let succeeded = Hashtbl.length State.prems_succeeded in
    let attempted = Hashtbl.length State.prems_attempted in
    let total = !State.total_prems in

    if total > 0 then (
      Format.fprintf !fmt
        "IL Premises: %d/%d attempted (%.2f%%), %d/%d succeeded (%.2f%%)\n"
        attempted total
        (percentage attempted total)
        succeeded total
        (percentage succeeded total);

      (* Breakdown for if-premises *)
      let total_if = !State.total_if_prems in
      let succeeded_if =
        Hashtbl.fold
          (fun k _ acc -> if is_if_prem_key k then acc + 1 else acc)
          State.prems_succeeded 0
      in
      let failed_if =
        Hashtbl.fold
          (fun k _ acc -> if is_if_prem_key k then acc + 1 else acc)
          State.prems_failed 0
      in
      let both_if =
        Hashtbl.fold
          (fun k _ acc ->
            if is_if_prem_key k && Hashtbl.mem State.prems_succeeded k then
              acc + 1
            else acc)
          State.prems_failed 0
      in
      let neither_if = total_if - (succeeded_if + failed_if - both_if) in
      let total_score = succeeded_if + failed_if in
      let twice_total_if = 2 * total_if in

      Format.fprintf !fmt "%d rel premises\n" !State.total_rel_prems;
      Format.fprintf !fmt
        "%d if-premises: succeeded %d/%d (%.2f%%), failed %d/%d (%.2f%%), \
         neither %d/%d (%.2f%%), total %d/%d (%.2f%%)\n"
        total_if succeeded_if total_if
        (percentage succeeded_if total_if)
        failed_if total_if
        (percentage failed_if total_if)
        neither_if total_if
        (percentage neither_if total_if)
        total_score twice_total_if
        (percentage total_score twice_total_if))

  let print_uncovered () =
    let total = !State.total_prems in
    if total > 0 then (
      (* Collect uncovered - premises never successfully executed *)
      let uncovered = ref [] in
      List.iter
        (fun def ->
          match def.it with
          | RelD { relid = id; rules; _ } ->
              List.iter
                (fun rule ->
                  let ({ ruleid = rule_id; prems; _ } : rule') = rule.it in
                  List.iter
                    (fun prem ->
                      if not (Hashtbl.mem State.prems_succeeded (prem_key prem))
                      then
                        uncovered :=
                          (id.it, rule_id.it, Print.string_of_prem prem)
                          :: !uncovered)
                    prems)
                rules
          | DecD { defid = id; clauses; _ } ->
              List.iteri
                (fun idx clause ->
                  let { prems; _ } = clause.it in
                  List.iter
                    (fun prem ->
                      if not (Hashtbl.mem State.prems_succeeded (prem_key prem))
                      then
                        uncovered :=
                          ( id.it,
                            Format.sprintf "clause/%d" idx,
                            Print.string_of_prem prem )
                          :: !uncovered)
                    prems)
                clauses
          | _ -> ())
        !State.il_spec;
      if !uncovered <> [] then (
        Format.fprintf !fmt "\nNever succeeded:\n";
        List.iter
          (fun (rel, rule, content) ->
            Format.fprintf !fmt "  %s/%s:\n    %s\n" rel rule
              (normalize_whitespace content))
          (List.rev !uncovered)))

  (* --- Output: Full mode (GCOV-style annotated spec) --- *)

  (* Format as succ/fail - omit fail for let premises *)
  let fmt_succ_fail prem =
    let key = prem_key prem in
    let succ =
      Hashtbl.find_opt State.prems_succeeded key |> Option.value ~default:0
    in
    let succ_str = format_count succ in
    match prem.it with
    | LetPr _ -> Format.sprintf "%s     " succ_str
    | _ ->
        let fail =
          Hashtbl.find_opt State.prems_failed key |> Option.value ~default:0
        in
        let fail_str = format_count fail in
        Format.sprintf "%s/%s" succ_str fail_str

  let get_prem_succeeded key =
    Hashtbl.find_opt State.prems_succeeded key |> Option.value ~default:0

  let print_prem indent prem =
    let content = Print.string_of_prem prem |> normalize_whitespace in
    let uid =
      match get_uid (prem_key prem) with Some uid -> uid | None -> -1
    in
    if is_fallible prem then
      let succ_fail = fmt_succ_fail prem in
      Format.fprintf !fmt "%4d: %s %s-- %s\n" uid succ_fail indent content
    else
      let succ = get_prem_succeeded (prem_key prem) in
      Format.fprintf !fmt "%4d: %s     %s-- %s\n" uid (format_count succ) indent
        content

  let print_prems indent result_str prems =
    List.iter (print_prem indent) prems;
    (* Print success count for final premise *)
    match List.rev prems with
    | last :: _ ->
        let key = prem_key last in
        let succ = get_prem_succeeded key in
        Format.fprintf !fmt "      %s      %s%s\n" (format_count succ) indent
          result_str
    | [] -> ()

  let print_full () =
    List.iter
      (fun def ->
        match def.it with
        | RelD { relid = id; rules; _ } ->
            Format.fprintf !fmt "\nrelation %s:\n" id.it;
            List.iter
              (fun rule ->
                let ({ ruleid = rule_id; concl; prems; _ } : rule') = rule.it in
                let result_str =
                  Print.string_of_notexp concl |> normalize_whitespace
                in
                Format.fprintf !fmt "      rule %s:\n" rule_id.it;
                print_prems "    " result_str prems)
              rules
        | DecD { defid = id; clauses; _ } ->
            Format.fprintf !fmt "\ndef $%s:\n" id.it;
            List.iteri
              (fun idx clause ->
                let { body; prems; _ } = clause.it in
                let result_str =
                  Print.string_of_exp body |> normalize_whitespace
                in
                Format.fprintf !fmt "      clause %d:\n" idx;
                print_prems "    " result_str prems)
              clauses
        | _ -> ())
      !State.il_spec

  (* --- Finish: print report --- *)

  let finish () =
    if !State.total_prems > 0 then (
      Format.fprintf !fmt "\n=== IL Node Coverage ===\n\n";
      match !config.level with
      | Summary ->
          print_stats ();
          print_uncovered ()
      | Full ->
          print_stats ();
          print_full ())
end

(* Result type for programmatic access *)
type result = {
  prems_attempted : ((region * string) * int) list; (* key * count *)
  prems_succeeded : ((region * string) * int) list; (* key * count *)
  prem_to_uid : ((region * string) * int) list; (* key * uid *)
  uid_to_prem : (int * (region * string)) list; (* uid * key *)
  prem_to_test : ((region * string) * string list) list;
      (* key * test_case_ids *)
  total_prems : int;
}

let get_result () =
  let prem_to_uid_list, uid_to_prem_list =
    match Instrumentation_static.Premise_uid.export () with
    | Some (prem_to_uid, uid_to_prem) -> (prem_to_uid, uid_to_prem)
    | None -> ([], [])
  in
  {
    prems_attempted = State.prems_attempted |> Hashtbl.to_seq |> List.of_seq;
    prems_succeeded = State.prems_succeeded |> Hashtbl.to_seq |> List.of_seq;
    prem_to_uid = prem_to_uid_list;
    uid_to_prem = uid_to_prem_list;
    prem_to_test = State.prem_to_test |> Hashtbl.to_seq |> List.of_seq;
    total_prems = !State.total_prems;
  }

(* Restore state from a previous result (for checkpoint resume) *)
let restore result =
  Hashtbl.clear State.prems_attempted;
  Hashtbl.clear State.prems_succeeded;
  Hashtbl.clear State.prems_failed;
  Hashtbl.clear State.prem_to_test;
  List.iter
    (fun (key, count) -> Hashtbl.replace State.prems_attempted key count)
    result.prems_attempted;
  List.iter
    (fun (key, count) -> Hashtbl.replace State.prems_succeeded key count)
    result.prems_succeeded;
  (* Restore UID mapping through static service *)
  Instrumentation_static.Premise_uid.restore
    (result.prem_to_uid, result.uid_to_prem);
  (* Reconstruct prems_failed from attempted - succeeded *)
  List.iter
    (fun (key, attempted_count) ->
      let succeeded_count =
        Hashtbl.find_opt State.prems_succeeded key |> Option.value ~default:0
      in
      let failed_count = attempted_count - succeeded_count in
      if failed_count > 0 then
        Hashtbl.replace State.prems_failed key failed_count)
    result.prems_attempted;
  List.iter
    (fun (key, test_cases) -> Hashtbl.replace State.prem_to_test key test_cases)
    result.prem_to_test;
  State.total_prems := result.total_prems

(* Merge two results — used for checkpoint merging *)
let merge_results r1 r2 =
  let merge_counts counts1 counts2 =
    let tbl = Hashtbl.create 256 in
    let add key count =
      let existing = Hashtbl.find_opt tbl key |> Option.value ~default:0 in
      Hashtbl.replace tbl key (existing + count)
    in
    List.iter (fun (key, count) -> add key count) counts1;
    List.iter (fun (key, count) -> add key count) counts2;
    Hashtbl.to_seq tbl |> List.of_seq
  in
  let merge_test_lists tests1 tests2 =
    let tbl = Hashtbl.create 256 in
    let union_test_ids existing new_ids =
      List.fold_left
        (fun existing test_id ->
          if List.mem test_id existing then existing else test_id :: existing)
        existing new_ids
    in
    let add key test_ids =
      let existing = Hashtbl.find_opt tbl key |> Option.value ~default:[] in
      Hashtbl.replace tbl key (union_test_ids existing test_ids)
    in
    List.iter (fun (key, test_ids) -> add key test_ids) tests1;
    List.iter (fun (key, test_ids) -> add key test_ids) tests2;
    Hashtbl.to_seq tbl |> List.of_seq
  in
  {
    prem_to_uid = r1.prem_to_uid;
    uid_to_prem = r1.uid_to_prem;
    total_prems = r1.total_prems;
    prems_attempted = merge_counts r1.prems_attempted r2.prems_attempted;
    prems_succeeded = merge_counts r1.prems_succeeded r2.prems_succeeded;
    prem_to_test = merge_test_lists r1.prem_to_test r2.prem_to_test;
  }

(* Expose test case ID setter for use by runner *)
let set_test_case_id = State.set_test_case_id
let clear_test_case_id = State.clear_test_case_id

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
     let handler, get_coverage = Node_coverage.make_with_data cfg in
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
  let name = "premise-coverage"
  let modes = [ `IL ]

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
