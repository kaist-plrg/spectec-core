(** SL node coverage: same shape as {!Node_coverage_il} but over SL
    instructions. [level] and [config] are type-aliased to the IL handler's so
    the two share a parser and CLI surface. *)

open Common.Source
module Sl = Lang.Sl
open Util

type level = Node_coverage_il.level = Summary | Full

type config = Node_coverage_il.config = {
  level : level;
  output : Instrumentation_api.Output.t;
}

let default_config = Node_coverage_il.default_config
let config = ref default_config
let fmt = ref Format.std_formatter

(* Runtime state - changes during execution *)
module State = struct
  let sl_spec : Sl.spec ref = ref []
  let instrs_hit : (region * string, int) Hashtbl.t = Hashtbl.create 256
  let total_instrs = ref 0

  let reset () =
    sl_spec := [];
    Hashtbl.clear instrs_hit;
    total_instrs := 0

  let incr tbl key =
    let count = Hashtbl.find_opt tbl key |> Option.value ~default:0 in
    Hashtbl.replace tbl key (count + 1)
end

(* Get short header for instruction (without recursive children) *)
let instr_header instr =
  match instr.it with
  | Sl.RelI { call; iterexps; _ } ->
      Format.sprintf "%s: %s%s"
        (Sl.Print.string_of_relid call.relid)
        (Sl.Print.string_of_notexp call.notexp)
        (Sl.Print.string_of_iterexps iterexps)
  | Sl.RelAssertI { call; expect; iterexps; _ } ->
      Format.sprintf "If (%s: %s %s)%s"
        (Sl.Print.string_of_relid call.relid)
        (Sl.Print.string_of_notexp call.notexp)
        (if expect then "holds" else "does not hold")
        (Sl.Print.string_of_iterexps iterexps)
  | Sl.IfI (exp, iterexps, _, _) ->
      Format.sprintf "If (%s)%s"
        (Sl.Print.string_of_exp exp)
        (Sl.Print.string_of_iterexps iterexps)
  | Sl.CaseI (exp, _, _) ->
      Format.sprintf "Case on %s" (Sl.Print.string_of_exp exp)
  | Sl.OtherwiseI _ -> "Otherwise"
  | Sl.LetI (exp_l, exp_r, iterexps, _) ->
      Format.sprintf "Let %s = %s%s"
        (Sl.Print.string_of_exp exp_l)
        (Sl.Print.string_of_exp exp_r)
        (Sl.Print.string_of_iterexps iterexps)
  | Sl.ResultI [] -> "Relation holds"
  | Sl.ResultI exps ->
      Format.sprintf "Result %s" (Sl.Print.string_of_exps ", " exps)
  | Sl.ReturnI exp -> Format.sprintf "Return %s" (Sl.Print.string_of_exp exp)
  | Sl.DebugI (exp, _) ->
      Format.sprintf "Debug: %s" (Sl.Print.string_of_exp exp)

(* Create a unique key for an instruction using region + content header *)
let instr_key instr =
  let content = instr_header instr |> normalize_whitespace in
  (instr.at, content)

module M : Instrumentation_api.Handler.S = struct
  let static_dependencies = []

  let rec count_instr instr =
    State.total_instrs := !State.total_instrs + 1;
    match instr.it with
    | Sl.RelI { block; _ } -> List.iter count_instr block
    | Sl.RelAssertI { block; _ } -> List.iter count_instr block
    | Sl.IfI (_, _, instrs, _) -> List.iter count_instr instrs
    | Sl.CaseI (_, cases, _) ->
        List.iter (fun (_, instrs) -> List.iter count_instr instrs) cases
    | Sl.OtherwiseI inner -> count_instr inner
    | Sl.LetI (_, _, _, block) -> List.iter count_instr block
    | _ -> ()

  let init ~spec =
    State.reset ();
    match spec with
    | Instrumentation_api.Handler.IlSpec _ -> ()
    | Instrumentation_api.Handler.SlSpec sl_spec ->
        State.sl_spec := sl_spec;
        List.iter
          (fun def ->
            match def.it with
            | Sl.RelD (_, _, block, elseblock_opt) ->
                List.iter count_instr block;
                Option.iter (List.iter count_instr) elseblock_opt
            | Sl.DecD (_, _, _, block, elseblock_opt) ->
                List.iter count_instr block;
                Option.iter (List.iter count_instr) elseblock_opt
            | _ -> ())
          sl_spec

  let handle : Instrumentation_api.Event.t -> unit = function
    | Instr { instr; at = _ } -> State.incr State.instrs_hit (instr_key instr)
    | _ -> ()

  (* --- Output: Summary mode (stats + uncovered only) --- *)

  let print_stats () =
    let hit = Hashtbl.length State.instrs_hit in
    let total = !State.total_instrs in
    if total > 0 then
      Format.fprintf !fmt "SL Instructions: %d/%d (%.2f%%)\n" hit total
        (percentage hit total)

  let print_uncovered () =
    let total = !State.total_instrs in
    if total > 0 then (
      (* Collect uncovered instructions *)
      let uncovered = ref [] in
      List.iter
        (fun def ->
          match def.it with
          | Sl.RelD (id, _, block, elseblock_opt) ->
              List.iter
                (fun instr ->
                  if not (Hashtbl.mem State.instrs_hit (instr_key instr)) then
                    uncovered := (id.it, instr_header instr) :: !uncovered)
                block;
              Option.iter
                (List.iter (fun instr ->
                     if not (Hashtbl.mem State.instrs_hit (instr_key instr))
                     then uncovered := (id.it, instr_header instr) :: !uncovered))
                elseblock_opt
          | Sl.DecD (id, _, _, block, elseblock_opt) ->
              List.iter
                (fun instr ->
                  if not (Hashtbl.mem State.instrs_hit (instr_key instr)) then
                    uncovered := (id.it, instr_header instr) :: !uncovered)
                block;
              Option.iter
                (List.iter (fun instr ->
                     if not (Hashtbl.mem State.instrs_hit (instr_key instr))
                     then uncovered := (id.it, instr_header instr) :: !uncovered))
                elseblock_opt
          | _ -> ())
        !State.sl_spec;
      if !uncovered <> [] then (
        Format.fprintf !fmt "\nUncovered SL instructions:\n";
        List.iter
          (fun (rel, content) ->
            Format.fprintf !fmt "  %s:\n    %s\n" rel
              (normalize_whitespace content))
          (List.rev !uncovered)))

  (* --- Output: Full mode (GCOV-style annotated spec) --- *)

  let fmt_count tbl key =
    match Hashtbl.find_opt tbl key with
    | Some n -> Format.sprintf "%4d" n
    | None -> "####"

  let rec print_instr indent instr =
    let count = fmt_count State.instrs_hit (instr_key instr) in
    let max_len = max 40 (80 - String.length indent) in
    let content = instr_header instr |> summarize ~max_len in
    Format.fprintf !fmt "%5s %s%s\n" count indent content;
    match instr.it with
    | Sl.RelI { block; _ } -> List.iter (print_instr (indent ^ "  ")) block
    | Sl.RelAssertI { block; _ } ->
        List.iter (print_instr (indent ^ "  ")) block
    | Sl.IfI (_, _, instrs, _) -> List.iter (print_instr (indent ^ "  ")) instrs
    | Sl.CaseI (_, cases, _) ->
        List.iter
          (fun (guard, instrs) ->
            (* Print hyphen for guards (untracked) *)
            Format.fprintf !fmt "    - %s  Case %s:\n" indent
              (Sl.Print.string_of_guard guard);
            List.iter (print_instr (indent ^ "    ")) instrs)
          cases
    | Sl.OtherwiseI inner -> print_instr (indent ^ "  ") inner
    | Sl.LetI (_, _, _, block) -> List.iter (print_instr (indent ^ "  ")) block
    | _ -> ()

  let print_full () =
    List.iter
      (fun def ->
        match def.it with
        | Sl.RelD (id, _, block, elseblock_opt) ->
            Format.fprintf !fmt "\nrelation %s:\n" id.it;
            List.iter (print_instr "  ") block;
            Option.iter (List.iter (print_instr "  ")) elseblock_opt
        | Sl.DecD (id, _, _, block, elseblock_opt) ->
            Format.fprintf !fmt "\ndef $%s:\n" id.it;
            List.iter (print_instr "  ") block;
            Option.iter (List.iter (print_instr "  ")) elseblock_opt
        | _ -> ())
      !State.sl_spec

  (* --- Finish: print report --- *)

  let finish () =
    if !State.total_instrs > 0 then (
      Format.fprintf !fmt "\n=== SL Node Coverage ===\n\n";
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
  instrs_hit : ((region * string) * int) list; (* key * count *)
  total_instrs : int;
}

let get_result () =
  {
    instrs_hit = State.instrs_hit |> Hashtbl.to_seq |> List.of_seq;
    total_instrs = !State.total_instrs;
  }

(* Restore state from a previous result (for checkpoint resume) *)
let restore result =
  Hashtbl.clear State.instrs_hit;
  List.iter
    (fun (key, count) -> Hashtbl.replace State.instrs_hit key count)
    result.instrs_hit;
  State.total_instrs := result.total_instrs

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
  {
    instrs_hit = merge_counts r1.instrs_hit r2.instrs_hit;
    total_instrs = r1.total_instrs;
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

module Spec : Instrumentation_spec.Spec.S = struct
  let name = "instruction-coverage"
  let mode = `SL

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
            mode;
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
