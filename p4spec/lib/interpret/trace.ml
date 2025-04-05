open Il.Ast
open Il.Print
open Util.Source

(* Execution trace *)

type time =
  | ING of float
  (* start time *)
  | END of (float * float)
(* accumulated duration, duration *)
  | CACHED

type t =
  | Rel of {
      id_rel : id;
      id_rule : id;
      values_input : value list;
      time : time;
      subtraces : t list;
    }
  | Dec of {
      id_func : id;
      idx_clause : int;
      values_input : value list;
      time : time;
      subtraces : t list;
    }
  | Iter of { inner : string; time : time; subtraces : t list }
  | Prem of prem
  | Empty

(* Openers *)

let open_time () : time = ING (Unix.gettimeofday ())

let open_rel (id_rel : id) (id_rule : id) (values_input : value list) : t =
  let time = open_time () in
  Rel { id_rel; id_rule; values_input; time; subtraces = [] }

let open_dec (id_func : id) (idx_clause : int) (values_input : value list) : t =
  let time = open_time () in
  Dec { id_func; idx_clause; values_input; time; subtraces = [] }

let open_iter (inner : string) : t =
  let time = open_time () in
  Iter { inner; time; subtraces = [] }

(* Closers *)

let close_time (time_start : time) (subtraces : t list) : time =
  let time_start =
    match time_start with ING time_start -> time_start | _ -> assert false
  in
  let time_end = Unix.gettimeofday () in
  let time_sub =
    subtraces
    |> List.map (fun trace ->
           match trace with
           | Rel { time; _ } | Dec { time; _ } | Iter { time; _ } -> (
               match time with
               | END (duration_acc, _) ->
  (if duration_acc < 0.0 then
     Format.asprintf "negative inner acc: %.6f" duration_acc |> print_endline);
                 duration_acc
               | CACHED -> 0.0
               | _ -> assert false)
           | _ -> 0.0)
    |> List.fold_left ( +. ) 0.0
  in
  let duration_acc = time_end -. time_start in
  let duration = duration_acc -. time_sub in
  END (duration_acc, duration)

let close (trace : t) : t =
  match trace with
  | Rel { id_rel; id_rule; values_input; time; subtraces } ->
      let time = close_time time subtraces in
      Rel { id_rel; id_rule; values_input; time; subtraces }
  | Dec { id_func; idx_clause; values_input; time; subtraces } ->
      let time = close_time time subtraces in
      Dec { id_func; idx_clause; values_input; time; subtraces }
  | Iter { inner; time; subtraces } ->
      let time = close_time time subtraces in
      Iter { inner; time; subtraces }
  | _ -> assert false

(* Pretty Printing *)

let pp_time fmt (time: time) =
  match time with
  | ING time_start -> Format.fprintf fmt "ING: %.6f ago" (Unix.gettimeofday () -. time_start)
  | END (_, dur) -> Format.fprintf fmt "%.6f" dur
  | CACHED -> Format.fprintf fmt "[cached]"

(* Caching *)

let rec wipe_time (trace: t) : t =
  match trace with
  | Rel t -> 
    Rel { t with time = CACHED; subtraces = List.map wipe_time t.subtraces }
  | Dec t -> 
    Dec { t with time = CACHED; subtraces = List.map wipe_time t.subtraces }
  | Iter t ->
    Iter { t with time = CACHED; subtraces = List.map wipe_time t.subtraces }
  | _ -> trace

let get_wiped_subtraces (trace: t) : t list =
  match trace with
  | Rel t -> List.map wipe_time t.subtraces
  | Dec t -> List.map wipe_time t.subtraces
  | Iter t -> List.map wipe_time t.subtraces
  | _ -> assert false

let replace_subtraces (trace : t) (subtraces_new : t list) : t =
  match trace with
  | Rel r ->
    Rel { r with subtraces=subtraces_new }
  | Dec r ->
    Dec { r with subtraces=subtraces_new }
  | Iter r ->
    Iter { r with subtraces=subtraces_new }
  | _ -> assert false

(* Committing *)

let commit (trace : t) (trace_sub : t) : t =
  match trace with
  | Rel { id_rel; id_rule; values_input; time; subtraces; _ } ->
      let subtraces = subtraces @ [ trace_sub ] in
      Rel { id_rel; id_rule; values_input; time; subtraces }
  | Dec { id_func; idx_clause; values_input; time; subtraces } ->
      let subtraces = subtraces @ [ trace_sub ] in
      Dec { id_func; idx_clause; values_input; time; subtraces }
  | Iter { inner; time; subtraces } ->
      let subtraces = subtraces @ [ trace_sub ] in
      Iter { inner; time; subtraces }
  | Prem _ -> assert false
  | Empty -> trace_sub

(* Extension *)

let extend (trace : t) (prem : prem) : t =
  match trace with
  | Rel { id_rel; id_rule; values_input; time; subtraces } ->
      let subtraces = subtraces @ [ Prem prem ] in
      Rel { id_rel; id_rule; values_input; time; subtraces }
  | Dec { id_func; idx_clause; values_input; time; subtraces } ->
      let subtraces = subtraces @ [ Prem prem ] in
      Dec { id_func; idx_clause; values_input; time; subtraces }
  | Iter { inner; time; subtraces } ->
      let subtraces = subtraces @ [ Prem prem ] in
      Iter { inner; time; subtraces }
  | Prem _ | Empty -> assert false

(* Printing *)

module Tagger = Map.Make (Int)

type tagger = int Tagger.t

let tag (tagger : tagger) (depth : int) : string =
  let tag = Tagger.find depth tagger in
  Format.asprintf "%d@%d" depth tag

let update_tagger (tagger : tagger) (depth : int) : tagger =
  let tag =
    match Tagger.find_opt depth tagger with None -> 0 | Some tag -> tag
  in
  Tagger.add depth (tag + 1) tagger

let rec log ?(tagger = Tagger.empty) ?(depth = 0) ?(idx = 0) ?(verbose = false)
    (trace : t) : string =
  let log_values values =
    match (verbose, values) with
    | false, _ | true, [] -> ""
    | _ ->
        Format.asprintf "--- input ---\n%s\n-------------\n"
          (String.concat "\n" (List.map string_of_value values))
  in
  let log_time fmt time =
    match time with
    | ING _ -> assert false
    | _ -> pp_time fmt time
  in
  match trace with
  | Rel { id_rel; id_rule; values_input; time; subtraces } ->
      let depth = depth + 1 in
      let tagger = update_tagger tagger depth in
      Format.asprintf "[>>> %s] Rule %s/%s\n%s%s[<<< %s] Rule %s/%s %a"
        (tag tagger depth) id_rel.it id_rule.it (log_values values_input)
        (logs ~tagger ~depth ~verbose subtraces)
        (tag tagger depth) id_rel.it id_rule.it log_time time
  | Dec { id_func; idx_clause; values_input; time; subtraces } ->
      let depth = depth + 1 in
      let tagger = update_tagger tagger depth in
      Format.asprintf "[>>> %s] Clause %s/%d\n%s%s[<<< %s] Clause %s/%d %a"
        (tag tagger depth) id_func.it idx_clause (log_values values_input)
        (logs ~tagger ~depth ~verbose subtraces)
        (tag tagger depth) id_func.it idx_clause log_time time
  | Iter { inner; time; subtraces } ->
      let depth = depth + 1 in
      let tagger = update_tagger tagger depth in
      Format.asprintf "[>>> %s] Iteration %s\n%s[<<< %s] Iteration %a"
        (tag tagger depth) inner
        (logs ~tagger ~depth ~verbose subtraces)
        (tag tagger depth) log_time time
  | Prem prem ->
      Format.asprintf "[%s-%d] %s" (tag tagger depth) idx (string_of_prem prem)
  | Empty -> ""

and logs ?(tagger = Tagger.empty) ?(depth = 0) ?(verbose = false)
    (traces : t list) : string =
  match traces with
  | [] -> ""
  | _ ->
      List.fold_left
        (fun (idx, straces) trace ->
          let idx = match trace with Prem _ -> idx + 1 | _ -> idx in
          let strace = log ~tagger ~depth ~idx ~verbose trace in
          (idx, straces @ [ strace ]))
        (0, []) traces
      |> snd |> String.concat "\n" |> Format.asprintf "%s\n"

(* Profiling *)

module Counter = Map.Make (String)

type counter = (int * float) Counter.t

let update_counter (id : string) (duration : float) (counter : counter) :
    counter =
  match Counter.find_opt id counter with
  | None -> Counter.add id (1, duration) counter
  | Some (count, duration_total) ->
      Counter.add id (count + 1, duration_total +. duration) counter

let log_counter (counter : counter) : string =
  Counter.bindings counter
  |> List.sort (fun (_, (_, duration_a)) (_, (_, duration_b)) ->
         compare duration_b duration_a)
  |> List.map (fun (id, (count, duration_total)) ->
         Format.asprintf "   [ %s ]: %d (%.6f / %.6f)" id count duration_total
           (duration_total /. float_of_int count))
  |> String.concat "\n"

let rec profile' (rules : counter) (funcs : counter) (trace : t) :
    counter * counter =
  match trace with
  | Rel { id_rel; subtraces; time; _ } ->
      let rules = 
        match time with
        | END (_, duration) -> update_counter id_rel.it duration rules 
        | CACHED -> rules
        | _ -> assert false
      in
      List.fold_left
        (fun (rules, funcs) trace -> profile' rules funcs trace)
        (rules, funcs) subtraces
  | Dec { id_func; subtraces; time; _ } ->
      let funcs = 
        match time with
        | END (_, duration) -> update_counter id_func.it duration funcs
        | CACHED -> funcs
        | _ -> assert false
      in
      List.fold_left
        (fun (rules, funcs) trace -> profile' rules funcs trace)
        (rules, funcs) subtraces
  | Iter { subtraces; _ } ->
      List.fold_left
        (fun (rules, funcs) trace -> profile' rules funcs trace)
        (rules, funcs) subtraces
  | _ -> (rules, funcs)

let profile (trace : t) : unit =
  Format.printf "Trace...\n";
  Format.printf "%s\n" (log trace);
  Format.printf "Profiling...\n";
  let rules, funcs = profile' Counter.empty Counter.empty trace in
  Format.printf "Rules:\n";
  Format.printf "%s\n" (log_counter rules);
  Format.printf "Functions:\n";
  Format.printf "%s\n" (log_counter funcs)
