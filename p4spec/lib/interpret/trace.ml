open Il.Ast
open Il.Print
open Util.Source

(* Execution trace *)

type t =
  | Rel of id * id * value list * t list
  | Dec of id * int * value list * t list
  | Prem of prem
  | Empty

(* Constructors *)

let open_rel (id_rel : id) (id_rule : id) (values_input : value list) : t =
  Rel (id_rel, id_rule, values_input, [])

let open_dec (id_func : id) (idx_clause : int) (values_input : value list) : t =
  Dec (id_func, idx_clause, values_input, [])

(* Nesting *)

let commit (trace : t) (trace_sub : t) : t =
  match trace with
  | Rel (id_rel, id_rule, values, traces) ->
      Rel (id_rel, id_rule, values, traces @ [ trace_sub ])
  | Dec (id_func, idx_clause, values, traces) ->
      Dec (id_func, idx_clause, values, traces @ [ trace_sub ])
  | Prem _ -> assert false
  | Empty -> trace_sub

(* Extension *)

let extend (trace : t) (prem : prem) : t =
  match trace with
  | Rel (id_rel, id_rule, values, traces) ->
      Rel (id_rel, id_rule, values, traces @ [ Prem prem ])
  | Dec (id_func, idx_clause, values, traces) ->
      Dec (id_func, idx_clause, values, traces @ [ Prem prem ])
  | Prem _ | Empty -> assert false

(* Analysis *)

module Counter = Map.Make (String)

type counter = int Counter.t

let update_counter (id : string) (counter : counter) : counter =
  match Counter.find_opt id counter with
  | None -> Counter.add id 1 counter
  | Some count -> Counter.add id (count + 1) counter

let log_counter (counter : counter) : string =
  Counter.bindings counter
  |> List.sort (fun (_, count_a) (_, count_b) -> count_b - count_a)
  |> List.map (fun (id, count) -> Format.asprintf "   [ %s ]: %d" id count)
  |> String.concat "\n"

let rec analyze' (rules : counter) (funcs : counter) (trace : t) :
    counter * counter =
  match trace with
  | Rel (id_rel, _, _, traces) ->
      let rules = update_counter id_rel.it rules in
      List.fold_left
        (fun (rules, funcs) trace -> analyze' rules funcs trace)
        (rules, funcs) traces
  | Dec (id_func, _, _, traces) ->
      let funcs = update_counter id_func.it funcs in
      List.fold_left
        (fun (rules, funcs) trace -> analyze' rules funcs trace)
        (rules, funcs) traces
  | _ -> (rules, funcs)

let analyze (trace : t) : unit =
  let rules, funcs = analyze' Counter.empty Counter.empty trace in
  Format.printf "Rules:\n";
  Format.printf "%s\n" (log_counter rules);
  Format.printf "Functions:\n";
  Format.printf "%s\n" (log_counter funcs)

(* Printing *)

let rec log ?(depth = 0) ?(idx = 0) ?(verbose = false) (trace : t) : string =
  let log_values values =
    match (verbose, values) with
    | false, _ | true, [] -> ""
    | _ ->
        Format.asprintf "--- input ---\n%s\n-------------\n"
          (String.concat "\n" (List.map string_of_value values))
  in
  match trace with
  | Rel (id_rel, id_rule, values, traces) ->
      Format.asprintf "[>>> %d] Rule %s/%s\n%s%s[<<< %d] Rule %s/%s" (depth + 1)
        id_rel.it id_rule.it (log_values values)
        (logs ~depth:(depth + 1) ~verbose traces)
        (depth + 1) id_rel.it id_rule.it
  | Dec (id_func, idx_clause, values, traces) ->
      Format.asprintf "[>>> %d] Clause %s/%d\n%s%s[<<< %d] Clause %s/%d"
        (depth + 1) id_func.it idx_clause (log_values values)
        (logs ~depth:(depth + 1) ~verbose traces)
        (depth + 1) id_func.it idx_clause
  | Prem prem -> Format.asprintf "[%d-%d] %s" depth idx (string_of_prem prem)
  | Empty -> ""

and logs ?(depth = 0) ?(verbose = false) (traces : t list) : string =
  match traces with
  | [] -> ""
  | _ ->
      List.fold_left
        (fun (idx, straces) trace ->
          let idx = match trace with Prem _ -> idx + 1 | _ -> idx in
          let strace = log ~depth ~idx ~verbose trace in
          (idx, straces @ [ strace ]))
        (0, []) traces
      |> snd |> String.concat "\n" |> Format.asprintf "%s\n"
