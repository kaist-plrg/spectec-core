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

let nest (trace : t) (trace_sub : t) : t =
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
