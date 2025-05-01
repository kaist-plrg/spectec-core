open Domain.Lib
open Sl.Ast

(* Get filenames per target phantom *)

let parse_line (line : string) : pid * string =
  let data = String.split_on_char ' ' line in
  match data with
  | [ pid; filename ] ->
      let pid = int_of_string pid in
      let filename = filename in
      (pid, filename)
  | _ -> assert false

let rec parse_lines (targets : string list PIdMap.t) (ic : in_channel) :
    string list PIdMap.t =
  try
    let line = input_line ic in
    let pid, filename = parse_line line in
    let targets =
      match PIdMap.find_opt pid targets with
      | Some filenames -> PIdMap.add pid (filename :: filenames) targets
      | None -> PIdMap.add pid [ filename ] targets
    in
    parse_lines targets ic
  with End_of_file -> targets

let target_phantom (filename_target : string) : string list PIdMap.t =
  let ic = open_in filename_target in
  let targets = parse_lines PIdMap.empty ic in
  close_in ic;
  targets
