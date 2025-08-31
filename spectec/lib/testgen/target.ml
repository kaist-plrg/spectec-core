open Domain.Lib
open Sl.Ast

(* Get filenames per target phantom *)

let parse_line (line : string) : pid * string list =
  let data = String.split_on_char ' ' line in
  match data with
  | pid :: filenames ->
      let pid = int_of_string pid in
      let filenames = filenames in
      (pid, filenames)
  | _ -> assert false

let rec parse_lines (targets : string list PIdMap.t) (ic : in_channel) :
    string list PIdMap.t =
  try
    let line = input_line ic in
    let pid, filenames = parse_line line in
    let targets =
      match PIdMap.find_opt pid targets with
      | Some filenames' -> PIdMap.add pid (filenames @ filenames') targets
      | None -> PIdMap.add pid filenames targets
    in
    parse_lines targets ic
  with End_of_file -> targets

let init (filename_target : string) : string list PIdMap.t =
  let ic = open_in filename_target in
  let targets = parse_lines PIdMap.empty ic in
  close_in ic;
  targets
