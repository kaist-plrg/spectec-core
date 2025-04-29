open Domain.Lib
open Sl.Ast
module MCov = Runtime_testgen.Cov.Multiple

(* Measure initial coverage of phantoms *)

(* On cold boot, first measure the coverage of the seed *)

let boot_cold (spec : spec) (includes_p4 : string list)
    (filenames_p4 : string list) : PIdSet.t =
  let cover_multi =
    Interp_sl.Interp.cover_typings spec includes_p4 filenames_p4
  in
  MCov.log ~short:true cover_multi;
  let misses = MCov.collect_miss cover_multi in
  misses |> List.map fst |> PIdSet.of_list

(* On warm boot, load the coverage from a file *)

let boot_warm (filename_cov : string) : PIdSet.t =
  let oc = open_in filename_cov in
  let rec read_lines pids =
    try
      let line = input_line oc in
      let line =
        if String.starts_with ~prefix:"Phantom#" line then
          String.sub line 8 (String.length line - 8)
        else line
      in
      match int_of_string_opt line with
      | Some pid -> read_lines (pid :: pids)
      | None -> read_lines pids
    with End_of_file -> List.rev pids
  in
  let pids = read_lines [] in
  close_in oc;
  pids |> PIdSet.of_list
