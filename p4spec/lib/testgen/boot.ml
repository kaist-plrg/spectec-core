open Sl.Ast
module MCov = Runtime_testgen.Cov.Multiple
open Util.Source

(* Measure initial coverage of phantoms *)

(* On cold boot, first measure the coverage of the seed *)

let boot_cold (spec : spec) (includes_p4 : string list)
    (filenames_p4 : string list) : MCov.Cover.t =
  Interp_sl.Typing.cover_typings spec includes_p4 filenames_p4

(* On warm boot, load the coverage from a file *)

let parse_line (line : string) : pid * MCov.Branch.t =
  let data = String.split_on_char ' ' line in
  match data with
  | pid :: status :: origin :: filenames ->
      let pid = int_of_string pid in
      let status =
        match status with
        | "Hit" -> MCov.Branch.Hit
        | "Miss" -> MCov.Branch.Miss filenames
        | _ -> assert false
      in
      let origin = origin $ no_region in
      let branch = MCov.Branch.{ origin; status } in
      (pid, branch)
  | _ -> assert false

let rec parse_lines (cover : MCov.Cover.t) (ic : in_channel) : MCov.Cover.t =
  try
    let line = input_line ic in
    let pid, branch = parse_line line in
    let cover = MCov.Cover.add pid branch cover in
    parse_lines cover ic
  with End_of_file -> cover

let boot_warm (filename_cov : string) : MCov.Cover.t =
  let ic = open_in filename_cov in
  parse_lines MCov.Cover.empty ic
