open Sl.Ast
module MCov = Runtime_testgen.Cov.Multiple
open Util.Source

(* Measure initial coverage of phantoms *)

(* On cold boot, first measure the coverage of the seed *)

let boot_cold ?(mini : bool = false) (spec : spec) (includes_p4 : string list)
    (excludes_p4 : string list) (dirname_p4 : string)
    (filenames_ignore : string list) : MCov.Cover.t =
  let excludes_p4 = Filesys.collect_excludes excludes_p4 in
  let filenames_p4 = Filesys.collect_files ~suffix:".p4" dirname_p4 in
  let filenames_p4 =
    List.filter
      (fun filename_p4 ->
        not (List.exists (String.equal filename_p4) excludes_p4))
      filenames_p4
  in
  Interp_sl.Typing.cover_typings ~mini spec includes_p4 filenames_p4
    filenames_ignore

(* On warm boot, load the coverage from a file *)

let parse_line (line : string) : pid * MCov.Branch.t =
  let data = String.split_on_char ' ' line in
  match data with
  | pid :: status :: origin :: filenames ->
      let pid = int_of_string pid in
      let status =
        match status with
        | "Hit_likely" -> MCov.Branch.Hit (true, filenames)
        | "Hit_unlikely" -> MCov.Branch.Hit (false, filenames)
        | "Miss" ->
            if
              (* Complete Miss is parsed as a newline character instead of an empty list *)
              List.length filenames == 1
              && String.length (List.hd filenames) < 2
            then MCov.Branch.Miss []
            else MCov.Branch.Miss filenames
        | _ -> assert false
      in
      let origin = origin $ no_region in
      let branch = MCov.Branch.{ origin; status } in
      (pid, branch)
  | _ -> assert false

let rec parse_lines (cover : MCov.Cover.t) (ic : in_channel) : MCov.Cover.t =
  try
    let line = input_line ic in
    if String.starts_with ~prefix:"#" line then parse_lines cover ic
    else
      let pid, branch = parse_line line in
      let cover = MCov.Cover.add pid branch cover in
      parse_lines cover ic
  with End_of_file -> cover

let boot_warm (filename_cov : string) : MCov.Cover.t =
  let ic = open_in filename_cov in
  parse_lines MCov.Cover.empty ic
