open Domain.Lib
open Util.Source

let parse_file (filename_ignore : string) : IdSet.t =
  let ic = open_in filename_ignore in
  let rec parse_lines ignores =
    try
      let name = input_line ic $ no_region in
      let ignores = IdSet.add name ignores in
      parse_lines ignores
    with End_of_file -> ignores
  in
  let ignores = parse_lines IdSet.empty in
  close_in ic;
  ignores

let init (filenames_ignore : string list) : IdSet.t =
  filenames_ignore |> List.map parse_file
  |> List.fold_left IdSet.union IdSet.empty
