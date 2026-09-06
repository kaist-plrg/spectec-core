type t = (string, string array) Hashtbl.t

let empty : t = Hashtbl.create 1

let lines_of contents =
  contents |> String.split_on_char '\n'
  |> List.map (fun line ->
         let n = String.length line in
         if n > 0 && line.[n - 1] = '\r' then String.sub line 0 (n - 1)
         else line)
  |> Array.of_list

let of_sources sources : t =
  let table = Hashtbl.create (List.length sources) in
  List.iter
    (fun (filename, contents) ->
      Hashtbl.replace table filename (lines_of contents))
    sources;
  table

(* Ignore comment delimiters inside quoted, escaped text. *)
let comment_start line =
  let n = String.length line in
  let rec scan i in_text =
    if i >= n then None
    else if in_text then
      match line.[i] with
      | '\\' -> scan (i + 2) true
      | '"' -> scan (i + 1) false
      | _ -> scan (i + 1) true
    else
      match line.[i] with
      | '"' -> scan (i + 1) true
      | ';' when i + 1 < n && line.[i + 1] = ';' -> Some i
      | _ -> scan (i + 1) false
  in
  scan 0 false

let split line =
  match comment_start line with
  | None -> (line, None)
  | Some i ->
      let rest = String.sub line (i + 2) (String.length line - i - 2) in
      (String.sub line 0 i, Some (String.trim rest))

let is_blank text = String.equal (String.trim text) ""

(* Trailing comments document code on their line. *)
let trailing lines i =
  if i < 0 || i >= Array.length lines then None
  else
    match split lines.(i) with
    | code, Some text when (not (is_blank code)) && text <> "" -> Some text
    | _ -> None

(* Bound lookback to avoid absorbing licence headers. *)
let max_leading_lines = 20

let leading lines i =
  let rec gather acc j depth =
    if j < 0 || depth >= max_leading_lines then acc
    else
      match split lines.(j) with
      (* Empty comments separate banners from declaration documentation. *)
      | code, Some text when is_blank code && text <> "" ->
          gather (text :: acc) (j - 1) (depth + 1)
      | _ -> acc
  in
  match gather [] (i - 1) 0 with
  | [] -> None
  | texts -> Some (String.concat "\n" texts)

let find (docs : t) ~file ~line =
  match Hashtbl.find_opt docs file with
  | None -> None
  | Some lines -> (
      (* Source lines are 1-based. *)
      let i = line - 1 in
      if i < 0 || i >= Array.length lines then None
      else
        match trailing lines i with
        | Some _ as found -> found
        | None -> leading lines i)
