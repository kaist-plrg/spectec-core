let spec_file_suffix = ".spectec"
let root_marker_suffix = ".spec"

let compare_natural a b =
  let is_digit c = '0' <= c && c <= '9' in
  (* "typing-5.11" splits into ["typing-"; "5"; "."; "11"]. *)
  let pieces s =
    String.fold_right
      (fun c pieces ->
        match pieces with
        | piece :: rest when is_digit c = is_digit piece.[0] ->
            (String.make 1 c ^ piece) :: rest
        | _ -> String.make 1 c :: pieces)
      s []
  in
  (* Compare digit runs as strings, not ints: a run may be longer than an
     int can hold. *)
  let compare_numbers p q =
    let rec strip_zeros p =
      if String.length p > 1 && p.[0] = '0' then
        strip_zeros (String.sub p 1 (String.length p - 1))
      else p
    in
    let p = strip_zeros p and q = strip_zeros q in
    if String.length p <> String.length q then
      Int.compare (String.length p) (String.length q)
    else String.compare p q
  in
  let compare_piece p q =
    if is_digit p.[0] && is_digit q.[0] then compare_numbers p q
    else String.compare p q
  in
  (* "05" and "5" are numerically equal; fall back to string order. *)
  match List.compare compare_piece (pieces a) (pieces b) with
  | 0 -> String.compare a b
  | c -> c

let collect root =
  let rec gather dir =
    let entries = Sys.readdir dir in
    Array.sort compare_natural entries;
    entries |> Array.to_list
    |> List.concat_map (fun entry ->
           let path = Filename.concat dir entry in
           if Sys.is_directory path then gather path
           else if Filename.check_suffix entry spec_file_suffix then [ path ]
           else [])
  in
  gather root

let root_of_file file =
  let holds_marker dir =
    match Sys.readdir dir with
    | exception Sys_error _ -> false
    | entries ->
        Array.exists
          (fun entry -> Filename.check_suffix entry root_marker_suffix)
          entries
  in
  let rec search dir =
    if holds_marker dir then Some dir
    else
      let parent = Filename.dirname dir in
      if String.equal parent dir then None else search parent
  in
  search (Filename.dirname file)
