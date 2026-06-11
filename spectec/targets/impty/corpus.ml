type outcome = Saved of string | Duplicate

(* mkdir -p. *)
let rec ensure_dir dir =
  if dir = "" || dir = "." || dir = Filename.dirname dir then ()
  else if Sys.file_exists dir then ()
  else (
    ensure_dir (Filename.dirname dir);
    Sys.mkdir dir 0o755)

let sanitize =
  String.map (fun c ->
      if
        (c >= 'a' && c <= 'z')
        || (c >= 'A' && c <= 'Z')
        || (c >= '0' && c <= '9')
        || c = '_' || c = '-'
      then c
      else '_')

(* Lowest free [<base>[_k]<ext>] slot in [dir]. *)
let rec free_slot dir base ext i =
  let candidate =
    if i = 0 then base ^ ext else Printf.sprintf "%s_%d%s" base i ext
  in
  if Sys.file_exists (Filename.concat dir candidate) then
    free_slot dir base ext (i + 1)
  else candidate

let read_file path =
  try
    let ic = open_in_bin path in
    Fun.protect
      ~finally:(fun () -> close_in ic)
      (fun () -> Some (really_input_string ic (in_channel_length ic)))
  with Sys_error _ -> None

let save ~out_dir ~base ~ext ~content =
  let base = sanitize base in
  try
    ensure_dir out_dir;
    let entries = try Sys.readdir out_dir with Sys_error _ -> [||] in
    let already_saved =
      Array.exists
        (fun f ->
          Filename.check_suffix f ext
          && read_file (Filename.concat out_dir f) = Some content)
        entries
    in
    if already_saved then Ok Duplicate
    else
      let path = Filename.concat out_dir (free_slot out_dir base ext 0) in
      let oc = open_out_bin path in
      Fun.protect
        ~finally:(fun () -> close_out oc)
        (fun () -> output_string oc content);
      Ok (Saved path)
  with Sys_error msg -> Error msg
