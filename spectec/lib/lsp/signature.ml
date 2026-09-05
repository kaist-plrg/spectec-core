module Lsp = Linol_eio

let is_name_char c =
  ('a' <= c && c <= 'z')
  || ('A' <= c && c <= 'Z')
  || ('0' <= c && c <= '9')
  || c = '_' || c = '\''

let opens = function '(' | '[' | '{' -> true | _ -> false
let closes = function ')' | ']' | '}' -> true | _ -> false

(* Render labels and argument spans together for consistency. *)
let render (parts : Index.part list) =
  let buffer = Buffer.create 64 in
  let spans = ref [] in
  List.iter
    (function
      | Index.Literal text -> Buffer.add_string buffer text
      | Index.Hole name ->
          let start = Buffer.length buffer in
          Buffer.add_string buffer name;
          spans := (start, Buffer.length buffer) :: !spans)
    parts;
  (Buffer.contents buffer, List.rev !spans)

let depth_upto text stop =
  let depth = ref 0 in
  for i = 0 to min stop (String.length text) - 1 do
    if opens text.[i] then incr depth
    else if closes text.[i] && !depth > 0 then decr depth
  done;
  !depth

(* Match separators only at the current bracket depth. *)
let find text ~from literal =
  let n = String.length text and width = String.length literal in
  let wanted = depth_upto text from in
  let depth = ref wanted in
  let found = ref None in
  let i = ref from in
  while !found = None && !i + width <= n do
    if !depth = wanted && String.equal (String.sub text !i width) literal then
      found := Some !i
    else (
      if opens text.[!i] then incr depth
      else if closes text.[!i] && !depth > 0 then decr depth;
      incr i)
  done;
  !found

(* Count entered holes by matching their notation separators. *)
let active_hole (parts : Index.part list) (typed : string) =
  let entered = ref (-1) in
  let pos = ref 0 in
  let stopped = ref false in
  List.iter
    (fun part ->
      if not !stopped then
        match part with
        | Index.Hole _ -> incr entered
        | Index.Literal literal -> (
            match find typed ~from:!pos literal with
            | Some at -> pos := at + String.length literal
            | None -> stopped := true))
    parts;
  (* Select the first argument before its separator appears. *)
  max 0 !entered

(* Find the innermost unfinished application around the cursor. *)
let head ~(index : Index.t) ~(line : string) ~(character : int) =
  let stop_at = min character (String.length line) in
  let name_ending_at stop =
    let start = ref stop in
    while !start > 0 && is_name_char line.[!start - 1] do
      decr start
    done;
    if !start = stop then None
    else
      let start =
        if !start > 0 && line.[!start - 1] = '$' then !start - 1 else !start
      in
      Some (start, String.sub line start (stop - start))
  in
  let applied (start, name) =
    Index.find index name
    |> List.find_opt (fun (entry : Index.entry) -> entry.shape <> [])
    |> Option.map (fun entry -> (entry, start))
  in
  let depth = ref 0 in
  let found = ref None in
  let stopped = ref false in
  let i = ref (stop_at - 1) in
  while (not !stopped) && !found = None && !i >= 0 do
    let c = line.[!i] in
    if closes c then incr depth
    else if opens c then
      if !depth > 0 then (
        decr depth;
        (* Skip completed calls together with their preceding names. *)
        if !depth = 0 then
          match name_ending_at !i with
          | Some (start, _) -> i := start
          | None -> ())
      else (
        (if c = '(' then
           match name_ending_at !i with
           | Some (_, name) as call
             when String.length name > 0 && name.[0] = '$' ->
               found := Option.bind call applied
           | _ -> ());
        stopped := true)
    else if
      !depth = 0 && is_name_char c
      && (!i + 1 >= stop_at || not (is_name_char line.[!i + 1]))
    then found := Option.bind (name_ending_at (!i + 1)) applied;
    decr i
  done;
  !found

let none = Lsp.SignatureHelp.create ~signatures:[] ()

let at ~(index : Index.t) ~(line : string) ~(character : int) =
  match head ~index ~line ~character with
  | None -> none
  | Some (entry, start) ->
      let label, spans = render entry.shape in
      let typed =
        String.sub line start (min character (String.length line) - start)
      in
      (* Clamp excess arguments to the last known parameter. *)
      let active =
        min (active_hole entry.shape typed) (List.length spans - 1)
      in
      let parameters =
        List.map
          (fun (from, until) ->
            Lsp.ParameterInformation.create ~label:(`Offset (from, until)) ())
          spans
      in
      let signature =
        Lsp.SignatureInformation.create ~label ~parameters
          ?documentation:(Option.map (fun doc -> `String doc) entry.doc)
          ~activeParameter:(Some active) ()
      in
      Lsp.SignatureHelp.create ~signatures:[ signature ] ~activeSignature:0
        ~activeParameter:(Some active) ()
