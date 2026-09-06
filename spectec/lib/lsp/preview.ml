(** Render whole specs; retain successful previews across failures. *)

open Common.Source

type stage = Il | Sl | Pl

let stage_of_string = function
  | "il" -> Some Il
  | "sl" -> Some Sl
  | "pl" -> Some Pl
  | _ -> None

let tag_of_stage = function Il -> "il" | Sl -> "sl" | Pl -> "pl"

type entry = { line : int; depth : int; region : region }
type reason = { message : string; region : region }

type render = {
  text : string;
  entries : entry list;
  stale : bool;
  reason : reason option;
}

(* Cache per spec/stage; source digests detect unchanged inputs. *)
type good = { digest : string; text : string; entries : entry list }
type t = (string, good) Hashtbl.t

let create () : t = Hashtbl.create 4
let is_digit c = '0' <= c && c <= '9'

(* Read trailing digits, returning the preceding index. *)
let digits_before text i =
  let j = ref i in
  while !j >= 0 && is_digit text.[!j] do
    decr j
  done;
  if !j = i then None
  else Some (int_of_string (String.sub text (!j + 1) (i - !j)), !j)

(* Parse line/column backwards, returning the separator's index. *)
let position_ending_at text i ~sep =
  match digits_before text i with
  | Some (column, i) when i >= 0 && text.[i] = ':' -> (
      match digits_before text (i - 1) with
      | Some (line, i) when i >= 0 && text.[i] = sep -> Some (line, column, i)
      | _ -> None)
  | _ -> None

(* Parse backwards around path separators; columns are 1-based. *)
let parse_region text =
  let length = String.length text in
  let length =
    if length > 0 && text.[length - 1] = ':' then length - 1 else length
  in
  let at file line column = { file; line; column = column - 1 } in
  let span left right = Some { left; right } in
  if length = 0 then None
  else
    match position_ending_at text (length - 1) ~sep:'-' with
    | Some (right_line, right_column, dash) -> (
        match position_ending_at text (dash - 1) ~sep:':' with
        | Some (left_line, left_column, colon) ->
            let file = String.sub text 0 colon in
            span
              (at file left_line left_column)
              (at file right_line right_column)
        | None -> None)
    | None -> (
        match position_ending_at text (length - 1) ~sep:':' with
        | Some (line, column, colon) ->
            let file = String.sub text 0 colon in
            span (at file line column) (at file line column)
        | None -> None)

(* Recover source mappings from printed region comments. *)
let comment_of_line raw =
  let indent = ref 0 in
  let length = String.length raw in
  while !indent < length && raw.[!indent] = ' ' do
    incr indent
  done;
  let rest = String.sub raw !indent (length - !indent) in
  let marker = ";; " in
  let width = String.length marker in
  if
    String.length rest <= width
    || not (String.equal (String.sub rest 0 width) marker)
  then None
  else
    (* Indentation distinguishes rules; only valid regions create mappings. *)
    parse_region (String.sub rest width (String.length rest - width))
    |> Option.map (fun region -> ((if !indent = 0 then 0 else 1), region))

let comment_entries text =
  String.split_on_char '\n' text
  |> List.mapi (fun line raw ->
         comment_of_line raw
         |> Option.map (fun (depth, region) -> { line; depth; region }))
  |> List.filter_map Fun.id

(* Offset per-definition mappings into the complete preview. *)
let render_defs render defs =
  let rendered = List.map render defs in
  let _, entries =
    List.fold_left
      (fun (offset, entries) (text, mappings) ->
        let entries =
          List.fold_left
            (fun entries entry ->
              { entry with line = offset + entry.line } :: entries)
            entries mappings
        in
        let next =
          offset + 2
          + String.fold_left
              (fun count c -> if c = '\n' then count + 1 else count)
              0 text
        in
        (next, entries))
      (0, []) rendered
  in
  (String.concat "\n\n" (List.map fst rendered), List.rev entries)

let prose_entries region steps =
  { line = 0; depth = 0; region }
  :: List.map (fun (line, region) -> { line; depth = 2; region }) steps

let digest_of (sources : Spectec.spec_source list) =
  let buffer = Buffer.create 4096 in
  List.iter
    (fun (source : Spectec.spec_source) ->
      Buffer.add_string buffer source.filename;
      Buffer.add_char buffer '\000';
      Buffer.add_string buffer source.contents;
      Buffer.add_char buffer '\000')
    sources;
  Digest.string (Buffer.contents buffer)

(* Render CLI output; source mapping stays within LSP. *)
let render_stage stage spec_el spec_il =
  match stage with
  | Il ->
      let text = Lang.Il.Print.string_of_spec spec_il in
      (text, comment_entries text)
  | Sl ->
      Spectec.structure spec_il
      |> render_defs (fun (def : Lang.Sl.def) ->
             let text = Lang.Sl.Print.string_of_def def in
             (text, prose_entries def.at (Preview_map.sl ~text def)))
  | Pl ->
      let spec_sl = Spectec.structure spec_il in
      let henv =
        Spectec.henv_with_il_spec (Spectec.henv_of_el_spec spec_el) spec_il
      in
      Spectec.shorten (Spectec.annotate ~henv spec_sl)
      |> render_defs (fun (def : Lang.Pl.def) ->
             let text =
               ";; "
               ^ string_of_region def.node.at
               ^ "\n"
               ^ Lang.Pl.Print.string_of_def def
             in
             (text, prose_entries def.node.at (Preview_map.pl ~text def)))

let first_error bag =
  Spectec.Diagnostic.Bag.to_sorted_list bag
  |> List.find_opt (fun (d : Spectec.Diagnostic.t) ->
         d.severity = Spectec.Diagnostic.Error)
  |> Option.map (fun (d : Spectec.Diagnostic.t) ->
         { message = d.message; region = d.region })

let stale cache key reason =
  match Hashtbl.find_opt cache key with
  | Some good ->
      { text = good.text; entries = good.entries; stale = true; reason }
  | None -> { text = ""; entries = []; stale = true; reason }

let render ?(stage = Il) (cache : t) ~open_path ~text =
  let open_path = Check.canonical open_path in
  match Check.sources_of ~open_path text with
  | Error unreadable ->
      let message =
        match unreadable with
        | (file, message) :: _ ->
            Printf.sprintf "cannot read spec file %s: %s"
              (Filename.basename file) message
        | [] -> "cannot read the spec"
      in
      stale cache open_path
        (Some { message; region = region_of_file open_path })
  | Ok sources -> (
      let key =
        String.concat "\000"
          (tag_of_stage stage
          :: List.map
               (fun (source : Spectec.spec_source) -> source.filename)
               sources)
      in
      let digest = digest_of sources in
      let cached = Hashtbl.find_opt cache key in
      if
        Option.fold ~none:false
          ~some:(fun good -> String.equal good.digest digest)
          cached
      then
        let good = Option.get cached in
        {
          text = good.text;
          entries = good.entries;
          stale = false;
          reason = None;
        }
      else
        let result, bag =
          Spectec.with_diagnostics (fun () ->
              Result.bind (Spectec.parse_spec_sources sources) (fun spec_el ->
                  Result.map
                    (fun spec_il -> (spec_el, spec_il))
                    (Spectec.elaborate spec_el)))
        in
        match result with
        | Ok (spec_el, spec_il) ->
            let text, entries = render_stage stage spec_el spec_il in
            Hashtbl.replace cache key { digest; text; entries };
            { text; entries; stale = false; reason = None }
        | Error _ -> stale cache key (first_error bag))

let render ?(stage = Il) (cache : t) ~open_path ~text =
  try render ~stage cache ~open_path ~text
  with exn ->
    stale cache
      (Check.canonical open_path)
      (Some
         {
           message = "internal error: " ^ Printexc.to_string exn;
           region = region_of_file open_path;
         })
