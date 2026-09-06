open Common.Source
module Lsp = Linol_eio

let ( let* ) = Result.bind

(* Convert source lines to LSP's zero-based positions. *)
let position (p : pos) : Lsp.Position.t =
  Lsp.Position.create ~line:(max 0 (p.line - 1)) ~character:(max 0 p.column)

let range (r : region) : Lsp.Range.t =
  Lsp.Range.create ~start:(position r.left) ~end_:(position r.right)

let location (r : region) : Lsp.Location.t =
  Lsp.Location.create
    ~uri:(Lsp.DocumentUri.of_path r.left.file)
    ~range:(range r)

let severity : Spectec.Diagnostic.severity -> Lsp.DiagnosticSeverity.t =
  function
  | Spectec.Diagnostic.Error -> Lsp.DiagnosticSeverity.Error
  | Spectec.Diagnostic.Warning -> Lsp.DiagnosticSeverity.Warning
  | Spectec.Diagnostic.Info -> Lsp.DiagnosticSeverity.Information
  | Spectec.Diagnostic.Hint -> Lsp.DiagnosticSeverity.Hint

(* Include diagnostic details, related locations, and traces. *)

(* Limit trace size in hovers and diagnostics. *)
let max_trace_nodes = 32

(* Preserve CLI traversal order and nesting depth. *)
let flatten_trace (trace : Spectec.Diagnostic.trace_node list) :
    (int * region * string) list =
  let rec go depth (acc : (int * region * string) list)
      (node : Spectec.Diagnostic.trace_node) =
    List.fold_left
      (go (depth + 1))
      ((depth, node.region, node.message) :: acc)
      node.children
  in
  List.rev (List.fold_left (go 0) [] trace)

let take n xs = List.filteri (fun i _ -> i < n) xs
let indent depth = String.make (depth * 2) ' '

(* Indent traces; expose locations through related information. *)
let trace_text (nodes : (int * region * string) list) : string option =
  if nodes = [] then None
  else
    let line (depth, _, message) = indent depth ^ message in
    let shown = List.map line (take max_trace_nodes nodes) in
    let elided = List.length nodes - List.length shown in
    let shown =
      if elided <= 0 then shown
      else shown @ [ Printf.sprintf "... and %d more" elided ]
    in
    Some ("trace:\n" ^ String.concat "\n" shown)

(* Skip related entries without a source location. *)
let related_information (d : Spectec.Diagnostic.t) nodes :
    Lsp.DiagnosticRelatedInformation.t list =
  let entry region message =
    if region = no_region then None
    else
      Some
        (Lsp.DiagnosticRelatedInformation.create ~location:(location region)
           ~message)
  in
  let of_related (r : Spectec.Diagnostic.related) = entry r.region r.message in
  let of_trace (depth, region, message) =
    entry region (indent depth ^ message)
  in
  List.filter_map of_related d.related
  @ List.filter_map of_trace (take max_trace_nodes nodes)

let of_diagnostic (d : Spectec.Diagnostic.t) : Lsp.Diagnostic.t =
  let open Spectec.Diagnostic in
  let nodes = flatten_trace d.trace in
  let message =
    [
      Some d.message;
      Option.map (fun s -> "note: " ^ s) d.detail;
      trace_text nodes;
    ]
    |> List.filter_map Fun.id |> String.concat "\n\n"
  in
  let related = related_information d nodes in
  Lsp.Diagnostic.create ~range:(range d.region) ~severity:(severity d.severity)
    ~source:d.source
    ?code:(Option.map (fun c -> `String c) d.code)
    ?relatedInformation:(if related = [] then None else Some related)
    ~message:(`String message) ()

let canonical path = try Unix.realpath path with Unix.Unix_error _ -> path
let read_file path = In_channel.with_open_bin path In_channel.input_all

(* Match core's natural ordering; parsing depends on it. *)
let compare_natural a b =
  let is_digit c = '0' <= c && c <= '9' in
  let pieces s =
    String.fold_right
      (fun c pieces ->
        match pieces with
        | piece :: rest when is_digit c = is_digit piece.[0] ->
            (String.make 1 c ^ piece) :: rest
        | _ -> String.make 1 c :: pieces)
      s []
  in
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
  match List.compare compare_piece (pieces a) (pieces b) with
  | 0 -> String.compare a b
  | c -> c

(* Collect sibling files only; never recurse unmarked directories. *)
let siblings_of open_path =
  let dir = Filename.dirname open_path in
  match Sys.readdir dir with
  | exception Sys_error _ -> []
  | entries ->
      Array.sort compare_natural entries;
      entries |> Array.to_list
      |> List.filter (fun entry -> Filename.check_suffix entry ".spectec")
      |> List.map (Filename.concat dir)
      |> List.filter (fun path ->
             match Sys.is_directory path with
             | is_dir -> not is_dir
             | exception Sys_error _ -> false)

let spec_files_of open_path =
  let files =
    match Spectec.spec_root_of_file open_path with
    | Some root -> Spectec.collect_spec_files root
    | None -> siblings_of open_path
  in
  let files = List.map canonical files in
  if List.mem open_path files then files else files @ [ open_path ]

(* Prefer open buffers; unreadable files fail the spec. *)
let sources_of ?(buffers = []) ~open_path text =
  let buffers = (open_path, text) :: buffers in
  let read filename =
    match List.assoc_opt filename buffers with
    | Some contents -> Either.Right Spectec.{ filename; contents }
    | None -> (
        match read_file filename with
        | contents -> Either.Right Spectec.{ filename; contents }
        | exception Sys_error message -> Either.Left (filename, message))
  in
  match List.partition_map read (spec_files_of open_path) with
  | [], sources -> Ok sources
  | unreadable, _ -> Error unreadable

let group_by_file (ds : Spectec.Diagnostic.t list) :
    (string * Spectec.Diagnostic.t list) list =
  List.fold_left
    (fun acc (d : Spectec.Diagnostic.t) ->
      let file = canonical d.region.left.file in
      match List.assoc_opt file acc with
      | Some ds -> (file, d :: ds) :: List.remove_assoc file acc
      | None -> (file, [ d ]) :: acc)
    [] ds
  |> List.rev_map (fun (file, ds) -> (file, List.rev ds))

let unreadable ~open_path file message : Spectec.Diagnostic.t =
  Spectec.Diagnostic.error ~source:"io" (region_of_file open_path)
    (Printf.sprintf "cannot read spec file %s: %s" (Filename.basename file)
       message)

let internal_error ~open_path exn : Spectec.Diagnostic.t =
  Spectec.Diagnostic.error ~source:"internal" (region_of_file open_path)
    ("internal error: " ^ Printexc.to_string exn)

type analysis = {
  parsed : bool;
  diagnostics : (string * Lsp.Diagnostic.t list) list;
  index : Index.t;
  uses : Uses.t;
  il : Lang.Il.spec option;
}

let diagnose ?buffers ~open_path text =
  match sources_of ?buffers ~open_path text with
  | Error unreadable_files ->
      {
        diagnostics =
          [
            ( open_path,
              List.map
                (fun (filename, message) ->
                  of_diagnostic (unreadable ~open_path filename message))
                unreadable_files );
          ];
        parsed = false;
        index = Index.empty;
        uses = Uses.empty;
        il = None;
      }
  | Ok sources ->
      (* Preserve parsed symbols even when elaboration fails. *)
      let parsed = ref None in
      let elaborated, bag =
        Spectec.with_diagnostics (fun () ->
            let* spec = Spectec.parse_spec_sources sources in
            parsed := Some spec;
            Spectec.elaborate spec)
      in
      let index, uses =
        match !parsed with
        | Some spec ->
            let texts =
              List.map
                (fun (source : Spectec.spec_source) ->
                  (source.filename, source.contents))
                sources
            in
            ( Index.with_docs ~sources:texts (Index.of_spec spec),
              Uses.of_spec spec )
        | None -> (Index.empty, Uses.empty)
      in
      (* Enrich diagnostics using their own source file. *)
      let text_of file =
        match
          List.find_opt
            (fun (s : Spectec.spec_source) -> String.equal s.filename file)
            sources
        with
        | Some s -> s.contents
        | None -> ""
      in
      let diagnostics =
        Spectec.Diagnostic.Bag.to_sorted_list bag
        |> group_by_file
        |> List.map (fun (file, ds) ->
               let text = text_of file in
               ( file,
                 List.map
                   (fun d -> Reason.enrich ~index ~text (of_diagnostic d))
                   ds ))
      in
      {
        parsed = Option.is_some !parsed;
        diagnostics;
        index;
        uses;
        il = Result.to_option elaborated;
      }

let analyze ?buffers ~path text =
  let open_path = canonical path in
  try diagnose ?buffers ~open_path text
  with exn ->
    {
      parsed = false;
      diagnostics =
        [ (open_path, [ of_diagnostic (internal_error ~open_path exn) ]) ];
      index = Index.empty;
      uses = Uses.empty;
      il = None;
    }

let run ?buffers ~path text : (string * Lsp.Diagnostic.t list) list =
  (analyze ?buffers ~path text).diagnostics
