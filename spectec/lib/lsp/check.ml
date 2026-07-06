open Common.Source
module Lsp = Linol_eio

let ( let* ) = Result.bind

(* Source line is 1-based, column 0-based; LSP is 0-based on both. *)
let position (p : pos) : Lsp.Position.t =
  Lsp.Position.create ~line:(max 0 (p.line - 1)) ~character:(max 0 p.column)

let range (r : region) : Lsp.Range.t =
  Lsp.Range.create ~start:(position r.left) ~end_:(position r.right)

let severity : Spectec.Diagnostic.severity -> Lsp.DiagnosticSeverity.t =
  function
  | Spectec.Diagnostic.Error -> Lsp.DiagnosticSeverity.Error
  | Spectec.Diagnostic.Warning -> Lsp.DiagnosticSeverity.Warning
  | Spectec.Diagnostic.Info -> Lsp.DiagnosticSeverity.Information
  | Spectec.Diagnostic.Hint -> Lsp.DiagnosticSeverity.Hint

let of_diagnostic (d : Spectec.Diagnostic.t) : Lsp.Diagnostic.t =
  let open Spectec.Diagnostic in
  Lsp.Diagnostic.create ~range:(range d.region) ~severity:(severity d.severity)
    ~source:d.source
    ?code:(Option.map (fun c -> `String c) d.code)
    ~message:(`String d.message) ()

let canonical path = try Unix.realpath path with Unix.Unix_error _ -> path
let read_file path = In_channel.with_open_bin path In_channel.input_all

let spec_files_of open_path =
  let files =
    match Spectec.spec_root_of_file open_path with
    | Some root -> List.map canonical (Spectec.collect_spec_files root)
    | None -> [ open_path ]
  in
  if List.mem open_path files then files else files @ [ open_path ]

let retag_as ~open_path (d : Spectec.Diagnostic.t) : Spectec.Diagnostic.t =
  {
    d with
    region = region_of_file open_path;
    message =
      Printf.sprintf "in %s: %s"
        (Filename.basename d.region.left.file)
        d.message;
  }

(* A sibling error fails the whole elaboration, so this file shows it too; a
   sibling warning is another file's concern. *)
let report_for ~open_path (d : Spectec.Diagnostic.t) : Lsp.Diagnostic.t option =
  if String.equal d.region.left.file open_path then Some (of_diagnostic d)
  else
    match d.severity with
    | Spectec.Diagnostic.Error -> Some (of_diagnostic (retag_as ~open_path d))
    | _ -> None

let unreadable ~open_path file message : Spectec.Diagnostic.t =
  Spectec.Diagnostic.error ~source:"io" (region_of_file open_path)
    (Printf.sprintf "cannot read spec file %s: %s" (Filename.basename file)
       message)

let internal_error ~open_path exn : Spectec.Diagnostic.t =
  Spectec.Diagnostic.error ~source:"internal" (region_of_file open_path)
    ("internal error: " ^ Printexc.to_string exn)

let diagnose ~open_path text =
  let read file =
    if String.equal file open_path then Either.Right (file, text)
    else
      match read_file file with
      | source -> Either.Right (file, source)
      | exception Sys_error message -> Either.Left (file, message)
  in
  let unreadable_files, sources =
    List.partition_map read (spec_files_of open_path)
  in
  match unreadable_files with
  | _ :: _ ->
      List.map
        (fun (file, message) ->
          of_diagnostic (unreadable ~open_path file message))
        unreadable_files
  | [] ->
      let _, bag =
        Spectec.with_diagnostics (fun () ->
            let* spec = Spectec.parse_spec_sources sources in
            Spectec.elaborate spec)
      in
      List.filter_map (report_for ~open_path)
        (Spectec.Diagnostic.Bag.to_sorted_list bag)

let run ~path text : Lsp.Diagnostic.t list =
  let open_path = canonical path in
  try diagnose ~open_path text
  with exn -> [ of_diagnostic (internal_error ~open_path exn) ]
