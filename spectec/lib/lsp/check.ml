open Common.Source
module Lsp = Linol_eio

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

let of_diag (d : Spectec.Diagnostic.t) : Lsp.Diagnostic.t =
  let open Spectec.Diagnostic in
  Lsp.Diagnostic.create ~range:(range d.region) ~severity:(severity d.severity)
    ~source:d.source
    ?code:(Option.map (fun c -> `String c) d.code)
    ~message:(`String d.message) ()

let run ~origin text : Lsp.Diagnostic.t list =
  let _, bag =
    Spectec.with_diagnostics (fun () ->
        match Spectec.parse_spec_string ~origin text with
        | Error e -> Error e
        | Ok el -> Spectec.elaborate el)
  in
  List.map of_diag (Spectec.Diagnostic.Bag.to_sorted_list bag)
