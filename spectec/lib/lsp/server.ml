open Linol_eio
open Common.Source

(* Cache whole-spec analyses per document for queries. *)
let analyses : (DocumentUri.t, Check.analysis) Hashtbl.t = Hashtbl.create 8
let contents : (DocumentUri.t, string) Hashtbl.t = Hashtbl.create 8

(* Render previews on request, retaining successful results. *)
let previews = Preview.create ()

(* Track diagnostics per document to clear obsolete markers. *)
let published : (DocumentUri.t, string list) Hashtbl.t = Hashtbl.create 8

(* Include other open buffers for unsaved cross-file references. *)
let buffers_except path =
  Hashtbl.fold
    (fun uri text acc ->
      let file = Check.canonical (DocumentUri.to_path uri) in
      if String.equal file path then acc else (file, text) :: acc)
    contents []

(* Publish directly; linol's helper targets only one document. *)
let send_for ~(notify_back : Jsonrpc2.notify_back) file diagnostics =
  let uri = DocumentUri.of_path file in
  notify_back#send_notification
    (Linol.Lsp.Server_notification.PublishDiagnostics
       (PublishDiagnosticsParams.create ~uri ~diagnostics ()))

(* Throttle expensive analyses; saves always trigger analysis. *)
let analysis_budget = 0.150

(* URI maps to analysis completion time and duration. *)
let timings : (DocumentUri.t, float * float) Hashtbl.t = Hashtbl.create 8

let worth_analysing uri =
  match Hashtbl.find_opt timings uri with
  | None -> true
  | Some (finished_at, duration) ->
      duration <= analysis_budget
      || Unix.gettimeofday () -. finished_at >= 4.0 *. duration

let publish ~(notify_back : Jsonrpc2.notify_back) uri text =
  let path = Check.canonical (DocumentUri.to_path uri) in
  Hashtbl.replace contents uri text;
  let started_at = Unix.gettimeofday () in
  let fresh = Check.analyze ~buffers:(buffers_except path) ~path text in
  let finished_at = Unix.gettimeofday () in
  Hashtbl.replace timings uri (finished_at, finished_at -. started_at);
  (* Keep previous symbol tables when parsing fails. *)
  let previous = Hashtbl.find_opt analyses uri in
  let analysis =
    if fresh.parsed then fresh
    else
      match previous with
      | Some previous ->
          { fresh with index = previous.index; uses = previous.uses }
      | None -> fresh
  in
  (* Retain previous types independently when elaboration fails. *)
  let analysis =
    match (analysis.il, previous) with
    | None, Some previous -> { analysis with il = previous.il }
    | _ -> analysis
  in
  Hashtbl.replace analyses uri analysis;
  (* Publish empty diagnostics too, clearing resolved document errors. *)
  let groups =
    if List.mem_assoc path analysis.diagnostics then analysis.diagnostics
    else (path, []) :: analysis.diagnostics
  in
  let reported = List.map fst groups in
  Hashtbl.find_opt published uri
  |> Option.value ~default:[]
  |> List.iter (fun file ->
         if not (List.mem file reported) then send_for ~notify_back file []);
  List.iter
    (fun (file, diagnostics) -> send_for ~notify_back file diagnostics)
    groups;
  Hashtbl.replace published uri
    (List.filter_map
       (fun (file, ds) -> if ds = [] then None else Some file)
       groups)

let position (p : pos) : Position.t =
  Position.create ~line:(max 0 (p.line - 1)) ~character:(max 0 p.column)

let range (r : region) : Range.t =
  Range.create ~start:(position r.left) ~end_:(position r.right)

let is_ident_char c =
  ('a' <= c && c <= 'z')
  || ('A' <= c && c <= 'Z')
  || ('0' <= c && c <= '9')
  || c = '_' || c = '\''

(* Allow dashes after rule slashes without swallowing subtraction. *)
let is_rule_char c = is_ident_char c || c = '-'

(* Expand identifiers to complete function or rule names. *)
let word_span_at (text : string) (pos : Position.t) :
    (string * int * int) option =
  let lines = String.split_on_char '\n' text in
  match List.nth_opt lines pos.line with
  | None -> None
  | Some line -> (
      let n = String.length line in
      let at = min pos.character (max 0 (n - 1)) in
      if n = 0 || not (is_ident_char line.[at]) then None
      else
        let scan_left ok from =
          let i = ref from in
          while !i > 0 && ok line.[!i - 1] do
            decr i
          done;
          !i
        in
        let scan_right ok from =
          let i = ref from in
          while !i < n - 1 && ok line.[!i + 1] do
            incr i
          done;
          !i
        in
        let s = scan_left is_ident_char at
        and e = scan_right is_ident_char at in
        (* Recognise either side of a qualified rule name. *)
        let before_dashes = scan_left is_rule_char s in
        let rule_span =
          if e + 1 < n && line.[e + 1] = '/' then
            Some (s, scan_right is_rule_char (e + 1))
          else if before_dashes > 0 && line.[before_dashes - 1] = '/' then
            Some
              ( scan_left is_ident_char (before_dashes - 1),
                scan_right is_rule_char e )
          else None
        in
        match rule_span with
        | Some (start, stop) ->
            Some (String.sub line start (stop - start + 1), start, stop + 1)
        | None ->
            (* Keep the function sigil in the selected span. *)
            let start = if s > 0 && line.[s - 1] = '$' then s - 1 else s in
            Some (String.sub line start (e - start + 1), start, e + 1))

let word_at (text : string) (pos : Position.t) : string option =
  Option.map (fun (word, _, _) -> word) (word_span_at text pos)

let word_under uri pos =
  match Hashtbl.find_opt contents uri with
  | None -> None
  | Some text -> word_at text pos

let entries_at uri pos =
  match (Hashtbl.find_opt analyses uri, word_under uri pos) with
  | Some analysis, Some word -> Index.find analysis.index word
  | _ -> []

(* Use Markdown hard breaks to preserve source lines. *)
let hard_breaks text = text |> String.split_on_char '\n' |> String.concat "  \n"
let divider = "\n\n---\n\n"

let hover_markdown (entries : Index.entry list) word =
  let render (e : Index.entry) =
    let head =
      "```spectec\n" ^ e.signature ^ "\n```"
      ^ match e.doc with Some doc -> "\n\n" ^ hard_breaks doc | None -> ""
    in
    match e.detail with Some detail -> head ^ divider ^ detail | None -> head
  in
  let body = String.concat divider (List.map render entries) in
  (* Explain when hover resolves a subscripted base name. *)
  match entries with
  | entry :: _ when not (String.equal entry.name word) ->
      body ^ "\n\n`" ^ word ^ "` is `" ^ entry.name ^ "` with a subscript."
  | _ -> body

let location_of (region : region) =
  Location.create
    ~uri:(DocumentUri.of_path region.left.file)
    ~range:(range region)

(* Combine uses with declarations when the client requests. *)
let references uri pos ~include_declaration =
  match (Hashtbl.find_opt analyses uri, word_under uri pos) with
  | Some analysis, Some word ->
      let declarations =
        if include_declaration then
          Index.find analysis.index word
          |> List.map (fun (entry : Index.entry) -> entry.region)
        else []
      in
      let regions = declarations @ Uses.find analysis.uses word in
      (* Deduplicate regions shared by declarations and references. *)
      regions |> List.sort_uniq compare_region |> List.map location_of
  | _ -> []

(* Rename only verified name spans, preserving metavariable subscripts. *)

let text_of_file file =
  let uri = DocumentUri.of_path file in
  match Hashtbl.find_opt contents uri with
  | Some text -> Some text
  | None -> (
      match In_channel.with_open_bin file In_channel.input_all with
      | text -> Some text
      | exception Sys_error _ -> None)

let line_of text line = List.nth_opt (String.split_on_char '\n' text) line

let edit_range (region : region) old_name =
  let line_index = region.left.line - 1 in
  match text_of_file region.left.file with
  | None -> None
  | Some text -> (
      match line_of text line_index with
      | None -> None
      | Some line ->
          let start = region.left.column in
          let stop = min region.right.column (String.length line) in
          if start >= stop then None
          else
            let covered = String.sub line start (stop - start) in
            let n = String.length old_name in
            if String.equal covered old_name then Some (range region)
            else if
              String.length covered > n
              && String.equal (String.sub covered 0 n) old_name
              && covered.[n] = '_'
            then
              (* Keep the subscript: replace the base only. *)
              Some
                (Range.create
                   ~start:(Position.create ~line:line_index ~character:start)
                   ~end_:
                     (Position.create ~line:line_index ~character:(start + n)))
            else None)

let rename uri pos new_name =
  match (Hashtbl.find_opt analyses uri, word_under uri pos) with
  | Some analysis, Some word ->
      let declarations =
        Index.find analysis.index word
        |> List.map (fun (entry : Index.entry) -> entry.region)
      in
      let regions =
        declarations @ Uses.find analysis.uses word
        |> List.sort_uniq compare_region
      in
      let edits =
        List.filter_map
          (fun (region : region) ->
            Option.map
              (fun range ->
                (region.left.file, TextEdit.create ~range ~newText:new_name))
              (edit_range region word))
          regions
      in
      if edits = [] then WorkspaceEdit.create ()
      else
        let by_file =
          List.fold_left
            (fun acc (file, edit) ->
              match List.assoc_opt file acc with
              | Some es -> (file, edit :: es) :: List.remove_assoc file acc
              | None -> (file, [ edit ]) :: acc)
            [] edits
        in
        let changes =
          List.rev_map
            (fun (file, es) -> (DocumentUri.of_path file, List.rev es))
            by_file
        in
        WorkspaceEdit.create ~changes ()
  | _ -> WorkspaceEdit.create ()

(* Reject unavailable renames before prompting for replacement text. *)
let prepare_rename uri (pos : Position.t) =
  match (Hashtbl.find_opt analyses uri, Hashtbl.find_opt contents uri) with
  | Some analysis, Some text -> (
      match word_span_at text pos with
      | None -> None
      | Some (word, start, stop) ->
          (* Rename only indexed names to avoid partial edits. *)
          if Index.find analysis.index word = [] then None
          else
            Some
              (Range.create
                 ~start:(Position.create ~line:pos.line ~character:start)
                 ~end_:(Position.create ~line:pos.line ~character:stop)))
  | _ -> None

let line_at uri (pos : Position.t) =
  match Hashtbl.find_opt contents uri with
  | None -> None
  | Some text ->
      Some
        (Option.value
           (List.nth_opt (String.split_on_char '\n' text) pos.line)
           ~default:"")

let preceding_lines uri (pos : Position.t) =
  match Hashtbl.find_opt contents uri with
  | None -> []
  | Some text ->
      String.split_on_char '\n' text |> List.filteri (fun i _ -> i < pos.line)

(* Cache type tables by retained IL object identity. *)
let typings : (DocumentUri.t, Lang.Il.spec option * Typing.t) Hashtbl.t =
  Hashtbl.create 8

let typing_for uri (analysis : Check.analysis) =
  match Hashtbl.find_opt typings uri with
  | Some (il, typing) when il == analysis.il -> typing
  | _ ->
      let typing = Typing.of_il analysis.il in
      Hashtbl.replace typings uri (analysis.il, typing);
      typing

let signature_help uri (pos : Position.t) =
  match (Hashtbl.find_opt analyses uri, line_at uri pos) with
  | Some analysis, Some line ->
      Signature.at ~index:analysis.index ~line ~character:pos.character
  | _ -> SignatureHelp.create ~signatures:[] ()

(* Preview requests trigger rendering independently of document edits. *)

let json_of_position (p : pos) =
  `Assoc
    [
      ("line", `Int (max 0 (p.line - 1))); ("character", `Int (max 0 p.column));
    ]

let json_of_region (r : region) =
  `Assoc
    [
      ("uri", `String (DocumentUri.to_string (DocumentUri.of_path r.left.file)));
      ( "range",
        `Assoc
          [
            ("start", json_of_position r.left); ("end", json_of_position r.right);
          ] );
    ]

let json_of_render (render : Preview.render) =
  `Assoc
    [
      ("text", `String render.text);
      ("stale", `Bool render.stale);
      ( "reason",
        match render.reason with
        | None -> `Null
        | Some reason ->
            `Assoc
              [
                ("message", `String reason.message);
                ("region", json_of_region reason.region);
              ] );
      ( "entries",
        `List
          (List.map
             (fun (entry : Preview.entry) ->
               `Assoc
                 [
                   ("line", `Int entry.line);
                   ("depth", `Int entry.depth);
                   ("region", json_of_region entry.region);
                 ])
             render.entries) );
    ]

let preview uri stage =
  let path = DocumentUri.to_path uri in
  (* Prefer open buffers; otherwise load previews from disk. *)
  let text =
    match Hashtbl.find_opt contents uri with
    | Some text -> Some text
    | None -> (
        match In_channel.with_open_bin path In_channel.input_all with
        | text -> Some text
        | exception Sys_error _ -> None)
  in
  match text with
  | None -> `Null
  | Some text ->
      json_of_render (Preview.render previews ~stage ~open_path:path ~text)

let uri_of_params params =
  match params with
  | Some (`Assoc fields) -> (
      match List.assoc_opt "textDocument" fields with
      | Some (`Assoc document) -> (
          match List.assoc_opt "uri" document with
          | Some (`String uri) -> Some (DocumentUri.t_of_yojson (`String uri))
          | _ -> None)
      | _ -> None)
  | _ -> None

(* Default to IL; reject unrecognised requested stages. *)
let stage_of_params params =
  match params with
  | Some (`Assoc fields) -> (
      match List.assoc_opt "stage" fields with
      | None | Some `Null -> Some Preview.Il
      | Some (`String name) -> Preview.stage_of_string name
      | _ -> None)
  | _ -> Some Preview.Il

let symbol_kind : Index.kind -> SymbolKind.t = function
  | Index.Syntax -> SymbolKind.Class
  | Index.Var -> SymbolKind.Variable
  | Index.Relation -> SymbolKind.Interface
  | Index.Dec -> SymbolKind.Function
  | Index.Rule -> SymbolKind.Method
  | Index.Case -> SymbolKind.EnumMember
  | Index.Field -> SymbolKind.Field

let server =
  object
    inherit Jsonrpc2.server
    method spawn_query_handler f = spawn f
    method! config_hover = Some (`Bool true)
    method! config_definition = Some (`Bool true)
    method! config_symbol = Some (`Bool true)

    (* Trigger completion for function sigils and premise dashes. *)
    method! config_completion =
      Some
        (CompletionOptions.create ~triggerCharacters:[ "$"; "-" ]
           ~resolveProvider:false ())

    (* Advertise manually; argument separators trigger and retrigger hints. *)
    method! config_modify_capabilities capabilities =
      {
        capabilities with
        referencesProvider = Some (`Bool true);
        (* Enable rename validation before clients prompt users. *)
        renameProvider =
          Some (`RenameOptions (RenameOptions.create ~prepareProvider:true ()));
        (* Advertise the custom preview request and supported stages. *)
        experimental =
          Some
            (`Assoc
               [
                 ("preview", `List [ `String "il"; `String "sl"; `String "pl" ]);
                 (* Retain the original request name for IL previews. *)
                 ("ilPreview", `Bool true);
               ]);
        signatureHelpProvider =
          Some
            (SignatureHelpOptions.create ~triggerCharacters:[ " "; "("; "," ]
               ~retriggerCharacters:[ " "; ","; ")" ] ());
      }

    method on_notif_doc_did_open ~notify_back (doc : TextDocumentItem.t)
        ~content =
      publish ~notify_back doc.uri content

    (* Keep cursor text current even when analysis waits. *)
    method on_notif_doc_did_change ~notify_back
        (doc : VersionedTextDocumentIdentifier.t) _changes ~old_content:_
        ~new_content =
      Hashtbl.replace contents doc.uri new_content;
      if worth_analysing doc.uri then publish ~notify_back doc.uri new_content

    method! on_notif_doc_did_save ~notify_back
        (params : DidSaveTextDocumentParams.t) =
      let uri = params.textDocument.uri in
      (* Reuse the current buffer because [includeText] is disabled. *)
      match
        match params.text with
        | Some text -> Some text
        | None -> Hashtbl.find_opt contents uri
      with
      | Some text -> publish ~notify_back uri text
      | None -> ()

    method on_notif_doc_did_close ~notify_back:_ doc =
      Hashtbl.remove analyses doc.uri;
      Hashtbl.remove typings doc.uri;
      Hashtbl.remove contents doc.uri;
      Hashtbl.remove timings doc.uri;
      (* Keep published diagnostics: they belong to the spec. *)
      Hashtbl.remove published doc.uri

    method! on_req_completion ~notify_back:_ ~id:_ ~uri ~pos ~ctx:_
        ~workDoneToken:_ ~partialResultToken:_ _ =
      match (Hashtbl.find_opt analyses uri, line_at uri pos) with
      | Some analysis, Some line ->
          Some
            (`CompletionList
               (Complete.in_context ~index:analysis.index
                  ~typing:(typing_for uri analysis)
                  ~preceding:(preceding_lines uri pos) ~line
                  ~character:pos.character))
      | _ -> None

    method! on_req_hover ~notify_back:_ ~id:_ ~uri ~pos ~workDoneToken:_ _ =
      match (entries_at uri pos, Hashtbl.find_opt contents uri) with
      | [], _ | _, None -> None
      | entries, Some text ->
          let word = Option.value (word_at text pos) ~default:"" in
          let value = hover_markdown entries word in
          Some
            (Hover.create
               ~contents:
                 (`MarkupContent
                    (MarkupContent.create ~kind:MarkupKind.Markdown ~value))
               ~range:(range (List.hd entries).region)
               ())

    method! on_req_definition ~notify_back:_ ~id:_ ~uri ~pos ~workDoneToken:_
        ~partialResultToken:_ _ =
      match entries_at uri pos with
      | [] -> None
      | entries ->
          Some
            (`Location
               (List.map
                  (fun (entry : Index.entry) ->
                    Location.create
                      ~uri:(DocumentUri.of_path entry.region.left.file)
                      ~range:(range entry.region))
                  entries))

    (* Handle custom protocol methods from undecoded requests. *)
    method! on_unknown_request ~notify_back:_ ~server_request:_ ~id:_ meth
        params =
      match meth with
      | "spectec/preview" -> (
          match (uri_of_params params, stage_of_params params) with
          | Some uri, Some stage -> preview uri stage
          | _ -> `Null)
      | "spectec/ilPreview" -> (
          match uri_of_params params with
          | Some uri -> preview uri Preview.Il
          | None -> `Null)
      | _ -> failwith ("unhandled request: " ^ meth)

    (* Handle modelled requests already decoded by linol. *)
    method! on_request_unhandled : type r.
        notify_back:_ -> id:_ -> r Linol_lsp.Client_request.t -> r =
      fun ~notify_back:_ ~id:_ request ->
        match request with
        | Linol_lsp.Client_request.TextDocumentReferences params ->
            Some
              (references params.textDocument.uri params.position
                 ~include_declaration:params.context.includeDeclaration)
        | Linol_lsp.Client_request.SignatureHelp params ->
            signature_help params.textDocument.uri params.position
        | Linol_lsp.Client_request.TextDocumentRename params ->
            rename params.textDocument.uri params.position params.newName
        | Linol_lsp.Client_request.TextDocumentPrepareRename params ->
            prepare_rename params.textDocument.uri params.position
        | _ -> failwith "unhandled request"

    method! on_req_symbol ~notify_back:_ ~id:_ ~uri ~workDoneToken:_
        ~partialResultToken:_ () =
      match Hashtbl.find_opt analyses uri with
      | None -> None
      | Some { index; _ } ->
          let file = DocumentUri.to_path uri in
          Some
            (`SymbolInformation
               (Index.in_file index file
               |> List.map (fun (entry : Index.entry) ->
                      SymbolInformation.create ~name:entry.name
                        ~kind:(symbol_kind entry.kind)
                        ~location:
                          (Location.create ~uri ~range:(range entry.region))
                        ())))
  end

let serve () =
  Eio_main.run @@ fun env -> Jsonrpc2.run (Jsonrpc2.create_stdio ~env server)
