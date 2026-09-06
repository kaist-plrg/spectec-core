(* End-to-end smoke test for the server wiring: drive a spawned `spectecx-lsp`
   over LSP and confirm it serves a diagnostic of the right kind. main.ml
   checks the exact contents. *)

module Json = Yojson.Safe.Util

let request id method_ params =
  `Assoc
    [
      ("jsonrpc", `String "2.0");
      ("id", `Int id);
      ("method", `String method_);
      ("params", params);
    ]

let notification method_ params =
  `Assoc
    [
      ("jsonrpc", `String "2.0"); ("method", `String method_); ("params", params);
    ]

(* Snippet support is what the client we ship advertises, and it decides what
   completion inserts, so the handshake here says so too. *)
let initialize =
  request 1 "initialize"
    (`Assoc
       [
         ( "capabilities",
           `Assoc
             [
               ( "textDocument",
                 `Assoc
                   [
                     ( "completion",
                       `Assoc
                         [
                           ( "completionItem",
                             `Assoc [ ("snippetSupport", `Bool true) ] );
                         ] );
                   ] );
             ] );
       ])

let initialized = notification "initialized" (`Assoc [])
let shutdown = request 2 "shutdown" `Null
let exit_ = notification "exit" `Null

let at uri line character =
  `Assoc
    [
      ("textDocument", `Assoc [ ("uri", `String uri) ]);
      ("position", `Assoc [ ("line", `Int line); ("character", `Int character) ]);
    ]

let hover id uri line character =
  request id "textDocument/hover" (at uri line character)

let symbols id uri =
  request id "textDocument/documentSymbol"
    (`Assoc [ ("textDocument", `Assoc [ ("uri", `String uri) ]) ])

let references id uri line character =
  request id "textDocument/references"
    (`Assoc
       [
         ("textDocument", `Assoc [ ("uri", `String uri) ]);
         ( "position",
           `Assoc [ ("line", `Int line); ("character", `Int character) ] );
         ("context", `Assoc [ ("includeDeclaration", `Bool true) ]);
       ])

let did_change uri text version =
  notification "textDocument/didChange"
    (`Assoc
       [
         ( "textDocument",
           `Assoc [ ("uri", `String uri); ("version", `Int version) ] );
         ("contentChanges", `List [ `Assoc [ ("text", `String text) ] ]);
       ])

let did_save uri =
  notification "textDocument/didSave"
    (`Assoc [ ("textDocument", `Assoc [ ("uri", `String uri) ]) ])

let completion id uri line character =
  request id "textDocument/completion" (at uri line character)

let signature_help id uri line character =
  request id "textDocument/signatureHelp" (at uri line character)

let rename id uri line character new_name =
  request id "textDocument/rename"
    (`Assoc
       [
         ("textDocument", `Assoc [ ("uri", `String uri) ]);
         ( "position",
           `Assoc [ ("line", `Int line); ("character", `Int character) ] );
         ("newName", `String new_name);
       ])

let prepare_rename id uri line character =
  request id "textDocument/prepareRename" (at uri line character)

let il_preview id uri =
  request id "spectec/ilPreview"
    (`Assoc [ ("textDocument", `Assoc [ ("uri", `String uri) ]) ])

let did_open uri text =
  notification "textDocument/didOpen"
    (`Assoc
       [
         ( "textDocument",
           `Assoc
             [
               ("uri", `String uri);
               ("languageId", `String "spectec");
               ("version", `Int 1);
               ("text", `String text);
             ] );
       ])

let send channel message =
  let body = Yojson.Safe.to_string message in
  Printf.fprintf channel "Content-Length: %d\r\n\r\n%s" (String.length body)
    body;
  flush channel

let receive channel =
  let rec content_length so_far =
    match String.trim (input_line channel) with
    | "" -> so_far
    | header -> (
        match String.split_on_char ':' header with
        | [ name; value ]
          when String.lowercase_ascii (String.trim name) = "content-length" ->
            content_length (int_of_string (String.trim value))
        | _ -> content_length so_far)
  in
  Yojson.Safe.from_string (really_input_string channel (content_length 0))

(* Close-on-exec so the server does not inherit our pipe ends; closing our write
   end then gives it EOF on stdin, which is what makes it exit. *)
let roundtrip server_bin messages =
  let from_server_r, from_server_w = Unix.pipe ~cloexec:true () in
  let to_server_r, to_server_w = Unix.pipe ~cloexec:true () in
  let pid =
    Unix.create_process server_bin [| server_bin |] to_server_r from_server_w
      Unix.stderr
  in
  Unix.close to_server_r;
  Unix.close from_server_w;
  let from_server = Unix.in_channel_of_descr from_server_r in
  let to_server = Unix.out_channel_of_descr to_server_w in
  List.iter (send to_server) messages;
  close_out to_server;
  let rec collect replies =
    match receive from_server with
    | reply -> collect (reply :: replies)
    | exception End_of_file -> List.rev replies
  in
  let replies = collect [] in
  close_in from_server;
  ignore (Unix.waitpid [] pid);
  replies

let () =
  Sys.set_signal Sys.sigalrm
    (Sys.Signal_handle
       (fun _ ->
         prerr_endline "e2e: timed out";
         exit 2));
  ignore (Unix.alarm 10);
  let uri = "file:///e2e.spectec" in
  (* A second document, kept elaborable: the fixture above deliberately does not
     elaborate, and a preview of it would only ever be the empty stale one. *)
  let preview_uri = "file:///e2e-preview.spectec" in
  let rename_uri = "file:///e2e-rename.spectec" in
  (* [n] is declared on line 3 and used on line 4; [EBin] is a case of [expr]
     declared on line 7 and used on line 10, inside a dashed rule name.

     The comments are part of the fixture: [addr] carries one on the lines above
     it and [n] and [EBin] carry one on their own line, so hover has all three
     to recover. The one on [addr] runs to two lines, which have to survive as
     two lines. *)
  let text =
    ";; A location in the store.\n\
     ;; Addresses are opaque.\n\
     syntax addr = nat\n\
     var n : addr ;; where a value lives\n\
     syntax t = n\n\
     syntax expr =\n\
    \  | ENum int\n\
    \  | EBin expr expr ;; left, right\n\
     relation Eval: |- expr\n\
     rule Eval/bin-add:\n\
    \  |- EBin (ENum n) (ENum n)\n"
  in
  let replies =
    roundtrip Sys.argv.(1)
      [
        initialize;
        initialized;
        did_open uri text;
        hover 3 uri 4 11;
        symbols 4 uri;
        (* the [EBin] use on the last line: its case declaration *)
        hover 5 uri 10 6;
        (* the dashed rule name, from the half after the slash *)
        hover 6 uri 9 12;
        (* [EBin]: declared on line 7, used on line 10 *)
        references 7 uri 10 6;
        (* completing [E] on line 10, after the turnstile: cases and types *)
        completion 8 uri 10 6;
        (* Start a premise. The half-written line does not parse, which is the
           normal state mid-edit -- and the reason completion answers from the
           tables of the last save rather than from this text. *)
        did_change uri (text ^ "  --") 2;
        (* Right after the [--], with no space: the candidates have to carry one,
           or the premise comes out as [--Eval]. *)
        completion 9 uri 11 4;
        (* [addr] in [var n : addr], whose declaration is documented by the
           comment sitting on the line above it. *)
        hover 10 uri 3 8;
        (* Put the buffer back, then ask for a hint from inside the [ENum] on
           the last line -- the innermost application, not the [EBin] around
           it. Column 17 is just after its [(]. *)
        did_change uri text 4;
        signature_help 11 uri 10 17;
        (* [EBin] is declared on line 7 and used on line 10, so renaming it from
           the use has to reach both. *)
        prepare_rename 14 uri 10 6;
        rename 15 uri 10 6 "EPair";
        (* A third document, to pin the subscript case: [e_1] is a mention of
           [e], so renaming [e] must rewrite the base and leave [_1] alone. *)
        did_open rename_uri
          "syntax t = nat\nvar e : t\nrelation R: |- t\nrule R/one:\n  |- e_1\n";
        rename 16 rename_uri 1 4 "x";
        did_open preview_uri "syntax foo =\n  | FOO\n";
        il_preview 12 preview_uri;
        (* The buffer stops elaborating: the render has to survive it. *)
        did_change preview_uri "syntax foo = undeclared\n" 2;
        il_preview 13 preview_uri;
        (* An edit alone must not re-check: analysing means elaborating the
           whole spec, so it waits for the save. *)
        did_change uri (text ^ "syntax broken = nope\n") 3;
        did_save uri;
        shutdown;
        exit_;
      ]
  in
  let result_of id =
    List.find_map
      (fun reply ->
        if Json.member "id" reply = `Int id then
          Some (Json.member "result" reply)
        else None)
      replies
  in
  let answered_initialize =
    match result_of 1 with Some r when r <> `Null -> true | _ -> false
  in
  Printf.printf "## end-to-end (spectecx-lsp over stdio)\n";
  Printf.printf "initialize: %s\n"
    (if answered_initialize then "ok" else "MISSING");
  (match result_of 1 with
  | Some result ->
      let capabilities = Json.member "capabilities" result in
      List.iter
        (fun name ->
          Printf.printf "capability %s: %s\n" name
            (Yojson.Safe.to_string (Json.member name capabilities)))
        [
          "hoverProvider";
          "definitionProvider";
          "documentSymbolProvider";
          "referencesProvider";
          "signatureHelpProvider";
          "experimental";
        ]
  | None -> print_endline "capabilities: MISSING");
  List.iter
    (fun (label, id) ->
      match result_of id with
      | Some (`Assoc _ as result) ->
          let value = Json.member "value" (Json.member "contents" result) in
          Printf.printf "hover %s: %s\n" label (Yojson.Safe.to_string value)
      | _ -> Printf.printf "hover %s: none\n" label)
    [ ("var", 3); ("case", 5); ("rule", 6); ("syntax", 10) ];
  (match result_of 4 with
  | Some (`List items) ->
      Printf.printf "symbols: %s\n"
        (String.concat ", "
           (List.map
              (fun item -> Json.to_string (Json.member "name" item))
              items))
  | _ -> print_endline "symbols: none");
  (match result_of 8 with
  | Some (`Assoc _ as result) ->
      let items = Json.to_list (Json.member "items" result) in
      Printf.printf "completion: %s\n"
        (String.concat ", "
           (List.map
              (fun item -> Json.to_string (Json.member "label" item))
              items))
  | _ -> print_endline "completion: none");
  (match result_of 9 with
  | Some (`Assoc _ as result) ->
      let items = Json.to_list (Json.member "items" result) in
      Printf.printf "completion after `--`: %s\n"
        (String.concat ", "
           (List.map
              (fun item ->
                Printf.sprintf "%s inserts %s"
                  (Json.to_string (Json.member "label" item))
                  (Yojson.Safe.to_string
                     (Json.member "newText" (Json.member "textEdit" item))))
              items))
  | _ -> print_endline "completion after `--`: none");
  (match result_of 11 with
  | Some (`Assoc _ as result) -> (
      match Json.to_list (Json.member "signatures" result) with
      | [] -> print_endline "signature help: none"
      | signature :: _ ->
          Printf.printf "signature help: %s, argument %s\n"
            (Yojson.Safe.to_string (Json.member "label" signature))
            (Yojson.Safe.to_string (Json.member "activeParameter" result)))
  | _ -> print_endline "signature help: none");
  List.iter
    (fun (label, id) ->
      match result_of id with
      | Some (`Assoc _ as result) ->
          Printf.printf "il preview %s: stale %s, %d definition(s), %s\n" label
            (Yojson.Safe.to_string (Json.member "stale" result))
            (List.length (Json.to_list (Json.member "entries" result)))
            (Yojson.Safe.to_string (Json.member "text" result))
      | _ -> Printf.printf "il preview %s: none\n" label)
    [ ("elaborates", 12); ("after a bad edit", 13) ];
  (match result_of 7 with
  | Some (`List locations) ->
      Printf.printf "references: %s\n"
        (String.concat ", "
           (List.map
              (fun location ->
                let start =
                  Json.member "start" (Json.member "range" location)
                in
                Printf.sprintf "line %s"
                  (Yojson.Safe.to_string (Json.member "line" start)))
              locations))
  | _ -> print_endline "references: none");
  (match result_of 14 with
  | Some (`Assoc _ as r) ->
      let start = Json.member "start" r and end_ = Json.member "end" r in
      Printf.printf "prepareRename: line %s chars %s-%s\n"
        (Yojson.Safe.to_string (Json.member "line" start))
        (Yojson.Safe.to_string (Json.member "character" start))
        (Yojson.Safe.to_string (Json.member "character" end_))
  | _ -> print_endline "prepareRename: none");
  (match result_of 15 with
  | Some result -> (
      match Json.member "changes" result with
      | `Assoc files ->
          List.iter
            (fun (file, edits) ->
              Printf.printf "rename %s: %s\n" (Filename.basename file)
                (String.concat ", "
                   (List.map
                      (fun edit ->
                        Printf.sprintf "line %s -> %s"
                          (Yojson.Safe.to_string
                             (Json.member "line"
                                (Json.member "start" (Json.member "range" edit))))
                          (Yojson.Safe.to_string (Json.member "newText" edit)))
                      (Json.to_list edits))))
            files
      | _ -> print_endline "rename: no changes")
  | _ -> print_endline "rename: none");
  (match result_of 16 with
  | Some result -> (
      match Json.member "changes" result with
      | `Assoc files ->
          List.iter
            (fun (_, edits) ->
              List.iter
                (fun edit ->
                  let r = Json.member "range" edit in
                  Printf.printf "rename subscript: line %s chars %s-%s -> %s\n"
                    (Yojson.Safe.to_string
                       (Json.member "line" (Json.member "start" r)))
                    (Yojson.Safe.to_string
                       (Json.member "character" (Json.member "start" r)))
                    (Yojson.Safe.to_string
                       (Json.member "character" (Json.member "end" r)))
                    (Yojson.Safe.to_string (Json.member "newText" edit)))
                (Json.to_list edits))
            files
      | _ -> print_endline "rename subscript: no changes")
  | _ -> print_endline "rename subscript: none");
  List.iter
    (fun reply ->
      if Json.member "method" reply = `String "textDocument/publishDiagnostics"
      then
        let params = Json.member "params" reply in
        let uri = Json.to_string (Json.member "uri" params) in
        let diagnostics = Json.to_list (Json.member "diagnostics" params) in
        let code diagnostic =
          match Json.member "code" diagnostic with `String s -> s | _ -> "?"
        in
        let codes = String.concat ", " (List.map code diagnostics) in
        Printf.printf "publishDiagnostics %s: %d diagnostic(s) [%s]\n" uri
          (List.length diagnostics) codes)
    replies
