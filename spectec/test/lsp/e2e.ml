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

let initialize = request 1 "initialize" (`Assoc [ ("capabilities", `Assoc []) ])
let initialized = notification "initialized" (`Assoc [])
let shutdown = request 2 "shutdown" `Null
let exit_ = notification "exit" `Null

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
  let replies =
    roundtrip Sys.argv.(1)
      [
        initialize;
        initialized;
        did_open uri "syntax t = foo\n";
        shutdown;
        exit_;
      ]
  in
  let answered_initialize =
    List.exists
      (fun reply ->
        Json.member "id" reply = `Int 1 && Json.member "result" reply <> `Null)
      replies
  in
  Printf.printf "## end-to-end (spectecx-lsp over stdio)\n";
  Printf.printf "initialize: %s\n"
    (if answered_initialize then "ok" else "MISSING");
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
