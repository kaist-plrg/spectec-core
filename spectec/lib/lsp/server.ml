open Linol_eio

let publish ~(notify_back : Jsonrpc2.notify_back) uri text =
  notify_back#set_uri uri;
  notify_back#send_diagnostic (Check.run ~path:(DocumentUri.to_path uri) text)

let server =
  object
    inherit Jsonrpc2.server
    method spawn_query_handler f = spawn f

    method on_notif_doc_did_open ~notify_back (doc : TextDocumentItem.t)
        ~content =
      publish ~notify_back doc.uri content

    method on_notif_doc_did_change ~notify_back
        (doc : VersionedTextDocumentIdentifier.t) _changes ~old_content:_
        ~new_content =
      publish ~notify_back doc.uri new_content

    method on_notif_doc_did_close ~notify_back:_ _doc = ()
  end

let serve () =
  Eio_main.run @@ fun env -> Jsonrpc2.run (Jsonrpc2.create_stdio ~env server)
