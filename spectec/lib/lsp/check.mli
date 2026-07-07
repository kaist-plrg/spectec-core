(** [run ~path text] returns LSP diagnostics for the document at [path], with
    [text] as its current contents. The document is checked as part of its whole
    spec (the other [.spectec] files in its [*.spec]-marked directory, read from
    disk), so cross-file references resolve. *)

val run : path:string -> string -> Linol_eio.Diagnostic.t list
