(** [run ~origin text] parses and elaborates [text] in memory and returns the
    resulting errors and warnings as LSP diagnostics. [origin] labels the buffer
    in diagnostic messages. *)

val run : origin:string -> string -> Linol_eio.Diagnostic.t list
