(** Run the SpecTecX language server: speak LSP over stdin and stdout, and
    publish diagnostics for each opened or edited document. *)

val serve : unit -> unit
