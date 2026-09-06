(** Show the current application's signature and active argument. *)

(** Infer applications from text; return empty outside calls. *)
val at :
  index:Index.t -> line:string -> character:int -> Linol_eio.SignatureHelp.t
