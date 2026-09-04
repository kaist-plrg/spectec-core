(** [commands ()] loads the installed target plugins and returns one command for
    each plugin. If loading fails, [commands] returns an unavailable command for
    that plugin. *)
val commands : unit -> (string * Core.Command.t) list
