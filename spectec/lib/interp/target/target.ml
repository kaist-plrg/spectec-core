module type S = sig
  val builtins : (string * Builtins.Define.t) list

  (** [with_state f] initializes target state, runs [f], and restores the
      previous state when [f] returns or raises. *)
  val with_state : (unit -> 'a) -> 'a

  val is_impure_func : string -> bool
  val is_impure_rel : string -> bool
  val state_version : int ref
end
