(** Static spec declarations for instrumentation handlers.

    {!S} describes a handler spec statically — its CLI parameters, how to parse
    flags into a configured {!Instrumentation_config.Handler_config.t}, and
    optionally how to checkpoint state. {!Instrumentation_api.Handler.S} is the
    counterpart: what the interpreter calls at runtime. The two are split so the
    CLI can enumerate specs without constructing handlers. *)

(** Checkpoint serialization hooks for handlers with persistent state. *)
type checkpoint_ops = {
  snapshot : unit -> bytes;
  restore : bytes -> unit;
  merge : bytes -> bytes -> bytes;
}

module type S = sig
  val name : string

  (** The interpreter modes this handler can observe. *)
  val modes : [ `IL | `SL | `PL ] list

  (** [(param_name, doc)] entries for CLI help. *)
  val params : (string * string) list

  val parse :
    (string * string option) list ->
    Instrumentation_config.Handler_config.t option

  val checkpoint : checkpoint_ops option
end

(** First-class module wrapper so handler specs can live in a heterogeneous
    list. *)
type t = (module S)
