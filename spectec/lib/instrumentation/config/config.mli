(** Active instrumentation configuration — the list of configured handlers built
    by parsing CLI flags against {!Instrumentation_spec.Spec.S}. *)

type t = Handler_config.t list

val default : t

(** Register every configured handler's static dependencies so
    {!Instrumentation_static.Static.init_all} sees them. *)
val register_static_dependencies : t -> unit

(** Extract the configured runtime handlers. *)
val handlers : t -> (module Instrumentation_api.Handler.S) list

(** Whether a handler with the given [name] is configured. *)
val has_handler : t -> name:string -> bool

(** [Error msg] when any configured handler is incompatible with the chosen
    interpreter mode ([sl_mode] selects SL vs IL). *)
val validate_mode : t -> sl_mode:bool -> (unit, string) result

val close_outputs : t -> unit
