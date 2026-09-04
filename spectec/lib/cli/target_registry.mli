type entry = { name : string; command : Core.Command.t }

exception Registration_error of string

(** [register target_cli] registers [target_cli] during the active
    [with_plugin_registration] call. *)
val register : (module Target_cli.S) -> unit

(** [with_plugin_registration ~expected_name load] returns the single target
    that [load] registers as [expected_name]. A failed [load] leaves the
    registry unchanged. *)
val with_plugin_registration : expected_name:string -> (unit -> unit) -> entry
