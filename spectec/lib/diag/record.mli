(** A single compiler diagnostic — error, warning, or informational note.

    This module defines the structured diagnostic type used across all compiler
    passes (parse, elaborate, interpret). Both the CLI and a future LSP server
    render diagnostics from this representation. *)

open Common.Source

(** {1 Core types} *)

type severity = Error | Warning | Info | Hint
type related = { region : region; message : string }
type fix = { message : string; edits : (region * string) list }

(** Structured backtracking context. Replaces the rendering role of
    {!Attempt.failtrace} — [failtrace] stays for internal control flow in the
    elaborator. *)
type trace_node = {
  region : region;
  message : string;
  children : trace_node list;
}

type t = {
  severity : severity;
  region : region;
  code : string option;
  message : string;
  detail : string option;
  related : related list;
  fixes : fix list;
  trace : trace_node list;
  source : string;
      (* error type or compiler pass, e.g. "parser", "elaborator", "interpreter" *)
}

(** {1 Smart constructors} *)

val error :
  ?code:string ->
  ?detail:string ->
  ?related:related list ->
  ?fixes:fix list ->
  ?trace:trace_node list ->
  source:string ->
  region ->
  string ->
  t

val warning :
  ?code:string -> ?detail:string -> source:string -> region -> string -> t

val info : source:string -> region -> string -> t
val hint : source:string -> region -> string -> t

(** {1 Bridge from [Attempt.failtrace]} *)

val trace_of_failtrace : Common.Attempt.failtrace -> trace_node
val traces_of_failtraces : Common.Attempt.failtrace list -> trace_node list

(** Builds an error diagnostic from backtracking failtraces; [fallback] is the
    headline used when there is not exactly one root failtrace. *)
val of_failtraces :
  source:string -> fallback:string -> Common.Attempt.failtrace list -> t

(** {1 Plain text rendering} *)

(** Plain text rendering matching the legacy format:
    [<region>Warning:<source>:<message>] for warnings,
    [<region>Error: <message>] for errors. *)
val to_string : t -> string

(** {1 Collection} *)

module Bag : sig
  type diagnostic = t
  type t

  val empty : t
  val singleton : diagnostic -> t
  val add : t -> diagnostic -> t
  val merge : t -> t -> t
  val of_list : diagnostic list -> t
  val to_list : t -> diagnostic list

  (** Sorted by region (file, line, column). *)
  val to_sorted_list : t -> diagnostic list

  val is_empty : t -> bool
  val has_errors : t -> bool
  val error_count : t -> int
  val warning_count : t -> int
end

(** {1 Mutable accumulator}

    Collects diagnostics emitted during a compiler pass. Pipeline pattern:
    {!Sink.reset_global} at entry, {!Sink.drain} at exit. *)

module Sink : sig
  type diagnostic = t
  type t

  val create : unit -> t
  val emit : t -> diagnostic -> unit

  (** Collect only diagnostics at least as severe as [s] ([Error] drops
      warnings); default [Hint] keeps all. *)
  val set_min_severity : severity -> unit

  (** Returns collected diagnostics and resets the sink. *)
  val drain : t -> Bag.t

  (** Returns collected diagnostics without resetting. *)
  val peek : t -> Bag.t

  (** Global sink for backward-compatible migration. *)
  val global : unit -> t

  val reset_global : unit -> unit
end

(** {1 Convenience}

    [warn at source msg] is shorthand for emitting a warning into the global
    sink — used by passes that want to record a warning without threading a sink
    through every call. *)
val warn : region -> string -> string -> unit
