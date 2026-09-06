(** Analyse the whole spec, including unsaved buffers. *)

type analysis = {
  parsed : bool;  (** Parse success; retain previous tables on failure. *)
  diagnostics : (string * Linol_eio.Diagnostic.t list) list;
      (** Nonempty diagnostic groups, keyed by canonical path. *)
  index : Index.t;  (** Where each name is declared. *)
  uses : Uses.t;  (** Where each name is mentioned. *)
  il : Lang.Il.spec option;
      (** Elaborated spec; retain previous IL on failure. *)
}

(** Check and index; parsing alone populates symbols. *)
val analyze :
  ?buffers:(string * string) list -> path:string -> string -> analysis

(** Run {!analyze}, returning only the grouped diagnostics. *)
val run :
  ?buffers:(string * string) list ->
  path:string ->
  string ->
  (string * Linol_eio.Diagnostic.t list) list

(** Resolve alternate paths to one canonical filename. *)
val canonical : string -> string

(** Collect spec sources, preferring buffers; report unreadable files. *)
val sources_of :
  ?buffers:(string * string) list ->
  open_path:string ->
  string ->
  (Spectec.spec_source list, (string * string) list) result
