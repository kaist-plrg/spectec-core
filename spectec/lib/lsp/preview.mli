(** Render a spec at one selected pipeline stage. *)

open Common.Source

(** Select elaborated IL, structured SL, or annotated PL. *)
type stage = Il | Sl | Pl

(** Parse request stages: ["il"], ["sl"], or ["pl"]. *)
val stage_of_string : string -> stage option

type entry = {
  line : int;  (** 0-based, into the rendered text. *)
  depth : int;  (** Depth: 0 definition, 1 rule/clause, 2 prose step. *)
  region : region;  (** The source it was elaborated from. *)
}

type reason = { message : string; region : region }

type render = {
  text : string;  (** The stage's rendering, as the CLI prints it. *)
  entries : entry list;
      (** Entries follow rendered order for sequential lookup. *)
  stale : bool;
      (** Failed elaboration: previous render, or empty without one. *)
  reason : reason option;  (** Why it is stale. *)
}

(** Cache successful renders separately per spec and stage. *)
type t

val create : unit -> t

(** Render buffered specs; reuse cache, marking failures stale. *)
val render : ?stage:stage -> t -> open_path:string -> text:string -> render
