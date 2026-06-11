(** QuickCheck test runner. *)

type config = {
  max_size : int;  (** Size grows from 0 to [max_size]. Default: 50. *)
  seed : [ `Deterministic of int | `Nondeterministic ];
      (** PRNG seed. Default: [`Deterministic 42]. *)
  verbose : bool;  (** Print each test case. Default: false. *)
}

val default_config : config

type outcome =
  | Pass of { num_tests : int; stamps : (string * int) list }
  | Fail of {
      num_tests : int;
      counterexample : string list;
      generalized : string list option;
          (** The widened counterexample family, when generalization found one.
          *)
    }
  | Gave_up of { num_tests : int }
      (** Triggered when discarded trials exceed 10x [num_tests]. *)

(** [run ~num_tests prop] drives [prop] for [num_tests] trials, growing the size
    parameter from 0 to [config.max_size], and returns the aggregate outcome. On
    a failing trial, applies the shrink and generalize callbacks populated by
    [Property.for_all] before reporting the counterexample. *)
val run : num_tests:int -> ?config:config -> Property.t -> outcome

(** [print_detail ~ansi ~label text] prints one indented report line with a
    dimmed fixed-width label column, for details nested under a verdict. *)
val print_detail : ansi:Diag.Ansi.t -> label:string -> string -> unit

(** [print_outcome ~ansi outcome] completes the property's report line with a
    colored verdict, followed by one detail line per counterexample part. *)
val print_outcome : ansi:Diag.Ansi.t -> outcome -> unit
