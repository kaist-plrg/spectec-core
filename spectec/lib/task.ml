(** Task - Defines a kind of interpreter task for a target.

    A TASK encapsulates how to handle one type of interpreter execution:
    - name: Identifier (e.g., "typecheck", "state_transition")
    - input: Target-specific input type
    - parse: Convert input into relation name + IL values
    - source: Get source identifier for tracing/errors
    - expectation: Whether input expects success or failure
    - collect: Gather inputs from a directory
    - format_output: Format output values for display
    - save_output: Save output to file (optional) *)

module Il = Lang.Il

type 'a result = ('a, Error.t) Stdlib.result

(** Test expectation: does the test expect success or failure? *)
type expectation = Positive | Negative

(** Test outcome after considering expectation *)
type test_outcome =
  | Pass of Il.Value.t list
  | Fail of Error.t
  | ExpectedFail of Error.t
  | UnexpectedPass of Il.Value.t list

(** Compute test outcome from expectation and result *)
let compute_outcome expectation result =
  match (expectation, result) with
  | Positive, Ok values -> Pass values
  | Positive, Error e -> Fail e
  | Negative, Error e -> ExpectedFail e
  | Negative, Ok values -> UnexpectedPass values

(** A task specification for interpreter execution *)
module type S = sig
  val name : string

  (* Reference to the parent Target. *)
  module Target : Target.S

  type input

  (** Parse a string into IL values. *)
  val parse_string :
    spec:Il.spec -> filename:string -> string -> Il.Value.t list result

  (** Unparse IL values back into a string for display. *)
  val unparse : spec:Il.spec -> Il.Value.t list -> string

  (** Parse input into relation name and IL values. The relation name is invoked
      for the input. *)
  val parse_input : spec:Il.spec -> input -> (string * Il.Value.t list) result

  (** Get source identifier for tracing/errors (e.g., filename, test name) *)
  val source : input -> string

  (** Get test expectation (Positive/Negative) *)
  val expectation : input -> expectation

  (** Collect inputs, from [dir] when provided. *)
  val collect : ?dir:string -> unit -> input list

  val format_output : Il.Value.t list -> string
  val save_output : string -> Il.Value.t list -> unit
end

(** Existential wrapper for heterogeneous tasks *)
type packed_task = Pack : (module S with type input = 'a) -> packed_task
