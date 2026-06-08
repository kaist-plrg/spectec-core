(** Direct evaluation of property and generator premises under a set of
    generator-supplied variable bindings, used in place of the prior
    synthesized-relation mechanic. *)

open Lang.Il

type bindings = (string * Value.t) list

(** Outcome of evaluating a premise or premise list. [Holds]: the relation
    accepted the inputs; it carries the bindings extended with the relation's
    output values (a fresh output variable is bound, an already-bound one having
    been checked for equality). [Fails]: it rejected, or an output contradicted
    an existing binding. [StepLimit]: hit the step budget. [Unsupported]:
    premise shape or argument shape not yet handled. *)
type outcome = Holds of bindings | Fails | StepLimit | Unsupported of string

type env = {
  target : (module Target.S);
  core_spec : spec;
  max_steps : int;
      (** Negative values disable the budget; non-negative caps relation
          entries. *)
}

(** Evaluates a single premise. Only [RelPr] and [RelAssertPr] are handled;
    other premise shapes return [Unsupported]. *)
val eval : env -> bindings:bindings -> prem -> outcome

(** Evaluates a list of premises left-to-right, threading each premise's output
    bindings into the next, and short-circuiting on the first non-[Holds]
    outcome. The returned [Holds] carries the bindings accumulated across all
    premises. *)
val eval_side : env -> bindings:bindings -> prem list -> outcome
