(** Direct evaluation of property and generator premises under a set of
    generator-supplied variable bindings, used in place of the prior
    synthesized-relation mechanic. *)

open Lang.Il

type bindings = (string * Value.t) list

(** Outcome of evaluating a premise or premise list. [Holds]: the relation
    accepted the inputs. [Fails]: it rejected. [StepLimit]: hit the step budget.
    [Unsupported]: premise shape or argument shape not yet handled. *)
type outcome = Holds | Fails | StepLimit | Unsupported of string

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

(** Evaluates a list of premises left-to-right, short-circuiting on the first
    non-[Holds] outcome. *)
val eval_side : env -> bindings:bindings -> prem list -> outcome
