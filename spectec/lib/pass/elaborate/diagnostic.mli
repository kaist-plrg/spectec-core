(** Elaboration diagnostics.

    All call sites go through one of [error], [warn], [check], or
    [error_with_traces]. Optional [?code], [?detail], and [?related] arguments
    attach extra fields; sites that do not need them omit the labels. The
    payload is the lib value type {!Diag.t} directly. *)

open Common.Source
open Common.Attempt

(** {1 Diagnostic payload} *)

type error = Diag.t list
type 'a result = ('a, error) Stdlib.result

exception ElabError of Diag.t

(** {1 Stable per-site identifier}

    One constructor per live elaborator diagnostic call site. Rendered as
    ["elab/<dashed-name>"] inside the diagnostic's [code] field.

    Constructor-name prefixes name the surface construct: [Typ_] for the
    [syntax T = ...] typdef body, [Syn_] for the [syntax T;] forward
    declaration, [Funparam_] for a function-shape parameter declaration,
    [Funarg_] for the corresponding argument-passing site, and so on. Group
    comments below classify by the kind of mistake; the prefix carries the
    site-shape information orthogonally. *)

type code =
  (* arity / parameter shape mismatches *)
  | Functyp_tparam_arity_mismatch
  | Functyp_param_arity_mismatch
  | Vart_targ_arity_mismatch
  | Call_targ_arity_mismatch
  | Call_arg_arity_mismatch
  | Funarg_shape_mismatch_call
  | Funarg_shape_mismatch_sig
  | Funarg_expected_exp_got_fun
  | Funarg_expected_fun_got_exp
  | Funparam_tparam_not_distinct
  | Builtin_dec_tparam_not_distinct
  | Dec_tparam_not_distinct
  | Typ_tparam_mismatch
  | Clause_tparam_mismatch
  | Clause_arg_arity_mismatch
  (* invalid identifiers / parameters *)
  | Syn_invalid_id
  | Typ_invalid_id
  | Typ_invalid_tparam
  (* extends / variant *)
  | Extend_non_variant_struct
  | Extend_incomplete
  | Extend_non_variant_primitive
  | Variant_mixop_collision
  | Typ_fully_redefined
  (* misplaced notation atoms *)
  | Hole_outside_hint
  | Fuse_outside_hint
  | Unparen_outside_hint
  | Latex_outside_hint
  (* var declarations / var premises *)
  | Var_prem_invalid_metavar
  | Var_prem_type_redefined
  | Var_def_invalid_metavar
  | Var_def_type_redefined
  (* premise shape *)
  | Negated_premise_takes_inputs
  | Iter_only_rule_or_if_premise
  (* relation input hints *)
  | Relation_input_hint_empty
  | Relation_input_hint_duplicate_index
  | Relation_input_hint_non_hole
  | Relation_no_input_hint
  (* warnings: empty bodies *)
  | Relation_missing_rules
  | Dec_missing_clauses
  (* ctx: lookup *)
  | Ctx_type_undefined
  | Ctx_relation_undefined
  | Ctx_defined_dec_undefined
  | Ctx_dec_undefined
  (* ctx: redefinition *)
  | Ctx_metavar_redefined
  | Ctx_type_redefined
  | Ctx_relation_redefined
  | Ctx_builtin_dec_redefined
  | Ctx_dec_redefined
  (* dataflow *)
  | Dataflow_free_variable_in_output
  | Dataflow_bind_both_sides_of_equality
  | Dataflow_multibind_dimension_mismatch
  | Dataflow_bind_in_non_invertible
  | Dataflow_empty_iter_expression
  | Dataflow_empty_iter_premise
  | Dataflow_iter_binding_only
  | Dataflow_iter_dimension_mismatch

(** {1 Raising and emitting}

    [?code] tags the diagnostic with a stable site identifier; [?detail]
    attaches longer prose; [?related] attaches secondary source spans (e.g.
    ["originally defined here"]). Sites that need none of these omit the labels.
*)

val error :
  ?code:code ->
  ?detail:string ->
  ?related:(region * string) list ->
  region ->
  string ->
  'a

val warn : ?code:code -> ?detail:string -> region -> string -> unit

val check :
  ?code:code ->
  ?detail:string ->
  ?related:(region * string) list ->
  bool ->
  region ->
  string ->
  unit

(** Raise from a backtracking failure tree. A singleton tree's leaf message
    becomes the diagnostic message; multi-leaf trees get the placeholder
    ["elaboration failed"] with the tree threaded into [trace]. *)
val error_with_traces :
  ?code:code ->
  ?detail:string ->
  ?related:(region * string) list ->
  failtrace list ->
  'a

(** {1 Boundary - payload to {!Diag.Bag.t}} *)

val to_diagnostics : error -> Diag.Bag.t
