open Common.Source
open Common.Attempt

type error = Diag.t list
type 'a result = ('a, error) Stdlib.result

exception ElabError of Diag.t

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

let string_of_code = function
  | Functyp_tparam_arity_mismatch -> "functyp-tparam-arity-mismatch"
  | Functyp_param_arity_mismatch -> "functyp-param-arity-mismatch"
  | Vart_targ_arity_mismatch -> "vart-targ-arity-mismatch"
  | Call_targ_arity_mismatch -> "call-targ-arity-mismatch"
  | Call_arg_arity_mismatch -> "call-arg-arity-mismatch"
  | Funarg_shape_mismatch_call -> "funarg-shape-mismatch-call"
  | Funarg_shape_mismatch_sig -> "funarg-shape-mismatch-sig"
  | Funarg_expected_exp_got_fun -> "funarg-expected-exp-got-fun"
  | Funarg_expected_fun_got_exp -> "funarg-expected-fun-got-exp"
  | Funparam_tparam_not_distinct -> "funparam-tparam-not-distinct"
  | Builtin_dec_tparam_not_distinct -> "builtin-dec-tparam-not-distinct"
  | Dec_tparam_not_distinct -> "dec-tparam-not-distinct"
  | Typ_tparam_mismatch -> "typ-tparam-mismatch"
  | Clause_tparam_mismatch -> "clause-tparam-mismatch"
  | Clause_arg_arity_mismatch -> "clause-arg-arity-mismatch"
  | Syn_invalid_id -> "syn-invalid-id"
  | Typ_invalid_id -> "typ-invalid-id"
  | Typ_invalid_tparam -> "typ-invalid-tparam"
  | Extend_non_variant_struct -> "extend-non-variant-struct"
  | Extend_incomplete -> "extend-incomplete"
  | Extend_non_variant_primitive -> "extend-non-variant-primitive"
  | Variant_mixop_collision -> "variant-mixop-collision"
  | Typ_fully_redefined -> "typ-fully-redefined"
  | Hole_outside_hint -> "hole-outside-hint"
  | Fuse_outside_hint -> "fuse-outside-hint"
  | Unparen_outside_hint -> "unparen-outside-hint"
  | Latex_outside_hint -> "latex-outside-hint"
  | Var_prem_invalid_metavar -> "var-prem-invalid-metavar"
  | Var_prem_type_redefined -> "var-prem-type-redefined"
  | Var_def_invalid_metavar -> "var-def-invalid-metavar"
  | Var_def_type_redefined -> "var-def-type-redefined"
  | Negated_premise_takes_inputs -> "negated-premise-takes-inputs"
  | Iter_only_rule_or_if_premise -> "iter-only-rule-or-if-premise"
  | Relation_input_hint_empty -> "relation-input-hint-empty"
  | Relation_input_hint_duplicate_index -> "relation-input-hint-duplicate-index"
  | Relation_input_hint_non_hole -> "relation-input-hint-non-hole"
  | Relation_no_input_hint -> "relation-no-input-hint"
  | Relation_missing_rules -> "relation-missing-rules"
  | Dec_missing_clauses -> "dec-missing-clauses"
  | Ctx_type_undefined -> "ctx-type-undefined"
  | Ctx_relation_undefined -> "ctx-relation-undefined"
  | Ctx_defined_dec_undefined -> "ctx-defined-dec-undefined"
  | Ctx_dec_undefined -> "ctx-dec-undefined"
  | Ctx_metavar_redefined -> "ctx-metavar-redefined"
  | Ctx_type_redefined -> "ctx-type-redefined"
  | Ctx_relation_redefined -> "ctx-relation-redefined"
  | Ctx_builtin_dec_redefined -> "ctx-builtin-dec-redefined"
  | Ctx_dec_redefined -> "ctx-dec-redefined"
  | Dataflow_free_variable_in_output -> "dataflow-free-variable-in-output"
  | Dataflow_bind_both_sides_of_equality ->
      "dataflow-bind-both-sides-of-equality"
  | Dataflow_multibind_dimension_mismatch ->
      "dataflow-multibind-dimension-mismatch"
  | Dataflow_bind_in_non_invertible -> "dataflow-bind-in-non-invertible"
  | Dataflow_empty_iter_expression -> "dataflow-empty-iter-expression"
  | Dataflow_empty_iter_premise -> "dataflow-empty-iter-premise"
  | Dataflow_iter_binding_only -> "dataflow-iter-binding-only"
  | Dataflow_iter_dimension_mismatch -> "dataflow-iter-dimension-mismatch"

let render_code (c : code) : string = "elab/" ^ string_of_code c

let related_of_pairs (pairs : (region * string) list) : Diag.related list =
  List.map (fun (region, message) -> { Diag.region; message }) pairs

let diag_of_failtraces (failtraces : failtrace list) : Diag.t =
  Diag.of_failtraces ~source:"elab" ~fallback:"elaboration failed" failtraces

let error ?code ?detail ?(related = []) (at : region) (msg : string) =
  let d =
    Diag.error
      ?code:(Option.map render_code code)
      ?detail ~related:(related_of_pairs related) ~source:"elab" at msg
  in
  raise (ElabError d)

let warn ?code ?detail (at : region) (msg : string) =
  let d =
    Diag.warning
      ?code:(Option.map render_code code)
      ?detail ~source:"elab" at msg
  in
  Diag.Sink.emit (Diag.Sink.global ()) d

let check ?code ?detail ?related (b : bool) (at : region) (msg : string) : unit
    =
  if not b then error ?code ?detail ?related at msg

let error_with_traces ?code ?detail ?(related = [])
    (failtraces : failtrace list) =
  let d = diag_of_failtraces failtraces in
  let d =
    {
      d with
      code = Option.fold ~none:d.code ~some:(fun c -> Some (render_code c)) code;
      detail = Option.fold ~none:d.detail ~some:Option.some detail;
      related = d.related @ related_of_pairs related;
    }
  in
  raise (ElabError d)

let to_diagnostics (errors : error) : Diag.Bag.t = Diag.Bag.of_list errors
