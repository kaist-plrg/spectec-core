open Il_gen
open Lang.Il
open Common.Source

type error = NoManualGenerator of string
type 'a result = ('a, error) Stdlib.result

let error_to_string = function
  | NoManualGenerator name ->
      Printf.sprintf
        "quickcheck: no manual generator named '%s'. Add a case in \
         manual_gen.ml gen_inputs."
        name

let error_to_diagnostic e =
  Diag.error ~source:"quickcheck" Common.Source.no_region (error_to_string e)

let find_generator_hint (hints : hint list) : string option =
  List.find_map
    (fun (h : hint) ->
      if h.hintid.it = "generator" then
        match h.hintexp.it with
        | Lang.El.CallE (id, [], []) -> Some id.it
        | _ -> None
      else None)
    hints

let shrink_env spec (env : (id' * value) list) : (id' * value) list list =
  let shrink_value = shrink spec in
  List.concat_map
    (fun (i, (_, vi)) ->
      List.map
        (fun vi' ->
          List.mapi
            (fun j (idj, vj) -> if j = i then (idj, vi') else (idj, vj))
            env)
        (shrink_value vi))
    (List.mapi (fun i p -> (i, p)) env)

let show_env (bindings : (id' * value) list) : string =
  String.concat ", "
    (List.map (fun (id, v) -> id ^ "=" ^ Print.string_of_value v) bindings)

let gen_free_vars (spec_il : spec) (inputs : (id * typ) list) :
    (id' * value) list Gen.t =
  Gen.sequence
    (List.map
       (fun (id, typ) ->
         Gen.map (fun value -> (id.it, value)) (gen_of_typ spec_il typ))
       inputs)

type manual_gen = spec -> (id' * value) list Gen.t

let gen_free_vars_manual ~(manual_gens : (string * manual_gen) list)
    (spec_il : spec) (name : string) :
    ((id' * value) list Gen.t, error) Stdlib.result =
  match List.assoc_opt name manual_gens with
  | Some gen_fn -> Ok (gen_fn spec_il)
  | None -> Error (NoManualGenerator name)

let run_property ~target ~generalize ~max_steps ~num_tests
    ~(manual_gens : (string * manual_gen) list) (core_spec : spec)
    ~(side_prems : prem list) ~(goal : prem) ~(hints : hint list) :
    ( Test.outcome * (id' * value) list option * ((id' * value) list -> bool),
      error )
    Stdlib.result =
  let config = { Test.default_config with Test.num_tests } in
  let generator = find_generator_hint hints in
  let inputs = Free_vars.of_premises ~core_spec (side_prems @ [ goal ]) in
  let eval_env = Premise_eval.{ target; core_spec; max_steps } in
  match
    match generator with
    | Some gen_name -> gen_free_vars_manual ~manual_gens core_spec gen_name
    | None -> Ok (gen_free_vars core_spec inputs)
  with
  | Error _ as e -> e
  | Ok gen ->
      let generalize_fn =
        if generalize then Some (Generalize.generalize_env core_spec) else None
      in
      (* Holds the most recent failing assignment. Shrinking re-runs the body
         on smaller inputs, so by the time [Test.run] returns this is the
         minimal counterexample. *)
      let captured : (id' * value) list option ref = ref None in
      let prop =
        Property.for_all ~shrink:(shrink_env core_spec)
          ?generalize:generalize_fn ~show:show_env gen (fun bindings ->
            let branch_snapshot =
              Instrumentation.Branch_coverage.get_result ()
            in
            let premise_snapshot =
              Instrumentation.Node_coverage_il.get_result ()
            in
            let verdict =
              match Premise_eval.eval_side eval_env ~bindings side_prems with
              | Premise_eval.Holds -> (
                  match Premise_eval.eval eval_env ~bindings goal with
                  | Premise_eval.Holds -> Property.Verdict.pass
                  | Premise_eval.Fails ->
                      captured := Some bindings;
                      Property.Verdict.fail
                  | Premise_eval.StepLimit | Premise_eval.Unsupported _ ->
                      Property.Verdict.discard)
              | Premise_eval.Fails | Premise_eval.StepLimit
              | Premise_eval.Unsupported _ ->
                  Property.Verdict.discard
            in
            (match verdict.Property.Verdict.status with
            | `Pass -> ()
            | `Fail | `Discard ->
                Instrumentation.Branch_coverage.restore branch_snapshot;
                Instrumentation.Node_coverage_il.restore premise_snapshot);
            Property.of_verdict verdict)
      in
      (* [true] iff [bindings] still reproduces: side premises hold, goal fails. *)
      let recheck (bindings : (id' * value) list) : bool =
        match Premise_eval.eval_side eval_env ~bindings side_prems with
        | Premise_eval.Holds -> (
            match Premise_eval.eval eval_env ~bindings goal with
            | Premise_eval.Fails -> true
            | _ -> false)
        | _ -> false
      in
      let outcome = Test.run ~config prop in
      Ok (outcome, !captured, recheck)

type counterexample = {
  name : string;
  env : (id' * value) list;
  recheck : (id' * value) list -> bool;
}

let check ~target ~generalize ~max_steps ~num_tests ~manual_gens
    (spec_il : spec) (qc_spec : Qc_il.spec) : counterexample list result =
  List.fold_left
    (fun acc qc_def ->
      match acc with
      | Error _ -> acc
      | Ok counterexamples -> (
          match qc_def with
          | Qc_il.BuiltinGeneratorD _ -> Ok counterexamples
          | Qc_il.PropertyD (name_id, side_prems, goal, hints) -> (
              let name = name_id.it in
              Printf.printf "[Quickcheck %s: Test]\n" name;
              match
                run_property ~target ~generalize ~max_steps ~num_tests
                  ~manual_gens spec_il ~side_prems ~goal ~hints
              with
              | Error _ as e -> e
              | Ok (outcome, captured, recheck) ->
                  Test.print_outcome outcome;
                  Ok
                    (match (outcome, captured) with
                    | Test.Fail _, Some env ->
                        { name; env; recheck } :: counterexamples
                    | _ -> counterexamples))))
    (Ok []) qc_spec
  |> Result.map List.rev
