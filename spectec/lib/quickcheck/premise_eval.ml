open Lang.Il
open Common.Source

type bindings = (string * Value.t) list
type outcome = Holds of bindings | Fails | StepLimit | Unsupported of string
type env = { target : (module Target.S); core_spec : spec; max_steps : int }

let reltyp_of (core_spec : spec) (rel_id : id) : reltyp option =
  List.find_map
    (fun def ->
      match def.it with
      | RelD { relid = id; reltyp; _ } when id.it = rel_id.it -> Some reltyp
      | _ -> None)
    core_spec

let rel_input_args (core_spec : spec) (rel_id : id) (args : exp list) : exp list
    =
  match reltyp_of core_spec rel_id with
  | Some reltyp -> fst (Mode.partition reltyp.it args)
  | None -> args

let rel_output_args (core_spec : spec) (rel_id : id) (args : exp list) :
    exp list =
  match reltyp_of core_spec rel_id with
  | Some reltyp -> snd (Mode.partition reltyp.it args)
  | None -> []

(* Bare VarE only: anything else would need an in-process exp evaluator. *)
let lookup_input (bindings : bindings) (arg : exp) : (Value.t, string) result =
  match arg.it with
  | VarE id -> (
      match List.assoc_opt id.it bindings with
      | Some v -> Ok v
      | None ->
          Error
            (Printf.sprintf "input variable %s not bound by generator" id.it))
  | _ ->
      Error
        "non-VarE input arguments in property premises are not yet supported"

(* Threads a relation's output values back into the bindings: a fresh output
   variable is bound, an already-bound one is checked for equality. A mismatch
   means the relation does not hold for this output, reported as [Fails]. *)
let rec bind_outputs (bindings : bindings) (exps : exp list)
    (values : Value.t list) : outcome =
  match (exps, values) with
  | [], [] -> Holds bindings
  | exp :: exps', value :: values' -> (
      match exp.it with
      | VarE id -> (
          match List.assoc_opt id.it bindings with
          | Some bound ->
              if Value.eq value bound then bind_outputs bindings exps' values'
              else Fails
          | None -> bind_outputs ((id.it, value) :: bindings) exps' values')
      | _ ->
          Unsupported
            "non-VarE output arguments in property premises are not yet \
             supported")
  | _ -> Unsupported "relation output arity mismatch"

let eval_rule_pr (env : env) ~(bindings : bindings) (rel_id : id)
    (args : exp list) : outcome =
  let input_exps = rel_input_args env.core_spec rel_id args in
  let output_exps = rel_output_args env.core_spec rel_id args in
  let rec collect_values acc = function
    | [] -> Ok (List.rev acc)
    | a :: rest -> (
        match lookup_input bindings a with
        | Ok v -> collect_values (v :: acc) rest
        | Error msg -> Error msg)
  in
  match collect_values [] input_exps with
  | Error msg -> Unsupported msg
  | Ok values -> (
      let max_steps_opt =
        if env.max_steps < 0 then None else Some env.max_steps
      in
      try
        Step_budget.with_budget ?max_steps:max_steps_opt env.core_spec
          (fun () ->
            match
              Eval_il.run env.target env.core_spec rel_id.it values
                "<quickcheck>"
            with
            | Ok (_, output_values) ->
                bind_outputs bindings output_exps output_values
            | Error _ -> Fails)
      with Step_budget.StepLimitExceeded -> StepLimit)

let eval (env : env) ~bindings (prem : prem) : outcome =
  match prem.it with
  | RelPr { relid = rel_id; notexp }
  | RelAssertPr { call = { relid = rel_id; notexp }; expect = true } ->
      eval_rule_pr env ~bindings rel_id (Mixfix.args notexp)
  | RelAssertPr { call = { relid = rel_id; notexp }; expect = false } -> (
      match eval_rule_pr env ~bindings rel_id (Mixfix.args notexp) with
      | Holds _ -> Fails
      | Fails -> Holds bindings
      | other -> other)
  | _ ->
      Unsupported
        "only relation premises (RelPr, RelAssertPr) are supported in property \
         and generator bodies"

let rec eval_side (env : env) ~bindings = function
  | [] -> Holds bindings
  | p :: rest -> (
      match eval env ~bindings p with
      | Holds bindings' -> eval_side env ~bindings:bindings' rest
      | other -> other)
