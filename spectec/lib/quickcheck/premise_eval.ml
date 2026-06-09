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

(* Names an iterated construct binds: its free variables not already bound in
   the outer scope, i.e. the ones the iteration introduces. Property elaboration
   leaves the explicit iteration-variable list empty, so they are recovered from
   the [free] set of the inner expression or premise this way. *)
let collected_names (free : Common.Domain.IdSet.t) (bindings : bindings) :
    string list =
  let outer = List.map fst bindings in
  Common.Domain.IdSet.elements free
  |> List.filter_map (fun id ->
         if List.mem id.it outer then None else Some id.it)

(* Bind each name to the sequence of its per-element values across [rows]
   (one row of bindings per element). [fallback] types the empty case. *)
let bind_collected ~(iter : iter) (names : string list) (rows : bindings list)
    ~(fallback : typ') (bindings : bindings) : bindings =
  List.fold_left
    (fun bindings name ->
      let column = List.filter_map (List.assoc_opt name) rows in
      let typ =
        (match column with value :: _ -> value.note.typ | [] -> fallback)
        $ no_region
      in
      let value =
        match iter with
        | List -> Value.list typ column
        | Opt ->
            Value.opt typ
              (match column with value :: _ -> Some value | [] -> None)
      in
      (name, value) :: List.remove_assoc name bindings)
    bindings names

(* Evaluate [step] once per element, each starting from the outer [bindings],
   and collect every variable [step] binds into a sequence. Short-circuits on
   the first element that does not hold. *)
let collect_iteration ~(iter : iter) ~(names : string list) ~(fallback : typ')
    (bindings : bindings) (step : 'a -> outcome) (elements : 'a list) : outcome
    =
  let rec loop rows_rev = function
    | [] ->
        Holds
          (bind_collected ~iter names (List.rev rows_rev) ~fallback bindings)
    | element :: rest -> (
        match step element with
        | Holds row -> loop (row :: rows_rev) rest
        | other -> other)
  in
  loop [] elements

(* Match a pattern expression against a value, extending [bindings]. A fresh
   variable is bound; an already-bound one is checked for equality (bind-or-
   check). Structural patterns recurse; iterated patterns collect their
   variables into sequences. Unhandled shapes yield [Unsupported] (-> discard),
   a genuine mismatch yields [Fails]. *)
let rec match_pat (bindings : bindings) (pat : exp) (value : Value.t) : outcome
    =
  match (pat.it, value.it) with
  | VarE id, _ -> (
      match List.assoc_opt id.it bindings with
      | Some bound -> if Value.eq value bound then Holds bindings else Fails
      | None -> Holds ((id.it, value) :: bindings))
  | TupleE pats, TupleV values -> match_pats bindings pats values
  | CaseE notexp, CaseV valuecase ->
      if Mixfix.eq_mixop notexp valuecase then
        match_pats bindings (Mixfix.args notexp) (Mixfix.args valuecase)
      else Fails
  | OptE None, OptV None -> Holds bindings
  | OptE (Some pat), OptV (Some value) -> match_pat bindings pat value
  | OptE _, OptV _ -> Fails
  | ListE pats, ListV values ->
      if List.length pats = List.length values then
        match_pats bindings pats values
      else Fails
  | ConsE (pat_h, pat_t), ListV (value_h :: values_t) -> (
      match match_pat bindings pat_h value_h with
      | Holds bindings ->
          match_pat bindings pat_t (Value.Make.list value.note.typ values_t)
      | other -> other)
  | ConsE _, ListV [] -> Fails
  | IterE (pat, (iter, _)), _ -> match_iter bindings pat iter value
  | _ -> Unsupported "unsupported pattern in let-premise"

and match_pats (bindings : bindings) (pats : exp list) (values : Value.t list) :
    outcome =
  match (pats, values) with
  | [], [] -> Holds bindings
  | pat :: pats, value :: values -> (
      match match_pat bindings pat value with
      | Holds bindings -> match_pats bindings pats values
      | other -> other)
  | _ -> Unsupported "pattern arity mismatch"

(* Match an iterated pattern by normalising the option/list value to its
   elements and matching [pat] against each. *)
and match_iter (bindings : bindings) (pat : exp) (iter : iter) (value : Value.t)
    : outcome =
  let elements =
    match (iter, value.it) with
    | Opt, OptV None -> Some []
    | Opt, OptV (Some v) -> Some [ v ]
    | List, ListV vs -> Some vs
    | _ -> None
  in
  match elements with
  | None -> Fails
  | Some elements ->
      let names = collected_names (Free.free_exp pat) bindings in
      collect_iteration ~iter ~names ~fallback:value.note.typ bindings
        (match_pat bindings pat) elements

(* Threads a relation's output values back into the bindings via [match_pat]. *)
let rec bind_outputs (bindings : bindings) (exps : exp list)
    (values : Value.t list) : outcome =
  match (exps, values) with
  | [], [] -> Holds bindings
  | exp :: exps, value :: values -> (
      match match_pat bindings exp value with
      | Holds bindings -> bind_outputs bindings exps values
      | other -> other)
  | _ -> Unsupported "relation output arity mismatch"

(* Evaluate an expression to a value under [bindings]. Covers the shapes that
   appear in let-premise right-hand sides; unhandled shapes yield None. *)
let rec eval_val (bindings : bindings) (exp : exp) : Value.t option =
  match exp.it with
  | VarE id -> List.assoc_opt id.it bindings
  | BoolE b -> Some (Value.bool b)
  | NumE n -> Some (Value.Make.num exp.note n)
  | TextE s -> Some (Value.text s)
  | TupleE exps ->
      Option.map (Value.Make.tuple exp.note) (eval_vals bindings exps)
  | ListE exps ->
      Option.map (Value.Make.list exp.note) (eval_vals bindings exps)
  | OptE None -> Some (Value.Make.opt exp.note None)
  | OptE (Some inner) ->
      Option.map
        (fun v -> Value.Make.opt exp.note (Some v))
        (eval_val bindings inner)
  | CaseE notexp ->
      Option.map
        (fun values ->
          Value.Make.case exp.note (Mixfix.fill (Mixfix.to_mixop notexp) values))
        (eval_vals bindings (Mixfix.args notexp))
  | _ -> None

and eval_vals (bindings : bindings) (exps : exp list) : Value.t list option =
  List.fold_right
    (fun exp acc ->
      match (acc, eval_val bindings exp) with
      | Some vs, Some v -> Some (v :: vs)
      | _ -> None)
    exps (Some [])

(* Evaluate a boolean condition under [bindings]. Covers the comparisons and
   connectives that appear in if-premises; unhandled shapes yield None. *)
let rec eval_pred (bindings : bindings) (exp : exp) : bool option =
  match exp.it with
  | BoolE b -> Some b
  | UnE (`NotOp, _, exp) -> Option.map not (eval_pred bindings exp)
  | BinE (op, _, l, r) -> (
      match (eval_pred bindings l, eval_pred bindings r) with
      | Some l, Some r -> (
          match op with
          | `AndOp -> Some (l && r)
          | `OrOp -> Some (l || r)
          | `ImplOp -> Some ((not l) || r)
          | `EquivOp -> Some (l = r)
          | _ -> None)
      | _ -> None)
  | CmpE (((`EqOp | `NeOp) as op), _, l, r) -> (
      match (eval_val bindings l, eval_val bindings r) with
      | Some l, Some r ->
          let eq = Value.eq l r in
          Some (match op with `EqOp -> eq | _ -> not eq)
      | _ -> None)
  | _ -> None

(* An [if l = r] premise doubles as a pattern match when one side cannot be
   evaluated because it carries unbound variables: bind/check that pattern side
   against the other side's value. When both sides evaluate, it is a plain
   equality check. *)
let eval_eq (bindings : bindings) (l : exp) (r : exp) : outcome =
  match (eval_val bindings l, eval_val bindings r) with
  | Some l, Some r -> if Value.eq l r then Holds bindings else Fails
  | Some value, None -> match_pat bindings r value
  | None, Some value -> match_pat bindings l value
  | None, None -> Unsupported "if-condition: neither side is evaluable"

let eval_cond (bindings : bindings) (cond : exp) : outcome =
  match cond.it with
  | CmpE (`EqOp, _, l, r) -> eval_eq bindings l r
  | _ -> (
      match eval_pred bindings cond with
      | Some true -> Holds bindings
      | Some false -> Fails
      | None -> Unsupported "unsupported condition in if-premise")

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

let rec eval (env : env) ~bindings (prem : prem) : outcome =
  match prem.it with
  | RelPr { relid = rel_id; notexp }
  | RelAssertPr { call = { relid = rel_id; notexp }; expect = true } ->
      eval_rule_pr env ~bindings rel_id (Mixfix.args notexp)
  | RelAssertPr { call = { relid = rel_id; notexp }; expect = false } -> (
      match eval_rule_pr env ~bindings rel_id (Mixfix.args notexp) with
      | Holds _ -> Fails
      | Fails -> Holds bindings
      | other -> other)
  | IfPr { cond; _ } -> eval_cond bindings cond
  | LetPr (lhs, rhs) -> (
      match eval_val bindings rhs with
      | Some value -> match_pat bindings lhs value
      | None -> Unsupported "unsupported right-hand side in let-premise")
  | IterPr (prem, (iter, _)) -> eval_iter env ~bindings prem iter
  | ElsePr -> Holds bindings
  | DebugPr _ -> Holds bindings

(* Iterate [prem] over the sequences its free variables are bound to, once per
   element. Property elaboration leaves the [IterPr] variable list empty, so the
   driving sequences are recovered from those free variables; anything the inner
   premise binds per element is collected back into a sequence. *)
and eval_iter (env : env) ~bindings (prem : prem) (iter : iter) : outcome =
  match iter with
  | Opt -> Unsupported "option iteration in property premises is not supported"
  | List -> (
      let free = Free.free_prem prem in
      let sequences =
        Common.Domain.IdSet.elements free
        |> List.filter_map (fun id ->
               match List.assoc_opt id.it bindings with
               | Some { it = ListV elements; _ } -> Some (id.it, elements)
               | _ -> None)
      in
      match sequences with
      | [] -> Unsupported "iteration has no bound sequence variable to drive it"
      | (driver, first) :: _ ->
          let length = List.length first in
          if
            not
              (List.for_all (fun (_, es) -> List.length es = length) sequences)
          then Fails
          else
            let names = collected_names free bindings in
            let fallback = (List.assoc driver bindings).note.typ in
            let element_bindings index =
              List.fold_left
                (fun bindings (name, elements) ->
                  (name, List.nth elements index)
                  :: List.remove_assoc name bindings)
                bindings sequences
            in
            collect_iteration ~iter:List ~names ~fallback bindings
              (fun bindings -> eval env ~bindings prem)
              (List.init length element_bindings))

let rec eval_side (env : env) ~bindings = function
  | [] -> Holds bindings
  | p :: rest -> (
      match eval env ~bindings p with
      | Holds bindings' -> eval_side env ~bindings:bindings' rest
      | other -> other)
