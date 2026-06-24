(** Tree: buffered derivation-tree renderer for a successful evaluation.

    Unlike {!Trace} (which streams every event), Tree accumulates events inside
    a top-level relation invocation and emits a single ASCII tree once that
    invocation completes. Failed rule attempts are pruned: only the rule that
    actually fired at each relation invocation remains visible.

    Each relation is drawn as a derivation tree in spec syntax: its conclusion
    on top, its sub-derivations below as premises led by [--].

    Levels:
    - [Rule]: each relation node tagged with the rule that matched.
    - [Conclusion]: [Rule] + the conclusion judgment under each tag (e.g.
      [[ x -> int ] |- 5 : int]).
    - [Premise]: [Conclusion] + each rule's premises in source order,
      concretized with runtime values. IL only. *)

module Il = Lang.Il
module El = Lang.El
module Ansi = Diag.Ansi
open Util
open Common.Source

type level = Rule | Conclusion | Premise
type config = { level : level; output : Instrumentation_api.Output.t }

let default_config =
  { level = Rule; output = Instrumentation_api.Output.stdout }

let config = ref default_config
let fmt = ref Format.std_formatter
let ansi = ref Ansi.plain

let summarize_value (v : Il.Value.t) : string =
  Il.Print.string_of_value v |> summarize ~max_len:100

(* === Tree representation =========================================== *)

type kind = Rel | Func
type judgment = (Il.Value.t, Il.Value.t option) Il.Mode.t

type outcome =
  | Rel_result of { conclusion : judgment; holds : bool }
  | Func_result of Il.Value.t option (* [None] if the call got stuck. *)

type node = {
  kind : kind;
  id : string;
  inputs : Il.Value.t list;
  mutable outcome : outcome option; (* [None] while still on the stack. *)
  (* The rule that fired, or the function body; each attempt restarts it. *)
  mutable derivation : derivation;
  (* Applied-but-failed rules; guard failures are dropped. *)
  mutable failures_rev : derivation list;
  (* The premise being evaluated now, awaiting its sub-derivation; premise level. *)
  mutable pending_prem : prem_entry option;
  (* Variable values across the rule's premises, synthesized binders included. *)
  mutable binding_env : (Il.id * Il.Value.t) list;
}

and prem_entry = { prem : Il.prem; mutable subderiv : node option }

(* One rule's attempt at a goal; [unmet_prem] is the premise that defeated it, or
   [None] if the rule fired. *)
and derivation = {
  mutable rule : string option;
  mutable prems_rev : prem_entry list;
  mutable children_rev : node list;
  mutable unmet_prem : Il.prem option;
}

let empty_derivation () =
  { rule = None; prems_rev = []; children_rev = []; unmet_prem = None }

let new_node kind id inputs =
  {
    kind;
    id;
    inputs;
    outcome = None;
    derivation = empty_derivation ();
    failures_rev = [];
    pending_prem = None;
    binding_env = [];
  }

let is_failed node =
  match node.outcome with
  | Some (Rel_result { holds = false; _ }) | Some (Func_result None) -> true
  | _ -> false

let is_applied = function
  | Some { it = Il.IfPr { role = Il.Guard; _ }; _ } -> false
  | _ -> true

(* === Mutable state ================================================= *)

module State = struct
  (* The stack's head is the node currently under evaluation. *)
  let stack : node list ref = ref []
  let reset () = stack := []
  let push node = stack := node :: !stack
  let with_current f = match !stack with [] -> () | current :: _ -> f current

  let attach ~parent node =
    match (!config.level, parent.pending_prem, node.kind) with
    | Premise, Some entry, Rel -> entry.subderiv <- Some node
    (* A function under a premise is absorbed into its text, not a node. *)
    | Premise, Some _, Func -> ()
    | _ ->
        parent.derivation.children_rev <- node :: parent.derivation.children_rev

  let pop ~outcome =
    match !stack with
    | [] -> assert false
    | current :: rest -> (
        current.outcome <- Some outcome;
        stack := rest;
        match rest with
        | [] -> Some current
        | parent :: _ ->
            attach ~parent current;
            None)

  let begin_rule_attempt () =
    with_current (fun current -> current.derivation <- empty_derivation ())

  let end_rule_attempt ~rule_id ~success =
    with_current (fun current ->
        let derivation = current.derivation in
        derivation.rule <- Some rule_id;
        if (not success) && is_applied derivation.unmet_prem then
          current.failures_rev <- derivation :: current.failures_rev)

  let enter_premise prem =
    with_current (fun current ->
        current.pending_prem <- Some { prem; subderiv = None })

  let record_premise () =
    with_current (fun current ->
        Option.iter
          (fun entry ->
            current.derivation.prems_rev <-
              entry :: current.derivation.prems_rev)
          current.pending_prem;
        current.pending_prem <- None)

  let record_bindings ~bindings =
    with_current (fun current ->
        current.binding_env <- bindings @ current.binding_env)

  let record_unmet_prem prem =
    with_current (fun current -> current.derivation.unmet_prem <- Some prem)
end

(* === Rendering ===================================================== *)

let dim s = Ansi.style !ansi [ Dim ] s
let accent s = Ansi.style !ansi [ Yellow ] s

let render_judgment c =
  let string_of_atom a =
    match Il.Print.string_of_atom a with "" -> "" | s -> dim s
  in
  let string_of_out = function
    | Some v -> summarize_value v
    | None -> dim "?"
  in
  Il.Mode.render ~pad_brackets:true ~string_of_atom
    ~string_of_in:summarize_value ~string_of_out c

let render_call node =
  let args = List.map summarize_value node.inputs |> String.concat ", " in
  Format.sprintf "$%s(%s)" node.id args

let render_tag node =
  match node.derivation.rule with
  | Some r when r <> "" -> node.id ^ "/" ^ r
  | _ -> node.id

(* Count code points, not bytes, and skip ANSI escapes, so the bar matches the
   conclusion's visible width. *)
let measure_width s =
  let _, width =
    String.fold_left
      (fun (in_escape, width) c ->
        if in_escape then (c <> 'm', width)
        else if c = '\027' then (true, width)
        else if Char.code c land 0xc0 = 0x80 then (false, width)
        else (false, width + 1))
      (false, 0) s
  in
  width

(* Box-drawing dash so the bar renders as one connected line. *)
let render_bar n = String.concat "" (List.init n (fun _ -> "─"))

let render_lines node =
  match (node.kind, !config.level, node.outcome) with
  | Rel, (Conclusion | Premise), Some (Rel_result { conclusion; _ }) ->
      let notation = render_judgment conclusion in
      [
        accent (render_tag node ^ ":");
        notation;
        dim (render_bar (measure_width notation));
      ]
  | Rel, _, _ -> [ accent (render_tag node) ]
  | Func, Premise, Some (Func_result (Some v)) ->
      [ Format.sprintf "%s = %s" (render_call node) (summarize_value v) ]
  | Func, _, _ -> [ "$" ^ node.id ]

let render_prem ~binding_env entry =
  let values varid =
    List.find_opt (fun (id, _) -> id.it = varid.it) binding_env
    |> Option.map (fun (_, v) -> summarize_value v)
  in
  match prov entry.prem with
  | Some (Il.Source el_prem) -> El.Unparse.string_of_prem ~values el_prem
  | _ -> Il.Print.string_of_prem entry.prem

let rec print_node ~first_lead ~rest_prefix node out =
  (match render_lines node with
  | [] -> ()
  | head :: rest ->
      Format.fprintf out "%s%s\n" first_lead head;
      List.iter (fun l -> Format.fprintf out "%s%s\n" rest_prefix l) rest);
  let child_lead = rest_prefix ^ dim "--" ^ " " in
  let child_rest = rest_prefix ^ "   " in
  let print_child child =
    print_node ~first_lead:child_lead ~rest_prefix:child_rest child out
  in
  match !config.level with
  | Premise ->
      List.iter
        (fun entry ->
          match (entry.prem.it, entry.subderiv) with
          | (Il.RelPr _ | Il.RelAssertPr { expect = true; _ }), Some subderiv ->
              print_child subderiv
          | _ ->
              Format.fprintf out "%s%s\n" child_lead
                (render_prem ~binding_env:node.binding_env entry))
        (List.rev node.derivation.prems_rev);
      List.iter print_child (List.rev node.derivation.children_rev)
  | Rule | Conclusion ->
      List.rev node.derivation.children_rev
      |> List.iter (fun child ->
             match child.kind with Rel -> print_child child | Func -> ())

let print_root node =
  print_node ~first_lead:"" ~rest_prefix:"" node !fmt;
  Format.pp_print_flush !fmt ()

let pop_and_maybe_print ~outcome =
  match State.pop ~outcome with
  | None -> ()
  | Some root when is_failed root -> ()
  | Some root -> print_root root

(* === Handler module ================================================ *)

let is_authored_prem prem =
  match prov prem with Some Il.Synthesized -> false | _ -> true

module M : Instrumentation_api.Handler.S = struct
  let static_dependencies = []
  let init ~spec:_ = State.reset ()
  let finish () = ()

  let handle : Instrumentation_api.Event.t -> unit = function
    | Test_start _ | Test_end _ -> State.reset ()
    | Rel_enter { id; at = _; inputs } -> State.push (new_node Rel id inputs)
    | Rel_exit { id = _; at = _; success; conclusion } ->
        pop_and_maybe_print
          ~outcome:(Rel_result { conclusion; holds = success })
    | Rule_enter _ -> State.begin_rule_attempt ()
    | Rule_exit { id = _; rule_id; at = _; success } ->
        State.end_rule_attempt ~rule_id ~success
    | Func_enter { id; at = _; inputs } -> State.push (new_node Func id inputs)
    | Func_exit { id = _; at = _; output } ->
        pop_and_maybe_print ~outcome:(Func_result output)
    (* Function clauses are tried like rules, but functions never invoke
       relations, so a failed clause leaves no sub-derivation to roll back. *)
    | Clause_enter _ | Clause_exit _ -> ()
    | Iter_prem_enter _ | Iter_prem_exit _ -> ()
    | Prem_enter { prem; at = _ } ->
        if !config.level = Premise && is_authored_prem prem then
          State.enter_premise prem
    | Prem_exit { prem; at = _; success; bindings } ->
        if not success then State.record_unmet_prem prem;
        if !config.level = Premise then (
          State.record_bindings ~bindings;
          if is_authored_prem prem then State.record_premise ())
    | Instr _ -> ()
end

let resolve_ansi : Instrumentation_api.Output.t -> Ansi.t = function
  | Stdout -> Ansi.auto ~tty:(Unix.isatty Unix.stdout)
  | File _ -> Ansi.plain

let make cfg =
  config := cfg;
  fmt := Instrumentation_api.Output.formatter cfg.output;
  ansi := resolve_ansi cfg.output;
  (module M : Instrumentation_api.Handler.S)

module Spec : Instrumentation_spec.Spec.S = struct
  let name = "tree"
  let mode = `Both

  let params =
    [
      ("level", "LEVEL verbosity level: rule|conclusion|premise");
      Instrumentation_spec.Param_utils.output_param;
    ]

  let parse_level = function
    | "rule" -> Rule
    | "conclusion" -> Conclusion
    | "premise" -> Premise
    | s ->
        failwith
          ("Invalid tree level: " ^ s ^ " (expected: rule|conclusion|premise)")

  (* Premises are IL-only. *)
  let mode_of_level = function Premise -> `IL | Rule | Conclusion -> `Both

  let parse alist =
    match Instrumentation_spec.Param_utils.get alist "level" with
    | None -> None
    | Some s ->
        let level = parse_level s in
        let output =
          Instrumentation_spec.Param_utils.output_of
            (Instrumentation_spec.Param_utils.get alist "output")
        in
        Some
          {
            Instrumentation_config.Handler_config.name;
            mode = mode_of_level level;
            handler = make { level; output };
            output;
          }

  let checkpoint = None
end

let spec : Instrumentation_spec.Spec.t = (module Spec)
