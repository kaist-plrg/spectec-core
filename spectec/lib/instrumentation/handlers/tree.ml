(** Tree: buffers a top-level relation invocation and emits one ASCII derivation
    tree when it completes -- conclusion on top, sub-derivations below as
    premises led by [--]. Unlike {!Trace}, backtracking is pruned to the rules
    that applied. On failure the premise level renders each applied-but-failed
    rule as a branch under the goal, crossing the premise that defeated it.

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
type judgment = (Il.Value.t option, Il.Value.t option) Il.Mode.t
type outcome = Failed | Rel_ok | Func_ok of Il.Value.t

type node = {
  kind : kind;
  id : string;
  inputs : Il.Value.t list;
  mutable rule : string option;
  mutable outcome : outcome;
  mutable judgment : judgment option;
  mutable children_rev : node list;
  (* Premise level only. *)
  mutable prems_rev : prem_entry list;
  mutable pending_prem : prem_entry option;
  (* Variable values across all the rule's premises; synthesized sideconditions
     are included because they bind an author premise's output variables. *)
  mutable binding_env : (Il.id * Il.Value.t) list;
  (* [Some] only between this node's [Rule_enter] and [Rule_exit]. *)
  mutable attempt : attempt option;
  (* Applied-but-failed rules; guard failures are dropped to mirror the pruned
     IL failtrace. *)
  mutable failures_rev : derivation list;
}

and prem_entry = { prem : Il.prem; mutable subderiv : node option }

and attempt = {
  saved_children : node list;
  saved_prems : prem_entry list;
  mutable failing : Il.prem option;
}

(* A relation node's candidate derivation: the rule that fired (success) or one
   applied-but-failed rule. *)
and derivation = {
  d_rule : string option;
  d_prems_rev : prem_entry list;
  d_children_rev : node list;
  d_failing : Il.prem option;
  d_failed : bool;
}

let new_node kind id inputs =
  {
    kind;
    id;
    inputs;
    rule = None;
    outcome = Failed;
    judgment = None;
    children_rev = [];
    prems_rev = [];
    pending_prem = None;
    binding_env = [];
    attempt = None;
    failures_rev = [];
  }

let outcome_of_output = function Some v -> Func_ok v | None -> Failed
let is_failed node = match node.outcome with Failed -> true | _ -> false

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
  let set_judgment j = with_current (fun current -> current.judgment <- Some j)

  let attach ~parent node =
    match (!config.level, parent.pending_prem, node.kind) with
    | Premise, Some entry, Rel -> entry.subderiv <- Some node
    (* A function under a premise is absorbed into its text, not a node. *)
    | Premise, Some _, Func -> ()
    | _ -> parent.children_rev <- node :: parent.children_rev

  let pop ~outcome =
    match !stack with
    | [] -> assert false
    | current :: rest -> (
        current.outcome <- outcome;
        stack := rest;
        match rest with
        | [] -> Some current
        | parent :: _ ->
            attach ~parent current;
            None)

  let begin_rule_attempt () =
    with_current (fun current ->
        current.attempt <-
          Some
            {
              saved_children = current.children_rev;
              saved_prems = current.prems_rev;
              failing = None;
            })

  let end_rule_attempt ~rule_id ~success =
    with_current (fun current ->
        Option.iter
          (fun a ->
            if success then current.rule <- Some rule_id
            else (
              if is_applied a.failing then
                current.failures_rev <-
                  {
                    d_rule = Some rule_id;
                    d_prems_rev = current.prems_rev;
                    d_children_rev = current.children_rev;
                    d_failing = a.failing;
                    d_failed = true;
                  }
                  :: current.failures_rev;
              current.children_rev <- a.saved_children;
              current.prems_rev <- a.saved_prems);
            current.attempt <- None)
          current.attempt)

  let enter_premise prem =
    with_current (fun current ->
        current.pending_prem <- Some { prem; subderiv = None })

  let record_premise () =
    with_current (fun current ->
        Option.iter
          (fun entry -> current.prems_rev <- entry :: current.prems_rev)
          current.pending_prem;
        current.pending_prem <- None)

  let record_bindings ~bindings =
    with_current (fun current ->
        current.binding_env <- bindings @ current.binding_env)

  let record_failing_prem prem =
    with_current (fun current ->
        Option.iter (fun a -> a.failing <- Some prem) current.attempt)
end

let is_authored_prem prem =
  match prov prem with Some Il.Synthesized -> false | _ -> true

(* === Rendering ===================================================== *)

let dim s = Ansi.style !ansi [ Dim ] s
let accent s = Ansi.style !ansi [ Yellow ] s
let alarm s = Ansi.style !ansi [ Bold; Red ] s

let render_judgment c =
  let string_of_atom a =
    match Il.Print.string_of_atom a with "" -> "" | s -> dim s
  in
  let string_of_arg = function
    | Some v -> summarize_value v
    | None -> dim "?"
  in
  Il.Mode.render ~pad_brackets:true ~string_of_atom ~string_of_arg c

let render_call node =
  let args = List.map summarize_value node.inputs |> String.concat ", " in
  Format.sprintf "$%s(%s)" node.id args

let render_tag node ~rule =
  match rule with Some r when r <> "" -> node.id ^ "/" ^ r | _ -> node.id

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

(* Box-drawing glyph so that the bar consistently renders connected. *)
let render_bar n = String.concat "" (List.init n (fun _ -> "─"))

(* The cross is padded to the width of [--] so crossed and uncrossed siblings
   stay aligned. *)
let connector marked = if marked then alarm "✗" ^ "  " else dim "--" ^ " "

let rel_head node ~rule =
  match (!config.level, node.judgment) with
  | (Conclusion | Premise), Some c ->
      let notation = render_judgment c in
      [
        accent (render_tag node ~rule ^ ":");
        notation;
        dim (render_bar (measure_width notation));
      ]
  | _ -> [ accent (render_tag node ~rule) ]

let render_lines node =
  match node.kind with
  | Rel -> rel_head node ~rule:node.rule
  | Func -> (
      match (!config.level, node.outcome) with
      | Premise, Func_ok v ->
          [ Format.sprintf "%s = %s" (render_call node) (summarize_value v) ]
      | _ -> [ "$" ^ node.id ])

let render_prem ~binding_env entry =
  (* Returning [None] would let the unparser print the variable's name; show [?]
     for an unbound variable instead. *)
  let values varid =
    match List.find_opt (fun (id, _) -> id.it = varid.it) binding_env with
    | Some (_, v) -> Some (summarize_value v)
    | None -> Some (dim "?")
  in
  match prov entry.prem with
  | Some (Il.Source el_prem) -> El.Unparse.string_of_prem ~values el_prem
  | _ -> Il.Print.string_of_prem entry.prem

let derivations_of node =
  if not (is_failed node) then
    [
      {
        d_rule = node.rule;
        d_prems_rev = node.prems_rev;
        d_children_rev = node.children_rev;
        d_failing = None;
        d_failed = false;
      };
    ]
  else
    match List.rev node.failures_rev with
    | [] ->
        (* No rule applied. *)
        [
          {
            d_rule = None;
            d_prems_rev = [];
            d_children_rev = [];
            d_failing = None;
            d_failed = true;
          };
        ]
    | ds -> ds

let print_lines out ~first_lead ~rest_prefix = function
  | [] -> ()
  | head :: rest ->
      Format.fprintf out "%s%s\n" first_lead head;
      List.iter (fun l -> Format.fprintf out "%s%s\n" rest_prefix l) rest

let rec print_node ~first_lead ~rest_prefix node out =
  match (node.kind, !config.level) with
  | Rel, Premise -> print_rel_premise ~first_lead ~rest_prefix node out
  | _ -> (
      print_lines out ~first_lead ~rest_prefix (render_lines node);
      let print_child child =
        print_node
          ~first_lead:(rest_prefix ^ connector (is_failed child))
          ~rest_prefix:(rest_prefix ^ "   ") child out
      in
      match !config.level with
      | Premise -> List.iter print_child (List.rev node.children_rev)
      | Rule | Conclusion ->
          List.rev node.children_rev
          |> List.iter (fun child ->
                 match child.kind with Rel -> print_child child | Func -> ()))

and print_rel_premise ~first_lead ~rest_prefix node out =
  match derivations_of node with
  | [ d ] ->
      print_lines out ~first_lead ~rest_prefix (rel_head node ~rule:d.d_rule);
      print_derivation_body ~rest_prefix node d out
  | ds ->
      (* Several rules applied: show the goal once, then each as a crossed branch. *)
      print_lines out ~first_lead ~rest_prefix (rel_head node ~rule:None);
      List.iter
        (fun d ->
          Format.fprintf out "%s%s\n"
            (rest_prefix ^ connector true)
            (accent (render_tag node ~rule:d.d_rule));
          print_derivation_body ~rest_prefix:(rest_prefix ^ "   ") node d out)
        ds

and print_derivation_body ~rest_prefix node d out =
  let print_child child =
    print_node
      ~first_lead:(rest_prefix ^ connector (is_failed child))
      ~rest_prefix:(rest_prefix ^ "   ") child out
  in
  let print_prem ?(marked = false) entry =
    let text = render_prem ~binding_env:node.binding_env entry in
    Format.fprintf out "%s%s\n" (rest_prefix ^ connector marked) text
  in
  (* A synthesized failure has no surface form, so its culprit is shown as the
     authored premise it came from, printed as text rather than recursed. *)
  let synth_failure =
    match d.d_failing with Some p -> not (is_authored_prem p) | None -> false
  in
  let entries = List.rev d.d_prems_rev in
  let last = List.length entries - 1 in
  List.iteri
    (fun i entry ->
      let is_culprit = d.d_failed && i = last in
      let show_as_text = is_culprit && synth_failure in
      match (entry.prem.it, entry.subderiv) with
      | (Il.RelPr _ | Il.RelAssertPr { expect = true; _ }), Some subderiv
        when not show_as_text ->
          print_child subderiv
      | _ -> print_prem ~marked:is_culprit entry)
    entries;
  List.iter print_child (List.rev d.d_children_rev)

let print_root node =
  print_node ~first_lead:"" ~rest_prefix:"" node !fmt;
  Format.pp_print_flush !fmt ()

let pop_and_maybe_print ~outcome =
  match State.pop ~outcome with
  | None -> ()
  | Some { outcome = Failed; _ } when !config.level <> Premise -> ()
  | Some root -> print_root root

(* === Handler module ================================================ *)

module M : Instrumentation_api.Handler.S = struct
  let static_dependencies = []
  let init ~spec:_ = State.reset ()
  let finish () = ()

  let handle : Instrumentation_api.Event.t -> unit = function
    | Test_start _ | Test_end _ -> State.reset ()
    | Rel_enter { id; at = _; inputs } -> State.push (new_node Rel id inputs)
    | Rel_exit { id = _; at = _; success; conclusion } ->
        State.set_judgment conclusion;
        pop_and_maybe_print ~outcome:(if success then Rel_ok else Failed)
    | Rule_enter _ -> State.begin_rule_attempt ()
    | Rule_exit { id = _; rule_id; at = _; success } ->
        State.end_rule_attempt ~rule_id ~success
    | Func_enter { id; at = _; inputs } -> State.push (new_node Func id inputs)
    | Func_exit { id = _; at = _; output } ->
        pop_and_maybe_print ~outcome:(outcome_of_output output)
    (* Function clauses are tried like rules, but functions never invoke
       relations, so a failed clause leaves no sub-derivation to roll back. *)
    | Clause_enter _ | Clause_exit _ -> ()
    | Iter_prem_enter _ | Iter_prem_exit _ -> ()
    | Prem_enter { prem; at = _ } ->
        if !config.level = Premise && is_authored_prem prem then
          State.enter_premise prem
    | Prem_exit { prem; at = _; success; bindings } ->
        if !config.level = Premise then (
          if not success then State.record_failing_prem prem;
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
