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

type outcome =
  | Failed
  | Rel_ok of (Il.Value.t, Il.Value.t) Il.Mode.t
  | Func_ok of Il.Value.t

type node = {
  kind : kind;
  id : string;
  inputs : Il.Value.t list;
  mutable rule : string option;
  mutable outcome : outcome;
  mutable children_rev : node list;
  mutable rollback_children : node list option;
  (* Premise level only. *)
  mutable prems_rev : prem_entry list;
  mutable pending_prem : prem_entry option;
  mutable rollback_prems : prem_entry list option;
  (* Variable values across all the rule's premises; synthesized sideconditions
     are included because they bind an author premise's output variables. *)
  mutable binding_env : (Il.id * Il.Value.t) list;
}

and prem_entry = { prem : Il.prem; mutable subderiv : node option }

let new_node kind id inputs =
  {
    kind;
    id;
    inputs;
    rule = None;
    outcome = Failed;
    children_rev = [];
    rollback_children = None;
    prems_rev = [];
    pending_prem = None;
    rollback_prems = None;
    binding_env = [];
  }

let outcome_of_conclusion = function Some c -> Rel_ok c | None -> Failed
let outcome_of_output = function Some v -> Func_ok v | None -> Failed

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
        current.rollback_children <- Some current.children_rev;
        current.rollback_prems <- Some current.prems_rev)

  let end_rule_attempt ~rule_id ~success =
    with_current (fun current ->
        if success then current.rule <- Some rule_id
        else (
          Option.iter
            (fun saved -> current.children_rev <- saved)
            current.rollback_children;
          Option.iter
            (fun saved -> current.prems_rev <- saved)
            current.rollback_prems);
        current.rollback_children <- None;
        current.rollback_prems <- None)

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
end

(* === Rendering ===================================================== *)

let dim s = Ansi.style !ansi [ Dim ] s
let accent s = Ansi.style !ansi [ Yellow ] s

let render_judgment c =
  let string_of_atom a =
    match Il.Print.string_of_atom a with "" -> "" | s -> dim s
  in
  Il.Mode.render ~pad_brackets:true ~string_of_atom
    ~string_of_in:summarize_value ~string_of_out:summarize_value c

let render_call node =
  let args = List.map summarize_value node.inputs |> String.concat ", " in
  Format.sprintf "$%s(%s)" node.id args

let render_tag node =
  match node.rule with Some r when r <> "" -> node.id ^ "/" ^ r | _ -> node.id

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

let render_lines node =
  match (node.kind, !config.level, node.outcome) with
  | Rel, (Conclusion | Premise), Rel_ok c ->
      let notation = render_judgment c in
      [
        accent (render_tag node ^ ":");
        notation;
        dim (render_bar (measure_width notation));
      ]
  | Rel, _, _ -> [ accent (render_tag node) ]
  | Func, Premise, Func_ok v ->
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
        (List.rev node.prems_rev);
      List.iter print_child (List.rev node.children_rev)
  | Rule | Conclusion ->
      List.rev node.children_rev
      |> List.iter (fun child ->
             match child.kind with Rel -> print_child child | Func -> ())

let print_root node =
  print_node ~first_lead:"" ~rest_prefix:"" node !fmt;
  Format.pp_print_flush !fmt ()

let pop_and_maybe_print ~outcome =
  match State.pop ~outcome with
  | Some { outcome = Failed; _ } | None -> ()
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
    | Rel_exit { id = _; at = _; conclusion } ->
        pop_and_maybe_print ~outcome:(outcome_of_conclusion conclusion)
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
    | Prem_exit { prem; at = _; success = _; bindings } ->
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
