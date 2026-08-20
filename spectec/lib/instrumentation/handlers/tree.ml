(** Tree: buffers a top-level relation invocation and emits one ASCII derivation
    tree when it completes: conclusion on top, sub-derivations below as premises
    led by [--]. Unlike {!Trace}, backtracking is pruned to the rules that
    applied, yielding a clean derivation tree.

    A failed run is rendered like a successful one, with the rule that could not
    be completed crossed out.

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

let render_value (v : Il.Value.t) : string =
  Il.Print.string_of_value v |> normalize_whitespace

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
  let string_of_out = function Some v -> render_value v | None -> dim "?" in
  Il.Mode.render ~pad_brackets:true ~string_of_atom ~string_of_in:render_value
    ~string_of_out c

let render_call node =
  let args = List.map render_value node.inputs |> String.concat ", " in
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

(* Box-drawing dash so the bar renders as one connected line. *)
let render_bar n = String.concat "" (List.init n (fun _ -> "─"))

(* The cross is padded to the width of [--] so crossed and uncrossed siblings
   stay aligned. *)
let connector marked = if marked then alarm "✗" ^ "  " else dim "--" ^ " "

let rel_head node ~rule =
  match (!config.level, node.outcome) with
  | (Conclusion | Premise), Some (Rel_result { conclusion; _ }) ->
      let notation = render_judgment conclusion in
      [
        accent (render_tag node ~rule ^ ":");
        notation;
        dim (render_bar (measure_width notation));
      ]
  | _ -> [ accent (render_tag node ~rule) ]

let render_func_lines node =
  match (!config.level, node.outcome) with
  | Premise, Some (Func_result (Some v)) ->
      [ Format.sprintf "%s = %s" (render_call node) (render_value v) ]
  | _ -> [ "$" ^ node.id ]

let render_prem ~binding_env entry =
  (* Returning [None] would let the unparser print the variable's name; show [?]
     for an unbound variable instead. *)
  let values varid =
    match List.find_opt (fun (id, _) -> id.it = varid.it) binding_env with
    | Some (_, v) -> Some (render_value v)
    | None -> Some (dim "?")
  in
  match prov entry.prem with
  | Some (Il.Source el_prem) -> El.Unparse.string_of_prem ~values el_prem
  | _ -> Il.Print.string_of_prem entry.prem

let derivations_of node =
  if is_failed node then List.rev node.failures_rev else [ node.derivation ]

let print_lines out ~first_lead ~rest_prefix = function
  | [] -> ()
  | head :: rest ->
      Format.fprintf out "%s%s\n" first_lead head;
      List.iter (fun l -> Format.fprintf out "%s%s\n" rest_prefix l) rest

let rec print_node ~first_lead ~rest_prefix node out =
  match node.kind with
  | Rel -> print_rel ~first_lead ~rest_prefix node out
  | Func ->
      print_lines out ~first_lead ~rest_prefix (render_func_lines node);
      List.iter
        (print_child ~rest_prefix out)
        (List.rev node.derivation.children_rev)

and print_child ~rest_prefix out child =
  print_node
    ~first_lead:(rest_prefix ^ connector (is_failed child))
    ~rest_prefix:(rest_prefix ^ "   ") child out

and print_rel ~first_lead ~rest_prefix node out =
  match derivations_of node with
  | [ derivation ] ->
      print_lines out ~first_lead ~rest_prefix
        (rel_head node ~rule:derivation.rule);
      print_derivation_body ~rest_prefix node derivation out
  | derivations ->
      print_lines out ~first_lead ~rest_prefix (rel_head node ~rule:None);
      List.iter
        (fun derivation ->
          Format.fprintf out "%s%s\n"
            (rest_prefix ^ connector true)
            (accent (render_tag node ~rule:derivation.rule));
          print_derivation_body ~rest_prefix:(rest_prefix ^ "   ") node
            derivation out)
        derivations

and print_derivation_body ~rest_prefix node derivation out =
  let print_prem ?(marked = false) entry =
    let text = render_prem ~binding_env:node.binding_env entry in
    Format.fprintf out "%s%s\n" (rest_prefix ^ connector marked) text
  in
  let is_synthesized =
    match derivation.unmet_prem with
    | Some p -> not (is_authored_prem p)
    | None -> false
  in
  let entries = List.rev derivation.prems_rev in
  let last = List.length entries - 1 in
  List.iteri
    (fun i entry ->
      let is_culprit = Option.is_some derivation.unmet_prem && i = last in
      let show_as_text = is_culprit && is_synthesized in
      match (entry.prem.it, entry.subderiv) with
      | (Il.RelPr _ | Il.RelAssertPr { expect = true; _ }), Some subderiv
        when not show_as_text ->
          print_child ~rest_prefix out subderiv
      | _ -> print_prem ~marked:is_culprit entry)
    entries;
  let children =
    match !config.level with
    | Premise -> List.rev derivation.children_rev
    | Rule | Conclusion ->
        List.rev derivation.children_rev |> List.filter (fun c -> c.kind = Rel)
  in
  List.iter (print_child ~rest_prefix out) children

let print_root node =
  print_node ~first_lead:"" ~rest_prefix:"" node !fmt;
  Format.pp_print_flush !fmt ()

let pop_and_maybe_print ~outcome =
  match State.pop ~outcome with None -> () | Some root -> print_root root

(* === Handler module ================================================ *)

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
  let modes = [ `IL; `SL ]

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
  let modes_of_level = function
    | Premise -> [ `IL ]
    | Rule | Conclusion -> [ `IL; `SL ]

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
            modes = modes_of_level level;
            handler = make { level; output };
            output;
          }

  let checkpoint = None
end

let spec : Instrumentation_spec.Spec.t = (module Spec)
