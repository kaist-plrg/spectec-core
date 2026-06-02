(** EL -> IL -> EL premise roundtrip: for each IL rule/clause, recover its EL
    premises and assert they match what the author wrote, catching a dropped,
    reordered, or mistagged premise. EL and IL pair positionally within an id,
    since rule ids and clause defids both repeat and cannot key the pairing. *)

open Core
open Test_lib
module Source = Common.Source
module El = Lang.El
module Il = Lang.Il

let recover (prems : Il.prem list) : El.prem list =
  List.filter_map prems ~f:(fun prem ->
      match Source.prov prem with Some (Il.Source el) -> Some el | _ -> None)

let authored_rule_premises (spec : El.spec) relid : El.prem list list =
  List.filter_map spec ~f:(fun def ->
      match Source.it def with
      | El.RuleD (id, _, _, prems) when String.equal (Source.it id) relid ->
          Some prems
      | _ -> None)

let authored_clause_premises (spec : El.spec) defid : El.prem list list =
  List.filter_map spec ~f:(fun def ->
      match Source.it def with
      | El.DefD (id, _, _, _, prems) when String.equal (Source.it id) defid ->
          Some prems
      | _ -> None)

type mismatch_kind = Structural | Content

let string_of_mismatch_kind = function
  | Structural -> "STRUCTURAL"
  | Content -> "CONTENT"

let find_mismatch ~label ~authored ~recovered =
  match authored with
  | None ->
      Some
        (sprintf "%s: %s (no authored rule/clause)" label
           (string_of_mismatch_kind Structural))
  | Some authored ->
      let authored_str = El.Print.string_of_prems authored
      and recovered_str = El.Print.string_of_prems recovered in
      if String.equal authored_str recovered_str then None
      else
        let kind =
          if List.length authored = List.length recovered then Content
          else Structural
        in
        Some
          (sprintf "%s: %s\n  authored:%s\n  recovered:%s" label
             (string_of_mismatch_kind kind)
             authored_str recovered_str)

type tally = {
  rules_checked : int;
  clauses_checked : int;
  failures : string list;
}

let check_def (spec : El.spec) (tally : tally) (def : Il.def) : tally =
  match Source.it def with
  | Il.RelD { relid; rules; _ } ->
      let authored = authored_rule_premises spec (Source.it relid) in
      let failures =
        List.filter_mapi rules ~f:(fun i rule ->
            let { Il.ruleid; prems; _ } = Source.it rule in
            find_mismatch
              ~label:(sprintf "%s/%s" (Source.it relid) (Source.it ruleid))
              ~authored:(List.nth authored i) ~recovered:(recover prems))
      in
      {
        tally with
        rules_checked = tally.rules_checked + List.length rules;
        failures = tally.failures @ failures;
      }
  | Il.DecD { defid; clauses; _ } ->
      let authored = authored_clause_premises spec (Source.it defid) in
      let failures =
        List.filter_mapi clauses ~f:(fun i clause ->
            let { Il.prems; _ } = Source.it clause in
            find_mismatch
              ~label:(sprintf "%s clause %d" (Source.it defid) i)
              ~authored:(List.nth authored i) ~recovered:(recover prems))
      in
      {
        tally with
        clauses_checked = tally.clauses_checked + List.length clauses;
        failures = tally.failures @ failures;
      }
  | _ -> tally

let run specdir =
  let result =
    let open Result.Let_syntax in
    let spec_files = Files.collect ~suffix:".spectec" specdir in
    let%bind spec = Spectec.parse_spec_files spec_files in
    let%bind spec_il = Spectec.elaborate spec in
    return (spec, spec_il)
  in
  match result with
  | Ok (spec, spec_il) ->
      let { rules_checked; clauses_checked; failures } =
        List.fold spec_il
          ~init:{ rules_checked = 0; clauses_checked = 0; failures = [] }
          ~f:(check_def spec)
      in
      List.iter failures ~f:(printf "%s\n");
      if List.is_empty failures then
        printf "checked %d rules, %d clauses; all premises recovered\n"
          rules_checked clauses_checked
      else
        printf "checked %d rules, %d clauses; %d mismatch(es)\n" rules_checked
          clauses_checked (List.length failures)
  | Error err ->
      printf "Elaboration failed:\n%s\n"
        (Spectec.Diagnostic.Render.render_bag
           ~ansi:Spectec.Diagnostic.Ansi.plain
           (Spectec.Error.to_diagnostics err))

let command =
  Command.basic ~summary:"run premise-provenance roundtrip test"
  @@
  let open Command.Let_syntax in
  let open Command.Param in
  let%map specdir = flag "-s" (required string) ~doc:"DIR spec directory" in
  fun () -> run specdir

let () = Command_unix.run command
