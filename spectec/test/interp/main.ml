(** Interpreter test - Generic test runner for IL/SL/PL using TASK *)

open Core
open Test_lib

(** Generic test runner - works with any TASK, in any interpreter mode *)
let run_with_task (type i) (module T : Spectec.Task.S with type input = i)
    ~(request : Spectec.Interp_mode.request) ~spec_files ~inputs ~exclude_dirs =
  let mode_name = Spectec.Interp_mode.to_string request in
  let open Core.Result.Let_syntax in
  let suite_result =
    let%bind spec = Spectec.parse_spec_files spec_files in
    let%bind spec_il = Spectec.elaborate spec in
    let mode : Spectec.Interp_mode.t =
      match request with
      | `IL -> Il
      | `SL -> Sl
      | `PL ->
          Pl (Spectec.henv_with_il_spec (Spectec.henv_of_el_spec spec) spec_il)
    in
    let exclude_set = Exclude.load exclude_dirs in
    let mode_suffix = "(" ^ String.lowercase mode_name ^ ")" in
    let config : Suite.config =
      {
        name = T.name ^ " " ^ mode_suffix |> String.capitalize;
        intro = "Running " ^ T.name ^ " test on";
        heading = T.name ^ " test";
        success = T.name ^ " success" |> String.capitalize;
        failure = T.name ^ " failed" |> String.capitalize;
        expected_failure = "Expected " ^ T.name ^ " failure";
        unexpected_success = "Unexpected " ^ T.name ^ " success";
      }
    in
    let filenames = List.map inputs ~f:(fun i -> T.source i) in
    let input_table =
      List.fold inputs
        ~init:(Map.empty (module String))
        ~f:(fun acc input -> Map.set acc ~key:(T.source input) ~data:input)
    in
    let run filename =
      match Map.find input_table filename with
      | None -> failwith ("T not found: " ^ filename)
      | Some input ->
          let%bind _ =
            Spectec.eval_task_with_instrumentation
              (module T)
              ~mode ~spec_il input
          in
          Ok ()
    in
    let expectation =
      match inputs with
      | [] -> Spectec.Task.Positive
      | i :: _ -> T.expectation i
    in
    Suite.run ~config ~exclude_set ~filenames ~expectation ~run;
    Ok ()
  in
  match suite_result with
  | Ok () -> ()
  | Error err ->
      Format.printf "Failed to run %s interpreter:\n%s\n" mode_name
        (Spectec.Diagnostic.Render.render_bag
           ~ansi:Spectec.Diagnostic.Ansi.plain
           (Spectec.Error.to_diagnostics err))

(** P4 Typecheck test - uses P4_Target.spec_dir *)
let run_p4_typecheck ~p4_old ~negative ~request ~includes ~exclude_dirs
    ~testdirs =
  let expectation =
    if negative then Spectec.Task.Negative else Spectec.Task.Positive
  in
  (* Prefix for dune test which runs from spectec/_build/default/test/interp *)
  let repo_root = "../../../../../" in
  if p4_old then
    let spec_dir = repo_root ^ Targets_p4.P4.Target_old.spec_dir in
    let spec_files = Files.collect ~suffix:".spectec" spec_dir in
    let inputs =
      List.concat_map testdirs ~f:(fun dir ->
          Targets_p4.P4.Typecheck_old.collect ~dir ())
      |> List.map ~f:(fun input ->
             {
               Targets_p4.P4.Typecheck_old.includes;
               filename = Targets_p4.P4.Typecheck_old.source input;
               expect = expectation;
             })
    in
    run_with_task
      (module Targets_p4.P4.Typecheck_old)
      ~request ~spec_files ~inputs ~exclude_dirs
  else
    let spec_dir = repo_root ^ Targets_p4.P4.Target.spec_dir in
    let spec_files = Files.collect ~suffix:".spectec" spec_dir in
    let inputs =
      List.concat_map testdirs ~f:(fun dir ->
          Targets_p4.P4.Typecheck.collect ~dir ())
      |> List.map ~f:(fun input ->
             {
               Targets_p4.P4.Typecheck.includes;
               filename = Targets_p4.P4.Typecheck.source input;
               expect = expectation;
             })
    in
    run_with_task
      (module Targets_p4.P4.Typecheck)
      ~request ~spec_files ~inputs ~exclude_dirs

(** Impty Typecheck test - per-variant spec dispatch *)
let run_impty_typecheck ~variant ~negative ~request ~testdirs =
  let expectation =
    if negative then Spectec.Task.Negative else Spectec.Task.Positive
  in
  let repo_root = "../../../../../" in
  let spec_dir = repo_root ^ "spectec/specs/impty/" ^ variant in
  let spec_files = Files.collect ~suffix:".spectec" spec_dir in
  let inputs =
    List.concat_map testdirs ~f:(fun dir ->
        Targets_impty.Impty.Typecheck.collect ~dir ())
    |> List.filter ~f:(fun input ->
           match
             (Targets_impty.Impty.Typecheck.expectation input, expectation)
           with
           | Spectec.Task.Positive, Spectec.Task.Positive -> true
           | Spectec.Task.Negative, Spectec.Task.Negative -> true
           | _ -> false)
  in
  run_with_task
    (module Targets_impty.Impty.Typecheck)
    ~request ~spec_files ~inputs ~exclude_dirs:[]

let command =
  Command.basic ~summary:"run interpreter typing test (IL, SL, or PL)"
  @@
  let open Command.Let_syntax in
  let open Command.Param in
  let%map includes = flag "-i" (listed string) ~doc:"DIR include paths"
  and exclude_dirs = flag "-e" (listed string) ~doc:"DIR exclude paths"
  and testdirs =
    flag "-d" (listed string) ~doc:"DIR test directory (repeatable)"
  and negative = flag "-neg" no_arg ~doc:" expect failures (negative mode)"
  and sl_mode = flag "--sl" no_arg ~doc:" use SL interpreter (default: IL)"
  and pl_mode = flag "--pl" no_arg ~doc:" use PL interpreter (default: IL)"
  and p4_old = flag "--p4-old" no_arg ~doc:" use p4-old target (default: p4)"
  and impty = flag "--impty" no_arg ~doc:" use impty target (default: p4)"
  and variant =
    flag "--variant" (optional string)
      ~doc:
        "VARIANT impty variant (base|closure|recursion); required with --impty"
  in
  fun () ->
    let request : Spectec.Interp_mode.request =
      match (sl_mode, pl_mode) with
      | false, false -> `IL
      | true, false -> `SL
      | false, true -> `PL
      | true, true -> failwith "--sl and --pl are mutually exclusive"
    in
    if impty then
      let v =
        match variant with
        | Some v -> v
        | None -> failwith "--variant is required when using --impty"
      in
      run_impty_typecheck ~variant:v ~negative ~request ~testdirs
    else
      run_p4_typecheck ~p4_old ~negative ~request ~includes ~exclude_dirs
        ~testdirs

let () = Command_unix.run command
