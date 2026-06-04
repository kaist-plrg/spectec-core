(** impty target — typed imperative language.

    Two specs share one parser:
    - [specs/impty/base] — imperative core (arithmetic, conditionals, loops).
    - [specs/impty/closure] — adds function values and call expressions. *)

let collect_files_recursive ~suffix dir =
  let rec gather acc path =
    if Sys.file_exists path && Sys.is_directory path then (
      let entries = Sys.readdir path in
      Array.sort String.compare entries;
      Array.fold_left
        (fun acc name -> gather acc (Filename.concat path name))
        acc entries)
    else if Filename.check_suffix path suffix then path :: acc
    else acc
  in
  if Sys.file_exists dir then gather [] dir |> List.rev else []

let contains_substring s sub =
  try
    let _ = Str.search_forward (Str.regexp_string sub) s 0 in
    true
  with Not_found -> false

module Target : Spectec.Target.S = struct
  let name = "impty"
  let spec_dir = "spectec/specs/impty/base"

  (* The impty spec is fully self-contained: arithmetic, comparison, boolean,
     and list/map operations are SpecTec primitives or defined in the spec. *)
  let builtins : (string * Builtins.Define.t) list = []

  let handler f =
    let vid_counter = ref 0 in
    let fresh_vid () =
      let v = !vid_counter in
      incr vid_counter;
      v
    in
    Lang.Il.Value.GlobalVidProvider.set fresh_vid;
    f ()

  let is_impure_func _ = false
  let is_impure_rel _ = false
  let state_version = ref 0
end

type input = { filename : string; expect : Spectec.Task.expectation }

let collect_with ~classify ?dir () =
  match dir with
  | None -> []
  | Some test_dir ->
      collect_files_recursive ~suffix:".imp" test_dir
      |> List.map (fun filename -> { filename; expect = classify filename })

(* [_errors_X.imp] = static error (Check_prog fails); [_errors_runtime_X.imp] =
   runtime error (Check_prog succeeds, Eval_prog fails). The eval task gates on
   typecheck via the spec's Run_prog rule, so static errors fail eval too. *)
let typecheck_classify filename =
  if
    contains_substring filename "_errors"
    && not (contains_substring filename "_errors_runtime")
  then Spectec.Task.Negative
  else Spectec.Task.Positive

let eval_classify filename =
  if contains_substring filename "_errors" then Spectec.Task.Negative
  else Spectec.Task.Positive

module Task_common = struct
  module Target = Target

  type nonrec input = input

  let unparse = Parse.unparse
  let parse_string = Parse.parse_string
  let source ({ filename; _ } : input) = filename
  let expectation ({ expect; _ } : input) = expect
  let save_output _ _ = ()
end

module Typecheck = struct
  include Task_common

  let name = "typechecker"
  let collect = collect_with ~classify:typecheck_classify

  let parse_input ~spec:_ { filename; _ } =
    Parse.parse_file ~handler:Target.handler filename
    |> Result.map (fun v -> ("Check_prog", [ v ]))

  let format_output _ = "Typecheck succeeded"
end

module Eval = struct
  include Task_common

  let name = "evaluator"
  let collect = collect_with ~classify:eval_classify

  let parse_input ~spec:_ { filename; _ } =
    Parse.parse_file ~handler:Target.handler filename
    |> Result.map (fun v -> ("Run_prog", [ v ]))

  let format_output = function
    | [] -> "Eval succeeded (no output)"
    | vs -> vs |> List.map Lang.Il.Print.string_of_value |> String.concat ", "
end

let cli_flags =
  let open Core.Command.Let_syntax in
  let open Core.Command.Param in
  let%map filename = flag "-p" (required string) ~doc:"FILE impty file" in
  { filename; expect = Spectec.Task.Positive }

module Typecheck_cli : Cli.Task_cli.S = struct
  module Task = Typecheck

  let flags = cli_flags
end

module Eval_cli : Cli.Task_cli.S = struct
  module Task = Eval

  let flags = cli_flags
end

let quickcheck_command =
  Core.Command.basic
    ~summary:"run quickcheck properties declared in an impty spec"
  @@
  let open Core.Command.Let_syntax in
  let open Core.Command.Param in
  let%map cli_source = Cli.Cli_args.Spec.source_flag
  and generalize =
    flag "--generalize" no_arg
      ~doc:" generalize counterexamples after shrinking"
  and max_steps =
    flag "--max-steps"
      (optional_with_default 100 int)
      ~doc:"N max steps per relation evaluation (default 100)"
  and num_tests =
    flag "--num-tests"
      (optional_with_default 100 int)
      ~doc:"N number of test cases to generate (default 100)"
  and config = Cli.Cli_args.Interpreter.config_flags
  and color = Cli.Cli_args.Output.color_flag in
  fun () ->
    Cli.Error_handling.guard_unit ~color @@ fun () ->
    (* Alias before [open Spectec] so [(module T)] packs impty's Target rather
       than the Spectec.Target module type holder. *)
    let module T = Target in
    let open Spectec in
    let ( let* ) = Result.bind in
    let* cfg = Cli.Config_file.load ~target:T.name () in
    let source =
      match cli_source with
      | Some s -> s
      | None ->
          Option.value cfg.Cli.Config_file.spec_source
            ~default:(Cli.Spec_source.Dir T.spec_dir)
    in
    let* filenames = Cli.Spec_source.files source in
    let* spec = parse_spec_files filenames in
    let* { lang; qc } = elaborate spec in
    Instrumentation.with_instrumentation config
      (Instrumentation.Static.IlSpec lang)
    @@ fun () ->
    Quickcheck.Driver.check
      ~target:(module T)
      ~generalize ~max_steps ~num_tests ~manual_gens:Manual_gen.manual_gens lang
      qc
    |> Result.map_error (fun e ->
           Error.QuickcheckError (Quickcheck.Driver.error_to_string e))

module Cli : Cli.Target_cli.S = struct
  module Target = Target

  let name = Target.name

  let command =
    let target = (module Target : Spectec.Target.S) in
    let module Subcommand = Cli.Subcommand in
    Core.Command.group ~summary:"impty commands"
      [
        Subcommand.make_task target ~name:"typecheck"
          ~summary:"Run impty typechecker"
          (module Typecheck_cli);
        Subcommand.make_task target ~name:"eval" ~summary:"Run impty evaluator"
          (module Eval_cli);
        Subcommand.make_parse target ~name:"parse"
          ~summary:"parse an impty program to an IL value"
          (module Typecheck_cli);
        Subcommand.make_batch target ~name:"batch"
          [ (module Typecheck_cli); (module Eval_cli) ];
        Subcommand.make_checkpoint target ~name:"checkpoint";
        ("quickcheck", quickcheck_command);
      ]
end
