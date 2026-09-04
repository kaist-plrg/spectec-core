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
  let name = "miniml"
  let spec_dir = "spectec/specs/miniml"
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
      collect_files_recursive ~suffix:".ml" test_dir
      |> List.map (fun filename -> { filename; expect = classify filename })

let eval_classify filename =
  if contains_substring filename "_error" then Spectec.Task.Negative
  else Spectec.Task.Positive

module Task_common = struct
  module Target = Target

  type nonrec input = input

  let unparse = Unparse.unparse
  let parse_string = Lexer.parse_string
  let source ({ filename; _ } : input) = filename
  let expectation ({ expect; _ } : input) = expect
  let save_output _ _ = ()
end

module Eval = struct
  include Task_common

  let name = "evaluator"
  let collect = collect_with ~classify:eval_classify

  let parse_input ~spec:_ { filename; _ } =
    Lexer.parse_file ~handler:Target.handler filename
    |> Result.map (fun v -> ("Eval_expr", [ v ]))

  let format_output = function
    | [ v ] -> Unparse.print_expr v
    | [] -> "Eval succeeded (no output)"
    | vs -> vs |> List.map Lang.Il.Print.string_of_value |> String.concat ", "
end

let cli_flags =
  let open Core.Command.Let_syntax in
  let open Core.Command.Param in
  let%map filename = flag "-p" (required string) ~doc:"FILE Mini-ML file" in
  { filename; expect = Spectec.Task.Positive }

module Eval_cli : Cli.Task_cli.S = struct
  module Task = Eval

  let flags = cli_flags
end

module Cli : Cli.Target_cli.S = struct
  module Target = Target

  let command =
    let target = (module Target : Spectec.Target.S) in
    let module Subcommand = Cli.Subcommand in
    Core.Command.group ~summary:"Mini-ML commands"
      [
        Subcommand.make_task target ~name:"eval"
          ~summary:"Run Mini-ML evaluator"
          (module Eval_cli);
        Subcommand.make_parse target ~name:"parse"
          ~summary:"Parse a Mini-ML program to an IL value"
          (module Eval_cli);
        Subcommand.make_checkpoint target ~name:"checkpoint";
      ]
end
