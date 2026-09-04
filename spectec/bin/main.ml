open Spectec

let version = "0.1"
let ( let* ) = Result.bind

(* Commands *)

let elab_command =
  Core.Command.basic ~summary:"parse and elaborate a spec"
  @@
  let open Core.Command.Let_syntax in
  let open Core.Command.Param in
  let%map filenames = anon (sequence ("spec files" %: string))
  and color = Cli.Cli_args.Output.color_flag in
  fun () ->
    Cli.Error_handling.guard ~color ~on_ok:(fun spec_il ->
        Format.printf "%s\n" (Lang.Il.Print.string_of_spec spec_il))
    @@ fun () ->
    let* spec = parse_spec_files filenames in
    let* spec_il = elaborate spec in
    Ok spec_il

let unparse_roundtrip filenames =
  let* spec_el = parse_spec_files filenames in
  let printed = Lang.El.Unparse.string_of_spec spec_el in
  let* spec_el' =
    parse_spec_source { filename = "<roundtrip>"; contents = printed }
  in
  if Lang.El.Eq.eq_spec spec_el spec_el' then Ok ()
  else
    Error
      (Error.RoundtripError
         ( Common.Source.no_region,
           "pretty-printed output did not reparse to the same AST" ))

let unparse_command =
  Core.Command.basic
    ~summary:"parse a spec and print it in canonical EL form (drops comments)"
  @@
  let open Core.Command.Let_syntax in
  let open Core.Command.Param in
  let%map filenames = anon (sequence ("spec files" %: string))
  and roundtrip =
    flag "-r" no_arg
      ~doc:
        " verify the pretty-printed output reparses to the same AST (prints \
         nothing on success)"
  and color = Cli.Cli_args.Output.color_flag in
  fun () ->
    if roundtrip then
      Cli.Error_handling.guard_unit ~color (fun () ->
          unparse_roundtrip filenames)
    else
      Cli.Error_handling.guard ~color ~on_ok:(fun spec_el ->
          Format.printf "%s" (Lang.El.Unparse.string_of_spec spec_el))
      @@ fun () -> parse_spec_files filenames

let grammar_command =
  Core.Command.basic
    ~summary:"extract the object-language grammar reachable from a start symbol"
  @@
  let open Core.Command.Let_syntax in
  let open Core.Command.Param in
  let%map filenames = anon (sequence ("spec files" %: string))
  and start =
    flag "--start" (required string)
      ~doc:"SYMBOL syntax to extract the reachable grammar from"
  and color = Cli.Cli_args.Output.color_flag in
  fun () ->
    Cli.Error_handling.guard ~color ~on_ok:(fun spec_il ->
        Format.printf "%s\n"
          (Grammar.string_of_t (Grammar.extract ~start spec_il)))
    @@ fun () ->
    let* spec = parse_spec_files filenames in
    let* spec_il = elaborate spec in
    Ok spec_il

let structure_command =
  Core.Command.basic ~summary:"structure a spec"
  @@
  let open Core.Command.Let_syntax in
  let open Core.Command.Param in
  let%map filenames = anon (sequence ("spec files" %: string))
  and color = Cli.Cli_args.Output.color_flag in
  fun () ->
    Cli.Error_handling.guard ~color ~on_ok:(fun spec_sl ->
        Format.printf "%s\n" (Lang.Sl.Print.string_of_spec spec_sl))
    @@ fun () ->
    let* spec = parse_spec_files filenames in
    let* spec_il = elaborate spec in
    let spec_sl = structure spec_il in
    Ok spec_sl

let annotate_command =
  Core.Command.basic ~summary:"annotate a structured spec into PL form"
  @@
  let open Core.Command.Let_syntax in
  let open Core.Command.Param in
  let%map filenames = anon (sequence ("spec files" %: string))
  and color = Cli.Cli_args.Output.color_flag in
  fun () ->
    Cli.Error_handling.guard ~color ~on_ok:(fun spec_pl ->
        Format.printf "%s\n" (Pl.Print.string_of_spec spec_pl))
    @@ fun () ->
    let* spec = parse_spec_files filenames in
    let* spec_il = elaborate spec in
    let spec_sl = structure spec_il in
    let henv = henv_of_el_spec spec in
    let henv = henv_with_il_spec henv spec_il in
    let spec_pl = annotate ~henv spec_sl |> shorten in
    Ok spec_pl

(* Walks [root] recursively and returns every file path under it whose
   basename ends in one of [exts]. Paths are returned relative to [root]. *)
let collect_files ~exts root =
  let rec walk acc rel_dir =
    let entries = Sys.readdir (Filename.concat root rel_dir) in
    Array.sort String.compare entries;
    Array.fold_left
      (fun acc entry ->
        let rel_path = Filename.concat rel_dir entry in
        if Sys.is_directory (Filename.concat root rel_path) then
          walk acc rel_path
        else if List.exists (Filename.check_suffix entry) exts then
          rel_path :: acc
        else acc)
      acc entries
  in
  walk [] "" |> List.rev

let splice_command =
  Core.Command.basic
    ~summary:"splice rendered spec text into AsciiDoc skeletons"
  @@
  let open Core.Command.Let_syntax in
  let open Core.Command.Param in
  let%map filenames = anon (sequence ("spec files" %: string))
  and input_dir =
    flag "-i" (required string)
      ~doc:"DIR directory of .adoc skeleton files (walked recursively)"
  and output_dir =
    flag "-o" (required string)
      ~doc:"DIR directory to write spliced output (mirrors input layout)"
  and missing_path =
    flag "--missing" (optional string)
      ~doc:"FILE write the unused-keys report to this path"
  and color = Cli.Cli_args.Output.color_flag in
  fun () ->
    Cli.Error_handling.guard ~color ~on_ok:(fun (spec_el, spec_pl) ->
        let inputs = collect_files ~exts:[ ".adoc" ] input_dir in
        let pairs =
          List.map
            (fun rel_path ->
              ( Filename.concat input_dir rel_path,
                Filename.concat output_dir rel_path ))
            inputs
        in
        let report =
          Splice.Driver.run ~spec_el ~spec_pl
            ~source_entries:Splice.Registry.source
            ~prose_entries:Splice.Registry.prose ~filenames:pairs
        in
        match missing_path with
        | Some path ->
            let oc = open_out path in
            Fun.protect
              (fun () ->
                Out_channel.output_string oc (Splice.Report.to_string report))
              ~finally:(fun () -> Out_channel.close oc)
        | None -> ())
    @@ fun () ->
    let* spec = parse_spec_files filenames in
    let* spec_il = elaborate spec in
    let spec_sl = structure spec_il in
    let henv = henv_of_el_spec spec in
    let henv = henv_with_il_spec henv spec_il in
    let spec_pl = annotate ~henv spec_sl |> shorten in
    Ok (spec, spec_pl)

let command =
  let module P4 = Targets_p4.P4.Cli in
  let module Impty = Targets_impty.Impty.Cli in
  let module Miniml = Targets_miniml.Miniml.Cli in
  Core.Command.group ~summary:"SpecTec command line tools"
    [
      ("unparse", unparse_command);
      ("elab", elab_command);
      ("grammar", grammar_command);
      ("struct", structure_command);
      ("annotate", annotate_command);
      ("splice", splice_command);
      (P4.Target.name, P4.command);
      (Impty.Target.name, Impty.command);
      (Miniml.Target.name, Miniml.command);
    ]

let () = Command_unix.run ~version command
