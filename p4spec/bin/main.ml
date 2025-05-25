open Util.Error

let version = "0.1"

(* File collector *)

let rec collect_files ~(suffix : string) dir =
  let files = Sys_unix.readdir dir in
  Array.sort String.compare files;
  Array.fold_left
    (fun files file ->
      let filename = dir ^ "/" ^ file in
      if Sys_unix.is_directory_exn filename && file <> "include" then
        files @ collect_files ~suffix filename
      else if String.ends_with ~suffix filename then files @ [ filename ]
      else files)
    [] files

(* Exclude collector *)

let collect_exclude filename_exclude =
  let ic = open_in filename_exclude in
  let rec parse_lines excludes =
    try
      let exclude = input_line ic in
      if String.starts_with ~prefix:"#" exclude then parse_lines excludes
      else parse_lines (exclude :: excludes)
    with End_of_file -> excludes
  in
  let excludes = parse_lines [] in
  close_in ic;
  excludes

let collect_excludes (paths_exclude : string list) =
  let filenames_exclude =
    List.concat_map (collect_files ~suffix:".exclude") paths_exclude
  in
  List.concat_map collect_exclude filenames_exclude

(* Commands *)

let elab_command =
  Core.Command.basic ~summary:"parse and elaborate a p4_16 spec"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map filenames = anon (sequence ("filename" %: string)) in
     fun () ->
       try
         let spec = List.concat_map Frontend.Parse.parse_file filenames in
         let spec_il = Elaborate.Elab.elab_spec spec in
         Format.printf "%s\n" (Il.Print.string_of_spec spec_il);
         ()
       with
       | ParseError (at, msg) -> Format.printf "%s\n" (string_of_error at msg)
       | ElabError (at, msg) -> Format.printf "%s\n" (string_of_error at msg))

let stat_def_el (count_prems : int) (def : El.Ast.def) : int =
  match def.it with
  | RuleD (_, _, _, prems) ->
      Format.printf "[Rule] Prems: %d\n" (List.length prems);
      count_prems + List.length prems
  | DefD (_, _, _, _, prems) ->
      Format.printf "[Def] Prems: %d\n" (List.length prems);
      count_prems + List.length prems
  | _ -> count_prems

let stat_spec_el (spec : El.Ast.spec) : unit =
  let count_prems = List.fold_left stat_def_el 0 spec in
  Format.printf "[Total] Prems: %d\n" count_prems

let stat_def_il (count_rels : int) (count_rules : int) (count_decs : int)
    (count_defs : int) (def : Il.Ast.def) : int * int * int * int =
  match def.it with
  | RelD (id, _, _, rules) ->
      Format.printf "[Rel %s] Rules: %d\n" id.it (List.length rules);
      let count_rels = count_rels + 1 in
      let count_rules = count_rules + List.length rules in
      (count_rels, count_rules, count_decs, count_defs)
  | DecD (id, _, _, _, clauses) ->
      Format.printf "[Func %s] Defs: %d\n" id.it (List.length clauses);
      let count_decs = count_decs + 1 in
      let count_defs = count_defs + List.length clauses in
      (count_rels, count_rules, count_decs, count_defs)
  | _ -> (count_rels, count_rules, count_decs, count_defs)

let stat_spec_il (spec : Il.Ast.spec) : unit =
  let count_rels, count_rules, count_decs, count_defs =
    List.fold_left
      (fun (count_rels, count_rules, count_decs, count_defs) def ->
        stat_def_il count_rels count_rules count_decs count_defs def)
      (0, 0, 0, 0) spec
  in
  Format.printf "[Total] Rels: %d, Rules: %d, Decs: %d, Defs: %d\n" count_rels
    count_rules count_decs count_defs

let stat_command =
  Core.Command.basic ~summary:"insert structured control flow to a p4_16 spec"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map filenames = anon (sequence ("filename" %: string)) in
     fun () ->
       try
         let spec = List.concat_map Frontend.Parse.parse_file filenames in
         stat_spec_el spec;
         let spec_il = Elaborate.Elab.elab_spec spec in
         stat_spec_il spec_il;
         ()
       with
       | ParseError (at, msg) -> Format.printf "%s\n" (string_of_error at msg)
       | ElabError (at, msg) -> Format.printf "%s\n" (string_of_error at msg))

let struct_command =
  Core.Command.basic ~summary:"insert structured control flow to a p4_16 spec"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map filenames = anon (sequence ("filename" %: string)) in
     fun () ->
       try
         let spec = List.concat_map Frontend.Parse.parse_file filenames in
         let spec_il = Elaborate.Elab.elab_spec spec in
         let spec_sl = Structure.Struct.struct_spec spec_il in
         Format.printf "%s\n" (Sl.Print.string_of_spec spec_sl);
         ()
       with
       | ParseError (at, msg) -> Format.printf "%s\n" (string_of_error at msg)
       | ElabError (at, msg) -> Format.printf "%s\n" (string_of_error at msg))

let run_sl_command =
  Core.Command.basic
    ~summary:"run static semantics of a p4_16 spec based on non-backtracking SL"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map filenames_spec = anon (sequence ("filename" %: string))
     and includes_p4 = flag "-i" (listed string) ~doc:"p4 include paths"
     and filename_p4 = flag "-p" (required string) ~doc:"p4 file to typecheck"
     and derive = flag "-derive" no_arg ~doc:"derive value dependency graph"
     and filenames_ignore =
       flag "-ignore" (listed string)
         ~doc:"relations or functions to ignore when reporting coverage"
     and mini =
       flag "-mini" no_arg ~doc:"run mini-typing instead of full typing"
     in
     fun () ->
       try
         let spec = List.concat_map Frontend.Parse.parse_file filenames_spec in
         let spec_il = Elaborate.Elab.elab_spec spec in
         let spec_sl = Structure.Struct.struct_spec spec_il in
         match
           Interp_sl.Typing.run_typing ~mini ~derive spec_sl includes_p4
             filename_p4 filenames_ignore
         with
         | WellTyped _ -> Format.printf "well-typed\n"
         | IllTyped (_, msg, _) -> Format.printf "ill-typed: %s\n" msg
         | IllFormed (msg, _) -> Format.printf "ill-formed: %s\n" msg
       with
       | ParseError (at, msg) -> Format.printf "%s\n" (string_of_error at msg)
       | ElabError (at, msg) -> Format.printf "%s\n" (string_of_error at msg))

let cover_sl_command =
  Core.Command.basic ~summary:"measure phantom coverage of SL"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map filenames_spec = anon (sequence ("filename" %: string))
     and includes_p4 = flag "-i" (listed string) ~doc:"p4 include paths"
     and excludes_p4 = flag "-e" (listed string) ~doc:"p4 test exclude paths"
     and dirnames_p4 =
       flag "-d" (listed string) ~doc:"p4 directories to typecheck"
     and filenames_ignore =
       flag "-ignore" (listed string)
         ~doc:"relations or functions to ignore when reporting coverage"
     and filename_cov =
       flag "-cov" (required string) ~doc:"output coverage file"
     and mini =
       flag "-mini" no_arg ~doc:"run mini-typing instead of full typing"
     in
     fun () ->
       try
         let spec = List.concat_map Frontend.Parse.parse_file filenames_spec in
         let spec_il = Elaborate.Elab.elab_spec spec in
         let spec_sl = Structure.Struct.struct_spec spec_il in
         let excludes_p4 = collect_excludes excludes_p4 in
         let filenames_p4 =
           List.concat_map (collect_files ~suffix:".p4") dirnames_p4
         in
         let filenames_p4 =
           List.filter
             (fun filename_p4 ->
               not (List.exists (String.equal filename_p4) excludes_p4))
             filenames_p4
         in
         let cover =
           Interp_sl.Typing.cover_typings ~mini spec_sl includes_p4 filenames_p4
             filenames_ignore
         in
         Runtime_testgen.Cov.Multiple.log ~filename_cov_opt:(Some filename_cov)
           cover
       with
       | ParseError (at, msg) -> Format.printf "%s\n" (string_of_error at msg)
       | ElabError (at, msg) -> Format.printf "%s\n" (string_of_error at msg))

let run_testgen_command =
  Core.Command.basic
    ~summary:"generate negative type checker tests from a p4_16 spec"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map filenames_spec = anon (sequence ("filename" %: string))
     and fuel = flag "-fuel" (required int) ~doc:"fuel for test generation"
     and includes_p4 = flag "-i" (listed string) ~doc:"p4 include paths"
     and excludes_p4 = flag "-e" (listed string) ~doc:"p4 test exclude paths"
     and filenames_ignore =
       flag "-ignore" (listed string)
         ~doc:"relations or functions to ignore when reporting coverage"
     and dirname_gen =
       flag "-gen" (required string) ~doc:"directory for generated p4 programs"
     and name_campaign =
       flag "-name" (optional string)
         ~doc:"name of the test generation campaign"
     and silent = flag "-silent" no_arg ~doc:"do not print logs to stdout"
     and randseed =
       flag "-seed" (optional int) ~doc:"seed for random number generator"
     and dirname_cold_boot =
       flag "-cold" (optional string) ~doc:"seed p4 directory for cold boot"
     and filename_boot =
       flag "-warm" (optional string) ~doc:"coverage file for warm boot"
     and random = flag "-random" no_arg ~doc:"randomize AST selection"
     and hybrid =
       flag "-hybrid" no_arg
         ~doc:"randomize AST selection when no derivations exist"
     and strict =
       flag "-strict" no_arg
         ~doc:"cover a new phantom only if it was intended by a mutation"
     and mini =
       flag "-mini" no_arg ~doc:"run mini-typing instead of full typing"
     in
     fun () ->
       try
         let spec = List.concat_map Frontend.Parse.parse_file filenames_spec in
         let spec_il = Elaborate.Elab.elab_spec spec in
         let spec_sl = Structure.Struct.struct_spec spec_il in
         let logmode =
           if silent then Testgen.Modes.Silent else Testgen.Modes.Verbose
         in
         let bootmode =
           match (dirname_cold_boot, filename_boot) with
           | Some dirname_cold_boot, None ->
               Testgen.Modes.Cold (excludes_p4, dirname_cold_boot)
           | None, Some filename_boot -> Testgen.Modes.Warm filename_boot
           | Some _, Some _ ->
               Format.asprintf
                 "Error: should specify only one of -cold or -warm\n"
               |> failwith
           | None, None ->
               Format.asprintf "Error: should specify either -cold or -warm\n"
               |> failwith
         in
         let mutationmode =
           if random then Testgen.Modes.Random
           else if hybrid then Testgen.Modes.Hybrid
           else Testgen.Modes.Derive
         in
         let covermode =
           if strict then Testgen.Modes.Strict else Testgen.Modes.Relaxed
         in
         Testgen.Gen.fuzz_typing ~mini fuel spec_sl includes_p4 filenames_ignore
           dirname_gen name_campaign randseed logmode bootmode mutationmode
           covermode
       with
       | ParseError (at, msg) -> Format.printf "%s\n" (string_of_error at msg)
       | ElabError (at, msg) -> Format.printf "%s\n" (string_of_error at msg))

let run_testgen_debug_command =
  Core.Command.basic
    ~summary:"debug close-AST deriver in negative type checker generator"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map filenames_spec = anon (sequence ("filename" %: string))
     and includes_p4 = flag "-i" (listed string) ~doc:"p4 include paths"
     and filename_p4 = flag "-p" (required string) ~doc:"p4 file to typecheck"
     and filenames_ignore =
       flag "-ignore" (listed string)
         ~doc:"relations or functions to ignore when reporting coverage"
     and dirname_debug =
       flag "-debug" (required string) ~doc:"directory for debug files"
     and pid = flag "-pid" (required int) ~doc:"phantom id to close-miss"
     and mini =
       flag "-mini" no_arg ~doc:"run mini-typing instead of full typing"
     in
     fun () ->
       try
         let spec = List.concat_map Frontend.Parse.parse_file filenames_spec in
         let spec_il = Elaborate.Elab.elab_spec spec in
         let spec_sl = Structure.Struct.struct_spec spec_il in
         Testgen.Derive.debug_phantom ~mini spec_sl includes_p4 filename_p4
           filenames_ignore dirname_debug pid
       with
       | ParseError (at, msg) -> Format.printf "%s\n" (string_of_error at msg)
       | ElabError (at, msg) -> Format.printf "%s\n" (string_of_error at msg))

let interesting_command =
  Core.Command.basic ~summary:"interestingness test for reducing p4_16 programs"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map filenames_spec = anon (sequence ("filename" %: string))
     and includes_p4 = flag "-i" (listed string) ~doc:"p4 include paths"
     and check_well_typed =
       flag "-well" no_arg
         ~doc:"'interesting' if well-typed (default: ill-typed)"
     and check_close_miss =
       flag "-close" no_arg ~doc:"'interesting' if close-miss (default: hit)"
     and pid = flag "-pid" (required int) ~doc:"phantom id to test"
     and filename_p4 = flag "-p" (required string) ~doc:"p4 file to typecheck"
     and dbg = flag "-dbg" no_arg ~doc:"print single coverage"
     and filenames_ignore =
       flag "-ignore" (listed string)
         ~doc:"relations or functions to ignore when reporting coverage"
     and mini =
       flag "-mini" no_arg ~doc:"run mini-typing instead of full typing"
     in
     fun () ->
       try
         let spec = List.concat_map Frontend.Parse.parse_file filenames_spec in
         let spec_il = Elaborate.Elab.elab_spec spec in
         let spec_sl = Structure.Struct.struct_spec spec_il in
         let typing_result =
           Interp_sl.Typing.run_typing ~mini spec_sl includes_p4 filename_p4
             filenames_ignore
         in
         if dbg then
           match typing_result with
           | IllTyped (_, _, cover_single) | WellTyped (_, _, cover_single) ->
               Interp_sl.Interp.SCov.Cover.iter
                 (fun pid (branch : Interp_sl.Interp.SCov.Branch.t) ->
                   match branch.status with
                   | Hit -> Printf.printf "%d Hit\n" pid
                   | Miss [] -> Printf.printf "%d Miss\n" pid
                   | Miss _ -> Printf.printf "%d Close\n" pid)
                 cover_single
           | _ -> ()
         else ();
         match typing_result with
         | WellTyped (_, _, cover_single) ->
             if check_well_typed then (
               let branch = Interp_sl.Interp.SCov.Cover.find pid cover_single in
               match branch.status with
               | Hit ->
                   Printf.printf "WellTyped: Hit\n";
                   if check_close_miss then exit 3 else exit 0
               | Miss (_ :: _) ->
                   Printf.printf "WellTyped: Close\n";
                   if check_close_miss then exit 0 else exit 2
               | Miss [] ->
                   Printf.printf "WellTyped: Miss\n";
                   exit 1)
             else (
               Printf.printf "WellTyped\n";
               exit 11)
         | IllTyped (_, _, cover_single) -> (
             if check_well_typed then (
               Printf.printf "IllTyped\n";
               exit 10)
             else
               let branch = Interp_sl.Interp.SCov.Cover.find pid cover_single in
               match branch.status with
               | Hit ->
                   Printf.printf "IllTyped: Hit\n";
                   if check_close_miss then exit 3 else exit 0
               | Miss (_ :: _) ->
                   Printf.printf "IllTyped: Close\n";
                   if check_close_miss then exit 0 else exit 2
               | Miss [] ->
                   Printf.printf "IllTyped: Miss\n";
                   exit 1)
         | IllFormed _ ->
             Printf.printf "IllFormed";
             exit 12
       with
       | ParseError (at, msg) -> Format.printf "%s\n" (string_of_error at msg)
       | ElabError (at, msg) -> Format.printf "%s\n" (string_of_error at msg))

let command =
  Core.Command.group
    ~summary:"p4spec: a language design framework for the p4_16 language"
    [
      ("elab", elab_command);
      ("stat", stat_command);
      ("struct", struct_command);
      ("run-sl", run_sl_command);
      ("cover-sl", cover_sl_command);
      ("testgen", run_testgen_command);
      ("testgen-dbg", run_testgen_debug_command);
      ("interesting", interesting_command);
    ]

let () = Command_unix.run ~version command
