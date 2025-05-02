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
       with Error (at, msg) -> Format.printf "%s\n" (string_of_error at msg))

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
       with Error (at, msg) -> Format.printf "%s\n" (string_of_error at msg))

let run_sl_command =
  Core.Command.basic
    ~summary:"run static semantics of a p4_16 spec based on non-backtracking SL"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map filenames_spec = anon (sequence ("filename" %: string))
     and includes_p4 = flag "-i" (listed string) ~doc:"p4 include paths"
     and filename_p4 = flag "-p" (required string) ~doc:"p4 file to typecheck"
     and derive = flag "-derive" no_arg ~doc:"derive value dependency graph" in
     fun () ->
       try
         let spec = List.concat_map Frontend.Parse.parse_file filenames_spec in
         let spec_il = Elaborate.Elab.elab_spec spec in
         let spec_sl = Structure.Struct.struct_spec spec_il in
         match
           Interp_sl.Interp.run_typing ~derive spec_sl includes_p4 filename_p4
         with
         | Well _ -> Format.printf "well-typed\n"
         | Ill (_, msg, _) -> Format.printf "ill-typed: %s\n" msg
       with Error (at, msg) -> Format.printf "%s\n" (string_of_error at msg))

let cover_sl_command =
  Core.Command.basic ~summary:"measure phantom coverage of SL"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map filenames_spec = anon (sequence ("filename" %: string))
     and includes_p4 = flag "-i" (listed string) ~doc:"p4 include paths"
     and dirnames_p4 =
       flag "-d" (listed string) ~doc:"p4 directories to typecheck"
     and filename_cov =
       flag "-cov" (required string) ~doc:"output coverage file"
     in
     fun () ->
       try
         let spec = List.concat_map Frontend.Parse.parse_file filenames_spec in
         let spec_il = Elaborate.Elab.elab_spec spec in
         let spec_sl = Structure.Struct.struct_spec spec_il in
         let filenames_p4 =
           List.concat_map (collect_files ~suffix:".p4") dirnames_p4
         in
         let cover =
           Interp_sl.Interp.cover_typings spec_sl includes_p4 filenames_p4
         in
         Runtime_testgen.Cov.Multiple.log ~filename_cov_opt:(Some filename_cov)
           cover
       with Error (at, msg) -> Format.printf "%s\n" (string_of_error at msg))

(* TODO: Merge with cover_sl_command *)
let cover_sl_filenames_command =
  Core.Command.basic
    ~summary:"measure phantom coverage and print pid with filenames"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map filenames_spec = anon (sequence ("filename" %: string))
     and includes_p4 = flag "-i" (listed string) ~doc:"p4 include paths"
     and dirnames_p4 =
       flag "-d" (listed string) ~doc:"p4 directories to typecheck"
     in
     fun () ->
       try
         let spec = List.concat_map Frontend.Parse.parse_file filenames_spec in
         let spec_il = Elaborate.Elab.elab_spec spec in
         let spec_sl = Structure.Struct.struct_spec spec_il in
         let filenames_p4 =
           List.concat_map (collect_files ~suffix:".p4") dirnames_p4
         in
         let cover_multi =
           Interp_sl.Interp.cover_typings spec_sl includes_p4 filenames_p4
         in
         Interp_sl.Interp.MCov.Cover.iter
           (fun pid (branch : Interp_sl.Interp.MCov.Branch.t) ->
             match branch.status with
             | Hit -> Format.printf "%d Hit\n" pid
             | Miss [] -> Format.printf "%d Miss\n" pid
             | Miss filenames ->
                 Format.printf "%d %s\n" pid
                   (filenames
                   |> Testgen.Rand.random_sample 3
                   |> List.fold_left (fun s file -> s ^ " " ^ file) "CM"))
           cover_multi
       with Error (at, msg) -> Format.printf "%s\n" (string_of_error at msg))

let run_testgen_command =
  Core.Command.basic
    ~summary:"generate negative type checker tests from a p4_16 spec"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map filenames_spec = anon (sequence ("filename" %: string))
     and fuel = flag "-fuel" (required int) ~doc:"fuel for test generation"
     and includes_p4 = flag "-i" (listed string) ~doc:"p4 include paths"
     and dirname_seed_p4 =
       flag "-seed" (required string) ~doc:"seed p4 directory"
     and dirname_gen =
       flag "-gen" (required string) ~doc:"directory for generated p4 programs"
     and filename_boot =
       flag "-warm" (optional string) ~doc:"coverage file for warm boot"
     and filename_target =
       flag "-target" (optional string) ~doc:"file for target mode"
     and silent = flag "-silent" no_arg ~doc:"do not print logs to stdout"
     and random = flag "-random" no_arg ~doc:"randomize AST selection" in
     fun () ->
       try
         let spec = List.concat_map Frontend.Parse.parse_file filenames_spec in
         let spec_il = Elaborate.Elab.elab_spec spec in
         let spec_sl = Structure.Struct.struct_spec spec_il in
         let filenames_seed_p4 = collect_files ~suffix:".p4" dirname_seed_p4 in
         let bootmode =
           match filename_boot with
           | Some filename_boot -> Testgen.Gen.Warm filename_boot
           | None -> Testgen.Gen.Cold
         in
         let targetmode =
           match filename_target with
           | Some filename_target -> Testgen.Gen.Target filename_target
           | None -> Testgen.Gen.Roundrobin
         in
         Testgen.Gen.fuzz_typing ~silent ~random fuel spec_sl includes_p4
           filenames_seed_p4 dirname_gen bootmode targetmode
       with Error (at, msg) -> Format.printf "%s\n" (string_of_error at msg))

let run_testgen_debug_command =
  Core.Command.basic
    ~summary:"debug close-AST deriver in negative type checker generator"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map filenames_spec = anon (sequence ("filename" %: string))
     and includes_p4 = flag "-i" (listed string) ~doc:"p4 include paths"
     and filename_p4 = flag "-p" (required string) ~doc:"p4 file to typecheck"
     and dirname_debug =
       flag "-debug" (required string) ~doc:"directory for debug files"
     and pid = flag "-pid" (required int) ~doc:"phantom id to close-miss" in
     fun () ->
       try
         let spec = List.concat_map Frontend.Parse.parse_file filenames_spec in
         let spec_il = Elaborate.Elab.elab_spec spec in
         let spec_sl = Structure.Struct.struct_spec spec_il in
         Testgen.Derive.debug_phantom spec_sl includes_p4 filename_p4 dirname_debug pid
       with Error (at, msg) -> Format.printf "%s\n" (string_of_error at msg))

let interesting_command =
  Core.Command.basic ~summary:"interestingness test for reducing p4_16 programs"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map filenames_spec = anon (sequence ("filename" %: string))
     and includes_p4 = flag "-i" (listed string) ~doc:"p4 include paths"
     and pid = flag "-pid" (required int) ~doc:"phantom id to close-miss"
     and filename_p4 =
       flag "-p" (required string) ~doc:"p4 file to typecheck"
     in
     fun () ->
       try
         let spec = List.concat_map Frontend.Parse.parse_file filenames_spec in
         let spec_il = Elaborate.Elab.elab_spec spec in
         let spec_sl = Structure.Struct.struct_spec spec_il in
         match Interp_sl.Interp.run_typing spec_sl includes_p4 filename_p4 with
         | Ill _ ->
             Format.printf "ill-typed\n";
             exit 1
         | Well (_, _, cover_single) -> (
             Interp_sl.Interp.SCov.Cover.iter
               (fun pid (branch : Interp_sl.Interp.SCov.Branch.t) ->
                 match branch.status with
                 | Hit -> Format.printf "%d: Hit\n" pid
                 | Miss (_ :: _) -> Format.printf "%d: Close-Miss\n" pid
                 | _ -> ())
               cover_single;
             let branch = Interp_sl.Interp.SCov.Cover.find pid cover_single in
             match branch.status with
             | Hit ->
                 "hit" |> print_endline;
                 exit 0
             | Miss [] ->
                 "miss" |> print_endline;
                 exit 2
             | Miss _ -> exit 0)
       with Error (at, msg) -> Format.printf "%s\n" (string_of_error at msg))

let command =
  Core.Command.group
    ~summary:"p4spec: a language design framework for the p4_16 language"
    [
      ("elab", elab_command);
      ("struct", struct_command);
      ("run-sl", run_sl_command);
      ("cover-sl", cover_sl_command);
      ("cover-sl-filenames", cover_sl_filenames_command);
      ("testgen", run_testgen_command);
      ("testgen-dbg", run_testgen_debug_command);
      ("interesting", interesting_command);
    ]

let () = Command_unix.run ~version command
