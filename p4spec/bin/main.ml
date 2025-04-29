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
         let _ =
           Interp_sl.Interp.run_typing ~derive spec_sl includes_p4 filename_p4
         in
         ()
       with Error (at, msg) -> Format.printf "%s\n" (string_of_error at msg))

let cover_sl_command =
  Core.Command.basic ~summary:"measure phantom coverage of SL"
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
         Runtime_testgen.Cov.Multiple.log cover_multi
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
     and dirnames_boot =
       flag "-cold" (listed string) ~doc:"directories for cold boot"
     in
     fun () ->
       try
         let spec = List.concat_map Frontend.Parse.parse_file filenames_spec in
         let spec_il = Elaborate.Elab.elab_spec spec in
         let spec_sl = Structure.Struct.struct_spec spec_il in
         let filenames_seed_p4 = collect_files ~suffix:".p4" dirname_seed_p4 in
         match (filename_boot, dirnames_boot) with
         | Some filename_boot, [] ->
             Testgen.Gen.fuzz_typing_warm fuel spec_sl includes_p4
               filenames_seed_p4 dirname_gen filename_boot
         | None, dirnames_boot ->
             let filenames_boot_p4 =
               List.concat_map (collect_files ~suffix:".p4") dirnames_boot
             in
             Testgen.Gen.fuzz_typing_cold fuel spec_sl includes_p4
               filenames_seed_p4 dirname_gen filenames_boot_p4
         | _ ->
             Format.printf
               "Please provide either a warm or cold boot coverage file\n"
       with Error (at, msg) -> Format.printf "%s\n" (string_of_error at msg))

let command =
  Core.Command.group
    ~summary:"p4spec: a language design framework for the p4_16 language"
    [
      ("elab", elab_command);
      ("struct", struct_command);
      ("run-sl", run_sl_command);
      ("cover-sl", cover_sl_command);
      ("testgen", run_testgen_command);
    ]

let () = Command_unix.run ~version command
