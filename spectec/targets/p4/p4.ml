(** P4 Typechecker target - Implements TARGET and TASK for P4 typechecking *)

module Builtins = Builtins_p4
module Frontend = Frontend_p4

(* Files that are includes, not test inputs *)
let skip_dirs = [ "include"; "includes" ]

(* Simple substring check *)
let contains_substring s sub =
  try
    let _ = Str.search_forward (Str.regexp_string sub) s 0 in
    true
  with Not_found -> false

(* Recursively collect files with given suffix from directory *)
let collect_files_recursive ~suffix dir =
  let rec gather acc path =
    if Sys.file_exists path && Sys.is_directory path then (
      let entries = Sys.readdir path in
      Array.sort String.compare entries;
      Array.fold_left
        (fun acc name ->
          let full_path = Filename.concat path name in
          if List.mem name skip_dirs then acc else gather acc full_path)
        acc entries)
    else if Filename.check_suffix path suffix then path :: acc
    else acc
  in
  if Sys.file_exists dir then gather [] dir |> List.rev else []

(* Load exclude patterns from .exclude files recursively *)
let load_excludes dir =
  let exclude_files = collect_files_recursive ~suffix:".exclude" dir in
  List.concat_map
    (fun path ->
      let ic = open_in path in
      let rec read_lines acc =
        try
          let line = input_line ic |> String.trim in
          if String.length line = 0 || line.[0] = '#' then read_lines acc
          else read_lines (line :: acc)
        with End_of_file ->
          close_in ic;
          acc
      in
      read_lines [])
    exclude_files

(* Check if a path should be excluded - matches against test-relative path *)
let is_excluded excludes path =
  List.exists (fun pattern -> contains_substring path pattern) excludes

(* Module-level tid_counter shared with cache as state_generation *)
let tid_counter = ref 0

(* Derive old builtin names from new: rename a few, drop new-only ones *)
let old_builtins =
  Builtins.builtins
  |> List.filter_map (fun (name, def) ->
         match name with
         | "fresh_typeId" -> Some ("fresh_tid", def)
         | "sum_nat" -> Some ("sum", def)
         | "max_nat" -> Some ("max", def)
         | "min_nat" -> Some ("min", def)
         | "bitstr_to_int" -> Some ("to_int", def)
         | "int_to_bitstr" -> Some ("to_bitstr", def)
         | "text_to_int" | "strip_all_whitespace" -> None
         | _ -> Some (name, def))

(* Shared handler logic — sets up fresh VID and TID providers *)
let handler f =
  let vid_counter = ref 0 in
  tid_counter := 0;
  let fresh_vid () =
    let vid = !vid_counter in
    incr vid_counter;
    vid
  in
  Lang.Il.Value.GlobalVidProvider.set fresh_vid;
  let fresh_tid () =
    let tid = "FRESH__" ^ string_of_int !tid_counter in
    incr tid_counter;
    tid
  in
  Builtins.Fresh.GlobalTidProvider.set fresh_tid;
  f ()

(* Functions/relations known to transitively call fresh_tid but safe to cache.
   These are cached unconditionally; everything else uses the purity guard. *)
let is_impure_func = function
  | "subst_type" | "subst_typeDef" | "specialize_typeDef" | "canon"
  | "free_type" | "is_nominal_typeIR" | "bound" | "gen_constraint_type"
  | "merge_constraint" | "merge_constraint'" | "find_matchings"
  | "nestable_struct" | "nestable_struct_in_header" | "find_map" ->
      true
  | _ -> false

let is_impure_rel = function
  | "Cast_expl" | "Cast_expl_canon" | "Cast_expl_canon_neq" | "Cast_impl"
  | "Cast_impl_canon" | "Cast_impl_canon_neq" | "Type_wf" | "Type_alpha" ->
      true
  | _ -> false

(* P4 target specification — new spec *)
module Target : Spectec.Target.S = struct
  let name = "p4"
  let spec_dir = "spectec/specs/p4"
  let builtins = Builtins.builtins
  let handler = handler
  let is_impure_func = is_impure_func
  let is_impure_rel = is_impure_rel
  let state_version = tid_counter
end

(* P4 target specification — old spec *)
module Target_old : Spectec.Target.S = struct
  let name = "p4-old"
  let spec_dir = "spectec/specs/p4-old"
  let builtins = old_builtins
  let handler = handler
  let is_impure_func = is_impure_func
  let is_impure_rel = is_impure_rel
  let state_version = tid_counter
end

(* P4 Typechecker task - extends TASK with make function for CLI *)
module Typecheck = struct
  let name = "typechecker"

  module Target = Target

  type input = {
    includes : string list;
    filename : string;
    expect : Spectec.Task.expectation;
  }

  let collect ?dir () =
    match dir with
    | None -> []
    | Some base ->
        (* includes live under the corpus base; excludes sit beside it *)
        let includes_dir = Filename.concat base "includes" in
        let excludes_dir = Filename.concat (Filename.dirname base) "excludes" in
        let excludes = load_excludes excludes_dir in
        collect_files_recursive ~suffix:".p4" base
        |> List.filter (fun filename -> not (is_excluded excludes filename))
        |> List.map (fun filename ->
               let expect =
                 if contains_substring filename "_errors" then
                   Spectec.Task.Negative
                 else Spectec.Task.Positive
               in
               { includes = [ includes_dir ]; filename; expect })

  let unparse = Frontend.unparse
  let parse_string = Frontend.parse_string

  let parse_input ~spec:_ { includes; filename; _ } =
    Frontend.parse_file ~handler:Target.handler includes filename
    |> Result.map (fun v -> ("Program_ok", [ v ]))

  let source { filename; _ } = filename
  let expectation { expect; _ } = expect
  let format_output _values = "Typechecker succeeded"
  let save_output _filename _values = ()
end

(* P4 Typechecker task — old spec *)
module Typecheck_old = struct
  let name = "typechecker"

  module Target = Target_old

  type input = {
    includes : string list;
    filename : string;
    expect : Spectec.Task.expectation;
  }

  let collect ?dir () =
    match dir with
    | None -> []
    | Some base ->
        (* includes live under the corpus base; excludes sit beside it *)
        let includes_dir = Filename.concat base "includes" in
        let excludes_dir = Filename.concat (Filename.dirname base) "excludes" in
        let excludes = load_excludes excludes_dir in
        collect_files_recursive ~suffix:".p4" base
        |> List.filter (fun filename -> not (is_excluded excludes filename))
        |> List.map (fun filename ->
               let expect =
                 if contains_substring filename "_errors" then
                   Spectec.Task.Negative
                 else Spectec.Task.Positive
               in
               { includes = [ includes_dir ]; filename; expect })

  let unparse = Frontend.unparse
  let parse_string = Frontend.parse_string

  let parse_input ~spec:_ { includes; filename; _ } =
    Frontend.parse_file ~handler:Target.handler includes filename
    |> Result.map (fun v -> ("Program_ok", [ v ]))

  let source { filename; _ } = filename
  let expectation { expect; _ } = expect
  let format_output _values = "Typechecker succeeded"
  let save_output _filename _values = ()
end

module Typecheck_cli : Cli.Task_cli.S = struct
  module Task = Typecheck

  let flags =
    let open Core.Command.Let_syntax in
    let open Core.Command.Param in
    let%map includes = flag "-i" (listed string) ~doc:"DIR P4 include paths"
    and filename = flag "-p" (required string) ~doc:"FILE P4 file to process" in
    { Typecheck.includes; filename; expect = Spectec.Task.Positive }
end

let target = (module Target : Spectec.Target.S)

module Cli : Cli.Target_cli.S = struct
  module Target = Target

  let name = "p4"

  let command =
    let module Subcommand = Cli.Subcommand in
    Core.Command.group ~summary:"P4 commands"
      [
        Subcommand.make_task target ~name:"typecheck"
          ~summary:"Run P4 typechecker"
          (module Typecheck_cli);
        Subcommand.make_parse target ~name:"parse"
          ~summary:"parse a P4 program to an IL value"
          (module Typecheck_cli);
        Subcommand.make_batch target ~name:"batch" [ (module Typecheck_cli) ];
        Subcommand.make_checkpoint target ~name:"checkpoint";
      ]
end
