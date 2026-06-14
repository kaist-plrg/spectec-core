(** Shared CLI argument parsers, grouped by role. *)

open Instrumentation

(** [--color] flag selection. [Auto] enables color on a TTY (and respects the
    [NO_COLOR] environment variable); [Always] / [Never] force the choice. *)
type color = Auto | Always | Never

let color_arg : color Core.Command.Arg_type.t =
  Core.Command.Arg_type.create (function
    | "auto" -> Auto
    | "always" -> Always
    | "never" -> Never
    | s -> failwith ("expected auto|always|never, got: " ^ s))

(** Output presentation. *)
module Output = struct
  let color_flag : color Core.Command.Param.t =
    let open Core.Command.Param in
    flag "--color"
      (optional_with_default Auto color_arg)
      ~doc:"WHEN colorize diagnostics: auto|always|never (default: auto)"

  let no_warnings_flag : bool Core.Command.Param.t =
    let open Core.Command.Param in
    flag "-w" no_arg ~doc:" suppress warnings (show only errors)"
end

(** Spec source override — an explicit list of files or a directory (mutually
    exclusive); either substitutes for the target's default spec dir. *)
module Spec = struct
  let files_flag : string list Core.Command.Param.t =
    let open Core.Command.Param in
    flag "--spec" (listed string)
      ~doc:"FILES spec files; mutually exclusive with --spec-dir"

  let dir_flag : string option Core.Command.Param.t =
    let open Core.Command.Param in
    flag "--spec-dir" (optional string)
      ~doc:
        "DIR directory of .spectec files, collected recursively; mutually \
         exclusive with --spec"

  let source_flag : Spec_source.t option Core.Command.Param.t =
    let open Core.Command.Let_syntax in
    let%map files = files_flag and dir = dir_flag in
    match (files, dir) with
    | [], None -> None
    | files, None -> Some (Spec_source.Files files)
    | [], Some dir -> Some (Spec_source.Dir dir)
    | _ :: _, Some _ -> failwith "--spec and --spec-dir are mutually exclusive"
end

(** Running a task across many inputs. *)
module Batch = struct
  let mode_flag : bool Core.Command.Param.t =
    let open Core.Command.Param in
    flag "--batch" no_arg ~doc:" run on a directory of inputs"

  let dir_flag : string option Core.Command.Param.t =
    let open Core.Command.Param in
    flag "--batch-dir" (optional string)
      ~doc:"DIR directory of inputs (default: target's test dir)"
end

(** Checkpoint persistence — these flags always travel together. *)
module Checkpoint = struct
  type t = {
    output : string option;
    resume : string option;
    save_interval : int;
  }

  let flags : t Core.Command.Param.t =
    let open Core.Command.Let_syntax in
    let open Core.Command.Param in
    let%map output =
      flag "--checkpoint" (optional string)
        ~doc:"FILE save checkpoint to file (enables resume)"
    and resume =
      flag "--resume" (optional string) ~doc:"FILE resume from checkpoint file"
    and save_interval =
      flag "--save-interval"
        (optional_with_default 100 int)
        ~doc:"N save checkpoint every N tests (default: 100)"
    in
    { output; resume; save_interval }
end

(** Interpreter selection and instrumentation. *)
module Interpreter = struct
  let sl_mode_flag : bool Core.Command.Param.t =
    let open Core.Command.Param in
    flag "--sl" no_arg ~doc:" use SL interpreter (default: IL)"

  (* One Param.t for one typed handler spec — maps each declared param to a
     --name.param_name flag, then calls D.parse. *)
  let handler_param (module D : Handler.Spec.S) :
      Handler.Config.t option Core.Command.Param.t =
    let open Core.Command.Param in
    let flag_params =
      List.map
        (fun (param_name, doc) ->
          flag ("--" ^ D.name ^ "." ^ param_name) (optional string) ~doc
          |> map ~f:(fun v -> (param_name, v)))
        D.params
    in
    let combined =
      List.fold_right
        (fun flag_p rest ->
          both flag_p rest |> map ~f:(fun (entry, entries) -> entry :: entries))
        flag_params (return [])
    in
    map combined ~f:D.parse

  (** Instrumentation handler flags — one set per handler spec. *)
  let config_flags : Config.t Core.Command.Param.t =
    let open Core.Command.Param in
    List.fold_right
      (fun handler_spec rest ->
        both (handler_param handler_spec) rest
        |> map ~f:(fun (handler_opt, handler_opts) ->
               handler_opt :: handler_opts))
      builtin_handler_specs (return [])
    |> map ~f:(List.filter_map Fun.id)
end
