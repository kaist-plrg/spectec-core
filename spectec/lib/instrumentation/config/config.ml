module Exn = Instrumentation_common.Exn

type t = Handler_config.t list

let default = []

let register_static_dependencies (config : t) =
  List.iter Handler_config.register_static_dependencies config

let handlers (config : t) = List.map Handler_config.to_handler config

let has_handler (config : t) ~name =
  List.exists (fun (hc : Handler_config.t) -> hc.name = name) config

let mode_name = function `IL -> "IL" | `SL -> "SL" | `PL -> "PL"

let validate_mode (config : t) ~mode =
  let incompatible =
    List.filter_map
      (fun ({ Handler_config.name; modes; _ } : Handler_config.t) ->
        if List.mem mode modes then None
        else
          let supported = String.concat " and " (List.map mode_name modes) in
          Some (name, supported ^ " only"))
      config
  in
  match incompatible with
  | [] -> Ok ()
  | errs ->
      let details =
        String.concat ", "
          (List.map (fun (n, reason) -> Printf.sprintf "%s (%s)" n reason) errs)
      in
      Error
        (Printf.sprintf "Instrumentation handlers incompatible with %s mode: %s"
           (mode_name mode) details)

let close_outputs (config : t) =
  config
  |> List.fold_left
       (fun first_error entry ->
         Exn.try_record_first_error first_error (fun () ->
             Handler_config.close_output entry))
       None
  |> Exn.raise_recorded_error
