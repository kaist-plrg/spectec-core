module Exn = Instrumentation_common.Exn

type t = Handler_config.t list

let default = []

let register_static_dependencies (config : t) =
  List.iter Handler_config.register_static_dependencies config

let handlers (config : t) = List.map Handler_config.to_handler config

let has_handler (config : t) ~name =
  List.exists (fun (hc : Handler_config.t) -> hc.name = name) config

let validate_mode (config : t) ~sl_mode =
  let interp_mode = if sl_mode then `SL else `IL in
  let incompatible =
    List.filter_map
      (fun ({ Handler_config.name; mode; _ } : Handler_config.t) ->
        match (interp_mode, mode) with
        | `IL, `SL -> Some (name, "SL only")
        | `SL, `IL -> Some (name, "IL only")
        | _ -> None)
      config
  in
  match incompatible with
  | [] -> Ok ()
  | errs ->
      let mode_str = if sl_mode then "SL" else "IL" in
      let details =
        String.concat ", "
          (List.map (fun (n, reason) -> Printf.sprintf "%s (%s)" n reason) errs)
      in
      Error
        (Printf.sprintf "Instrumentation handlers incompatible with %s mode: %s"
           mode_str details)

let close_outputs (config : t) =
  config
  |> List.fold_left
       (fun first_error entry ->
         Exn.try_record_first_error first_error (fun () ->
             Handler_config.close_output entry))
       None
  |> Exn.raise_recorded_error
