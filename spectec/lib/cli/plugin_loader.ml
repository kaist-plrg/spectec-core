module Registry = Target_registry
module Plugin_site = Target_sites.Plugins.Target_plugins

type load_error = { plugin : string; reason : string }

let error plugin reason = Error { plugin; reason }

let load plugin =
  try
    Ok
      (Registry.with_plugin_registration ~expected_name:plugin (fun () ->
           Plugin_site.load plugin))
  with
  | Dynlink.Error dynlink_error ->
      error plugin (Dynlink.error_message dynlink_error)
  | exn -> error plugin (Printexc.to_string exn)

let unavailable_command { plugin; reason } =
  Core.Command.basic
    ~summary:(Printf.sprintf "unavailable target plugin %s" plugin)
    (Core.Command.Param.return (fun () ->
         Format.eprintf "error: target plugin %S could not be loaded\n  %s\n%!"
           plugin reason;
         Stdlib.exit 1))

let command plugin =
  match load plugin with
  | Ok Registry.{ name; command } -> (name, command)
  | Error error -> (plugin, unavailable_command error)

let commands () =
  Plugin_site.list () |> List.sort_uniq String.compare |> List.map command
