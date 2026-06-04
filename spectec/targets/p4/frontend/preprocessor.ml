open Core

let preprocess includes filename =
  let cmd =
    String.concat ~sep:" "
      ([ "cc" ]
      @ List.map ~f:(Printf.sprintf "-I%s") includes
      @ [ "-undef"; "-nostdinc"; "-E"; "-x"; "c"; filename ])
  in
  let channels =
    Core_unix.open_process_full cmd ~env:(Core_unix.environment ())
  in
  let program =
    In_channel.input_all channels.Core_unix.Process_channels.stdout
  in
  let diagnostics =
    In_channel.input_all channels.Core_unix.Process_channels.stderr
  in
  match Core_unix.close_process_full channels with
  | Ok () -> program
  | Error _ ->
      Out_channel.(
        output_string stderr diagnostics;
        flush stderr);
      program
