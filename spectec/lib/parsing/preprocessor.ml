open Core

let preprocess includes filename =
  let cmd =
    String.concat ~sep:" "
      ([ "cc" ]
      @ List.map ~f:(Printf.sprintf "-I%s") includes
      @ [ "-undef"; "-nostdinc"; "-E"; "-x"; "c"; filename ])
  in
  let in_chan = Core_unix.open_process_in cmd in
  let program = In_channel.input_all in_chan in
  let _ = Core_unix.close_process_in in_chan in
  program
