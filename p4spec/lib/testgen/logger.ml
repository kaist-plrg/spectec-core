(* Logging fuzz states *)

type t = out_channel

(* Constructor *)

let setup_signal_handler (logger : t) =
  let handler _ =
    print_endline ">>> Caught interrupt, flushing logs";
    close_out logger;
    exit 1
  in
  Sys.set_signal Sys.sigint (Sys.Signal_handle handler)

let init (logname : string) : t =
  let logger = open_out logname in
  setup_signal_handler logger;
  logger

(* Logging *)

let log (logger : t) (msg : string) : unit =
  let msg = ">>> " ^ msg in
  print_endline msg;
  msg ^ "\n" |> output_string logger;
  flush logger

(* Closing *)

let close (logger : t) = close_out logger
