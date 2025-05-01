(* Logging fuzz states *)

type t = bool * out_channel

(* Constructor *)

let setup_signal_handler (logger : out_channel) =
  let handler _ =
    print_endline ">>> Caught interrupt, flushing logs";
    close_out logger;
    exit 1
  in
  Sys.set_signal Sys.sigint (Sys.Signal_handle handler)

let init ?(silent : bool = false) (logname : string) : t =
  let logger = open_out logname in
  setup_signal_handler logger;
  (silent, logger)

(* Logging *)

let log ((silent, logger) : t) (msg : string) : unit =
  let timestamp =
    let tm = Unix.localtime (Unix.time ()) in
    Printf.sprintf "[%02d:%02d:%02d]" tm.tm_hour tm.tm_min tm.tm_sec
  in
  let msg = Format.asprintf "[%s] >>> %s" timestamp msg in
  if not silent then print_endline msg;
  msg ^ "\n" |> output_string logger;
  flush logger

let warn ((silent, logger) : t) (msg : string) : unit =
  let timestamp =
    let tm = Unix.localtime (Unix.time ()) in
    Printf.sprintf "[%02d:%02d:%02d]" tm.tm_hour tm.tm_min tm.tm_sec
  in
  let msg = Format.asprintf "[%s] !!! %s" timestamp msg in
  if not silent then print_endline msg;
  msg ^ "\n" |> output_string logger;
  flush logger

(* Closing *)

let close ((_, logger) : t) = close_out logger
