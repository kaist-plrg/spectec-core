(* Logging fuzz states *)

type t = out_channel

(* Constructor *)

let setup_signal_handler (oc : out_channel) =
  let handler _ =
    print_endline ">>> Caught interrupt, flushing logs";
    close_out oc;
    exit 1
  in
  Sys.set_signal Sys.sigint (Sys.Signal_handle handler)

let init (logname : string) : t =
  let oc = open_out logname in
  setup_signal_handler oc;
  oc

(* Logging *)

let log (logmode : Modes.logmode) (oc : t) (msg : string) : unit =
  let timestamp =
    let tm = Unix.localtime (Unix.time ()) in
    Printf.sprintf "[%02d:%02d:%02d]" tm.tm_hour tm.tm_min tm.tm_sec
  in
  let msg = Format.asprintf "[%s] >>> %s" timestamp msg in
  if logmode = Verbose then print_endline msg;
  msg ^ "\n" |> output_string oc;
  flush oc

let mark (logmode : Modes.logmode) (oc : t) (msg : string) : unit =
  let timestamp =
    let tm = Unix.localtime (Unix.time ()) in
    Printf.sprintf "[%02d:%02d:%02d]" tm.tm_hour tm.tm_min tm.tm_sec
  in
  let msg = Format.asprintf "[%s] ### %s" timestamp msg in
  if logmode = Verbose then print_endline msg;
  msg ^ "\n" |> output_string oc;
  flush oc

let warn (logmode : Modes.logmode) (oc : t) (msg : string) : unit =
  let timestamp =
    let tm = Unix.localtime (Unix.time ()) in
    Printf.sprintf "[%02d:%02d:%02d]" tm.tm_hour tm.tm_min tm.tm_sec
  in
  let msg = Format.asprintf "[%s] !!! %s" timestamp msg in
  if logmode = Verbose then print_endline msg;
  msg ^ "\n" |> output_string oc;
  flush oc

(* Closing *)

let close (oc : t) = close_out oc
