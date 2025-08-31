(* Logging fuzz queries *)

type t = out_channel

(* Constructor *)

let setup_signal_handler (queries : t) =
  let handler _ =
    print_endline ">>> Caught interrupt, flushing queries";
    close_out queries;
    exit 1
  in
  Sys.set_signal Sys.sigint (Sys.Signal_handle handler)

let init (queryname : string) : t =
  let queries = open_out queryname in
  setup_signal_handler queries;
  queries

(* Logging *)

let query (queries : t) (msg : string) : unit =
  let timestamp =
    let tm = Unix.localtime (Unix.time ()) in
    Printf.sprintf "[%02d:%02d:%02d]" tm.tm_hour tm.tm_min tm.tm_sec
  in
  Format.asprintf "[%s] Query\n%s\n" timestamp msg |> output_string queries;
  flush queries

let answer (queries : t) (msg : string) : unit =
  let timestamp =
    let tm = Unix.localtime (Unix.time ()) in
    Printf.sprintf "[%02d:%02d:%02d]" tm.tm_hour tm.tm_min tm.tm_sec
  in
  Format.asprintf "[%s] Answer\n%s\n" timestamp msg |> output_string queries;
  flush queries

(* Closing *)

let close (queries : t) = close_out queries
