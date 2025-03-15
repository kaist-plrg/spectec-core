open Source

exception Error of region * string

let debug_errors = false

let string_of_error at msg =
  if at = no_region then msg else string_of_region at ^ ": " ^ msg

let error (at : region) (category : string) (msg : string) =
  if debug_errors then (
    let bar = String.make 80 '-' in
    Printf.eprintf "%s\n" bar;
    Printexc.(get_callstack 100 |> print_raw_backtrace stderr);
    Printf.eprintf "%s\n%!" bar);
  raise (Error (at, category ^ " error: " ^ msg))

let warn (at : region) (category : string) (msg : string) =
  Printf.eprintf "%s\n%!" (string_of_error at (category ^ " warning: " ^ msg))
