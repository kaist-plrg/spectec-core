open Source
open Attempt

exception ParseError of region * string
exception ElabError of region * failtrace list
exception ConvertInError of string
exception ConvertOutError of string
exception InterpError of region * string

let debug_errors = false

let string_of_error at msg =
  if at = no_region then msg else string_of_region at ^ ":\nError: " ^ msg

type elaboration_error = region * failtrace list

let string_of_elab_error at failtraces : string =
  string_of_error at ("\n" ^ string_of_failtraces ~region_parent:at ~depth:0 failtraces)

let string_of_elab_errors (errors : elaboration_error list) : string =
  let errors_sorted =
    List.sort (fun (at_l, _) (at_r, _) -> compare_region at_l at_r) errors
  in
  let formatted_errors =
    List.map (fun (at, failtraces) -> string_of_elab_error at failtraces) errors_sorted
  in
  String.concat "" formatted_errors

let warn (at : region) (category : string) (msg : string) =
  Printf.eprintf "%s\n%!"
    ((if at = no_region then "" else string_of_region at ^ ":")
    ^ "Warning:" ^ category ^ ":" ^ msg)

(* Parser errors *)

let error_parse (at : region) (msg : string) = raise (ParseError (at, msg))
let error_parse_no_region (msg : string) = raise (ParseError (no_region, msg))

(* Elaboration errors *)

let error_elab_with_traces (at : region) (failtraces : failtrace list) =
  raise (ElabError (at, failtraces))

let error_elab (at : region) (msg : string) =
  raise (ElabError (at, [ Failtrace (at, msg, []) ]))

let warn_elab (at : region) (msg : string) = warn at "elab" msg

(* Conversion errors *)

let error_convert_in (msg : string) = raise (ConvertInError msg)
let error_convert_out (msg : string) = raise (ConvertOutError msg)

(* Interpreter errors *)

let error_interp (at : region) (msg : string) = raise (InterpError (at, msg))
let warn_interp (at : region) (msg : string) = warn at "interp" msg
