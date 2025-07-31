include Util.Attempt
open Error
open Util.Source

(* Monadic interface *)

let ( let* ) (attempt : 'a attempt) (f : 'a -> 'b) : 'b =
  match attempt with Ok a -> f a | Fail _ as fail -> fail

let string_of_subfailtrace (subfailtrace : failtrace) : string =
  let (Failtrace (_, msg, _subfailtraces)) = subfailtrace in
  Format.asprintf " || because %s" msg

let format_failtraces (failtraces : failtrace list) : string =
  let error_of_failtrace (failtrace : failtrace) :
      (region * string) =
    let (Failtrace (at, msg, subfailtraces)) = failtrace in
    let string_subfailtraces = 
        List.map string_of_subfailtrace subfailtraces
        |> String.concat ""
    in
    (at, msg ^ string_subfailtraces)
  in
  let all_errors = List.map error_of_failtrace failtraces in
  let formatted_errors =
    List.map (fun (at, msg) -> Util.Error.string_of_error at msg) all_errors
  in
  String.concat "\n" formatted_errors

let error_with_failtraces (failtraces : failtrace list) =
  (* Format failtraces in standard error format for neovim quickfix *)
  let formatted_errors = format_failtraces failtraces in
  if formatted_errors <> "" then Printf.eprintf "%s\n" formatted_errors;
  error no_region ""

let ( let+ ) (attempt : 'a attempt) (f : 'a -> 'b) : 'b =
  match attempt with Ok a -> f a | Fail traces -> error_with_failtraces traces
