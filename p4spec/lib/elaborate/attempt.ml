include Util.Attempt
open Error
open Util.Source
open Util.Print

(* Monadic interface *)

let ( let* ) (attempt : 'a attempt) (f : 'a -> 'b) : 'b =
  match attempt with Ok a -> f a | Fail _ as fail -> fail

let rec string_of_failtrace_noregion ?(level = 0) ~(depth : int)
    ~(bullet : string) (failtrace : failtrace) : string =
  let (Failtrace (_, msg, subfailtraces)) = failtrace in
  let smsg =
    if level < depth then ""
    else Format.asprintf "  %s%s %s\n" (indent (level - depth)) bullet msg
  in
  Format.asprintf "%s%s" smsg
    (string_of_failtraces_noregion ~level:(level + 1) ~depth subfailtraces)

and string_of_failtraces_noregion ?(level = 0) ~(depth : int)
    (failtraces : failtrace list) : string =
  match failtraces with
  | [] -> ""
  | [ failtrace ] ->
      string_of_failtrace_noregion ~level ~depth ~bullet:"└─" failtrace
  | failtraces ->
      List.mapi
        (fun idx failtrace ->
          string_of_failtrace_noregion ~level ~depth
            ~bullet:(string_of_int (idx + 1) ^ ".")
            failtrace)
        failtraces
      |> String.concat ""

let format_failtraces (failtraces : failtrace list) : string =
  let error_of_failtrace (failtrace : failtrace) : region * string =
    let (Failtrace (at, msg, subfailtraces)) = failtrace in
    let string_subfailtraces =
      string_of_failtraces_noregion ~depth:0 subfailtraces
    in
    (at, msg ^ "\n" ^ string_subfailtraces)
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
