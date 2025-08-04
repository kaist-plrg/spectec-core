include Util.Attempt
open Util.Source
open Util.Print

(* Monadic interface *)

let ( let* ) (attempt : 'a attempt) (f : 'a -> 'b) : 'b =
  match attempt with Ok a -> f a | Fail _ as fail -> fail

let rec string_of_failtrace_noregion ?(level = 0) ~(depth : int)
    ~(bullet : string) (failtrace : failtrace) : string =
  let (Failtrace (region, msg, subfailtraces)) = failtrace in
  let len = region |> string_of_region |> String.length in
  let smsg =
    if level < depth then ""
    else Format.asprintf "\n%s%s%s %s" (String.make (len+7) ' ') (indent (level - depth)) bullet msg
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
  let compare_trace failtrace_l failtrace_r =
    let Failtrace (region_l, _, _) = failtrace_l in
    let Failtrace (region_r, _, _) = failtrace_r in
    if region_l.left.file = region_r.left.file then
    (if region_l.left.line = region_r.left.line then
    compare region_l.left.column region_r.left.column
    else compare region_l.left.line region_r.left.line)
    else compare region_l.left.file region_r.left.file
  in
  let failtraces_sorted = List.sort compare_trace failtraces in
  let error_of_failtrace (failtrace : failtrace) : region * string =
    let (Failtrace (at, msg, subfailtraces)) = failtrace in
    let string_subfailtraces =
      string_of_failtraces_noregion ~depth:0 subfailtraces
    in
    (at, msg ^ string_subfailtraces)
  in
  let all_errors = List.map error_of_failtrace failtraces_sorted in
  let formatted_errors =
    List.map (fun (at, msg) -> Util.Error.string_of_error at msg) all_errors
  in
  String.concat "\n" formatted_errors

(* Create a special exception type that can carry failtraces *)
exception Failtraces of failtrace list

let error_with_failtraces (failtraces : failtrace list) =
  raise (Failtraces failtraces)

let ( let+ ) (attempt : 'a attempt) (f : 'a -> 'b) : 'b =
  match attempt with Ok a -> f a | Fail traces -> error_with_failtraces traces
