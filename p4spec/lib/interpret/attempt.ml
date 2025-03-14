open Error
open Util.Source

(* Backtracking *)

type trace = Trace of region * string * trace list
type 'a attempt = Ok of 'a | Fail of trace list

let fail (at : region) (msg : string) : 'a attempt =
  Fail [ Trace (at, msg, []) ]

let rec string_of_trace ?(level = 0) ~(bullet : string) (trace : trace) : string
    =
  let (Trace (region, msg, subtraces)) = trace in
  let sbullet = String.make level ' ' ^ bullet in
  let sreason = " because " ^ msg ^ " (" ^ string_of_region region ^ ")\n" in
  let ssubtraces = string_of_traces ~level:(level + 1) subtraces in
  sbullet ^ sreason ^ ssubtraces

and string_of_traces ?(level = 0) (traces : trace list) : string =
  match traces with
  | [] -> ""
  | [ trace ] -> string_of_trace ~level ~bullet:"-" trace
  | traces ->
      List.mapi
        (fun idx trace ->
          string_of_trace ~level ~bullet:(string_of_int (idx + 1) ^ ".") trace)
        traces
      |> String.concat ""

let error_with_traces (traces : trace list) =
  let strace = string_of_traces traces in
  error no_region ("interpretation failed with trace:\n" ^ strace)

let ( let* ) (attempt : 'a attempt) (f : 'a -> 'b) : 'b =
  match attempt with Ok a -> f a | Fail _ as fail -> fail

let ( let+ ) (attempt : 'a attempt) (f : 'a -> 'b) : 'b =
  match attempt with Ok a -> f a | Fail traces -> error_with_traces traces

let rec choice = function
  | [] -> Fail []
  | f :: fs -> (
      match f () with
      | Ok a -> Ok a
      | Fail traces_h -> (
          match choice fs with
          | Ok a -> Ok a
          | Fail traces_t -> Fail (traces_h @ traces_t)))

let nest at msg attempt =
  match attempt with
  | Ok a -> Ok a
  | Fail traces -> Fail [ Trace (at, msg, traces) ]
