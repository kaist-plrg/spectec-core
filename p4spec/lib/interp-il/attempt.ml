include Util.Attempt
open Error
open Util.Source

(* Check *)

let check_fail (b : bool) (at : region) (msg : string) =
  if b then Ok () else fail at msg

(* Monadic interface *)

let ( let* ) (attempt : 'a attempt) (f : 'a -> 'b) : 'b =
  match attempt with Ok a -> f a | Fail _ as fail -> fail

let error_with_failtraces (failtraces : failtrace list) =
  let sfailtrace =
    match failtraces with
    | [] -> ""
    | [ failtrace ] ->
        let depth = depth failtrace in
        let depth = max 0 depth in
        string_of_failtrace ~depth ~bullet:"-" failtrace
    | failtraces ->
        List.mapi
          (fun idx failtrace ->
            let depth = depth failtrace in
            let depth = max 0 depth in
            string_of_failtrace ~depth
              ~bullet:(string_of_int (idx + 1) ^ ".")
              failtrace)
          failtraces
        |> String.concat ""
  in
  error no_region ("tracing backtrack logs:\n" ^ sfailtrace)

let ( let+ ) (attempt : 'a attempt) (f : 'a -> 'b) : 'b =
  match attempt with Ok a -> f a | Fail traces -> error_with_failtraces traces
