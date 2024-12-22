open Util.Source

(* Error handling *)

type err = string * info
type 'a res = ('a, err) result

let ( let* ) = Result.bind

let check ?(info = no_info) (b : bool) (msg : string) : unit res =
  if b then Ok () else Error (msg, info)

let implies (p : bool) (q : bool) : bool = (not p) || q
let error_no_info (msg : string) = Error (msg, no_info)

let error_info (info : info) = function
  | Error (msg, M "") -> Error (msg, info)
  | _ as res -> res
