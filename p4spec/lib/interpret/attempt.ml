open Error
open Util.Source

(* Backtracking *)

type 'a attempt = Ok of 'a | Fail of region * string

let fail (at : region) (msg : string) : 'a attempt = Fail (at, msg)

let ( let* ) (attempt : 'a attempt) (f : 'a -> 'b) : 'b =
  match attempt with Ok a -> f a | Fail _ as fail -> fail

let ( let+ ) (attempt : 'a attempt) (f : 'a -> 'b) : 'b =
  match attempt with Ok a -> f a | Fail (at, msg) -> error at msg

let rec choice (at : region) (reason : string) = function
  | [] -> fail at ("all trials have failed for " ^ reason)
  | f :: fs -> ( match f () with Ok a -> Ok a | Fail _ -> choice at reason fs)
