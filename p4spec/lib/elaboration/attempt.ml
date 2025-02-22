open Util.Error
open Util.Source

(* Error *)

let error (at : region) (msg : string) = error at "elab" msg
let warn (at : region) (msg : string) = warn at "elab" msg

(* Backtracking *)

type 'a attempt = Ok of 'a | Fail of region * string

let fail (at : region) (msg : string) : 'a attempt = Fail (at, msg)

let ( let* ) (attempt : 'a attempt) (f : 'a -> 'b) : 'b =
  match attempt with Ok a -> f a | Fail _ as fail -> fail

let ( let+ ) (attempt : 'a attempt) (f : 'a -> 'b) : 'b =
  match attempt with Ok a -> f a | Fail (at, msg) -> error at msg

let rec choice (at : region) = function
  | [] -> fail at "all trials have failed"
  | f :: fs -> ( match f () with Ok a -> Ok a | Fail _ -> choice at fs)
