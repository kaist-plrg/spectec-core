include Util.Attempt

(* Monadic interface *)

let ( let* ) (attempt : 'a attempt) (f : 'a -> 'b) : 'b =
  match attempt with Ok a -> f a | Fail _ as fail -> fail

let ( let+ ) (attempt : 'a attempt) (f : 'a -> 'b) : 'b =
  match attempt with
  | Ok a -> f a
  | Fail traces -> Util.Error.error_elab_with_traces traces
