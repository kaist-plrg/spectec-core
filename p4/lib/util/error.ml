open Source

(* Error handling *)

type err = string * info
type 'a res = ('a, err) result

let ( let* ) = Result.bind
let ( ||> ) = Result.bind

(* Constructors *)

let ok = Result.ok
let error_no_info (msg : string) = Error (msg, no_info)

let error_pass_info (info : info) = function
  | Error (msg, M "") -> Error (msg, info)
  | _ as res -> res

(* Conditionals *)

let check ?(info = no_info) (b : bool) (msg : string) : unit res =
  if b then Ok () else Error (msg, info)

let implies (p : bool) (q : bool) : bool = (not p) || q

(* Iterators *)

let fold_left_res (f : 'a -> 'b -> 'a res) (acc : 'a) (l : 'b list) : 'a res =
  List.fold_left
    (fun acc x ->
      let* acc = acc in
      f acc x)
    (Ok acc) l

let map_res (f : 'a -> 'b res) (l : 'a list) : 'b list res =
  fold_left_res
    (fun acc x ->
      let* x = f x in
      Ok (x :: acc))
    [] l
  |> Result.map List.rev

let iter_res (f : 'a -> unit res) (l : 'a list) : unit res =
  fold_left_res (fun () x -> f x) () l
