open Common.Source
open Lang.Il

type t =
  | TypeError of region * string * value
  | ArityError of region * string
  | RuntimeError of region * string
  | MissingImplError of region * string

type 'a result = ('a, t) Stdlib.result

let runtime (r : region) (msg : string) : t = RuntimeError (r, msg)
let arity (r : region) (msg : string) : t = ArityError (r, msg)
let type_err (r : region) (msg : string) (v : value) : t = TypeError (r, msg, v)
let missing_impl (r : region) (msg : string) : t = MissingImplError (r, msg)

let region_and_message = function
  | TypeError (at, expected, got) ->
      ( at,
        Printf.sprintf "Type error: expected %s, got %s" expected
          (Value.to_string got) )
  | ArityError (at, msg) -> (at, Printf.sprintf "Arity error: %s" msg)
  | RuntimeError (at, msg) -> (at, Printf.sprintf "Runtime error: %s" msg)
  | MissingImplError (at, msg) ->
      (at, Printf.sprintf "Missing builtin implementation: %s" msg)

let to_diagnostic (err : t) : Diag.t =
  let at, message = region_and_message err in
  Diag.error ~source:"builtins" at message
