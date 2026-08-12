open Common.Source

exception InterpError of region * string

(* Error *)

let error (at : region) (msg : string) = raise (InterpError (at, msg))
let warn (at : region) (msg : string) = Diag.warn at "sl-interp" msg

(* Builtin errors *)

let unwrap_builtin (result : 'a Builtins.result) : 'a =
  match result with
  | Ok res -> res
  | Error err ->
      let at, msg =
        match err with
        | Builtins.Error.TypeError (at, expected, v) ->
            ( at,
              Printf.sprintf "Builtin type error: expected %s, got %s" expected
                (Lang.Il.Value.to_string v) )
        | Builtins.Error.RuntimeError (at, msg) ->
            (at, Printf.sprintf "Builtin arity error: %s" msg)
        | Builtins.Error.ArityError (at, msg) ->
            (at, Printf.sprintf "Builtin runtime error: %s" msg)
        | Builtins.Error.MissingImplError (at, msg) ->
            (at, Printf.sprintf "Builtin missing implementation: %s" msg)
      in
      error at msg

(* Check *)

(* On the success path [ifprintf] walks the format without building the
   message, so it is rendered only when a check fails. *)
let null_ppf = Format.make_formatter (fun _ _ _ -> ()) ignore

let checkf (b : bool) (at : region) fmt =
  if b then Format.ifprintf null_ppf fmt else Format.kasprintf (error at) fmt

type error = region * string
