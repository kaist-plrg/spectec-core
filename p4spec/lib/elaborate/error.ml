open Util.Error
open Util.Source

(* Error *)

let error (at : region) (msg : string) = error at "elab" msg
let warn (at : region) (msg : string) = warn at "elab" msg

(* Checks *)

let check (b : bool) (at : region) (msg : string) : unit =
  if not b then error at msg
