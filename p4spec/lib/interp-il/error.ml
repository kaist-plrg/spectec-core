open Util.Error
open Util.Source

(* Error *)

let error (at : region) (msg : string) = error_interp at msg
let warn (at : region) (msg : string) = warn_interp at msg

(* Check *)

let check (b : bool) (at : region) (msg : string) : unit =
  if not b then error at msg

let guard (b : bool) (at : region) (msg : string) : unit =
  if not b then warn at msg
