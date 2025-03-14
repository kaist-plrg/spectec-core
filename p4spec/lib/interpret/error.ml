open Util.Error
open Util.Source

(* Error *)

let error (at : region) (msg : string) = error at "interp" msg
let warn (at : region) (msg : string) = warn at "interp" msg
