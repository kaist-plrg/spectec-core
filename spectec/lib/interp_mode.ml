(** Which interpreter runs a task. Only PL consults hints, so only PL carries a
    hint environment. *)

type t = Il | Sl | Pl of Hints.Henv.t

(** What a caller asked for, before the spec PL needs has been loaded. *)
type request = [ `IL | `SL | `PL ]

let resolve ~henv : request -> t = function
  | `IL -> Il
  | `SL -> Sl
  | `PL -> Pl henv

let to_string = function Il -> "IL" | Sl -> "SL" | Pl _ -> "PL"
