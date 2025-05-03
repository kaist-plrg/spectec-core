open Source
open Print

(* Backtracking *)

type failtrace = Failtrace of region * string * failtrace list
type 'a attempt = Ok of 'a | Fail of failtrace list

let rec depth (failtrace : failtrace) : int =
  let (Failtrace (_, _, subfailtraces)) = failtrace in
  let subdepth = List.map depth subfailtraces |> List.fold_left max 0 in
  subdepth + 1

let fail (at : region) (msg : string) : 'a attempt =
  Fail [ Failtrace (at, msg, []) ]

let fail_silent : 'a attempt = Fail []

let rec choice = function
  | [] -> fail_silent
  | f :: fs -> (
      match f () with
      | Ok a -> Ok a
      | Fail failtraces_h -> (
          match choice fs with
          | Ok a -> Ok a
          | Fail failtraces_t -> Fail (failtraces_h @ failtraces_t)))

let nest at msg attempt =
  match attempt with
  | Ok a -> Ok a
  | Fail failtraces -> Fail [ Failtrace (at, msg, failtraces) ]

(* Error with backfailtraces *)

let rec string_of_failtrace ?(level = 0) ~(depth : int) ~(bullet : string)
    (failtrace : failtrace) : string =
  let (Failtrace (region, msg, subfailtraces)) = failtrace in
  let smsg =
    if level < depth then ""
    else
      Format.asprintf "%s%s because %s (%s)\n"
        (indent (level - depth))
        bullet msg (string_of_region region)
  in
  Format.asprintf "%s%s" smsg
    (string_of_failtraces ~level:(level + 1) ~depth subfailtraces)

and string_of_failtraces ?(level = 0) ~(depth : int)
    (failtraces : failtrace list) : string =
  match failtraces with
  | [] -> ""
  | [ failtrace ] -> string_of_failtrace ~level ~depth ~bullet:"-" failtrace
  | failtraces ->
      List.mapi
        (fun idx failtrace ->
          string_of_failtrace ~level ~depth
            ~bullet:(string_of_int (idx + 1) ^ ".")
            failtrace)
        failtraces
      |> String.concat ""
