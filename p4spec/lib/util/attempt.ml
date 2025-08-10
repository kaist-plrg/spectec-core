open Source

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

(* Extract region from Fail *)

let rec region_of_failtrace failtrace =
  let (Failtrace (region, _, failtraces)) = failtrace in
  if region = no_region then region_of_failtraces failtraces else region

and region_of_failtraces failtraces =
  match failtraces with
  | [] -> no_region
  | [ failtrace ] -> region_of_failtrace failtrace
  | failtrace :: _ -> region_of_failtrace failtrace

let compare_failtrace failtrace_l failtrace_r =
  let (Failtrace (region_l, _, _)) = failtrace_l in
  let (Failtrace (region_r, _, _)) = failtrace_r in
  compare_region region_l region_r

(* Error with backfailtraces *)

let rec string_of_failtrace ?(indent = "") ?(level = 0) ~(region_parent : region) ~(is_last : bool)
    ~(depth : int) ~(bullet : string) (failtrace : failtrace) : string =
  let (Failtrace (region, msg, subfailtraces)) = failtrace in
  let smsg =
    if level < depth then ""
    else
      let prefix = if is_last then "└── " else "├── " in
      let indent_prefix = String.make 4 ' ' in
      Format.asprintf "%s%s%s%s%s\n" indent prefix
        (if region_parent = region || region = no_region then ""
         else
           string_of_region region ^ "\n"
            ^ indent ^ indent_prefix) bullet
        msg
  in
  let region_parent = if region = no_region then region_parent else region in
  let indent = if is_last then indent ^ "    " else indent ^ "│   " in
  Format.asprintf "%s%s" smsg
    (string_of_failtraces ~indent ~level:(level+1) ~region_parent ~depth subfailtraces)

and string_of_failtraces ?(indent = "") ?(level = 0) ~(region_parent : region) ~(depth : int)
    (failtraces : failtrace list) : string =
  match failtraces with
  | [] -> ""
  | [ failtrace ] ->
      string_of_failtrace ~indent ~level ~region_parent ~is_last:true ~depth ~bullet:"" failtrace
  | failtraces ->
      List.mapi
        (fun idx failtrace ->
          let is_last = idx = List.length failtraces - 1 in
          let bullet = string_of_int (idx + 1) ^ ". "
          in
          string_of_failtrace ~indent ~level ~region_parent ~is_last ~depth ~bullet failtrace)
        failtraces
      |> String.concat ""
