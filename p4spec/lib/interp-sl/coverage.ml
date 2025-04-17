open Domain.Lib
open Sl.Ast
open Util.Source

(* Phantom coverage *)

module Cover = Set.Make (Int)

type t = Cover.t

(* Collecting coverage *)

let empty = Cover.empty
let size = Cover.cardinal
let add = Cover.add
let diff = Cover.diff
let fold = Cover.fold

(* Phantom node to its enclosing relation or function *)

module Origin = Map.Make (Int)

type origin = Id.t Origin.t

(* Collect pids over the entire spec *)

let rec collect_instr (origin : origin) (id : id) (instr : instr) : origin =
  match instr.it with
  | IfI (_, _, instrs_then, instrs_else) ->
      let origin = collect_instrs origin id instrs_then in
      collect_instrs origin id instrs_else
  | OtherwiseI instr -> collect_instr origin id instr
  | PhantomI (pid, _) -> Origin.add pid id origin
  | _ -> origin

and collect_instrs (origin : origin) (id : id) (instrs : instr list) : origin =
  List.fold_left
    (fun origin instr -> collect_instr origin id instr)
    origin instrs

let collect_def (origin : origin) (def : def) : origin =
  match def.it with
  | TypD _ -> origin
  | RelD (id, _, _, instrs) | DecD (id, _, _, instrs) ->
      collect_instrs origin id instrs

let collect_spec (spec : spec) : origin =
  let origin = Origin.empty in
  List.fold_left collect_def origin spec

(* Log coverage *)

let log_cover_detail (origin : origin) (cover : int list) : unit =
  let rec log_lines cover count =
    if count = 9 then print_newline ();
    match cover with
    | [] -> print_newline ()
    | pid :: cover ->
        Format.asprintf "%d " pid |> print_string;
        if count = 9 then log_lines cover 0 else log_lines cover (count + 1)
  in
  let revorigin =
    List.fold_left
      (fun revorigin pid ->
        let id = Origin.find pid origin in
        match IdMap.find_opt id revorigin with
        | Some pids -> IdMap.add id (pids @ [ pid ]) revorigin
        | None -> IdMap.add id [ pid ] revorigin)
      IdMap.empty cover
  in
  IdMap.iter
    (fun id pids ->
      Format.asprintf "* %s" id.it |> print_endline;
      log_lines pids 0;
      print_newline ())
    revorigin

let log_cover (mode : bool) (total : int) (origin : origin) (cover : t) : unit =
  let cover =
    fold (fun pid pids -> pid :: pids) cover [] |> List.sort Int.compare
  in
  let coverage =
    (List.length cover |> float_of_int) /. (total |> float_of_int) *. 100.0
  in
  Format.asprintf "----- %s (%d/%d[%.2f%%]) -----"
    (if mode then "Covered" else "Uncovered")
    (List.length cover) total coverage
  |> print_endline;
  log_cover_detail origin cover

let log (spec : spec) (cover : t) : unit =
  let origin = collect_spec spec in
  let cover_all = Origin.fold (fun pid _ cover -> add pid cover) origin empty in
  let uncover = diff cover_all cover in
  let total = size cover_all in
  log_cover true total origin cover;
  log_cover false total origin uncover
