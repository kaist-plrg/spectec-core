open Domain.Lib
open Sl.Ast
open Util.Source
module S = Set.Make (Int)
module M = Map.Make (Int)

(* Phantom coverage set *)

module Dom = S

(* Phantom coverage map

   A map from a phantom branch to hit filenames and closest-miss filenames *)

module Cover = struct
  type t = (string list * string list) M.t

  let empty = M.empty

  let dom_hit (cover : t) : Dom.t =
    M.fold
      (fun pid (filenames_hit, _) dom ->
        if List.length filenames_hit > 0 then Dom.add pid dom else dom)
      cover Dom.empty

  let find (pid : int) (cover : t) : string list * string list =
    match M.find_opt pid cover with
    | Some (filenames_hit, filenames_miss) -> (filenames_hit, filenames_miss)
    | None -> ([], [])

  (* Constructor *)

  let hit (pid : int) (filename : string) (cover : t) : t =
    match M.find_opt pid cover with
    | Some (filenames_hit, filenames_miss) ->
        let filenames_hit = filename :: filenames_hit in
        M.add pid (filenames_hit, filenames_miss) cover
    | None ->
        let filenames_hit = [ filename ] in
        let filenames_miss = [] in
        M.add pid (filenames_hit, filenames_miss) cover

  let miss (pid : int) (filename : string) (cover : t) : t =
    match M.find_opt pid cover with
    | Some (filenames_hit, filenames_miss) ->
        let filenames_miss = filename :: filenames_miss in
        M.add pid (filenames_hit, filenames_miss) cover
    | None ->
        let filenames_hit = [] in
        let filenames_miss = [ filename ] in
        M.add pid (filenames_hit, filenames_miss) cover
end

(* Phantom node to its enclosing relation or function *)

module Origin = struct
  type t = Id.t M.t

  let dom (origin : t) : Dom.t =
    M.fold (fun pid _ dom -> Dom.add pid dom) origin Dom.empty

  let find = M.find

  (* Constructor *)

  let rec collect_instr (origin : t) (id : id) (instr : instr) : t =
    match instr.it with
    | IfI (_, _, instrs_then, phantom_opt) -> (
        let origin = collect_instrs origin id instrs_then in
        match phantom_opt with
        | Some (pid, _) -> M.add pid id origin
        | None -> origin)
    | CaseI (_, cases, phantom_opt) -> (
        let blocks = cases |> List.split |> snd in
        let origin =
          List.fold_left
            (fun origin instrs -> collect_instrs origin id instrs)
            origin blocks
        in
        match phantom_opt with
        | Some (pid, _) -> M.add pid id origin
        | None -> origin)
    | OtherwiseI instr -> collect_instr origin id instr
    | _ -> origin

  and collect_instrs (origin : t) (id : id) (instrs : instr list) : t =
    List.fold_left
      (fun origin instr -> collect_instr origin id instr)
      origin instrs

  let collect_def (origin : t) (def : def) : t =
    match def.it with
    | TypD _ -> origin
    | RelD (id, _, _, instrs) | DecD (id, _, _, instrs) ->
        collect_instrs origin id instrs

  let collect_spec (spec : spec) : t =
    let origin = M.empty in
    List.fold_left collect_def origin spec

  (* Reverse the origin map *)

  let reverse (origin : t) (pids : int list) : int list IdMap.t =
    List.fold_left
      (fun origin_rev pid ->
        let id = find pid origin in
        match IdMap.find_opt id origin_rev with
        | Some pids -> IdMap.add id (pids @ [ pid ]) origin_rev
        | None -> IdMap.add id [ pid ] origin_rev)
      IdMap.empty pids
end

(* Log coverage *)

let log_cover_originwise (origin : Origin.t) (cover : Cover.t) (pids : int list)
    : unit =
  let rec log_lines pids count =
    if count = 9 then print_newline ();
    match pids with
    | [] -> print_newline ()
    | pid :: pids ->
        let filenames_hit, filenames_miss = Cover.find pid cover in
        Format.asprintf "%d(H%d,M%d) " pid
          (List.length filenames_hit)
          (List.length filenames_miss)
        |> print_string;
        if count = 9 then log_lines pids 0 else log_lines pids (count + 1)
  in
  Origin.reverse origin pids
  |> IdMap.iter (fun id pids ->
         Format.asprintf "* %s" id.it |> print_endline;
         log_lines pids 0;
         print_newline ())

let log_cover (mode : bool) (total : int) (origin : Origin.t) (cover : Cover.t)
    (dom : Dom.t) : unit =
  let pids =
    Dom.fold (fun pid pids -> pid :: pids) dom [] |> List.sort Int.compare
  in
  let coverage =
    (List.length pids |> float_of_int) /. (total |> float_of_int) *. 100.0
  in
  Format.asprintf "----- %s (%d/%d[%.2f%%]) -----\n"
    (if mode then "Covered" else "Uncovered")
    (List.length pids) total coverage
  |> print_endline;
  log_cover_originwise origin cover pids

let log_closest_miss (cover : Cover.t) (dom_uncover : Dom.t)
    (dirname_closest_miss : string) : unit =
  let pids_uncover =
    Dom.fold (fun pid pids_uncover -> pid :: pids_uncover) dom_uncover []
    |> List.sort Int.compare
  in
  let dirname_closest_miss =
    dirname_closest_miss ^ "/closest-miss-"
    ^ (Unix.gettimeofday () |> string_of_float)
  in
  Unix.mkdir dirname_closest_miss 0o755;
  List.iter
    (fun pid ->
      let filenames_miss = Cover.find pid cover |> snd in
      if List.length filenames_miss = 0 then ()
      else
        let filename =
          Format.asprintf "%s/phantom-%d.log" dirname_closest_miss pid
        in
        let oc = open_out filename in
        String.concat "\n" filenames_miss |> output_string oc;
        close_out oc)
    pids_uncover

let log (spec : spec) (cover : Cover.t)
    (dirname_closest_miss_opt : string option) : unit =
  let origin = Origin.collect_spec spec in
  let dom_all = Origin.dom origin in
  let dom_cover = Cover.dom_hit cover in
  let dom_uncover = Dom.diff dom_all dom_cover in
  let total = Dom.cardinal dom_all in
  log_cover true total origin cover dom_cover;
  log_cover false total origin cover dom_uncover;
  Option.iter (log_closest_miss cover dom_uncover) dirname_closest_miss_opt
