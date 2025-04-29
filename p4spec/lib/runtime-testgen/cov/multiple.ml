open Domain.Lib
open Sl.Ast
open Util.Source

(* Phantom coverage map *)

module Branch = struct
  (* Enclosing relation or function id *)

  type origin = id

  (* Status of a branch:
     if missed, record the closest-missing filenames *)

  type status = Hit | Miss of string list
  type t = { origin : origin; status : status }

  (* Constructor *)

  let init (id : id) : t = { origin = id; status = Miss [] }

  (* Printer *)

  let to_string (branch : t) : string =
    match branch.status with
    | Hit -> "H" ^ branch.origin.it
    | Miss _ -> "M" ^ branch.origin.it
end

module Cover = struct
  include MakeVIdEnv (Branch)

  (* Constructor *)

  let rec init_instr (cover : t) (id : id) (instr : instr) : t =
    match instr.it with
    | IfI (_, _, instrs_then, phantom_opt) -> (
        let cover = init_instrs cover id instrs_then in
        match phantom_opt with
        | Some (pid, _) ->
            let branch = Branch.init id in
            add pid branch cover
        | None -> cover)
    | CaseI (_, cases, phantom_opt) -> (
        let blocks = cases |> List.split |> snd in
        let cover =
          List.fold_left
            (fun cover instrs -> init_instrs cover id instrs)
            cover blocks
        in
        match phantom_opt with
        | Some (pid, _) ->
            let branch = Branch.init id in
            add pid branch cover
        | None -> cover)
    | OtherwiseI instr -> init_instr cover id instr
    | _ -> cover

  and init_instrs (cover : t) (id : id) (instrs : instr list) : t =
    List.fold_left (fun cover instr -> init_instr cover id instr) cover instrs

  let init_def (cover : t) (def : def) : t =
    match def.it with
    | TypD _ -> cover
    | RelD (id, _, _, instrs) | DecD (id, _, _, instrs) ->
        init_instrs cover id instrs

  let init_spec (spec : spec) : t = List.fold_left init_def empty spec
end

(* Collector *)

let collect_miss (cover : Cover.t) : (pid * string list) list =
  Cover.fold
    (fun (pid : pid) (branch : Branch.t) (misses : (pid * string list) list) ->
      match branch.status with
      | Hit -> misses
      | Miss filenames -> (pid, filenames) :: misses)
    cover []
  |> List.rev

(* Extension *)

let extend (cover : Cover.t) (filename_p4 : string)
    (cover_single : Single.Cover.t) : Cover.t =
  Cover.mapi
    (fun (pid : pid) (branch : Branch.t) ->
      let branch_single = Single.Cover.find pid cover_single in
      match branch.status with
      | Hit -> branch
      | Miss filenames_p4 -> (
          match branch_single.status with
          | Hit -> { branch with status = Hit }
          | Miss (_ :: _) ->
              let filenames_p4 = filename_p4 :: filenames_p4 in
              { branch with status = Miss filenames_p4 }
          | Miss _ -> branch))
    cover

(* Logging *)

let log_miss (cover : Cover.t) (pids_miss : PIdSet.t) : unit =
  let module PGroup = IdMap in
  let pid_group =
    PIdSet.fold
      (fun pid pid_group ->
        let branch = Cover.find pid cover in
        let origin = branch.origin in
        match PGroup.find_opt origin pid_group with
        | Some pids -> PGroup.add origin (pid :: pids) pid_group
        | None -> PGroup.add origin [ pid ] pid_group)
      pids_miss PGroup.empty
  in
  PGroup.iter
    (fun origin pids ->
      Format.asprintf "=== %s ===" origin.it |> print_endline;
      List.iter (fun pid -> pid |> Sl.Print.string_of_pid |> print_endline) pids)
    pid_group

let log ?(short = false) (cover : Cover.t) : unit =
  let pids_miss =
    Cover.fold
      (fun (pid : pid) (branch : Branch.t) (pids_miss : PIdSet.t) ->
        match branch.status with
        | Hit -> pids_miss
        | Miss _ -> PIdSet.add pid pids_miss)
      cover PIdSet.empty
  in
  let total = Cover.cardinal cover in
  let hit = total - PIdSet.cardinal pids_miss in
  Format.asprintf "[Coverage] %d/%d (%.2f%%)" hit total
    (float_of_int hit /. float_of_int total *. 100.)
  |> print_endline;
  if short then () else log_miss cover pids_miss

(* Constructor *)

let init (spec : spec) : Cover.t = Cover.init_spec spec
