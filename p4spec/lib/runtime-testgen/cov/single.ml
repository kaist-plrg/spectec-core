open Domain.Lib
open Sl.Ast
open Util.Source

(* Phantom branch *)

module Branch = struct
  (* Enclosing relation or function id *)

  type origin = id

  (* Status of a branch:
     if missed, record the value ids for closest-AST derivation *)

  type status = Hit | Miss of vid list
  type t = { origin : origin; status : status }

  (* Constructor *)

  let init (id : id) : t = { origin = id; status = Miss [] }

  (* Printer *)

  let to_string (branch : t) : string =
    match branch.status with
    | Hit -> "H" ^ branch.origin.it
    | Miss _ -> "M" ^ branch.origin.it
end

(* Phantom coverage map *)

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

(* Hit and miss *)

let hit (cover : Cover.t) (pid : pid) : Cover.t =
  let branch = Cover.find pid cover in
  let branch = { branch with status = Hit } in
  Cover.add pid branch cover

let miss (cover : Cover.t) (pid : pid) (vid : vid) : Cover.t =
  let branch = Cover.find pid cover in
  match branch.status with
  | Hit -> cover
  | Miss vids ->
      let branch = { branch with status = Miss (vid :: vids) } in
      Cover.add pid branch cover

(* Collector *)

let collect_hit (cover : Cover.t) : pid list =
  Cover.fold
    (fun (pid : pid) (branch : Branch.t) (hits : pid list) ->
      match branch.status with Hit -> pid :: hits | Miss _ -> hits)
    cover []
  |> List.rev

let collect_miss (cover : Cover.t) : (pid * vid list) list =
  Cover.fold
    (fun (pid : pid) (branch : Branch.t) (misses : (pid * vid list) list) ->
      match branch.status with
      | Hit -> misses
      | Miss vids -> (pid, vids) :: misses)
    cover []
  |> List.rev

(* Constructor *)

let init (spec : spec) : Cover.t = Cover.init_spec spec
