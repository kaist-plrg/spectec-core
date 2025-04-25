open Sl.Ast
open Util.Source

(* Phantom coverage set *)

module Dom = Set.Make (Int)

(* Phantom coverage map *)

module Branch = struct
  (* Enclosing relation or function id *)

  type origin = id

  (* Status of a branch:
     if missed, record the value ids for closest-AST derivation *)

  type status = Hit | Miss of vid list
  type t = { origin : origin; status : status }

  (* Constructor *)

  let init (id : id) : t = { origin = id; status = Miss [] }
end

module Cover = struct
  module M = Map.Make (Int)

  type t = Branch.t M.t

  (* Hit and miss *)

  let hit (cover : t) (pid : pid) : t =
    let branch = M.find pid cover in
    let branch = { branch with status = Hit } in
    M.add pid branch cover

  let miss (cover : t) (pid : pid) (vid : vid) : t =
    let branch = M.find pid cover in
    match branch.status with
    | Hit -> cover
    | Miss vids ->
        let branch = { branch with status = Miss (vid :: vids) } in
        M.add pid branch cover

  (* Collector *)

  let collect_miss (cover : t) : (pid * vid list) list =
    M.fold
      (fun (pid : pid) (branch : Branch.t) (misses : (pid * vid list) list) ->
        match branch.status with
        | Hit -> misses
        | Miss vids -> (pid, vids) :: misses)
      cover []
    |> List.rev

  (* Constructor *)

  let rec init_instr (cover : t) (id : id) (instr : instr) : t =
    match instr.it with
    | IfI (_, _, instrs_then, phantom_opt) -> (
        let cover = init_instrs cover id instrs_then in
        match phantom_opt with
        | Some (pid, _) ->
            let branch = Branch.init id in
            M.add pid branch cover
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
            M.add pid branch cover
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

  let init_spec (spec : spec) : t =
    let cover = M.empty in
    List.fold_left init_def cover spec
end

(* Interface *)

let init (spec : spec) : Cover.t = Cover.init_spec spec
let hit (cover : Cover.t) (pid : pid) : Cover.t = Cover.hit cover pid

let miss (cover : Cover.t) (pid : pid) (vid : vid) : Cover.t =
  Cover.miss cover pid vid

let collect_miss (cover : Cover.t) : (pid * vid list) list =
  Cover.collect_miss cover
