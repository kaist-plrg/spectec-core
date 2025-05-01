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
  include MakePIdEnv (Branch)

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

(* Measuring coverage *)

let is_hit (cover : Cover.t) (pid : pid) : bool =
  let branch = Cover.find pid cover in
  match branch.status with Hit -> true | Miss _ -> false

let is_miss (cover : Cover.t) (pid : pid) : bool =
  let branch = Cover.find pid cover in
  match branch.status with Hit -> false | Miss _ -> true

let is_close_miss (cover : Cover.t) (pid : pid) : bool =
  let branch = Cover.find pid cover in
  match branch.status with
  | Hit -> false
  | Miss filenames -> List.length filenames > 0

let coverage (cover : Cover.t) : int * int * float =
  let total = Cover.cardinal cover in
  let hits =
    Cover.fold
      (fun _ (branch : Branch.t) (hits : int) ->
        match branch.status with Hit -> hits + 1 | Miss _ -> hits)
      cover 0
  in
  let coverage =
    if total = 0 then 0. else float_of_int hits /. float_of_int total *. 100.
  in
  (total, hits, coverage)

(* Targeting close-miss to a restricted set of files *)

let target (cover : Cover.t) (targets : string list PIdMap.t) : Cover.t =
  Cover.mapi
    (fun (pid : pid) (branch : Branch.t) ->
      match branch.status with
      | Hit -> branch
      | Miss _ ->
          let filenames =
            PIdMap.find_opt pid targets |> Option.value ~default:[]
          in
          { branch with status = Miss filenames })
    cover

(* Extension from single coverage *)

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

let log ~(filename_cov_opt : string option) (cover : Cover.t) : unit =
  let output oc_opt =
    match oc_opt with Some oc -> output_string oc | None -> print_string
  in
  let oc_opt = Option.map open_out filename_cov_opt in
  Cover.iter
    (fun (pid : pid) (branch : Branch.t) ->
      let origin = branch.origin in
      match branch.status with
      | Hit -> Format.asprintf "%d Hit %s\n" pid origin.it |> output oc_opt
      | Miss [] -> Format.asprintf "%d Miss %s\n" pid origin.it |> output oc_opt
      | Miss filenames ->
          let filenames = String.concat " " filenames in
          Format.asprintf "%d Miss %s %s\n" pid origin.it filenames
          |> output oc_opt)
    cover;
  Option.iter close_out oc_opt

(* Constructor *)

let init (spec : spec) : Cover.t = Cover.init_spec spec
