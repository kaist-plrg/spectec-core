open Domain.Lib
open Sl.Ast
open Util.Source

(* Phantom branch *)

module Branch = struct
  (* Enclosing relation or function id *)

  type origin = id

  (* Status of a branch:
     if hit, record the filenames that hit it with its likeliness;
     if missed, record the closest-missing filenames;
     note that close-missing files must be well-formed and well-typed *)

  type status = Hit of bool * string list | Miss of string list
  type t = { origin : origin; status : status }

  (* Constructor *)

  let init (id : id) : t = { origin = id; status = Miss [] }

  (* Printer *)

  let to_string (branch : t) : string =
    match branch.status with
    | Hit _ -> "H" ^ branch.origin.it
    | Miss _ -> "M" ^ branch.origin.it
end

(* Phantom coverage map:

   Note that its domain must be set-up initially,
   and no new pid is added during the analysis *)

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

  let init_def (ignores : IdSet.t) (cover : t) (def : def) : t =
    match def.it with
    | TypD _ -> cover
    | RelD (id, _, _, instrs) | DecD (id, _, _, instrs) ->
        if IdSet.mem id ignores then cover else init_instrs cover id instrs

  let init_spec (ignores : IdSet.t) (spec : spec) : t =
    List.fold_left (init_def ignores) empty spec
end

(* Querying coverage *)

let is_hit (cover : Cover.t) (pid : pid) : bool =
  let branch = Cover.find pid cover in
  match branch.status with Hit _ -> true | Miss _ -> false

let is_miss (cover : Cover.t) (pid : pid) : bool =
  let branch = Cover.find pid cover in
  match branch.status with Hit _ -> false | Miss _ -> true

let is_close_miss (cover : Cover.t) (pid : pid) : bool =
  let branch = Cover.find pid cover in
  match branch.status with
  | Hit _ -> false
  | Miss filenames -> List.length filenames > 0

(* Measuring coverage *)

let measure_coverage (cover : Cover.t) : int * int * float =
  let total = Cover.cardinal cover in
  let hits =
    Cover.fold
      (fun _ (branch : Branch.t) (hits : int) ->
        match branch.status with Hit _ -> hits + 1 | Miss _ -> hits)
      cover 0
  in
  let coverage =
    if total = 0 then 0. else float_of_int hits /. float_of_int total *. 100.
  in
  (total, hits, coverage)

(* Extension from single coverage:

   A close-miss is added only if the program is well-typed and well-formed *)

let extend (cover : Cover.t) (filename_p4 : string) (wellformed : bool)
    (welltyped : bool) (cover_single : Single.Cover.t) : Cover.t =
  Cover.mapi
    (fun (pid : pid) (branch : Branch.t) ->
      let branch_single = Single.Cover.find pid cover_single in
      match branch.status with
      | Hit (likely, filenames_p4) -> (
          match branch_single.status with
          | Hit ->
              let likely = likely && not (wellformed && welltyped) in
              let filenames_p4 = filename_p4 :: filenames_p4 in
              { branch with status = Hit (likely, filenames_p4) }
          | _ -> branch)
      | Miss filenames_p4 -> (
          match branch_single.status with
          | Hit ->
              let likely = not (wellformed && welltyped) in
              let filenames_p4 = [ filename_p4 ] in
              { branch with status = Hit (likely, filenames_p4) }
          | Miss (_ :: _) when wellformed && welltyped ->
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
  (* Output overall coverage *)
  let total, hits, coverage = measure_coverage cover in
  Format.asprintf "# Overall Coverage: %d/%d (%.2f%%)\n" hits total coverage
  |> output oc_opt;
  (* Collect covers by origin *)
  let covers_origin =
    Cover.fold
      (fun (pid : pid) (branch : Branch.t) (covers_origin : Cover.t IdMap.t) ->
        let origin = branch.origin in
        let cover_origin =
          match IdMap.find_opt origin covers_origin with
          | Some cover_origin -> Cover.add pid branch cover_origin
          | None -> Cover.add pid branch Cover.empty
        in
        IdMap.add origin cover_origin covers_origin)
      cover IdMap.empty
  in
  IdMap.iter
    (fun origin cover_origin ->
      let total, hits, coverage = measure_coverage cover_origin in
      Format.asprintf "# Coverage for %s: %d/%d (%.2f%%)\n" origin.it hits total
        coverage
      |> output oc_opt;
      Cover.iter
        (fun (pid : pid) (branch : Branch.t) ->
          let origin = branch.origin in
          match branch.status with
          | Hit (likely, filenames) ->
              let filenames = String.concat " " filenames in
              Format.asprintf "%d Hit_%s %s %s\n" pid
                (if likely then "likely" else "unlikely")
                origin.it filenames
              |> output oc_opt
          | Miss [] ->
              Format.asprintf "%d Miss %s\n" pid origin.it |> output oc_opt
          | Miss filenames ->
              let filenames = String.concat " " filenames in
              Format.asprintf "%d Miss %s %s\n" pid origin.it filenames
              |> output oc_opt)
        cover_origin)
    covers_origin;
  Option.iter close_out oc_opt

(* Constructor *)

let init (ignores : IdSet.t) (spec : spec) : Cover.t =
  Cover.init_spec ignores spec
