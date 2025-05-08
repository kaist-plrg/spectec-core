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

  type status = Hit of bool * string list | Miss of string list * string list
  type t = { origin : origin; status : status }

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

  let init_target (target : string list PIdMap.t)
      (cover_multi : Multiple.Cover.t) : t =
    Multiple.Cover.mapi
      (fun (pid : pid) (branch : Multiple.Branch.t) ->
        let origin = branch.origin in
        let status =
          match branch.status with
          | Hit (likely, filenames) -> Branch.Hit (likely, filenames)
          | Miss filenames ->
              let filenames_target =
                match PIdMap.find_opt pid target with
                | Some filenames_target -> filenames_target
                | None -> []
              in
              let filenames_target, filenames_untarget =
                List.partition
                  (fun filename -> List.mem filename filenames_target)
                  filenames
              in
              Branch.Miss (filenames_target, filenames_untarget)
        in
        Branch.{ origin; status })
      cover_multi

  let init_roundrobin (cover_multi : Multiple.Cover.t) : t =
    Multiple.Cover.map
      (fun (branch : Multiple.Branch.t) ->
        let origin = branch.origin in
        let status =
          match branch.status with
          | Hit (likely, filenames) -> Branch.Hit (likely, filenames)
          | Miss filenames -> Branch.Miss (filenames, [])
        in
        Branch.{ origin; status })
      cover_multi
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
  | Miss (filenames_target, filenames_untarget) ->
      List.length filenames_target > 0 || List.length filenames_untarget > 0

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

(* Logging *)

let log ~(target : bool) ~(filename_cov_opt : string option) (cover : Cover.t) :
    unit =
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
          | Miss ([], []) ->
              Format.asprintf "%d Miss %s\n" pid origin.it |> output oc_opt
          | Miss (filenames_target, filenames_untarget) ->
              let filenames =
                if target then filenames_target
                else filenames_target @ filenames_untarget
              in
              let filenames = String.concat " " filenames in
              Format.asprintf "%d Miss %s %s\n" pid origin.it filenames
              |> output oc_opt)
        cover_origin)
    covers_origin;
  Option.iter close_out oc_opt

(* Constructor *)

let init_roundrobin (cover_multi : Multiple.Cover.t) : Cover.t =
  Cover.init_roundrobin cover_multi

let init_target (target : string list PIdMap.t) (cover_multi : Multiple.Cover.t)
    : Cover.t =
  Cover.init_target target cover_multi
