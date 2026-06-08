type config = {
  max_size : int;
  seed : [ `Deterministic of int | `Nondeterministic ];
  verbose : bool;
}

let default_config =
  { max_size = 50; seed = `Deterministic 42; verbose = false }

type outcome =
  | Pass of { num_tests : int; stamps : (string * int) list }
  | Fail of { num_tests : int; counterexample : string list }
  | Gave_up of { num_tests : int }

let count_stamps stamps =
  let tbl = Hashtbl.create 16 in
  List.iter
    (fun s ->
      let n = try Hashtbl.find tbl s with Not_found -> 0 in
      Hashtbl.replace tbl s (n + 1))
    stamps;
  Hashtbl.fold (fun k v acc -> (k, v) :: acc) tbl []
  |> List.sort (fun (_, a) (_, b) -> compare b a)

let rec shrink_loop (v : Property.Verdict.t) : Property.Verdict.t =
  let candidate_gens = v.Property.Verdict.shrink () in
  let failing =
    List.find_map
      (fun gen ->
        let v' = Gen.run gen ~size:0 ~rand:(Random.make 0) in
        if v'.Property.Verdict.status = `Fail then Some v' else None)
      candidate_gens
  in
  match failing with None -> v | Some v' -> shrink_loop v'

let rec generalize_loop (v : Property.Verdict.t) : Property.Verdict.t =
  let candidates = v.Property.Verdict.generalize () in
  let found =
    List.find_map
      (fun (_, gens) ->
        if gens = [] then None
        else
          let results =
            List.map (fun gen -> Gen.run gen ~size:3 ~rand:(Random.make 0)) gens
          in
          if
            List.exists (fun v' -> v'.Property.Verdict.status = `Fail) results
            && List.for_all
                 (fun v' -> v'.Property.Verdict.status <> `Pass)
                 results
          then Some (List.hd results)
          else None)
      candidates
  in
  match found with
  | None -> v
  | Some v' ->
      if v'.Property.Verdict.arguments = v.Property.Verdict.arguments then v
      else generalize_loop v'

let run ~num_tests ?(config = default_config) prop =
  let rand =
    match config.seed with
    | `Deterministic n -> Random.make n
    | `Nondeterministic -> Random.make_self_init ()
  in
  let rec loop i discarded all_stamps rand =
    if i >= num_tests then
      Pass { num_tests = i; stamps = count_stamps all_stamps }
    else if discarded > num_tests * 10 then Gave_up { num_tests = i }
    else
      let size =
        if config.max_size = 0 then 0 else i mod (config.max_size + 1)
      in
      let trial_rand, next_rand = Random.split rand in
      let verdict = Gen.run prop ~size ~rand:trial_rand in
      if config.verbose then
        Printf.printf "Test %d: %s\n%!" (i + 1)
          (match verdict.Property.Verdict.status with
          | `Pass -> "OK"
          | `Fail -> "FAIL"
          | `Discard -> "discarded");
      match verdict.Property.Verdict.status with
      | `Fail ->
          let minimal = shrink_loop verdict in
          let generalized = generalize_loop minimal in
          Fail
            {
              num_tests = i + 1;
              counterexample = generalized.Property.Verdict.arguments;
            }
      | `Pass ->
          loop (i + 1) discarded
            (verdict.Property.Verdict.stamp @ all_stamps)
            next_rand
      | `Discard -> loop i (discarded + 1) all_stamps next_rand
  in
  loop 0 0 [] rand

let print_outcome = function
  | Pass { num_tests; stamps } ->
      Printf.printf "OK, passed %d samples.\n" num_tests;
      List.iter
        (fun (lbl, count) ->
          Printf.printf "%3d%% %s\n\n" (count * 100 / num_tests) lbl)
        stamps
  | Fail { num_tests; counterexample } ->
      Printf.printf "Falsifiable, after %d tests:\n" num_tests;
      List.iter (fun s -> Printf.printf "  %s\n" s) counterexample
  | Gave_up { num_tests } ->
      Printf.printf "Gave up after %d tests (too many discarded).\n" num_tests
