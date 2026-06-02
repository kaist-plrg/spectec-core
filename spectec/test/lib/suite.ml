(** Test suite runner infrastructure *)

open Core

type expectation = Spectec.Task.expectation

type config = {
  name : string;
  intro : string;
  heading : string;
  success : string;
  failure : string;
  expected_failure : string;
  unexpected_success : string;
}

(* Safely run a test, catching exceptions and tracking duration accurately *)
let run_safely run filename : float * (unit, Stats.test_error) result =
  let start = Timer.now () in
  let result =
    try
      match run filename with
      | Ok () -> Ok ()
      | Error err -> Error (Stats.Runner_error err)
    with exn -> Error (Stats.Exception exn)
  in
  let duration = Timer.now () -. start in
  (duration, result)

let run ~(config : config) ~(exclude_set : Exclude.t) ~(filenames : string list)
    ~(expectation : expectation)
    ~(run : string -> (unit, Spectec.Error.t) result) =
  let total = List.length filenames in
  Format.printf "%s %d files\n\n" config.intro total;
  let stats =
    List.fold filenames ~init:Stats.empty ~f:(fun stats filename ->
        Format.printf ">>> Running %s on %s\n" config.heading filename;
        if Exclude.mem exclude_set filename then (
          Format.printf "Excluding file: %s\n\n" filename;
          Stats.add_skip stats ~label:filename)
        else
          let duration, result = run_safely run filename in
          let stats =
            match (expectation, result) with
            (* Positive tests: success expected *)
            | Positive, Ok () ->
                Format.printf "%s: %s\n\n" config.success filename;
                Stats.add_pass stats ~label:filename ~duration
            | Positive, Error err ->
                let prefix =
                  if Stats.is_exception err then "CRASHED" else config.failure
                in
                Format.printf "%s: %s\n%s\n\n" prefix filename
                  (Stats.string_of_test_error err);
                Stats.add_fail stats ~label:filename ~duration
                  (Stats.failure_from_test_error err)
            (* Negative tests: failure expected *)
            | Negative, Ok () ->
                Format.printf "%s: %s\n\n" config.unexpected_success filename;
                Stats.add_fail stats ~label:filename ~duration
                  Stats.failure_unexpected_success
            | Negative, Error err ->
                let prefix =
                  if Stats.is_exception err then "CRASHED (counted as failure)"
                  else config.expected_failure
                in
                Format.printf "%s: %s\n%s\n\n" prefix filename
                  (Stats.string_of_test_error err);
                Stats.add_pass stats ~label:filename ~duration
          in
          stats)
  in
  Stats.print_summary config.name stats;
  Format.printf "\n%!"
