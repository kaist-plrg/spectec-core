(** Test statistics collection and reporting *)

open Core

(* Unified error type for test runs *)
type test_error = Runner_error of Spectec.Error.t | Exception of exn

let string_of_test_error = function
  | Runner_error err ->
      Spectec.Diagnostic.Render.render_bag_trace
        (Spectec.Error.to_diagnostics err)
  | Exception exn -> "uncaught exception: " ^ Exn.to_string exn

let is_exception = function Exception _ -> true | Runner_error _ -> false

(** Failure type for statistics tracking *)
type failure = Test_error of test_error | Unexpected_success

type float_summary = {
  count : int;
  sum : float;
  mean : float option;
  median : float option;
  p95 : float option;
  min : float option;
  max : float option;
}

type t = {
  passed : int;
  failed : int;
  skipped : int;
  failures : (string * failure) list;
  passed_durations : float list;
  failed_durations : float list;
}

let empty : t =
  {
    passed = 0;
    failed = 0;
    skipped = 0;
    failures = [];
    passed_durations = [];
    failed_durations = [];
  }

let add_pass (stat : t) ~(label : string) ~(duration : float) : t =
  ignore label;
  {
    stat with
    passed = stat.passed + 1;
    passed_durations = duration :: stat.passed_durations;
  }

let add_fail (stat : t) ~(label : string) ~(duration : float)
    (failure : failure) : t =
  {
    stat with
    failed = stat.failed + 1;
    failures = (label, failure) :: stat.failures;
    failed_durations = duration :: stat.failed_durations;
  }

let add_skip (stat : t) ~(label : string) : t =
  ignore label;
  { stat with skipped = stat.skipped + 1 }

let failure_from_test_error err = Test_error err
let failure_unexpected_success = Unexpected_success

let summarize_floats values =
  let count = List.length values in
  let sum = List.fold values ~init:0.0 ~f:( +. ) in
  if Int.equal count 0 then
    {
      count;
      sum;
      mean = None;
      median = None;
      p95 = None;
      min = None;
      max = None;
    }
  else
    let sorted = List.sort values ~compare:Float.compare in
    let mean = Some (sum /. Float.of_int count) in
    let min = List.hd sorted in
    let max = List.last sorted in
    let median =
      if Int.( % ) count 2 = 1 then List.nth sorted (count / 2)
      else
        let lower = List.nth sorted ((count / 2) - 1) in
        let upper = List.nth sorted (count / 2) in
        Option.both lower upper |> Option.map ~f:(fun (l, u) -> (l +. u) /. 2.0)
    in
    let percentile p =
      let index = Float.of_int (count - 1) *. p |> Float.iround_down_exn in
      List.nth sorted index
    in
    { count; sum; mean; median; p95 = percentile 0.95; min; max }

let format_float value = Printf.sprintf "%.6f" value

let format_float_option = function
  | None -> "n/a"
  | Some value -> format_float value

let print_summary name (stat : t) =
  let {
    passed;
    failed;
    skipped;
    failures = _;
    passed_durations;
    failed_durations;
  } =
    stat
  in
  let total = passed + failed + skipped in
  let executed = passed + failed in
  let executed_rate =
    if total = 0 then 0.0
    else float_of_int executed /. float_of_int total *. 100.0
  in
  let skip_rate =
    if total = 0 then 0.0
    else float_of_int skipped /. float_of_int total *. 100.0
  in
  let pass_rate =
    if executed = 0 then 0.0
    else float_of_int passed /. float_of_int executed *. 100.0
  in
  let fail_rate =
    if executed = 0 then 0.0
    else float_of_int failed /. float_of_int executed *. 100.0
  in
  let duration_pass = summarize_floats passed_durations in
  let duration_fail = summarize_floats failed_durations in
  let duration_all =
    summarize_floats (stat.passed_durations @ failed_durations)
  in
  let title = String.capitalize name in
  Format.printf "%s summary\n" title;
  Format.printf "  Ran:     %6d / %-6d (%.2f%%)\n" executed total executed_rate;
  Format.printf "  Passed:  %6d (%.2f%% of ran)\n" stat.passed pass_rate;
  Format.printf "  Failed:  %6d (%.2f%% of ran)\n" stat.failed fail_rate;
  Format.printf "  Skipped: %6d (%.2f%% of total)\n\n" stat.skipped skip_rate;
  Format.eprintf "  Duration (s):\n";

  Format.eprintf "    Total:   %9.3f\n" duration_all.sum;
  Format.eprintf "    Mean:    %9s\n" (format_float_option duration_all.mean);
  Format.eprintf "    Median:  %9s\n" (format_float_option duration_all.median);
  Format.eprintf "    Min:     %9s\n" (format_float_option duration_all.min);
  Format.eprintf "    Max:     %9s\n" (format_float_option duration_all.max);
  Format.eprintf "    P95:     %9s\n" (format_float_option duration_all.p95);

  if stat.passed > 0 && stat.failed > 0 then (
    Format.eprintf "  Pass Stats (s):\n";
    Format.eprintf "    Mean:    %9s  Min: %9s  Max: %9s\n"
      (format_float_option duration_pass.mean)
      (format_float_option duration_pass.min)
      (format_float_option duration_pass.max);
    Format.eprintf "  Fail Stats (s):\n";
    Format.eprintf "    Mean:    %9s  Min: %9s  Max: %9s\n"
      (format_float_option duration_fail.mean)
      (format_float_option duration_fail.min)
      (format_float_option duration_fail.max));
  if stat.failed > 0 then (
    Format.printf "\nFailures (%d):\n" stat.failed;
    List.iter (List.rev stat.failures) ~f:(fun (label, failure) ->
        match failure with
        | Test_error err ->
            let tag = if is_exception err then "CRASH" else "FAIL" in
            Format.printf "  [%s] %s\n    %s\n" tag label
              (string_of_test_error err)
        | Unexpected_success ->
            Format.printf "  [FAIL] %s\n    expected failure but succeeded\n"
              label));

  Format.printf "%!" (* Flush the output *)
