open Common.Source

(* Core types *)

type severity = Error | Warning | Info | Hint

let severity_rank = function Error -> 0 | Warning -> 1 | Info -> 2 | Hint -> 3
let compare_severity a b = Int.compare (severity_rank a) (severity_rank b)

type related = { region : region; message : string }
type fix = { message : string; edits : (region * string) list }

type trace_node = {
  region : region;
  message : string;
  children : trace_node list;
}

type t = {
  severity : severity;
  region : region;
  code : string option;
  message : string;
  detail : string option;
  related : related list;
  fixes : fix list;
  trace : trace_node list;
  source : string;
}

(* Smart constructors *)

let error ?code ?detail ?(related = []) ?(fixes = []) ?(trace = []) ~source
    region message =
  {
    severity = Error;
    region;
    code;
    message;
    detail;
    related;
    fixes;
    trace;
    source;
  }

let warning ?code ?detail ~source region message =
  {
    severity = Warning;
    region;
    code;
    message;
    detail;
    related = [];
    fixes = [];
    trace = [];
    source;
  }

let info ~source region message =
  {
    severity = Info;
    region;
    code = None;
    message;
    detail = None;
    related = [];
    fixes = [];
    trace = [];
    source;
  }

let hint ~source region message =
  {
    severity = Hint;
    region;
    code = None;
    message;
    detail = None;
    related = [];
    fixes = [];
    trace = [];
    source;
  }

(* Bridge from Attempt.failtrace *)

let rec trace_of_failtrace (failtrace : Common.Attempt.failtrace) =
  let children =
    match failtrace.kind with Common.Attempt.Failed cs -> cs | Guard -> []
  in
  {
    region = failtrace.region;
    message = failtrace.message;
    children = List.map trace_of_failtrace children;
  }

let traces_of_failtraces = List.map trace_of_failtrace

let of_failtraces ~source ~fallback (failtraces : Common.Attempt.failtrace list)
    : t =
  let at = Common.Attempt.region_of_failtraces failtraces in
  let message, trace =
    match failtraces with
    | [] -> (fallback, [])
    | [ failtrace ] ->
        let { message; children; _ } = trace_of_failtrace failtrace in
        (message, children)
    | _ -> (fallback, traces_of_failtraces failtraces)
  in
  error ~source ~trace at message

(* Plain text rendering *)

let to_string d =
  let prefix = if d.region = no_region then "" else string_of_region d.region in
  match d.severity with
  | Warning -> prefix ^ "Warning:" ^ d.source ^ ":" ^ d.message
  | Error -> prefix ^ "Error: " ^ d.message
  | Info -> prefix ^ "Info: " ^ d.message
  | Hint -> prefix ^ "Hint: " ^ d.message

(* Collection *)

module Bag = struct
  type diagnostic = t

  (* Reversed list for O(1) add *)
  type t = diagnostic list

  let empty = []
  let singleton d = [ d ]
  let add ds d = d :: ds

  (* [merge older newer] — keeps insertion order; [to_sorted_list] re-sorts. *)
  let merge ds1 ds2 = ds2 @ ds1
  let of_list ds = List.rev ds
  let to_list ds = List.rev ds

  let to_sorted_list ds =
    List.rev ds
    |> List.sort (fun a b ->
           match compare_severity a.severity b.severity with
           | 0 -> compare_region a.region b.region
           | c -> c)

  let is_empty = function [] -> true | _ -> false
  let has_errors ds = List.exists (fun d -> d.severity = Error) ds

  let error_count ds =
    List.length (List.filter (fun d -> d.severity = Error) ds)

  let warning_count ds =
    List.length (List.filter (fun d -> d.severity = Warning) ds)
end

(* Mutable accumulator *)

module Sink = struct
  type diagnostic = t
  type t = { mutable diagnostics : diagnostic list }

  let create () = { diagnostics = [] }
  let emit sink diag = sink.diagnostics <- diag :: sink.diagnostics

  let drain sink =
    let ds = Bag.of_list (List.rev sink.diagnostics) in
    sink.diagnostics <- [];
    ds

  let peek sink = Bag.of_list (List.rev sink.diagnostics)
  let global_ref = ref (create ())
  let global () = !global_ref
  let reset_global () = global_ref := create ()
end

(* Convenience: emit a warning into the global sink. Used by passes that just
   want to record a warning without threading a sink through every call. *)

let warn (at : region) (source : string) (msg : string) =
  Sink.emit (Sink.global ()) (warning ~source at msg)
