(** Profiler: collects per-relation/function call counts and inclusive and
    exclusive timing, and prints a report on {!finish}. *)

type config = { output : Instrumentation_api.Output.t }

let default_config = { output = Instrumentation_api.Output.stdout }
let config = ref default_config
let fmt = ref Format.std_formatter

type stats = {
  mutable count : int;
  mutable inclusive_time : float;
  mutable exclusive_time : float;
}

type frame = {
  id : string;
  is_rel : bool;
  start_time : float;
  mutable child_time : float;
  is_recursive : bool;
}

(* Runtime state - changes during execution *)
module State = struct
  let frame_stack : frame Stack.t = Stack.create ()
  let rel_stats : (string, stats) Hashtbl.t = Hashtbl.create 64
  let func_stats : (string, stats) Hashtbl.t = Hashtbl.create 64

  let reset () =
    Stack.clear frame_stack;
    Hashtbl.clear rel_stats;
    Hashtbl.clear func_stats

  let get_or_create_stats tbl id =
    match Hashtbl.find_opt tbl id with
    | Some s -> s
    | None ->
        let s = { count = 0; inclusive_time = 0.0; exclusive_time = 0.0 } in
        Hashtbl.add tbl id s;
        s
end

(* Use monotonic clock to ensure positive durations *)
let now () =
  Core.Time_ns.now () |> Core.Time_ns.to_span_since_epoch
  |> Core.Time_ns.Span.to_sec

module M : Instrumentation_api.Handler.S = struct
  open State

  let static_dependencies = []
  let init ~spec:_ = State.reset ()

  let handle : Instrumentation_api.Event.t -> unit = function
    | Rel_enter { id; at = _; inputs = _ } ->
        let is_recursive =
          frame_stack |> Stack.to_seq
          |> Seq.exists (fun f -> f.is_rel && f.id = id)
        in
        let frame =
          {
            id;
            is_rel = true;
            start_time = now ();
            child_time = 0.0;
            is_recursive;
          }
        in
        Stack.push frame frame_stack
    | Rel_exit { id; at = _; success = _; conclusion = _ } ->
        if not (Stack.is_empty frame_stack) then (
          let frame = Stack.pop frame_stack in
          let elapsed = now () -. frame.start_time in
          let exclusive = elapsed -. frame.child_time in
          let stats = get_or_create_stats rel_stats id in
          stats.count <- stats.count + 1;
          if not frame.is_recursive then
            stats.inclusive_time <- stats.inclusive_time +. elapsed;
          stats.exclusive_time <- stats.exclusive_time +. exclusive;
          if not (Stack.is_empty frame_stack) then
            let parent = Stack.top frame_stack in
            parent.child_time <- parent.child_time +. elapsed)
    | Func_enter { id; at = _; inputs = _ } ->
        let is_recursive =
          frame_stack |> Stack.to_seq
          |> Seq.exists (fun f -> (not f.is_rel) && f.id = id)
        in
        let frame =
          {
            id;
            is_rel = false;
            start_time = now ();
            child_time = 0.0;
            is_recursive;
          }
        in
        Stack.push frame frame_stack
    | Func_exit { id; at = _; output = _ } ->
        if not (Stack.is_empty frame_stack) then (
          let frame = Stack.pop frame_stack in
          let elapsed = now () -. frame.start_time in
          let exclusive = elapsed -. frame.child_time in
          let stats = get_or_create_stats func_stats id in
          stats.count <- stats.count + 1;
          if not frame.is_recursive then
            stats.inclusive_time <- stats.inclusive_time +. elapsed;
          stats.exclusive_time <- stats.exclusive_time +. exclusive;
          if not (Stack.is_empty frame_stack) then
            let parent = Stack.top frame_stack in
            parent.child_time <- parent.child_time +. elapsed)
    | _ -> ()

  let finish () =
    let rel_list =
      Hashtbl.fold (fun id stats acc -> (id, stats) :: acc) rel_stats []
    in
    let func_list =
      Hashtbl.fold (fun id stats acc -> (id, stats) :: acc) func_stats []
    in
    let rel_sorted =
      List.sort
        (fun (_, a) (_, b) -> Float.compare b.inclusive_time a.inclusive_time)
        rel_list
    in
    let func_sorted =
      List.sort
        (fun (_, a) (_, b) -> Float.compare b.inclusive_time a.inclusive_time)
        func_list
    in

    Format.fprintf !fmt "\n=== Profiling Results ===\n\n";

    if rel_sorted <> [] then (
      Format.fprintf !fmt "Relations (sorted by inclusive time):\n";
      Format.fprintf !fmt "  %-40s %8s %12s %12s %12s\n" "Name" "Calls"
        "Inclusive" "Exclusive" "Avg";
      Format.fprintf !fmt "  %s\n" (String.make 90 '-');
      List.iter
        (fun (id, stats) ->
          let avg =
            if stats.count > 0 then
              stats.inclusive_time /. float_of_int stats.count
            else 0.0
          in
          Format.fprintf !fmt "  %-40s %8d %11.4fs %11.4fs %11.6fs\n" id
            stats.count stats.inclusive_time stats.exclusive_time avg)
        rel_sorted;
      Format.fprintf !fmt "\n");

    if func_sorted <> [] then (
      Format.fprintf !fmt "Functions (sorted by inclusive time):\n";
      Format.fprintf !fmt "  %-40s %8s %12s %12s %12s\n" "Name" "Calls"
        "Inclusive" "Exclusive" "Avg";
      Format.fprintf !fmt "  %s\n" (String.make 90 '-');
      List.iter
        (fun (id, stats) ->
          let avg =
            if stats.count > 0 then
              stats.inclusive_time /. float_of_int stats.count
            else 0.0
          in
          Format.fprintf !fmt "  $%-39s %8d %11.4fs %11.4fs %11.6fs\n" id
            stats.count stats.inclusive_time stats.exclusive_time avg)
        func_sorted)
end

let make cfg =
  config := cfg;
  fmt := Instrumentation_api.Output.formatter cfg.output;
  (module M : Instrumentation_api.Handler.S)

module Spec : Instrumentation_spec.Spec.S = struct
  let name = "profile"
  let mode = `Both
  let params = [ Instrumentation_spec.Param_utils.output_param ]

  let parse alist =
    match Instrumentation_spec.Param_utils.get alist "output" with
    | None -> None
    | Some path ->
        let output = Instrumentation_api.Output.file path in
        Some
          {
            Instrumentation_config.Handler_config.name;
            mode;
            handler = make { output };
            output;
          }

  let checkpoint = None
end

let spec : Instrumentation_spec.Spec.t = (module Spec)
