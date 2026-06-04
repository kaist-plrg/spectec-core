module Exn = Instrumentation_common.Exn

type session = Idle | Active of (module Instrumentation_api.Handler.S) list

let session = ref Idle

let finish_all_handlers handlers =
  handlers
  |> List.fold_left
       (fun first_error -> function
         | (module H : Instrumentation_api.Handler.S) ->
             Exn.try_record_first_error first_error H.finish)
       None
  |> Exn.raise_recorded_error

let rec init_handlers ~spec initialized = function
  | [] -> ()
  | ((module H : Instrumentation_api.Handler.S) as handler) :: rest -> (
      try
        H.init ~spec;
        init_handlers ~spec (handler :: initialized) rest
      with exn ->
        let captured = Exn.capture exn in
        (try finish_all_handlers initialized with _ -> ());
        Exn.raise_captured captured)

let init ~spec ~handlers =
  (match !session with
  | Idle -> ()
  | Active _ ->
      failwith "Instrumentation.Dispatcher.init: instrumentation already active");
  init_handlers ~spec [] handlers;
  session := Active handlers

let dispatch hs (ev : Instrumentation_api.Event.t) : unit =
  List.iter (fun (module H : Instrumentation_api.Handler.S) -> H.handle ev) hs

let emit (ev : Instrumentation_api.Event.t) : unit =
  match !session with Active hs -> dispatch hs ev | Idle -> ()

let emit_on_demand (make : unit -> Instrumentation_api.Event.t) : unit =
  match !session with Active hs -> dispatch hs (make ()) | Idle -> ()

let finish () =
  match !session with
  | Idle -> ()
  | Active hs ->
      session := Idle;
      finish_all_handlers hs

let with_extra_handler ~spec handler f =
  match !session with
  | Idle ->
      failwith
        "Instrumentation.Dispatcher.with_extra_handler: no active session"
  | Active hs ->
      let (module H : Instrumentation_api.Handler.S) = handler in
      H.init ~spec;
      session := Active (hs @ [ handler ]);
      let restore () =
        let err = Exn.try_record_first_error None H.finish in
        session := Active hs;
        Exn.raise_recorded_error err
      in
      Exn.with_cleanup ~cleanup:restore f
