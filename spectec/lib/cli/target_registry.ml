module String_map = Map.Make (String)

type entry = { name : string; command : Core.Command.t }

exception Registration_error of string

type active_registration = { expected_name : string; entry : entry option ref }

let registered = ref String_map.empty
let active_registration = ref None

let registration_error format =
  Printf.ksprintf (fun message -> raise (Registration_error message)) format

let register (module Target_cli : Target_cli.S) =
  let name = Target_cli.Target.name in
  let entry = { name; command = Target_cli.command } in
  match !active_registration with
  | None ->
      registration_error "target %S registered outside plugin loading" name
  | Some active ->
      if not (String.equal name active.expected_name) then
        registration_error "plugin %S registered target %S" active.expected_name
          name;
      if Option.is_some !(active.entry) then
        registration_error "plugin %S registered multiple targets"
          active.expected_name;
      if String_map.mem name !registered then
        registration_error "target %S is already registered" name;
      registered := String_map.add name entry !registered;
      active.entry := Some entry

let with_plugin_registration ~expected_name load =
  if Option.is_some !active_registration then
    registration_error "cannot load plugin %S during another plugin load"
      expected_name;
  let active = { expected_name; entry = ref None } in
  active_registration := Some active;
  Fun.protect
    ~finally:(fun () -> active_registration := None)
    (fun () ->
      try
        load ();
        match !(active.entry) with
        | Some entry -> entry
        | None ->
            registration_error "plugin %S did not register a target"
              expected_name
      with exn ->
        Option.iter
          (fun { name; _ } -> registered := String_map.remove name !registered)
          !(active.entry);
        raise exn)

let () =
  Printexc.register_printer (function
    | Registration_error message -> Some message
    | _ -> None)
