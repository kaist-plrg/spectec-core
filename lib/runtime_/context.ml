open Base_env
open Func_env

module Context = struct
  type t = {
    typedef: TDEnv.t;
    func: FEnv.t;
    global: VEnv.t;
    block: VEnv.t;
    frame: VEnv.t list
  }

  let empty typedef = {
    typedef = typedef;
    func = FEnv.empty;
    global = VEnv.empty;
    block = VEnv.empty;
    frame = []
  }

  let find name context =
    List.fold_left
      (fun value frame ->
        match value with
        | Some _ -> value
        | None -> VEnv.find name frame)
      None (context.frame @ [ context.block; context.global ])

  let enter_frame context =
    { context with frame = VEnv.empty :: context.frame }
  let exit_frame context =
    match context.frame with
    | [] -> context
    | _ :: tl -> { context with frame = tl }

  let pp fmt context =
    Format.fprintf fmt "{@[<v 2>global = %a;@ block = %a;@ frame = %a@]}"
      VEnv.pp context.global
      VEnv.pp context.block
      (Format.pp_print_list VEnv.pp) context.frame
end
