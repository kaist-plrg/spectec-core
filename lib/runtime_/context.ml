open Base
open Func

module Context = struct
  type t = {
    typedef : TDEnv.t;
    func : FEnv.t;
    global : VTEnv.t;
    block : VTEnv.t;
    frame : VTEnv.t list;
  }

  let empty typedef =
    {
      typedef;
      func = FEnv.empty;
      global = VTEnv.empty;
      block = VTEnv.empty;
      frame = [];
    }

  let find name context =
    List.fold_left
      (fun value frame ->
        match value with Some _ -> value | None -> VTEnv.find name frame)
      None
      (context.frame @ [ context.block; context.global ])

  let enter_frame context =
    { context with frame = VTEnv.empty :: context.frame }

  let exit_frame context =
    match context.frame with
    | [] -> context
    | _ :: tl -> { context with frame = tl }

  let pp fmt context =
    Format.fprintf fmt "{@[<v 2>global = %a;@ block = %a;@ frame = %a@]}"
      VTEnv.pp context.global VTEnv.pp context.block
      (Format.pp_print_list VTEnv.pp)
      context.frame
end
