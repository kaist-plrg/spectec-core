open Runtime.Domain
open Runtime.Scope

(* (TODO) Hardcoded for basic_routing_explicit-bmv2.p4 test case,
   how to actually extract bits from a packet? Where should the
   input packet's bits reside? *)

let extract (benv : benv) =
  let _theader, vheader = find_var "hdr" benv in
  let vheader =
    match vheader with
    | VHeader { entries; _ } ->
        let entries =
          List.map
            (fun (key, value) ->
              if key = "etherType" then
                ( key,
                  VBit
                    { value = Bigint.of_int 0x0800; width = Bigint.of_int 16 }
                )
              else (key, value))
            entries
        in
        VHeader { valid = true; entries }
    | _ -> assert false
  in
  let benv = update_value "hdr" vheader benv in
  benv

(* Entry point for builtin functions *)

let eval_builtin (benv : benv) (mthd : string) =
  match mthd with
  | "extract" -> extract benv
  | _ -> "Unknown builtin method " ^ mthd |> failwith
