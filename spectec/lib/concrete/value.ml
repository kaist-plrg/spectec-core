(* Ticker for node identifier tracking *)

let tick = ref 0
let refresh () = tick := 0

let fresh () =
  let id = !tick in
  tick := id + 1;
  id

(* Extract text from TextV values *)
let get_text (value : Il.Ast.value) =
  match value.it with Il.Ast.TextV s -> s | _ -> failwith "get_text"
