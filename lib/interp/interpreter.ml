open Syntax
open Ast
open Runtime

let eval_program (_program: program) (store: Store.t) =
  print_endline "(TODO: interpreter)";
  Printf.sprintf
    "Instantiation results in this store:\n%s"
    (Store.print store ~indent:1)
  |> print_endline;
  let main = Store.find [ "main" ] store in
  Printf.sprintf
    "main object:\n%s"
    (Object.print main ~indent:1)
  |> print_endline
