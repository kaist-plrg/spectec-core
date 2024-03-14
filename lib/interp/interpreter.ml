open Syntax
open Ast
open Runtime

let eval_program (_program: program) (store: Store.t) =
  print_endline "(TODO: interpreter)";
  Printf.sprintf
    "Instantiation result:\n%s" (Store.print store)
  |> print_endline;
  let main = Store.find [ "main" ] store in
  print_endline "main object:";
  Object.print_object main |> print_endline
