open Syntax
open Ast
open Instance

let eval_program (_program: program) (store: Object.store) =
  print_endline "(TODO: interpreter)";
  Printf.sprintf
    "Instantiation result:\n%s" (Object.print_store store)
  |> print_endline;
  let main = Object.find_store [ "main" ] store in
  print_endline "main object:";
  Object.print_object main |> print_endline
