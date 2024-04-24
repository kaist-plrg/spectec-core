open Syntax
open Syntax.Ast
open Runtime.Domain
open Utils

let add_known = Scope.add_double

let init_benv _tdenv =
  let genv = Env.empty in
  let lenv = Env.empty in
  let tsto = Heap.empty in
  let vsto = Heap.empty in
  let lenv, tsto, vsto =
    let typ = TBit { width = Bigint.of_int 32 } in
    let value = VBit { value = Bigint.of_int 42; width = Bigint.of_int 32 } in
    add_known "b" typ value lenv tsto vsto
  in
  (genv, lenv, tsto, vsto)

(* Architecture *)

let apply_args (args : string list) =
  List.map
    (fun arg ->
      Argument.Expression
        {
          tags = Info.M "";
          value =
            Expression.Name
              {
                tags = Info.M "";
                name = BareName { tags = Info.M ""; str = arg };
              };
        })
    args

let drive_proto (tdenv : tdenv) (benv : benv) =
  let proto = "main._p" in
  let proto_args = apply_args [ "b" ] in
  Interpreter.eval_method_call tdenv benv proto "apply" proto_args []

let drive (tdenv : tdenv) (_ccenv : ccenv) (ienv : ienv) =
  (* Register the store *)
  Interpreter.register_store ienv;
  (* Build an environment to call control apply *)
  let benv = init_benv tdenv in
  print_endline "Initial environment:";
  Interpreter.print_benv benv;
  (* Obtain control object from store and call apply *)
  print_endline "Calling main._p.apply()";
  let benv = drive_proto tdenv benv in
  print_endline "After the call:";
  Interpreter.print_benv benv;
  ()
