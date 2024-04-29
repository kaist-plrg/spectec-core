open Syntax
open Syntax.Ast
open Domain
open Domain.Scope
open Domain.Ccenv
open Domain.Ienv

type bscope = Env.t * Env.t * Sto.t

let init_bscope _tdenv =
  let genv = Env.empty in
  let lenv = Env.empty in
  let sto = Sto.empty in
  let lenv, sto =
    let typ = Type.TBit { width = Bigint.of_int 32 } in
    let value = Value.VBit { value = Bigint.of_int 42; width = Bigint.of_int 32 } in
    add_var "b" typ value lenv sto 
  in
  (genv, lenv, sto)

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

let drive_proto (tdenv : TDEnv.t) (bscope : bscope) =
  let proto = "main._p" in
  let proto_args = apply_args [ "b" ] in
  Interpreter.eval_method_call tdenv bscope proto "apply" proto_args []

let drive (tdenv : TDEnv.t) (_ccenv : CcEnv.t) (ienv : IEnv.t) =
  (* Register the instantiated environment *)
  Interpreter.register_ienv ienv;
  (* Build an environment to call control apply *)
  let bscope = init_bscope tdenv in
  print_endline "Initial environment:";
  Interpreter.print_bscope bscope;
  (* Obtain control object from store and call apply *)
  print_endline "Calling main._p.apply()";
  let bscope = drive_proto tdenv bscope in
  print_endline "After the call:";
  Interpreter.print_bscope bscope;
  ()
