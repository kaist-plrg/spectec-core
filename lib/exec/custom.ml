open Syntax
open Syntax.Ast
open Runtime
open Runtime.Scope
open Runtime.Ccenv
open Runtime.Ienv

let init_bscope (tdenv : TDEnv.t) : bscope =
  let genv = Env.empty in
  let lenv = Env.empty in
  let sto = Sto.empty in
  let lenv, sto =
    let typ = Type.TBit { width = Bigint.of_int 32 } in
    let value =
      Value.VBit { value = Bigint.of_int 42; width = Bigint.of_int 32 }
    in
    add_var "b" typ value lenv sto
  in
  (tdenv, genv, lenv, sto)

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

let drive_proto (bscope : bscope) =
  let proto = "main._p" in
  let proto_args = apply_args [ "b" ] in
  Interpreter.eval_method_call bscope proto "apply" proto_args []

let drive (tdenv : TDEnv.t) (_ccenv : CcEnv.t) (ienv : IEnv.t) =
  (* Register the instantiated environment *)
  Interpreter.register_ienv ienv;
  (* Build an environment to call control apply *)
  let bscope = init_bscope tdenv in
  Format.printf "Initial environment:\n";
  Format.printf "%a" pp bscope;
  (* Obtain control object from store and call apply *)
  Format.printf "Calling main._p.apply()\n";
  let bscope = drive_proto bscope in
  Format.printf "%a" pp bscope;
  ()
