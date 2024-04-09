open Syntax
open Ast
open Runtime
open Envs
open Store

type store = GSto.t

(* This drives simple.p4 after instantiation,
   by implicitly calling the control block, _n *)

let drive_simple (store : store) =
  let obj_control = GSto.find [ "main"; "_p" ] store in
  Interpreter.register_store store;
  let tags = Info.M "" in
  let cenv = CEnv.empty in
  let lenv = LEnv.empty in
  let tsto = TSto.empty in
  let vsto = VSto.empty in
  let lenv = LEnv.add "b" lenv in
  let vsto =
    VSto.add "b"
      (Value.Bit { value = Bigint.of_int 42; width = Bigint.of_int 32 })
      vsto
  in
  let env = (cenv, lenv, tsto, vsto) in
  let args =
    [
      Argument.Expression
        {
          tags;
          value =
            Expression.Name { tags; name = Name.BareName { tags; str = "b" } };
        };
    ]
  in
  print_endline "Calling apply";
  Interpreter.print_env env;
  let env = Interpreter.eval_method_call env obj_control "apply" args in
  print_endline "Returned from apply";
  Interpreter.print_env env;
  env
