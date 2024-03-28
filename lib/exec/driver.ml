open Syntax
open Ast
open Runtime
open Value
open Env

(* (TODO) how does a target arch invoke the blocks? *)
(* (TODO) implement copy-in/out calling convention *)

(* This drives simple.p4 after instantiation,
   by implicitly calling the control block, _n *)

let drive_simple (store : Store.t) =
  let controlobj = Store.find [ "main"; "_p" ] store in
  Interpreter.register_store store;
  let tags = Info.M "" in
  let env = Env.empty in
  let env =
    Env.insert "b"
      (Value.Bit { value = Bigint.of_int 42; width = Bigint.of_int 32 })
      env
  in
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
  Interpreter.eval_object_apply env controlobj args
