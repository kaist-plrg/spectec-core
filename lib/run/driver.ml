open Runtime

(* (TODO) how does a target arch invoke the blocks? *)
(* (TODO) implement copy-in/out calling convention *)

(* This drives simple.p4 after instantiation,
   by implicitly calling the control block, _n *)

let drive_simple (store : Store.t) =
  let controlobj = Store.find [ "main"; "_n" ] store in
  Interpreter.eval_object_apply store controlobj []
