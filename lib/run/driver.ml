open Runtime

let drive_simple (store : Store.t) =
  let controlobj = Store.find [ "main"; "_n" ] store in
  Interpreter.eval_object_apply store controlobj []
