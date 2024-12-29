open Il.Ast

type t =
  | ActionF of param list * block
  | ExternF of param list
  | FuncF of tparam list * param list * block
  | ExternMethodF of tparam list * param list
  | ParserApplyMethodF of param list * block
  | ControlApplyMethodF of param list * block
  | BuiltinMethodF of param list
  | TableApplyMethodF

let pp _fmt = failwith "TODO"
let eq_kind _ _ = failwith "TODO"
