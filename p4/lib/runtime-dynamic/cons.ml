open Il.Ast

type t =
  | ExternC of tparam list * cparam list * mthd list
  | ParserC of tparam list * param list * cparam list * decl list * parser_state list
  | ControlC of tparam list * param list * cparam list * decl list * block
  | PackageC of tparam list * cparam list
  | TableC

let pp _fmt = failwith "TODO"
let eq_kind _ _ = failwith "TODO"
