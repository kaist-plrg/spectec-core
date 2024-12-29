open Domain.Dom
open Il.Ast

type t =
  | ExternO of Func.t FIdMap.t
  | ParserO of param list * decl list * parser_state list
  | ControlO of param list * decl list * block
  | PackageO
  | TableO

let pp _fmt = failwith "TODO"
