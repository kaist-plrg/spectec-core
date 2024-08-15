open Domain
module Lang = Common.Ast

(* Elaborated parameters *)

type param = (typ, Value.t, Dir.t) Lang.param'

(* Types *)
and typ =
  (* Base types *)
  | VoidT
  | ErrT
  | MatchKindT
  | StrT
  | BoolT
  | IntT
  | FIntT of Bigint.t
  | FBitT of Bigint.t
  | VBitT of Bigint.t
  (* Parameterized types *)
  (* Invariant: variables should always be bound *)
  | VarT of Lang.id'
  (* Alias types *)
  | DefT of typ
  | NewT of typ
  (* Aggregate types *)
  | TupleT of typ list
  | StackT of typ * Bigint.t
  | StructT of (Lang.member' * typ) list
  | HeaderT of (Lang.member' * typ) list
  | UnionT of (Lang.member' * typ) list
  (* (TODO) maybe just id suffices *)
  | EnumT of Lang.member' list
  | SEnumT of typ * (Lang.member' * Value.t) list
  (* Object types *)
  | ExternT of funcdef FIdMap.t
  | ParserT of param list
  | ControlT of param list
  | PackageT
  (* Top type *)
  | TopT
  (* Synthesized types : variables can never be declared of this type *)
  | RecordT of (Lang.member' * typ) list
  | SetT of typ
  | StateT

(* Type definitions *)
and typdef =
  (* Aliased type definitions *)
  | DefD of typ
  | NewD of typ
  (* Aggregate type definitions *)
  (* These will become generic in the future *)
  | StructD of (Lang.member' * typ) list
  | HeaderD of (Lang.member' * typ) list
  | UnionD of (Lang.member' * typ) list
  | EnumD of Lang.id' * Lang.member' list
  | SEnumD of Lang.id' * typ * (Lang.member' * Value.t) list
  (* Object type definitions *)
  | ExternD of Lang.tparam' list * funcdef FIdMap.t
  | ParserD of Lang.tparam' list * param list
  | ControlD of Lang.tparam' list * param list
  | PackageD of Lang.tparam' list

(* Function types *)
and functyp =
  | ExternFunctionT of param list * typ
  | FunctionT of param list * typ
  | ActionT of param list
  | ExternMethodT of param list * typ
  | ExternAbstractMethodT of param list * typ
  | ParserApplyMethodT of param list
  | ControlApplyMethodT of param list
  | BuiltinMethodT of param list * typ

(* Function definitions *)
and funcdef =
  | ExternFunctionD of Lang.tparam' list * param list * typ
  | FunctionD of Lang.tparam' list * param list * typ
  | ActionD of param list
  | ExternMethodD of Lang.tparam' list * param list * typ
  | ExternAbstractMethodD of Lang.tparam' list * param list * typ

(* Constructor types *)

type constyp = param list * typ

(* Constructor definitions *)

type consdef = Lang.tparam' list * param list * typ

(* Modules *)

module Type = struct
  type t = typ
end

module TypeDef = struct
  type t = typdef
end

module FuncType = struct
  type t = functyp
end

module FuncDef = struct
  type t = funcdef
end

module ConsType = struct
  type t = constyp
end

module ConsDef = struct
  type t = consdef
end
