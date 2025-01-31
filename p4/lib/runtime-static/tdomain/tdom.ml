module F = Format
open Domain.Dom
module L = Lang.Ast
module Value = Vdomain.Value

(* Elaborated parameters *)

type tparam = L.tparam'

type param = L.id' * L.dir' * typ * Value.t option
and cparam = param

(* Types *)
and typ =
  (* 1. Base types *)
  | VoidT
  | ErrT
  | MatchKindT
  | StrT
  | BoolT
  | IntT
  | FIntT of Bigint.t
  | FBitT of Bigint.t
  | VBitT of Bigint.t
  (* 2. Derived types *)
  (* 2a. Derived type constructor *)
  | VarT of L.id'
  | SpecT of typdef_poly * typ list
  (* 2b. Derived value types *)
  | DefT of typ
  | NewT of L.id' * typ
  | EnumT of L.id' * L.member' list
  | SEnumT of L.id' * typ * (L.member' * Value.t) list
  | ListT of typ
  | TupleT of typ list
  | StackT of typ * Bigint.t
  | StructT of L.id' * (L.member' * typ) list
  | HeaderT of L.id' * (L.member' * typ) list
  | UnionT of L.id' * (L.member' * typ) list
  (* 2c. Derived object types *)
  | ExternT of L.id' * funcdef FIdMap.t
  | ParserT of param list
  | ControlT of param list
  | PackageT of typ list
  | TableT of L.id' * typ
  (* 3. Synthesized types *)
  | AnyT
  | TableEnumT of L.id' * L.member' list
  | TableStructT of L.id' * (L.member' * typ) list
  | SeqT of typ list
  | SeqDefaultT of typ list
  | RecordT of (L.member' * typ) list
  | RecordDefaultT of (L.member' * typ) list
  | DefaultT
  | InvalidT
  | SetT of typ
  | StateT

(* Type definitions *)
and typdef = MonoD of typdef_mono | PolyD of typdef_poly
and typdef_mono = typ
and typdef_poly = tparam list * tparam list * typ

(* Function types *)
and functyp =
  | ActionT of param list
  | ExternFunctionT of param list * typ
  | FunctionT of param list * typ
  | ExternMethodT of param list * typ
  | ExternAbstractMethodT of param list * typ
  | ParserApplyMethodT of param list
  | ControlApplyMethodT of param list
  | BuiltinMethodT of param list * typ
  | TableApplyMethodT of typ

(* Function definitions *)
and funcdef =
  | MonoFD of functyp
  | PolyFD of tparam list * tparam list * functyp

(* Constructor types *)

type constyp = param list * typ

(* Constructor definitions *)

type consdef = tparam list * tparam list * constyp
