open Domain.Dom
module Dir = Runtime_static.Dir
module Types = Runtime_static.Tdomain.Types
module Type = Types.Type
module TypeDef = Types.TypeDef
module FuncType = Types.FuncType
module FuncDef = Types.FuncDef
module ConsType = Types.ConsType
module ConsDef = Types.ConsDef
module Envs = Runtime_static.Envs
module F = Format
open Util.Error

let check = check_checker
let error_no_info = error_checker_no_info

let check_distinct_names (names : string list) : unit =
  let distinct =
    List.fold_left
      (fun (distinct, names) name ->
        if not distinct then (distinct, names)
        else if List.mem name names then (false, names)
        else (distinct, name :: names))
      (true, []) names
    |> fst
  in
  check distinct "(check_distinct_names) names are not distinct"

let check_distinct_vars (vars : Lang.Ast.var list) : unit =
  let ids_top, ids_current =
    List.partition_map
      (fun (var : Lang.Ast.var) ->
        match var.it with
        | Top id -> Either.Left id.it
        | Current id -> Either.Right id.it)
      vars
  in
  check_distinct_names ids_top;
  check_distinct_names ids_current

(* Well-formedness checks for
   types, typedefs, functypes, funcdefs, constypes, and consdefs *)

(* (7.2.8) Type nesting rules

   The table below lists all types that may appear as members of headers, header unions, structs,
   tuples, and lists. Note that int by itself (i.e. not as part of an int<N> type expression)
   means an arbitrary-precision integer, without a width specified.

   Element type       |	header    | header_union | struct or tuple | list    | header stack
   bit<W>	            | allowed   |	error        | allowed         | allowed | error
   int<W>             |	allowed   |	error        | allowed         | allowed | error
   varbit<W>          |	allowed   |	error        | allowed         | allowed | error
   int                |	error     |	error        | error           | allowed | error
   void               |	error     |	error        | error           | error   | error
   string             |	error     |	error        | error           | allowed | error
   error              | error     |	error        | allowed         | allowed | error
   match_kind         |	error     |	error        | error           | allowed | error
   bool               | allowed   |	error        | allowed         | allowed | error
   enumeration types  |	allowed^1	| error        | allowed         | allowed | error
   header types       | error	    | allowed      | allowed         | allowed | allowed
   header stacks      |	error     |	error        | allowed         | allowed | error
   header unions      |	error     | error	       | allowed         | allowed | allowed
   struct types       | allowed^2	| error	       | allowed         | allowed | error
   tuple types        |	error     |	error        | allowed         | allowed | error
   list types         |	error     |	error        | error           | allowed | error

   ^1 an enum type used as a field in a header must specify a underlying type
    and representation for enum elements
   ^2 a struct or nested struct type that has the same properties,
    used as a field in a header must contain only bit<W>, int<W>, a serializable enum, or a bool

   The table below lists all types that may appear as base types in a typedef or type declaration.

   Base type B          | typedef B <name> |	type B <name>
   bit<W>               | allowed          |	allowed
   int<W>               | allowed          |	allowed
   varbit<W>            | allowed          |	error
   int                  | allowed          |  error
   void                 | error            |  error
   error                | allowed          |  error
   match_kind           | error            |  error
   bool                 | allowed          |  allowed
   enumeration types    | allowed          |  error
   header types         | allowed          |  error
   header stacks        | allowed          |  error
   header unions        | allowed          |  error
   struct types         | allowed          |  error
   tuple types          | allowed          |  error
   list types           | allowed          |  error
   a typedef name       | allowed          |  allowed^3
   a type name	        | allowed          |  allowed *)

(* (7.2.1) Enumeration types

   for each symbolic entry in the enumeration. The symbol typeRef in the grammar above must be one of the following types:

    - an unsigned integer, i.e. bit<W> for some compile-time known W.
    - a signed integer, i.e. int<W> for some compile-time known W.
    - a type name declared via typedef, where the base type of that type is either one of the types listed above,
      or another typedef name that meets these conditions. *)

(* (13.6) Select expressions

   The type of the values in the set must be either bit<>, int<>, tuple, struct, or serializable enum. *)

let rec check_valid_typ ?(tids_fresh = []) (cursor : Ctx.cursor) (ctx : Ctx.t)
    (typ : Type.t) : unit =
  let tset = Ctx.get_tparams cursor ctx @ tids_fresh |> TIdSet.of_list in
  check_valid_typ' tset typ

and check_valid_typ' (tset : TIdSet.t) (typ : Type.t) : unit =
  match typ with
  | VoidT | ErrT | MatchKindT | StrT | BoolT | IntT | FIntT _ | FBitT _
  | VBitT _ ->
      ()
  | VarT id ->
      check (TIdSet.mem id tset)
        (Format.asprintf "(check_valid_typ) %s is a free type variable" id)
  | SpecT (tdp, typs_inner) ->
      TypeDef.specialize_poly tdp typs_inner |> check_valid_typ' tset
  | DefT typ_inner ->
      check_valid_typ' tset typ_inner;
      check_valid_typ_nesting typ typ_inner
  | NewT (_, typ_inner) ->
      check_valid_typ' tset typ_inner;
      check_valid_typ_nesting typ typ_inner
  | EnumT (_, members) -> check_distinct_names members
  | SEnumT (_, typ_inner, fidles) ->
      check_valid_typ_nesting typ typ_inner;
      let members = List.map fst fidles in
      check_distinct_names members
  | ListT typ_inner ->
      check_valid_typ' tset typ_inner;
      check_valid_typ_nesting typ typ_inner
  | TupleT typs_inner -> check_valid_typs' tset typ typs_inner
  | StackT (typ_inner, _) ->
      check_valid_typ' tset typ_inner;
      check_valid_typ_nesting typ typ_inner
  | StructT (_, fields) | HeaderT (_, fields) | UnionT (_, fields) ->
      let members, typs_inner = List.split fields in
      check_distinct_names members;
      check_valid_typs' tset typ typs_inner
  | ExternT (_, fdenv) ->
      Envs.FDEnv.bindings fdenv |> List.map snd |> check_valid_funcdefs' tset
  | ParserT params | ControlT params -> check_valid_params' tset params
  | PackageT typs_inner -> check_valid_typs' tset typ typs_inner
  | TableT (_, typ_inner) ->
      check_valid_typ' tset typ_inner;
      check_valid_typ_nesting typ typ_inner
  | AnyT -> ()
  | TableEnumT (_, members) -> check_distinct_names members
  | TableStructT (_, fields) -> (
      match fields with
      | [
       ("hit", BoolT);
       ("miss", BoolT);
       ("action_run", (TableEnumT _ as typ_inner));
      ] ->
          check_valid_typ' tset typ_inner
      | _ ->
          "(check_valid_typ) table struct must have exactly 3 fields: hit, \
           miss, and action_run" |> error_no_info)
  | SeqT typs_inner | SeqDefaultT typs_inner ->
      check_valid_typs' tset typ typs_inner
  | RecordT fields | RecordDefaultT fields ->
      let members, typs_inner = List.split fields in
      check_distinct_names members;
      check_valid_typs' tset typ typs_inner
  | DefaultT | InvalidT -> ()
  | SetT typ_inner ->
      check_valid_typ' tset typ_inner;
      check_valid_typ_nesting typ typ_inner
  | StateT -> ()

and check_valid_typs' (tset : TIdSet.t) (typ : Type.t)
    (typs_inner : Type.t list) : unit =
  List.iter
    (fun typ_inner ->
      check_valid_typ' tset typ_inner;
      check_valid_typ_nesting typ typ_inner)
    typs_inner

and check_valid_typ_nesting (typ : Type.t) (typ_inner : Type.t) : unit =
  check
    (check_valid_typ_nesting' typ typ_inner)
    (Format.asprintf "(check_valid_typ_nesting) invalid nesting of %a inside %a"
       (Type.pp ~level:0) typ_inner (Type.pp ~level:0) typ)

and check_valid_typ_nesting_new_in_senum' (typ_inner : Type.t) : bool =
  let typ_inner = Type.canon typ_inner in
  match typ_inner with
  | SpecT _ | DefT _ -> assert false
  | VoidT | ErrT | MatchKindT | StrT | BoolT | IntT -> false
  | FIntT _ | FBitT _ -> true
  | VBitT _ -> false
  | VarT _ | NewT _ -> true
  | EnumT _ | SEnumT _ | ListT _ | TupleT _ | StackT _ | StructT _ | HeaderT _
  | UnionT _ | ExternT _ | ParserT _ | ControlT _ | PackageT _ | TableT _ | AnyT
  | TableEnumT _ | TableStructT _ | SeqT _ | SeqDefaultT _ | RecordT _
  | RecordDefaultT _ | DefaultT | InvalidT | SetT _ | StateT ->
      false

and check_valid_typ_nesting_struct_in_header' (typ_inner : Type.t) : bool =
  let typ_inner = Type.canon typ_inner in
  match typ_inner with
  | SpecT _ | DefT _ -> assert false
  | VoidT | ErrT | MatchKindT | StrT -> false
  | BoolT -> true
  | IntT -> false
  | FIntT _ | FBitT _ -> true
  | VBitT _ -> false
  | VarT _ | NewT _ -> true
  | EnumT _ -> false
  | SEnumT _ -> true
  | ListT _ | TupleT _ | StackT _ -> false
  | StructT (_, fields) ->
      let _, typs_inner = List.split fields in
      List.for_all check_valid_typ_nesting_struct_in_header' typs_inner
  | HeaderT _ | UnionT _ | ExternT _ | ParserT _ | ControlT _ | PackageT _
  | TableT _ | AnyT | TableEnumT _ | TableStructT _ | SeqT _ | SeqDefaultT _
  | RecordT _ | RecordDefaultT _ | DefaultT | InvalidT | SetT _ | StateT ->
      false

and check_valid_typ_nesting_tuple_in_set' (typ_inner : Type.t) : bool =
  let typ_inner = Type.canon typ_inner in
  match typ_inner with
  | SpecT _ | DefT _ -> assert false
  | VoidT | ErrT | MatchKindT | StrT -> false
  | BoolT -> true
  | IntT -> false
  | FIntT _ | FBitT _ -> true
  | VBitT _ -> false
  | VarT _ | NewT _ | EnumT _ | SEnumT _ -> true
  | ListT _ -> false
  | TupleT typs_inner ->
      List.for_all check_valid_typ_nesting_tuple_in_set' typs_inner
  | StackT _ | StructT _ | HeaderT _ | UnionT _ | ExternT _ | ParserT _
  | ControlT _ | PackageT _ | TableT _ | AnyT | TableEnumT _ | TableStructT _
  | SeqT _ | SeqDefaultT _ | RecordT _ | RecordDefaultT _ | DefaultT | InvalidT
  | SetT _ | StateT ->
      false

and check_valid_typ_nesting_seq_in_set' (typ_inner : Type.t) : bool =
  let typ_inner = Type.canon typ_inner in
  match typ_inner with
  | SpecT _ | DefT _ -> assert false
  | VoidT | ErrT | MatchKindT | StrT -> false
  | BoolT -> true
  | IntT -> false
  | FIntT _ | FBitT _ -> true
  | VBitT _ -> false
  | VarT _ | NewT _ | EnumT _ | SEnumT _ -> true
  | ListT _ -> false
  | TupleT typs_inner ->
      List.for_all check_valid_typ_nesting_seq_in_set' typs_inner
  | StackT _ | StructT _ | HeaderT _ | UnionT _ | ExternT _ | ParserT _
  | ControlT _ | PackageT _ | TableT _ | AnyT | TableEnumT _ | TableStructT _ ->
      false
  | SeqT typs_inner ->
      List.for_all check_valid_typ_nesting_seq_in_set' typs_inner
  | SeqDefaultT _ | RecordT _ | RecordDefaultT _ | DefaultT | InvalidT | SetT _
  | StateT ->
      false

and check_valid_typ_nesting' (typ : Type.t) (typ_inner : Type.t) : bool =
  let error_not_nest () : bool =
    Format.printf "(check_valid_typ_nesting) %a is not a nested type\n"
      (Type.pp ~level:0) typ;
    false
  in
  let typ_inner = Type.canon typ_inner in
  match typ with
  | VoidT | ErrT | MatchKindT | StrT | BoolT | IntT | FIntT _ | FBitT _
  | VBitT _ | VarT _ | SpecT _ ->
      error_not_nest ()
  | DefT _ -> (
      match typ_inner with
      | SpecT _ | DefT _ -> assert false
      | VoidT -> false
      | ErrT -> true
      | MatchKindT -> false
      | StrT | BoolT | IntT | FIntT _ | FBitT _ | VBitT _ | VarT _ | NewT _ ->
          true
      | EnumT _ | SEnumT _ | ListT _ | TupleT _ | StackT _ | StructT _
      | HeaderT _ | UnionT _ ->
          true
      | ExternT _ | ParserT _ | ControlT _ | PackageT _ | TableT _ | AnyT
      | TableEnumT _ | TableStructT _ | SeqT _ | SeqDefaultT _ | RecordT _
      | RecordDefaultT _ | DefaultT | InvalidT | SetT _ | StateT ->
          false)
  | NewT _ -> (
      match typ_inner with
      | SpecT _ | DefT _ -> assert false
      | VoidT | ErrT | MatchKindT | StrT -> false
      | BoolT -> true
      | IntT -> false
      | FIntT _ | FBitT _ -> true
      | VBitT _ -> false
      | VarT _ | NewT _ -> true
      | EnumT _ | SEnumT _ | ListT _ | TupleT _ | StackT _ | StructT _
      | HeaderT _ | UnionT _ | ExternT _ | ParserT _ | ControlT _ | PackageT _
      | TableT _ | AnyT | TableEnumT _ | TableStructT _ | SeqT _ | SeqDefaultT _
      | RecordT _ | RecordDefaultT _ | DefaultT | InvalidT | SetT _ | StateT ->
          false)
  | EnumT _ -> error_not_nest ()
  | SEnumT _ -> (
      match typ_inner with
      | SpecT _ | DefT _ -> assert false
      | VoidT | ErrT | MatchKindT | StrT | BoolT | IntT -> false
      | FIntT _ | FBitT _ -> true
      | VBitT _ -> false
      | VarT _ -> true
      | NewT (_, typ_inner) -> check_valid_typ_nesting_new_in_senum' typ_inner
      | EnumT _ | SEnumT _ | ListT _ | TupleT _ | StackT _ | StructT _
      | HeaderT _ | UnionT _ | ExternT _ | ParserT _ | ControlT _ | PackageT _
      | TableT _ | AnyT | TableEnumT _ | TableStructT _ | SeqT _ | SeqDefaultT _
      | RecordT _ | RecordDefaultT _ | DefaultT | InvalidT | SetT _ | StateT ->
          false)
  | ListT _ -> (
      match typ_inner with
      | SpecT _ | DefT _ -> assert false
      | VoidT -> false
      | ErrT | MatchKindT | StrT | BoolT | IntT | FIntT _ | FBitT _ | VBitT _
      | VarT _ | NewT _ ->
          true
      | EnumT _ | SEnumT _ | ListT _ | TupleT _ | StackT _ | StructT _
      | HeaderT _ | UnionT _ ->
          true
      | ExternT _ | ParserT _ | ControlT _ | PackageT _ | TableT _ | AnyT
      | TableEnumT _ | TableStructT _ | SeqT _ | SeqDefaultT _ | RecordT _
      | RecordDefaultT _ | DefaultT | InvalidT | SetT _ | StateT ->
          false)
  | TupleT _ -> (
      match typ_inner with
      | SpecT _ | DefT _ -> assert false
      | VoidT -> false
      | ErrT -> true
      | MatchKindT | StrT -> false
      | BoolT -> true
      | IntT -> false
      | FIntT _ | FBitT _ | VBitT _ | VarT _ | NewT _ -> true
      | EnumT _ | SEnumT _ -> true
      | ListT _ -> false
      | TupleT _ | StackT _ | StructT _ | HeaderT _ | UnionT _ -> true
      | ExternT _ | ParserT _ | ControlT _ | PackageT _ | TableT _ | AnyT
      | TableEnumT _ | TableStructT _ | SeqT _ | SeqDefaultT _ | RecordT _
      | RecordDefaultT _ | DefaultT | InvalidT | SetT _ | StateT ->
          false)
  | StackT _ -> (
      match typ_inner with
      | SpecT _ | DefT _ -> assert false
      | VoidT | ErrT | MatchKindT | StrT | BoolT | IntT | FIntT _ | FBitT _
      | VBitT _ ->
          false
      | VarT _ -> true
      | NewT _ | EnumT _ | SEnumT _ | ListT _ | TupleT _ | StackT _ | StructT _
        ->
          false
      | HeaderT _ | UnionT _ -> true
      | ExternT _ | ParserT _ | ControlT _ | PackageT _ | TableT _ | AnyT
      | TableEnumT _ | TableStructT _ | SeqT _ | SeqDefaultT _ | RecordT _
      | RecordDefaultT _ | DefaultT | InvalidT | SetT _ | StateT ->
          false)
  | StructT _ -> (
      match typ_inner with
      | SpecT _ | DefT _ -> assert false
      | VoidT -> false
      | ErrT -> true
      | MatchKindT | StrT -> false
      | BoolT -> true
      | IntT -> false
      | FIntT _ | FBitT _ | VBitT _ | VarT _ | NewT _ -> true
      | EnumT _ | SEnumT _ -> true
      | ListT _ -> false
      | TupleT _ | StackT _ | StructT _ | HeaderT _ | UnionT _ -> true
      | ExternT _ | ParserT _ | ControlT _ | PackageT _ | TableT _ | AnyT
      | TableEnumT _ | TableStructT _ | SeqT _ | SeqDefaultT _ | RecordT _
      | RecordDefaultT _ | DefaultT | InvalidT | SetT _ | StateT ->
          false)
  | HeaderT _ -> (
      match typ_inner with
      | SpecT _ | DefT _ -> assert false
      | VoidT | ErrT | MatchKindT | StrT -> false
      | BoolT -> true
      | IntT -> false
      | FIntT _ | FBitT _ | VBitT _ | VarT _ | NewT _ -> true
      | EnumT _ -> false
      | SEnumT _ -> true
      | ListT _ | TupleT _ | StackT _ -> false
      (* A special case: when struct is nested inside a header,
         because structs allow more nested types than a header, we need to check recursively *)
      | StructT _ -> check_valid_typ_nesting_struct_in_header' typ_inner
      | HeaderT _ | UnionT _ -> false
      | ExternT _ | ParserT _ | ControlT _ | PackageT _ | TableT _ | AnyT
      | TableEnumT _ | TableStructT _ | SeqT _ | SeqDefaultT _ | RecordT _
      | RecordDefaultT _ | DefaultT | InvalidT | SetT _ | StateT ->
          false)
  | UnionT _ -> (
      match typ_inner with
      | SpecT _ | DefT _ -> assert false
      | VoidT | ErrT | MatchKindT | StrT | BoolT | IntT | FIntT _ | FBitT _
      | VBitT _ ->
          false
      | VarT _ | NewT _ -> true
      | EnumT _ | SEnumT _ | ListT _ | TupleT _ | StackT _ -> false
      | StructT _ -> false
      | HeaderT _ -> true
      | UnionT _ | ExternT _ | ParserT _ | ControlT _ | PackageT _ | TableT _
      | AnyT | TableEnumT _ | TableStructT _ | SeqT _ | SeqDefaultT _
      | RecordT _ | RecordDefaultT _ | DefaultT | InvalidT | SetT _ | StateT ->
          false)
  | ExternT _ | ParserT _ | ControlT _ -> error_not_nest ()
  | PackageT _ -> true
  | TableT _ -> (
      match typ_inner with
      | SpecT _ | DefT _ -> assert false
      | VoidT | ErrT | MatchKindT | StrT | BoolT | IntT | FIntT _ | FBitT _
      | VBitT _ | VarT _ | NewT _ | EnumT _ | SEnumT _ | ListT _ | TupleT _
      | StackT _ | StructT _ | HeaderT _ | UnionT _ | ExternT _ | ParserT _
      | ControlT _ | PackageT _ | TableT _ | AnyT | TableEnumT _ ->
          false
      | TableStructT _ -> true
      | SeqT _ | SeqDefaultT _ | RecordT _ | RecordDefaultT _ | DefaultT
      | InvalidT | SetT _ | StateT ->
          false)
  | AnyT | TableEnumT _ | TableStructT _ -> error_not_nest ()
  | SeqT _ | SeqDefaultT _ | RecordT _ | RecordDefaultT _ -> true
  | DefaultT | InvalidT -> error_not_nest ()
  | SetT _ -> (
      match typ_inner with
      | SpecT _ | DefT _ -> assert false
      | VoidT -> false
      | ErrT -> true
      | MatchKindT | StrT -> false
      | BoolT -> true
      | IntT -> false
      | FIntT _ | FBitT _ | VBitT _ -> true
      | VarT _ | NewT _ | EnumT _ | SEnumT _ -> true
      | ListT _ -> false
      (* A special case: when tuple is nested inside a set,
         because tuples allow more nested types than a set, we need to check recursively *)
      (* This recursion holds because the inner types that a tuple allows is a
         superset of the inner types that a set allows *)
      | TupleT typs_inner ->
          List.for_all check_valid_typ_nesting_tuple_in_set' typs_inner
      | StackT _ | StructT _ | HeaderT _ | UnionT _ | ExternT _ | ParserT _
      | ControlT _ | PackageT _ | TableT _ | AnyT | TableEnumT _
      | TableStructT _ ->
          false
      (* A special case: when sequence is nested inside a set,
         because sequences allow more nested types than a set, we need to check recursively *)
      (* This recursion holds because the inner types that a sequence allows is a
         superset of the inner types that a set allows *)
      | SeqT typs_inner ->
          List.for_all check_valid_typ_nesting_seq_in_set' typs_inner
      | SeqDefaultT _ | RecordT _ | RecordDefaultT _ | DefaultT | InvalidT
      | SetT _ | StateT ->
          false)
  | StateT -> error_not_nest ()

and check_valid_typdef (cursor : Ctx.cursor) (ctx : Ctx.t) (td : TypeDef.t) :
    unit =
  let tset = Ctx.get_tparams cursor ctx |> TIdSet.of_list in
  check_valid_typdef'' tset td

and check_valid_typdef'' (tset : TIdSet.t) (td : TypeDef.t) : unit =
  match td with
  | MonoD typ_inner ->
      check
        (match typ_inner with
        | VarT _ | DefT _ | NewT _ | EnumT _ | SEnumT _ | TableT _ -> true
        | _ -> false)
        (Format.asprintf
           "(check_valid_typdef') %a is not a definable monomorphic type"
           (Type.pp ~level:0) typ_inner);
      check_valid_typ' tset typ_inner
  | PolyD (tparams, tparams_hidden, typ_inner) ->
      check
        (match typ_inner with
        | ListT _ | TupleT _ | StackT _ | StructT _ | HeaderT _ | UnionT _
        | ExternT _ | ParserT _ | ControlT _ | PackageT _ ->
            true
        | _ -> false)
        (Format.asprintf
           "(check_valid_typdef') %a is not a definable generic type"
           (Type.pp ~level:0) typ_inner);
      let tparams = tparams @ tparams_hidden in
      check_distinct_names tparams;
      let tset = tparams |> TIdSet.of_list |> TIdSet.union tset in
      check_valid_typ' tset typ_inner

(* (6.8.1) Justification

   Following is a summary of the constraints imposed by the parameter directions:

    - When used as arguments, extern objects can only be passed as directionless parameters.
    - All constructor parameters are evaluated at compilation-time,
      and in consequence they must all be directionless (they cannot be in, out, or inout);
      this applies to package, control, parser, and extern objects.
      Values for these parameters must be specified at compile-time, and must evaluate to compile-time known values.
      See Section 15 for further details.
    - For actions all directionless parameters must be at the end of the parameter list.
      When an action appears in a table's actions list, only the parameters with a direction must be bound.
      See Section 14.1 for further details.
    - Default parameter values are only allowed for ‘in’ or direction-less parameters; these values must evaluate to compile-time constants.
      If parameters with default values do not appear at the end of the list of parameters,
      invocations that use the default values must use named arguments. *)

(* (Appendix F)trictions on compile time and run time calls

   The stateful types of objects in P416 are packages, parsers, controls, externs, tables, and value-sets.
   P416 functions are also considered to be in that group, even if they happen to be pure functions of
   their arguments. All other types are referred to as “value types” here.

   The next table liststrictions on what types can be passed as run-time parameters to other
   callable things that have run-time parameters: parsers, controls, externs
   (including methods and extern functions), actions, and functions.

   can be a run-time parameter to this callable thing
   This type   | parser | control | extern | action | function
   package	   | no	    | no	    | no	   | no	    | no
   parser	     | no	    | no	    | no	   | no	    | no
   control	   | no	    | no	    | no	   | no	    | no
   extern	     | yes	  | yes	    | yes	   | no	    | no
   table	     | no	    | no	    | no	   | no	    | no
   value-set	 | no	    | no	    | no	   | no	    | no
   action	     | no	    | no	    | no	   | no	    | no
   function	   | no	    | no	    | no	   | no	    | no
   value types | yes	  | yes	    | yes	   | yes	  | yes *)

(* (7.1.5) Strings

   Parameters with type string can be only directionless (see Section 6.8). *)

(* (7.1.6.5) Arbitrary-precision integers

   Parameters with type int are not supported for actions.
   Parameters with type int for other callable entities of a program,
   e.g. controls, parsers, or functions, must be directionless,
   indicating that all calls must provide a compile-time known value
   as an argument for such a parameter *)

and check_valid_param (cursor : Ctx.cursor) (ctx : Ctx.t) (param : Types.param)
    : unit =
  let tset = Ctx.get_tparams cursor ctx |> TIdSet.of_list in
  check_valid_param' tset param

and check_valid_param' (tset : TIdSet.t) (param : Types.param) : unit =
  let _, dir, typ, value_default = param in
  check_valid_typ' tset typ;
  check
    (match Type.canon typ with ExternT _ -> dir = No | _ -> true)
    (Format.asprintf
       "(check_valid_param') extern objects can only be passed as \
        directionless parameters but direction %a was given"
       Dir.pp dir);
  match value_default with
  | Some _ when not (match dir with In | No -> true | _ -> false) ->
      Format.asprintf
        "(check_valid_param') default values are only allowed for in or \
         directionless parameters but direction %a was given"
        Dir.pp dir
      |> error_no_info
  | _ -> ()

and check_valid_params' (tset : TIdSet.t) (params : Types.param list) : unit =
  let ids = List.map (fun (id, _, _, _) -> id) params in
  check_distinct_names ids;
  List.iter (check_valid_param' tset) params

and check_valid_functyp (cursor : Ctx.cursor) (ctx : Ctx.t) (ft : FuncType.t) :
    unit =
  let tset = Ctx.get_tparams cursor ctx |> TIdSet.of_list in
  check_valid_functyp' tset ft

and check_valid_functyp' (tset : TIdSet.t) (ft : FuncType.t) : unit =
  match ft with
  | ActionT params ->
      check_valid_params' tset params;
      let check_trailing_action params =
        let rec check_trailing_action' allow_directionless = function
          | [] -> ()
          | (_, dir, _, _) :: params ->
              if dir = Lang.Ast.No && allow_directionless then
                check_trailing_action' allow_directionless params
              else if dir = Lang.Ast.No then
                Format.asprintf
                  "(check_valid_functyp') all directionless action parameters \
                   must be at the end of the parameter list"
                |> error_no_info
              else check_trailing_action' false params
        in
        check_trailing_action' true (List.rev params)
      in
      check_trailing_action params;
      check_valid_functyp_nesting ft params
  | ExternFunctionT (params, typ_ret)
  | FunctionT (params, typ_ret)
  | ExternMethodT (params, typ_ret)
  | ExternAbstractMethodT (params, typ_ret) ->
      check_valid_params' tset params;
      check_valid_typ' tset typ_ret;
      check_valid_functyp_nesting ft params
  | ParserApplyMethodT params | ControlApplyMethodT params ->
      check_valid_params' tset params;
      check_valid_functyp_nesting ft params
  | BuiltinMethodT (params, typ_ret) ->
      check_valid_params' tset params;
      check_valid_typ' tset typ_ret;
      check_valid_functyp_nesting ft params
  | TableApplyMethodT typ_ret ->
      check_valid_typ' tset typ_ret;
      check_valid_functyp_nesting ft []

and check_valid_functyp_nesting (ft : FuncType.t) (params : Types.param list) :
    unit =
  List.iter
    (fun (_, dir, typ, _) ->
      check
        (check_valid_functyp_nesting' ft dir typ)
        (Format.asprintf
           "(check_valid_functyp_nesting) invalid nesting of %a inside %a"
           (Type.pp ~level:0) typ (FuncType.pp ~level:0) ft))
    params

and check_valid_functyp_nesting' (ft : FuncType.t) (dir : Lang.Ast.dir')
    (typ_inner : Type.t) : bool =
  let typ_inner = Type.canon typ_inner in
  match ft with
  | ActionT _ -> (
      match (dir, typ_inner) with
      | _, (DefT _ | SpecT _) -> assert false
      | _, IntT
      | (In | Out | InOut), StrT
      | _, (ExternT _ | ParserT _ | ControlT _ | PackageT _ | TableT _) ->
          false
      | _ -> true)
  | ExternFunctionT _ -> (
      match (dir, typ_inner) with
      | _, (DefT _ | SpecT _) -> assert false
      | (In | Out | InOut), (StrT | IntT)
      | _, (ParserT _ | ControlT _ | PackageT _ | TableT _) ->
          false
      | _ -> true)
  | FunctionT _ -> (
      match (dir, typ_inner) with
      | _, (DefT _ | SpecT _) -> assert false
      | (In | Out | InOut), (StrT | IntT)
      | _, (ExternT _ | ParserT _ | ControlT _ | PackageT _ | TableT _) ->
          false
      | _ -> true)
  | ExternMethodT _ | ExternAbstractMethodT _ -> (
      match (dir, typ_inner) with
      | _, (DefT _ | SpecT _) -> assert false
      | (In | Out | InOut), (StrT | IntT)
      | _, (ParserT _ | ControlT _ | PackageT _ | TableT _) ->
          false
      | _ -> true)
  | ParserApplyMethodT _ | ControlApplyMethodT _ -> (
      match (dir, typ_inner) with
      | _, (DefT _ | SpecT _) -> assert false
      | (In | Out | InOut), (StrT | IntT)
      | _, (ParserT _ | ControlT _ | PackageT _ | TableT _) ->
          false
      | _ -> true)
  | _ -> true

and check_valid_funcdef (cursor : Ctx.cursor) (ctx : Ctx.t) (fd : FuncDef.t) :
    unit =
  let tset = Ctx.get_tparams cursor ctx |> TIdSet.of_list in
  check_valid_funcdef' tset fd

and check_valid_funcdef' (tset : TIdSet.t) (fd : FuncDef.t) : unit =
  match fd with
  | MonoFD ft ->
      check
        (match ft with
        | ActionT _ | ParserApplyMethodT _ | ControlApplyMethodT _
        | BuiltinMethodT _ | TableApplyMethodT _ ->
            true
        | _ -> false)
        (Format.asprintf
           "(check_valid_funcdef) %a is not a definable monomorphic function"
           (FuncType.pp ~level:0) ft);
      check_valid_functyp' tset ft
  | PolyFD (tparams, tparams_hidden, ft) ->
      check
        (match ft with
        | ExternFunctionT _ | FunctionT _ | ExternMethodT _
        | ExternAbstractMethodT _ ->
            true
        | _ -> false)
        (Format.asprintf
           "(check_valid_funcdef) %a is not a definable generic function"
           (FuncType.pp ~level:0) ft);
      let tparams = tparams @ tparams_hidden in
      check_distinct_names tparams;
      let tset = tparams |> TIdSet.of_list |> TIdSet.union tset in
      check_valid_functyp' tset ft

and check_valid_funcdefs' (tset : TIdSet.t) (fds : FuncDef.t list) : unit =
  List.iter (check_valid_funcdef' tset) fds

(* (Appendix F)trictions on compile time and run time calls

   The first table liststrictions on what types can be passed as
   constructor parameters to other types.

   can be a constructor parameter for this type
   This type   | package	| parser | control | extern
   package	   | yes	    | no	   | no	     | no
   parser	     | yes	    | yes	   | no	     | no
   control	   | yes	    | no	   | yes	   | no
   extern	     | yes	    | yes	   | yes	   | yes
   function	   | no	      | no	   | no	     | no
   table	     | no	      | no	   | no	     | no
   value-set	 | no	      | no	   | no	     | no
   value types | yes	    | yes    | yes	   | yes *)

and check_valid_cparam (cursor : Ctx.cursor) (ctx : Ctx.t)
    (cparam : Types.cparam) : unit =
  let tset = Ctx.get_tparams cursor ctx |> TIdSet.of_list in
  check_valid_cparam' tset cparam

and check_valid_cparam' (tset : TIdSet.t) (cparam : Types.cparam) : unit =
  let _, dir, typ, _ = cparam in
  check
    (match (dir : Il.Ast.dir') with No -> true | _ -> false)
    (Format.asprintf
       "(check_valid_cparam') constructor parameters must be directionless");
  check_valid_typ' tset typ

and check_valid_cparams' (tset : TIdSet.t) (cparams : Types.cparam list) : unit
    =
  let ids = List.map (fun (id, _, _, _) -> id) cparams in
  check_distinct_names ids;
  List.iter (check_valid_cparam' tset) cparams

and check_valid_constyp (cursor : Ctx.cursor) (ctx : Ctx.t) (ct : ConsType.t) :
    unit =
  let tset = Ctx.get_tparams cursor ctx |> TIdSet.of_list in
  check_valid_constyp' tset ct

and check_valid_constyp' (tset : TIdSet.t) (ct : ConsType.t) : unit =
  let cparams, typ = ct in
  check_valid_cparams' tset cparams;
  check_valid_typ' tset typ;
  check_valid_constyp_nesting typ cparams

and check_valid_constyp_nesting (typ : Type.t) (cparams : Types.cparam list) :
    unit =
  List.iter
    (fun (_, _, typ_inner, _) ->
      check
        (check_valid_constyp_nesting' typ typ_inner)
        (Format.asprintf
           "(check_valid_constyp_nesting) invalid nesting of %a inside %a"
           (Type.pp ~level:0) typ_inner (Type.pp ~level:0) typ))
    cparams

and check_valid_constyp_nesting' (typ : Type.t) (typ_inner : Type.t) : bool =
  let typ = Type.canon typ in
  let typ_inner = Type.canon typ_inner in
  match typ with
  | ExternT _ -> (
      match typ_inner with
      | DefT _ | SpecT _ -> assert false
      | ParserT _ | ControlT _ | PackageT _ | TableT _ -> false
      | _ -> true)
  | ParserT _ -> (
      match typ_inner with
      | DefT _ | SpecT _ -> assert false
      | ControlT _ | PackageT _ | TableT _ -> false
      | _ -> true)
  | ControlT _ -> (
      match typ_inner with
      | DefT _ | SpecT _ -> assert false
      | ParserT _ | PackageT _ | TableT _ -> false
      | _ -> true)
  | PackageT _ -> (
      match typ_inner with
      | DefT _ | SpecT _ -> assert false
      | TableT _ -> false
      | _ -> true)
  | _ -> true

and check_valid_consdef (cursor : Ctx.cursor) (ctx : Ctx.t) (cd : ConsDef.t) :
    unit =
  let tset = Ctx.get_tparams cursor ctx |> TIdSet.of_list in
  check_valid_consdef' tset cd

and check_valid_consdef' (tset : TIdSet.t) (cd : ConsDef.t) : unit =
  let tparams, tparams_hidden, ct = cd in
  let cparams, typ = ct in
  check
    (match Type.canon typ with
    | ExternT _ | ParserT _ | ControlT _ | PackageT _ -> true
    | _ -> false)
    (Format.asprintf
       "(check_valid_consdef') %a is not a definable constructor type"
       (Type.pp ~level:0) typ);
  let tparams = tparams @ tparams_hidden in
  check_distinct_names tparams;
  let tset = tparams |> TIdSet.of_list |> TIdSet.union tset in
  let ct = (cparams, typ) in
  check_valid_constyp' tset ct
