open Runtime.Domain
module Types = Runtime.Types
module Type = Types.Type
module TypeDef = Types.TypeDef
module FuncType = Types.FuncType
module FuncDef = Types.FuncDef
module ConsType = Types.ConsType
module ConsDef = Types.ConsDef
module Envs = Runtime.Envs
module F = Format

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
  if not distinct then (
    Format.printf "(check_distinct_names) Names are not distinct\n";
    assert false)
  else ()

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

   Element type |	header    | header_union | struct or tuple | list    | header stack
   bit<W>	      | allowed   |	error        | allowed         | allowed | error
   int<W>       |	allowed   |	error        | allowed         | allowed | error
   varbit<W>    |	allowed   |	error        | allowed         | allowed | error
   int          |	error     |	error        | error           | allowed | error
   void         |	error     |	error        | error           | error   | error
   string       |	error     |	error        | error           | allowed | error
   error        | error     |	error        | allowed         | allowed | error
   match_kind   |	error     |	error        | error           | allowed | error
   bool         | allowed   |	error        | allowed         | allowed | error
   enum         |	allowed^1	| error        | allowed         | allowed | error
   header       | error	    | allowed      | allowed         | allowed | allowed
   header stack |	error     |	error        | allowed         | allowed | error
   header_union |	error     | error	       | allowed         | allowed | allowed
   struct       | allowed^2	| error	       | allowed         | allowed | error
   tuple        |	error     |	error        | allowed         | allowed | error
   list         |	error     |	error        | error           | allowed | error

   ^1 an enum type used as a field in a header must specify a underlying type
    and representation for enum elements
   ^2 a struct or nested struct type that has the same properties,
    used as a field in a header must contain only bit<W>, int<W>, a serializable enum, or a bool

   The table below lists all types that may appear as base types in a typedef or type declaration.

   Base type B    | typedef B <name> |	type B <name>
   bit<W>         | allowed          |	allowed
   int<W>         | allowed          |	allowed
   varbit<W>      | allowed          |	error
   int            | allowed          |  error
   void           | error            |  error
   error          | allowed          |  error
   match_kind     | error            |  error
   bool           | allowed          |  allowed
   enum           | allowed          |  error
   header         | allowed          |  error
   header stack   | allowed          |  error
   header_union   | allowed          |  error
   struct         | allowed          |  error
   tuple          | allowed          |  error
   a typedef name | allowed          |  allowed^3
   a type name	  | allowed          |  allowed *)

(* (7.2.1) Enumeration types

   for each symbolic entry in the enumeration. The symbol typeRef in the grammar above must be one of the following types:

    - an unsigned integer, i.e. bit<W> for some compile-time known W.
    - a signed integer, i.e. int<W> for some compile-time known W.
    - a type name declared via typedef, where the base type of that type is either one of the types listed above,
      or another typedef name that meets these conditions. *)

(* (TODO) check_valid_type and check_valid_typedef quite redundant for
   typedefs that are not generic. maybe consider only check_valid_type
   after evaluating surface types of StackT, TupleT and SpecT *)

let rec check_valid_type (cursor : Ctx.cursor) (ctx : Ctx.t) (typ : Type.t) :
    unit =
  let tset = Ctx.get_tparams cursor ctx |> TIdSet.of_list in
  check_valid_type' tset typ

and check_valid_type' (tset : TIdSet.t) (typ : Type.t) : unit =
  match typ with
  | VoidT | ErrT | MatchKindT | StrT | BoolT | IntT | FIntT _ | FBitT _
  | VBitT _ ->
      ()
  | VarT id ->
      if not (TIdSet.mem id tset) then (
        Format.printf "(check_valid_type) %s is a free type variable\n" id;
        assert false)
      else ()
  | NewT (_id, typ_inner) ->
      check_valid_type' tset typ_inner;
      check_valid_type_nesting typ typ_inner
  | EnumT _ -> ()
  | SEnumT (_, typ_inner) -> check_valid_type_nesting typ typ_inner
  | TupleT typs_inner ->
      List.iter
        (fun typ_inner ->
          check_valid_type' tset typ_inner;
          check_valid_type_nesting typ typ_inner)
        typs_inner
  | StackT (typ_inner, _) ->
      check_valid_type' tset typ_inner;
      check_valid_type_nesting typ typ_inner
  | StructT (_id, fields) ->
      let members, typs_inner = List.split fields in
      check_distinct_names members;
      List.iter
        (fun typ_inner ->
          check_valid_type' tset typ_inner;
          check_valid_type_nesting typ typ_inner)
        typs_inner
  | HeaderT (_id, fields) ->
      let members, typs_inner = List.split fields in
      check_distinct_names members;
      List.iter
        (fun typ_inner ->
          check_valid_type' tset typ_inner;
          check_valid_type_nesting typ typ_inner)
        typs_inner
  | UnionT (_id, fields) ->
      let members, typs_inner = List.split fields in
      check_distinct_names members;
      List.iter
        (fun typ_inner ->
          check_valid_type' tset typ_inner;
          check_valid_type_nesting typ typ_inner)
        typs_inner
  | ExternT (_id, fdenv) ->
      Envs.FDEnv.iter (fun _ fd -> check_valid_funcdef' tset fd) fdenv
  | ParserT params | ControlT params ->
      List.iter (fun fd -> check_valid_param' tset fd) params
  | PackageT | TopT -> ()
  | SeqT typs_inner -> List.iter (check_valid_type' tset) typs_inner
  | RecordT fields ->
      let members, typs_inner = List.split fields in
      check_distinct_names members;
      List.iter (check_valid_type' tset) typs_inner
  | SetT typ_inner ->
      check_valid_type' tset typ_inner;
      check_valid_type_nesting typ typ_inner
  | StateT -> ()
  (* (TODO) Table type must be decl to valid type. But sholud we add check validity of typ_inner? *)
  | TableT _ -> ()

and check_valid_type_nesting (typ : Type.t) (typ_inner : Type.t) : unit =
  if not (check_valid_type_nesting' typ typ_inner) then (
    Format.printf "(check_valid_type_nesting) Invalid nesting of %a inside %a\n"
      Type.pp typ_inner Type.pp typ;
    assert false)
  else ()

and check_valid_type_nesting' (typ : Type.t) (typ_inner : Type.t) : bool =
  let error_not_nest () : bool =
    Format.printf "(check_valid_type_nesting) %a is not a nested type\n" Type.pp
      typ;
    false
  in
  match typ with
  | VoidT | ErrT | MatchKindT | StrT | BoolT | IntT | FIntT _ | FBitT _
  | VBitT _ | VarT _ ->
      error_not_nest ()
  | NewT _ -> (
      match typ_inner with
      | VoidT | ErrT | MatchKindT | StrT -> false
      | BoolT -> true
      | IntT -> false
      | FIntT _ | FBitT _ -> true
      | VBitT _ -> false
      | VarT _ -> true
      | NewT (_id, typ_inner) -> check_valid_type_nesting' typ typ_inner
      | EnumT _ | SEnumT _ | TupleT _ | StackT _ | StructT _ | HeaderT _
      | UnionT _ ->
          false
      | ExternT _ | ParserT _ | ControlT _ | PackageT | TableT _ -> false
      | TopT -> true
      | SeqT _ | RecordT _ | SetT _ | StateT -> false)
  | EnumT _ -> error_not_nest ()
  | SEnumT _ -> (
      match typ_inner with
      | VoidT | ErrT | MatchKindT | StrT | BoolT | IntT -> false
      | FIntT _ | FBitT _ -> true
      | VBitT _ -> false
      | VarT _ -> true
      | NewT (_id, typ_inner) -> check_valid_type_nesting' typ typ_inner
      | EnumT _ | SEnumT _ | TupleT _ | StackT _ | StructT _ | HeaderT _
      | UnionT _ | ExternT _ | ParserT _ | ControlT _ | PackageT | TableT _ ->
          false
      | TopT -> true
      | SeqT _ | RecordT _ | SetT _ | StateT -> false)
  | TupleT _ -> (
      match typ_inner with
      | VoidT -> false
      | ErrT -> true
      | MatchKindT | StrT -> false
      | BoolT -> true
      | IntT -> false
      | FIntT _ | FBitT _ | VBitT _ | VarT _ -> true
      | NewT (_id, typ_inner) -> check_valid_type_nesting' typ typ_inner
      | EnumT _ | SEnumT _ | TupleT _ | StackT _ | StructT _ | HeaderT _
      | UnionT _ ->
          true
      | ExternT _ | ParserT _ | ControlT _ | PackageT | TableT _ -> false
      | TopT -> true
      | SeqT _ | RecordT _ | SetT _ | StateT -> false)
  | StackT _ -> (
      match typ_inner with
      | VoidT | ErrT | MatchKindT | StrT | BoolT | IntT | FIntT _ | FBitT _
      | VBitT _ ->
          false
      | VarT _ -> true
      | NewT (_id, typ_inner) -> check_valid_type_nesting' typ typ_inner
      | EnumT _ | SEnumT _ | TupleT _ | StackT _ | StructT _ -> false
      | HeaderT _ | UnionT _ -> true
      | ExternT _ | ParserT _ | ControlT _ | PackageT | TableT _ -> false
      | TopT -> true
      | SeqT _ | RecordT _ | SetT _ | StateT -> false)
  | StructT _ -> (
      match typ_inner with
      | VoidT -> false
      | ErrT -> true
      | MatchKindT | StrT -> false
      | BoolT -> true
      | IntT -> false
      | FIntT _ | FBitT _ | VBitT _ | VarT _ -> true
      | NewT (_id, typ_inner) -> check_valid_type_nesting' typ typ_inner
      | EnumT _ | SEnumT _ | TupleT _ | StackT _ | StructT _ | HeaderT _
      | UnionT _ ->
          true
      | ExternT _ | ParserT _ | ControlT _ | PackageT | TableT _ -> false
      | TopT -> true
      | SeqT _ | RecordT _ | SetT _ | StateT -> false)
  | HeaderT _ -> (
      match typ_inner with
      | VoidT | ErrT | MatchKindT | StrT -> false
      | BoolT -> true
      | IntT -> false
      | FIntT _ | FBitT _ | VBitT _ | VarT _ -> true
      | NewT (_id, typ_inner) -> check_valid_type_nesting' typ typ_inner
      | EnumT _ -> false
      | SEnumT _ -> true
      | TupleT _ | StackT _ -> false
      (* A special case: when struct is nested inside a header,
         because structs allow more nested types than a header, we need to check recursively *)
      | StructT (_, fields) ->
          let _, typs_inner = List.split fields in
          List.for_all (check_valid_type_nesting' typ) typs_inner
      | HeaderT _ | UnionT _ -> false
      | ExternT _ | ParserT _ | ControlT _ | PackageT | TableT _ -> false
      | TopT -> true
      | SeqT _ | RecordT _ | SetT _ | StateT -> false)
  | UnionT _ -> (
      match typ_inner with
      | VoidT | ErrT | MatchKindT | StrT | BoolT | IntT | FIntT _ | FBitT _
      | VBitT _ ->
          false
      | VarT _ -> true
      | NewT (_id, typ_inner) -> check_valid_type_nesting' typ typ_inner
      | EnumT _ | SEnumT _ | TupleT _ | StackT _ -> false
      | StructT _ -> false
      | HeaderT _ -> true
      | UnionT _ | ExternT _ | ParserT _ | ControlT _ | PackageT | TableT _ ->
          false
      | TopT -> true
      | SeqT _ | RecordT _ | SetT _ | StateT -> false)
  | ExternT _ | ParserT _ | ControlT _ | PackageT | TopT | SeqT _ | RecordT _ ->
      error_not_nest ()
  | SetT _ -> (
      match typ_inner with
      | VoidT | ErrT | MatchKindT | StrT -> false
      | BoolT | IntT | FIntT _ | FBitT _ -> true
      | VBitT _ -> false
      | VarT _ -> false
      | NewT (_id, typ_inner) -> check_valid_type_nesting' typ typ_inner
      | EnumT _ | SEnumT _ -> true
      (* A special case: when tuple is nested inside a set,
         because tuples allow more nested types than a set, we need to check recursively *)
      (* This recursion holds because the inner types that a tuple allows is a
         superset of the inner types that a set allows *)
      | TupleT typs_inner ->
          List.for_all (check_valid_type_nesting' typ) typs_inner
      | StackT _ | StructT _ | HeaderT _ | UnionT _ -> false
      | ExternT _ | ParserT _ | ControlT _ | PackageT | TableT _ -> false
      | TopT -> true
      (* A special case: when sequence is nested inside a set,
         because sequences allow more nested types than a set, we need to check recursively *)
      (* This recursion holds because the inner types that a sequence allows is a
         superset of the inner types that a set allows *)
      | SeqT typs_inner ->
          List.for_all (check_valid_type_nesting' typ) typs_inner
      | RecordT _ | SetT _ | StateT -> false)
  | StateT -> error_not_nest ()
  | TableT _ -> error_not_nest ()

and check_valid_typedef (cursor : Ctx.cursor) (ctx : Ctx.t) (td : TypeDef.t) :
    unit =
  if cursor <> Ctx.Global then (
    Format.printf "(check_valid_typedef) Type definitions must be global\n";
    assert false);
  let tset = Ctx.get_tparams cursor ctx |> TIdSet.of_list in
  check_valid_typedef' tset td

and check_valid_typedef' (tset : TIdSet.t) (td : TypeDef.t) : unit =
  match td with
  | DefD typ_inner ->
      check_valid_type' tset typ_inner;
      check_valid_typedef_nesting td typ_inner
  | NewD (_id, typ_inner) ->
      check_valid_type' tset typ_inner;
      check_valid_typedef_nesting td typ_inner
  | StructD (_id, fields) ->
      let members, typs_inner = List.split fields in
      check_distinct_names members;
      List.iter
        (fun typ_inner ->
          check_valid_type' tset typ_inner;
          check_valid_typedef_nesting td typ_inner)
        typs_inner
  | HeaderD (_id, fields) ->
      let members, typs_inner = List.split fields in
      check_distinct_names members;
      List.iter
        (fun typ_inner ->
          check_valid_type' tset typ_inner;
          check_valid_typedef_nesting td typ_inner)
        typs_inner
  | UnionD (_id, fields) ->
      let members, typs_inner = List.split fields in
      check_distinct_names members;
      List.iter
        (fun typ_inner ->
          check_valid_type' tset typ_inner;
          check_valid_typedef_nesting td typ_inner)
        typs_inner
  | EnumD (_id, members) -> check_distinct_names members
  | SEnumD (_id, typ_inner, fields) ->
      let members, _ = List.split fields in
      check_distinct_names members;
      check_valid_type' tset typ_inner;
      check_valid_typedef_nesting td typ_inner
  | ExternD (_id, tparams, fdenv) ->
      let tset = TIdSet.union tset (TIdSet.of_list tparams) in
      Envs.FDEnv.iter (fun _ fd -> check_valid_funcdef' tset fd) fdenv
  | ParserD (tparams, params) | ControlD (tparams, params) ->
      let tset = TIdSet.union tset (TIdSet.of_list tparams) in
      List.iter (fun fd -> check_valid_param' tset fd) params
  | PackageD _tparams -> ()

and check_valid_typedef_nesting (td : TypeDef.t) (typ_inner : Type.t) : unit =
  if not (check_valid_typedef_nesting' td typ_inner) then (
    Format.printf
      "(check_valid_typedef_nesting) Invalid nesting of %a inside %a\n" Type.pp
      typ_inner TypeDef.pp td;
    assert false)
  else ()

and check_valid_typedef_nesting' (td : TypeDef.t) (typ_inner : Type.t) : bool =
  let error_not_nest () : bool =
    Format.printf
      "(check_valid_typedef_nesting) %a is not a nested type definition\n"
      TypeDef.pp td;
    false
  in
  match td with
  | DefD _ -> (
      match typ_inner with
      | VoidT -> false
      | ErrT -> true
      | MatchKindT -> false
      | StrT | BoolT | IntT | FIntT _ | FBitT _ | VBitT _ | VarT _ -> true
      | NewT (_id, typ_inner) -> check_valid_typedef_nesting' td typ_inner
      | EnumT _ | SEnumT _ | TupleT _ | StackT _ | StructT _ | HeaderT _
      | UnionT _ ->
          true
      | ExternT _ | ParserT _ | ControlT _ | PackageT | TableT _ -> false
      | TopT -> true
      | SeqT _ | RecordT _ | SetT _ | StateT -> false)
  | NewD _ -> (
      match typ_inner with
      | VoidT | ErrT | MatchKindT | StrT -> false
      | BoolT -> true
      | IntT -> false
      | FIntT _ | FBitT _ -> true
      | VBitT _ -> false
      | VarT _ -> true
      | NewT (_id, typ_inner) -> check_valid_typedef_nesting' td typ_inner
      | EnumT _ | SEnumT _ | TupleT _ | StackT _ | StructT _ | HeaderT _
      | UnionT _ ->
          false
      | ExternT _ | ParserT _ | ControlT _ | PackageT | TableT _ -> false
      | TopT -> true
      | SeqT _ | RecordT _ | SetT _ | StateT -> false)
  | EnumD _ -> error_not_nest ()
  | SEnumD _ -> (
      match typ_inner with
      | VoidT | ErrT | MatchKindT | StrT | BoolT | IntT -> false
      | FIntT _ | FBitT _ -> true
      | VBitT _ -> false
      | VarT _ -> true
      | NewT (_id, typ_inner) -> check_valid_typedef_nesting' td typ_inner
      | EnumT _ | SEnumT _ | TupleT _ | StackT _ | StructT _ | HeaderT _
      | UnionT _ | ExternT _ | ParserT _ | ControlT _ | PackageT | TableT _ ->
          false
      | TopT -> true
      | SeqT _ | RecordT _ | SetT _ | StateT -> false)
  | StructD _ -> (
      match typ_inner with
      | VoidT -> false
      | ErrT -> true
      | MatchKindT | StrT -> false
      | BoolT -> true
      | IntT -> false
      | FIntT _ | FBitT _ | VBitT _ | VarT _ -> true
      | NewT (_id, typ_inner) -> check_valid_typedef_nesting' td typ_inner
      | EnumT _ | SEnumT _ | TupleT _ | StackT _ | StructT _ | HeaderT _
      | UnionT _ ->
          true
      | ExternT _ | ParserT _ | ControlT _ | PackageT | TableT _ -> false
      | TopT -> true
      | SeqT _ | RecordT _ | SetT _ | StateT -> false)
  | HeaderD _ -> (
      match typ_inner with
      | VoidT | ErrT | MatchKindT | StrT -> false
      | BoolT -> true
      | IntT -> false
      | FIntT _ | FBitT _ | VBitT _ | VarT _ -> true
      | NewT (_id, typ_inner) -> check_valid_typedef_nesting' td typ_inner
      | EnumT _ -> false
      | SEnumT _ -> true
      | TupleT _ | StackT _ -> false
      (* A special case: when struct is nested inside a header,
         because structs allow more nested types than a header, we need to check recursively *)
      (* This recursion holds because the inner types that a struct allows is a
         superset of the inner types that a header allows *)
      | StructT (_, fields) ->
          let _, typs_inner = List.split fields in
          List.for_all (check_valid_typedef_nesting' td) typs_inner
      | HeaderT _ | UnionT _ -> false
      | ExternT _ | ParserT _ | ControlT _ | PackageT | TableT _ -> false
      | TopT -> true
      | SeqT _ | RecordT _ | SetT _ | StateT -> false)
  | UnionD _ -> (
      match typ_inner with
      | VoidT | ErrT | MatchKindT | StrT | BoolT | IntT | FIntT _ | FBitT _
      | VBitT _ ->
          false
      | VarT _ -> true
      | NewT (_id, typ_inner) -> check_valid_typedef_nesting' td typ_inner
      | EnumT _ | SEnumT _ -> false
      | TupleT _ | StackT _ -> false
      | StructT _ -> false
      | HeaderT _ -> true
      | UnionT _ | ExternT _ | ParserT _ | ControlT _ | PackageT | TableT _ ->
          false
      | TopT -> true
      | SeqT _ | RecordT _ | SetT _ | StateT -> false)
  | ExternD _ | ParserD _ | ControlD _ | PackageD _ -> error_not_nest ()

(* (TODO) Appendix F. Restrictions on compile time and runtime calls *)

and check_valid_param (cursor : Ctx.cursor) (ctx : Ctx.t) (param : Types.param)
    : unit =
  let tset = Ctx.get_tparams cursor ctx |> TIdSet.of_list in
  check_valid_param' tset param

and check_valid_param' (tset : TIdSet.t) (param : Types.param) : unit =
  let _, _, typ, _ = param in
  check_valid_type' tset typ

and check_valid_functype (cursor : Ctx.cursor) (ctx : Ctx.t) (ft : FuncType.t) :
    unit =
  let tset = Ctx.get_tparams cursor ctx |> TIdSet.of_list in
  check_valid_functype' tset ft

and check_valid_functype' (tset : TIdSet.t) (ft : FuncType.t) : unit =
  match ft with
  | ActionT params -> List.iter (check_valid_param' tset) params
  | ExternFunctionT (params, typ_ret) | FunctionT (params, typ_ret) ->
      List.iter (check_valid_param' tset) params;
      check_valid_type' tset typ_ret
  | ExternMethodT (params, typ_ret) | ExternAbstractMethodT (params, typ_ret) ->
      List.iter (check_valid_param' tset) params;
      check_valid_type' tset typ_ret
  | ParserApplyMethodT params | ControlApplyMethodT params ->
      List.iter (check_valid_param' tset) params
  | BuiltinMethodT (params, typ_ret) ->
      List.iter (check_valid_param' tset) params;
      check_valid_type' tset typ_ret
  | TableApplyMethodT typ_ret -> check_valid_type' tset typ_ret

and check_valid_funcdef (cursor : Ctx.cursor) (ctx : Ctx.t) (fd : FuncDef.t) :
    unit =
  if cursor = Ctx.Local then (
    Format.printf
      "(check_valid_funcdef) Function definitions must not be local\n";
    assert false);
  let tset = Ctx.get_tparams cursor ctx |> TIdSet.of_list in
  check_valid_funcdef' tset fd

and check_valid_funcdef' (tset : TIdSet.t) (fd : FuncDef.t) : unit =
  match fd with
  | ActionD params -> List.iter (check_valid_param' tset) params
  | ExternFunctionD (tparams, params, typ_ret) ->
      let tset = TIdSet.union tset (TIdSet.of_list tparams) in
      check_valid_functype' tset (ExternFunctionT (params, typ_ret))
  | FunctionD (tparams, params, typ_ret) ->
      let tset = TIdSet.union tset (TIdSet.of_list tparams) in
      check_valid_functype' tset (FunctionT (params, typ_ret))
  | ExternMethodD (tparams, params, typ_ret) ->
      let tset = TIdSet.union tset (TIdSet.of_list tparams) in
      check_valid_functype' tset (ExternMethodT (params, typ_ret))
  | ExternAbstractMethodD (tparams, params, typ_ret) ->
      let tset = TIdSet.union tset (TIdSet.of_list tparams) in
      check_valid_functype' tset (ExternAbstractMethodT (params, typ_ret))

and check_valid_cparam (cursor : Ctx.cursor) (ctx : Ctx.t)
    (cparam : Types.cparam) : unit =
  let tset = Ctx.get_tparams cursor ctx |> TIdSet.of_list in
  check_valid_cparam' tset cparam

and check_valid_cparam' (tset : TIdSet.t) (cparam : Types.cparam) : unit =
  let _, dir, typ, _ = cparam in
  if not (match (dir : Il.Ast.dir') with No -> true | _ -> false) then (
    Format.printf
      "(check_valid_cparam') Constructor parameters must be directionless\n";
    assert false);
  check_valid_type' tset typ

and check_valid_consdef (cursor : Ctx.cursor) (ctx : Ctx.t) (cd : ConsDef.t) :
    unit =
  if cursor <> Ctx.Block then (
    Format.printf
      "(check_valid_consdef) Constructor definitions must be in a block\n";
    assert false);
  let tset = Ctx.get_tparams cursor ctx |> TIdSet.of_list in
  check_valid_consdef' tset cd

and check_valid_consdef' (tset : TIdSet.t) (cd : ConsDef.t) : unit =
  let tparams, cparams, typ = cd in
  let tset = TIdSet.union tset (TIdSet.of_list tparams) in
  List.iter (check_valid_cparam' tset) cparams;
  check_valid_type' tset typ
