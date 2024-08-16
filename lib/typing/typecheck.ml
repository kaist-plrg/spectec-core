open Runtime.Domain
module Value = Runtime.Value
module Dir = Runtime.Dir
module Types = Runtime.Types
module Type = Types.Type
module TypeDef = Types.TypeDef
module FuncType = Types.FuncType
module FuncDef = Types.FuncDef
module ConsType = Types.ConsType
module ConsDef = Types.ConsDef
module Envs = Runtime.Envs
module F = Format
open Util.Source

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
        Format.eprintf "(check_valid_type) %s is a free type variable\n" id;
        assert false)
      else ()
  | DefT typ_inner ->
      check_valid_type' tset typ_inner;
      check_valid_type_nesting typ typ_inner
  | NewT typ_inner ->
      check_valid_type' tset typ_inner;
      check_valid_type_nesting typ typ_inner
  | TupleT typs_inner ->
      List.iter
        (fun typ_inner ->
          check_valid_type' tset typ_inner;
          check_valid_type_nesting typ typ_inner)
        typs_inner
  | StackT (typ_inner, _) ->
      check_valid_type' tset typ_inner;
      check_valid_type_nesting typ typ_inner
  | StructT fields ->
      let members, typs_inner = List.split fields in
      check_distinct_names members;
      List.iter
        (fun typ_inner ->
          check_valid_type' tset typ_inner;
          check_valid_type_nesting typ typ_inner)
        typs_inner
  | HeaderT fields ->
      let members, typs_inner = List.split fields in
      check_distinct_names members;
      List.iter
        (fun typ_inner ->
          check_valid_type' tset typ_inner;
          check_valid_type_nesting typ typ_inner)
        typs_inner
  | UnionT fields ->
      let members, typs_inner = List.split fields in
      check_distinct_names members;
      List.iter
        (fun typ_inner ->
          check_valid_type' tset typ_inner;
          check_valid_type_nesting typ typ_inner)
        typs_inner
  | EnumT members -> check_distinct_names members
  | SEnumT (typ_inner, fields) ->
      let members, _ = List.split fields in
      check_distinct_names members;
      check_valid_type' tset typ_inner
  | ExternT fdenv ->
      Envs.FDEnv.iter (fun _ fd -> check_valid_funcdef' tset fd) fdenv
  | ParserT params | ControlT params ->
      List.iter (fun fd -> check_valid_param' tset fd) params
  | PackageT | TopT -> ()
  | RecordT fields ->
      let members, typs_inner = List.split fields in
      check_distinct_names members;
      List.iter (check_valid_type' tset) typs_inner
  | SetT typ_inner ->
      check_valid_type' tset typ_inner;
      check_valid_type_nesting typ typ_inner
  | StateT -> ()

and check_distinct_names (names : string list) : unit =
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
    Format.eprintf "(check_distinct_names) Names are not distinct\n";
    assert false)
  else ()

and check_valid_type_nesting (typ : Type.t) (typ_inner : Type.t) : unit =
  if not (check_valid_type_nesting' typ typ_inner) then (
    Format.eprintf
      "(check_valid_type_nesting) Invalid nesting of %a inside %a\n" Type.pp
      typ_inner Type.pp typ;
    assert false)
  else ()

and check_valid_type_nesting' (typ : Type.t) (typ_inner : Type.t) : bool =
  let error_not_nest () : bool =
    Format.eprintf "(check_valid_type_nesting) %a is not a nested type\n"
      Type.pp typ;
    false
  in
  match typ with
  | VoidT | ErrT | MatchKindT | StrT | BoolT | IntT | FIntT _ | FBitT _
  | VBitT _ | VarT _ ->
      error_not_nest ()
  | DefT _ -> (
      match typ_inner with
      | VoidT -> false
      | ErrT -> true
      | MatchKindT -> false
      | StrT | BoolT | IntT | FIntT _ | FBitT _ | VBitT _ | VarT _ -> true
      | DefT typ_inner | NewT typ_inner ->
          check_valid_type_nesting' typ typ_inner
      | TupleT _ | StackT _ | StructT _ | HeaderT _ | UnionT _ | EnumT _
      | SEnumT _ ->
          true
      | ExternT _ | ParserT _ | ControlT _ | PackageT -> false
      | TopT -> true
      | RecordT _ | SetT _ | StateT -> false)
  | NewT _ -> (
      match typ_inner with
      | VoidT | ErrT | MatchKindT | StrT -> false
      | BoolT -> true
      | IntT -> false
      | FIntT _ | FBitT _ -> true
      | VBitT _ -> false
      | VarT _ -> true
      | DefT typ_inner | NewT typ_inner ->
          check_valid_type_nesting' typ typ_inner
      | TupleT _ | StackT _ | StructT _ | HeaderT _ | UnionT _ | EnumT _
      | SEnumT _ ->
          false
      | ExternT _ | ParserT _ | ControlT _ | PackageT -> false
      | TopT -> true
      | RecordT _ | SetT _ | StateT -> false)
  | TupleT _ -> (
      match typ_inner with
      | VoidT -> false
      | ErrT -> true
      | MatchKindT | StrT -> false
      | BoolT -> true
      | IntT -> false
      | FIntT _ | FBitT _ | VBitT _ | VarT _ -> true
      | DefT typ_inner | NewT typ_inner ->
          check_valid_type_nesting' typ typ_inner
      | TupleT _ | StackT _ | StructT _ | HeaderT _ | UnionT _ | EnumT _
      | SEnumT _ ->
          true
      | ExternT _ | ParserT _ | ControlT _ | PackageT -> false
      | TopT -> true
      | RecordT _ | SetT _ | StateT -> false)
  | StackT _ -> (
      match typ_inner with
      | VoidT | ErrT | MatchKindT | StrT | BoolT | IntT | FIntT _ | FBitT _
      | VBitT _ ->
          false
      | VarT _ -> true
      | DefT typ_inner | NewT typ_inner ->
          check_valid_type_nesting' typ typ_inner
      | TupleT _ | StackT _ | StructT _ -> false
      | HeaderT _ | UnionT _ -> true
      | EnumT _ | SEnumT _ | ExternT _ | ParserT _ | ControlT _ | PackageT ->
          false
      | TopT -> true
      | RecordT _ | SetT _ | StateT -> false)
  | StructT _ -> (
      match typ_inner with
      | VoidT -> false
      | ErrT -> true
      | MatchKindT | StrT -> false
      | BoolT -> true
      | IntT -> false
      | FIntT _ | FBitT _ | VBitT _ | VarT _ -> true
      | DefT typ_inner | NewT typ_inner ->
          check_valid_type_nesting' typ typ_inner
      | TupleT _ | StackT _ | StructT _ | HeaderT _ | UnionT _ | EnumT _
      | SEnumT _ ->
          true
      | ExternT _ | ParserT _ | ControlT _ | PackageT -> false
      | TopT -> true
      | RecordT _ | SetT _ | StateT -> false)
  | HeaderT _ -> (
      match typ_inner with
      | VoidT | ErrT | MatchKindT | StrT -> false
      | BoolT -> true
      | IntT -> false
      | FIntT _ | FBitT _ | VBitT _ | VarT _ -> true
      | DefT typ_inner | NewT typ_inner ->
          check_valid_type_nesting' typ typ_inner
      | TupleT _ | StackT _ -> false
      (* A special case: when struct is nested inside a header,
         because structs allow more nested types than a header, we need to check recursively *)
      | StructT fields ->
          let _, typs_inner = List.split fields in
          List.for_all (check_valid_type_nesting' typ) typs_inner
      | HeaderT _ | UnionT _ | EnumT _ -> false
      | SEnumT _ -> true
      | ExternT _ | ParserT _ | ControlT _ | PackageT -> false
      | TopT -> true
      | RecordT _ | SetT _ | StateT -> false)
  | UnionT _ -> (
      match typ_inner with
      | VoidT | ErrT | MatchKindT | StrT | BoolT | IntT | FIntT _ | FBitT _
      | VBitT _ ->
          false
      | VarT _ -> true
      | DefT typ_inner | NewT typ_inner ->
          check_valid_type_nesting' typ typ_inner
      | TupleT _ | StackT _ -> false
      | StructT _ -> false
      | HeaderT _ -> true
      | UnionT _ | EnumT _ | SEnumT _ | ExternT _ | ParserT _ | ControlT _
      | PackageT ->
          false
      | TopT -> true
      | RecordT _ | SetT _ | StateT -> false)
  | EnumT _ -> error_not_nest ()
  | SEnumT _ -> (
      match typ_inner with
      | VoidT | ErrT | MatchKindT | StrT | BoolT | IntT -> false
      | FIntT _ | FBitT _ -> true
      | VBitT _ -> false
      | VarT _ -> true
      | DefT typ_inner | NewT typ_inner ->
          check_valid_type_nesting' typ typ_inner
      | TupleT _ | StackT _ | StructT _ | HeaderT _ | UnionT _ | EnumT _
      | SEnumT _ | ExternT _ | ParserT _ | ControlT _ | PackageT ->
          false
      | TopT -> true
      | RecordT _ | SetT _ | StateT -> false)
  | ExternT _ | ParserT _ | ControlT _ | PackageT | TopT | RecordT _ ->
      error_not_nest ()
  | SetT _ -> (
      match typ_inner with
      | VoidT | ErrT | MatchKindT | StrT -> false
      | BoolT -> true
      | IntT -> false
      | FIntT _ | FBitT _ -> true
      | VBitT _ -> false
      | VarT _ -> false
      | DefT typ_inner | NewT typ_inner ->
          check_valid_type_nesting' typ typ_inner
      (* A special case: when tuple is nested inside a set,
         because tuples allow more nested types than a set, we need to check recursively *)
      | TupleT typs_inner ->
          List.for_all (check_valid_type_nesting' typ) typs_inner
      | StackT _ | StructT _ | HeaderT _ | UnionT _ -> false
      | EnumT _ | SEnumT _ -> true
      | ExternT _ | ParserT _ | ControlT _ | PackageT -> false
      | TopT -> true
      | RecordT _ | SetT _ | StateT -> false)
  | StateT -> error_not_nest ()

and check_valid_typedef (cursor : Ctx.cursor) (ctx : Ctx.t) (td : TypeDef.t) :
    unit =
  if cursor <> Ctx.Global then (
    Format.eprintf "(check_valid_typedef) Type definitions must be global\n";
    assert false);
  let tset = Ctx.get_tparams cursor ctx |> TIdSet.of_list in
  match td with
  | DefD typ | NewD typ -> check_valid_type' tset typ
  | StructD fields -> check_valid_type' tset (StructT fields)
  | HeaderD fields -> check_valid_type' tset (HeaderT fields)
  | UnionD fields -> check_valid_type' tset (UnionT fields)
  | EnumD (_id, members) -> check_valid_type' tset (EnumT members)
  | SEnumD (_id, typ, fields) -> check_valid_type' tset (SEnumT (typ, fields))
  | ExternD (tparams, fdenv) ->
      let tset = TIdSet.union tset (TIdSet.of_list tparams) in
      check_valid_type' tset (ExternT fdenv)
  | ParserD (tparams, fdenv) ->
      let tset = TIdSet.union tset (TIdSet.of_list tparams) in
      check_valid_type' tset (ParserT fdenv)
  | ControlD (tparams, fdenv) ->
      let tset = TIdSet.union tset (TIdSet.of_list tparams) in
      check_valid_type' tset (ControlT fdenv)
  | PackageD tparams ->
      let tset = TIdSet.union tset (TIdSet.of_list tparams) in
      check_valid_type' tset PackageT

(* (TODO) Appendix F. Restrictions on compile time and runtime calls *)

and check_valid_param (cursor : Ctx.cursor) (ctx : Ctx.t)
    (param : Il.Ast.param') : unit =
  let tset = Ctx.get_tparams cursor ctx |> TIdSet.of_list in
  check_valid_param' tset param

and check_valid_param' (tset : TIdSet.t) (param : Il.Ast.param') : unit =
  let _, _, typ, _, _ = param in
  check_valid_type' tset typ

and check_valid_functype (cursor : Ctx.cursor) (ctx : Ctx.t) (ft : FuncType.t) :
    unit =
  let tset = Ctx.get_tparams cursor ctx |> TIdSet.of_list in
  check_valid_functype' tset ft

and check_valid_functype' (tset : TIdSet.t) (ft : FuncType.t) : unit =
  match ft with
  | ExternFunctionT (params, typ_ret) | FunctionT (params, typ_ret) ->
      List.iter (check_valid_param' tset) params;
      check_valid_type' tset typ_ret
  | ActionT params -> List.iter (check_valid_param' tset) params
  | ExternMethodT (params, typ_ret) | ExternAbstractMethodT (params, typ_ret) ->
      List.iter (check_valid_param' tset) params;
      check_valid_type' tset typ_ret
  | ParserApplyMethodT params | ControlApplyMethodT params ->
      List.iter (check_valid_param' tset) params
  | BuiltinMethodT (params, typ_ret) ->
      List.iter (check_valid_param' tset) params;
      check_valid_type' tset typ_ret

and check_valid_funcdef (cursor : Ctx.cursor) (ctx : Ctx.t) (fd : FuncDef.t) :
    unit =
  if cursor = Ctx.Local then (
    Format.eprintf
      "(check_valid_funcdef) Function definitions must not be local\n";
    assert false);
  let tset = Ctx.get_tparams cursor ctx |> TIdSet.of_list in
  check_valid_funcdef' tset fd

and check_valid_funcdef' (tset : TIdSet.t) (fd : FuncDef.t) : unit =
  match fd with
  | ExternFunctionD (tparams, params, typ_ret) ->
      let tset = TIdSet.union tset (TIdSet.of_list tparams) in
      check_valid_functype' tset (ExternFunctionT (params, typ_ret))
  | FunctionD (tparams, params, typ_ret) ->
      let tset = TIdSet.union tset (TIdSet.of_list tparams) in
      check_valid_functype' tset (FunctionT (params, typ_ret))
  | ActionD params -> List.iter (check_valid_param' tset) params
  | ExternMethodD (tparams, params, typ_ret) ->
      let tset = TIdSet.union tset (TIdSet.of_list tparams) in
      check_valid_functype' tset (ExternMethodT (params, typ_ret))
  | ExternAbstractMethodD (tparams, params, typ_ret) ->
      let tset = TIdSet.union tset (TIdSet.of_list tparams) in
      check_valid_functype' tset (ExternAbstractMethodT (params, typ_ret))

and check_valid_cparam (cursor : Ctx.cursor) (ctx : Ctx.t)
    (cparam : Il.Ast.param') : unit =
  let tset = Ctx.get_tparams cursor ctx |> TIdSet.of_list in
  check_valid_cparam' tset cparam

and check_valid_cparam' (tset : TIdSet.t) (cparam : Il.Ast.param') : unit =
  let _, dir, typ, _, _ = cparam in
  if not (match (dir : Dir.t) with No _ -> true | _ -> false) then (
    Format.eprintf
      "(check_valid_cparam') Control parameters must be directionless\n";
    assert false);
  check_valid_type' tset typ

and check_valid_consdef (cursor : Ctx.cursor) (ctx : Ctx.t) (cd : ConsDef.t) :
    unit =
  if cursor <> Ctx.Block then (
    Format.eprintf
      "(check_valid_consdef) Constructor definitions must be in a block\n";
    assert false);
  let tset = Ctx.get_tparams cursor ctx |> TIdSet.of_list in
  check_valid_consdef' tset cd

and check_valid_consdef' (tset : TIdSet.t) (cd : ConsDef.t) : unit =
  let tparams, cparams, typ = cd in
  let tset = TIdSet.union tset (TIdSet.of_list tparams) in
  List.iter (check_valid_cparam' tset) cparams;
  check_valid_type' tset typ

(* Type evaluation *)

module TIdMap = MakeTIdEnv (Type)

let rec substitute_type (tidmap : TIdMap.t) (typ : Il.Ast.typ) : Il.Ast.typ =
  match typ with
  | VoidT | ErrT | MatchKindT | StrT | BoolT | IntT | FIntT _ | FBitT _
  | VBitT _ ->
      typ
  | VarT id ->
      let typ = TIdMap.find_opt id tidmap in
      if Option.is_none typ then (
        Format.eprintf "(substitute_type) %s is a free type variable\n" id;
        assert false);
      let typ = Option.get typ in
      typ
  | DefT typ_inner -> DefT (substitute_type tidmap typ_inner)
  | NewT typ_inner -> NewT (substitute_type tidmap typ_inner)
  | TupleT typs_inner -> TupleT (List.map (substitute_type tidmap) typs_inner)
  | StackT (typ_inner, size) -> StackT (substitute_type tidmap typ_inner, size)
  | StructT fields ->
      let members, typs_inner = List.split fields in
      let typs_inner = List.map (substitute_type tidmap) typs_inner in
      StructT (List.combine members typs_inner)
  | HeaderT fields ->
      let members, typs_inner = List.split fields in
      let typs_inner = List.map (substitute_type tidmap) typs_inner in
      HeaderT (List.combine members typs_inner)
  | UnionT fields ->
      let members, typs_inner = List.split fields in
      let typs_inner = List.map (substitute_type tidmap) typs_inner in
      UnionT (List.combine members typs_inner)
  | EnumT _ -> typ
  | SEnumT (typ_inner, fields) ->
      let typ_inner = substitute_type tidmap typ_inner in
      SEnumT (typ_inner, fields)
  | ExternT fdenv ->
      let fdenv = Envs.FDEnv.map (substitute_funcdef tidmap) fdenv in
      ExternT fdenv
  | ParserT params ->
      let params = List.map (substitute_param tidmap) params in
      ParserT params
  | ControlT params ->
      let params = List.map (substitute_param tidmap) params in
      ControlT params
  | PackageT | TopT -> typ
  | SetT typ_inner -> SetT (substitute_type tidmap typ_inner)
  | RecordT fields ->
      let members, typs_inner = List.split fields in
      let typs_inner = List.map (substitute_type tidmap) typs_inner in
      RecordT (List.combine members typs_inner)
  | StateT -> typ

and substitute_param (tidmap : TIdMap.t) (param : Il.Ast.param') : Il.Ast.param'
    =
  let id, dir, typ, value_default, annos = param in
  let typ = substitute_type tidmap typ in
  (id, dir, typ, value_default, annos)

and substitute_funcdef (tidmap : TIdMap.t) (fd : FuncDef.t) : FuncDef.t =
  match fd with
  | ExternFunctionD (tparams, params, typ_ret) ->
      let tidmap' =
        List.fold_left
          (fun tidmap' tparam -> TIdMap.add tparam (Types.VarT tparam) tidmap')
          tidmap tparams
      in
      let params = List.map (substitute_param tidmap') params in
      let typ_ret = substitute_type tidmap' typ_ret in
      ExternFunctionD (tparams, params, typ_ret)
  | FunctionD (tparams, params, typ_ret) ->
      let tidmap' =
        List.fold_left
          (fun tidmap' tparam -> TIdMap.add tparam (Types.VarT tparam) tidmap')
          tidmap tparams
      in
      let params = List.map (substitute_param tidmap') params in
      let typ_ret = substitute_type tidmap' typ_ret in
      FunctionD (tparams, params, typ_ret)
  | ActionD params ->
      let params = List.map (substitute_param tidmap) params in
      ActionD params
  | ExternMethodD (tparams, params, typ_ret) ->
      let tidmap' =
        List.fold_left
          (fun tidmap' tparam -> TIdMap.add tparam (Types.VarT tparam) tidmap')
          tidmap tparams
      in
      let params = List.map (substitute_param tidmap') params in
      let typ_ret = substitute_type tidmap' typ_ret in
      ExternMethodD (tparams, params, typ_ret)
  | ExternAbstractMethodD (tparams, params, typ_ret) ->
      let tidmap' =
        List.fold_left
          (fun tidmap' tparam -> TIdMap.add tparam (Types.VarT tparam) tidmap')
          tidmap tparams
      in
      let params = List.map (substitute_param tidmap') params in
      let typ_ret = substitute_type tidmap' typ_ret in
      ExternAbstractMethodD (tparams, params, typ_ret)

let specialize_funcdef (fd : FuncDef.t) (targs : Type.t list) : FuncType.t =
  let check_arity tparams =
    if List.length targs <> List.length tparams then (
      Format.eprintf
        "(specialize_funcdef) Function %a expects %d type arguments but %d \
         were given\n"
        FuncDef.pp fd (List.length tparams) (List.length targs);
      assert false)
  in
  match fd with
  | ExternFunctionD (tparams, params, typ_ret) ->
      check_arity tparams;
      let tidmap = List.combine tparams targs |> TIdMap.of_list in
      let params = List.map (substitute_param tidmap) params in
      let typ_ret = substitute_type tidmap typ_ret in
      ExternFunctionT (params, typ_ret)
  | FunctionD (tparams, params, typ_ret) ->
      check_arity tparams;
      let tidmap = List.combine tparams targs |> TIdMap.of_list in
      let params = List.map (substitute_param tidmap) params in
      let typ_ret = substitute_type tidmap typ_ret in
      FunctionT (params, typ_ret)
  | ActionD params ->
      let params = List.map (substitute_param TIdMap.empty) params in
      ActionT params
  | ExternMethodD (tparams, params, typ_ret) ->
      check_arity tparams;
      let tidmap = List.combine tparams targs |> TIdMap.of_list in
      let params = List.map (substitute_param tidmap) params in
      let typ_ret = substitute_type tidmap typ_ret in
      ExternMethodT (params, typ_ret)
  | ExternAbstractMethodD (tparams, params, typ_ret) ->
      check_arity tparams;
      let tidmap = List.combine tparams targs |> TIdMap.of_list in
      let params = List.map (substitute_param tidmap) params in
      let typ_ret = substitute_type tidmap typ_ret in
      ExternAbstractMethodT (params, typ_ret)

let specialize_typedef (td : TypeDef.t) (targs : Type.t list) : Type.t =
  let check_arity tparams =
    if List.length targs <> List.length tparams then (
      Format.eprintf
        "(specialize_typedef) Type definition %a expects %d type arguments but \
         %d were given\n"
        TypeDef.pp td (List.length tparams) (List.length targs);
      assert false)
  in
  match td with
  (* Aliased types are not generic *)
  | DefD typ ->
      check_arity [];
      Types.DefT typ
  | NewD typ ->
      check_arity [];
      Types.NewT typ
  (* Aggregate types are not generic (yet, to be added in v1.2.2) *)
  | StructD fields ->
      check_arity [];
      Types.StructT fields
  | HeaderD fields ->
      check_arity [];
      Types.HeaderT fields
  | UnionD fields ->
      check_arity [];
      Types.UnionT fields
  | EnumD (_id, members) ->
      check_arity [];
      Types.EnumT members
  | SEnumD (_id, typ, fields) ->
      check_arity [];
      Types.SEnumT (typ, fields)
  (* Object types are generic *)
  | ExternD (tparams, fdenv) ->
      check_arity tparams;
      let tidmap = List.combine tparams targs |> TIdMap.of_list in
      let fdenv = Envs.FDEnv.map (substitute_funcdef tidmap) fdenv in
      Types.ExternT fdenv
  | ParserD (tparams, params) ->
      check_arity tparams;
      let tidmap = List.combine tparams targs |> TIdMap.of_list in
      let params = List.map (substitute_param tidmap) params in
      Types.ParserT params
  | ControlD (tparams, params) ->
      check_arity tparams;
      let tidmap = List.combine tparams targs |> TIdMap.of_list in
      let params = List.map (substitute_param tidmap) params in
      Types.ControlT params
  | PackageD tparams ->
      check_arity tparams;
      Types.PackageT

let rec eval_type' (cursor : Ctx.cursor) (ctx : Ctx.t) (typ : El.Ast.typ) :
    Type.t =
  match typ.it with
  | VoidT -> Types.VoidT
  | ErrT -> Types.ErrT
  | StrT -> Types.StrT
  | BoolT -> Types.BoolT
  | IntT -> Types.IntT
  | FIntT expr_width ->
      let width =
        static_eval_expr cursor ctx expr_width
        |> expect_static_value expr_width
        |> Value.get_num
      in
      Types.FIntT width
  | FBitT expr_width ->
      let width =
        static_eval_expr cursor ctx expr_width
        |> expect_static_value expr_width
        |> Value.get_num
      in
      Types.FBitT width
  | VBitT expr_width ->
      let width =
        static_eval_expr cursor ctx expr_width
        |> expect_static_value expr_width
        |> Value.get_num
      in
      Types.VBitT width
  | StackT (typ_inner, expr_size) ->
      let typ_inner = eval_type' cursor ctx typ_inner in
      let size =
        static_eval_expr cursor ctx expr_size
        |> expect_static_value expr_size
        |> Value.get_num
      in
      Types.StackT (typ_inner, size)
  | TupleT typs_inner ->
      let typs_inner = List.map (eval_type' cursor ctx) typs_inner in
      Types.TupleT typs_inner
  (* When a type variable is bound by the type parameters (top-scoped variable should be unallowed) *)
  (* (TODO) Why not shove in the type parameters into the typedef environment? *)
  | NameT { it = Current id; _ }
    when Ctx.find_tparam_opt cursor id.it ctx |> Option.is_some ->
      Types.VarT id.it
  | NameT var ->
      let td = Ctx.find_opt Ctx.find_typedef_opt cursor var ctx in
      if Option.is_none td then (
        Format.eprintf "(eval_type') Type definition %a does not exist\n"
          El.Pp.pp_var var;
        assert false);
      let td = Option.get td in
      specialize_typedef td []
  | SpecT (var, typs) ->
      let td = Ctx.find_opt Ctx.find_typedef_opt cursor var ctx in
      if Option.is_none td then (
        Format.eprintf "(eval_type') Type definition %a does not exist\n"
          El.Pp.pp_var var;
        assert false);
      let td = Option.get td in
      let typs = List.map (eval_type' cursor ctx) typs in
      specialize_typedef td typs
  | AnyT -> Types.TopT

and eval_type (cursor : Ctx.cursor) (ctx : Ctx.t) (typ : El.Ast.typ) : Type.t =
  let typ = eval_type' cursor ctx typ in
  check_valid_type cursor ctx typ;
  typ

(* Direction evaluation *)

and eval_dir (dir : El.Ast.dir) : Dir.t =
  match dir.it with
  | No -> Dir.No `DYN
  | In -> Dir.In
  | Out -> Dir.Out
  | InOut -> Dir.InOut

(* Static expression evaluation *)

(* (18.1) Compile-time known values

   The following are compile-time known values:

   - Integer literals, Boolean literals, and string literals.
   - Identifiers declared in an error, enum, or match_kind declaration.
   - The default identifier.
   - The size field of a value with type header stack.
   - The _ identifier when used as a select expression label
   - The expression {#} representing an invalid header or header union value.
   - Identifiers that represent declared types, actions, tables, parsers, controls, or packages.
   - Tuple expression where all components are compile-time known values.
   - Structure-valued expressions, where all fields are compile-time known values.
   - Expressions evaluating to a list type, where all elements are compile-time known values.
   - Instances constructed by instance declarations (Section 11.3) and constructor invocations.
   - A legal cast applied to a compile-time known value
   - The following expressions (+, -, *, / , %, !, &, |, &&, ||, << , >> , ~ ,  >, <, ==, !=, <=, >=, ++, [:], ?:) when their operands are all compile-time known values.
   - Identifiers declared as constants using the const keyword.
   - Expressions of the form e.minSizeInBits(), e.minSizeInBytes(), e.maxSizeInBits() and e.maxSizeInBytes() *)

and static_eval_expr (cursor : Ctx.cursor) (ctx : Ctx.t) (expr : El.Ast.expr) :
    Value.t option =
  match expr.it with
  | BoolE b -> static_eval_bool b
  | StrE s -> static_eval_str s
  | NumE num -> static_eval_num num
  | VarE var -> static_eval_var cursor ctx var
  | TupleE exprs -> static_eval_tuple cursor ctx exprs
  | RecordE fields -> static_eval_record cursor ctx fields
  | UnE (unop, expr) -> static_eval_unop cursor ctx unop expr
  | BinE (binop, expr_l, expr_r) ->
      static_eval_binop cursor ctx binop expr_l expr_r
  | TernE (expr_cond, expr_then, expr_else) ->
      static_eval_ternop cursor ctx expr_cond expr_then expr_else
  (* | CastE (typ, expr) -> static_eval_cast ctx typ expr *)
  | BitAccE (expr_base, expr_lo, expr_hi) ->
      static_eval_bitstring_acc cursor ctx expr_base expr_lo expr_hi
  | TypeAccE (var, member) -> static_eval_type_acc cursor ctx var member
  | ErrAccE member -> static_eval_error_acc ctx member
  (* | ExprAccE (expr_base, member) -> static_eval_expr_acc ctx expr_base member *)
  (* | CallE (expr_func, targs, args) -> static_eval_call ctx expr_func targs args *)
  | _ -> None

and static_eval_exprs (cursor : Ctx.cursor) (ctx : Ctx.t)
    (exprs : El.Ast.expr list) : Value.t list option =
  let values = List.map (static_eval_expr cursor ctx) exprs in
  if
    List.for_all Option.is_some values && List.length exprs = List.length values
  then Some (List.map Option.get values)
  else None

and expect_static_value (expr : El.Ast.expr) (value : Value.t option) : Value.t
    =
  match value with
  | Some value -> value
  | None ->
      F.eprintf "(expect_static_value) %a is not a compile-time known value\n"
        (El.Pp.pp_expr ~level:0) expr;
      assert false

and static_eval_bool (b : bool) : Value.t option = Some (BoolV b)
and static_eval_str (t : El.Ast.text) : Value.t option = Some (StrV t.it)

and static_eval_num (num : El.Ast.num) : Value.t option =
  match num.it with
  | i, Some (width, signed) ->
      if signed then Some (FIntV (width, i)) else Some (FBitV (width, i))
  | i, None -> Some (IntV i)

and static_eval_var (cursor : Ctx.cursor) (ctx : Ctx.t) (var : El.Ast.var) :
    Value.t option =
  Ctx.find_opt Ctx.find_value_opt cursor var ctx

and static_eval_tuple (cursor : Ctx.cursor) (ctx : Ctx.t)
    (exprs : El.Ast.expr list) : Value.t option =
  let values = static_eval_exprs cursor ctx exprs in
  Option.map (fun values -> Value.TupleV values) values

and static_eval_record (cursor : Ctx.cursor) (ctx : Ctx.t)
    (fields : (El.Ast.member * El.Ast.expr) list) : Value.t option =
  let members, exprs = List.split fields in
  let members = List.map it members in
  let values = static_eval_exprs cursor ctx exprs in
  Option.map (fun values -> Value.StructV (List.combine members values)) values

and static_eval_unop (cursor : Ctx.cursor) (ctx : Ctx.t) (unop : El.Ast.unop)
    (expr : El.Ast.expr) : Value.t option =
  let value = static_eval_expr cursor ctx expr in
  Option.map (Runtime.Numerics.eval_unop unop) value

and static_eval_binop (cursor : Ctx.cursor) (ctx : Ctx.t) (binop : El.Ast.binop)
    (expr_l : El.Ast.expr) (expr_r : El.Ast.expr) : Value.t option =
  let values = static_eval_exprs cursor ctx [ expr_l; expr_r ] in
  Option.map
    (fun values ->
      let value_l, value_r = (List.nth values 0, List.nth values 1) in
      Runtime.Numerics.eval_binop binop value_l value_r)
    values

and static_eval_ternop (cursor : Ctx.cursor) (ctx : Ctx.t)
    (expr_cond : El.Ast.expr) (expr_then : El.Ast.expr)
    (expr_else : El.Ast.expr) : Value.t option =
  let value_cond = static_eval_expr cursor ctx expr_cond in
  Option.map
    (fun value_cond ->
      let cond = Value.get_bool value_cond in
      let expr = if cond then expr_then else expr_else in
      static_eval_expr cursor ctx expr)
    value_cond
  |> Option.join

(* and static_eval_cast (ctx : Ctx.t) (typ : typ) (expr : expr) : Value.t option *)
(*     = *)
(*   let typ = eval_type ctx typ in *)
(*   let typ = saturate_type ctx typ in *)
(*   let value = static_eval_expr ctx expr in *)
(*   Option.map (Runtime.Ops.eval_cast typ) value *)

and static_eval_bitstring_acc (cursor : Ctx.cursor) (ctx : Ctx.t)
    (expr_base : El.Ast.expr) (expr_lo : El.Ast.expr) (expr_hi : El.Ast.expr) :
    Value.t option =
  let values = static_eval_exprs cursor ctx [ expr_base; expr_hi; expr_lo ] in
  Option.map
    (fun values ->
      let value_base, value_hi, value_lo =
        (List.nth values 0, List.nth values 1, List.nth values 2)
      in
      Runtime.Numerics.eval_bitstring_access value_base value_hi value_lo)
    values

and static_eval_type_acc (cursor : Ctx.cursor) (ctx : Ctx.t) (var : El.Ast.var)
    (member : El.Ast.member) : Value.t option =
  let td = Ctx.find_opt Ctx.find_typedef_opt cursor var ctx in
  Option.map
    (fun (td : TypeDef.t) ->
      match td with
      | EnumD (id, members) when List.mem member.it members ->
          Some (Value.EnumFieldV (id, member.it))
      | SEnumD (id, _typ, fields) when List.mem_assoc member.it fields ->
          let value = List.assoc member.it fields in
          Some (Value.SEnumFieldV (id, member.it, value))
      | _ -> None)
    td
  |> Option.join

and static_eval_error_acc (ctx : Ctx.t) (member : El.Ast.member) :
    Value.t option =
  let id = "error." ^ member.it in
  Ctx.find_value_opt Ctx.Global id ctx

(* and static_eval_expr_acc (ctx : Ctx.t) (expr_base : expr) (member : member) : *)
(*     Value.t option = *)
(*   let value_base = static_eval_expr ctx expr_base in *)
(*   match value_base with *)
(*   | Some value_base -> ( *)
(*       match value_base with *)
(*       | StructV fields when List.mem_assoc member.it fields -> *)
(*           Some (List.assoc member.it fields) *)
(*       | StackV (_, _, size) when member.it = "size" -> Some (AIntV size) *)
(*       | _ -> None) *)
(*   | _ -> None *)

(* and static_eval_call (_ctx : Ctx.t) (_expr_func : expr) (_targs : typ list) *)
(*     (_args : arg list) : Value.t option = *)
(*   failwith "(TODO: static_eval_call) Handle static function call" *)

(* Static parameter evaluation *)

and static_eval_param (cursor : Ctx.cursor) (ctx : Ctx.t)
    (param : El.Ast.param') : Il.Ast.param' =
  let id, dir, typ, expr_default, _annos = param in
  let typ = eval_type cursor ctx typ in
  let dir = eval_dir dir in
  let value_default =
    Option.map
      (fun expr_default ->
        static_eval_expr cursor ctx expr_default
        |> expect_static_value expr_default)
      expr_default
  in
  (* (TODO) evaluate annotations *)
  (id, dir, typ, value_default, [])

(* Expression typing *)

let rec type_expr (cursor : Ctx.cursor) (ctx : Ctx.t) (expr : El.Ast.expr) :
    Type.t =
  match expr.it with
  | BoolE _ -> Types.BoolT
  | StrE _ -> Types.StrT
  | NumE num -> type_num_expr num
  | VarE var -> type_var_expr cursor ctx var
  | TupleE exprs -> type_tuple_expr cursor ctx exprs
  | RecordE fields -> type_record_expr cursor ctx fields
  | UnE (unop, expr) -> type_unop_expr cursor ctx unop expr
  | BinE (binp, expr_l, expr_r) -> type_binop_expr cursor ctx binp expr_l expr_r
  | TernE (expr_cond, expr_then, expr_else) ->
      type_ternop_expr cursor ctx expr_cond expr_then expr_else
  | CastE (typ, expr) -> type_cast_expr cursor ctx typ expr
  | MaskE (expr_base, expr_mask) ->
      type_mask_expr cursor ctx expr_base expr_mask
  | RangeE (expr_lb, expr_ub) -> type_range_expr cursor ctx expr_lb expr_ub
  | SelectE (exprs_key, select_cases) ->
      type_select_expr cursor ctx exprs_key select_cases
  | ArrAccE (expr_base, expr_idx) ->
      type_array_acc_expr cursor ctx expr_base expr_idx
  | BitAccE (expr_base, expr_lo, expr_hi) ->
      type_bitstring_acc_expr cursor ctx expr_base expr_lo expr_hi
  | TypeAccE (var_base, member) -> type_type_acc_expr cursor ctx var_base member
  | ErrAccE member -> type_error_acc_expr cursor ctx member
  | ExprAccE (expr_base, member) ->
      type_expr_acc_expr cursor ctx expr_base member
  | CallE (expr_func, targs, args) ->
      type_call_expr cursor ctx expr_func targs args
  | InstE _ ->
      Format.eprintf "(type_expr) %a\n" (El.Pp.pp_expr ~level:0) expr;
      assert false

and type_num_expr (num : El.Ast.num) : Type.t =
  match num.it with
  | _, Some (width, signed) ->
      if signed then Types.FIntT width else Types.FBitT width
  | _, None -> Types.IntT

and type_var_expr (cursor : Ctx.cursor) (ctx : Ctx.t) (var : El.Ast.var) :
    Type.t =
  let typ = Ctx.find_opt Ctx.find_type_opt cursor var ctx in
  if Option.is_none typ then (
    Format.eprintf "(type_var_expr) %a is a free identifier\n" El.Pp.pp_var var;
    assert false);
  Option.get typ

(* (8.12) Operations on tuple expressions

   The empty tuple expression has type tuple<> - a tuple with no components. *)

and type_tuple_expr (cursor : Ctx.cursor) (ctx : Ctx.t)
    (exprs : El.Ast.expr list) : Type.t =
  let typs = List.map (type_expr cursor ctx) exprs in
  let typ = Types.TupleT typs in
  check_valid_type cursor ctx typ;
  typ

(* (8.13) Operations on structure-valued expressions

   One can write expressions that evaluate to a structure or header.

   For a structure-valued expression typeRef is the name of a struct or header type.
   The typeRef can be omitted if it can be inferred from context, e.g., when initializing a variable with a struct type.
   Structure-valued expressions that evaluate to a value of some header type are always valid.

   Structure-valued expressions can be used in the right-hand side of assignments, in comparisons,
   in field selection expressions, and as arguments to functions, method or actions.
   Structure-valued expressions are not left values.

   Structure-valued expressions that do not have ... as their last element must provide a value
   for every member of the struct or header type to which it evaluates, by mentioning each field name exactly once.

   Structure-valued expressions that have ... as their last element are allowed to give values to only
   a subset of the fields of the struct or header type to which it evaluates.
   Any field names not given a value explicitly will be given their default value (see Section 8.26).

   The order of the fields of the struct or header type does not need to
   match the order of the values of the structure-valued expression.

   It is a compile-time error if a field name appears more than once in the same structure-valued expression. *)

and type_record_expr (cursor : Ctx.cursor) (ctx : Ctx.t)
    (fields : (El.Ast.member * El.Ast.expr) list) : Type.t =
  let members, exprs = List.split fields in
  let members = List.map it members in
  let typs = List.map (type_expr cursor ctx) exprs in
  let typ = Types.RecordT (List.combine members typs) in
  check_valid_type cursor ctx typ;
  typ

(* (8.6) Operations on fixed-width bit types (unsigned integers)

   This section discusses all operations that can be performed on expressions of
   type bit<W> for some width W, also known as bit-strings.

   Each of the following operations produces a bit-string result
   when applied to bit-strings of the same width:

    - Negation, denoted by unary -.
        The result is computed by subtracting the value from 2W.
        The result is unsigned and has the same width as the input.
        The semantics is the same as the C negation of unsigned numbers.
    - Unary plus, denoted by +. This operation behaves like a no-op.
    - Bitwise “complement” of a single bit-string, denoted by ~.

   (8.7) Operations on fixed-width signed integers

   This section discusses all operations that can be performed on expressions of type int<W> for some W.
   Recall that the int<W> denotes signed W-bit integers, represented using two's complement.

   The int<W> datatype supports the following operations;
   all binary operations require both operands to have the exact same type.
   The result always has the same width as the left operand.

    - Negation, denoted by unary -.
    - Unary plus, denoted by +. This operation behaves like a no-op.
    - Bitwise “complement” of a single bit-string, denoted by ~.

   (8.8) Operations on arbitrary-precision integers

   The type int denotes arbitrary-precision integers.
   In P4, all expressions of type int must be compile-time known values.
   The type int supports the following operations:

    - Negation, denoted by unary -
    - Unary plus, denoted by +. This operation behaves like a no-op.

   Note: bitwise-operations (|,&,^,~) are not defined on expressions of type int.
   In addition, it is illegal to apply division and modulo to negative values. *)

and type_unop_expr (cursor : Ctx.cursor) (ctx : Ctx.t) (unop : El.Ast.unop)
    (expr : El.Ast.expr) : Type.t =
  let typ = type_expr cursor ctx expr in
  match unop.it with
  | BNotOp ->
      if not (match typ with FIntT _ | FBitT _ -> true | _ -> false) then (
        Format.eprintf
          "(type_unop_expr) ~ expects a fixed-size integer but %a was given\n"
          (El.Pp.pp_expr ~level:0) expr;
        assert false);
      typ
  | LNotOp ->
      if not (match typ with BoolT -> true | _ -> false) then (
        Format.eprintf "(type_unop_expr) ! expects a boolean but %a was given\n"
          (El.Pp.pp_expr ~level:0) expr;
        assert false);
      typ
  | UMinusOp ->
      if not (match typ with IntT | FIntT _ | FBitT _ -> true | _ -> false)
      then (
        Format.eprintf
          "(type_unop_expr) - expects an integer but %a was given\n"
          (El.Pp.pp_expr ~level:0) expr;
        assert false);
      typ

(* (8.2) Operaions on error types

    The error type only supports equality (==) and inequality (!=) comparisons.
    The result of such a comparison is a Boolean value.

    (8.3) Operations on enum types

    Similar to errors, enum expressions without a specified underlying type only support
    equality (==) and inequality (!=) comparisons.

    (8.4) Operations on match_kind types

    They support only assignment and comparisons for equality and inequality.

    (8.5) Expressions on Booleans

    The following operations are provided on Boolean expressions:

     - “And”, denoted by &&
     - “Or”, denoted by ||
     - Negation, denoted by !
     - Equality and inequality tests, denoted by == and != respectively.

    (8.6) Operations on fixed-width bit types (unsigned integers)

    All binary operations except shifts and concatenation require both operands to have the same exact type and width;
    supplying operands with different widths produces an error at compile time.
    No implicit casts are inserted by the compiler to equalize the widths.
    There are no other binary operations that accept signed and unsigned values simultaneously besides shifts and concatenation.
    The following operations are provided on bit-string expressions:

     - Test for equality between bit-strings of the same width, designated by ==. The result is a Boolean value.
     - Test for inequality between bit-strings of the same width, designated by !=. The result is a Boolean value.
     - Unsigned comparisons <,>,<=,>=. Both operands must have the same width and the result is a Boolean value.

   Each of the following operations produces a bit-string result when applied to bit-strings of the same width:

     - Addition, denoted by +.
     - Subtraction, denoted by -.
         The result is unsigned, and has the same type as the operands.
     - Multiplication, denoted by *.
         The result has the same width as the operands and is computed by truncating the result to the output's width.
         P4 architectures may impose additional restrictions
         — e.g., they may only allow multiplication by a non-negative integer power of two.
     - Bitwise “and” between two bit-strings of the same width, denoted by &.
     - Bitwise “or” between two bit-strings of the same width, denoted by |.
     - Bitwise “complement” of a single bit-string, denoted by ~.
     - Bitwise “xor” of two bit-strings of the same width, denoted by ^.
     - Saturating addition, denoted by |+|.
     - Saturating subtraction, denoted by |-|.

    Bit-strings also support the following operations:

     - Logical shift left and right by a (not-necessarily-known-at-compile-time) non-negative integer value,
       denoted by << and >> respectively. In a shift, the left operand is unsigned, and right operand must be either
       an expression of type bit<S> or a non-negative integer value that is known at compile time.
       The result has the same type as the left operand.
     - Concatenation of bit-strings and/or fixed-width signed integers, denoted by ++.
       The two operands must be either bit<W> or int<W>, and they can be of different signedness and width.
       The result has the same signedness as the left operand and the width equal to the sum of the two operands' width.

    (8.7) Operations on fixed-width signed integers

    All binary operations except shifts and concatenation require both operands to have the same exact type (signedness)
    and width and supplying operands with different widths or signedness produces a compile-time error.
    No implicit casts are inserted by the compiler to equalize the types. Except for shifts and concatenation,
    P4 does not have any binary operations that operate simultaneously on signed and unsigned values.

    Note that bitwise operations on signed integers are well-defined, since the representation is mandated to be two's complement.

    The int<W> datatype supports the following operations; all binary operations require both operands to have the exact same type.
    The result always has the same width as the left operand.

     - Addition, denoted by +.
     - Subtraction, denoted by -.
     - Comparison for equality and inequality, denoted == and != respectively.
         These operations produce a Boolean result.
     - Numeric comparisons, denoted by <,<=,>, and >=.
         These operations produce a Boolean result.
     - Multiplication, denoted by *.
         Result has the same width as the operands.
         P4 architectures may impose additional restrictions
         —e.g., they may only allow multiplication by a power of two.
     - Bitwise “and” between two bit-strings of the same width, denoted by &.
     - Bitwise “or” between two bit-strings of the same width, denoted by |.
     - Bitwise “complement” of a single bit-string, denoted by ~.
     - Bitwise “xor” of two bit-strings of the same width, denoted by ^.
     - Saturating addition, denoted by |+|.
     - Saturating subtraction, denoted by |-|.

    The int<W> datatype also support the following operations:

     - Arithmetic shift left and right denoted by << and >>.
       The left operand is signed and the right operand must be either an unsigned number of type bit<S>
       or a non-negative integer compile-time known value. The result has the same type as the left operand.
     - Concatenation of bit-strings and/or fixed-width signed integers, denoted by ++.
       The two operands must be either bit<W> or int<W>, and they can be of different signedness and width.
       The result has the same signedness as the left operand and the width equal to the sum of the two operands' width.

    (8.8) Operations on arbitrary-precsion integers

    The type int denotes arbitrary-precision integers. In P4, all expressions of type int must be compile-time known values. The type int supports the following operations:

     - Addition, denoted by +.
     - Subtraction, denoted by -.
     - Comparison for equality and inequality, denoted by == and != respectively.
         These operations produce a Boolean result.
     - Numeric comparisons <,<=,>, and >=.
         These operations produce a Boolean result.
     - Multiplication, denoted by *.
     - Truncating integer division between positive values, denoted by /.
     - Modulo between positive values, denoted by %.
     - Arithmetic shift left and right denoted by << and >>.
         These operations produce an int result.
         The right operand must be either an unsigned constant of type bit<S> or a non-negative integer compile-time known value.

    Each operand that participates in any of these operation must have type int (except shifts).
    Binary operations cannot be used to combine values of type int with values of a fixed-width type (except shifts).
    However, the compiler automatically inserts casts from int to fixed-width types in certain situations—see Section 8.11.

    Note: bitwise-operations (|,&,^,~) are not defined on expressions of type int.
          In addition, it is illegal to apply division and modulo to negative values.
    Note: saturating arithmetic is not supported for arbitrary-precision integers.

    (8.9) Concatentation and shifts

    (8.9.1) Concatenation

    Concatenation is applied to two bit-strings (signed or unsigned). It is denoted by the infix operator ++.
    The result is a bit-string whose length is the sum of the lengths of the inputs
    where the most significant bits are taken from the left operand; the sign of the result is taken from the left operand.

    (8.9.2) A note about shifts

    The left operand of shifts can be any one out of unsigned bit-strings, signed bit-strings,
    and arbitrary-precision integers, and the right operand of shifts must be either an expression of type bit<S>
    or a non-negative integer compile-time known value. The result has the same type as the left operand.

    (8.10) Operations on variable-size bit types

    To support parsing headers with variable-length fields, P4 offers a type varbit.
    Each occurrence of the type varbit has a statically-declared maximum width, as well as a dynamic width,
    which must not exceed the static bound. Prior to initialization a variable-size bit-string has an unknown dynamic width.

    Variable-length bit-strings support a limited set of operations:

     - Assignment to another variable-sized bit-string.
         The target of the assignment must have the same static width as the source.
         When executed, the assignment sets the dynamic width of the target to the dynamic width of the source.
     - Comparison for equality or inequality with another varbit field.
         Two varbit fields can be compared only if they have the same type.
         Two varbits are equal if they have the same dynamic width and all the bits up to the dynamic width are the same.

    (8.12) Operations on tuple expressions

    Tuples can be compared for equality using == and !=; two tuples are equal if and only if all their fields are respectively equal.

    (8.16) Operations on struct types

    Two structs can be compared for equality (==) or inequality (!=) only if they
    have the same type and all of their fields can be recursively compared for equality.
    Two structures are equal if and only if all their corresponding fields are equal.

    (8.17) Operations on header types

    Two headers can be compared for equality (==) or inequality (!=) only if they
    have the same type. Two headers are equal if and only if they are both invalid,
    or they are both valid and all their corresponding fields are equal.

    (8.18) Operations on header stacks

    Two header stacks can be compared for equality (==) or inequality (!=) only if they
    have the same element type and the same length. Two stacks are equal if and only if
    all their corresponding elements are equal. Note that the nextIndex value is
    not used in the equality comparison.

    (8.19) Operations on header unions

    Two header unions can be compared for equality (==) or inequality (!=) if they
    have the same type. The unions are equal if and only if all their corresponding fields are equal
    (i.e., either all fields are invalid in both unions, or in both unions the same field is valid,
    and the values of the valid fields are equal as headers).

    (8.23) Operations on types introduced by type

    Values with a type introduced by the type keyword provide only a few operations:

     - comparisons for equality and inequality if the original type supported such comparisons *)

and type_binop_plus_minus_mult (typ_l : Type.t) (typ_r : Type.t) : Type.t =
  match (typ_l, typ_r) with
  | IntT, IntT -> Types.IntT
  | FIntT width_l, FIntT width_r when Bigint.(width_l = width_r) ->
      Types.FIntT width_l
  | FBitT width_l, FBitT width_r when Bigint.(width_l = width_r) ->
      Types.FBitT width_l
  | _ ->
      Format.eprintf
        "(type_binop_plus_minus_mult) Addition, subtraction, and \
         multiplication operator expects either two arbitrary precision \
         integers or \n\
        \        two integers of the same signedness and width but %a and %a \
         were given\n"
        Type.pp typ_l Type.pp typ_r;
      assert false

and type_binop_saturating_plus_minus (typ_l : Type.t) (typ_r : Type.t) : Type.t
    =
  match (typ_l, typ_r) with
  | FIntT width_l, FIntT width_r when Bigint.(width_l = width_r) ->
      Types.FIntT width_l
  | FBitT width_l, FBitT width_r when Bigint.(width_l = width_r) ->
      Types.FBitT width_l
  | _ ->
      Format.eprintf
        "(type_binop_saturating_plus_minus) Saturating addition and \
         subtraction operator expects two integers of the same signedness and \
         width but %a and %a were given\n"
        Type.pp typ_l Type.pp typ_r;
      assert false

and type_binop_div_mod (typ_l : Type.t) (typ_r : Type.t) : Type.t =
  match (typ_l, typ_r) with
  (* (TODO) Non-negativity can only be checked dynamically *)
  | IntT, IntT -> Types.IntT
  | _ ->
      Format.eprintf
        "(type_binop_div) Division and modulo operator expects two arbitrary \
         precision integers but %a and %a were given\n"
        Type.pp typ_l Type.pp typ_r;
      assert false

and type_binop_shift (typ_l : Type.t) (typ_r : Type.t) : Type.t =
  match (typ_l, typ_r) with
  | FBitT _, FBitT _ -> typ_l
  (* (TODO) FBitT _, Int is allowed when the int is a compile-time known value *)
  | FIntT _, FBitT _ -> typ_l
  (* (TODO) FIntT _, Int is allowed when the int is a compile-time known value *)
  | IntT, FBitT _ -> typ_l
  (* (TODO) Int _, Int is allowed when the int is a compile-time known value *)
  | _ ->
      Format.eprintf
        "(type_binop_shift) Shift operator expects an unsigned bitstring, \
         signed bitstring, or an integer as the left operand and an unsigned \
         bitstring or a compile-time known integer as the right operand but %a \
         and %a were given\n"
        Type.pp typ_l Type.pp typ_r;
      assert false

and type_binop_compare (typ_l : Type.t) (typ_r : Type.t) : Type.t =
  match (typ_l, typ_r) with
  | IntT, IntT -> Types.BoolT
  | FIntT width_l, FIntT width_r when Bigint.(width_l = width_r) -> Types.BoolT
  | FBitT width_l, FBitT width_r when Bigint.(width_l = width_r) -> Types.BoolT
  | _ ->
      Format.eprintf
        "(type_binop_compare) Comparison expects either two arbitrary \
         precision integers or two integers of the same signedness and width \
         but %a and %a were given\n"
        Type.pp typ_l Type.pp typ_r;
      assert false

and check_type_equals' (typ_l : Type.t) (typ_r : Type.t) : bool =
  match (typ_l, typ_r) with
  | ErrT, ErrT | MatchKindT, MatchKindT | BoolT, BoolT | IntT, IntT -> true
  | FIntT width_l, FIntT width_r -> Bigint.(width_l = width_r)
  | FBitT width_l, FBitT width_r -> Bigint.(width_l = width_r)
  | VBitT width_l, VBitT width_r -> Bigint.(width_l = width_r)
  | DefT typ_inner_l, DefT typ_inner_r | NewT typ_inner_l, NewT typ_inner_r ->
      check_type_equals' typ_inner_l typ_inner_r
  | TupleT typs_inner_l, TupleT typs_inner_r ->
      List.for_all2 check_type_equals' typs_inner_l typs_inner_r
  | StackT (typ_inner_l, size_l), StackT (typ_inner_r, size_r) ->
      check_type_equals' typ_inner_l typ_inner_r && Bigint.(size_l = size_r)
  (* (TODO) Fields can come in different order *)
  | StructT fields_l, StructT fields_r
  | HeaderT fields_l, HeaderT fields_r
  | UnionT fields_l, UnionT fields_r ->
      List.for_all2
        (fun (member_l, typ_l) (member_r, typ_r) ->
          member_l = member_r && check_type_equals' typ_l typ_r)
        fields_l fields_r
  (* (TODO) Enums should be checked of ids, not members *)
  | EnumT members_l, EnumT members_r ->
      List.equal String.equal members_l members_r
  | SEnumT (typ_inner_l, fields_l), SEnumT (typ_inner_r, fields_r) ->
      check_type_equals' typ_inner_l typ_inner_r
      && List.for_all2
           (fun (member_l, _value_l) (member_r, _value_r) ->
             member_l = member_r)
           fields_l fields_r
  | _ -> false

and type_binop_compare_equal (typ_l : Type.t) (typ_r : Type.t) : Type.t =
  if not (check_type_equals' typ_l typ_r) then (
    Format.eprintf
      "(type_binop_compare_equal) %a and %a cannot be compared of equality\n"
      Type.pp typ_l Type.pp typ_r;
    assert false);
  Types.BoolT

and type_binop_bitwise (typ_l : Type.t) (typ_r : Type.t) : Type.t =
  match (typ_l, typ_r) with
  | FIntT width_l, FIntT width_r when Bigint.(width_l = width_r) -> typ_l
  | FBitT width_l, FBitT width_r when Bigint.(width_l = width_r) -> typ_l
  | _ ->
      Format.eprintf
        "(type_binop_bitwise) Bitwise operator expects two bit-strings of the \
         same width but %a and %a were given\n"
        Type.pp typ_l Type.pp typ_r;
      assert false

and type_binop_concat (typ_l : Type.t) (typ_r : Type.t) : Type.t =
  match (typ_l, typ_r) with
  | FIntT width_l, FIntT width_r -> FIntT Bigint.(width_l + width_r)
  | FIntT width_l, FBitT width_r -> FIntT Bigint.(width_l + width_r)
  | FBitT width_l, FIntT width_r -> FBitT Bigint.(width_l + width_r)
  | FBitT width_l, FBitT width_r -> FBitT Bigint.(width_l + width_r)
  | _ ->
      Format.eprintf
        "(type_binop_concat) Concatenation expects two bit-strings but %a and \
         %a were given\n"
        Type.pp typ_l Type.pp typ_r;
      assert false

and type_binop_logical (typ_l : Type.t) (typ_r : Type.t) : Type.t =
  match (typ_l, typ_r) with
  | BoolT, BoolT -> BoolT
  | _ ->
      Format.eprintf
        "(type_binop_logical_expr) Logical operator expects two boolean \
         operands but %a and %a were given\n"
        Type.pp typ_l Type.pp typ_r;
      assert false

and type_binop_expr (cursor : Ctx.cursor) (ctx : Ctx.t) (binop : El.Ast.binop)
    (expr_l : El.Ast.expr) (expr_r : El.Ast.expr) : Type.t =
  let typ_l = type_expr cursor ctx expr_l in
  let typ_r = type_expr cursor ctx expr_r in
  (* (TODO) Insert implicit casts if possible *)
  match binop.it with
  | PlusOp | MinusOp | MulOp -> type_binop_plus_minus_mult typ_l typ_r
  | SPlusOp | SMinusOp -> type_binop_saturating_plus_minus typ_l typ_r
  | DivOp | ModOp -> type_binop_div_mod typ_l typ_r
  | ShlOp | ShrOp -> type_binop_shift typ_l typ_r
  | LeOp | GeOp | LtOp | GtOp -> type_binop_compare typ_l typ_r
  | EqOp | NeOp -> type_binop_compare_equal typ_l typ_r
  | BAndOp | BXorOp | BOrOp -> type_binop_bitwise typ_l typ_r
  | ConcatOp -> type_binop_concat typ_l typ_r
  | LAndOp | LOrOp -> type_binop_logical typ_l typ_r

(* (8.5.1) Conditional operator

   A conditional expression of the form e1 ? e2 : e3 behaves the same as in languages like C.
   As described above, the expression e1 is evaluated first, and second either e2 or e3 is evaluated depending
   on the result.

   The first sub-expression e1 must have Boolean type and the second and third sub-expressions must have the same type,
   which cannot both be arbitrary-precision integers unless the condition itself can be evaluated at compilation time.
   This restriction is designed to ensure that the width of the result of the conditional expression can be inferred
   statically at compile time. *)

and type_ternop_expr (cursor : Ctx.cursor) (ctx : Ctx.t)
    (expr_cond : El.Ast.expr) (expr_then : El.Ast.expr)
    (expr_else : El.Ast.expr) : Type.t =
  let typ_cond = type_expr cursor ctx expr_cond in
  if typ_cond <> Types.BoolT then (
    Format.eprintf "(type_if_stmt) Condition %a must be a boolean\n"
      (El.Pp.pp_expr ~level:0) expr_cond;
    assert false);
  let typ_then = type_expr cursor ctx expr_then in
  let typ_else = type_expr cursor ctx expr_else in
  if typ_then <> typ_else then (
    Format.eprintf
      "(type_ternop_expr) Branches %a and %a must have the same type\n"
      (El.Pp.pp_expr ~level:0) expr_then (El.Pp.pp_expr ~level:0) expr_else;
    assert false);
  (* (TODO) What if expr_cond is compile-time known? *)
  if typ_then = Types.IntT && typ_else = Types.IntT then (
    Format.eprintf
      "(type_ternop_expr) Branches %a and %a cannot both be \
       arbitrary-precision integers\n"
      (El.Pp.pp_expr ~level:0) expr_then (El.Pp.pp_expr ~level:0) expr_else;
    assert false);
  typ_then

(* (8.11) Casts

   P4 provides a limited set of casts between types. A cast is written (t) e,
   where t is a type and e is an expression. Casts are only permitted on base types and derived types
   introduced by typedef, type, and enum.

   (8.11.1) Explicit casts

   The following casts are legal in P4:

    - bit<1> ↔ bool:
        converts the value 0 to false, the value 1 to true, and vice versa.
    - int → bool:
        only if the int value is 0 (converted to false) or 1 (converted to true)
    - int<W> → bit<W>:
        preserves all bits unchanged and reinterprets negative values as positive values
    - bit<W> → int<W>:
        preserves all bits unchanged and reinterprets values whose most-significant bit is 1 as negative values
    - bit<W> → bit<X>:
        truncates the value if W > X, and otherwise (i.e., if W <= X) pads the value with zero bits.
    - int<W> → int<X>:
        truncates the value if W > X, and otherwise (i.e., if W < X) extends it with the sign bit.
    - bit<W> → int:
        preserves the value unchanged but converts it to an unlimited-precision integer;
        the result is always non-negative
    - int<W> → int:
        preserves the value unchanged but converts it to an unlimited-precision integer;
        the result may be negative
    - int → bit<W>:
        converts the integer value into a sufficiently large two's complement bit string to avoid information loss,
        and then truncates the result to W bits. The compiler should emit a warning on
        overflow or on conversion of negative value.
    - int → int<W>:
        converts the integer value into a sufficiently-large two's complement bit string to avoid information loss,
        and then truncates the result to W bits. The compiler should emit a warning on overflow.
    - casts between two types that are introduced by typedef and are equivalent to one of the above combinations.
    - casts between a typedef and the original type.
    - casts between a type introduced by type and the original type.
    - casts between an enum with an explicit type and its underlying type
    - casts of a key-value list to a struct type or a header type (see Section 8.13)
    - casts of a tuple expression to a header stack type
    - casts of an invalid expression {#} to a header or a header union type
    - casts where the destination type is the same as the source type
      if the destination type appears in this list (this excludes e.g., parsers or externs). *)

and type_cast_equal (typ : Type.t) (typ_target : Type.t) : bool =
  match (typ, typ_target) with
  | BoolT, BoolT | IntT, IntT -> true
  | FIntT width, FIntT width_target when width = width_target -> true
  | FBitT width, FBitT width_target when width = width_target -> true
  | DefT typ_inner, DefT typ_target_inner ->
      type_cast_equal typ_inner typ_target_inner
  | NewT typ_inner, NewT typ_target_inner ->
      type_cast_equal typ_inner typ_target_inner
  | SEnumT (typ_inner, _), typ_target -> type_cast_equal typ_inner typ_target
  | _ -> false

and type_cast_explicit (typ : Type.t) (typ_target : Type.t) : bool =
  match (typ, typ_target) with
  | FBitT width, BoolT when width = Bigint.one -> true
  | BoolT, FBitT width when width = Bigint.one -> true
  (* (TODO) int to bool can only be checked dynamically *)
  | FIntT width_target, FBitT width when width_target = width -> true
  | FBitT width_target, FIntT width when width_target = width -> true
  | FBitT _, FBitT _
  | FIntT _, FIntT _
  | FBitT _, IntT
  | FIntT _, IntT
  | IntT, FBitT _
  | IntT, FIntT _ ->
      true
  | DefT typ_inner, DefT typ_target_inner ->
      type_cast_explicit typ_inner typ_target_inner
  | DefT typ_inner, typ_target -> type_cast_explicit typ_inner typ_target
  | typ, DefT typ_target_inner -> type_cast_explicit typ typ_target_inner
  | NewT typ_inner, typ_target -> type_cast_explicit typ_inner typ_target
  | typ, NewT typ_target_inner -> type_cast_explicit typ typ_target_inner
  | SEnumT (typ_inner, _), typ_target -> type_cast_explicit typ_inner typ_target
  | typ, SEnumT (typ_target_inner, _) -> type_cast_explicit typ typ_target_inner
  (* (TODO) Add key-value list as runtime value, e.g., RecordV *)
  (* (TODO) Cast from tuple expression to a header stack type *)
  (* (TODO) Support invalid expression {#} *)
  | _ -> type_cast_equal typ typ_target

and type_cast_expr (cursor : Ctx.cursor) (ctx : Ctx.t) (typ : El.Ast.typ)
    (expr : El.Ast.expr) : Type.t =
  let typ_target = eval_type cursor ctx typ in
  let typ = type_expr cursor ctx expr in
  if not (type_cast_explicit typ typ_target) then (
    Format.eprintf "(type_cast_expr) Invalid cast from %a to %a\n" Type.pp typ
      Type.pp typ_target;
    assert false);
  typ_target

(* (8.15.3) Masks

   The infix operator &&& takes two arguments of the same numeric type (Section 7.4),
   and creates a value of the same type. The right value is used as a “mask”,
   where each bit set to 0 in the mask indicates a “don't care” bit.

   Similar to other binary operations, the mask operator allows the compiler to
   automatically insert casts to unify the argument types in certain situations (section 8.11.2).

   P4 architectures may impose additional restrictions on the expressions on the left and
   right-hand side of a mask operator: for example, they may require that
   either or both sub-expressions be compile-time known values. *)

and type_mask_expr (cursor : Ctx.cursor) (ctx : Ctx.t) (expr_base : El.Ast.expr)
    (expr_mask : El.Ast.expr) : Type.t =
  let typ_base = type_expr cursor ctx expr_base in
  let typ_mask = type_expr cursor ctx expr_mask in
  (* (TODO) Insert cast if possible *)
  (* (CHECK) Which types are allowed in mask expression? *)
  let typ_set =
    match (typ_base, typ_mask) with
    | FBitT width_base, FBitT width_mask when width_base = width_mask ->
        Types.SetT (FBitT width_base)
    | FIntT width_base, FIntT width_mask when width_base = width_mask ->
        Types.SetT (FIntT width_base)
    | _ ->
        Format.eprintf
          "(type_mask_expr) Incompatible types %a and %a for mask operation\n"
          Type.pp typ_base Type.pp typ_mask;
        assert false
  in
  check_valid_type cursor ctx typ_set;
  typ_set

(* (8.15.4) Ranges

   The infix operator .. takes two arguments of the same numeric type T (Section 7.4),
   and creates a value of the type set<T>. The set contains all values numerically between
   the first and the second, inclusively.

   Similar to other binary operations, the range operator allows the compiler to
   automatically insert casts to unify the argument types in certain situations (section 8.11.2). *)

and type_range_expr (cursor : Ctx.cursor) (ctx : Ctx.t) (expr_lb : El.Ast.expr)
    (expr_ub : El.Ast.expr) : Type.t =
  let typ_lb = type_expr cursor ctx expr_lb in
  let typ_ub = type_expr cursor ctx expr_ub in
  (* (TODO) Insert cast if possible *)
  (* (CHECK) Which types are allowed in range expression? *)
  let typ_set =
    match (typ_lb, typ_ub) with
    | IntT, IntT -> Types.SetT IntT
    | FBitT width_lb, FBitT width_ub when width_lb = width_ub ->
        Types.SetT (FBitT width_lb)
    | FIntT width_lb, FIntT width_ub when width_lb = width_ub ->
        Types.SetT (FIntT width_lb)
    | _ ->
        Format.eprintf
          "(type_range_expr) Incompatible types %a and %a for range operation\n"
          Type.pp typ_lb Type.pp typ_ub;
        assert false
  in
  check_valid_type cursor ctx typ_set;
  typ_set

(* (13.6) Select expressions

   A select expression evaluates to a state.

   Each expression in the expressionList must have a type of
   bit<W>, int<W>, bool, enum, serializable enum, or a tuple type with fields of one of the above types.

   In a select expression, if the expressionList has type tuple<T>,
   then each keysetExpression must have type set<tuple<T>>.
   In particular, if a set is specified as a range or mask expression, the endpoints of the range
   and mask expression are implicitly cast to type T using the standard rules for casts. *)

(* (8.15.1) Singleton sets

   In a set context, expressions denote singleton sets.

   (8.15.2) The universal set

   In a set context, the expressions default and _ denote the universal set,
   which contains all possible values of a given type. *)

and type_select_match (ctx : Ctx.t) (typ_key : Type.t) (keyset : El.Ast.keyset)
    : unit =
  match keyset.it with
  | ExprK expr ->
      let typ =
        match expr.it with
        | MaskE _ | RangeE _ -> type_expr Ctx.Local ctx expr
        | _ ->
            let typ = type_expr Ctx.Local ctx expr in
            let typ = Types.SetT typ in
            check_valid_type Ctx.Local ctx typ;
            typ
      in
      if typ_key <> typ then (
        Format.eprintf
          "(type_select_match) Key type %a must match the type of the keyset \
           expression %a\n"
          Type.pp typ_key Type.pp typ;
        assert false)
  | DefaultK | AnyK -> ()

and type_select_matches (ctx : Ctx.t) (typs_key : Type.t list)
    (keysets : El.Ast.keyset list) : unit =
  match (typs_key, keysets) with
  | _, [ { it = DefaultK | AnyK; _ } ] -> ()
  | typs_key, keysets ->
      if List.length typs_key <> List.length keysets then (
        Format.eprintf
          "(type_select_matches) Number of select keys must match the number \
           of cases\n";
        assert false);
      List.iter2 (type_select_match ctx) typs_key keysets

and type_select_case (ctx : Ctx.t) (typs_key : Type.t list)
    (case : El.Ast.select_case) : unit =
  let keysets, state_label = case.it in
  type_select_matches ctx typs_key keysets;
  let typ_label = Ctx.find_type_opt Ctx.Local state_label.it ctx in
  if not (match typ_label with Some Types.StateT -> true | _ -> false) then (
    Format.eprintf "(type_transition_stmt) Label %s is not a valid label\n"
      state_label.it;
    assert false)

and type_select_expr (cursor : Ctx.cursor) (ctx : Ctx.t)
    (exprs_key : El.Ast.expr list) (cases : El.Ast.select_case list) : Type.t =
  if
    not
      (cursor = Ctx.Local
      && match ctx.local.kind with Ctx.ParserState -> true | _ -> false)
  then (
    Format.eprintf
      "(type_select_expr) Select expression must be in a parser state (more \
       strictly, only nested inside a transition statement)\n";
    assert false);
  let typs_key =
    List.map
      (fun expr_key ->
        let typ_key = type_expr Ctx.Local ctx expr_key in
        let typ_key = Types.SetT typ_key in
        check_valid_type Ctx.Local ctx typ_key;
        typ_key)
      exprs_key
  in
  List.iter (check_valid_type Ctx.Local ctx) typs_key;
  List.iter (type_select_case ctx typs_key) cases;
  Types.StateT

(* (8.12) Operations on tuple expressions

   The fields of a tuple can be accessed using array index syntax x[0], x[1].
   The array indexes must be compile-time constants,
   to enable the type-checker to identify the field types statically.

   (8.18) Operations on header stacks

   Given a header stack value hs of size n, the following expressions are legal:

    - hs[index]: produces a reference to the header at the specified position within the stack;
      if hs is an l-value, the result is also an l-value. The header may be invalid.
      Some implementations may impose the constraint that the index expression evaluates to a value
      that is known at compile time. A P4 compiler must give an error if an index value that
      is a compile-time constant is out of range.
      Accessing a header stack hs with an index less than 0 or greater than or equal to hs.size
      results in an undefined value. See Section 8.25 for more details.
      The index is an expression that must be of numeric types (Section 7.4). *)

and type_array_acc_expr (cursor : Ctx.cursor) (ctx : Ctx.t)
    (expr_base : El.Ast.expr) (expr_idx : El.Ast.expr) : Type.t =
  let typ_base = type_expr cursor ctx expr_base in
  let typ_idx = type_expr cursor ctx expr_idx in
  if not (match typ_idx with IntT | FIntT _ | FBitT _ -> true | _ -> false)
  then (
    Format.eprintf "(type_array_acc_expr) Index %a must be of numeric type\n"
      (El.Pp.pp_expr ~level:0) expr_idx;
    assert false);
  match typ_base with
  | TupleT typs_base_inner ->
      let idx =
        static_eval_expr cursor ctx expr_idx
        |> expect_static_value expr_idx
        |> Value.get_num |> Bigint.to_int |> Option.get
      in
      if idx < 0 || idx >= List.length typs_base_inner then (
        Format.eprintf "(type_array_acc_expr) Index %d out of range for %a\n"
          idx Type.pp typ_base;
        assert false);
      List.nth typs_base_inner idx
  (* (TODO) Below doesn't treat index as a compile-time known value *)
  | StackT (typ_base_inner, _) -> typ_base_inner
  | _ ->
      Format.eprintf "(type_array_acc_expr) %a cannot be indexed\n" Type.pp
        typ_base;
      assert false

(* (8.6) Operations on fixed-width bit types (unsigned integers)

   Bit-strings also support the following operations:

    - Extraction of a set of contiguous bits, also known as a slice, denoted by [H:L],
      where H and L must be expressions that evaluate to non-negative compile-time known values, and H >= L.
      The types of H and L (which do not need to be identical) must be numeric (Section 7.4).
      The result is a bit-string of width H - L + 1, including the bits numbered from L
      (which becomes the least significant bit of the result) to H (the most significant bit of the result)
      from the source operand. The conditions 0 <= L <= H < W are checked statically
      (where W is the length of the source bit-string). Note that both endpoints of the extraction are inclusive.
      The bounds are required to be known-at-compile-time values so that the result width can be computed at
      compile time. Slices are also l-values, which means that P4 supports assigning to a slice:  e[H:L] = x .
      The effect of this statement is to set bits H through L (inclusive of both) of e to the
      bit-pattern represented by x, and leaves all other bits of e unchanged.
      A slice of an unsigned integer is an unsigned integer.

   (8.7) Operations on fixed-width signed integers

    - Extraction of a set of contiguous bits, also known as a slice, denoted by [H:L],
      where H and L must be expressions that evaluate to non-negative compile-time known values,
      and H >= L must be true.
      The result is an unsigned bit-string of width H - L + 1, including the bits numbered from L
      (which becomes the least significant bit of the result) to H (the most significant bit of the result)
      from the source operand.

   (8.8) Operations on arbitrary-precision integers

   Bit slices, denoted by [H:L], where H and L must be expressions that evaluate to
   non-negative compile-time known values, and H >= L must be true. The types of H and L
   (which do not need to be identical) must be one of the following:

    - int - an arbitrary-precision integer (section 7.1.6.5)
    - bit<W> - a W-bit unsigned integer where W >= 0 (section 7.1.6.2)
    - int<W> - a W-bit signed integer where W >= 1 (section 7.1.6.3)
    - a serializable enum with an underlying type that is bit<W> or int<W> (section 7.2.1). *)

and check_bitstring_base' (typ : Type.t) : bool =
  match typ with
  | VoidT | ErrT | MatchKindT | StrT | BoolT -> false
  | IntT -> true
  | FIntT width -> Bigint.(width > zero)
  | FBitT width -> Bigint.(width >= zero)
  | VBitT _ | VarT _ -> false
  | DefT typ_inner -> check_bitstring_base' typ_inner
  | NewT _ | TupleT _ | StackT _ | StructT _ | HeaderT _ | UnionT _ | EnumT _
  | SEnumT _ | ExternT _ | ParserT _ | ControlT _ | PackageT | TopT | RecordT _
  | SetT _ | StateT ->
      false

and check_bitstring_base (typ : Type.t) : unit =
  if not (check_bitstring_base' typ) then (
    Format.eprintf "(check_bitstring_base) %a is not a valid base type\n"
      Type.pp typ;
    assert false)

and check_bitstring_index' (typ : Type.t) : bool =
  match typ with
  | VoidT | ErrT | MatchKindT | StrT | BoolT -> false
  | IntT | FIntT _ | FBitT _ -> true
  | VBitT _ | VarT _ -> false
  | DefT typ_inner -> check_bitstring_index' typ_inner
  | NewT _ | TupleT _ | StackT _ | StructT _ | HeaderT _ | UnionT _ | EnumT _ ->
      false
  | SEnumT (typ_inner, _) -> check_bitstring_index' typ_inner
  | ExternT _ | ParserT _ | ControlT _ | PackageT | TopT | RecordT _ | SetT _
  | StateT ->
      false

and check_bitstring_index (typ : Type.t) : unit =
  if not (check_bitstring_index' typ) then (
    Format.eprintf "(check_bitstring_index) %a is not a valid index type\n"
      Type.pp typ;
    assert false)

and check_bitstring_slice_range' (typ_base : Type.t) (width_slice : Bigint.t) :
    bool =
  match typ_base with
  | VoidT | ErrT | MatchKindT | StrT | BoolT -> false
  | IntT -> true
  | FIntT width_base | FBitT width_base -> Bigint.(width_slice <= width_base)
  | VBitT _ | VarT _ -> false
  | DefT typ_inner -> check_bitstring_slice_range' typ_inner width_slice
  | NewT _ | TupleT _ | StackT _ | StructT _ | HeaderT _ | UnionT _ | EnumT _
  | SEnumT _ | ExternT _ | ParserT _ | ControlT _ | PackageT | TopT | RecordT _
  | SetT _ | StateT ->
      false

and check_bitstring_slice_range (typ_base : Type.t) (idx_lo : Bigint.t)
    (idx_hi : Bigint.t) : unit =
  let width_slice = Bigint.(idx_hi - idx_lo + one) in
  if
    Bigint.(idx_lo < zero)
    || Bigint.(idx_hi < zero)
    || Bigint.(idx_lo > idx_hi)
    || not (check_bitstring_slice_range' typ_base width_slice)
  then (
    Format.eprintf "(type_bitstring_acc_expr) Invalid slice [%a:%a] for %a\n"
      Bigint.pp idx_lo Bigint.pp idx_hi Type.pp typ_base;
    assert false)

and type_bitstring_acc_expr (cursor : Ctx.cursor) (ctx : Ctx.t)
    (expr_base : El.Ast.expr) (expr_lo : El.Ast.expr) (expr_hi : El.Ast.expr) :
    Type.t =
  let typ_base = type_expr cursor ctx expr_base in
  check_bitstring_base typ_base;
  let typ_lo = type_expr cursor ctx expr_lo in
  check_bitstring_index typ_lo;
  let idx_lo =
    static_eval_expr cursor ctx expr_lo
    |> expect_static_value expr_lo
    |> Value.get_num
  in
  let typ_hi = type_expr cursor ctx expr_hi in
  check_bitstring_index typ_hi;
  let idx_hi =
    static_eval_expr cursor ctx expr_hi
    |> expect_static_value expr_hi
    |> Value.get_num
  in
  check_bitstring_slice_range typ_base idx_lo idx_hi;
  let width_slice = Bigint.(idx_hi - idx_lo + one) in
  Types.FBitT width_slice

(* (8.3) Operations on enum types

   Symbolic names declared by an enum belong to the namespace introduced by the enum declaration
   rather than the top-level namespace. *)

and type_type_acc_expr (cursor : Ctx.cursor) (ctx : Ctx.t)
    (var_base : El.Ast.var) (member : El.Ast.member) : Type.t =
  let td_base = Ctx.find_opt Ctx.find_typedef_opt cursor var_base ctx in
  if Option.is_none td_base then (
    Format.eprintf "(type_type_acc_expr) %a is a free identifier\n" El.Pp.pp_var
      var_base;
    assert false);
  let td_base = Option.get td_base in
  match td_base with
  | EnumD (_, members) ->
      if not (List.mem member.it members) then (
        Format.eprintf "(type_type_acc_expr) Member %s does not exist in %a\n"
          member.it TypeDef.pp td_base;
        assert false);
      EnumT members
  | SEnumD (_, typ_inner, fields) ->
      if not (List.mem_assoc member.it fields) then (
        Format.eprintf "(type_type_acc_expr) Member %s does not exist in %a\n"
          member.it TypeDef.pp td_base;
        assert false);
      SEnumT (typ_inner, fields)
  | _ ->
      Format.eprintf "(type_type_acc_expr) %a cannot be accessed\n" TypeDef.pp
        td_base;
      assert false

(* (8.2) Operations on error types

   Symbolic names declared by an error declaration belong to the error namespace. *)

and type_error_acc_expr (cursor : Ctx.cursor) (ctx : Ctx.t)
    (member : El.Ast.member) : Type.t =
  let value_error = Ctx.find_value_opt cursor ("error." ^ member.it) ctx in
  if Option.is_none value_error then (
    Format.eprintf "(type_error_acc_expr) Member %s does not exist in error\n"
      member.it;
    assert false);
  Types.ErrT

(* (8.16) Operations on struct types

   The only operation defined on expressions whose type is a struct is field access,
   written using dot (“.”) notation—e.g., s.field.
   If s is an l-value, then s.field is also an l-value.

   (8.17) Operations on headers

   Headers provide the same operations as structs.

   (8.18) Operatins on header stacks

   Given a header stack value hs of size n, the following expressions are legal:

   - hs.size: produces a 32-bit unsigned integer that returns the size
     of the header stack (a compile-time constant).
   - hs.next: produces a reference to the element with index hs.nextIndex in the stack.
     May only be used in a parser. If the stack's nextIndex counter is greater than or equal to size,
     then evaluating this expression results in a transition to reject and sets the error to error.StackOutOfBounds.
     If hs is an l-value, then hs.next is also an l-value.
   - hs.last: produces a reference to the element with index hs.nextIndex - 1 in the stack, if such an element exists.
     May only be used in a parser. If the nextIndex counter is less than 1, or greater than size,
     then evaluating this expression results in a transition to reject and sets the error to error.StackOutOfBounds.
     Unlike hs.next, the resulting reference is never an l-value.
   - hs.lastIndex: produces a 32-bit unsigned integer that encodes the index hs.nextIndex - 1.
     May only be used in a parser. If the nextIndex counter is 0, then evaluating this expression produces an undefined value.

   (8.19) Operations on header unions *)

and type_expr_acc_expr (cursor : Ctx.cursor) (ctx : Ctx.t)
    (expr_base : El.Ast.expr) (member : El.Ast.member) : Type.t =
  let typ_base = type_expr cursor ctx expr_base in
  match typ_base with
  | StackT (typ_inner, _) -> (
      match member.it with
      | "size" | "lastIndex" -> Types.FBitT (Bigint.of_int 32)
      | "next" | "last" -> typ_inner
      | _ ->
          Format.eprintf
            "(type_expr_acc_expr) Invalid member %s for header stack\n"
            member.it;
          assert false)
  | StructT fields | HeaderT fields | UnionT fields ->
      let typ_inner = List.assoc_opt member.it fields in
      if Option.is_none typ_inner then (
        Format.eprintf "(type_expr_acc_expr) Member %s does not exist in %a\n"
          member.it Type.pp typ_base;
        assert false);
      Option.get typ_inner
  | _ ->
      Format.eprintf "(type_expr_acc_expr) %a cannot be accessed\n" Type.pp
        typ_base;
      assert false

(* (8.20) Method invocations and function calls

   A function call or method invocation can optionally specify for each argument the corresponding parameter name.
   It is illegal to use names only for some arguments: either all or no arguments must specify the parameter name.
   Function arguments are evaluated in the order they appear, left to right, before the function invocation takes place.

   The calling convention is copy-in/copy-out (Section 6.8).
   For generic functions the type arguments can be explicitly specified in the function call.
   The compiler only inserts implicit casts for direction in arguments to methods or functions as described in Section 8.11.
   The types for all other arguments must match the parameter types exactly.

   The result returned by a function call is discarded when the function call is used as a statement.

   The “don't care” identifier (_) can only be used for an out function/method argument,
   when the value of returned in that argument is ignored by subsequent computations.
   When used in generic functions or methods, the compiler may reject the program if it is
   unable to infer a type for the don't care argument. *)

(* (6.8) Calling convention: call by copy in/copy out

   Invocations are executed using copy-in/copy-out semantics.

   Each parameter may be labeled with a direction:

   - in parameters are read-only. It is an error to use an in parameter on the left-hand side of an assignment
     or to pass it to a callee as a non-in argument.
   - out parameters are, with a few exceptions listed below, uninitialized and are treated as l-values (See Section 6.7)
     within the body of the method or function. An argument passed as an out parameter must be an l-value;
   - inout parameters behave like a combination of in and out parameters simultaneously:
     In consequence, an argument passed as an inout parameter must be an l-value.
   - The meaning of parameters with no direction depends upon the kind of entity the parameter is for:
      - For anything other than an action, e.g. a control, parser, or function, a directionless parameter means that
        the value supplied as an argument in a call must be a compile-time known value (see Section 18.1).
      - For an action, a directionless parameter indicates that it is “action data”.
        See Section 14.1 for the meaning of action data. *)

(* (Appendix F) Restrictions on compile time and run time calls

   The next table lists restrictions on what kinds of calls can be made from which places in a P4 program.
   Calling a parser, control, or table means invoking its apply() method.
   Calling a value-set means using it in a select expression.
   The row for extern describes where extern method calls can be made from.

   One way that an extern can be called from the top level of a parser or control is in an initializer expression
   for a declared variable, e.g. bit<32> x = rand.get();.

                | can be called at run time from this place in a P4 program
   This type    | parser state | control apply	block | parser/control top level | action | extern | function
   package	    | N/A          | N/A                  | N/A                      | N/A    | N/A    | N/A
   parser       | yes          | no                   | no                       | no     | no     | no
   control      | no           | yes                  | no                       | no     | no     | no
   extern       | yes          | yes                  | yes                      | yes    | no     | no
   table        | no           | yes                  | no                       | no     | no     | no
   value-set    | yes          | no                   | no                       | no     | no     | no
   action       | no           | yes                  | no                       | yes    | no     | no
   function     | yes          | yes                  | no                       | yes    | no     | yes
   value types	| N/A          | N/A                  | N/A                      | N/A    | N/A    | N/A

   There may not be any recursion in calls, neither by a thing calling itself directly, nor mutual recursion.
   An extern can never cause any other type of P4 program object to be called. See Section 6.8.1.
   Actions may be called directly from a control apply block.

   Note that while the extern row shows that extern methods can be called from many places,
   particular externs may have additional restrictions not listed in this table.
   Any such restrictions should be documented in the description for each extern,
   as part of the documentation for the architecture that defines the extern. *)

(* (8.17) Operations on headers

   In addition, headers support the following methods:

    - The method isValid() returns the value of the “validity” bit of the header.
    - The method setValid() sets the header's validity bit to “true”. It can only be applied to an l-value.
    - The method setInvalid() sets the header's validity bit to “false”. It can only be applied to an l-value.

   (8.18) Operations on header stacks

   Finally, P4 offers the following computations that can be used to manipulate
   the elements at the front and back of the stack:

    - hs.push_front(int count): shifts hs “right” by count. The first count elements become invalid.
      The last count elements in the stack are discarded. The hs.nextIndex counter is incremented by count.
      The count argument must be a positive integer that is a compile-time known value. The return type is void.

    - hs.pop_front(int count): shifts hs “left” by count (i.e., element with index count is copied in stack at index 0).
      The last count elements become invalid. The hs.nextIndex counter is decremented by count.
      The count argument must be a positive integer that is a compile-time known value. The return type is void.

   (8.19) Operations on header unions

   u.isValid() returns true if any member of the header union u is valid, otherwise it returns false.
   setValid() and setInvalid() methods are not defined for header unions.

   (9) Compile-time size determination

   The method calls minSizeInBits, minSizeInBytes, maxSizeInBits, and maxSizeInBytes can be applied to
   certain expressions. These method calls return the minimum (or maximum) size in bits (or bytes)
   required to store the expression. Thus, the result type of these methods has type int.
   Except in certain situations involving type variables, discussed below, these method calls produce
   local compile-time known values; otherwise they produce compile-time known values. None of these methods evaluate
   the expression that is the receiver of the method call, so it may be invalid (e.g., an out-of-bounds header stack access).

   The definition of e.minSizeInBits() and e.maxSizeInBits() is
   given recursively on the type of e as described in the following table:

   Type         |	minSizeInBits                                          | maxSizeInBits
   bit<N>       |	N	                                                     | N
   int<N>	      | N	                                                     | N
   bool	        | 1	                                                     | 1
   enum bit<N>  | N                                                      | N
   enum int<N>  | N                                                      | N
   tuple	      | foreach field(tuple) sum of	field.minSizeInBits()      | foreach field(tuple) sum of field.maxSizeInBits()
   varbit<N>    |	0                                                      | N
   struct       | foreach field(struct) sum of field.minSizeInBits()     | foreach field(struct) sum of field.maxSizeInBits()
   header       | foreach field(header) sum of field.minSizeInBits()     | foreach field(header) sum of field.maxSizeInBits()
   H[N]	        | N * H.minSizeInBits()                                  | N * H.maxSizeInBits()
   header_union	| max(foreach field(header_union)	field.minSizeInBits()) | max(foreach field(header_union) field.maxSizeInBits())

   The methods can also be applied to type name expressions e:

    - if the type of e is a type introduced by type, the result is the application of the method to the underlying type
    - if e is the name of a type (e.g., introduced by a typedef declaration), where the type given a name is one of the above,
      then the result is obtained by applying the method to the underlying type.

   These methods are defined for:

    - all serializable types
    - for a type that does not contain varbit fields, both methods return the same result
    - for a type that does contain varbit fields, maxSizeInBits is the worst-case size
        of the serialized representation of the data and minSizeInBits is the “best” case.
    - Every other case is undefined and will produce a compile-time error. *)

and check_call_arity (expr_func : El.Ast.expr) (params : Il.Ast.param' list)
    (args : El.Ast.arg' list) : unit =
  if List.length params <> List.length args then (
    Format.eprintf
      "(check_call_arity) Function %a expects %d arguments but %d were given\n"
      (El.Pp.pp_expr ~level:0) expr_func (List.length params) (List.length args);
    assert false)

and check_named_args (args : El.Ast.arg' list) : unit =
  let is_named arg =
    match (arg : El.Ast.arg') with NameA _ -> true | _ -> false
  in
  if
    not
      (List.for_all is_named args
      || List.for_all (fun arg -> not (is_named arg)) args)
  then (
    Format.eprintf
      "(check_named_args) Either all or no arguments must specify the \
       parameter name\n";
    assert false)

(* Invariant: parameters and arguments are checked of arity and all-or-nothing named *)
and align_params_with_args (params : Il.Ast.param' list)
    (args : El.Ast.arg' list) =
  let module PMap = Map.Make (String) in
  let params_map =
    List.fold_left
      (fun params_map param ->
        let id, _, _, _, _ = param in
        PMap.add id.it param params_map)
      PMap.empty params
  in
  List.fold_left2
    (fun (params, exprs_arg) param arg ->
      match (arg : El.Ast.arg') with
      | ExprA expr_arg -> (params @ [ param ], exprs_arg @ [ Some expr_arg ])
      | NameA (id, expr_arg) ->
          let param = PMap.find id.it params_map in
          (params @ [ param ], exprs_arg @ [ Some expr_arg ])
      | AnyA -> (params @ [ param ], exprs_arg @ [ None ]))
    ([], []) params args

and type_method (cursor : Ctx.cursor) (ctx : Ctx.t) (expr_base : El.Ast.expr)
    (fid : El.Ast.member) (targs : Type.t list) (args : El.Ast.arg' list) :
    FuncType.t option =
  let typ_base = type_expr cursor ctx expr_base in
  match typ_base with
  | StackT _ -> (
      match fid.it with
      | "push_front" | "pop_front" ->
          let params =
            [ ("count" $ no_info, Dir.No `DYN, Types.IntT, None, []) ]
          in
          let typ_ret = Types.VoidT in
          Some (Types.BuiltinMethodT (params, typ_ret))
      | "minSizeInBits" | "minSizeInBytes" ->
          let params = [] in
          let typ_ret = Types.IntT in
          Some (Types.BuiltinMethodT (params, typ_ret))
      | _ -> None)
  | StructT _ -> (
      match fid.it with
      | "minSizeInBits" | "minSizeInBytes" ->
          let params = [] in
          let typ_ret = Types.IntT in
          Some (Types.BuiltinMethodT (params, typ_ret))
      | _ -> None)
  | HeaderT _ -> (
      match fid.it with
      | "isValid" ->
          let params = [] in
          let typ_ret = Types.BoolT in
          Some (Types.BuiltinMethodT (params, typ_ret))
      | "setValid" | "setInvalid" ->
          let params = [] in
          let typ_ret = Types.VoidT in
          Some (Types.BuiltinMethodT (params, typ_ret))
      | "minSizeInBits" | "minSizeInBytes" ->
          let params = [] in
          let typ_ret = Types.IntT in
          Some (Types.BuiltinMethodT (params, typ_ret))
      | _ -> None)
  | UnionT _ -> (
      match fid.it with
      | "isValid" ->
          let params = [] in
          let typ_ret = Types.BoolT in
          Some (Types.BuiltinMethodT (params, typ_ret))
      | "minSizeInBits" | "minSizeInBytes" ->
          let params = [] in
          let typ_ret = Types.IntT in
          Some (Types.BuiltinMethodT (params, typ_ret))
      | _ -> None)
  | ExternT fdenv ->
      let fd = Envs.FDEnv.find_opt (fid.it, args) fdenv in
      Option.map (fun fd -> specialize_funcdef fd targs) fd
  | ParserT params -> (
      match fid.it with
      | "apply" -> Some (Types.ParserApplyMethodT params)
      | _ -> None)
  | ControlT params -> (
      match fid.it with
      | "apply" -> Some (Types.ControlApplyMethodT params)
      | _ -> None)
  | _ -> None

and type_call (cursor : Ctx.cursor) (ctx : Ctx.t) (expr_func : El.Ast.expr)
    (targs : El.Ast.targ list) (args : El.Ast.arg list) : Type.t =
  let args = List.map it args in
  (* Find the function definition and specialize it if necessary *)
  (* (TODO) Implement type inference for missing type arguments *)
  let targs = List.map (eval_type cursor ctx) targs in
  let ft =
    match expr_func.it with
    | VarE var ->
        let fd =
          Ctx.find_overloaded_opt Ctx.find_funcdef_opt cursor var args ctx
        in
        Option.map (fun fd -> specialize_funcdef fd targs) fd
    | ExprAccE (expr_base, fid) ->
        type_method cursor ctx expr_base fid targs args
    | _ -> None
  in
  if Option.is_none ft then (
    Format.eprintf "(type_call_stmt) %a is not a function expression\n"
      (El.Pp.pp_expr ~level:0) expr_func;
    assert false);
  let ft = Option.get ft in
  (* (TODO) Implement restrictions on compile-time and run-time calls (Appendix F) *)
  (* Check if the arguments match the parameters *)
  (* (TODO) Consider default parameters/arguments, in such case arity can appear to mismatch *)
  let params = FuncType.get_params ft in
  check_call_arity expr_func params args;
  check_named_args args;
  let params, exprs_arg = align_params_with_args params args in
  List.iter2
    (fun param expr_arg ->
      let _id_param, dir_param, typ_param, _value_default, _annos = param in
      match expr_arg with
      | Some expr_arg ->
          let typ_arg = type_expr cursor ctx expr_arg in
          (* (TODO) Consider direction of parameters/arguments *)
          (* (TODO) Check subtype instead of stric type equality,
             and insert implicit cast to argument if possible *)
          if typ_param <> typ_arg then (
            Format.eprintf "(type_call) Argument %a is not of type %a\n"
              (El.Pp.pp_expr ~level:0) expr_arg Type.pp typ_param;
            assert false)
      | None ->
          if dir_param <> Dir.Out then (
            Format.eprintf
              "(type_call) Don't care argument can only be used for an out \
               function/method argument\n";
            assert false))
    params exprs_arg;
  FuncType.get_typ_ret ft

and type_call_expr (cursor : Ctx.cursor) (ctx : Ctx.t) (expr_func : El.Ast.expr)
    (targs : El.Ast.targ list) (args : El.Ast.arg list) : Type.t =
  let typ = type_call cursor ctx expr_func targs args in
  if typ = Types.VoidT then (
    Format.eprintf
      "(type_call_expr) Function call as an expression must return a value\n";
    assert false);
  typ

(* Statement typing *)

type flow = Cont | Ret

let rec type_stmt (cursor : Ctx.cursor) (ctx : Ctx.t) (flow : flow)
    (stmt : El.Ast.stmt) : Ctx.t * flow =
  if cursor <> Ctx.Local then (
    Format.eprintf "(type_stmt) Statements must be local\n";
    assert false);
  match stmt.it with
  | EmptyS -> (ctx, flow)
  | AssignS (expr_lhs, expr_rhs) -> type_assign_stmt ctx flow expr_lhs expr_rhs
  | SwitchS _ -> (ctx, flow)
  | IfS (expr_cond, stmt_then, stmt_else) ->
      type_if_stmt ctx flow expr_cond stmt_then stmt_else
  | BlockS block -> type_block_stmt ctx flow block
  | ExitS -> (ctx, flow)
  | RetS expr_ret -> type_return_stmt ctx flow expr_ret
  | CallS (expr_func, targs, args) ->
      type_call_stmt ctx flow expr_func targs args
  | TransS expr -> type_transition_stmt ctx flow expr
  | DeclS decl -> type_decl_stmt ctx flow decl

and type_stmts (cursor : Ctx.cursor) (ctx : Ctx.t) (flow : flow)
    (stmts : El.Ast.stmt list) : Ctx.t * flow =
  List.fold_left
    (fun (ctx, flow) stmt -> type_stmt cursor ctx flow stmt)
    (ctx, flow) stmts

(* (12.1) Assignment statement

   An assignment, written with the = sign, first evaluates its left sub-expression to an l-value,
   then evaluates its right sub-expression to a value, and finally copies the value into the l-value.
   Derived types (e.g. structs) are copied recursively, and all components of headers are copied,
   including “validity” bits. Assignment is not defined for extern values. *)

(* (6.7) L-values

   L-values are expressions that may appear on the left side of an assignment operation
   or as arguments corresponding to out and inout function parameters.
   An l-value represents a storage reference. The following expressions are legal l-values:

   - Identifiers of a base or derived type.
   - Structure, header, and header union field member access operations (using the dot notation).
   - References to elements within header stacks (see Section 8.18): indexing, and references to last and next.
   - The result of a bit-slice operator [m:l]. *)

(* (TODO) Consider direction also *)
and check_lvalue_type' (ctx : Ctx.t) (typ : Type.t) : bool =
  match typ with
  | DefT typ_inner | NewT typ_inner -> check_lvalue_type' ctx typ_inner
  | ExternT _ | ParserT _ | ControlT _ | PackageT | TopT -> false
  | _ -> true

and check_lvalue' (ctx : Ctx.t) (expr : El.Ast.expr) : bool =
  match expr.it with
  | VarE var ->
      let typ = Ctx.find_opt Ctx.find_type_opt Ctx.Local var ctx in
      if Option.is_none typ then (
        Format.eprintf "(check_lvalue) %a is a free identifier\n" El.Pp.pp_var
          var;
        assert false);
      let typ = Option.get typ in
      let is_const =
        Ctx.find_opt Ctx.find_value_opt Ctx.Local var ctx |> Option.is_some
      in
      (not is_const) && check_lvalue_type' ctx typ
  | ArrAccE (expr_base, _) | BitAccE (expr_base, _, _) | ExprAccE (expr_base, _)
    ->
      check_lvalue' ctx expr_base
  | _ -> false

and check_lvalue (ctx : Ctx.t) (expr : El.Ast.expr) : unit =
  if not (check_lvalue' ctx expr) then (
    Format.eprintf "(check_lvalue) %a is not an l-value\n"
      (El.Pp.pp_expr ~level:0) expr;
    assert false)

and type_assign_stmt (ctx : Ctx.t) (flow : flow) (expr_lhs : El.Ast.expr)
    (expr_rhs : El.Ast.expr) : Ctx.t * flow =
  check_lvalue ctx expr_lhs;
  let typ_lhs = type_expr Ctx.Local ctx expr_lhs in
  let typ_rhs = type_expr Ctx.Local ctx expr_rhs in
  (* (TODO) Insert cast if possible *)
  if typ_lhs <> typ_rhs then (
    Format.eprintf
      "(type_assign_stmt) %a on the left side has type %a but %a on the right \
       side has type %a\n"
      (El.Pp.pp_expr ~level:0) expr_lhs Type.pp typ_lhs (El.Pp.pp_expr ~level:0)
      expr_rhs Type.pp typ_rhs;
    assert false);
  (ctx, flow)

(* (12.6) Conditional statement

   However, the condition expression in P4 is required to be a Boolean
   (and not an integer). *)

and type_if_stmt (ctx : Ctx.t) (flow : flow) (expr_cond : El.Ast.expr)
    (stmt_then : El.Ast.stmt) (stmt_else : El.Ast.stmt) : Ctx.t * flow =
  let typ_cond = type_expr Ctx.Local ctx expr_cond in
  if typ_cond <> Types.BoolT then (
    Format.eprintf "(type_if_stmt) Condition %a must be a boolean\n"
      (El.Pp.pp_expr ~level:0) expr_cond;
    assert false);
  let _ctx', flow_then = type_stmt Ctx.Local ctx flow stmt_then in
  let _ctx', flow_else = type_stmt Ctx.Local ctx flow stmt_else in
  match (flow_then, flow_else) with Ret, Ret -> (ctx, Ret) | _ -> (ctx, Cont)

(* (12.3) Block statement

   It contains a sequence of statements and declarations, which are executed sequentially.
   The variables and constants within a block statement are only visible within the block. *)

and type_block_stmt (ctx : Ctx.t) (flow : flow) (block : El.Ast.block) :
    Ctx.t * flow =
  let ctx = Ctx.enter_frame ctx in
  let stmts, _annos = block.it in
  let ctx, flow = type_stmts Ctx.Local ctx flow stmts in
  let ctx = Ctx.exit_frame ctx in
  (ctx, flow)

(* (12.4) Return statement

   The return statement immediately terminates the execution of the action, function or control containing it.
   return statements are not allowed within parsers. return statements followed by an expression are only
   allowed within functions that return values; in this case the type of the expression must match the return type
   of the function. Any copy-out behavior due to direction out or inout parameters of the enclosing action, function,
   or control are still performed after the execution of the return statement.
   See Section 6.8 for details on copy-out behavior. *)

and type_return_stmt (ctx : Ctx.t) (_flow : flow)
    (expr_ret : El.Ast.expr option) : Ctx.t * flow =
  if
    not
      (match ctx.local.kind with
      | Function _ | Action | ExternAbstractMethod _ | ControlApplyMethod ->
          true
      | _ -> false)
  then (
    Format.eprintf
      "(type_return_stmt) Return statement must be in a function, action, \
       abstract extern method, and control method\n";
    assert false);
  let typ_ret =
    match expr_ret with
    | Some expr_ret -> type_expr Ctx.Local ctx expr_ret
    | None -> Types.VoidT
  in
  let typ_ret_func =
    match ctx.local.kind with
    | Function typ_ret_func -> typ_ret_func
    | Action -> Types.VoidT
    | ExternAbstractMethod typ_ret_func -> typ_ret_func
    | ControlApplyMethod -> Types.VoidT
    | _ -> assert false
  in
  (* (TODO) Insert implicit cast, if possible *)
  if typ_ret <> typ_ret_func then (
    Format.eprintf
      "(type_return_stmt) Return type %a does not match the function return \
       type %a\n"
      Type.pp typ_ret Type.pp typ_ret_func;
    assert false);
  (ctx, Ret)

(* (8.20) Method invocations and function calls *)

and type_call_stmt (ctx : Ctx.t) (flow : flow) (expr_func : El.Ast.expr)
    (targs : El.Ast.targ list) (args : El.Ast.arg list) : Ctx.t * flow =
  let _typ = type_call Ctx.Local ctx expr_func targs args in
  (ctx, flow)

(* (13.5) Transition statements

   The last statement in a parser state is an optional transition statement,
   which transfers control to another state, possibly accept or reject. *)

and type_transition_stmt (ctx : Ctx.t) (flow : flow) (expr : El.Ast.expr) :
    Ctx.t * flow =
  if not (match ctx.local.kind with Ctx.ParserState -> true | _ -> false) then (
    Format.eprintf
      "(type_transition_stmt) Transition statement must be in a parser state\n";
    assert false);
  let typ = type_expr Ctx.Local ctx expr in
  if not (match typ with StateT -> true | _ -> false) then (
    Format.eprintf "(type_transition_stmt) Label %a is not a valid label\n"
      (El.Pp.pp_expr ~level:0) expr;
    assert false);
  (ctx, flow)

and type_decl_stmt (ctx : Ctx.t) (flow : flow) (decl : El.Ast.decl) :
    Ctx.t * flow =
  let ctx = type_decl Ctx.Local ctx decl in
  (ctx, flow)

(* Declaration typing *)

and type_decl (cursor : Ctx.cursor) (ctx : Ctx.t) (decl : El.Ast.decl) =
  match decl.it with
  (* Constant, variable, and object declarations *)
  | ConstD { id; typ; value; annos = _annos } ->
      type_const_decl cursor ctx id typ value
  | VarD { id; typ; init; annos = _annos } ->
      type_var_decl cursor ctx id typ init
  | InstD _ ->
      Format.eprintf "(type_decl) %a\n" (El.Pp.pp_decl ~level:0) decl;
      ctx
  (* Derived type declarations *)
  | ErrD { members } -> type_error_decl cursor ctx members
  | MatchKindD { members } -> type_match_kind_decl cursor ctx members
  | StructD { id; fields; annos = _annos } ->
      type_struct_decl cursor ctx id fields
  | HeaderD { id; fields; annos = _annos } ->
      type_header_decl cursor ctx id fields
  | UnionD { id; fields; annos = _annos } ->
      type_union_decl cursor ctx id fields
  | EnumD { id; members; annos = _annos } ->
      type_enum_decl cursor ctx id members
  | SEnumD { id; typ; fields; annos = _annos } ->
      type_senum_decl cursor ctx id typ fields
  | NewTypeD { id; typdef; annos = _annos } ->
      type_newtype_decl cursor ctx id typdef
  | TypeDefD { id; typdef; annos = _annos } ->
      type_typedef_decl cursor ctx id typdef
  (* Function declarations *)
  | ActionD { id; params; body; annos = _annos } ->
      type_action_decl cursor ctx id params body
  | FuncD { id; typ_ret; tparams; params; body } ->
      type_function_decl cursor ctx id tparams params typ_ret body
  | ExternFuncD { id; typ_ret; tparams; params; annos = _annos } ->
      type_extern_function_decl cursor ctx id tparams params typ_ret
  (* Object declarations *)
  (* Extern *)
  | ExternConstructorD { id; cparams; annos = _annos } ->
      type_extern_constructor_decl cursor ctx id cparams
  | ExternAbstractMethodD { id; typ_ret; tparams; params; annos = _annos } ->
      type_extern_abstract_method_decl cursor ctx id tparams params typ_ret
  | ExternMethodD { id; typ_ret; tparams; params; annos = _annos } ->
      type_extern_method_decl cursor ctx id tparams params typ_ret
  | ExternObjectD { id; tparams; mthds; annos = _annos } ->
      type_extern_object_decl cursor ctx id tparams mthds
  (* Parser *)
  | ValueSetD _ ->
      Format.eprintf "(type_decl) %a\n" (El.Pp.pp_decl ~level:0) decl;
      ctx
  | ParserTypeD { id; tparams; params; annos = _annos } ->
      type_parser_type_decl cursor ctx id tparams params
  | ParserD { id; tparams; params; cparams; locals; states; annos = _annos } ->
      type_parser_decl cursor ctx id tparams params cparams locals states
  (* Control *)
  | TableD { id; table; annos = _annos } -> type_table_decl cursor ctx id table
  | ControlTypeD { id; tparams; params; annos = _annos } ->
      type_control_type_decl cursor ctx id tparams params
  | ControlD { id; tparams; params; cparams; locals; body; annos = _annos } ->
      type_control_decl cursor ctx id tparams params cparams locals body
  (* Package *)
  | PackageTypeD { id; tparams; cparams; annos = _annos } ->
      type_package_type_decl cursor ctx id tparams cparams

and type_decls (cursor : Ctx.cursor) (ctx : Ctx.t) (decls : El.Ast.decl list) :
    Ctx.t =
  List.fold_left (type_decl cursor) ctx decls

(* (11.1) Constants

   The initializer expression must be a compile-time known value. *)

and type_const_decl (cursor : Ctx.cursor) (ctx : Ctx.t) (id : El.Ast.id)
    (typ : El.Ast.typ) (expr : El.Ast.expr) : Ctx.t =
  let typ_target = eval_type cursor ctx typ in
  let typ = type_expr cursor ctx expr in
  (* (TODO) Insert cast if possible *)
  if typ_target <> typ then (
    Format.eprintf
      "(type_const_decl) The type of constant %a doesn't match the \
       initializer's type %a\n"
      Type.pp typ_target Type.pp typ;
    assert false);
  let value = static_eval_expr cursor ctx expr in
  if Option.is_none value then (
    Format.eprintf
      "(type_const_decl) %a is not a compile-time known expression\n"
      (El.Pp.pp_expr ~level:0) expr;
    assert false);
  let value = Option.get value in
  Ctx.add_value cursor id.it value ctx |> Ctx.add_type cursor id.it typ_target

(* (11.2) Variables

   Local variables are declared with a type, a name, and an optional initializer
   (as well as an optional annotation).

   Variable declarations without an initializer are uninitialized (except for headers and other header-related types,
   which are initialized to invalid in the same way as described for direction out parameters in Section 6.8).
   The language places few restrictions on the types of the variables: most P4 types that can be written explicitly can be used
   (e.g., base types, struct, header, header stack, tuple). However, it is impossible to declare variables with type int,
   or with types that are only synthesized by the compiler (e.g., set) In addition, variables of
   type parser, control, package, or extern types must be declared using instantiations (see Section 11.3).

   Reading the value of a variable that has not been initialized yields an undefined result.
   The compiler should attempt to detect and emit a warning in such situations.

   Variables declarations can appear in the following locations within a P4 program:

    - In a block statement,
    - In a parser state,
    - In an action body,
    - In a control block's apply sub-block,
    - In the list of local declarations in a parser, and
    - In the list of local declarations in a control.

   Variables have local scope, and behave like stack-allocated variables in languages such as C.
   The value of a variable is never preserved from one invocation of its enclosing block to the next.
   In particular, variables cannot be used to maintain state between different network packets. *)

(* (7.1.5) Strings

   There are no operations on string values; one cannot declare variables with a string type. *)

and check_valid_var_type' (typ : Type.t) : bool =
  match typ with
  | VoidT -> false
  | ErrT | MatchKindT -> true
  | StrT -> false
  | BoolT -> true
  | IntT -> false
  | FIntT _ | FBitT _ | VBitT _ | VarT _ -> true
  | DefT typ_inner | NewT typ_inner -> check_valid_var_type' typ_inner
  | TupleT _ | StackT _ | StructT _ | HeaderT _ | UnionT _ | EnumT _ | SEnumT _
    ->
      true
  | ExternT _ | ParserT _ | ControlT _ | PackageT | TopT | RecordT _ | SetT _
  | StateT ->
      false

and check_valid_var_type (typ : Type.t) : unit =
  if not (check_valid_var_type' typ) then (
    Format.eprintf
      "(check_valid_var_type) Type %a is not a valid variable type\n" Type.pp
      typ;
    assert false)

and type_var_decl (cursor : Ctx.cursor) (ctx : Ctx.t) (id : El.Ast.id)
    (typ : El.Ast.typ) (expr_init : El.Ast.expr option) : Ctx.t =
  if
    (not
       (cursor = Ctx.Block
       && match ctx.block.kind with Parser | Control -> true | _ -> false))
    && not
         (cursor = Ctx.Local
         &&
         match ctx.local.kind with
         | Function _ | Action | ExternAbstractMethod _ | ParserState
         | ControlApplyMethod ->
             true
         | _ -> false)
  then (
    Format.eprintf
      "(type_var_decl) Variables must be declared in a block statement, a \
       parser state, an action body, a control block's apply sub-block, the \
       list of local declarations in a parser or a control\n";
    assert false);
  let typ_target = eval_type cursor ctx typ in
  check_valid_var_type typ_target;
  let typ = Option.map (type_expr cursor ctx) expr_init in
  (* (TODO) Insert cast if possible *)
  (match typ with
  | Some typ when typ_target <> typ ->
      Format.eprintf
        "(type_var_decl) The type of variable %a doesn't match the \
         initializer's type %a\n"
        Type.pp typ_target Type.pp typ;
      assert false
  | _ -> ());
  Ctx.add_type cursor id.it typ_target ctx

(* (7.1.2) The error type

   All error constants are inserted into the error namespace, irrespective of the place where an error is defined.
   error is similar to an enumeration (enum) type in other languages. A program can contain multiple error declarations,
   which the compiler will merge together. It is an error to declare the same identifier multiple times. *)

and type_error_decl (cursor : Ctx.cursor) (ctx : Ctx.t)
    (members : El.Ast.member list) =
  if cursor <> Ctx.Global then (
    Format.eprintf "(type_error_decl) Error declarations must be global\n";
    assert false);
  let type_error_decl' (ctx : Ctx.t) (member : El.Ast.member) : Ctx.t =
    let id = "error." ^ member.it in
    if Ctx.find_value_opt cursor id ctx |> Option.is_some then (
      Format.eprintf "(type_error_decl_glob) Error %s was already defined\n" id;
      assert false);
    let value = Value.ErrV member.it in
    let typ = Types.ErrT in
    Ctx.add_value cursor id value ctx |> Ctx.add_type cursor id typ
  in
  List.fold_left type_error_decl' ctx members

(* (7.1.3) The match kind type

   The match_kind type is very similar to the error type and is used to declare a set of distinct names
   that may be used in a table's key property (described in Section 14.2.1).
   All identifiers are inserted into the top-level namespace.
   It is an error to declare the same match_kind identifier multiple times.

   (TODO) Can the type system enforce the following constraint?

   The declaration of new match_kinds can only occur within model description files;
   P4 programmers cannot declare new match kinds. *)

and type_match_kind_decl (cursor : Ctx.cursor) (ctx : Ctx.t)
    (members : El.Ast.member list) : Ctx.t =
  if cursor <> Ctx.Global then (
    Format.eprintf
      "(type_match_kind_decl) Match kind declarations must be global\n";
    assert false);
  let type_match_kind_decl' (ctx : Ctx.t) (member : El.Ast.member) : Ctx.t =
    let id = member.it in
    if Ctx.find_value_opt cursor id ctx |> Option.is_some then (
      Format.eprintf
        "(type_match_kind_decl) Match kind %s was already defined\n" id;
      assert false);
    let value = Value.MatchKindV member.it in
    let typ = Types.MatchKindT in
    Ctx.add_value cursor id value ctx |> Ctx.add_type cursor id typ
  in
  List.fold_left type_match_kind_decl' ctx members

(* (7.2.5) Struct types

   This declaration introduces a new type with the specified name in the current scope.
   Field names have to be distinct. An empty struct (with no fields) is legal. *)

and type_struct_decl (cursor : Ctx.cursor) (ctx : Ctx.t) (id : El.Ast.id)
    (fields : (El.Ast.member * El.Ast.typ * El.Ast.anno list) list) : Ctx.t =
  if cursor <> Ctx.Global then (
    Format.eprintf "(type_struct_decl) Struct declarations must be global\n";
    assert false);
  let members, typs =
    List.map (fun (member, typ, _annos) -> (member, typ)) fields |> List.split
  in
  let members = List.map it members in
  let typs = List.map (eval_type cursor ctx) typs in
  let fields = List.combine members typs in
  let td = Types.StructD fields in
  check_valid_typedef cursor ctx td;
  Ctx.add_typedef cursor id.it td ctx

(* (7.2.2) Header types *)

and type_header_decl (cursor : Ctx.cursor) (ctx : Ctx.t) (id : El.Ast.id)
    (fields : (El.Ast.member * El.Ast.typ * El.Ast.anno list) list) : Ctx.t =
  if cursor <> Ctx.Global then (
    Format.eprintf "(type_header_decl) Header declarations must be global\n";
    assert false);
  let members, typs =
    List.map (fun (member, typ, _annos) -> (member, typ)) fields |> List.split
  in
  let members = List.map it members in
  let typs = List.map (eval_type cursor ctx) typs in
  let fields = List.combine members typs in
  let td = Types.HeaderD fields in
  check_valid_typedef cursor ctx td;
  Ctx.add_typedef cursor id.it td ctx

(* (7.2.4) Header unions *)

and type_union_decl (cursor : Ctx.cursor) (ctx : Ctx.t) (id : El.Ast.id)
    (fields : (El.Ast.member * El.Ast.typ * El.Ast.anno list) list) : Ctx.t =
  if cursor <> Ctx.Global then (
    Format.eprintf "(type_union_decl) Union declarations must be global\n";
    assert false);
  let members, typs =
    List.map (fun (member, typ, _annos) -> (member, typ)) fields |> List.split
  in
  let members = List.map it members in
  let typs = List.map (eval_type cursor ctx) typs in
  let fields = List.combine members typs in
  let td = Types.UnionD fields in
  check_valid_typedef cursor ctx td;
  Ctx.add_typedef cursor id.it td ctx

(* (7.2.1) Enumeration types

   An enum declaration introduces a new identifier in the current scope for
   naming the created type along with its distinct constants. *)

and type_enum_decl (cursor : Ctx.cursor) (ctx : Ctx.t) (id : El.Ast.id)
    (members : El.Ast.member list) : Ctx.t =
  if cursor <> Ctx.Global then (
    Format.eprintf "(type_enum_decl) Enum declarations must be global\n";
    assert false);
  let members = List.map it members in
  let td = Types.EnumD (id.it, members) in
  check_valid_typedef cursor ctx td;
  Ctx.add_typedef cursor id.it td ctx

(* (7.2.1) Enumeration types

   It is also possible to specify an enum with an underlying representation.
   These are sometimes called serializable enums, because headers are allowed to have fields with such enum types.
   This requires the programmer provide both the fixed-width unsigned (or signed) integer type and an associated integer value
   for each symbolic entry in the enumeration. The symbol typeRef in the grammar above must be one of the following types:

    - an unsigned integer, i.e. bit<W> for some compile-time known W.
    - a signed integer, i.e. int<W> for some compile-time known W.
    - a type name declared via typedef, where the base type of that type is either one of the types listed above,
      or another typedef name that meets these conditions.

   Compiler implementations are expected to raise an error if the fixed-width integer representation for an enumeration entry
   falls outside the representation range of the underlying type. *)

and type_senum_decl (cursor : Ctx.cursor) (ctx : Ctx.t) (id : El.Ast.id)
    (typ : El.Ast.typ) (fields : (El.Ast.member * El.Ast.expr) list) : Ctx.t =
  if cursor <> Ctx.Global then (
    Format.eprintf
      "(type_senum_decl) Serializable enum declarations must be global\n";
    assert false);
  let typ = eval_type cursor ctx typ in
  let members, exprs = List.split fields in
  let members = List.map it members in
  (* (TODO) Check that values are of typ *)
  let values =
    List.map
      (fun expr -> static_eval_expr cursor ctx expr |> expect_static_value expr)
      exprs
  in
  let fields = List.combine members values in
  let td = Types.SEnumD (id.it, typ, fields) in
  check_valid_typedef cursor ctx td;
  Ctx.add_typedef cursor id.it td ctx

(* (7.6) Introducing new types

   Similarly to typedef, the keyword type can be used to introduce a new type.
   While similar to typedef, the type keyword introduces a new type which is not a synonym with the original type:
   values of the original type and the newly introduced type cannot be mixed in expressions.
   Currently the types that can be created by the type keyword are restricted to one of:
   bit<>, int<>, bool, or types defined using type from such types. *)

and type_newtype_decl (cursor : Ctx.cursor) (ctx : Ctx.t) (id : El.Ast.id)
    (typdef : (El.Ast.typ, El.Ast.decl) El.Ast.alt) : Ctx.t =
  if cursor <> Ctx.Global then (
    Format.eprintf "(type_newtype_decl) New type declarations must be global\n";
    assert false);
  match typdef with
  | Left typ ->
      let typ = eval_type cursor ctx typ in
      let td = Types.NewD typ in
      check_valid_typedef cursor ctx td;
      Ctx.add_typedef cursor id.it td ctx
  | Right _ -> failwith "(TODO: type_newtype_decl) Handle newtype with decl"

(* (7.5) typedef

   A typedef declaration can be used to give an alternative name to a type.
   The two types are treated as synonyms, and all operations that can be executed using
   the original type can be also executed using the newly created type.
   If typedef is used with a generic type the type must be specialized with the suitable number of type arguments: *)

and type_typedef_decl (cursor : Ctx.cursor) (ctx : Ctx.t) (id : El.Ast.id)
    (typdef : (El.Ast.typ, El.Ast.decl) El.Ast.alt) : Ctx.t =
  if cursor <> Ctx.Global then (
    Format.eprintf "(type_typedef_decl) Typedef declarations must be global\n";
    assert false);
  match typdef with
  | Left typ ->
      let typ = eval_type cursor ctx typ in
      let td = Types.DefD typ in
      check_valid_typedef cursor ctx td;
      Ctx.add_typedef cursor id.it td ctx
  | Right _ -> failwith "(TODO: type_typedef_decl) Handle typedef with decl"

(* (14.1) Actions

   Actions are code fragments that can read and write the data being processed.
   Actions may contain data values taht can be written by the control plane and read by the data plane.

   Syntactically actions resemble functions with no return value.
   Actions may be declared within a control block: in this case they can only be used within
   instances of that control block.

   Action parameters may not have extern types. Action parameters that have no direction
   (e.g., port in the previous example) indicate "action data." All such parameters must appear
   at the end of the parameter list. When used in a match-action table (see Section 14.2.1.2), these
   parameters will be provided by the table entries (e.g., as specified by the control plane, the
   default_action table property, or the entries table property).

   The body of an action consists of a sequence of statements and declarations. No table, control, or parser
   applications can appear within actions.

   Some targets may impose additional restrictions on action bodies-e.g., only allowing straight-line
   code, with no conditional statements or expressions. *)

and type_action_decl (cursor : Ctx.cursor) (ctx : Ctx.t) (id : El.Ast.id)
    (params : El.Ast.param list) (body : El.Ast.block) : Ctx.t =
  if
    (not (cursor = Ctx.Global))
    && not (cursor = Ctx.Block && ctx.block.kind = Ctx.Control)
  then (
    Format.eprintf
      "(type_action_decl) Action declarations must be global or in a control \
       block\n";
    assert false);
  let params = List.map it params in
  let fid = FId.to_fid id.it params in
  (* Construct action layer context *)
  let ctx' = Ctx.set_id Ctx.Local id.it ctx in
  let ctx' = Ctx.set_localkind Ctx.Action ctx' in
  (* Typecheck and add parameters to the local context *)
  let params = List.map (static_eval_param Ctx.Local ctx') params in
  let ctx' =
    List.fold_left
      (fun ctx' param ->
        let id, _, typ, _, _ = param in
        Ctx.add_type Ctx.Local id.it typ ctx')
      ctx' params
  in
  (* Typecheck body *)
  let stmts, _annos = body.it in
  let _ctx', _flow = type_stmts Ctx.Local ctx' Cont stmts in
  (* Create an action definition *)
  let fd = Types.ActionD params in
  check_valid_funcdef cursor ctx fd;
  Ctx.add_funcdef cursor fid fd ctx

(* (10) Function declarations

   Functions can only be declared at the top level and all parameters must have a direction.
   P4 functions are modeled after functions as found in most other programming languages,
   but the language does not permit recursive functions.

   A function returns a value using the return statement.
   A function with a return type of void can simply use the return statement with no arguments.
   A function with a non-void return type must return a value of the suitable type
   on all possible execution paths. *)

and type_function_decl (cursor : Ctx.cursor) (ctx : Ctx.t) (id : El.Ast.id)
    (tparams : El.Ast.tparam list) (params : El.Ast.param list)
    (typ_ret : El.Ast.typ) (body : El.Ast.block) : Ctx.t =
  if cursor <> Ctx.Global then (
    Format.eprintf "(type_function_decl) Function declarations must be global\n";
    assert false);
  let tparams = List.map it tparams in
  let params = List.map it params in
  let fid = FId.to_fid id.it params in
  (* Construct function layer context *)
  let ctx' = Ctx.set_id Ctx.Local id.it ctx in
  let ctx' = Ctx.add_tparams Ctx.Local tparams ctx' in
  let typ_ret = eval_type Ctx.Local ctx' typ_ret in
  let ctx' = Ctx.set_localkind (Ctx.Function typ_ret) ctx' in
  (* Typecheck and add parameters to the local context *)
  let params = List.map (static_eval_param Ctx.Local ctx') params in
  let ctx' =
    List.fold_left
      (fun ctx' param ->
        let id, _, typ, _, _ = param in
        Ctx.add_type Ctx.Local id.it typ ctx')
      ctx' params
  in
  (* Typecheck body *)
  let stmts, _annos = body.it in
  let _ctx', flow = type_stmts Ctx.Local ctx' Cont stmts in
  if flow <> Ret then (
    Format.eprintf
      "(type_function_decl) A function must return a value on all possible \
       execution paths\n";
    assert false);
  (* Create a function definition *)
  let fd = Types.FunctionD (tparams, params, typ_ret) in
  check_valid_funcdef cursor ctx fd;
  Ctx.add_funcdef cursor fid fd ctx

(* (7.2.10.1) Extern functions

   An extern function declaration describes the name and type signature
   of the function, but not its implementation. *)

and type_extern_function_decl (cursor : Ctx.cursor) (ctx : Ctx.t)
    (id : El.Ast.id) (tparams : El.Ast.tparam list) (params : El.Ast.param list)
    (typ_ret : El.Ast.typ) : Ctx.t =
  if cursor <> Ctx.Global then (
    Format.eprintf
      "(type_extern_function_decl) Extern function declarations must be global\n";
    assert false);
  let tparams = List.map it tparams in
  let params = List.map it params in
  let fid = FId.to_fid id.it params in
  let ctx' = Ctx.set_id Ctx.Local id.it ctx in
  let ctx' = Ctx.add_tparams Ctx.Local tparams ctx' in
  let ctx' = Ctx.set_localkind Ctx.ExternFunction ctx' in
  let params = List.map (static_eval_param Ctx.Local ctx') params in
  let typ_ret = eval_type Ctx.Local ctx' typ_ret in
  let fd = Types.ExternFunctionD (tparams, params, typ_ret) in
  check_valid_funcdef cursor ctx fd;
  Ctx.add_funcdef cursor fid fd ctx

(* (7.2.12) Parser and control blocks types

   Parsers and control blocks types are similar to function types: they describe the signature of parsers and control blocks.
   Such functions have no return values. Declarations of parsers and control block types in architectures may be generic
   (i.e., have type parameters).

   (7.2.12.1) Parser type declarations

   A parser should have at least one argument of type packet_in, representing the received packet that is processed. *)

and type_parser_type_decl (cursor : Ctx.cursor) (ctx : Ctx.t) (id : El.Ast.id)
    (tparams : El.Ast.tparam list) (params : El.Ast.param list) : Ctx.t =
  if cursor <> Ctx.Global then (
    Format.eprintf
      "(type_parser_type_decl) Parser type declarations must be global\n";
    assert false);
  let tparams = List.map it tparams in
  let params = List.map it params in
  (* Typecheck implicit "apply" method
     to construct function definition environment *)
  let ctx' = Ctx.set_id Ctx.Block id.it ctx in
  let ctx' = Ctx.set_blockkind Ctx.Parser ctx' in
  let ctx' = Ctx.add_tparams Ctx.Block tparams ctx' in
  let params = List.map (static_eval_param Ctx.Block ctx') params in
  (* Create a parser type definition
     and add it to the context *)
  let td = Types.ParserD (tparams, params) in
  check_valid_typedef cursor ctx td;
  Ctx.add_typedef cursor id.it td ctx

(* (13.2) Parser declarations

   A parser declaration comprises a name, a list of parameters, an optional list of constructor parameters,
   local elements, and parser states (as well as optional annotations).
   Unlike parser type declarations, parser declarations may not be generic.

   At least one state, named start, must be present in any parser. A parser may not define
   two states with the same name. It is also illegal for a parser to give explicit definitions
   for the accept and reject states—those states are logically distinct from the states defined by the programmer.

   State declarations are described below. Preceding the parser states, a parser may also contain
   a list of local elements. These can be constants, variables, or instantiations of objects that
   may be used within the parser. Such objects may be instantiations of extern objects, or other parsers
   that may be invoked as subroutines. However, it is illegal to instantiate a control block within a parser.

   The states and local elements are all in the same namespace. *)

and type_parser_state (cursor : Ctx.cursor) (ctx : Ctx.t) (block : El.Ast.block)
    : Ctx.t =
  if
    not
      (cursor = Ctx.Local
      && ctx.block.kind = Ctx.Parser
      && match ctx.local.kind with Ctx.ParserState -> true | _ -> false)
  then (
    Format.eprintf "(type_parser_state) Parser state must be local\n";
    assert false);
  let stmts, _annos = block.it in
  let ctx, _flow = type_stmts Ctx.Local ctx Cont stmts in
  ctx

and type_parser_states (_cursor : Ctx.cursor) (ctx : Ctx.t)
    (states : El.Ast.parser_state list) : Ctx.t =
  let states = List.map it states in
  let labels = List.map (fun (label, _, _) -> label.it) states in
  if not (List.mem "start" labels) then (
    Format.eprintf "(type_parser_states) A \"start\" state must exist";
    assert false);
  if List.mem "accept" labels || List.mem "reject" labels then (
    Format.eprintf
      "(type_parser_states) \"accpet\" and \"reject\" states are reserved";
    assert false);
  let labels = "accept" :: "reject" :: labels in
  check_distinct_names labels;
  let ctx' = Ctx.set_localkind Ctx.ParserState ctx in
  let ctx' =
    List.fold_left
      (fun ctx' label -> Ctx.add_type Ctx.Local label Types.StateT ctx')
      ctx' labels
  in
  let _ctx' =
    List.fold_left
      (fun ctx' (label, block, _annos) ->
        let ctx' = Ctx.set_id Ctx.Local label.it ctx' in
        type_parser_state Ctx.Local ctx' block)
      ctx' states
  in
  ctx

and type_parser_decl (cursor : Ctx.cursor) (ctx : Ctx.t) (id : El.Ast.id)
    (tparams : El.Ast.tparam list) (params : El.Ast.param list)
    (cparams : El.Ast.cparam list) (locals : El.Ast.decl list)
    (states : El.Ast.parser_state list) : Ctx.t =
  if cursor <> Ctx.Global then (
    Format.eprintf "(type_parser_decl) Parser declarations must be global\n";
    assert false);
  if tparams <> [] then (
    Format.eprintf "(type_parser_decl) Parser declarations cannot be generic\n";
    assert false);
  let params = List.map it params in
  let cparams = List.map it cparams in
  let cid = FId.to_fid id.it cparams in
  (* Typecheck and add constructor parameters to the block context *)
  let ctx' = Ctx.set_id Ctx.Block id.it ctx in
  let ctx' = Ctx.set_blockkind Ctx.Parser ctx' in
  let cparams = List.map (static_eval_param Ctx.Block ctx') cparams in
  let ctx' =
    List.fold_left
      (fun ctx' cparam ->
        let id, _, typ, _, _ = cparam in
        Ctx.add_type Ctx.Block id.it typ ctx')
      ctx' cparams
  in
  (* Typecheck and add apply parameters to the block context *)
  let params = List.map (static_eval_param Ctx.Block ctx') params in
  let ctx' =
    List.fold_left
      (fun ctx' param ->
        let id, _, typ, _, _ = param in
        Ctx.add_type Ctx.Block id.it typ ctx')
      ctx' params
  in
  (* Typecheck and add local declarations to the block context *)
  let ctx' = type_decls Ctx.Block ctx' locals in
  (* Typecheck parser states *)
  let _ctx' = type_parser_states Ctx.Block ctx' states in
  (* Create a parser constructor definition *)
  let typ = Types.ParserT params in
  let cd = ([], cparams, typ) in
  Ctx.add_consdef cid cd ctx

(* (14.2) Tables

   A table declaration introduces a table instance.
   To obtain multiple instances of a table, it must be declared within a control block
   that is itself instantiated multiple times.

   Syntactically a table is defined in terms of a set of key-value properties.
   Some of these properties are “standard” properties, but the set of properties can
   be extended by target-specific compilers as needed.
   Note duplicated properties are invalid and the compiler should reject them.

   In addition, the tables may optionally define the following properties,

    - default_action: an action to execute when the lookup in the lookup table
        fails to find a match for the key used.
    - size: an integer specifying the desired size of the table.
    - entries: entries that are initially added to a table when the P4 program is loaded,
        some or all of which may be unchangeable by the control plane software.
    - largest_priority_wins - Only useful for some tables with the entries property.
        See section 14.2.1.4 for details.
    - priority_delta - Only useful for some tables with the entries property.
        See section 14.2.1.4 for details.

   The compiler must set the default_action to NoAction (and also insert it into the list of actions)
   for tables that do not define the default_action property.  Hence, all tables can be thought of
   as having a default_action` property, either implicitly or explicitly.

   A property marked as const cannot be changed dynamically by the control plane.
   The key, actions, and size properties are always constant, so the const keyword is not needed for these.

   (14.2.1.1) Keys

   A key is a list of pairs of the form (e : m), where e is an expression that describes the data to be matched
   in the table, and m is a match_kind constant that describes
   the algorithm used to perform the lookup (see Section 7.1.3).

   If a table has no key property, or if the value of its key property is the empty tuple, i.e. key = {},
   then it contains no look-up table, just a default action—i.e., the associated lookup table is always the empty map.

   (14.2.1.2) Actions

   Each action in the list of actions for a table must have a distinct name.

   Each action parameter that has a direction (in, inout, or out) must be bound in the actions list specification;
   conversely, no directionless parameters may be bound in the list.
   The expressions supplied as arguments to an action are not evaluated until the action is invoked.
   Applying tables, whether directly via an expression like table1.apply().hit, or indirectly,
   are forbidden in the expressions supplied as action arguments.

   (14.2.1.3) Default action

   If present, the default_action property must appear after the action property.
   The default action must be one of the actions that appear in the actions list.
   In particular, the expressions passed as in, out, or inout parameters must be
   syntactically identical to the expressions used in one of the elements of the actions list.

   (14.2.1.4) Entries

   Entries cannot be specified for a table with no key (see Sec. 14.2.1.1).

   The keysetExpression component of an entry is a tuple that must provide
   a field for each key in the table keys (see Sec. 14.2.1). The table key type must match
   the type of the element of the set. The actionRef component must be an action which appears
   in the table actions list (and must not have the @defaultonly annotation), with all its arguments bound.

   (14.2.2) Match-action unit invocation

   A table can be invoked by calling its apply method. Calling an apply method on a table instance
   returns a value with a struct type with three fields. This structure is synthesized by the compiler automatically.
   For each table T, the compiler synthesizes an enum and a struct, shown in pseudo-P4:

      enum action_list(T) {
         // one field for each action in the actions list of table T
      }
      struct apply_result(T) {
          bool hit;
          bool miss;
          action_list(T) action_run;
      } *)

and type_table_decl (cursor : Ctx.cursor) (ctx : Ctx.t) (_id : El.Ast.id)
    (_table : El.Ast.table) : Ctx.t =
  if not (cursor = Ctx.Block && ctx.block.kind = Ctx.Control) then (
    Format.eprintf
      "(type_table_decl) Table declarations must be in a control block\n";
    assert false);
  (* (TODO) Check that table properties are valid *)
  ctx

(* (7.2.12.2) Control type declarations *)

and type_control_type_decl (cursor : Ctx.cursor) (ctx : Ctx.t) (id : El.Ast.id)
    (tparams : El.Ast.tparam list) (params : El.Ast.param list) : Ctx.t =
  if cursor <> Ctx.Global then (
    Format.eprintf
      "(type_control_type_decl) Control type declarations must be global\n";
    assert false);
  let tparams = List.map it tparams in
  let params = List.map it params in
  (* Typecheck implicit "apply" method
     to construct function definition environment *)
  let ctx' = Ctx.set_id Ctx.Local id.it ctx in
  let ctx' = Ctx.set_blockkind Ctx.Control ctx' in
  let ctx' = Ctx.add_tparams Ctx.Block tparams ctx' in
  let params = List.map (static_eval_param Ctx.Block ctx') params in
  (* Create a control type definition
     and add it to the context *)
  let td = Types.ControlD (tparams, params) in
  check_valid_typedef cursor ctx td;
  Ctx.add_typedef cursor id.it td ctx

(* (14) Control blocks

   Syntactically, a control block is declared with a name, parameters, optional type parameters,
   and a sequence of declarations of constants, variables, actions, tables, and other instantiations.
   It is illegal to instantiate a parser within a control block.
   Unlike control type declarations, control declarations may not be generic.

   P4 does not support exceptional control-flow within a control block.
   The only statement which has a non-local effect on control flow is exit, which causes execution of
   the enclosing control block to immediately terminate. That is, there is no equivalent of the
   verify statement or the reject state from parsers.
   Hence, all error handling must be performed explicitly by the programmer. *)

and type_control_decl (cursor : Ctx.cursor) (ctx : Ctx.t) (id : El.Ast.id)
    (tparams : El.Ast.tparam list) (params : El.Ast.param list)
    (cparams : El.Ast.cparam list) (locals : El.Ast.decl list)
    (body : El.Ast.block) : Ctx.t =
  if cursor <> Ctx.Global then (
    Format.eprintf "(type_control_decl) Control declarations must be global\n";
    assert false);
  if tparams <> [] then (
    Format.eprintf
      "(type_control_decl) Control declarations cannot be generic\n";
    assert false);
  let params = List.map it params in
  let cparams = List.map it cparams in
  let cid = FId.to_fid id.it cparams in
  (* Typecheck and add constructor parameters to the block context *)
  let ctx' = Ctx.set_id Ctx.Block id.it ctx in
  let ctx' = Ctx.set_blockkind Ctx.Control ctx' in
  let cparams = List.map (static_eval_param Ctx.Block ctx') cparams in
  let ctx' =
    List.fold_left
      (fun ctx' cparam ->
        let id, _, typ, _, _ = cparam in
        Ctx.add_type Ctx.Block id.it typ ctx')
      ctx' cparams
  in
  (* Typecheck and add apply parameters to the block context *)
  let params = List.map (static_eval_param Ctx.Block ctx') params in
  let ctx' =
    List.fold_left
      (fun ctx' param ->
        let id, _, typ, _, _ = param in
        Ctx.add_type Ctx.Block id.it typ ctx')
      ctx' params
  in
  (* Typecheck and add local declarations to the block context *)
  let ctx' = type_decls Ctx.Block ctx' locals in
  (* Typecheck implicit "apply" method *)
  let ctx' = Ctx.set_id Ctx.Local "apply" ctx' in
  let ctx' = Ctx.set_localkind Ctx.ControlApplyMethod ctx' in
  let stmts, _annos = body.it in
  let _ctx', _flow = type_stmts Ctx.Local ctx' Cont stmts in
  (* Create a control constructor definition *)
  let typ = Types.ControlT params in
  let cd = ([], cparams, typ) in
  Ctx.add_consdef cid cd ctx

(* (7.2.13) Package types

   All parameters of a package are evaluated at compilation time, and in consequence they must all be directionless
   (they cannot be in, out, or inout). Otherwise package types are very similar to parser type declarations. *)

and type_package_constructor_decl (cursor : Ctx.cursor) (ctx : Ctx.t)
    (id : El.Ast.id) (cparams : El.Ast.cparam list) : Ctx.t =
  if not (cursor = Ctx.Block && ctx.block.kind = Package) then (
    Format.eprintf
      "(type_package_constructor_decl) Package constructor declarations must \
       be in a package block\n";
    assert false);
  if id.it <> ctx.block.id then (
    Format.eprintf
      "(type_package_constructor_decl) Package constructor must have the same \
       name as the object\n";
    assert false);
  let tparams = Ctx.get_tparams cursor ctx in
  let cparams = List.map it cparams in
  let cid = FId.to_fid id.it cparams in
  let cparams = List.map (static_eval_param Ctx.Block ctx) cparams in
  let td = Ctx.find_typedef Ctx.Global id.it ctx in
  let typ_args = List.map (fun tparam -> Types.VarT tparam) tparams in
  let typ = specialize_typedef td typ_args in
  let cd = (tparams, cparams, typ) in
  check_valid_consdef cursor ctx cd;
  Ctx.add_consdef cid cd ctx

and type_package_type_decl (cursor : Ctx.cursor) (ctx : Ctx.t) (id : El.Ast.id)
    (tparams : El.Ast.tparam list) (cparams : El.Ast.cparam list) : Ctx.t =
  if cursor <> Ctx.Global then (
    Format.eprintf
      "(type_package_type_decl) Package type declarations must be global\n";
    assert false);
  let tparams = List.map it tparams in
  (* Create a package type definition
     and add it to the context *)
  let td = Types.PackageD tparams in
  check_valid_typedef cursor ctx td;
  let ctx = Ctx.add_typedef cursor id.it td ctx in
  (* Package type declaration is implicitly a constructor declaration *)
  let ctx' = Ctx.set_id Ctx.Block id.it ctx in
  let ctx' = Ctx.set_blockkind Ctx.Package ctx' in
  let ctx' = Ctx.add_tparams Ctx.Block tparams ctx' in
  let ctx' = type_package_constructor_decl Ctx.Block ctx' id cparams in
  (* Update the context with the constructor definition environment *)
  { ctx with global = { ctx.global with cdenv = ctx'.global.cdenv } }

(* (7.2.10.2) Extern objects

   An extern object declaration declares an object and all methods that can be invoked to
   perform computations and to alter the state of the object.
   Extern object declarations can also optionally declare constructor methods;
   these must have the same name as the enclosing extern type, no type parameters, and no return type.
   Extern declarations may only appear as allowed by the architecture model and may be specific to a target. *)

and type_extern_constructor_decl (cursor : Ctx.cursor) (ctx : Ctx.t)
    (id : El.Ast.id) (cparams : El.Ast.cparam list) : Ctx.t =
  if not (cursor = Ctx.Block && ctx.block.kind = Ctx.Extern) then (
    Format.eprintf
      "(type_extern_constructor_decl) Extern constructor declarations must be \
       in an extern block\n";
    assert false);
  if id.it <> ctx.block.id then (
    Format.eprintf
      "(type_extern_constructor_decl) Extern constructor must have the same \
       name as the object\n";
    assert false);
  let tparams = Ctx.get_tparams cursor ctx in
  let cparams = List.map it cparams in
  let cid = FId.to_fid id.it cparams in
  let cparams = List.map (static_eval_param cursor ctx) cparams in
  let td = Ctx.find_typedef Ctx.Global id.it ctx in
  let typ_args = List.map (fun tparam -> Types.VarT tparam) tparams in
  let typ = specialize_typedef td typ_args in
  let cd = (tparams, cparams, typ) in
  check_valid_consdef cursor ctx cd;
  Ctx.add_consdef cid cd ctx

(* (7.2.10.2) Extern objects - Abstract methods

   However, some types of extern objects may provide methods that can be implemented by the P4 programmers.
   Such methods are described with the abstract keyword prior to the method definition.
   When such an object is instantiated the user has to supply an implementation of all the abstract methods. *)

and type_extern_abstract_method_decl (cursor : Ctx.cursor) (ctx : Ctx.t)
    (id : El.Ast.id) (tparams : El.Ast.tparam list) (params : El.Ast.param list)
    (typ_ret : El.Ast.typ) : Ctx.t =
  if not (cursor = Ctx.Block && ctx.block.kind = Ctx.Extern) then (
    Format.eprintf
      "(type_extern_abstract_method_decl) Extern method declarations must be \
       in an extern block\n";
    assert false);
  let tparams = List.map it tparams in
  let params = List.map it params in
  let fid = FId.to_fid id.it params in
  let ctx' = Ctx.set_id Ctx.Local id.it ctx in
  let ctx' = Ctx.add_tparams Ctx.Local tparams ctx' in
  let typ_ret = eval_type Ctx.Local ctx' typ_ret in
  let ctx' = Ctx.set_localkind (Ctx.ExternAbstractMethod typ_ret) ctx' in
  let params = List.map (static_eval_param Ctx.Local ctx') params in
  let fd = Types.ExternAbstractMethodD (tparams, params, typ_ret) in
  check_valid_funcdef cursor ctx fd;
  Ctx.add_funcdef cursor fid fd ctx

and type_extern_method_decl (cursor : Ctx.cursor) (ctx : Ctx.t) (id : El.Ast.id)
    (tparams : El.Ast.tparam list) (params : El.Ast.param list)
    (typ_ret : El.Ast.typ) : Ctx.t =
  if not (cursor = Ctx.Block && ctx.block.kind = Ctx.Extern) then (
    Format.eprintf
      "(type_extern_method_decl) Extern method declarations must be in an \
       extern block\n";
    assert false);
  let tparams = List.map it tparams in
  let params = List.map it params in
  let fid = FId.to_fid id.it params in
  let ctx' = Ctx.set_id Ctx.Local id.it ctx in
  let ctx' = Ctx.add_tparams Ctx.Local tparams ctx' in
  let ctx' = Ctx.set_localkind Ctx.ExternMethod ctx' in
  let params = List.map (static_eval_param Ctx.Local ctx') params in
  let typ_ret = eval_type Ctx.Local ctx' typ_ret in
  let fd = Types.ExternMethodD (tparams, params, typ_ret) in
  check_valid_funcdef cursor ctx fd;
  Ctx.add_funcdef cursor fid fd ctx

and type_extern_object_decl (cursor : Ctx.cursor) (ctx : Ctx.t) (id : El.Ast.id)
    (tparams : El.Ast.tparam list) (mthds : El.Ast.decl list) : Ctx.t =
  if cursor <> Ctx.Global then (
    Format.eprintf
      "(type_extern_object_decl) Extern object declarations must be global\n";
    assert false);
  let cons, mthds =
    List.partition
      (fun (mthd : El.Ast.decl) ->
        match mthd.it with ExternConstructorD _ -> true | _ -> false)
      mthds
  in
  let tparams = List.map it tparams in
  (* Typecheck methods and abstract methods
     to construct function definition environment *)
  let ctx' = Ctx.set_id Ctx.Block id.it ctx in
  let ctx' = Ctx.set_blockkind Ctx.Extern ctx' in
  let ctx' = Ctx.add_tparams Ctx.Block tparams ctx' in
  let ctx' = type_decls Ctx.Block ctx' mthds in
  (* Create an extern object type definition
     and add it to the context *)
  let td = Types.ExternD (tparams, ctx'.block.fdenv) in
  check_valid_typedef cursor ctx td;
  let ctx = Ctx.add_typedef cursor id.it td ctx in
  (* Typecheck constructors
     to update constructor definition environment *)
  let ctx'' = Ctx.set_id Ctx.Block id.it ctx in
  let ctx'' = Ctx.set_blockkind Ctx.Extern ctx'' in
  let ctx'' = Ctx.add_tparams Ctx.Block tparams ctx'' in
  let ctx'' = type_decls Ctx.Block ctx'' cons in
  (* Update the context with the constructor definition environment *)
  { ctx with global = { ctx.global with cdenv = ctx''.global.cdenv } }

(* Entry point : Program typing *)

let type_program (program : El.Ast.program) =
  type_decls Ctx.Global Ctx.empty program
