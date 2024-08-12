open Syntax.Ast
open Runtime.Domain
open Util.Source
open Types
module Value = Runtime.Value

(* Utils *)

let expect_value (value : Value.t option) : Value.t =
  match value with Some value -> value | None -> assert false

let expect_values (values : Value.t list option) : Value.t list =
  match values with Some values -> values | None -> assert false

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

module TSet = MakeVis (TId)

let rec check_valid_type (cursor : Ctx.cursor) (ctx : Ctx.t) (typ : Type.t) :
    unit =
  let tset = Ctx.get_tparams cursor ctx |> TSet.of_list in
  check_valid_type' tset typ

and check_valid_type' (tset : TSet.t) (typ : Type.t) : unit =
  match typ with
  | VoidT | ErrT | MatchKindT | StrT | BoolT | IntT | FIntT _ | FBitT _
  | VBitT _ ->
      ()
  | VarT id ->
      if not (TSet.mem id tset) then (
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
  | ExternT fdenv | ParserT fdenv | ControlT fdenv ->
      FDEnv.iter (fun _ fd -> check_valid_funcdef' tset fd) fdenv
  | PackageT | TopT -> ()
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
      | SetT _ | StateT -> false)
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
      | SetT _ | StateT -> false)
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
      | SetT _ | StateT -> false)
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
      | SetT _ | StateT -> false)
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
      | SetT _ | StateT -> false)
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
      | SetT _ | StateT -> false)
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
      | SetT _ | StateT -> false)
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
      | SetT _ | StateT -> false)
  | ExternT _ | ParserT _ | ControlT _ | PackageT | TopT -> error_not_nest ()
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
      | SetT _ | StateT -> false)
  | StateT -> error_not_nest ()

and check_valid_typedef (cursor : Ctx.cursor) (ctx : Ctx.t) (td : TypeDef.t) :
    unit =
  if cursor <> Ctx.Global then (
    Format.eprintf "(check_valid_typedef) Type definitions must be global\n";
    assert false);
  let tset = Ctx.get_tparams cursor ctx |> TSet.of_list in
  match td with
  | DefD typ | NewD typ -> check_valid_type' tset typ
  | StructD fields -> check_valid_type' tset (StructT fields)
  | HeaderD fields -> check_valid_type' tset (HeaderT fields)
  | UnionD fields -> check_valid_type' tset (UnionT fields)
  | EnumD (_id, members) -> check_valid_type' tset (EnumT members)
  | SEnumD (_id, typ, fields) -> check_valid_type' tset (SEnumT (typ, fields))
  | ExternD (tparams, fdenv) ->
      let tset = TSet.union tset (TSet.of_list tparams) in
      check_valid_type' tset (ExternT fdenv)
  | ParserD (tparams, fdenv) ->
      let tset = TSet.union tset (TSet.of_list tparams) in
      check_valid_type' tset (ParserT fdenv)
  | ControlD (tparams, fdenv) ->
      let tset = TSet.union tset (TSet.of_list tparams) in
      check_valid_type' tset (ControlT fdenv)
  | PackageD tparams ->
      let tset = TSet.union tset (TSet.of_list tparams) in
      check_valid_type' tset PackageT

(* (TODO) Appendix F. Restrictions on compile time and runtime calls *)

and check_valid_param (cursor : Ctx.cursor) (ctx : Ctx.t)
    (param : id' * dir' * Type.t * Value.t option) : unit =
  let tset = Ctx.get_tparams cursor ctx |> TSet.of_list in
  check_valid_param' tset param

and check_valid_param' (tset : TSet.t)
    (param : id' * dir' * Type.t * Value.t option) : unit =
  let _, _, typ, _ = param in
  check_valid_type' tset typ

and check_valid_functype (cursor : Ctx.cursor) (ctx : Ctx.t) (ft : FuncType.t) :
    unit =
  let tset = Ctx.get_tparams cursor ctx |> TSet.of_list in
  check_valid_functype' tset ft

and check_valid_functype' (tset : TSet.t) (ft : FuncType.t) : unit =
  match ft with
  | ExternFunctionT (params, typ_ret) | FunctionT (params, typ_ret) ->
      List.iter (check_valid_param' tset) params;
      check_valid_type' tset typ_ret
  | ActionT params -> List.iter (check_valid_param' tset) params
  | ExternMethodT (params, typ_ret) | ExternAbstractMethodT (params, typ_ret) ->
      List.iter (check_valid_param' tset) params;
      check_valid_type' tset typ_ret
  | ParserMethodT params | ControlMethodT params ->
      List.iter (check_valid_param' tset) params
  | TableMethodT -> ()

and check_valid_funcdef (cursor : Ctx.cursor) (ctx : Ctx.t) (fd : FuncDef.t) :
    unit =
  if cursor = Ctx.Local then (
    Format.eprintf
      "(check_valid_funcdef) Function definitions must not be local\n";
    assert false);
  let tset = Ctx.get_tparams cursor ctx |> TSet.of_list in
  check_valid_funcdef' tset fd

and check_valid_funcdef' (tset : TSet.t) (fd : FuncDef.t) : unit =
  match fd with
  | ExternFunctionD (tparams, params, typ_ret) ->
      let tset = TSet.union tset (TSet.of_list tparams) in
      check_valid_functype' tset (ExternFunctionT (params, typ_ret))
  | FunctionD (tparams, params, typ_ret) ->
      let tset = TSet.union tset (TSet.of_list tparams) in
      check_valid_functype' tset (FunctionT (params, typ_ret))
  | ActionD params -> List.iter (check_valid_param' tset) params
  | ExternMethodD (tparams, params, typ_ret) ->
      let tset = TSet.union tset (TSet.of_list tparams) in
      check_valid_functype' tset (ExternMethodT (params, typ_ret))
  | ExternAbstractMethodD (tparams, params, typ_ret) ->
      let tset = TSet.union tset (TSet.of_list tparams) in
      check_valid_functype' tset (ExternAbstractMethodT (params, typ_ret))
  | ParserMethodD params -> check_valid_functype' tset (ParserMethodT params)
  | ControlMethodD params -> check_valid_functype' tset (ControlMethodT params)
  | TableMethodD -> check_valid_functype' tset TableMethodT

and check_valid_cparam (cursor : Ctx.cursor) (ctx : Ctx.t)
    (cparam : id' * dir' * Type.t * Value.t option) : unit =
  let tset = Ctx.get_tparams cursor ctx |> TSet.of_list in
  check_valid_cparam' tset cparam

and check_valid_cparam' (tset : TSet.t)
    (cparam : id' * dir' * Type.t * Value.t option) : unit =
  let _, dir, typ, _ = cparam in
  if dir <> No then (
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
  let tset = Ctx.get_tparams cursor ctx |> TSet.of_list in
  check_valid_consdef' tset cd

and check_valid_consdef' (tset : TSet.t) (cd : ConsDef.t) : unit =
  let tset = TSet.union tset (TSet.of_list cd.tparams) in
  List.iter (check_valid_cparam' tset) cd.cparams;
  check_valid_type' tset cd.typ

(* Type evaluation *)

module TMap = MakeEnv (TId) (Type)

let rec substitute_type (tmap : TMap.t) (typ : Type.t) : Type.t =
  match typ with
  | VoidT | ErrT | MatchKindT | StrT | BoolT | IntT | FIntT _ | FBitT _
  | VBitT _ ->
      typ
  | VarT id ->
      let typ = TMap.find_opt id tmap in
      if Option.is_none typ then (
        Format.eprintf "(substitute_type) %s is a free type variable\n" id;
        assert false);
      let typ = Option.get typ in
      typ
  | DefT typ_inner -> DefT (substitute_type tmap typ_inner)
  | NewT typ_inner -> NewT (substitute_type tmap typ_inner)
  | TupleT typs_inner -> TupleT (List.map (substitute_type tmap) typs_inner)
  | StackT (typ_inner, size) -> StackT (substitute_type tmap typ_inner, size)
  | StructT fields ->
      let members, typs_inner = List.split fields in
      let typs_inner = List.map (substitute_type tmap) typs_inner in
      StructT (List.combine members typs_inner)
  | HeaderT fields ->
      let members, typs_inner = List.split fields in
      let typs_inner = List.map (substitute_type tmap) typs_inner in
      HeaderT (List.combine members typs_inner)
  | UnionT fields ->
      let members, typs_inner = List.split fields in
      let typs_inner = List.map (substitute_type tmap) typs_inner in
      UnionT (List.combine members typs_inner)
  | EnumT _ -> typ
  | SEnumT (typ_inner, fields) ->
      let typ_inner = substitute_type tmap typ_inner in
      SEnumT (typ_inner, fields)
  | ExternT fdenv ->
      let fdenv = FDEnv.map (substitute_funcdef tmap) fdenv in
      ExternT fdenv
  | ParserT fdenv ->
      let fdenv = FDEnv.map (substitute_funcdef tmap) fdenv in
      ParserT fdenv
  | ControlT fdenv ->
      let fdenv = FDEnv.map (substitute_funcdef tmap) fdenv in
      ControlT fdenv
  | PackageT | TopT -> typ
  | SetT typ_inner -> SetT (substitute_type tmap typ_inner)
  | StateT -> typ

and substitute_param (tmap : TMap.t)
    (param : id' * dir' * Type.t * Value.t option) :
    id' * dir' * Type.t * Value.t option =
  let id, dir, typ, value_default = param in
  let typ = substitute_type tmap typ in
  (id, dir, typ, value_default)

and substitute_funcdef (tmap : TMap.t) (fd : FuncDef.t) : FuncDef.t =
  match fd with
  | ExternFunctionD (tparams, params, typ_ret) ->
      let tmap' =
        List.fold_left
          (fun tmap' tparam -> TMap.add tparam (Type.VarT tparam) tmap')
          tmap tparams
      in
      let params = List.map (substitute_param tmap') params in
      let typ_ret = substitute_type tmap' typ_ret in
      ExternFunctionD (tparams, params, typ_ret)
  | FunctionD (tparams, params, typ_ret) ->
      let tmap' =
        List.fold_left
          (fun tmap' tparam -> TMap.add tparam (Type.VarT tparam) tmap')
          tmap tparams
      in
      let params = List.map (substitute_param tmap') params in
      let typ_ret = substitute_type tmap' typ_ret in
      FunctionD (tparams, params, typ_ret)
  | ActionD params ->
      let params = List.map (substitute_param tmap) params in
      ActionD params
  | ExternMethodD (tparams, params, typ_ret) ->
      let tmap' =
        List.fold_left
          (fun tmap' tparam -> TMap.add tparam (Type.VarT tparam) tmap')
          tmap tparams
      in
      let params = List.map (substitute_param tmap') params in
      let typ_ret = substitute_type tmap' typ_ret in
      ExternMethodD (tparams, params, typ_ret)
  | ExternAbstractMethodD (tparams, params, typ_ret) ->
      let tmap' =
        List.fold_left
          (fun tmap' tparam -> TMap.add tparam (Type.VarT tparam) tmap')
          tmap tparams
      in
      let params = List.map (substitute_param tmap') params in
      let typ_ret = substitute_type tmap' typ_ret in
      ExternAbstractMethodD (tparams, params, typ_ret)
  | ParserMethodD params ->
      let params = List.map (substitute_param tmap) params in
      ParserMethodD params
  | ControlMethodD params ->
      let params = List.map (substitute_param tmap) params in
      ControlMethodD params
  | TableMethodD -> TableMethodD

let specialize_funcdef (fd : FuncDef.t) (typ_args : Type.t list) : FuncType.t =
  let check_arity tparams =
    if List.length typ_args <> List.length tparams then (
      Format.eprintf
        "(specialize_funcdef) Function %a expects %d type arguments but %d \
         were given\n"
        FuncDef.pp fd (List.length tparams) (List.length typ_args);
      assert false)
  in
  match fd with
  | ExternFunctionD (tparams, params, typ_ret) ->
      check_arity tparams;
      let tmap = List.combine tparams typ_args |> TMap.of_list in
      let params = List.map (substitute_param tmap) params in
      let typ_ret = substitute_type tmap typ_ret in
      ExternFunctionT (params, typ_ret)
  | FunctionD (tparams, params, typ_ret) ->
      check_arity tparams;
      let tmap = List.combine tparams typ_args |> TMap.of_list in
      let params = List.map (substitute_param tmap) params in
      let typ_ret = substitute_type tmap typ_ret in
      FunctionT (params, typ_ret)
  | ActionD params ->
      let params = List.map (substitute_param TMap.empty) params in
      ActionT params
  | ExternMethodD (tparams, params, typ_ret) ->
      check_arity tparams;
      let tmap = List.combine tparams typ_args |> TMap.of_list in
      let params = List.map (substitute_param tmap) params in
      let typ_ret = substitute_type tmap typ_ret in
      ExternMethodT (params, typ_ret)
  | ExternAbstractMethodD (tparams, params, typ_ret) ->
      check_arity tparams;
      let tmap = List.combine tparams typ_args |> TMap.of_list in
      let params = List.map (substitute_param tmap) params in
      let typ_ret = substitute_type tmap typ_ret in
      ExternAbstractMethodT (params, typ_ret)
  | ParserMethodD params ->
      let params = List.map (substitute_param TMap.empty) params in
      ParserMethodT params
  | ControlMethodD params ->
      let params = List.map (substitute_param TMap.empty) params in
      ControlMethodT params
  | TableMethodD -> TableMethodT

let specialize_typedef (td : TypeDef.t) (typ_args : Type.t list) : Type.t =
  let check_arity tparams =
    if List.length typ_args <> List.length tparams then (
      Format.eprintf
        "(specialize_typedef) Type definition %a expects %d type arguments but \
         %d were given\n"
        TypeDef.pp td (List.length tparams) (List.length typ_args);
      assert false)
  in
  match td with
  (* Aliased types are not generic *)
  | DefD typ ->
      check_arity [];
      Type.DefT typ
  | NewD typ ->
      check_arity [];
      Type.NewT typ
  (* Aggregate types are not generic (yet, to be added in v1.2.2) *)
  | StructD fields ->
      check_arity [];
      Type.StructT fields
  | HeaderD fields ->
      check_arity [];
      Type.HeaderT fields
  | UnionD fields ->
      check_arity [];
      Type.UnionT fields
  | EnumD (_id, members) ->
      check_arity [];
      Type.EnumT members
  | SEnumD (_id, typ, fields) ->
      check_arity [];
      Type.SEnumT (typ, fields)
  (* Object types are generic *)
  | ExternD (tparams, fdenv) ->
      check_arity tparams;
      let tmap = List.combine tparams typ_args |> TMap.of_list in
      let fdenv = FDEnv.map (substitute_funcdef tmap) fdenv in
      Type.ExternT fdenv
  | ParserD (tparams, fdenv) ->
      check_arity tparams;
      let tmap = List.combine tparams typ_args |> TMap.of_list in
      let fdenv = FDEnv.map (substitute_funcdef tmap) fdenv in
      Type.ParserT fdenv
  | ControlD (tparams, fdenv) ->
      check_arity tparams;
      let tmap = List.combine tparams typ_args |> TMap.of_list in
      let fdenv = FDEnv.map (substitute_funcdef tmap) fdenv in
      Type.ControlT fdenv
  | PackageD tparams ->
      check_arity tparams;
      Type.PackageT

let rec eval_type' (cursor : Ctx.cursor) (ctx : Ctx.t) (typ : typ) : Type.t =
  match typ.it with
  | VoidT -> Type.VoidT
  | ErrT -> Type.ErrT
  | StrT -> Type.StrT
  | BoolT -> Type.BoolT
  | IntT -> Type.IntT
  | FIntT expr ->
      let width =
        static_eval_expr cursor ctx expr |> expect_value |> Value.get_num
      in
      Type.FIntT width
  | FBitT expr ->
      let width =
        static_eval_expr cursor ctx expr |> expect_value |> Value.get_num
      in
      Type.FBitT width
  | VBitT expr ->
      let width =
        static_eval_expr cursor ctx expr |> expect_value |> Value.get_num
      in
      Type.VBitT width
  | StackT (typ_inner, expr) ->
      let typ_inner = eval_type' cursor ctx typ_inner in
      let size =
        static_eval_expr cursor ctx expr |> expect_value |> Value.get_num
      in
      Type.StackT (typ_inner, size)
  | TupleT typs_inner ->
      let typs_inner = List.map (eval_type' cursor ctx) typs_inner in
      Type.TupleT typs_inner
  | NameT { it = Current id; _ }
    when Ctx.find_tparam_opt cursor id.it ctx |> Option.is_some ->
      Type.VarT id.it
  | NameT var ->
      let td = Ctx.find_opt Ctx.find_typedef_opt cursor var ctx in
      if Option.is_none td then (
        Format.eprintf "(eval_type') Type definition %a does not exist\n"
          Syntax.Pp.pp_var var;
        assert false);
      let td = Option.get td in
      specialize_typedef td []
  | SpecT (var, typs) ->
      let td = Ctx.find_opt Ctx.find_typedef_opt cursor var ctx in
      if Option.is_none td then (
        Format.eprintf "(eval_type') Type definition %a does not exist\n"
          Syntax.Pp.pp_var var;
        assert false);
      let td = Option.get td in
      let typs = List.map (eval_type' cursor ctx) typs in
      specialize_typedef td typs
  | AnyT -> Type.TopT

and eval_type (cursor : Ctx.cursor) (ctx : Ctx.t) (typ : typ) : Type.t =
  let typ = eval_type' cursor ctx typ in
  check_valid_type cursor ctx typ;
  typ

(* Static parameter evaluation *)

and static_eval_param (cursor : Ctx.cursor) (ctx : Ctx.t) (param : param') :
    id' * dir' * Type.t * Value.t option =
  let id, dir, typ, expr_default, _ = param in
  let typ = eval_type cursor ctx typ in
  let value_default =
    Option.map
      (fun expr_default ->
        static_eval_expr cursor ctx expr_default |> expect_value)
      expr_default
  in
  (id.it, dir.it, typ, value_default)

(* Static expression evaluation *)

(* (18.1)
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

and static_eval_expr (cursor : Ctx.cursor) (ctx : Ctx.t) (expr : expr) :
    Value.t option =
  match expr.it with
  | BoolE b -> static_eval_bool b
  | StrE s -> static_eval_str s
  | NumE { it = value, encoding; _ } -> static_eval_num value encoding
  | VarE var -> static_eval_var cursor ctx var
  | ListE exprs -> static_eval_list cursor ctx exprs
  | RecordE fields -> static_eval_record cursor ctx fields
  | UnE (unop, expr) -> static_eval_unop cursor ctx unop expr
  | BinE (binop, expr_fst, expr_snd) ->
      static_eval_binop cursor ctx binop expr_fst expr_snd
  | TernE (expr_cond, expr_tru, expr_fls) ->
      static_eval_ternop cursor ctx expr_cond expr_tru expr_fls
  (* | CastE (typ, expr) -> static_eval_cast ctx typ expr *)
  | BitAccE (expr_base, expr_lo, expr_hi) ->
      static_eval_bitstring_acc cursor ctx expr_base expr_lo expr_hi
  | TypeAccE (var, member) -> static_eval_type_acc cursor ctx var member
  | ErrAccE member -> static_eval_error_acc ctx member
  (* | ExprAccE (expr_base, member) -> static_eval_expr_acc ctx expr_base member *)
  (* | CallE (expr_func, targs, args) -> static_eval_call ctx expr_func targs args *)
  | _ -> None

and static_eval_bool (b : bool) : Value.t option = Some (BoolV b)
and static_eval_str (t : text) : Value.t option = Some (StrV t.it)

and static_eval_num (value : Bigint.t) (encoding : (Bigint.t * bool) option) :
    Value.t option =
  match encoding with
  | Some (width, signed) ->
      if signed then Some (FIntV (width, value))
      else Some (FBitV (width, value))
  | None -> Some (IntV value)

and static_eval_var (cursor : Ctx.cursor) (ctx : Ctx.t) (var : var) :
    Value.t option =
  Ctx.find_opt Ctx.find_value_opt cursor var ctx

and static_eval_list (cursor : Ctx.cursor) (ctx : Ctx.t) (exprs : expr list) :
    Value.t option =
  let values = static_eval_exprs cursor ctx exprs in
  Option.map (fun values -> Value.TupleV values) values

and static_eval_record (cursor : Ctx.cursor) (ctx : Ctx.t)
    (fields : (member * expr) list) : Value.t option =
  let members, exprs = List.split fields in
  let members = List.map it members in
  let values = static_eval_exprs cursor ctx exprs in
  Option.map (fun values -> Value.StructV (List.combine members values)) values

and static_eval_unop (cursor : Ctx.cursor) (ctx : Ctx.t) (unop : unop)
    (expr : expr) : Value.t option =
  let value = static_eval_expr cursor ctx expr in
  Option.map (Runtime.Ops.eval_unop unop) value

and static_eval_binop (cursor : Ctx.cursor) (ctx : Ctx.t) (binop : binop)
    (expr_fst : expr) (expr_snd : expr) : Value.t option =
  let values = static_eval_exprs cursor ctx [ expr_fst; expr_snd ] in
  Option.map
    (fun values ->
      let value_fst, value_snd = (List.nth values 0, List.nth values 1) in
      Runtime.Ops.eval_binop binop value_fst value_snd)
    values

and static_eval_ternop (cursor : Ctx.cursor) (ctx : Ctx.t) (expr_cond : expr)
    (expr_tru : expr) (expr_fls : expr) : Value.t option =
  let value_cond = static_eval_expr cursor ctx expr_cond in
  Option.map
    (fun value_cond ->
      let cond = Value.get_bool value_cond in
      let expr = if cond then expr_tru else expr_fls in
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
    (expr_base : expr) (expr_lo : expr) (expr_hi : expr) : Value.t option =
  let values = static_eval_exprs cursor ctx [ expr_base; expr_hi; expr_lo ] in
  Option.map
    (fun values ->
      let value_base, value_hi, value_lo =
        (List.nth values 0, List.nth values 1, List.nth values 2)
      in
      Runtime.Ops.eval_bitstring_access value_base value_hi value_lo)
    values

and static_eval_type_acc (cursor : Ctx.cursor) (ctx : Ctx.t) (var : var)
    (member : member) : Value.t option =
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

and static_eval_error_acc (ctx : Ctx.t) (member : member) : Value.t option =
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

and static_eval_exprs (cursor : Ctx.cursor) (ctx : Ctx.t) (exprs : expr list) :
    Value.t list option =
  let values = List.map (static_eval_expr cursor ctx) exprs in
  if
    List.for_all Option.is_some values && List.length exprs = List.length values
  then Some (List.map Option.get values)
  else None

(* Type checking *)

(* Expression typing *)

let rec type_expr (cursor : Ctx.cursor) (ctx : Ctx.t) (expr : expr) : Type.t =
  match expr.it with
  | BoolE _ -> Type.BoolT
  | StrE _ -> Type.StrT
  | NumE { it = _, encoding; _ } -> type_num_expr encoding
  | VarE var -> type_var_expr cursor ctx var
  | ListE _ | RecordE _ | UnE _ | BinE _ | TernE _ ->
      Format.eprintf "(type_expr) %a\n" (Syntax.Pp.pp_expr ~level:0) expr;
      assert false
  | CastE (typ, expr) -> type_cast_expr cursor ctx typ expr
  | MaskE (expr_base, expr_mask) ->
      type_mask_expr cursor ctx expr_base expr_mask
  | RangeE (expr_lb, expr_ub) -> type_range_expr cursor ctx expr_lb expr_ub
  | SelectE (exprs_key, select_cases) ->
      type_select_expr cursor ctx exprs_key select_cases
  | ArrAccE (expr_base, expr_idx) ->
      type_array_acc_expr cursor ctx expr_base expr_idx
  | BitAccE _ | TypeAccE _ | ErrAccE _ ->
      Format.eprintf "(type_expr) %a\n" (Syntax.Pp.pp_expr ~level:0) expr;
      assert false
  | ExprAccE (expr_base, member) ->
      type_expr_acc_expr cursor ctx expr_base member
  | CallE _ | InstE _ ->
      Format.eprintf "(type_expr) %a\n" (Syntax.Pp.pp_expr ~level:0) expr;
      assert false

and type_num_expr (encoding : (Bigint.t * bool) option) : Type.t =
  match encoding with
  | Some (width, signed) ->
      if signed then Type.FIntT width else Type.FBitT width
  | None -> Type.IntT

and type_var_expr (cursor : Ctx.cursor) (ctx : Ctx.t) (var : var) : Type.t =
  let typ = Ctx.find_opt Ctx.find_type_opt cursor var ctx in
  Format.printf "Ctx:\n%a\n" Ctx.pp ctx;
  if Option.is_none typ then (
    Format.eprintf "(type_var_expr) %a is a free identifier\n" Syntax.Pp.pp_var
      var;
    assert false);
  Option.get typ

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

and type_cast_expr (cursor : Ctx.cursor) (ctx : Ctx.t) (typ : typ) (expr : expr)
    : Type.t =
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

and type_mask_expr (cursor : Ctx.cursor) (ctx : Ctx.t) (expr_base : expr)
    (expr_mask : expr) : Type.t =
  let typ_base = type_expr cursor ctx expr_base in
  let typ_mask = type_expr cursor ctx expr_mask in
  (* (TODO) Insert cast if possible *)
  (* (CHECK) Which types are allowed in mask expression? *)
  match (typ_base, typ_mask) with
  | FBitT width_base, FBitT width_mask when width_base = width_mask ->
      SetT (FBitT width_base)
  | FIntT width_base, FIntT width_mask when width_base = width_mask ->
      SetT (FIntT width_base)
  | _ ->
      Format.eprintf
        "(type_mask_expr) Incompatible types %a and %a for mask operation\n"
        Type.pp typ_base Type.pp typ_mask;
      assert false

(* (8.15.4) Ranges

   The infix operator .. takes two arguments of the same numeric type T (Section 7.4),
   and creates a value of the type set<T>. The set contains all values numerically between
   the first and the second, inclusively.

   Similar to other binary operations, the range operator allows the compiler to
   automatically insert casts to unify the argument types in certain situations (section 8.11.2). *)

and type_range_expr (cursor : Ctx.cursor) (ctx : Ctx.t) (expr_lb : expr)
    (expr_ub : expr) : Type.t =
  let typ_lb = type_expr cursor ctx expr_lb in
  let typ_ub = type_expr cursor ctx expr_ub in
  (* (TODO) Insert cast if possible *)
  (* (CHECK) Which types are allowed in range expression? *)
  match (typ_lb, typ_ub) with
  | IntT, IntT -> SetT IntT
  | FBitT width_lb, FBitT width_ub when width_lb = width_ub ->
      SetT (FBitT width_lb)
  | FIntT width_lb, FIntT width_ub when width_lb = width_ub ->
      SetT (FIntT width_lb)
  | _ ->
      Format.eprintf
        "(type_range_expr) Incompatible types %a and %a for range operation\n"
        Type.pp typ_lb Type.pp typ_ub;
      assert false

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

and type_select_match (ctx : Ctx.t) (typ_key : Type.t) (keyset : keyset) : unit
    =
  match keyset.it with
  | ExprK expr ->
      let typ =
        match expr.it with
        | MaskE _ | RangeE _ -> type_expr Ctx.Local ctx expr
        | _ ->
            let typ = type_expr Ctx.Local ctx expr in
            SetT typ
      in
      if typ_key <> typ then (
        Format.eprintf
          "(type_select_match) Key type %a must match the type of the keyset \
           expression %a\n"
          Type.pp typ_key Type.pp typ;
        assert false)
  | DefaultK | AnyK -> ()

and type_select_matches (ctx : Ctx.t) (typs_key : Type.t list)
    (keysets : keyset list) : unit =
  match (typs_key, keysets) with
  | _, [ { it = DefaultK | AnyK; _ } ] -> ()
  | typs_key, keysets ->
      if List.length typs_key <> List.length keysets then (
        Format.eprintf
          "(type_select_matches) Number of select keys must match the number \
           of cases\n";
        assert false);
      List.iter2 (type_select_match ctx) typs_key keysets

and type_select_case (ctx : Ctx.t) (typs_key : Type.t list) (case : select_case)
    : unit =
  let keysets, state_label = case.it in
  type_select_matches ctx typs_key keysets;
  let typ_label = Ctx.find_type_opt Ctx.Local state_label.it ctx in
  if not (match typ_label with Some Type.StateT -> true | _ -> false) then (
    Format.eprintf "(type_transition_stmt) Label %s is not a valid label\n"
      state_label.it;
    assert false)

and type_select_expr (cursor : Ctx.cursor) (ctx : Ctx.t) (exprs_key : expr list)
    (cases : select_case list) : Type.t =
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
        Type.SetT typ_key)
      exprs_key
  in
  List.iter (check_valid_type Ctx.Local ctx) typs_key;
  List.iter (type_select_case ctx typs_key) cases;
  Type.StateT

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

and type_array_acc_expr (cursor : Ctx.cursor) (ctx : Ctx.t) (expr_base : expr)
    (expr_idx : expr) : Type.t =
  let typ_base = type_expr cursor ctx expr_base in
  let typ_idx = type_expr cursor ctx expr_idx in
  if not (match typ_idx with IntT | FIntT _ | FBitT _ -> true | _ -> false)
  then (
    Format.eprintf "(type_array_acc_expr) Index %a must be of numeric type\n"
      (Syntax.Pp.pp_expr ~level:0)
      expr_idx;
    assert false);
  match typ_base with
  | TupleT typs_base_inner ->
      let idx =
        static_eval_expr cursor ctx expr_idx
        |> expect_value |> Value.get_num |> Bigint.to_int |> Option.get
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

and type_expr_acc_expr (cursor : Ctx.cursor) (ctx : Ctx.t) (expr_base : expr)
    (member : member) : Type.t =
  let typ_base = type_expr cursor ctx expr_base in
  match typ_base with
  | StackT (typ_inner, _) -> (
      match member.it with
      | "size" | "lastIndex" -> Type.FBitT (Bigint.of_int 32)
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

(* Statement typing *)

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

let rec type_stmt (cursor : Ctx.cursor) (ctx : Ctx.t) (stmt : stmt) : Ctx.t =
  if cursor <> Ctx.Local then (
    Format.eprintf "(type_stmt) Statements must be local\n";
    assert false);
  match stmt.it with
  | EmptyS -> ctx
  | AssignS (expr_lhs, expr_rhs) -> type_assign_stmt ctx expr_lhs expr_rhs
  | SwitchS _ -> ctx
  | IfS (expr_cond, stmt_tru, stmt_fls) ->
      type_if_stmt ctx expr_cond stmt_tru stmt_fls
  | BlockS block -> type_block_stmt ctx block
  | ExitS -> ctx
  | RetS _ -> ctx
  | CallS (expr_func, typ_args, args) ->
      type_call_stmt ctx expr_func typ_args args;
      ctx
  | TransS expr ->
      type_transition_stmt ctx expr;
      ctx
  | DeclS decl -> type_decl_stmt ctx decl

and type_stmts (cursor : Ctx.cursor) (ctx : Ctx.t) (stmts : stmt list) : Ctx.t =
  List.fold_left (type_stmt cursor) ctx stmts

(* (TODO) Consider direction also *)
and check_lvalue_type' (ctx : Ctx.t) (typ : Type.t) : bool =
  match typ with
  | DefT typ_inner | NewT typ_inner -> check_lvalue_type' ctx typ_inner
  | ExternT _ | ParserT _ | ControlT _ | PackageT | TopT -> false
  | _ -> true

and check_lvalue' (ctx : Ctx.t) (expr : expr) : bool =
  match expr.it with
  | VarE var ->
      let typ = Ctx.find_opt Ctx.find_type_opt Ctx.Local var ctx in
      if Option.is_none typ then (
        Format.eprintf "(check_lvalue) %a is a free identifier\n"
          Syntax.Pp.pp_var var;
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

and check_lvalue (ctx : Ctx.t) (expr : expr) : unit =
  if not (check_lvalue' ctx expr) then (
    Format.eprintf "(check_lvalue) %a is not an l-value\n"
      (Syntax.Pp.pp_expr ~level:0)
      expr;
    assert false)

and type_assign_stmt (ctx : Ctx.t) (expr_lhs : expr) (expr_rhs : expr) : Ctx.t =
  check_lvalue ctx expr_lhs;
  let typ_lhs = type_expr Ctx.Local ctx expr_lhs in
  let typ_rhs = type_expr Ctx.Local ctx expr_rhs in
  (* (TODO) Insert cast if possible *)
  if typ_lhs <> typ_rhs then (
    Format.eprintf
      "(type_assign_stmt) The type of left side type %a doesn't match the \
       right side type %a\n"
      Type.pp typ_lhs Type.pp typ_rhs;
    assert false);
  ctx

(* (12.6) Conditional statement

   However, the condition expression in P4 is required to be a Boolean
   (and not an integer). *)

and type_if_stmt (ctx : Ctx.t) (expr_cond : expr) (stmt_tru : stmt)
    (stmt_fls : stmt) : Ctx.t =
  let typ_cond = type_expr Ctx.Local ctx expr_cond in
  if typ_cond <> Type.BoolT then (
    Format.eprintf "(type_if_stmt) Condition must be a boolean\n";
    assert false);
  let _ctx' = type_stmt Ctx.Local ctx stmt_tru in
  let _ctx' = type_stmt Ctx.Local ctx stmt_fls in
  ctx

(* (12.3) Block statement

   It contains a sequence of statements and declarations, which are executed sequentially.
   The variables and constants within a block statement are only visible within the block. *)

and type_block_stmt (ctx : Ctx.t) (block : block) : Ctx.t =
  let ctx = Ctx.enter_frame ctx in
  let stmts, _annos = block.it in
  let ctx = type_stmts Ctx.Local ctx stmts in
  Ctx.exit_frame ctx

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

and check_call_arity (expr_func : expr)
    (params : (id' * dir' * Type.t * Value.t option) list) (args : arg' list) :
    unit =
  if List.length params <> List.length args then (
    Format.eprintf
      "(check_call_arity) Function %a expects %d arguments but %d were given\n"
      (Syntax.Pp.pp_expr ~level:0)
      expr_func (List.length params) (List.length args);
    assert false)

and check_named_args (args : arg' list) : unit =
  let is_named arg = match (arg : arg') with NameA _ -> true | _ -> false in
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
and align_params_with_args
    (params : (id' * dir' * Type.t * Value.t option) list) (args : arg' list) =
  let module PMap = Map.Make (String) in
  let params_map =
    List.fold_left
      (fun params_map param ->
        let id, _, _, _ = param in
        PMap.add id param params_map)
      PMap.empty params
  in
  List.fold_left2
    (fun (params, exprs_arg) param arg ->
      match arg with
      | ExprA expr_arg -> (params @ [ param ], exprs_arg @ [ Some expr_arg ])
      | NameA (id, expr_arg) ->
          let param = PMap.find id.it params_map in
          (params @ [ param ], exprs_arg @ [ Some expr_arg ])
      | AnyA -> (params @ [ param ], exprs_arg @ [ None ]))
    ([], []) params args

and type_call_stmt (ctx : Ctx.t) (expr_func : expr) (typ_args : typ list)
    (args : arg list) : unit =
  let args = List.map it args in
  (* Find the function definition *)
  let fd =
    match expr_func.it with
    | VarE var ->
        Ctx.find_overloaded_opt Ctx.find_funcdef_opt Ctx.Local var args ctx
    | ExprAccE (expr_base, fid) -> (
        let typ_base = type_expr Ctx.Local ctx expr_base in
        match typ_base with
        | ExternT fdenv | ParserT fdenv | ControlT fdenv ->
            FDEnv.find_opt (fid.it, args) fdenv
        | _ -> None)
    | _ -> None
  in
  if Option.is_none fd then (
    Format.eprintf "(type_call_stmt) %a is not a function expression\n"
      (Syntax.Pp.pp_expr ~level:0)
      expr_func;
    assert false);
  let fd = Option.get fd in
  (* (TODO) Implement restrictions on compile-time and run-time calls (Appendix F) *)
  (* Specialize the function definition to a function type, if necessary *)
  (* (TODO) Implement type inference *)
  let typ_args = List.map (eval_type Ctx.Local ctx) typ_args in
  let ft = specialize_funcdef fd typ_args in
  (* Check if the arguments match the parameters *)
  (* (TODO) Consider default parameters/arguments, in such case arity can appear to mismatch *)
  let params = FuncType.get_params ft in
  check_call_arity expr_func params args;
  check_named_args args;
  let params, exprs_arg = align_params_with_args params args in
  List.iter2
    (fun param expr_arg ->
      let _id_param, dir_param, typ_param, _value_default = param in
      match expr_arg with
      | Some expr_arg ->
          let typ_arg = type_expr Ctx.Local ctx expr_arg in
          (* (TODO) Consider direction of parameters/arguments *)
          (* (TODO) Check subtype instead of stric type equality,
             and insert implicit cast to argument if possible *)
          if typ_param <> typ_arg then (
            Format.eprintf "(type_call_stmt) Argument %a is not of type %a\n"
              (Syntax.Pp.pp_expr ~level:0)
              expr_arg Type.pp typ_param;
            assert false)
      | None ->
          if dir_param <> Out then (
            Format.eprintf
              "(type_call_stmt) Don't care argument can only be used for an \
               out function/method argument\n";
            assert false))
    params exprs_arg

(* (13.5) Transition statements

   The last statement in a parser state is an optional transition statement,
   which transfers control to another state, possibly accept or reject. *)

and type_transition_stmt (ctx : Ctx.t) (expr : expr) : unit =
  if not (match ctx.local.kind with Ctx.ParserState -> true | _ -> false) then (
    Format.eprintf
      "(type_transition_stmt) Transition statement must be in a parser state\n";
    assert false);
  let typ = type_expr Ctx.Local ctx expr in
  if not (match typ with StateT -> true | _ -> false) then (
    Format.eprintf "(type_transition_stmt) Label %a is not a valid label\n"
      (Syntax.Pp.pp_expr ~level:0)
      expr;
    assert false)

and type_decl_stmt (ctx : Ctx.t) (decl : decl) : Ctx.t =
  type_decl Ctx.Local ctx decl

(* Declaration typing *)

and type_decl (cursor : Ctx.cursor) (ctx : Ctx.t) (decl : decl) =
  match decl.it with
  (* Constant, variable, and object declarations *)
  | ConstD { id; typ; value; annos = _annos } ->
      type_const_decl cursor ctx id typ value
  | VarD _ ->
      Format.eprintf "(type_decl) %a\n" (Syntax.Pp.pp_decl ~level:0) decl;
      ctx
  | InstD _ ->
      Format.eprintf "(type_decl) %a\n" (Syntax.Pp.pp_decl ~level:0) decl;
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
  | ActionD _ ->
      Format.eprintf "(type_decl) %a\n" (Syntax.Pp.pp_decl ~level:0) decl;
      ctx
  | FuncD _ ->
      Format.eprintf "(type_decl) %a\n" (Syntax.Pp.pp_decl ~level:0) decl;
      ctx
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
      Format.eprintf "(type_decl) %a\n" (Syntax.Pp.pp_decl ~level:0) decl;
      ctx
  | ParserTypeD { id; tparams; params; annos = _annos } ->
      type_parser_type_decl cursor ctx id tparams params
  | ParserD { id; tparams; params; cparams; locals; states; annos = _annos } ->
      type_parser_decl cursor ctx id tparams params cparams locals states
  (* Control *)
  | TableD _ ->
      Format.eprintf "(type_decl) %a\n" (Syntax.Pp.pp_decl ~level:0) decl;
      ctx
  | ControlTypeD { id; tparams; params; annos = _annos } ->
      type_control_type_decl cursor ctx id tparams params
  | ControlD { id; tparams; params; cparams; locals; body; annos = _annos } ->
      type_control_decl cursor ctx id tparams params cparams locals body
  (* Package *)
  | PackageTypeD { id; tparams; cparams; annos = _annos } ->
      type_package_type_decl cursor ctx id tparams cparams

and type_decls (cursor : Ctx.cursor) (ctx : Ctx.t) (decls : decl list) : Ctx.t =
  List.fold_left (type_decl cursor) ctx decls

and type_const_decl (cursor : Ctx.cursor) (ctx : Ctx.t) (id : id) (typ : typ)
    (expr : expr) : Ctx.t =
  let typ_lhs = eval_type cursor ctx typ in
  let typ_rhs = type_expr cursor ctx expr in
  (* (TODO) Insert cast if possible *)
  if typ_lhs <> typ_rhs then (
    Format.eprintf
      "(type_const_decl) The type of left side type %a doesn't match the right \
       side type %a\n"
      Type.pp typ_lhs Type.pp typ_rhs;
    assert false);
  let value = static_eval_expr cursor ctx expr in
  if Option.is_none value then (
    Format.eprintf
      "(type_const_decl) %a is not a compile-time known expression\n"
      (Syntax.Pp.pp_expr ~level:0)
      expr;
    assert false);
  let value = Option.get value in
  Ctx.add_value cursor id.it value ctx |> Ctx.add_type cursor id.it typ_lhs

(* (7.1.2) The error type

   All error constants are inserted into the error namespace, irrespective of the place where an error is defined.
   error is similar to an enumeration (enum) type in other languages. A program can contain multiple error declarations,
   which the compiler will merge together. It is an error to declare the same identifier multiple times. *)

and type_error_decl (cursor : Ctx.cursor) (ctx : Ctx.t) (members : member list)
    =
  if cursor <> Ctx.Global then (
    Format.eprintf "(type_error_decl) Error declarations must be global\n";
    assert false);
  let type_error_decl' (ctx : Ctx.t) (member : member) : Ctx.t =
    let id = "error." ^ member.it in
    if Ctx.find_value_opt cursor id ctx |> Option.is_some then (
      Format.eprintf "(type_error_decl_glob) Error %s was already defined\n" id;
      assert false);
    let value = Value.ErrV member.it in
    let typ = Type.ErrT in
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
    (members : member list) : Ctx.t =
  if cursor <> Ctx.Global then (
    Format.eprintf
      "(type_match_kind_decl) Match kind declarations must be global\n";
    assert false);
  let type_match_kind_decl' (ctx : Ctx.t) (member : member) : Ctx.t =
    let id = member.it in
    if Ctx.find_value_opt cursor id ctx |> Option.is_some then (
      Format.eprintf
        "(type_match_kind_decl) Match kind %s was already defined\n" id;
      assert false);
    let value = Value.MatchKindV member.it in
    let typ = Type.MatchKindT in
    Ctx.add_value cursor id value ctx |> Ctx.add_type cursor id typ
  in
  List.fold_left type_match_kind_decl' ctx members

(* (7.2.5) Struct types

   This declaration introduces a new type with the specified name in the current scope.
   Field names have to be distinct. An empty struct (with no fields) is legal. *)

and type_struct_decl (cursor : Ctx.cursor) (ctx : Ctx.t) (id : id)
    (fields : (member * typ * anno list) list) : Ctx.t =
  if cursor <> Ctx.Global then (
    Format.eprintf "(type_struct_decl) Struct declarations must be global\n";
    assert false);
  let members, typs =
    List.map (fun (member, typ, _annos) -> (member, typ)) fields |> List.split
  in
  let members = List.map it members in
  let typs = List.map (eval_type cursor ctx) typs in
  let fields = List.combine members typs in
  let td = TypeDef.StructD fields in
  check_valid_typedef cursor ctx td;
  Ctx.add_typedef cursor id.it td ctx

(* (7.2.2) Header types *)

and type_header_decl (cursor : Ctx.cursor) (ctx : Ctx.t) (id : id)
    (fields : (member * typ * anno list) list) : Ctx.t =
  if cursor <> Ctx.Global then (
    Format.eprintf "(type_header_decl) Header declarations must be global\n";
    assert false);
  let members, typs =
    List.map (fun (member, typ, _annos) -> (member, typ)) fields |> List.split
  in
  let members = List.map it members in
  let typs = List.map (eval_type cursor ctx) typs in
  let fields = List.combine members typs in
  let td = TypeDef.HeaderD fields in
  check_valid_typedef cursor ctx td;
  Ctx.add_typedef cursor id.it td ctx

(* (7.2.4) Header unions *)

and type_union_decl (cursor : Ctx.cursor) (ctx : Ctx.t) (id : id)
    (fields : (member * typ * anno list) list) : Ctx.t =
  if cursor <> Ctx.Global then (
    Format.eprintf "(type_union_decl) Union declarations must be global\n";
    assert false);
  let members, typs =
    List.map (fun (member, typ, _annos) -> (member, typ)) fields |> List.split
  in
  let members = List.map it members in
  let typs = List.map (eval_type cursor ctx) typs in
  let fields = List.combine members typs in
  let td = TypeDef.UnionD fields in
  check_valid_typedef cursor ctx td;
  Ctx.add_typedef cursor id.it td ctx

(* (7.2.1) Enumeration types

   An enum declaration introduces a new identifier in the current scope for
   naming the created type along with its distinct constants. *)

and type_enum_decl (cursor : Ctx.cursor) (ctx : Ctx.t) (id : id)
    (members : member list) : Ctx.t =
  if cursor <> Ctx.Global then (
    Format.eprintf "(type_enum_decl) Enum declarations must be global\n";
    assert false);
  let members = List.map it members in
  let td = TypeDef.EnumD (id.it, members) in
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

and type_senum_decl (cursor : Ctx.cursor) (ctx : Ctx.t) (id : id) (typ : typ)
    (fields : (member * expr) list) : Ctx.t =
  if cursor <> Ctx.Global then (
    Format.eprintf
      "(type_senum_decl) Serializable enum declarations must be global\n";
    assert false);
  let typ = eval_type cursor ctx typ in
  let members, exprs = List.split fields in
  let members = List.map it members in
  (* (TODO) Check that values are of typ *)
  let values = static_eval_exprs cursor ctx exprs |> expect_values in
  let fields = List.combine members values in
  let td = TypeDef.SEnumD (id.it, typ, fields) in
  check_valid_typedef cursor ctx td;
  Ctx.add_typedef cursor id.it td ctx

(* (7.6) Introducing new types

   Similarly to typedef, the keyword type can be used to introduce a new type.
   While similar to typedef, the type keyword introduces a new type which is not a synonym with the original type:
   values of the original type and the newly introduced type cannot be mixed in expressions.
   Currently the types that can be created by the type keyword are restricted to one of:
   bit<>, int<>, bool, or types defined using type from such types. *)

and type_newtype_decl (cursor : Ctx.cursor) (ctx : Ctx.t) (id : id)
    (typdef : (typ, decl) alt) : Ctx.t =
  if cursor <> Ctx.Global then (
    Format.eprintf "(type_newtype_decl) New type declarations must be global\n";
    assert false);
  match typdef with
  | Left typ ->
      let typ = eval_type cursor ctx typ in
      let td = TypeDef.NewD typ in
      check_valid_typedef cursor ctx td;
      Ctx.add_typedef cursor id.it td ctx
  | Right _ -> failwith "(TODO: type_newtype_decl) Handle newtype with decl"

(* (7.5) typedef

   A typedef declaration can be used to give an alternative name to a type.
   The two types are treated as synonyms, and all operations that can be executed using
   the original type can be also executed using the newly created type.
   If typedef is used with a generic type the type must be specialized with the suitable number of type arguments: *)

and type_typedef_decl (cursor : Ctx.cursor) (ctx : Ctx.t) (id : id)
    (typdef : (typ, decl) alt) : Ctx.t =
  if cursor <> Ctx.Global then (
    Format.eprintf "(type_typedef_decl) Typedef declarations must be global\n";
    assert false);
  match typdef with
  | Left typ ->
      let typ = eval_type cursor ctx typ in
      let td = TypeDef.DefD typ in
      check_valid_typedef cursor ctx td;
      Ctx.add_typedef cursor id.it td ctx
  | Right _ -> failwith "(TODO: type_typedef_decl) Handle typedef with decl"

(* (7.2.10.1) Extern functions

   An extern function declaration describes the name and type signature
   of the function, but not its implementation. *)

and type_extern_function_decl (cursor : Ctx.cursor) (ctx : Ctx.t) (id : id)
    (tparams : tparam list) (params : param list) (typ_ret : typ) : Ctx.t =
  if cursor <> Ctx.Global then (
    Format.eprintf
      "(type_extern_function_decl) Extern function declarations must be global\n";
    assert false);
  let tparams = List.map it tparams in
  let params = List.map it params in
  let fid = Runtime.Domain.FId.to_fid id.it params in
  let ctx' = Ctx.set_id Ctx.Local id.it ctx in
  let ctx' = Ctx.set_localkind Ctx.ExternFunction ctx' in
  let ctx' = Ctx.add_tparams Ctx.Local tparams ctx' in
  let params = List.map (static_eval_param Ctx.Local ctx') params in
  let typ_ret = eval_type Ctx.Local ctx' typ_ret in
  let fd = FuncDef.ExternFunctionD (tparams, params, typ_ret) in
  check_valid_funcdef cursor ctx fd;
  Ctx.add_funcdef cursor fid fd ctx

(* (7.2.12) Parser and control blocks types

   Parsers and control blocks types are similar to function types: they describe the signature of parsers and control blocks.
   Such functions have no return values. Declarations of parsers and control block types in architectures may be generic
   (i.e., have type parameters).

   (7.2.12.1) Parser type declarations

   A parser should have at least one argument of type packet_in, representing the received packet that is processed. *)

and type_parser_type_apply_method_decl (cursor : Ctx.cursor) (ctx : Ctx.t)
    (params : param' list) : Ctx.t =
  if not (cursor = Ctx.Block && ctx.block.kind = Ctx.Parser) then (
    Format.eprintf
      "(type_parser_apply_method_decl) Parser apply method declarations must \
       be in a parser block\n";
    assert false);
  let fid = Runtime.Domain.FId.to_fid "apply" params in
  let ctx' = Ctx.set_id Ctx.Local "apply" ctx in
  let ctx' = Ctx.set_localkind Ctx.ParserMethod ctx' in
  let params = List.map (static_eval_param Ctx.Local ctx') params in
  let fd = FuncDef.ParserMethodD params in
  check_valid_funcdef Ctx.Block ctx fd;
  Ctx.add_funcdef Ctx.Block fid fd ctx

and type_parser_type_decl (cursor : Ctx.cursor) (ctx : Ctx.t) (id : id)
    (tparams : tparam list) (params : param list) : Ctx.t =
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
  let ctx' = type_parser_type_apply_method_decl Ctx.Block ctx' params in
  (* Create a parser type definition
     and add it to the context *)
  let td = TypeDef.ParserD (tparams, ctx'.block.fdenv) in
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

(* (NOTE) A different view on parser declaration

   parser id (params) (cparams) {
     locals <-- can be initialized with params and cparams (e.g. bit<8> l = p; )
     states
   }

   "apply" is an implicit method that is a single entry point for the parser.
   Conceptually, the parser declaration above is equivalent to:

   parser id (cparams) {
     locals <-- not initialized yet, only declared (e.g. bit<8> l; )
     "apply" (params) {
       locals are initialized with params and cparams (e.g. l = p; )
       transition start;
       states
      }
   } *)

and type_parser_local_decls (cursor : Ctx.cursor) (ctx : Ctx.t)
    (locals : decl list) : Ctx.t * stmt list =
  if not (cursor = Ctx.Block && ctx.block.kind = Ctx.Parser) then (
    Format.eprintf
      "(type_parser_local_decls) Parser local declarations must be in a parser \
       block\n";
    assert false);
  let decls_var, decls =
    List.partition_map
      (fun local ->
        match local.it with
        | VarD { id; typ; init; annos } ->
            Either.Left (id, typ, init, annos, local.at)
        | _ -> Either.Right local)
      locals
  in
  let decls_var, stmts_var_init =
    List.map
      (fun (id, typ, init, annos, at) ->
        let decl_var = VarD { id; typ; init = None; annos } $ at in
        let stmt_var_init =
          Option.map
            (fun expr -> AssignS (VarE (Current id $ id.at) $ id.at, expr) $ at)
            init
        in
        (decl_var, stmt_var_init))
      decls_var
    |> List.split
  in
  let stmts_var_init = List.filter_map (fun stmt -> stmt) stmts_var_init in
  let decls = decls @ decls_var in
  let ctx = type_decls Ctx.Block ctx decls in
  (ctx, stmts_var_init)

and type_parser_state (cursor : Ctx.cursor) (ctx : Ctx.t) (block : block) :
    Ctx.t =
  if
    not
      (cursor = Ctx.Local
      && ctx.block.kind = Ctx.Parser
      && match ctx.local.kind with Ctx.ParserState -> true | _ -> false)
  then (
    Format.eprintf "(type_parser_state) Parser state must be local\n";
    assert false);
  let stmts, _annos = block.it in
  type_stmts Ctx.Local ctx stmts

and type_parser_states (cursor : Ctx.cursor) (ctx : Ctx.t)
    (states : parser_state list) : Ctx.t =
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
      (fun ctx' label -> Ctx.add_type Ctx.Local label Type.StateT ctx')
      ctx' labels
  in
  let _ctx' =
    List.fold_left
      (fun ctx' (label, block, _annos) ->
        let ctx' = Ctx.set_id Ctx.Local label.it ctx' in
        type_parser_state cursor ctx' block)
      ctx' states
  in
  ctx

and type_parser_decl (cursor : Ctx.cursor) (ctx : Ctx.t) (id : id)
    (tparams : tparam list) (params : param list) (cparams : cparam list)
    (locals : decl list) (states : parser_state list) : Ctx.t =
  if cursor <> Ctx.Global then (
    Format.eprintf "(type_parser_decl) Parser declarations must be global\n";
    assert false);
  if tparams <> [] then (
    Format.eprintf "(type_parser_decl) Parser declarations cannot be generic\n";
    assert false);
  let params = List.map it params in
  let cparams = List.map it cparams in
  let fid = Runtime.Domain.FId.to_fid "apply" params in
  let cid = Runtime.Domain.FId.to_fid id.it cparams in
  (* Typecheck and add constructor parameters to the block context *)
  let ctx' = Ctx.set_id Ctx.Block id.it ctx in
  let ctx' = Ctx.set_blockkind Ctx.Parser ctx' in
  let cparams = List.map (static_eval_param Ctx.Block ctx') cparams in
  let ctx' =
    List.fold_left
      (fun ctx' cparam ->
        let id, _, typ, _ = cparam in
        Ctx.add_type Ctx.Block id typ ctx')
      ctx' cparams
  in
  (* Typecheck and add local declarations to the block context *)
  (* According to (NOTE) above, locals are declared but not initialized in the block cursor *)
  let ctx', stmts_var_init = type_parser_local_decls Ctx.Block ctx' locals in
  (* Typecheck implicit "apply" method *)
  (* Typecheck and add apply parameters to the local context *)
  let ctx' = Ctx.set_id Ctx.Local "apply" ctx' in
  let ctx' = Ctx.set_localkind Ctx.ParserMethod ctx' in
  let params = List.map (static_eval_param Ctx.Local ctx') params in
  let ctx' =
    List.fold_left
      (fun ctx' param ->
        let id, _, typ, _ = param in
        Ctx.add_type Ctx.Local id typ ctx')
      ctx' params
  in
  let ctx' = type_stmts Ctx.Local ctx' stmts_var_init in
  (* Typecheck parser states *)
  let _ctx' = type_parser_states Ctx.Local ctx' states in
  (* Create a parser constructor definition *)
  let fd = FuncDef.ParserMethodD params in
  let fdenv = FDEnv.add fid fd FDEnv.empty in
  let typ = Type.ParserT fdenv in
  let cd = ConsDef.{ tparams = []; cparams; typ } in
  Ctx.add_consdef cid cd ctx

(* (7.2.12.2) Control type declarations *)

and type_control_type_apply_method_decl (cursor : Ctx.cursor) (ctx : Ctx.t)
    (params : param' list) : Ctx.t =
  if not (cursor = Ctx.Block && ctx.block.kind = Ctx.Control) then (
    Format.eprintf
      "(type_control_apply_method_decl) Control apply method declarations must \
       be in a block\n";
    assert false);
  let fid = Runtime.Domain.FId.to_fid "apply" params in
  let ctx' = Ctx.set_id Ctx.Local "apply" ctx in
  let ctx' = Ctx.set_localkind Ctx.ControlMethod ctx' in
  let params = List.map (static_eval_param Ctx.Local ctx') params in
  let fd = FuncDef.ControlMethodD params in
  check_valid_funcdef Ctx.Block ctx fd;
  Ctx.add_funcdef cursor fid fd ctx

and type_control_type_decl (cursor : Ctx.cursor) (ctx : Ctx.t) (id : id)
    (tparams : tparam list) (params : param list) : Ctx.t =
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
  let ctx' = type_control_type_apply_method_decl Ctx.Block ctx' params in
  (* Create a control type definition
     and add it to the context *)
  let td = TypeDef.ControlD (tparams, ctx'.block.fdenv) in
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

(* (NOTE) A different view on control declaration

   control id (params) (cparams) {
     locals <-- can be initialized with params and cparams (e.g. bit<8> l = p; )
     block
   }

   "apply" is an implicit method that is a single entry point for the control.
   Conceptually, the control declaration above is equivalent to:

   control id (cparams) {
     locals <-- not initialized yet, only declared (e.g. bit<8> l; )
     "apply" (params) {
       locals are initialized with params and cparams (e.g. l = p; )
      }
   } *)

and type_control_local_decls (cursor : Ctx.cursor) (ctx : Ctx.t)
    (locals : decl list) : Ctx.t * stmt list =
  if not (cursor = Ctx.Block && ctx.block.kind = Ctx.Control) then (
    Format.eprintf
      "(type_control_local_decls) Control local declarations must be in a \
       control block\n";
    assert false);
  let decls_var, decls =
    List.partition_map
      (fun local ->
        match local.it with
        | VarD { id; typ; init; annos } ->
            Either.Left (id, typ, init, annos, local.at)
        | _ -> Either.Right local)
      locals
  in
  let decls_var, stmts_var_init =
    List.map
      (fun (id, typ, init, annos, at) ->
        let decl_var = VarD { id; typ; init = None; annos } $ at in
        let stmt_var_init =
          Option.map
            (fun expr -> AssignS (VarE (Current id $ id.at) $ id.at, expr) $ at)
            init
        in
        (decl_var, stmt_var_init))
      decls_var
    |> List.split
  in
  let stmts_var_init = List.filter_map (fun stmt -> stmt) stmts_var_init in
  let decls = decls @ decls_var in
  let ctx = type_decls Ctx.Block ctx decls in
  (ctx, stmts_var_init)

and type_control_decl (cursor : Ctx.cursor) (ctx : Ctx.t) (id : id)
    (tparams : tparam list) (params : param list) (cparams : cparam list)
    (locals : decl list) (body : block) : Ctx.t =
  if cursor <> Ctx.Global then (
    Format.eprintf "(type_control_decl) Control declarations must be global\n";
    assert false);
  if tparams <> [] then (
    Format.eprintf
      "(type_control_decl) Control declarations cannot be generic\n";
    assert false);
  let params = List.map it params in
  let cparams = List.map it cparams in
  let fid = Runtime.Domain.FId.to_fid "apply" params in
  let cid = Runtime.Domain.FId.to_fid id.it cparams in
  (* Typecheck and add constructor parameters to the block context *)
  let ctx' = Ctx.set_id Ctx.Block id.it ctx in
  let ctx' = Ctx.set_blockkind Ctx.Control ctx' in
  let cparams = List.map (static_eval_param Ctx.Block ctx') cparams in
  let ctx' =
    List.fold_left
      (fun ctx' cparam ->
        let id, _, typ, _ = cparam in
        Ctx.add_type Ctx.Block id typ ctx')
      ctx' cparams
  in
  (* Typecheck and add local declarations to the block context *)
  (* According to (NOTE) above, locals are declared but not initialized in the block cursor *)
  let ctx', stmts_var_init = type_control_local_decls Ctx.Block ctx' locals in
  (* Typecheck implicit "apply" method *)
  (* Typecheck and add apply parameters to the local context *)
  let ctx' = Ctx.set_id Ctx.Local "apply" ctx' in
  let ctx' = Ctx.set_localkind Ctx.ControlMethod ctx' in
  let params = List.map (static_eval_param Ctx.Local ctx') params in
  let ctx' =
    List.fold_left
      (fun ctx' param ->
        let id, _, typ, _ = param in
        Ctx.add_type Ctx.Local id typ ctx')
      ctx' params
  in
  let stmts = stmts_var_init @ [ BlockS body $ no_info ] in
  let _ctx' = type_stmts Ctx.Local ctx' stmts in
  (* Create a control constructor definition *)
  let fd = FuncDef.ControlMethodD params in
  let fdenv = FDEnv.add fid fd FDEnv.empty in
  let typ = Type.ControlT fdenv in
  let cd = ConsDef.{ tparams = []; cparams; typ } in
  Ctx.add_consdef cid cd ctx

(* (7.2.13) Package types

   All parameters of a package are evaluated at compilation time, and in consequence they must all be directionless
   (they cannot be in, out, or inout). Otherwise package types are very similar to parser type declarations. *)

and type_package_constructor_decl (cursor : Ctx.cursor) (ctx : Ctx.t) (id : id)
    (cparams : cparam list) : Ctx.t =
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
  let cid = Runtime.Domain.FId.to_fid id.it cparams in
  let cparams = List.map (static_eval_param Ctx.Block ctx) cparams in
  let td = Ctx.find_typedef Ctx.Global id.it ctx in
  let typ_args = List.map (fun tparam -> Type.VarT tparam) tparams in
  let typ = specialize_typedef td typ_args in
  let cd = ConsDef.{ tparams; cparams; typ } in
  check_valid_consdef cursor ctx cd;
  Ctx.add_consdef cid cd ctx

and type_package_type_decl (cursor : Ctx.cursor) (ctx : Ctx.t) (id : id)
    (tparams : tparam list) (cparams : cparam list) : Ctx.t =
  if cursor <> Ctx.Global then (
    Format.eprintf
      "(type_package_type_decl) Package type declarations must be global\n";
    assert false);
  let tparams = List.map it tparams in
  (* Create a package type definition
     and add it to the context *)
  let td = TypeDef.PackageD tparams in
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

and type_extern_constructor_decl (cursor : Ctx.cursor) (ctx : Ctx.t) (id : id)
    (cparams : cparam list) : Ctx.t =
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
  let cid = Runtime.Domain.FId.to_fid id.it cparams in
  let cparams = List.map (static_eval_param cursor ctx) cparams in
  let td = Ctx.find_typedef Ctx.Global id.it ctx in
  let typ_args = List.map (fun tparam -> Type.VarT tparam) tparams in
  let typ = specialize_typedef td typ_args in
  let cd = ConsDef.{ tparams; cparams; typ } in
  check_valid_consdef cursor ctx cd;
  Ctx.add_consdef cid cd ctx

(* (7.2.10.2) Extern objects - Abstract methods

   However, some types of extern objects may provide methods that can be implemented by the P4 programmers.
   Such methods are described with the abstract keyword prior to the method definition.
   When such an object is instantiated the user has to supply an implementation of all the abstract methods. *)

and type_extern_abstract_method_decl (cursor : Ctx.cursor) (ctx : Ctx.t)
    (id : id) (tparams : tparam list) (params : param list) (typ_ret : typ) :
    Ctx.t =
  if not (cursor = Ctx.Block && ctx.block.kind = Ctx.Extern) then (
    Format.eprintf
      "(type_extern_abstract_method_decl) Extern method declarations must be \
       in an extern block\n";
    assert false);
  let tparams = List.map it tparams in
  let params = List.map it params in
  let fid = Runtime.Domain.FId.to_fid id.it params in
  let ctx' = Ctx.set_id Ctx.Local id.it ctx in
  let ctx' = Ctx.add_tparams Ctx.Local tparams ctx' in
  let params = List.map (static_eval_param Ctx.Local ctx') params in
  let typ_ret = eval_type Ctx.Local ctx' typ_ret in
  let fd = FuncDef.ExternAbstractMethodD (tparams, params, typ_ret) in
  check_valid_funcdef cursor ctx fd;
  Ctx.add_funcdef cursor fid fd ctx

and type_extern_method_decl (cursor : Ctx.cursor) (ctx : Ctx.t) (id : id)
    (tparams : tparam list) (params : param list) (typ_ret : typ) : Ctx.t =
  if not (cursor = Ctx.Block && ctx.block.kind = Ctx.Extern) then (
    Format.eprintf
      "(type_extern_method_decl) Extern method declarations must be in an \
       extern block\n";
    assert false);
  let tparams = List.map it tparams in
  let params = List.map it params in
  let fid = Runtime.Domain.FId.to_fid id.it params in
  let ctx' = Ctx.set_id Ctx.Local id.it ctx in
  let ctx' = Ctx.set_localkind Ctx.ExternMethod ctx' in
  let ctx' = Ctx.add_tparams Ctx.Local tparams ctx' in
  let params = List.map (static_eval_param Ctx.Local ctx') params in
  let typ_ret = eval_type Ctx.Local ctx' typ_ret in
  let fd = FuncDef.ExternMethodD (tparams, params, typ_ret) in
  check_valid_funcdef cursor ctx fd;
  Ctx.add_funcdef cursor fid fd ctx

and type_extern_object_decl (cursor : Ctx.cursor) (ctx : Ctx.t) (id : id)
    (tparams : tparam list) (mthds : decl list) : Ctx.t =
  if cursor <> Ctx.Global then (
    Format.eprintf
      "(type_extern_object_decl) Extern object declarations must be global\n";
    assert false);
  let cons, mthds =
    List.partition
      (fun mthd ->
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
  let td = TypeDef.ExternD (tparams, ctx'.block.fdenv) in
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

let type_program (program : program) = type_decls Ctx.Global Ctx.empty program
