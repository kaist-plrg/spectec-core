open Syntax.Ast
open Util.Source
module R = Runtime

(* Utils *)

let expect_value (value : R.Value.t option) : R.Value.t =
  match value with Some value -> value | None -> assert false

let expect_values (values : R.Value.t list option) : R.Value.t list =
  match values with Some values -> values | None -> assert false

(* Checkers for well-formedness *)

(* Eliminate all type references and replace them with the type they refer to.
   Warning: this will loop forever if there is a cycle in the type references. *)

let rec saturate_type (ctx : Ctx.t) (typ : R.Type.t) : R.Type.t =
  match typ with
  | VoidT | BoolT | AIntT | IntT _ | BitT _ | VBitT _ | StrT | ErrT | MatchKindT
    ->
      typ
  | NameT id -> Ctx.find_td id ctx |> saturate_type ctx
  | NewT _ -> typ
  | StackT (typ, size) -> StackT (saturate_type ctx typ, size)
  | TupleT typs -> TupleT (List.map (saturate_type ctx) typs)
  | StructT fields ->
      let members, typs = List.split fields in
      let typs = List.map (saturate_type ctx) typs in
      StructT (List.combine members typs)
  | HeaderT fields ->
      let members, typs = List.split fields in
      let typs = List.map (saturate_type ctx) typs in
      HeaderT (List.combine members typs)
  | UnionT fields ->
      let members, typs = List.split fields in
      let typs = List.map (saturate_type ctx) typs in
      UnionT (List.combine members typs)
  | EnumT _ -> typ
  | SEnumT (id, typ, fields) ->
      let typ = saturate_type ctx typ in
      SEnumT (id, typ, fields)
  | RefT -> typ

let check_distinct_members (members : member' list) : unit =
  let distinct =
    List.fold_left
      (fun (distinct, seen) member ->
        if not distinct then (distinct, seen)
        else if List.mem member seen then (false, seen)
        else (distinct, member :: seen))
      (true, []) members
    |> fst
  in
  if not distinct then (
    Format.eprintf "(check_distinct_members) Members are not distinct\n";
    assert false)
  else ()

(* (7.2.8)
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

(* (7.2.1)
   (???) if we consider serializable enum as nested type

   for each symbolic entry in the enumeration. The symbol typeRef in the grammar above must be one of the following types:

    - an unsigned integer, i.e. bit<W> for some compile-time known W.
    - a signed integer, i.e. int<W> for some compile-time known W.
    - a type name declared via typedef, where the base type of that type is either one of the types listed above,
      or another typedef name that meets these conditions. *)

let check_valid_alias' (_ctx : Ctx.t) ~(is_newtype : bool) (typ : R.Type.t) :
    bool =
  if is_newtype then
    match typ with BitT _ | IntT _ | BoolT -> true | _ -> false
  else
    match typ with
    | BitT _ | IntT _ | VBitT _ | AIntT | ErrT | BoolT | EnumT _ | SEnumT _
    | HeaderT _ | StackT _ | UnionT _ | StructT _ | TupleT _ ->
        true
    | _ -> false

let check_valid_alias (ctx : Ctx.t) ~(is_newtype : bool) (typ : R.Type.t) : unit
    =
  if not (check_valid_alias' ctx ~is_newtype typ) then (
    Format.eprintf "(check_valid_alias) Type %a is not a valid %s alias\n"
      R.Type.pp typ
      (if is_newtype then "newtype" else "typedef");
    assert false)
  else ()

let check_valid_nesting' ?(in_header = false) (_ctx : Ctx.t)
    (typ_outer : R.Type.t) (typ_inner : R.Type.t) : bool =
  match typ_outer with
  | HeaderT _ -> (
      match typ_inner with
      | BitT _ | IntT _ | VBitT _ | BoolT | SEnumT _ | StructT _ -> true
      | _ -> false)
  | UnionT _ -> ( match typ_inner with HeaderT _ -> true | _ -> false)
  | StructT _ when in_header -> (
      match typ_inner with BitT _ | IntT _ | BoolT -> true | _ -> false)
  | StructT _ when not in_header -> (
      match typ_inner with
      | BitT _ | IntT _ | VBitT _ | ErrT | BoolT | EnumT _ | SEnumT _
      | HeaderT _ | StackT _ | UnionT _ | StructT _ | TupleT _ ->
          true
      | _ -> false)
  | TupleT _ -> (
      match typ_inner with
      | BitT _ | IntT _ | VBitT _ | ErrT | BoolT | EnumT _ | SEnumT _
      | HeaderT _ | StackT _ | UnionT _ | StructT _ | TupleT _ ->
          true
      | _ -> false)
  | StackT _ -> (
      match typ_inner with HeaderT _ | UnionT _ -> true | _ -> false)
  | SEnumT _ -> ( match typ_inner with BitT _ | IntT _ -> true | _ -> false)
  | _ -> false

let check_valid_nesting ?(in_header = false) (ctx : Ctx.t)
    (typ_outer : R.Type.t) (typ_inner : R.Type.t) : unit =
  if not (check_valid_nesting' ctx typ_outer typ_inner ~in_header) then (
    Format.eprintf "(check_valid_nesting) Nested types are not well-formed\n";
    assert false)
  else ()

let rec check_well_formed ?(in_header = false) (ctx : Ctx.t) (typ : R.Type.t) :
    unit =
  match saturate_type ctx typ with
  | VoidT | BoolT | AIntT | IntT _ | BitT _ | VBitT _ | StrT | ErrT | MatchKindT
    ->
      ()
  | NameT id | NewT id -> (
      match Ctx.find_td_opt id ctx with
      | None -> failwith "(check_well_formed) Unbound type reference"
      | Some _ -> ())
  | StackT (typ_inner, _) ->
      check_well_formed ctx typ_inner ~in_header;
      check_valid_nesting ctx typ typ_inner ~in_header
  | TupleT typs -> List.iter (check_well_formed ctx ~in_header) typs
  | StructT fields ->
      let members, typs_inner = List.split fields in
      check_distinct_members members;
      List.iter
        (fun typ_inner ->
          check_well_formed ctx typ_inner ~in_header;
          check_valid_nesting ctx typ typ_inner ~in_header)
        typs_inner
  | HeaderT fields ->
      let members, typs_inner = List.split fields in
      check_distinct_members members;
      List.iter
        (fun typ_inner ->
          check_well_formed ctx typ_inner ~in_header:true;
          check_valid_nesting ctx typ typ_inner ~in_header:true)
        typs_inner
  | UnionT fields ->
      let members, typs_inner = List.split fields in
      check_distinct_members members;
      List.iter
        (fun typ_inner ->
          check_well_formed ctx typ_inner ~in_header;
          check_valid_nesting ctx typ typ_inner ~in_header)
        typs_inner
  | EnumT (_, members) -> check_distinct_members members
  | SEnumT (_, typ_inner, fields) ->
      let members, _ = List.split fields in
      check_distinct_members members;
      check_valid_nesting ctx typ typ_inner ~in_header
  | RefT -> ()

(* Static type and expression evaluation *)

let rec static_eval_type (ctx : Ctx.t) (typ : typ) : R.Type.t =
  match typ.it with
  | VoidT -> R.Type.VoidT
  | BoolT -> R.Type.BoolT
  | ErrT -> R.Type.ErrT
  | StrT -> R.Type.StrT
  | AIntT -> R.Type.AIntT
  | IntT expr ->
      let width =
        static_eval_expr ctx expr |> expect_value |> R.Value.get_num
      in
      R.Type.IntT width
  | BitT expr ->
      let width =
        static_eval_expr ctx expr |> expect_value |> R.Value.get_num
      in
      R.Type.BitT width
  | VBitT expr ->
      let width =
        static_eval_expr ctx expr |> expect_value |> R.Value.get_num
      in
      R.Type.VBitT width
  | NameT var -> (
      match var.it with
      | Top id -> Ctx.find_td_glob id.it ctx
      | Bare id -> Ctx.find_td id.it ctx)
  | SpecT (_var, _typs) -> failwith "(TODO: static_eval_type) Handle spec type"
  | StackT (typ, expr) ->
      let typ = static_eval_type ctx typ in
      let size = static_eval_expr ctx expr |> expect_value |> R.Value.get_num in
      R.Type.StackT (typ, size)
  | TupleT typs ->
      let typs = List.map (static_eval_type ctx) typs in
      R.Type.TupleT typs
  | AnyT -> failwith "(TODO: static_eval_type) Handle any type"

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

and static_eval_expr (ctx : Ctx.t) (expr : expr) : R.Value.t option =
  match expr.it with
  | BoolE b -> static_eval_bool b
  | StrE s -> static_eval_str s
  | NumE { it = value, encoding; _ } -> static_eval_num value encoding
  | VarE var -> static_eval_var ctx var
  | ListE exprs -> static_eval_list ctx exprs
  | RecordE fields -> static_eval_record ctx fields
  | UnE (unop, expr) -> static_eval_unop ctx unop expr
  | BinE (binop, expr_fst, expr_snd) ->
      static_eval_binop ctx binop expr_fst expr_snd
  | TernE (expr_cond, expr_tru, expr_fls) ->
      static_eval_ternop ctx expr_cond expr_tru expr_fls
  | CastE (typ, expr) -> static_eval_cast ctx typ expr
  | BitAccE (expr_base, expr_lo, expr_hi) ->
      static_eval_bitstring_acc ctx expr_base expr_lo expr_hi
  | TypeAccE (var, member) -> static_eval_type_acc ctx var member
  | ErrAccE member -> static_eval_error_acc ctx member
  | ExprAccE (expr_base, member) -> static_eval_expr_acc ctx expr_base member
  | CallE (expr_func, targs, args) -> static_eval_call ctx expr_func targs args
  | _ -> None

and static_eval_bool (b : bool) : R.Value.t option = Some (BoolV b)
and static_eval_str (s : string) : R.Value.t option = Some (StrV s)

and static_eval_num (value : Bigint.t) (encoding : (Bigint.t * bool) option) :
    R.Value.t option =
  match encoding with
  | Some (width, signed) ->
      if signed then Some (IntV (width, value)) else Some (BitV (width, value))
  | None -> Some (AIntV value)

and static_eval_var (ctx : Ctx.t) (var : var) : R.Value.t option =
  match var.it with
  | Top id -> Ctx.find_const_glob_opt id.it ctx
  | Bare id -> Ctx.find_const_opt id.it ctx

and static_eval_list (ctx : Ctx.t) (exprs : expr list) : R.Value.t option =
  let values = static_eval_exprs ctx exprs in
  Option.map (fun values -> R.Value.TupleV values) values

and static_eval_record (ctx : Ctx.t) (fields : (member * expr) list) :
    R.Value.t option =
  let members, exprs = List.split fields in
  let members = List.map it members in
  let values = static_eval_exprs ctx exprs in
  Option.map
    (fun values -> R.Value.StructV (List.combine members values))
    values

and static_eval_unop (ctx : Ctx.t) (op : unop) (expr : expr) : R.Value.t option
    =
  let value = static_eval_expr ctx expr in
  Option.map (Runtime.Ops.eval_unop op) value

and static_eval_binop (ctx : Ctx.t) (op : binop) (expr_fst : expr)
    (expr_snd : expr) : R.Value.t option =
  let values = static_eval_exprs ctx [ expr_fst; expr_snd ] in
  Option.map
    (fun values ->
      let value_fst, value_snd = (List.nth values 0, List.nth values 1) in
      Runtime.Ops.eval_binop op value_fst value_snd)
    values

and static_eval_ternop (ctx : Ctx.t) (expr_cond : expr) (expr_tru : expr)
    (expr_fls : expr) : R.Value.t option =
  let value_cond = static_eval_expr ctx expr_cond in
  Option.bind value_cond (fun value_cond ->
      let cond = R.Value.get_bool value_cond in
      let expr = if cond then expr_tru else expr_fls in
      static_eval_expr ctx expr)

and static_eval_cast (ctx : Ctx.t) (typ : typ) (expr : expr) : R.Value.t option
    =
  let typ = static_eval_type ctx typ in
  let typ = saturate_type ctx typ in
  let value = static_eval_expr ctx expr in
  Option.map (Runtime.Ops.eval_cast typ) value

and static_eval_bitstring_acc (ctx : Ctx.t) (expr_base : expr) (expr_lo : expr)
    (expr_hi : expr) : R.Value.t option =
  let values = static_eval_exprs ctx [ expr_base; expr_hi; expr_lo ] in
  Option.map
    (fun values ->
      let value_base, value_hi, value_lo =
        (List.nth values 0, List.nth values 1, List.nth values 2)
      in
      Runtime.Ops.eval_bitstring_access value_base value_hi value_lo)
    values

and static_eval_type_acc (ctx : Ctx.t) (var : var) (member : member) :
    R.Value.t option =
  let typ =
    match var.it with
    | Top id -> Ctx.find_td_glob id.it ctx
    | Bare id -> Ctx.find_td id.it ctx
  in
  match typ with
  | EnumT (id, members) when List.mem member.it members ->
      Some (EnumFieldV (id, member.it))
  | SEnumT (id, _typ, fields) when List.mem_assoc member.it fields ->
      let value = List.assoc member.it fields in
      Some (SEnumFieldV (id, member.it, value))
  | _ -> None

and static_eval_error_acc (ctx : Ctx.t) (member : member) : R.Value.t option =
  let id = "error." ^ member.it in
  match Ctx.find_const_glob_opt id ctx with
  | Some (ErrV _ as value) -> Some value
  | _ -> None

and static_eval_expr_acc (ctx : Ctx.t) (expr_base : expr) (member : member) :
    R.Value.t option =
  let value_base = static_eval_expr ctx expr_base in
  match value_base with
  | Some value_base -> (
      match value_base with
      | StructV fields when List.mem_assoc member.it fields ->
          Some (List.assoc member.it fields)
      | StackV (_, _, size) when member.it = "size" -> Some (AIntV size)
      | _ -> None)
  | _ -> None

and static_eval_call (_ctx : Ctx.t) (_expr_func : expr) (_targs : typ list)
    (_args : arg list) : R.Value.t option =
  failwith "(TODO: static_eval_call) Handle static function call"

and static_eval_exprs (ctx : Ctx.t) (exprs : expr list) : R.Value.t list option
    =
  let values = List.map (static_eval_expr ctx) exprs in
  if
    List.for_all Option.is_some values && List.length exprs = List.length values
  then Some (List.map Option.get values)
  else None

(* Type casting *)

let cast_expr (_ctx : Ctx.t) (_typ : R.Type.t) (expr : expr) : expr =
  Format.eprintf "(TODO: cast_expr) Handle type cast insertion\n";
  expr

(* Type checking *)

let type_constant_decl_glob (ctx : Ctx.t) (id : id) (typ : typ) (value : expr) :
    Ctx.t =
  let typ = static_eval_type ctx typ in
  let value = cast_expr ctx typ value in
  let value = static_eval_expr ctx value |> expect_value in
  (* (TODO) AST transformation: replace expression with the value *)
  let ctx = Ctx.add_const_glob id.it value ctx in
  let ctx = Ctx.add_type_glob id.it typ ctx in
  ctx

(* (7.1.2)
   All error constants are inserted into the error namespace, irrespective of the place where an error is defined.
   error is similar to an enumeration (enum) type in other languages. A program can contain multiple error declarations,
   which the compiler will merge together. It is an error to declare the same identifier multiple times. *)

let type_error_decl_glob (ctx : Ctx.t) (members : member list) : Ctx.t =
  let type_error_decl_glob' (ctx : Ctx.t) (member : member) : Ctx.t =
    let id = "error." ^ member.it in
    if Ctx.find_const_glob_opt id ctx |> Option.is_some then (
      Format.eprintf "(type_error_decl_glob) Error %s was already defined\n" id;
      assert false);
    let value = R.Value.ErrV member.it in
    let typ = R.Type.ErrT in
    let ctx = Ctx.add_const_glob id value ctx in
    let ctx = Ctx.add_type_glob id typ ctx in
    ctx
  in
  List.fold_left type_error_decl_glob' ctx members

(* (7.1.3)
   The match_kind type is very similar to the error type and is used to declare a set of distinct names
   that may be used in a table's key property (described in Section 14.2.1).
   All identifiers are inserted into the top-level namespace.
   It is an error to declare the same match_kind identifier multiple times.

   (TODO) Can the type system enforce the following constraint?

   The declaration of new match_kinds can only occur within model description files;
   P4 programmers cannot declare new match kinds. *)

let type_match_kind_decl_glob (ctx : Ctx.t) (members : member list) : Ctx.t =
  let type_match_kind_decl_glob' (ctx : Ctx.t) (member : member) : Ctx.t =
    let id = member.it in
    if Ctx.find_const_glob_opt id ctx |> Option.is_some then (
      Format.eprintf
        "(type_match_kind_decl_glob) Match kind %s was already defined\n" id;
      assert false);
    let value = R.Value.MatchKindV member.it in
    let typ = R.Type.MatchKindT in
    let ctx = Ctx.add_const_glob id value ctx in
    let ctx = Ctx.add_type_glob id typ ctx in
    ctx
  in
  List.fold_left type_match_kind_decl_glob' ctx members

(* (7.2.5)
   This declaration introduces a new type with the specified name in the current scope.
   Field names have to be distinct. An empty struct (with no fields) is legal. *)

let type_struct_decl_glob (ctx : Ctx.t) (id : id) (fields : (member * typ) list)
    : Ctx.t =
  let members, typs = List.split fields in
  let members = List.map it members in
  List.iter (fun typ -> Format.eprintf "%a\n" Syntax.Pp.pp_type typ) typs;
  let typs = List.map (static_eval_type ctx) typs in
  let fields = List.combine members typs in
  let typ = R.Type.StructT fields in
  check_well_formed ctx typ;
  let ctx = Ctx.add_td_glob id.it typ ctx in
  ctx

(* (7.2.2) *)

let type_header_decl_glob (ctx : Ctx.t) (id : id) (fields : (member * typ) list)
    : Ctx.t =
  let members, typs = List.split fields in
  let members = List.map it members in
  let typs = List.map (static_eval_type ctx) typs in
  let fields = List.combine members typs in
  let typ = R.Type.HeaderT fields in
  check_well_formed ctx typ;
  let ctx = Ctx.add_td_glob id.it typ ctx in
  ctx

(* (7.2.4) *)

let type_union_decl_glob (ctx : Ctx.t) (id : id) (fields : (member * typ) list)
    : Ctx.t =
  let members, typs = List.split fields in
  let members = List.map it members in
  let typs = List.map (static_eval_type ctx) typs in
  let fields = List.combine members typs in
  let typ = R.Type.UnionT fields in
  check_well_formed ctx typ;
  let ctx = Ctx.add_td_glob id.it typ ctx in
  ctx

(* (7.2.1)
   An enum declaration introduces a new identifier in the current scope for
   naming the created type along with its distinct constants. *)

let type_enum_decl_glob (ctx : Ctx.t) (id : id) (members : member list) : Ctx.t
    =
  let members = List.map it members in
  let typ = R.Type.EnumT (id.it, members) in
  check_well_formed ctx typ;
  let ctx = Ctx.add_td_glob id.it typ ctx in
  ctx

(* (7.2.1)
   It is also possible to specify an enum with an underlying representation.
   These are sometimes called serializable enums, because headers are allowed to have fields with such enum types.
   This requires the programmer provide both the fixed-width unsigned (or signed) integer type and an associated integer value
   for each symbolic entry in the enumeration. The symbol typeRef in the grammar above must be one of the following types:

    - an unsigned integer, i.e. bit<W> for some compile-time known W.
    - a signed integer, i.e. int<W> for some compile-time known W.
    - a type name declared via typedef, where the base type of that type is either one of the types listed above,
      or another typedef name that meets these conditions.

   Compiler implementations are expected to raise an error if the fixed-width integer representation for an enumeration entry
   falls outside the representation range of the underlying type.
*)

let type_senum_decl_glob (ctx : Ctx.t) (id : id) (typ : typ)
    (fields : (member * expr) list) : Ctx.t =
  let typ = static_eval_type ctx typ in
  let members, exprs = List.split fields in
  let members = List.map it members in
  (* (TODO) Check that values are of typ *)
  let values = static_eval_exprs ctx exprs |> expect_values in
  let fields = List.combine members values in
  let typ = R.Type.SEnumT (id.it, typ, fields) in
  check_well_formed ctx typ;
  let ctx = Ctx.add_td_glob id.it typ ctx in
  ctx

(* (7.6)
   Similarly to typedef, the keyword type can be used to introduce a new type.
   While similar to typedef, the type keyword introduces a new type which is not a synonym with the original type:
   values of the original type and the newly introduced type cannot be mixed in expressions.
   Currently the types that can be created by the type keyword are restricted to one of:
   bit<>, int<>, bool, or types defined using type from such types. *)

let type_newtype_decl_glob (ctx : Ctx.t) (id : id) (typ : typ) : Ctx.t =
  let typ = static_eval_type ctx typ in
  check_valid_alias ctx ~is_newtype:true typ;
  let ctx = Ctx.add_td_glob id.it typ ctx in
  ctx

(* (7.5)
   A typedef declaration can be used to give an alternative name to a type.
   The two types are treated as synonyms, and all operations that can be executed using
   the original type can be also executed using the newly created type.
   If typedef is used with a generic type the type must be specialized with the suitable number of type arguments: *)

let type_typedef_decl_glob (ctx : Ctx.t) (id : id) (typ : typ) : Ctx.t =
  let typ = static_eval_type ctx typ in
  check_valid_alias ctx ~is_newtype:false typ;
  let ctx = Ctx.add_td_glob id.it typ ctx in
  ctx

(* (7.2.12)
   Parsers and control blocks types are similar to function types: they describe the signature of parsers and control blocks.
   Such functions have no return values. Declarations of parsers and control block types in architectures may be generic
   (i.e., have type parameters).

   (7.2.12.1)
   A parser should have at least one argument of type packet_in, representing the received packet that is processed. *)

let type_parser_type_decl_glob (ctx : Ctx.t) (_id : id) (_tparams : tparam list)
    (_param : param list) : Ctx.t =
  Format.eprintf
    "(TODO: type_parser_type_decl_glob) Handle parser type declaration\n";
  ctx

(* (7.2.12.2) *)

let type_control_type_decl_glob (ctx : Ctx.t) (_id : id)
    (_tparams : tparam list) (_param : param list) : Ctx.t =
  Format.eprintf
    "(TODO: type_control_type_decl_glob) Handle control type declaration\n";
  ctx

(* (7.2.13)
   All parameters of a package are evaluated at compilation time, and in consequence they must all be directionless
   (they cannot be in, out, or inout). Otherwise package types are very similar to parser type declarations. *)

let type_package_type_decl_glob (ctx : Ctx.t) (_id : id)
    (_tparams : tparam list) (_cparams : cparam list) : Ctx.t =
  Format.eprintf
    "(TODO: type_package_type_decl_glob) Handle package type declaration\n";
  ctx

let type_decl_glob (ctx : Ctx.t) (decl : decl) : Ctx.t =
  match decl.it with
  (* constantDeclaration *)
  | ConstD { id; typ; value } -> type_constant_decl_glob ctx id typ value
  (* errorDeclaration *)
  | ErrD { members } -> type_error_decl_glob ctx members
  (* matchKindDeclaration *)
  | MatchKindD { members } -> type_match_kind_decl_glob ctx members
  (* typeDeclaration *)
  | StructD { id; fields } -> type_struct_decl_glob ctx id fields
  | HeaderD { id; fields } -> type_header_decl_glob ctx id fields
  | UnionD { id; fields } -> type_union_decl_glob ctx id fields
  | EnumD { id; members } -> type_enum_decl_glob ctx id members
  | SEnumD { id; typ; fields } -> type_senum_decl_glob ctx id typ fields
  | NewTypeD { id; typ } -> (
      match typ with
      | Left typ -> type_newtype_decl_glob ctx id typ
      | Right _ -> failwith "(TODO: type_decl_glob) Handle newtype with decl")
  | TypeDefD { id; typ } -> (
      match typ with
      | Left typ -> type_typedef_decl_glob ctx id typ
      | Right _ -> failwith "(TODO: type_decl_glob) Handle typedef with decl")
  | ParserTypeD { id; tparams; params } ->
      type_parser_type_decl_glob ctx id tparams params
  | ControlTypeD { id; tparams; params } ->
      type_control_type_decl_glob ctx id tparams params
  | PackageTypeD { id; tparams; cparams } ->
      type_package_type_decl_glob ctx id tparams cparams
  (* functionDeclaration *)
  (* actionDeclaration *)
  (* externDeclaration *)
  (* parserDeclaration *)
  (* controlDeclaration *)
  (* instantiation *)
  | _ -> ctx

let type_program (program : program) =
  let ctx = Ctx.empty in
  let _ = List.fold_left type_decl_glob ctx program in
  program
