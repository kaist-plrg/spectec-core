open Syntax.Ast
open Util.Source
open Types
module Value = Runtime.Value

(* Utils *)

let expect_value (value : Value.t option) : Value.t =
  match value with Some value -> value | None -> assert false

let expect_values (values : Value.t list option) : Value.t list =
  match values with Some values -> values | None -> assert false

(* Check well-formedness *)

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
   (Question) Shouldn't we consider serializable enum as nested type also?

   for each symbolic entry in the enumeration. The symbol typeRef in the grammar above must be one of the following types:

    - an unsigned integer, i.e. bit<W> for some compile-time known W.
    - a signed integer, i.e. int<W> for some compile-time known W.
    - a type name declared via typedef, where the base type of that type is either one of the types listed above,
      or another typedef name that meets these conditions. *)

let check_distinct_members (members : member' list) : unit =
  let distinct =
    List.fold_left
      (fun (distinct, members) member ->
        if not distinct then (distinct, members)
        else if List.mem member members then (false, members)
        else (distinct, member :: members))
      (true, []) members
    |> fst
  in
  if not distinct then (
    Format.eprintf "(check_distinct_members) Members are not distinct\n";
    assert false)
  else ()

(* Check the validity of the typedef `td` in the context `ctx`.
   Invariant : All typedefs already in the context are valid. *)

let rec check_valid_type_nesting' (ctx : Ctx.t) (typ : BaseType.t)
    (typ_inner : BaseType.t) : bool =
  match typ with
  | VoidT | ErrT | MatchKindT | StrT | BoolT | AIntT | IntT _ | BitT _ | VBitT _
    ->
      true
  | TupleT _ -> (
      match typ_inner with
      | VoidT -> false
      | ErrT -> true
      | MatchKindT | StrT | AIntT -> false
      | BoolT | IntT _ | BitT _ | VBitT _ | TupleT _ | StackT _ -> true
      | VarT var ->
          let td_inner = Ctx.find Ctx.find_td var ctx in
          check_valid_type_nesting'' ctx typ td_inner
      | SVarT _ -> false)
  | StackT _ -> (
      match typ_inner with
      | VoidT | ErrT | MatchKindT | StrT | BoolT | AIntT | IntT _ | BitT _
      | VBitT _ | TupleT _ | StackT _ ->
          false
      | VarT var ->
          let td_inner = Ctx.find Ctx.find_td var ctx in
          check_valid_type_nesting'' ctx typ td_inner
      | SVarT _ -> false)
  | VarT _ | SVarT _ -> true

and check_valid_type_nesting'' (ctx : Ctx.t) (typ : BaseType.t)
    (td_inner : TypeDef.t) : bool =
  match typ with
  | VoidT | ErrT | MatchKindT | StrT | BoolT | AIntT | IntT _ | BitT _ | VBitT _
    ->
      true
  | TupleT _ -> (
      match td_inner with
      | DefT typ_inner -> check_valid_type_nesting' ctx typ typ_inner
      | NewT typ_inner -> check_valid_type_nesting' ctx typ typ_inner
      | StructT _ | HeaderT _ | UnionT _ | EnumT _ | SEnumT _ -> true
      | ExternT _ | ParserT _ | ControlT _ | PackageT _ -> false)
  | StackT _ -> (
      match td_inner with
      | DefT typ_inner -> check_valid_type_nesting' ctx typ typ_inner
      | NewT typ_inner -> check_valid_type_nesting' ctx typ typ_inner
      | StructT _ -> false
      | HeaderT _ | UnionT _ -> true
      | EnumT _ | SEnumT _ | ExternT _ | ParserT _ | ControlT _ | PackageT _ ->
          false)
  | VarT _ | SVarT _ -> true

let check_valid_type_nesting (ctx : Ctx.t) (typ : BaseType.t)
    (typ_inner : BaseType.t) : unit =
  if not (check_valid_type_nesting' ctx typ typ_inner) then (
    Format.eprintf "(check_valid_nesting) Invalid nesting of %a inside %a\n"
      BaseType.pp typ_inner BaseType.pp typ;
    assert false)
  else ()

let rec check_valid_typedef_nesting' (ctx : Ctx.t) (td : TypeDef.t)
    (typ_inner : BaseType.t) : bool =
  match td with
  | DefT _ -> (
      match typ_inner with
      | VoidT -> false
      | ErrT -> true
      | MatchKindT | StrT -> false
      | BoolT | AIntT | IntT _ | BitT _ | VBitT _ | TupleT _ | StackT _ -> true
      | VarT var ->
          let td_inner = Ctx.find Ctx.find_td var ctx in
          check_valid_typedef_nesting'' ctx td td_inner
      | SVarT _ -> false)
  | NewT _ -> (
      match typ_inner with
      | VoidT | ErrT | MatchKindT | StrT -> false
      | BoolT -> true
      | AIntT -> false
      | IntT _ | BitT _ -> true
      | VBitT _ | TupleT _ | StackT _ -> false
      | VarT var ->
          let td_inner = Ctx.find Ctx.find_td var ctx in
          check_valid_typedef_nesting'' ctx td td_inner
      | SVarT _ -> false)
  | StructT _ -> (
      match typ_inner with
      | VoidT -> false
      | ErrT -> true
      | MatchKindT | StrT -> false
      | BoolT -> true
      | AIntT -> false
      | IntT _ | BitT _ | VBitT _ -> true
      | TupleT _ | StackT _ -> true
      | VarT var ->
          let td_inner = Ctx.find Ctx.find_td var ctx in
          check_valid_typedef_nesting'' ctx td td_inner
      | SVarT _ -> false)
  | HeaderT _ -> (
      match typ_inner with
      | VoidT | ErrT | MatchKindT | StrT -> false
      | BoolT -> true
      | AIntT -> false
      | IntT _ | BitT _ | VBitT _ -> true
      | TupleT _ | StackT _ -> false
      | VarT var ->
          let td_inner = Ctx.find Ctx.find_td var ctx in
          check_valid_typedef_nesting'' ctx td td_inner
      | SVarT _ -> false)
  | UnionT _ -> (
      match typ_inner with
      | VoidT | ErrT | MatchKindT | StrT | BoolT | AIntT | IntT _ | BitT _
      | VBitT _ | TupleT _ | StackT _ ->
          false
      | VarT var ->
          let td_inner = Ctx.find Ctx.find_td var ctx in
          check_valid_typedef_nesting'' ctx td td_inner
      | SVarT _ -> false)
  | _ -> false

and check_valid_typedef_nesting'' (ctx : Ctx.t) (td : TypeDef.t)
    (td_inner : TypeDef.t) : bool =
  match td with
  | DefT _ -> (
      match td_inner with
      | DefT _ | NewT _ | StructT _ | HeaderT _ | UnionT _ | EnumT _ | SEnumT _
        ->
          true
      | ExternT _ | ParserT _ | ControlT _ | PackageT _ -> false)
  | NewT _ -> (
      match td_inner with
      | DefT typ_inner -> check_valid_typedef_nesting' ctx td typ_inner
      | NewT _ -> true
      | StructT _ | HeaderT _ | UnionT _ | EnumT _ | SEnumT _ | ExternT _
      | ParserT _ | ControlT _ | PackageT _ ->
          false)
  | StructT _ -> (
      match td_inner with
      | DefT typ_inner -> check_valid_typedef_nesting' ctx td typ_inner
      | NewT typ_inner -> check_valid_typedef_nesting' ctx td typ_inner
      | StructT _ | HeaderT _ | UnionT _ | EnumT _ | SEnumT _ -> true
      | ExternT _ | ParserT _ | ControlT _ | PackageT _ -> false)
  | HeaderT _ -> (
      match td_inner with
      | DefT typ_inner -> check_valid_typedef_nesting' ctx td typ_inner
      | NewT typ_inner -> check_valid_typedef_nesting' ctx td typ_inner
      | StructT fields ->
          let _, typs_inner = List.split fields in
          List.for_all (check_valid_typedef_nesting' ctx td) typs_inner
      | HeaderT _ | UnionT _ | EnumT _ -> false
      | SEnumT (typ_inner, _) -> check_valid_typedef_nesting' ctx td typ_inner
      | ExternT _ | ParserT _ | ControlT _ | PackageT _ -> false)
  | UnionT _ -> (
      match td_inner with
      | DefT typ_inner -> check_valid_typedef_nesting' ctx td typ_inner
      | NewT typ_inner -> check_valid_typedef_nesting' ctx td typ_inner
      | StructT _ -> false
      | HeaderT _ -> true
      | UnionT _ | EnumT _ | SEnumT _ | ExternT _ | ParserT _ | ControlT _
      | PackageT _ ->
          false)
  | _ -> false

let check_valid_typedef_nesting (ctx : Ctx.t) (td : TypeDef.t)
    (typ_inner : BaseType.t) : unit =
  if not (check_valid_typedef_nesting' ctx td typ_inner) then (
    Format.eprintf "(check_valid_nesting) Invalid nesting of %a inside %a\n"
      BaseType.pp typ_inner TypeDef.pp td;
    assert false)
  else ()

let rec check_valid_type (ctx : Ctx.t) (typ : BaseType.t) : unit =
  match typ with
  | VoidT | ErrT | MatchKindT | StrT | BoolT | AIntT | IntT _ | BitT _ | VBitT _
    ->
      ()
  | TupleT typs_inner ->
      List.iter
        (fun typ_inner ->
          check_valid_type ctx typ_inner;
          check_valid_type_nesting ctx typ typ_inner)
        typs_inner
  | StackT (typ_inner, _) ->
      check_valid_type ctx typ_inner;
      check_valid_type_nesting ctx typ typ_inner
  | VarT var ->
      let exists = Ctx.find_opt Ctx.find_td_opt var ctx |> Option.is_some in
      if not exists then (
        Format.eprintf "(check_valid_type) Type %a does not exist\n"
          Syntax.Pp.pp_var (var $ no_info);
        assert false)
  | SVarT (var, typs_arg) ->
      let td_opt = Ctx.find_opt Ctx.find_td_opt var ctx in
      let exists = Option.is_some td_opt in
      if not exists then (
        Format.eprintf "(check_valid_type) Type %a does not exist\n"
          Syntax.Pp.pp_var (var $ no_info);
        assert false);
      (* (TODO) Check arity for type params *)
      (* (TODO) Check that the specialized type is valid *)
      List.iter (check_valid_type ctx) typs_arg

let check_valid_typedef (ctx : Ctx.t) (td : TypeDef.t) : unit =
  match td with
  | DefT typ_inner ->
      check_valid_type ctx typ_inner;
      check_valid_typedef_nesting ctx td typ_inner
  | NewT typ_inner ->
      check_valid_type ctx typ_inner;
      check_valid_typedef_nesting ctx td typ_inner
  | StructT fields ->
      let members, typs_inner = List.split fields in
      check_distinct_members members;
      List.iter
        (fun typ_inner ->
          check_valid_type ctx typ_inner;
          check_valid_typedef_nesting ctx td typ_inner)
        typs_inner
  | HeaderT fields ->
      let members, typs_inner = List.split fields in
      check_distinct_members members;
      List.iter
        (fun typ_inner ->
          check_valid_type ctx typ_inner;
          check_valid_typedef_nesting ctx td typ_inner)
        typs_inner
  | UnionT fields ->
      let members, typs_inner = List.split fields in
      check_distinct_members members;
      List.iter
        (fun typ_inner ->
          check_valid_type ctx typ_inner;
          check_valid_typedef_nesting ctx td typ_inner)
        typs_inner
  | _ -> ()

(* Static type and expression evaluation *)

let rec static_eval_type (ctx : Ctx.t) (typ : typ) : BaseType.t =
  match typ.it with
  | VoidT -> BaseType.VoidT
  | ErrT -> BaseType.ErrT
  | StrT -> BaseType.StrT
  | BoolT -> BaseType.BoolT
  | AIntT -> BaseType.AIntT
  | IntT expr ->
      let width = static_eval_expr ctx expr |> expect_value |> Value.get_num in
      BaseType.IntT width
  | BitT expr ->
      let width = static_eval_expr ctx expr |> expect_value |> Value.get_num in
      BaseType.BitT width
  | VBitT expr ->
      let width = static_eval_expr ctx expr |> expect_value |> Value.get_num in
      BaseType.VBitT width
  | NameT var ->
      let exists =
        (match var.it with
        | Top id -> Ctx.find_td_opt Ctx.Global id.it ctx
        | Bare id -> Ctx.find_td_opt Ctx.Local id.it ctx)
        |> Option.is_some
      in
      if not exists then (
        Format.eprintf "(static_eval_type) Type %a does not exist\n"
          Syntax.Pp.pp_var var;
        assert false);
      BaseType.VarT var.it
  | SpecT (var, typs) ->
      let td =
        match var.it with
        | Top id -> Ctx.find_td_opt Ctx.Global id.it ctx
        | Bare id -> Ctx.find_td_opt Ctx.Local id.it ctx
      in
      if Option.is_none td then (
        Format.eprintf "(static_eval_type) Type %a does not exist\n"
          Syntax.Pp.pp_var var;
        assert false);
      let typs = List.map (static_eval_type ctx) typs in
      (* (TODO) Check that the specialized type is valid *)
      BaseType.SVarT (var.it, typs)
  | StackT (typ, expr) ->
      let typ = static_eval_type ctx typ in
      let size = static_eval_expr ctx expr |> expect_value |> Value.get_num in
      BaseType.StackT (typ, size)
  | TupleT typs ->
      let typs = List.map (static_eval_type ctx) typs in
      BaseType.TupleT typs
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

and static_eval_expr (ctx : Ctx.t) (expr : expr) : Value.t option =
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
  (* | CastE (typ, expr) -> static_eval_cast ctx typ expr *)
  | BitAccE (expr_base, expr_lo, expr_hi) ->
      static_eval_bitstring_acc ctx expr_base expr_lo expr_hi
  (* | TypeAccE (var, member) -> static_eval_type_acc ctx var member *)
  | ErrAccE member -> static_eval_error_acc ctx member
  (* | ExprAccE (expr_base, member) -> static_eval_expr_acc ctx expr_base member *)
  (* | CallE (expr_func, targs, args) -> static_eval_call ctx expr_func targs args *)
  | _ -> None

and static_eval_bool (b : bool) : Value.t option = Some (BoolV b)
and static_eval_str (s : string) : Value.t option = Some (StrV s)

and static_eval_num (value : Bigint.t) (encoding : (Bigint.t * bool) option) :
    Value.t option =
  match encoding with
  | Some (width, signed) ->
      if signed then Some (IntV (width, value)) else Some (BitV (width, value))
  | None -> Some (AIntV value)

and static_eval_var (ctx : Ctx.t) (var : var) : Value.t option =
  match var.it with
  | Top id -> Ctx.find_value_opt Ctx.Global id.it ctx
  | Bare id -> Ctx.find_value_opt Ctx.Local id.it ctx

and static_eval_list (ctx : Ctx.t) (exprs : expr list) : Value.t option =
  let values = static_eval_exprs ctx exprs in
  Option.map (fun values -> Value.TupleV values) values

and static_eval_record (ctx : Ctx.t) (fields : (member * expr) list) :
    Value.t option =
  let members, exprs = List.split fields in
  let members = List.map it members in
  let values = static_eval_exprs ctx exprs in
  Option.map (fun values -> Value.StructV (List.combine members values)) values

and static_eval_unop (ctx : Ctx.t) (unop : unop) (expr : expr) : Value.t option
    =
  let value = static_eval_expr ctx expr in
  Option.map (Runtime.Ops.eval_unop unop) value

and static_eval_binop (ctx : Ctx.t) (binop : binop) (expr_fst : expr)
    (expr_snd : expr) : Value.t option =
  let values = static_eval_exprs ctx [ expr_fst; expr_snd ] in
  Option.map
    (fun values ->
      let value_fst, value_snd = (List.nth values 0, List.nth values 1) in
      Runtime.Ops.eval_binop binop value_fst value_snd)
    values

and static_eval_ternop (ctx : Ctx.t) (expr_cond : expr) (expr_tru : expr)
    (expr_fls : expr) : Value.t option =
  let value_cond = static_eval_expr ctx expr_cond in
  Option.map
    (fun value_cond ->
      let cond = Value.get_bool value_cond in
      let expr = if cond then expr_tru else expr_fls in
      static_eval_expr ctx expr)
    value_cond
  |> Option.join

(* and static_eval_cast (ctx : Ctx.t) (typ : typ) (expr : expr) : Value.t option *)
(*     = *)
(*   let typ = static_eval_type ctx typ in *)
(*   let typ = saturate_type ctx typ in *)
(*   let value = static_eval_expr ctx expr in *)
(*   Option.map (Runtime.Ops.eval_cast typ) value *)

and static_eval_bitstring_acc (ctx : Ctx.t) (expr_base : expr) (expr_lo : expr)
    (expr_hi : expr) : Value.t option =
  let values = static_eval_exprs ctx [ expr_base; expr_hi; expr_lo ] in
  Option.map
    (fun values ->
      let value_base, value_hi, value_lo =
        (List.nth values 0, List.nth values 1, List.nth values 2)
      in
      Runtime.Ops.eval_bitstring_access value_base value_hi value_lo)
    values

(* and static_eval_type_acc (ctx : Ctx.t) (var : var) (member : member) : *)
(*     Value.t option = *)
(*   let typ = *)
(*     match var.it with *)
(*     | Top id -> Ctx.find_td_glob id.it ctx *)
(*     | Bare id -> Ctx.find_td id.it ctx *)
(*   in *)
(*   match typ with *)
(*   | EnumT (id, members) when List.mem member.it members -> *)
(*       Some (EnumFieldV (id, member.it)) *)
(*   | SEnumT (id, _typ, fields) when List.mem_assoc member.it fields -> *)
(*       let value = List.assoc member.it fields in *)
(*       Some (SEnumFieldV (id, member.it, value)) *)
(*   | _ -> None *)

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

and static_eval_exprs (ctx : Ctx.t) (exprs : expr list) : Value.t list option =
  let values = List.map (static_eval_expr ctx) exprs in
  if
    List.for_all Option.is_some values && List.length exprs = List.length values
  then Some (List.map Option.get values)
  else None

(* Type checking *)

let type_const_decl (layer : Ctx.layer) (ctx : Ctx.t) (id : id) (typ : typ)
    (value : expr) : Ctx.t =
  let typ = static_eval_type ctx typ in
  check_valid_type ctx typ;
  match static_eval_expr ctx value with
  | Some value ->
      Ctx.add_value layer id.it value ctx |> Ctx.add_type layer id.it typ
  | None ->
      Format.eprintf
        "(type_const_decl) %a is not a compile-time known expression."
        Syntax.Pp.pp_expr value;
      assert false

(* (7.1.2)
   All error constants are inserted into the error namespace, irrespective of the place where an error is defined.
   error is similar to an enumeration (enum) type in other languages. A program can contain multiple error declarations,
   which the compiler will merge together. It is an error to declare the same identifier multiple times. *)

let type_error_decl (layer : Ctx.layer) (ctx : Ctx.t) (members : member list) =
  if layer <> Ctx.Global then (
    Format.eprintf "(type_error_decl) Error declarations must be global\n";
    assert false);
  let type_error_decl' (ctx : Ctx.t) (member : member) : Ctx.t =
    let id = "error." ^ member.it in
    if Ctx.find_value_opt layer id ctx |> Option.is_some then (
      Format.eprintf "(type_error_decl_glob) Error %s was already defined\n" id;
      assert false);
    let value = Value.ErrV member.it in
    let typ = BaseType.ErrT in
    Ctx.add_value layer id value ctx |> Ctx.add_type layer id typ
  in
  List.fold_left type_error_decl' ctx members

(* (7.1.3)
   The match_kind type is very similar to the error type and is used to declare a set of distinct names
   that may be used in a table's key property (described in Section 14.2.1).
   All identifiers are inserted into the top-level namespace.
   It is an error to declare the same match_kind identifier multiple times.

   (TODO) Can the type system enforce the following constraint?

   The declaration of new match_kinds can only occur within model description files;
   P4 programmers cannot declare new match kinds. *)

let type_match_kind_decl (layer : Ctx.layer) (ctx : Ctx.t)
    (members : member list) : Ctx.t =
  if layer <> Ctx.Global then (
    Format.eprintf
      "(type_match_kind_decl) Match kind declarations must be global\n";
    assert false);
  let type_match_kind_decl' (ctx : Ctx.t) (member : member) : Ctx.t =
    let id = member.it in
    if Ctx.find_value_opt layer id ctx |> Option.is_some then (
      Format.eprintf
        "(type_match_kind_decl) Match kind %s was already defined\n" id;
      assert false);
    let value = Value.MatchKindV member.it in
    let typ = BaseType.MatchKindT in
    Ctx.add_value layer id value ctx |> Ctx.add_type layer id typ
  in
  List.fold_left type_match_kind_decl' ctx members

(* (7.2.5)
   This declaration introduces a new type with the specified name in the current scope.
   Field names have to be distinct. An empty struct (with no fields) is legal. *)

let type_struct_decl (layer : Ctx.layer) (ctx : Ctx.t) (id : id)
    (fields : (member * typ) list) : Ctx.t =
  if layer <> Ctx.Global then (
    Format.eprintf "(type_struct_decl) Struct declarations must be global\n";
    assert false);
  let members, typs = List.split fields in
  let members = List.map it members in
  let typs = List.map (static_eval_type ctx) typs in
  let fields = List.combine members typs in
  let td = TypeDef.StructT fields in
  check_valid_typedef ctx td;
  Ctx.add_td layer id.it td ctx

(* (7.2.2) *)

let type_header_decl (layer : Ctx.layer) (ctx : Ctx.t) (id : id)
    (fields : (member * typ) list) : Ctx.t =
  if layer <> Ctx.Global then (
    Format.eprintf "(type_header_decl) Header declarations must be global\n";
    assert false);
  let members, typs = List.split fields in
  let members = List.map it members in
  let typs = List.map (static_eval_type ctx) typs in
  let fields = List.combine members typs in
  let td = TypeDef.HeaderT fields in
  check_valid_typedef ctx td;
  Ctx.add_td layer id.it td ctx

(* (7.2.4) *)

let type_union_decl (layer : Ctx.layer) (ctx : Ctx.t) (id : id)
    (fields : (member * typ) list) : Ctx.t =
  if layer <> Ctx.Global then (
    Format.eprintf "(type_union_decl) Union declarations must be global\n";
    assert false);
  let members, typs = List.split fields in
  let members = List.map it members in
  let typs = List.map (static_eval_type ctx) typs in
  let fields = List.combine members typs in
  let td = TypeDef.UnionT fields in
  check_valid_typedef ctx td;
  Ctx.add_td layer id.it td ctx

(* (7.2.1)
   An enum declaration introduces a new identifier in the current scope for
   naming the created type along with its distinct constants. *)

let type_enum_decl (layer : Ctx.layer) (ctx : Ctx.t) (id : id)
    (members : member list) : Ctx.t =
  if layer <> Ctx.Global then (
    Format.eprintf "(type_enum_decl) Enum declarations must be global\n";
    assert false);
  let members = List.map it members in
  let td = TypeDef.EnumT members in
  check_valid_typedef ctx td;
  Ctx.add_td layer id.it td ctx

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

let type_senum_decl (layer : Ctx.layer) (ctx : Ctx.t) (id : id) (typ : typ)
    (fields : (member * expr) list) : Ctx.t =
  if layer <> Ctx.Global then (
    Format.eprintf
      "(type_senum_decl) Serializable enum declarations must be global\n";
    assert false);
  let typ = static_eval_type ctx typ in
  let members, exprs = List.split fields in
  let members = List.map it members in
  (* (TODO) Check that values are of typ *)
  let values = static_eval_exprs ctx exprs |> expect_values in
  let fields = List.combine members values in
  let td = TypeDef.SEnumT (typ, fields) in
  check_valid_typedef ctx td;
  Ctx.add_td layer id.it td ctx

(* (7.6)
   Similarly to typedef, the keyword type can be used to introduce a new type.
   While similar to typedef, the type keyword introduces a new type which is not a synonym with the original type:
   values of the original type and the newly introduced type cannot be mixed in expressions.
   Currently the types that can be created by the type keyword are restricted to one of:
   bit<>, int<>, bool, or types defined using type from such types. *)

let type_newtype_decl (layer : Ctx.layer) (ctx : Ctx.t) (id : id)
    (typdef : (typ, decl) alt) : Ctx.t =
  if layer <> Ctx.Global then (
    Format.eprintf "(type_newtype_decl) New type declarations must be global\n";
    assert false);
  match typdef with
  | Left typ ->
      let typ = static_eval_type ctx typ in
      let td = TypeDef.NewT typ in
      check_valid_typedef ctx td;
      Ctx.add_td layer id.it td ctx
  | Right _ -> failwith "(TODO: type_newtype_decl) Handle newtype with decl"

(* (7.5)
   A typedef declaration can be used to give an alternative name to a type.
   The two types are treated as synonyms, and all operations that can be executed using
   the original type can be also executed using the newly created type.
   If typedef is used with a generic type the type must be specialized with the suitable number of type arguments: *)

let type_typedef_decl (layer : Ctx.layer) (ctx : Ctx.t) (id : id)
    (typdef : (typ, decl) alt) : Ctx.t =
  if layer <> Ctx.Global then (
    Format.eprintf "(type_typedef_decl) Typedef declarations must be global\n";
    assert false);
  match typdef with
  | Left typ ->
      let typ = static_eval_type ctx typ in
      let td = TypeDef.DefT typ in
      check_valid_typedef ctx td;
      Ctx.add_td layer id.it td ctx
  | Right _ -> failwith "(TODO: type_typedef_decl) Handle typedef with decl"

(* (7.2.12)
   Parsers and control blocks types are similar to function types: they describe the signature of parsers and control blocks.
   Such functions have no return values. Declarations of parsers and control block types in architectures may be generic
   (i.e., have type parameters).

   (7.2.12.1)
   A parser should have at least one argument of type packet_in, representing the received packet that is processed. *)

let type_parser_type_decl (layer : Ctx.layer) (ctx : Ctx.t) (id : id)
    (tparams : tparam list) (params : param list) : Ctx.t =
  if layer <> Ctx.Global then (
    Format.eprintf
      "(type_parser_type_decl) Parser type declarations must be global\n";
    assert false);
  let tparams = List.map it tparams in
  let params = List.map it params in
  let td = TypeDef.ParserT (tparams, params) in
  check_valid_typedef ctx td;
  Ctx.add_td layer id.it td ctx

(* (7.2.12.2) *)

let type_control_type_decl (layer : Ctx.layer) (ctx : Ctx.t) (id : id)
    (tparams : tparam list) (params : param list) : Ctx.t =
  if layer <> Ctx.Global then (
    Format.eprintf
      "(type_control_type_decl) Control type declarations must be global\n";
    assert false);
  let tparams = List.map it tparams in
  let params = List.map it params in
  let td = TypeDef.ControlT (tparams, params) in
  check_valid_typedef ctx td;
  Ctx.add_td layer id.it td ctx

(* (7.2.13)
   All parameters of a package are evaluated at compilation time, and in consequence they must all be directionless
   (they cannot be in, out, or inout). Otherwise package types are very similar to parser type declarations. *)

let type_package_type_decl (layer : Ctx.layer) (ctx : Ctx.t) (id : id)
    (tparams : tparam list) (cparams : cparam list) : Ctx.t =
  if layer <> Ctx.Global then (
    Format.eprintf
      "(type_package_type_decl) Package type declarations must be global\n";
    assert false);
  let tparams = List.map it tparams in
  let cparams = List.map it cparams in
  let td = TypeDef.PackageT (tparams, cparams) in
  check_valid_typedef ctx td;
  Ctx.add_td layer id.it td ctx

let type_decl_glob (layer : Ctx.layer) (ctx : Ctx.t) (decl : decl) =
  (* Format.printf "%a\n" Ctx.pp ctx; *)
  (* Format.printf "(type_decl_glob) %a\n" Syntax.Pp.pp_decl (0, decl); *)
  match decl.it with
  (* constantDeclaration *)
  | ConstD { id; typ; value } -> type_const_decl layer ctx id typ value
  (* errorDeclaration *)
  | ErrD { members } -> type_error_decl layer ctx members
  (* matchKindDeclaration *)
  | MatchKindD { members } -> type_match_kind_decl layer ctx members
  (* typeDeclaration *)
  | StructD { id; fields } -> type_struct_decl layer ctx id fields
  | HeaderD { id; fields } -> type_header_decl layer ctx id fields
  | UnionD { id; fields } -> type_union_decl layer ctx id fields
  | EnumD { id; members } -> type_enum_decl layer ctx id members
  | SEnumD { id; typ; fields } -> type_senum_decl layer ctx id typ fields
  | NewTypeD { id; typdef } -> type_newtype_decl layer ctx id typdef
  | TypeDefD { id; typdef } -> type_typedef_decl layer ctx id typdef
  | ParserTypeD { id; tparams; params } ->
      type_parser_type_decl layer ctx id tparams params
  | ControlTypeD { id; tparams; params } ->
      type_control_type_decl layer ctx id tparams params
  | PackageTypeD { id; tparams; cparams } ->
      type_package_type_decl layer ctx id tparams cparams
  (* functionDeclaration *)
  (* actionDeclaration *)
  (* externDeclaration *)
  (* parserDeclaration *)
  (* controlDeclaration *)
  (* instantiation *)
  | _ -> ctx

let type_program (program : program) =
  let ctx = Ctx.empty in
  let layer = Ctx.Global in
  let ctx = List.fold_left (type_decl_glob layer) ctx program in
  ctx
