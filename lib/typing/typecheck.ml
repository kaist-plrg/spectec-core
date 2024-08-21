open Syntax.Ast
open Runtime.Base
open Util.Source

(* Static type and expression evaluation *)

(* Eliminate all type references and replace them with the type they refer to.
   Warning: this will loop forever if there is a cycle in the type references. *)
let rec saturate_type (ctx : Ctx.t) (typ : Type.t) : Type.t =
  match typ with
  | VoidT | BoolT | FIntT _ | IntT | FBitT _ | VBitT _ | StrT | ErrT | MatchKindT
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

let rec static_eval_type (ctx : Ctx.t) (typ : typ) : Type.t =
  match typ.it with
  | VoidT -> Type.VoidT
  | BoolT -> Type.BoolT
  | ErrT -> failwith "(TODO: static_eval_type) Handle error type"
  | StrT -> Type.StrT
  | IntT -> Type.IntT
  | FIntT _expr -> failwith "(TODO: static_eval_type) Handle int type"
  | FBitT _expr -> failwith "(TODO: static_eval_type) Handle bit type"
  | VBitT _expr -> failwith "(TODO: static_eval_type) Handle vbit type"
  | NameT _var -> failwith "(TODO: static_eval_type) Handle named type"
  | SpecT (_var, _typs) -> failwith "(TODO: static_eval_type) Handle spec type"
  | StackT (_typ, _expr) ->
      failwith "(TODO: static_eval_type) Handle stack type"
  | TupleT typs ->
      let typs = List.map (static_eval_type ctx) typs in
      Type.TupleT typs
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
  | CastE (typ, expr) -> static_eval_cast ctx typ expr
  | BitAccE (expr_base, expr_lo, expr_hi) ->
      static_eval_bitstring_acc ctx expr_base expr_lo expr_hi
  | TypeAccE (var, member) -> static_eval_type_acc ctx var member
  | ErrAccE member -> static_eval_error_acc ctx member
  | ExprAccE (expr_base, member) -> static_eval_expr_acc ctx expr_base member
  | CallE (expr_func, targs, args) -> static_eval_call ctx expr_func targs args
  | _ ->
      Format.eprintf "(static_eval_expr) %a is not compile-time known"
        (Syntax.Pp.pp_expr ~level:0) expr;
      assert false

and static_eval_bool (b : bool) : Value.t option = Some (BoolV b)
and static_eval_str (t : text) : Value.t option = Some (StrV t.it)

and static_eval_num (value : Bigint.t) (encoding : (Bigint.t * bool) option) :
    Value.t option =
  match encoding with
  | Some (width, signed) ->
      if signed then Some (FIntV (width, value)) else Some (FBitV (width, value))
  | None -> Some (IntV value)

and static_eval_var (ctx : Ctx.t) (var : var) : Value.t option =
  match var.it with
  | Top id -> Ctx.find_const_glob_opt id.it ctx
  | Current id -> Ctx.find_const_opt id.it ctx

and static_eval_list (ctx : Ctx.t) (exprs : expr list) : Value.t option =
  let values = static_eval_exprs ctx exprs in
  Option.map (fun values -> Value.TupleV values) values

and static_eval_record (ctx : Ctx.t) (fields : (member * expr) list) :
    Value.t option =
  let members, exprs = List.split fields in
  let members = List.map it members in
  let values = static_eval_exprs ctx exprs in
  Option.map (fun values -> Value.StructV (List.combine members values)) values

and static_eval_unop (ctx : Ctx.t) (op : unop) (expr : expr) : Value.t option =
  let value = static_eval_expr ctx expr in
  Option.map (Runtime.Ops.eval_unop op) value

and static_eval_binop (ctx : Ctx.t) (op : binop) (expr_fst : expr)
    (expr_snd : expr) : Value.t option =
  let values = static_eval_exprs ctx [ expr_fst; expr_snd ] in
  Option.map
    (fun values ->
      let value_fst, value_snd = (List.nth values 0, List.nth values 1) in
      Runtime.Ops.eval_binop op value_fst value_snd)
    values

and static_eval_ternop (ctx : Ctx.t) (expr_cond : expr) (expr_tru : expr)
    (expr_fls : expr) : Value.t option =
  let value_cond = static_eval_expr ctx expr_cond in
  Option.bind value_cond (fun value_cond ->
      let cond = Value.get_bool value_cond in
      let expr = if cond then expr_tru else expr_fls in
      static_eval_expr ctx expr)

and static_eval_cast (ctx : Ctx.t) (typ : typ) (expr : expr) : Value.t option =
  let typ = static_eval_type ctx typ in
  let typ = saturate_type ctx typ in
  let value = static_eval_expr ctx expr in
  Option.map (Runtime.Ops.eval_cast typ) value

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

and static_eval_type_acc (ctx : Ctx.t) (var : var) (member : member) :
    Value.t option =
  let typ =
    match var.it with
    | Top id -> Ctx.find_td_glob id.it ctx
    | Current id -> Ctx.find_td id.it ctx
  in
  match typ with
  | EnumT (id, members) when List.mem member.it members ->
      Some (EnumFieldV (id, member.it))
  | SEnumT (id, _typ, fields) when List.mem_assoc member.it fields ->
      let value = List.assoc member.it fields in
      Some (SEnumFieldV (id, member.it, value))
  | _ -> None

and static_eval_error_acc (ctx : Ctx.t) (member : member) : Value.t option =
  let id = "error." ^ member.it in
  match Ctx.find_const_glob_opt id ctx with
  | Some (ErrV _ as value) -> Some value
  | _ -> None

and static_eval_expr_acc (ctx : Ctx.t) (expr_base : expr) (member : member) :
    Value.t option =
  let value_base = static_eval_expr ctx expr_base in
  match value_base with
  | Some value_base -> (
      match value_base with
      | StructV fields when List.mem_assoc member.it fields ->
          Some (List.assoc member.it fields)
      | StackV (_, _, size) when member.it = "size" -> Some (IntV size)
      | _ -> None)
  | _ -> None

and static_eval_call (_ctx : Ctx.t) (_expr_func : expr) (_targs : typ list)
    (_args : arg list) : Value.t option =
  failwith "(TODO: static_eval_expr) Handle static function call"

and static_eval_exprs (ctx : Ctx.t) (exprs : expr list) : Value.t list option =
  let values = List.map (static_eval_expr ctx) exprs in
  if
    List.for_all Option.is_some values && List.length exprs = List.length values
  then Some (List.map Option.get values)
  else None

(* Checkers for well-formedness *)

let check_distinct_members (members : member list) =
  List.fold_left
    (fun (distinct, seen) member ->
      if not distinct then (distinct, seen)
      else if List.mem member.it seen then (false, seen)
      else (distinct, member.it :: seen))
    (true, []) members
  |> fst

(* Type casting *)

let cast_expr (_ctx : Ctx.t) (_typ : Type.t) (_expr : expr) : expr =
  failwith "(TODO: cast_expr) Handle type cast insertion"

(* Type checking *)

let type_constant_decl_glob (ctx : Ctx.t) (id : id) (typ : typ) (value : expr) :
    Ctx.t =
  let typ = static_eval_type ctx typ in
  let value = cast_expr ctx typ value in
  match static_eval_expr ctx value with
  | Some value ->
      (* (TODO) AST transformation: replace expression with the value *)
      let ctx = Ctx.add_const_glob id.it value ctx in
      let ctx = Ctx.add_type_glob id.it typ ctx in
      ctx
  | None ->
      Format.eprintf "(type_constant_decl) %a is not a compile-time known value"
        (Syntax.Pp.pp_expr ~level:0) value;
      assert false

(* (7.1.2)
   All error constants are inserted into the error namespace, irrespective of the place where an error is defined.
   error is similar to an enumeration (enum) type in other languages. A program can contain multiple error declarations,
   which the compiler will merge together. It is an error to declare the same identifier multiple times. *)
let type_error_decl_glob (ctx : Ctx.t) (members : member list) : Ctx.t =
  List.fold_left
    (fun ctx member ->
      let id = "error." ^ member.it in
      match Ctx.find_const_glob_opt id ctx with
      | None ->
          let value = Value.ErrV member.it in
          let typ = Type.ErrT in
          let ctx = Ctx.add_const_glob id value ctx in
          let ctx = Ctx.add_type_glob id typ ctx in
          ctx
      | Some _ ->
          Format.eprintf "(type_error_decl_glob) Error %a was already defined\n"
            (Syntax.Pp.pp_member ~level:0) member;
          assert false)
    ctx members

(* (7.1.3)
   The match_kind type is very similar to the error type and is used to declare a set of distinct names
   that may be used in a table's key property (described in Section 14.2.1).
   All identifiers are inserted into the top-level namespace.
   It is an error to declare the same match_kind identifier multiple times.

   (TODO) Can the type system enforce the following constraint?

   The declaration of new match_kinds can only occur within model description files;
   P4 programmers cannot declare new match kinds. *)
let type_match_kind_decl_glob (ctx : Ctx.t) (members : member list) : Ctx.t =
  List.fold_left
    (fun ctx member ->
      let id = member.it in
      match Ctx.find_const_glob_opt id ctx with
      | None ->
          let value = Value.MatchKindV member.it in
          let typ = Type.MatchKindT in
          let ctx = Ctx.add_const_glob id value ctx in
          let ctx = Ctx.add_type_glob id typ ctx in
          ctx
      | Some _ ->
          Format.eprintf
            "(type_match_kind_decl_glob) Match kind %a was already defined\n"
            (Syntax.Pp.pp_member ~level:0) member;
          assert false)
    ctx members

(* (7.2.5)
   This declaration introduces a new type with the specified name in the current scope.
   Field names have to be distinct. An empty struct (with no fields) is legal. *)
let type_struct_decl_glob (_ctx : Ctx.t) (_id : id)
    (_fields : (member * typ * anno list) list) : Ctx.t =
  failwith "(TODO: type_struct_decl_glob) Handle struct declaration"

(* (7.2.2) *)
let type_header_decl_glob (_ctx : Ctx.t) (_id : id)
    (_fields : (member * typ * anno list) list) : Ctx.t =
  failwith "(TODO: type_header_decl_glob) Handle header declaration"

(* (7.2.4) *)
let type_union_decl_glob (_ctx : Ctx.t) (_id : id)
    (_fields : (member * typ * anno list) list) : Ctx.t =
  failwith "(TODO: type_union_decl_glob) Handle header union declaration"

(* (7.2.1)
   An enum declaration introduces a new identifier in the current scope for
   naming the created type along with its distinct constants. *)
let type_enum_decl_glob (ctx : Ctx.t) (id : id) (members : member list) : Ctx.t
    =
  if not (check_distinct_members members) then
    failwith "(type_enum_decl_glob) Enum members are not distinct";
  let members = List.map it members in
  let typ = Type.EnumT (id.it, members) in
  let ctx = Ctx.add_type_glob id.it typ ctx in
  ctx

(* (7.2.1)
   It is also possible to specify an enum with an underlying representation.
   These are sometimes called serializable enums, because headers are allowed to have fields with such enum types.
   This requires the programmer provide both the fixed-width unsigned (or signed) integer type and an associated integer value
   for each symbolic entry in the enumeration. The symbol typeRef in the grammar above must be one of the following types:

    - an unsigned integer, i.e. bit<W> for some compile-time known W.
    - a signed integer, i.e. int<W> for some compile-time known W.
    - a type name declared via typedef, where the base type of that type is either one of the types listed above, or another typedef name that meets these conditions.

   Compiler implementations are expected to raise an error if the fixed-width integer representation for an enumeration entry
   falls outside the representation range of the underlying type.
*)
let type_senum_decl_glob (_ctx : Ctx.t) (_id : id) (_typ : typ)
    (_fields : (member * expr) list) : Ctx.t =
  failwith "(TODO: type_senum_decl_glob) Handle struct enum declaration"

(* (7.6)
   Similarly to typedef, the keyword type can be used to introduce a new type.
   While similar to typedef, the type keyword introduces a new type which is not a synonym with the original type:
   values of the original type and the newly introduced type cannot be mixed in expressions.
   Currently the types that can be created by the type keyword are restricted to one of:
   bit<>, int<>, bool, or types defined using type from such types. *)
let type_newtype_decl_glob (_ctx : Ctx.t) (_id : id) (_typ : typ) : Ctx.t =
  failwith "(TODO: type_newtype_decl_glob) Handle newtype declaration"

(* (7.5)
   A typedef declaration can be used to give an alternative name to a type.
   The two types are treated as synonyms, and all operations that can be executed using
   the original type can be also executed using the newly created type.
   If typedef is used with a generic type the type must be specialized with the suitable number of type arguments: *)
let type_typedef_decl_glob (_ctx : Ctx.t) (_id : id) (_typ : typ) : Ctx.t =
  failwith "(TODO: type_typedef_decl_glob) Handle typedef declaration"

(* (7.2.12)
   Parsers and control blocks types are similar to function types: they describe the signature of parsers and control blocks.
   Such functions have no return values. Declarations of parsers and control block types in architectures may be generic
   (i.e., have type parameters).

   (7.2.12.1)
   A parser should have at least one argument of type packet_in, representing the received packet that is processed. *)
let type_parser_type_decl_glob (_ctx : Ctx.t) (_id : id)
    (_tparams : tparam list) (_param : param list) : Ctx.t =
  failwith "(TODO: type_parser_type_decl_glob) Handle parser type declaration"

(* (7.2.12.2) *)
let type_control_type_decl_glob (_ctx : Ctx.t) (_id : id)
    (_tparams : tparam list) (_param : param list) : Ctx.t =
  failwith "(TODO: type_control_type_decl_glob) Handle control type declaration"

(* (7.2.13)
   All parameters of a package are evaluated at compilation time, and in consequence they must all be directionless
   (they cannot be in, out, or inout). Otherwise package types are very similar to parser type declarations. *)
let type_package_type_decl_glob (_ctx : Ctx.t) (_id : id)
    (_tparams : tparam list) (_cparams : cparam list) : Ctx.t =
  failwith "(TODO: type_package_type_decl_glob) Handle package type declaration"

let type_decl_glob (ctx : Ctx.t) (decl : decl) : Ctx.t =
  match decl.it with
  (* constantDeclaration *)
  | ConstD { id; typ; value; annos } -> let _ = annos in  type_constant_decl_glob ctx id typ value
  (* errorDeclaration *)
  | ErrD { members; } -> type_error_decl_glob ctx members
  (* matchKindDeclaration *)
  | MatchKindD { members } -> type_match_kind_decl_glob ctx members
  (* typeDeclaration *)
  | StructD { id; fields; annos } -> let _ = annos in type_struct_decl_glob ctx id fields
  | HeaderD { id; fields; annos } -> let _ = annos in type_header_decl_glob ctx id fields
  | UnionD { id; fields; annos} -> let _ = annos in type_union_decl_glob ctx id fields
  | EnumD { id; members; annos } -> let _ = annos in type_enum_decl_glob ctx id members
  | SEnumD { id; typ; fields; annos } -> let _ = annos in type_senum_decl_glob ctx id typ fields
  | NewTypeD { id; typ; annos } -> (
      let _ = annos in 
      match typ with
      | Left typ -> type_newtype_decl_glob ctx id typ
      | Right _ -> failwith "(TODO: type_decl_glob) Handle newtype with decl")
  | TypeDefD { id; typ; annos } -> (
      let _ = annos in 
      match typ with
      | Left typ -> type_typedef_decl_glob ctx id typ
      | Right _ -> failwith "(TODO: type_decl_glob) Handle typedef with decl")
  | ParserTypeD { id; tparams; params; annos } ->
    let _ = annos in 
      type_parser_type_decl_glob ctx id tparams params
  | ControlTypeD { id; tparams; params; annos } ->
    let _ = annos in 
      type_control_type_decl_glob ctx id tparams params
  | PackageTypeD { id; tparams; cparams; annos } ->
    let _ = annos in 
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
