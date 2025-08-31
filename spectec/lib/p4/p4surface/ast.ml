(* Copyright 2018-present Cornell University
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not
 * use this file except in compliance with the License. You may obtain a copy
 * of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
 * WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
 * License for the specific language governing permissions and limitations
 * under the License.
 *)

open P4util

type ('a, 'b) alternative = Left of 'a | Right of 'b

(* Basics *)

module Text : sig
  type t = Source.info t'
  and 'a t' = { tags : 'a; str : string }

  val tags : 'a t' -> 'a
end = struct
  type t = Source.info t'
  and 'a t' = { tags : 'a; str : string }

  let tags (t : 'a t') : 'a = t.tags
end

module Number : sig
  type t = Source.info t'

  and 'a t' = {
    tags : 'a;
    value : Bigint.t;
    width_signed : (Bigint.t * bool) option;
  }

  val tags : 'a t' -> 'a
end = struct
  type t = Source.info t'

  and 'a t' = {
    tags : 'a;
    value : Bigint.t;
    width_signed : (Bigint.t * bool) option;
  }

  let tags (t : 'a t') : 'a = t.tags
end

module Name : sig
  type t = Source.info t'
  and 'a t' = BareName of Text.t | QualifiedName of (Text.t list * Text.t)

  val tags : Source.info t' -> Source.info
end = struct
  type t = Source.info t'
  and 'a t' = BareName of Text.t | QualifiedName of (Text.t list * Text.t)

  let tags (t : 'a t') : 'a =
    match t with
    | BareName name -> Text.tags name
    | QualifiedName (prefix, name) ->
        let infos = List.map Text.tags prefix in
        List.fold_right Source.merge infos (Text.tags name)
end

module Direction : sig
  type t = Source.info t'

  and 'a t' =
    | In of { tags : 'a }
    | Out of { tags : 'a }
    | InOut of { tags : 'a }

  val tags : 'a t' -> 'a
end = struct
  type t = Source.info t'

  and 'a t' =
    | In of { tags : 'a }
    | Out of { tags : 'a }
    | InOut of { tags : 'a }

  let tags (t : 'a t') : 'a =
    match t with In { tags } | Out { tags } | InOut { tags } -> tags
end

(* Operators *)

module Op : sig
  type un = Source.info un'

  and 'a un' =
    | Not of { tags : 'a }
    | BitNot of { tags : 'a }
    | UPlus of { tags : 'a }
    | UMinus of { tags : 'a }

  val tags_un : 'a un' -> 'a

  type bin = Source.info bin'

  and 'a bin' =
    | Plus of { tags : 'a }
    | PlusSat of { tags : 'a }
    | Minus of { tags : 'a }
    | MinusSat of { tags : 'a }
    | Mul of { tags : 'a }
    | Div of { tags : 'a }
    | Mod of { tags : 'a }
    | Shl of { tags : 'a }
    | Shr of { tags : 'a }
    | Le of { tags : 'a }
    | Ge of { tags : 'a }
    | Lt of { tags : 'a }
    | Gt of { tags : 'a }
    | Eq of { tags : 'a }
    | NotEq of { tags : 'a }
    | BitAnd of { tags : 'a }
    | BitXor of { tags : 'a }
    | BitOr of { tags : 'a }
    | PlusPlus of { tags : 'a }
    | And of { tags : 'a }
    | Or of { tags : 'a }

  val tags_bin : 'a bin' -> 'a
end = struct
  type un = Source.info un'

  and 'a un' =
    | Not of { tags : 'a }
    | BitNot of { tags : 'a }
    | UPlus of { tags : 'a }
    | UMinus of { tags : 'a }

  let tags_un (un : 'a un') : 'a =
    match un with
    | Not { tags } | BitNot { tags } | UPlus { tags } | UMinus { tags } -> tags

  type bin = Source.info bin'

  and 'a bin' =
    | Plus of { tags : 'a }
    | PlusSat of { tags : 'a }
    | Minus of { tags : 'a }
    | MinusSat of { tags : 'a }
    | Mul of { tags : 'a }
    | Div of { tags : 'a }
    | Mod of { tags : 'a }
    | Shl of { tags : 'a }
    | Shr of { tags : 'a }
    | Le of { tags : 'a }
    | Ge of { tags : 'a }
    | Lt of { tags : 'a }
    | Gt of { tags : 'a }
    | Eq of { tags : 'a }
    | NotEq of { tags : 'a }
    | BitAnd of { tags : 'a }
    | BitXor of { tags : 'a }
    | BitOr of { tags : 'a }
    | PlusPlus of { tags : 'a }
    | And of { tags : 'a }
    | Or of { tags : 'a }

  let tags_bin (bin : 'a bin') : 'a =
    match bin with
    | Plus { tags }
    | PlusSat { tags }
    | Minus { tags }
    | MinusSat { tags }
    | Mul { tags }
    | Div { tags }
    | Mod { tags }
    | Shl { tags }
    | Shr { tags }
    | Le { tags }
    | Ge { tags }
    | Lt { tags }
    | Gt { tags }
    | Eq { tags }
    | NotEq { tags }
    | BitAnd { tags }
    | BitXor { tags }
    | BitOr { tags }
    | PlusPlus { tags }
    | And { tags }
    | Or { tags } ->
        tags
end

(* Types *)

module rec Type : sig
  type t = Source.info t'

  and 'a t' =
    | Bool of { tags : 'a }
    | MatchKind of { tags : 'a }
    | Error of { tags : 'a }
    | Integer of { tags : 'a }
    | IntType of { tags : 'a; expr : Expression.t }
    | BitType of { tags : 'a; expr : Expression.t }
    | VarBit of { tags : 'a; expr : Expression.t }
    | TypeName of { tags : 'a; name : Name.t }
    | SpecializedType of { tags : 'a; base : t; args : t list }
    | HeaderStack of { tags : 'a; header : t; size : Expression.t }
    | List of { tags : 'a; typ : t }
    | Tuple of { tags : 'a; args : t list }
    | String of { tags : 'a }
    | Void of { tags : 'a }
    | DontCare of { tags : 'a }

  val tags : 'a t' -> 'a
end = struct
  type t = Source.info t'

  and 'a t' =
    | Bool of { tags : 'a }
    | MatchKind of { tags : 'a }
    | Error of { tags : 'a }
    | Integer of { tags : 'a }
    | IntType of { tags : 'a; expr : Expression.t }
    | BitType of { tags : 'a; expr : Expression.t }
    | VarBit of { tags : 'a; expr : Expression.t }
    | TypeName of { tags : 'a; name : Name.t }
    | SpecializedType of { tags : 'a; base : t; args : t list }
    | HeaderStack of { tags : 'a; header : t; size : Expression.t }
    | List of { tags : 'a; typ : t }
    | Tuple of { tags : 'a; args : t list }
    | String of { tags : 'a }
    | Void of { tags : 'a }
    | DontCare of { tags : 'a }

  let tags (t : 'a t') : 'a =
    match t with
    | Bool { tags }
    | MatchKind { tags }
    | Error { tags }
    | Integer { tags }
    | IntType { tags; _ }
    | BitType { tags; _ }
    | VarBit { tags; _ }
    | TypeName { tags; _ }
    | SpecializedType { tags; _ }
    | HeaderStack { tags; _ }
    | List { tags; _ }
    | Tuple { tags; _ }
    | String { tags }
    | Void { tags }
    | DontCare { tags } ->
        tags
end

(* Arguments and Parameters *)
and Argument : sig
  type t = Source.info t'

  and 'a t' =
    | Expression of { tags : 'a; value : Expression.t }
    | KeyValue of { tags : 'a; key : Text.t; value : Expression.t option }
    | Missing of { tags : 'a }
end = struct
  type t = Source.info t'

  and 'a t' =
    | Expression of { tags : 'a; value : Expression.t }
    | KeyValue of { tags : 'a; key : Text.t; value : Expression.t option }
    | Missing of { tags : 'a }
end

and Parameter : sig
  type t = Source.info t'

  and 'a t' = {
    tags : 'a;
    annotations : Annotation.t list;
    direction : Direction.t option;
    typ : Type.t;
    variable : Text.t;
    opt_value : Expression.t option;
  }
end = struct
  type t = Source.info t'

  and 'a t' = {
    tags : 'a;
    annotations : Annotation.t list;
    direction : Direction.t option;
    typ : Type.t;
    variable : Text.t;
    opt_value : Expression.t option;
  }
end

(* Annotations *)
and Annotation : sig
  type body = Source.info body'

  and 'a body' =
    | Empty of { tags : 'a }
    | Unparsed of { tags : 'a; str : Text.t list }
    | Expression of { tags : 'a; exprs : Expression.t list }
    | KeyValue of { tags : 'a; key_values : KeyValue.t list }

  type t = Source.info t'
  and 'a t' = { tags : 'a; name : Text.t; body : body }
end = struct
  type body = Source.info body'

  and 'a body' =
    | Empty of { tags : 'a }
    | Unparsed of { tags : 'a; str : Text.t list }
    | Expression of { tags : 'a; exprs : Expression.t list }
    | KeyValue of { tags : 'a; key_values : KeyValue.t list }

  type t = Source.info t'
  and 'a t' = { tags : 'a; name : Text.t; body : body }
end

(* Expressions *)
and KeyValue : sig
  type t = Source.info t'
  and 'a t' = { tags : 'a; key : Text.t; value : Expression.t }
end = struct
  type t = Source.info t'
  and 'a t' = { tags : 'a; key : Text.t; value : Expression.t }
end

and Expression : sig
  type t = Source.info t'

  and 'a t' =
    | True of { tags : 'a }
    | False of { tags : 'a }
    | Int of { tags : 'a; i : Number.t }
    | String of { tags : 'a; text : Text.t }
    | Name of { tags : 'a; name : Name.t }
    | Dots of { tags : 'a }
    | ArrayAccess of { tags : 'a; array : t; index : t }
    | BitStringAccess of { tags : 'a; bits : t; lo : t; hi : t }
    | List of { tags : 'a; values : t list }
    | ListDots of { tags : 'a; values : t list }
    | Record of { tags : 'a; entries : KeyValue.t list }
    | RecordDots of { tags : 'a; entries : KeyValue.t list }
    | Invalid of { tags : 'a }
    | UnaryOp of { tags : 'a; op : Op.un; arg : t }
    | BinaryOp of { tags : 'a; op : Op.bin; args : t * t }
    | Cast of { tags : 'a; typ : Type.t; expr : t }
    | TypeMember of { tags : 'a; typ : Name.t; name : Text.t }
    | ErrorMember of { tags : 'a; err : Text.t }
    | ExpressionMember of { tags : 'a; expr : t; name : Text.t }
    | Ternary of { tags : 'a; cond : t; tru : t; fls : t }
    | FunctionCall of {
        tags : 'a;
        func : t;
        type_args : Type.t list;
        args : Argument.t list;
      }
    | NamelessInstantiation of {
        tags : 'a;
        typ : Type.t;
        args : Argument.t list;
      }
    | Mask of { tags : 'a; expr : t; mask : t }
    | Range of { tags : 'a; lo : t; hi : t }

  val tags : 'a t' -> 'a
  val update_tags : 'a t' -> 'a -> 'a t'
end = struct
  type t = Source.info t'

  and 'a t' =
    | True of { tags : 'a }
    | False of { tags : 'a }
    | Int of { tags : 'a; i : Number.t }
    | String of { tags : 'a; text : Text.t }
    | Name of { tags : 'a; name : Name.t }
    | Dots of { tags : 'a }
    | ArrayAccess of { tags : 'a; array : t; index : t }
    | BitStringAccess of { tags : 'a; bits : t; lo : t; hi : t }
    | List of { tags : 'a; values : t list }
    | ListDots of { tags : 'a; values : t list }
    | Record of { tags : 'a; entries : KeyValue.t list }
    | RecordDots of { tags : 'a; entries : KeyValue.t list }
    | Invalid of { tags : 'a }
    | UnaryOp of { tags : 'a; op : Op.un; arg : t }
    | BinaryOp of { tags : 'a; op : Op.bin; args : t * t }
    | Cast of { tags : 'a; typ : Type.t; expr : t }
    | TypeMember of { tags : 'a; typ : Name.t; name : Text.t }
    | ErrorMember of { tags : 'a; err : Text.t }
    | ExpressionMember of { tags : 'a; expr : t; name : Text.t }
    | Ternary of { tags : 'a; cond : t; tru : t; fls : t }
    | FunctionCall of {
        tags : 'a;
        func : t;
        type_args : Type.t list;
        args : Argument.t list;
      }
    | NamelessInstantiation of {
        tags : 'a;
        typ : Type.t;
        args : Argument.t list;
      }
    | Mask of { tags : 'a; expr : t; mask : t }
    | Range of { tags : 'a; lo : t; hi : t }

  let tags (t : 'a t') : 'a =
    match t with
    | True { tags }
    | False { tags }
    | Int { tags; _ }
    | String { tags; _ }
    | Name { tags; _ }
    | Dots { tags }
    | ArrayAccess { tags; _ }
    | BitStringAccess { tags; _ }
    | List { tags; _ }
    | ListDots { tags; _ }
    | Record { tags; _ }
    | RecordDots { tags; _ }
    | Invalid { tags }
    | UnaryOp { tags; _ }
    | BinaryOp { tags; _ }
    | Cast { tags; _ }
    | TypeMember { tags; _ }
    | ErrorMember { tags; _ }
    | ExpressionMember { tags; _ }
    | Ternary { tags; _ }
    | FunctionCall { tags; _ }
    | NamelessInstantiation { tags; _ }
    | Mask { tags; _ }
    | Range { tags; _ } ->
        tags

  let update_tags (t : 'a t') (tags : 'a) : 'a t' =
    match t with
    | True { tags = _ } -> True { tags }
    | False { tags = _ } -> False { tags }
    | Int { i; _ } -> Int { tags; i }
    | String { text; _ } -> String { tags; text }
    | Name { name; _ } -> Name { tags; name }
    | Dots { tags = _ } -> Dots { tags }
    | ArrayAccess { array; index; _ } -> ArrayAccess { tags; array; index }
    | BitStringAccess { bits; lo; hi; _ } ->
        BitStringAccess { tags; bits; lo; hi }
    | List { values; _ } -> List { tags; values }
    | ListDots { values; _ } -> ListDots { tags; values }
    | Record { entries; _ } -> Record { tags; entries }
    | RecordDots { entries; _ } -> RecordDots { tags; entries }
    | Invalid _ -> Invalid { tags }
    | UnaryOp { op; arg; _ } -> UnaryOp { tags; op; arg }
    | BinaryOp { op; args; _ } -> BinaryOp { tags; op; args }
    | Cast { typ; expr; _ } -> Cast { tags; typ; expr }
    | TypeMember { typ; name; _ } -> TypeMember { typ; name; tags }
    | ErrorMember { err; _ } -> ErrorMember { err; tags }
    | ExpressionMember { expr; name; _ } ->
        ExpressionMember { tags; expr; name }
    | Ternary { cond; tru; fls; _ } -> Ternary { tags; cond; tru; fls }
    | FunctionCall { func; type_args; args; _ } ->
        FunctionCall { tags; func; type_args; args }
    | NamelessInstantiation { typ; args; _ } ->
        NamelessInstantiation { tags; typ; args }
    | Mask { expr; mask; _ } -> Mask { tags; expr; mask }
    | Range { lo; hi; _ } -> Range { tags; lo; hi }
end

(* Statements *)
and Statement : sig
  type switch_label = Source.info switch_label'

  and 'a switch_label' =
    | Default of { tags : 'a }
    | Expression of { tags : 'a; expr : Expression.t }

  val tags_label : 'a switch_label' -> 'a

  type switch_case = Source.info switch_case'

  and 'a switch_case' =
    | Action of { tags : 'a; label : switch_label; code : Block.t }
    | FallThrough of { tags : 'a; label : switch_label }

  val tags_case : 'a switch_case' -> 'a

  type t = Source.info t'

  and 'a t' =
    | MethodCall of {
        tags : 'a;
        func : Expression.t;
        type_args : Type.t list;
        args : Argument.t list;
      }
    | Assignment of { tags : 'a; lhs : Expression.t; rhs : Expression.t }
    | DirectApplication of { tags : 'a; typ : Type.t; args : Argument.t list }
    | Conditional of { tags : 'a; cond : Expression.t; tru : t; fls : t option }
    | BlockStatement of { tags : 'a; block : Block.t }
    | Exit of { tags : 'a }
    | EmptyStatement of { tags : 'a }
    | Return of { tags : 'a; expr : Expression.t option }
    | Switch of { tags : 'a; expr : Expression.t; cases : switch_case list }
    | DeclarationStatement of { tags : 'a; decl : Declaration.t }

  val tags : 'a t' -> 'a
end = struct
  type switch_label = Source.info switch_label'

  and 'a switch_label' =
    | Default of { tags : 'a }
    | Expression of { tags : 'a; expr : Expression.t }

  let tags_label (t : 'a switch_label') : 'a =
    match t with Default { tags } | Expression { tags; _ } -> tags

  type switch_case = Source.info switch_case'

  and 'a switch_case' =
    | Action of { tags : 'a; label : switch_label; code : Block.t }
    | FallThrough of { tags : 'a; label : switch_label }

  let tags_case (t : 'a switch_case') : 'a =
    match t with Action { tags; _ } | FallThrough { tags; _ } -> tags

  type t = Source.info t'

  and 'a t' =
    | MethodCall of {
        tags : 'a;
        func : Expression.t;
        type_args : Type.t list;
        args : Argument.t list;
      }
    | Assignment of { tags : 'a; lhs : Expression.t; rhs : Expression.t }
    | DirectApplication of { tags : 'a; typ : Type.t; args : Argument.t list }
    | Conditional of { tags : 'a; cond : Expression.t; tru : t; fls : t option }
    | BlockStatement of { tags : 'a; block : Block.t }
    | Exit of { tags : 'a }
    | EmptyStatement of { tags : 'a }
    | Return of { tags : 'a; expr : Expression.t option }
    | Switch of { tags : 'a; expr : Expression.t; cases : switch_case list }
    | DeclarationStatement of { tags : 'a; decl : Declaration.t }

  let tags (t : 'a t') : 'a =
    match t with
    | MethodCall { tags; _ }
    | Assignment { tags; _ }
    | DirectApplication { tags; _ }
    | Conditional { tags; _ }
    | BlockStatement { tags; _ }
    | Exit { tags }
    | EmptyStatement { tags }
    | Return { tags; _ }
    | Switch { tags; _ }
    | DeclarationStatement { tags; _ } ->
        tags
end

and Block : sig
  type t = Source.info t'

  and 'a t' = {
    tags : 'a;
    annotations : Annotation.t list;
    statements : Statement.t list;
  }
end = struct
  type t = Source.info t'

  and 'a t' = {
    tags : 'a;
    annotations : Annotation.t list;
    statements : Statement.t list;
  }
end

(* Matches *)
and Match : sig
  type t = Source.info t'

  and 'a t' =
    | Default of { tags : 'a }
    | DontCare of { tags : 'a }
    | Expression of { tags : 'a; expr : Expression.t }

  val tags : 'a t' -> 'a
end = struct
  type t = Source.info t'

  and 'a t' =
    | Default of { tags : 'a }
    | DontCare of { tags : 'a }
    | Expression of { tags : 'a; expr : Expression.t }

  let tags (t : 'a t') : 'a =
    match t with
    | Default { tags } | DontCare { tags } | Expression { tags; _ } -> tags
end

(* Parsers *)
and Parser : sig
  type case = Source.info case'
  and 'a case' = { tags : 'a; matches : Match.t list; next : Text.t }

  type transition = Source.info transition'

  and 'a transition' =
    | Direct of { tags : 'a; next : Text.t }
    | Select of { tags : 'a; exprs : Expression.t list; cases : case list }

  val transition_tags : 'a transition' -> 'a
  val update_transition_tags : 'a transition' -> 'a -> 'a transition'

  type state = Source.info state'

  and 'a state' = {
    tags : 'a;
    annotations : Annotation.t list;
    name : Text.t;
    statements : Statement.t list;
    transition : transition;
  }
end = struct
  type case = Source.info case'
  and 'a case' = { tags : 'a; matches : Match.t list; next : Text.t }

  type transition = Source.info transition'

  and 'a transition' =
    | Direct of { tags : 'a; next : Text.t }
    | Select of { tags : 'a; exprs : Expression.t list; cases : case list }

  let transition_tags (t : 'a transition') : 'a =
    match t with Direct { tags; _ } | Select { tags; _ } -> tags

  let update_transition_tags (t : 'a transition') (tags : 'a) : 'a transition' =
    match t with
    | Direct { next; _ } -> Direct { tags; next }
    | Select { exprs; cases; _ } -> Select { tags; exprs; cases }

  type state = Source.info state'

  and 'a state' = {
    tags : 'a;
    annotations : Annotation.t list;
    name : Text.t;
    statements : Statement.t list;
    transition : transition;
  }
end

(* Tables *)
and Table : sig
  type action_ref = Source.info action_ref'

  and 'a action_ref' = {
    tags : 'a;
    annotations : Annotation.t list;
    name : Name.t;
    args : Argument.t list;
  }

  type key = Source.info key'

  and 'a key' = {
    tags : 'a;
    annotations : Annotation.t list;
    key : Expression.t;
    match_kind : Text.t;
  }

  type entry = Source.info entry'

  and 'a entry' = {
    tags : 'a;
    annotations : Annotation.t list;
    matches : Match.t list;
    action : action_ref;
    priority : Expression.t option;
    const : bool;
  }

  type property = Source.info property'

  and 'a property' =
    | Key of { tags : 'a; keys : key list }
    | Actions of { tags : 'a; actions : action_ref list }
    | Entries of {
        tags : 'a;
        annotations : Annotation.t list;
        entries : entry list;
        const : bool;
      }
    | DefaultAction of { tags : 'a; action : action_ref; const : bool }
    | Custom of {
        tags : 'a;
        annotations : Annotation.t list;
        const : bool;
        name : Text.t;
        value : Expression.t;
      }
end = struct
  type action_ref = Source.info action_ref'

  and 'a action_ref' = {
    tags : 'a;
    annotations : Annotation.t list;
    name : Name.t;
    args : Argument.t list;
  }

  type key = Source.info key'

  and 'a key' = {
    tags : 'a;
    annotations : Annotation.t list;
    key : Expression.t;
    match_kind : Text.t;
  }

  type entry = Source.info entry'

  and 'a entry' = {
    tags : 'a;
    annotations : Annotation.t list;
    matches : Match.t list;
    action : action_ref;
    priority : Expression.t option;
    const : bool;
  }

  type property = Source.info property'

  and 'a property' =
    | Key of { tags : 'a; keys : key list }
    | Actions of { tags : 'a; actions : action_ref list }
    | Entries of {
        tags : 'a;
        annotations : Annotation.t list;
        entries : entry list;
        const : bool;
      }
    | DefaultAction of { tags : 'a; action : action_ref; const : bool }
    | Custom of {
        tags : 'a;
        annotations : Annotation.t list;
        const : bool;
        name : Text.t;
        value : Expression.t;
      }
end

(* Methods *)
and MethodPrototype : sig
  type t = Source.info t'

  and 'a t' =
    | Constructor of {
        tags : 'a;
        annotations : Annotation.t list;
        name : Text.t;
        params : Parameter.t list;
      }
    | AbstractMethod of {
        tags : 'a;
        annotations : Annotation.t list;
        return : Type.t;
        name : Text.t;
        type_params : Text.t list;
        params : Parameter.t list;
      }
    | Method of {
        tags : 'a;
        annotations : Annotation.t list;
        return : Type.t;
        name : Text.t;
        type_params : Text.t list;
        params : Parameter.t list;
      }
end = struct
  type t = Source.info t'

  and 'a t' =
    | Constructor of {
        tags : 'a;
        annotations : Annotation.t list;
        name : Text.t;
        params : Parameter.t list;
      }
    | AbstractMethod of {
        tags : 'a;
        annotations : Annotation.t list;
        return : Type.t;
        name : Text.t;
        type_params : Text.t list;
        params : Parameter.t list;
      }
    | Method of {
        tags : 'a;
        annotations : Annotation.t list;
        return : Type.t;
        name : Text.t;
        type_params : Text.t list;
        params : Parameter.t list;
      }
end

(* Declarations *)
and Declaration : sig
  type field = Source.info field'

  and 'a field' = {
    tags : 'a;
    annotations : Annotation.t list;
    typ : Type.t;
    name : Text.t;
  }

  and t = Source.info t'

  and 'a t' =
    | Constant of {
        tags : 'a;
        annotations : Annotation.t list;
        typ : Type.t;
        name : Text.t;
        value : Expression.t;
      }
    | Instantiation of {
        tags : 'a;
        annotations : Annotation.t list;
        typ : Type.t;
        args : Argument.t list;
        name : Text.t;
        init : t list;
      }
    | Parser of {
        tags : 'a;
        annotations : Annotation.t list;
        name : Text.t;
        type_params : Text.t list;
        params : Parameter.t list;
        constructor_params : Parameter.t list;
        locals : t list;
        states : Parser.state list;
      }
    | Control of {
        tags : 'a;
        annotations : Annotation.t list;
        name : Text.t;
        type_params : Text.t list;
        params : Parameter.t list;
        constructor_params : Parameter.t list;
        locals : t list;
        apply : Block.t;
      }
    | Function of {
        tags : 'a;
        annotations : Annotation.t list;
        return : Type.t;
        name : Text.t;
        type_params : Text.t list;
        params : Parameter.t list;
        body : Block.t;
      }
    | ExternFunction of {
        tags : 'a;
        annotations : Annotation.t list;
        return : Type.t;
        name : Text.t;
        type_params : Text.t list;
        params : Parameter.t list;
      }
    | Variable of {
        tags : 'a;
        annotations : Annotation.t list;
        typ : Type.t;
        name : Text.t;
        init : Expression.t option;
      }
    | ValueSet of {
        tags : 'a;
        annotations : Annotation.t list;
        typ : Type.t;
        size : Expression.t;
        name : Text.t;
      }
    | Action of {
        tags : 'a;
        annotations : Annotation.t list;
        name : Text.t;
        params : Parameter.t list;
        body : Block.t;
      }
    | Table of {
        tags : 'a;
        annotations : Annotation.t list;
        name : Text.t;
        properties : Table.property list;
      }
    | Header of {
        tags : 'a;
        annotations : Annotation.t list;
        name : Text.t;
        type_params : Text.t list;
        fields : field list;
      }
    | HeaderUnion of {
        tags : 'a;
        annotations : Annotation.t list;
        name : Text.t;
        type_params : Text.t list;
        fields : field list;
      }
    | Struct of {
        tags : 'a;
        annotations : Annotation.t list;
        name : Text.t;
        type_params : Text.t list;
        fields : field list;
      }
    | Error of { tags : 'a; members : Text.t list }
    | MatchKind of { tags : 'a; members : Text.t list }
    | Enum of {
        tags : 'a;
        annotations : Annotation.t list;
        name : Text.t;
        members : Text.t list;
      }
    | SerializableEnum of {
        tags : 'a;
        annotations : Annotation.t list;
        typ : Type.t;
        name : Text.t;
        members : (Text.t * Expression.t) list;
      }
    | ExternObject of {
        tags : 'a;
        annotations : Annotation.t list;
        name : Text.t;
        type_params : Text.t list;
        methods : MethodPrototype.t list;
      }
    | TypeDef of {
        tags : 'a;
        annotations : Annotation.t list;
        name : Text.t;
        typ_or_decl : (Type.t, t) alternative;
      }
    | NewType of {
        tags : 'a;
        annotations : Annotation.t list;
        name : Text.t;
        typ_or_decl : (Type.t, t) alternative;
      }
    | ControlType of {
        tags : 'a;
        annotations : Annotation.t list;
        name : Text.t;
        type_params : Text.t list;
        params : Parameter.t list;
      }
    | ParserType of {
        tags : 'a;
        annotations : Annotation.t list;
        name : Text.t;
        type_params : Text.t list;
        params : Parameter.t list;
      }
    | PackageType of {
        tags : 'a;
        annotations : Annotation.t list;
        name : Text.t;
        type_params : Text.t list;
        params : Parameter.t list;
      }

  val tags : 'a t' -> 'a
  val name : t -> Text.t
  val has_type_params : t -> bool
end = struct
  type field = Source.info field'

  and 'a field' = {
    tags : 'a;
    annotations : Annotation.t list;
    typ : Type.t;
    name : Text.t;
  }

  and t = Source.info t'

  and 'a t' =
    | Constant of {
        tags : 'a;
        annotations : Annotation.t list;
        typ : Type.t;
        name : Text.t;
        value : Expression.t;
      }
    | Instantiation of {
        tags : 'a;
        annotations : Annotation.t list;
        typ : Type.t;
        args : Argument.t list;
        name : Text.t;
        init : t list;
      }
    | Parser of {
        tags : 'a;
        annotations : Annotation.t list;
        name : Text.t;
        type_params : Text.t list;
        params : Parameter.t list;
        constructor_params : Parameter.t list;
        locals : t list;
        states : Parser.state list;
      }
    | Control of {
        tags : 'a;
        annotations : Annotation.t list;
        name : Text.t;
        type_params : Text.t list;
        params : Parameter.t list;
        constructor_params : Parameter.t list;
        locals : t list;
        apply : Block.t;
      }
    | Function of {
        tags : 'a;
        annotations : Annotation.t list;
        return : Type.t;
        name : Text.t;
        type_params : Text.t list;
        params : Parameter.t list;
        body : Block.t;
      }
    | ExternFunction of {
        tags : 'a;
        annotations : Annotation.t list;
        return : Type.t;
        name : Text.t;
        type_params : Text.t list;
        params : Parameter.t list;
      }
    | Variable of {
        tags : 'a;
        annotations : Annotation.t list;
        typ : Type.t;
        name : Text.t;
        init : Expression.t option;
      }
    | ValueSet of {
        tags : 'a;
        annotations : Annotation.t list;
        typ : Type.t;
        size : Expression.t;
        name : Text.t;
      }
    | Action of {
        tags : 'a;
        annotations : Annotation.t list;
        name : Text.t;
        params : Parameter.t list;
        body : Block.t;
      }
    | Table of {
        tags : 'a;
        annotations : Annotation.t list;
        name : Text.t;
        properties : Table.property list;
      }
    | Header of {
        tags : 'a;
        annotations : Annotation.t list;
        name : Text.t;
        type_params : Text.t list;
        fields : field list;
      }
    | HeaderUnion of {
        tags : 'a;
        annotations : Annotation.t list;
        name : Text.t;
        type_params : Text.t list;
        fields : field list;
      }
    | Struct of {
        tags : 'a;
        annotations : Annotation.t list;
        name : Text.t;
        type_params : Text.t list;
        fields : field list;
      }
    | Error of { tags : 'a; members : Text.t list }
    | MatchKind of { tags : 'a; members : Text.t list }
    | Enum of {
        tags : 'a;
        annotations : Annotation.t list;
        name : Text.t;
        members : Text.t list;
      }
    | SerializableEnum of {
        tags : 'a;
        annotations : Annotation.t list;
        typ : Type.t;
        name : Text.t;
        members : (Text.t * Expression.t) list;
      }
    | ExternObject of {
        tags : 'a;
        annotations : Annotation.t list;
        name : Text.t;
        type_params : Text.t list;
        methods : MethodPrototype.t list;
      }
    | TypeDef of {
        tags : 'a;
        annotations : Annotation.t list;
        name : Text.t;
        typ_or_decl : (Type.t, t) alternative;
      }
    | NewType of {
        tags : 'a;
        annotations : Annotation.t list;
        name : Text.t;
        typ_or_decl : (Type.t, t) alternative;
      }
    | ControlType of {
        tags : 'a;
        annotations : Annotation.t list;
        name : Text.t;
        type_params : Text.t list;
        params : Parameter.t list;
      }
    | ParserType of {
        tags : 'a;
        annotations : Annotation.t list;
        name : Text.t;
        type_params : Text.t list;
        params : Parameter.t list;
      }
    | PackageType of {
        tags : 'a;
        annotations : Annotation.t list;
        name : Text.t;
        type_params : Text.t list;
        params : Parameter.t list;
      }

  let tags (t : 'a t') : 'a =
    match t with
    | Constant { tags; _ }
    | Instantiation { tags; _ }
    | Parser { tags; _ }
    | Control { tags; _ }
    | Function { tags; _ }
    | ExternFunction { tags; _ }
    | Variable { tags; _ }
    | ValueSet { tags; _ }
    | Action { tags; _ }
    | Table { tags; _ }
    | Header { tags; _ }
    | HeaderUnion { tags; _ }
    | Struct { tags; _ }
    | Enum { tags; _ }
    | SerializableEnum { tags; _ }
    | ExternObject { tags; _ }
    | TypeDef { tags; _ }
    | NewType { tags; _ }
    | ControlType { tags; _ }
    | ParserType { tags; _ }
    | PackageType { tags; _ }
    | Error { tags; _ }
    | MatchKind { tags; _ } ->
        tags

  let name t =
    match t with
    | Constant { name; _ }
    | Instantiation { name; _ }
    | Parser { name; _ }
    | Control { name; _ }
    | Function { name; _ }
    | ExternFunction { name; _ }
    | Variable { name; _ }
    | ValueSet { name; _ }
    | Action { name; _ }
    | Table { name; _ }
    | Header { name; _ }
    | HeaderUnion { name; _ }
    | Struct { name; _ }
    | Enum { name; _ }
    | SerializableEnum { name; _ }
    | ExternObject { name; _ }
    | TypeDef { name; _ }
    | NewType { name; _ }
    | ControlType { name; _ }
    | ParserType { name; _ }
    | PackageType { name; _ } ->
        name
    | Error _ | MatchKind _ -> failwith "no name"

  let has_type_params t =
    match t with
    | Constant _ | Instantiation _ | Parser _ | Control _ | Variable _
    | ValueSet _ | Action _ | Table _ | Enum _ | SerializableEnum _
    | MatchKind _ | Error _ | TypeDef _ | NewType _ ->
        false
    | Header { type_params; _ }
    | HeaderUnion { type_params; _ }
    | Struct { type_params; _ }
    | Function { type_params; _ }
    | ExternFunction { type_params; _ }
    | ExternObject { type_params; _ }
    | ControlType { type_params; _ }
    | ParserType { type_params; _ }
    | PackageType { type_params; _ } ->
        List.length type_params > 0
end

(* Program *)

type p4program = Program of Declaration.t list
