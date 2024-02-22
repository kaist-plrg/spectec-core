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

open Util

module rec KeyValue : sig 
  type t = Info.t t' 
  and 'a t' =
    { tags : 'a;
      key : Text.t;
      value : Expression.t }
end = struct
  type t = Info.t t' 
  and 'a t' =
    { tags : 'a;
      key : Text.t;
      value : Expression.t }
end

and Annotation : sig 
  type body = Info.t body' 
  and 'a body' =
    | Empty of 
        { tags: 'a }
    | Unparsed of 
        { tags: 'a; 
          str: Text.t list }
    | Expression of 
        { tags: 'a; 
          exprs: Expression.t list }
    | KeyValue of 
        { tags: 'a; 
          k_v: KeyValue.t list }

  type t = Info.t t'
  and 'a t' =
    { tags: 'a;
      name: Text.t;
      body: body }
end = struct
  type body = Info.t body'
  and 'a body' =
    | Empty of 
        { tags: 'a }
    | Unparsed of 
        { tags: 'a; 
          str: Text.t list }
    | Expression of 
        { tags: 'a; 
          exprs: Expression.t list }
    | KeyValue of 
        { tags: 'a; 
          k_v: KeyValue.t list }

  type t = Info.t t'
  and 'a t' =
    { tags: 'a;
      name: Text.t;
      body: body }
end

and Parameter : sig 
  type t = Info.t t'
  and 'a t' =
    { tags: 'a;
      annotations: Annotation.t list;
      direction: Direction.t option;
      typ: Type.t;
      variable: Text.t;
      opt_value: Expression.t option } 
end = struct
  type t = Info.t t'
  and 'a t' =
    { tags: 'a;
      annotations: Annotation.t list;
      direction: Direction.t option;
      typ: Type.t;
      variable: Text.t;
      opt_value: Expression.t option }
end

and Op : sig 
  type un = Info.t un'
  and 'a un' =
    | Not of { tags: 'a }
    | BitNot of { tags: 'a }
    | UMinus of { tags: 'a } 

  type bin = Info.t bin'
  and 'a bin' =
    | Plus of { tags: 'a }
    | PlusSat of { tags: 'a }
    | Minus of { tags: 'a }
    | MinusSat of { tags: 'a }
    | Mul of { tags: 'a }
    | Div of { tags: 'a }
    | Mod of { tags: 'a }
    | Shl of { tags: 'a }
    | Shr of { tags: 'a }
    | Le of { tags: 'a }
    | Ge of { tags: 'a }
    | Lt of { tags: 'a }
    | Gt of { tags: 'a }
    | Eq of { tags: 'a }
    | NotEq of { tags: 'a }
    | BitAnd of { tags: 'a }
    | BitXor of { tags: 'a }
    | BitOr of { tags: 'a }
    | PlusPlus of { tags: 'a }
    | And of { tags: 'a }
    | Or of { tags: 'a }
end = struct
  type un = Info.t un'
  and 'a un' =
    | Not of { tags: 'a }
    | BitNot of { tags: 'a }
    | UMinus of { tags: 'a } 

  type bin = Info.t bin'
  and 'a bin' =
    | Plus of { tags: 'a }
    | PlusSat of { tags: 'a }
    | Minus of { tags: 'a }
    | MinusSat of { tags: 'a }
    | Mul of { tags: 'a }
    | Div of { tags: 'a }
    | Mod of { tags: 'a }
    | Shl of { tags: 'a }
    | Shr of { tags: 'a }
    | Le of { tags: 'a }
    | Ge of { tags: 'a }
    | Lt of { tags: 'a }
    | Gt of { tags: 'a }
    | Eq of { tags: 'a }
    | NotEq of { tags: 'a }
    | BitAnd of { tags: 'a }
    | BitXor of { tags: 'a }
    | BitOr of { tags: 'a }
    | PlusPlus of { tags: 'a }
    | And of { tags: 'a }
    | Or of { tags: 'a }
end

and Type : sig 
  type t = Info.t t'
  and 'a t' =
    | Bool of { tags: 'a } 
    | Error of { tags: 'a } 
    | Integer of { tags: 'a } 
    | IntType of 
        { tags: 'a;
          expr: Expression.t } 
    | BitType of 
        { tags: 'a;
          expr: Expression.t } 
    | VarBit of 
        { tags: 'a;
          expr: Expression.t } 
    | TypeName of 
        { tags: 'a;
          name: string }
    | SpecializedType of
        { tags: 'a;
          base: t;
          args: t list } 
    | HeaderStack of
        { tags: 'a;
          header: t;
          size:  Expression.t } 
    | Tuple of 
        { tags: 'a;
          args: t list }
    | String of { tags: 'a } 
    | Void of { tags: 'a } 
    | DontCare of { tags: 'a } 
end = struct
  type t = Info.t t'
  and 'a t' =
    | Bool of { tags: 'a } 
    | Error of { tags: 'a } 
    | Integer of { tags: 'a } 
    | IntType of 
        { tags: 'a;
          expr: Expression.t } 
    | BitType of 
        { tags: 'a;
          expr: Expression.t } 
    | VarBit of 
        { tags: 'a;
          expr: Expression.t } 
    | TypeName of 
        { tags: 'a;
          name: string }
    | SpecializedType of
        { tags: 'a;
          base: t;
          args: t list } 
    | HeaderStack of
        { tags: 'a;
          header: t;
          size:  Expression.t } 
    | Tuple of 
        { tags: 'a;
          args: t list }
    | String of { tags: 'a } 
    | Void of { tags: 'a } 
    | DontCare of { tags: 'a } 
end

and MethodPrototype : sig 
  type t = Info.t t'
  and 'a t' =
    | Constructor of
        { tags: 'a;
          annotations: Annotation.t list;
          name: Text.t;
          params: Parameter.t list }
    | AbstractMethod of
        { tags: 'a;
          annotations: Annotation.t list;
          return: Type.t;
          name: Text.t;
          type_params: Text.t list;
          params: Parameter.t list }
    | Method of
        { tags: 'a;
          annotations: Annotation.t list;
          return: Type.t;
          name: Text.t;
          type_params: Text.t list;
          params: Parameter.t list }
end = struct
  type t = Info.t t'
  and 'a t' =
    | Constructor of
        { tags: 'a;
          annotations: Annotation.t list;
          name: Text.t;
          params: Parameter.t list }
    | AbstractMethod of
        { tags: 'a;
          annotations: Annotation.t list;
          return: Type.t;
          name: Text.t;
          type_params: Text.t list;
          params: Parameter.t list }
    | Method of
        { tags: 'a;
          annotations: Annotation.t list;
          return: Type.t;
          name: Text.t;
          type_params: Text.t list;
          params: Parameter.t list }
end

and Argument : sig 
  type t = Info.t t'
  and 'a t' =
    | Expression of
        { tags: 'a;
          value: Expression.t }
    | KeyValue of
        { tags: 'a;
          key: Text.t;
          value: Expression.t }
    | Missing of { tags: 'a }
end = struct
  type t = Info.t t'
  and 'a t' =
    | Expression of
        { tags: 'a;
          value: Expression.t }
    | KeyValue of
        { tags: 'a;
          key: Text.t;
          value: Expression.t }
    | Missing of { tags: 'a }
end

and Direction : sig 
  type t = Info.t t'
  and 'a t' =
    | In of { tags: 'a }
    | Out of { tags: 'a }
    | InOut of { tags: 'a }
end = struct
  type t = Info.t t'
  and 'a t' =
    | In of { tags: 'a }
    | Out of { tags: 'a }
    | InOut of { tags: 'a }
end

and Expression : sig 
  type t = Info.t t'
  and 'a t' =
    | True of { tags: 'a } 
    | False of { tags: 'a } 
    | Int of 
        { tags: 'a;
          i: Number.t } 
    | String of 
        { tags: 'a;
          str: Text.t } 
    | Name of 
        { tags: 'a;
          name: string } 
    | ArrayAccess of
        { tags: 'a;
          array: t;
          index: t } 
    | BitStringAccess of
        { tags: 'a;
          bits: t;
          lo: t;
          hi: t }
    | List of
        { tags: 'a;
          values: t list } 
    | Record of
        { tags: 'a;
          entries: KeyValue.t list } 
    | UnaryOp of
        { tags: 'a;
          op: Op.un;
          arg: t } 
    | BinaryOp of
        { tags: 'a;
          op: Op.bin;
          args: (t * t) } 
    | Cast of
        { tags: 'a;
          typ: Type.t;
          expr: t }
    | TypeMember of
        { tags: 'a;
          typ: string;
          name: Text.t } 
    | ErrorMember of 
        { tags: 'a;
          err: Text.t } 
    | ExpressionMember of
        { tags: 'a;
          expr: t;
          name: Text.t } 
    | Ternary of
        { tags: 'a;
          cond: t;
          tru: t;
          fls: t } 
    | FunctionCall of
        { tags: 'a;
          func: t;
          type_args: Type.t list;
          args: Argument.t list } 
    | NamelessInstantiation of
        { tags: 'a;
          typ: Type.t;
          args: Argument.t list } 
    | Mask of
        { tags: 'a;
          expr: t;
          mask: t } 
    | Range of
        { tags: 'a;
          lo: t;
          hi: t }
end = struct
  type t = Info.t t'
  and 'a t' =
    | True of { tags: 'a } 
    | False of { tags: 'a } 
    | Int of 
        { tags: 'a;
          i: Number.t } 
    | String of 
        { tags: 'a;
          str: Text.t } 
    | Name of 
        { tags: 'a;
          name: string } 
    | ArrayAccess of
        { tags: 'a;
          array: t;
          index: t } 
    | BitStringAccess of
        { tags: 'a;
          bits: t;
          lo: t;
          hi: t }
    | List of
        { tags: 'a;
          values: t list } 
    | Record of
        { tags: 'a;
          entries: KeyValue.t list } 
    | UnaryOp of
        { tags: 'a;
          op: Op.un;
          arg: t } 
    | BinaryOp of
        { tags: 'a;
          op: Op.bin;
          args: (t * t) } 
    | Cast of
        { tags: 'a;
          typ: Type.t;
          expr: t }
    | TypeMember of
        { tags: 'a;
          typ: string;
          name: Text.t } 
    | ErrorMember of 
        { tags: 'a;
          err: Text.t } 
    | ExpressionMember of
        { tags: 'a;
          expr: t;
          name: Text.t } 
    | Ternary of
        { tags: 'a;
          cond: t;
          tru: t;
          fls: t } 
    | FunctionCall of
        { tags: 'a;
          func: t;
          type_args: Type.t list;
          args: Argument.t list } 
    | NamelessInstantiation of
        { tags: 'a;
          typ: Type.t;
          args: Argument.t list } 
    | Mask of
        { tags: 'a;
          expr: t;
          mask: t } 
    | Range of
        { tags: 'a;
          lo: t;
          hi: t }
end

and Table : sig 
  type action_ref = Info.t action_ref'
  and 'a action_ref' =
    { tags: 'a;
      annotations: Annotation.t list;
      name: string;
      args: Argument.t list }

  type key = Info.t key'
  and 'a key' =
    { tags: 'a;
      annotations: Annotation.t list;
      key: Expression.t;
      match_kind: Text.t }

  type entry = Info.t entry'
  and 'a entry' =
    { tags: 'a;
      annotations: Annotation.t list;
      matches: Match.t list;
      action: action_ref }

  type property = Info.t property'
  and 'a property' =
    | Key of
        { tags: 'a;
          keys: key list }
    | Actions of
        { tags: 'a;
          actions: action_ref list }
    | Entries of
        { tags: 'a;
          entries: entry list }
    | DefaultAction of
        { tags: 'a;
          action: action_ref;
          const: bool }
    | Custom of
        { tags: 'a;
          annotations: Annotation.t list;
          const: bool;
          name: Text.t;
          value: Expression.t }
end = struct
  type action_ref = Info.t action_ref'
  and 'a action_ref' =
    { tags: 'a;
      annotations: Annotation.t list;
      name: string;
      args: Argument.t list }

  type key = Info.t key'
  and 'a key' =
    { tags: 'a;
      annotations: Annotation.t list;
      key: Expression.t;
      match_kind: Text.t }

  type entry = Info.t entry'
  and 'a entry' =
    { tags: 'a;
      annotations: Annotation.t list;
      matches: Match.t list;
      action: action_ref }

  type property = Info.t property'
  and 'a property' =
    | Key of
        { tags: 'a;
          keys: key list }
    | Actions of
        { tags: 'a;
          actions: action_ref list }
    | Entries of
        { tags: 'a;
          entries: entry list }
    | DefaultAction of
        { tags: 'a;
          action: action_ref;
          const: bool }
    | Custom of
        { tags: 'a;
          annotations: Annotation.t list;
          const: bool;
          name: Text.t;
          value: Expression.t }
end

and Match : sig 
  type t = Info.t t'
  and 'a t' =
    | Default of { tags: 'a }
    | DontCare of { tags: 'a }
    | Expression of
        { tags: 'a;
          expr: Expression.t }
end = struct
  type t = Info.t t'
  and 'a t' =
    | Default of { tags: 'a }
    | DontCare of { tags: 'a }
    | Expression of
        { tags: 'a;
          expr: Expression.t }
end

and Parser : sig
  type case = Info.t case'
  and 'a case' =
    { tags: 'a;
      matches: Match.t list;
      next: Text.t } 

  type transition = Info.t transition'
  and 'a transition' =
    | Direct of
        { tags: 'a;
          next: Text.t }
    | Select of
        { tags: 'a;
          exprs: Expression.t list;
          cases: case list }

  type state = Info.t state'
  and 'a state' =
    { tags: 'a;
      annotations: Annotation.t list;
      name: Text.t;
      statements: Statement.t list;
      transition: transition }
end = struct
  type case = Info.t case'
  and 'a case' =
    { tags: 'a;
      matches: Match.t list;
      next: Text.t } 

  type transition = Info.t transition'
  and 'a transition' =
    | Direct of
        { tags: 'a;
          next: Text.t }
    | Select of
        { tags: 'a;
          exprs: Expression.t list;
          cases: case list }

  type state = Info.t state'
  and 'a state' =
    { tags: 'a;
      annotations: Annotation.t list;
      name: Text.t;
      statements: Statement.t list;
      transition: transition }
end

and Declaration : sig 
  type t = Info.t t'
  and 'a t' =
    | Constant of
        { tags: 'a;
          annotations: Annotation.t list;
          typ: Type.t;
          name: Text.t;
          value: Expression.t }
    | Instantiation of
        { tags: 'a;
          annotations: Annotation.t list;
          typ: Type.t;
          args: Argument.t list;
          name: Text.t;
          init: Block.t option; }
    | Parser of
        { tags: 'a;
          annotations: Annotation.t list;
          name: Text.t;
          type_params: Text.t list;
          params: Parameter.t list;
          constructor_params: Parameter.t list;
          locals: t list;
          states: Parser.state list }
    | Control of
        { tags: 'a;
          annotations: Annotation.t list;
          name: Text.t;
          type_params: Text.t list;
          params: Parameter.t list;
          constructor_params: Parameter.t list;
          locals: t list;
          apply: Block.t }
    | Function of
        { tags: 'a;
          return: Type.t;
          name: Text.t;
          type_params: Text.t list;
          params: Parameter.t list;
          body: Block.t }
    | ExternFunction of
        { tags: 'a;
          annotations: Annotation.t list;
          return: Type.t;
          name: string;
          type_params: string list;
          params: Parameter.t list }
    | Variable of
        { tags: 'a;
          annotations: Annotation.t list;
          typ: Type.t;
          name: Text.t;
          init: Expression.t option }
    | ValueSet of
        { tags: 'a;
          annotations: Annotation.t list;
          typ: Type.t;
          size: Expression.t;
          name: Text.t }
    | Action of
        { tags: 'a;
          annotations: Annotation.t list;
          name: Text.t;
          params: Parameter.t list;
          body: Block.t }
    | Table of
        { tags: 'a;
          annotations: Annotation.t list;
          name: Text.t;
          properties: Table.property list }
    | Header of
        { tags: 'a;
          annotations: Annotation.t list;
          name: Text.t;
          fields: field list }
    | HeaderUnion of
        { tags: 'a;
          annotations: Annotation.t list;
          name: Text.t;
          fields: field list }
    | Struct of
        { tags: 'a;
          annotations: Annotation.t list;
          name: Text.t;
          fields: field list }
    | Error of
        { tags: 'a;
          members: string list }
    | MatchKind of
        { tags: 'a;
          members: Text.t list }
    | Enum of
        { tags: 'a;
          annotations: Annotation.t list;
          name: Text.t;
          members: Text.t list }
    | SerializableEnum of
        { tags: 'a;
          annotations: Annotation.t list;
          typ: Type.t;
          name: Text.t;
          members: (Text.t * Expression.t) list }
    | ExternObject of
        { tags: 'a;
          annotations: Annotation.t list;
          name: Text.t;
          type_params: Text.t list;
          methods: MethodPrototype.t list }
    | TypeDef of
        { tags: 'a;
          annotations: Annotation.t list;
          name: Text.t;
          typ_or_decl: (Type.t, t) alternative }
    | NewType of
        { tags: 'a;
          annotations: Annotation.t list;
          name: Text.t;
          typ_or_decl: (Type.t, t) alternative }
    | ControlType of
        { tags: 'a;
          annotations: Annotation.t list;
          name: Text.t;
          type_params: Text.t list;
          params: Parameter.t list }
    | ParserType of
        { tags: 'a;
          annotations: Annotation.t list;
          name: Text.t;
          type_params: Text.t list;
          params: Parameter.t list }
    | PackageType of
        { tags: 'a;
          annotations: Annotation.t list;
          name: Text.t;
          type_params: Text.t list;
          params: Parameter.t list }

  and field = Info.t field'
  and 'a field' =
    { tags: 'a; 
      annotations: Annotation.t list;
      typ: Type.t;
      name: Text.t } 
end = struct
  type t = Info.t t'
  and 'a t' =
    | Constant of
        { tags: 'a;
          annotations: Annotation.t list;
          typ: Type.t;
          name: Text.t;
          value: Expression.t }
    | Instantiation of
        { tags: 'a;
          annotations: Annotation.t list;
          typ: Type.t;
          args: Argument.t list;
          name: Text.t;
          init: Block.t option; }
    | Parser of
        { tags: 'a;
          annotations: Annotation.t list;
          name: Text.t;
          type_params: Text.t list;
          params: Parameter.t list;
          constructor_params: Parameter.t list;
          locals: t list;
          states: Parser.state list }
    | Control of
        { tags: 'a;
          annotations: Annotation.t list;
          name: Text.t;
          type_params: Text.t list;
          params: Parameter.t list;
          constructor_params: Parameter.t list;
          locals: t list;
          apply: Block.t }
    | Function of
        { tags: 'a;
          return: Type.t;
          name: Text.t;
          type_params: Text.t list;
          params: Parameter.t list;
          body: Block.t }
    | ExternFunction of
        { tags: 'a;
          annotations: Annotation.t list;
          return: Type.t;
          name: string;
          type_params: string list;
          params: Parameter.t list }
    | Variable of
        { tags: 'a;
          annotations: Annotation.t list;
          typ: Type.t;
          name: Text.t;
          init: Expression.t option }
    | ValueSet of
        { tags: 'a;
          annotations: Annotation.t list;
          typ: Type.t;
          size: Expression.t;
          name: Text.t }
    | Action of
        { tags: 'a;
          annotations: Annotation.t list;
          name: Text.t;
          params: Parameter.t list;
          body: Block.t }
    | Table of
        { tags: 'a;
          annotations: Annotation.t list;
          name: Text.t;
          properties: Table.property list }
    | Header of
        { tags: 'a;
          annotations: Annotation.t list;
          name: Text.t;
          fields: field list }
    | HeaderUnion of
        { tags: 'a;
          annotations: Annotation.t list;
          name: Text.t;
          fields: field list }
    | Struct of
        { tags: 'a;
          annotations: Annotation.t list;
          name: Text.t;
          fields: field list }
    | Error of
        { tags: 'a;
          members: string list }
    | MatchKind of
        { tags: 'a;
          members: Text.t list }
    | Enum of
        { tags: 'a;
          annotations: Annotation.t list;
          name: Text.t;
          members: Text.t list }
    | SerializableEnum of
        { tags: 'a;
          annotations: Annotation.t list;
          typ: Type.t;
          name: Text.t;
          members: (Text.t * Expression.t) list }
    | ExternObject of
        { tags: 'a;
          annotations: Annotation.t list;
          name: Text.t;
          type_params: Text.t list;
          methods: MethodPrototype.t list }
    | TypeDef of
        { tags: 'a;
          annotations: Annotation.t list;
          name: Text.t;
          typ_or_decl: (Type.t, t) alternative }
    | NewType of
        { tags: 'a;
          annotations: Annotation.t list;
          name: Text.t;
          typ_or_decl: (Type.t, t) alternative }
    | ControlType of
        { tags: 'a;
          annotations: Annotation.t list;
          name: Text.t;
          type_params: Text.t list;
          params: Parameter.t list }
    | ParserType of
        { tags: 'a;
          annotations: Annotation.t list;
          name: Text.t;
          type_params: Text.t list;
          params: Parameter.t list }
    | PackageType of
        { tags: 'a;
          annotations: Annotation.t list;
          name: Text.t;
          type_params: Text.t list;
          params: Parameter.t list }

  and field = Info.t field'
  and 'a field' =
    { tags: 'a; 
      annotations: Annotation.t list;
      typ: Type.t;
      name: Text.t }
end

and Statement : sig 
  type switch_label = Info.t switch_label'
  and 'a switch_label' =
    | Default of { tags: 'a } 
    | Name of 
        { tags: 'a;
          name: Text.t } 

  type switch_case = Info.t switch_case'
  and 'a switch_case' =
    | Action of
        { tags: 'a;
          label: switch_label;
          code: Block.t }
    | FallThrough of
        { tags: 'a;
          label: switch_label }

  type t = Info.t t'
  and 'a t' =
    | MethodCall of
        { tags: 'a;
          func: Expression.t;
          type_args: Type.t list;
          args: Argument.t list } 
    | Assignment of
        { tags: 'a;
          lhs: Expression.t;
          rhs: Expression.t } 
    | DirectApplication of
        { tags: 'a;
          typ: Type.t;
          args: Argument.t list } 
    | Conditional of
        { tags: 'a;
          cond: Expression.t;
          tru: t;
          fls: t option } 
    | BlockStatement of
        { tags: 'a;
          block: Block.t } 
    | Exit of { tags: 'a } 
    | EmptyStatement of { tags: 'a } 
    | Return of
        { tags: 'a;
          expr: Expression.t option } 
    | Switch of
        { tags: 'a;
          expr: Expression.t;
          cases: switch_case list } 
    | DeclarationStatement of
        { tags: 'a;
          decl: Declaration.t } 
end = struct
  type switch_label = Info.t switch_label'
  and 'a switch_label' =
    | Default of { tags: 'a } 
    | Name of 
        { tags: 'a;
          name: Text.t } 

  type switch_case = Info.t switch_case'
  and 'a switch_case' =
    | Action of
        { tags: 'a;
          label: switch_label;
          code: Block.t }
    | FallThrough of
        { tags: 'a;
          label: switch_label }

  type t = Info.t t'
  and 'a t' =
    | MethodCall of
        { tags: 'a;
          func: Expression.t;
          type_args: Type.t list;
          args: Argument.t list } 
    | Assignment of
        { tags: 'a;
          lhs: Expression.t;
          rhs: Expression.t } 
    | DirectApplication of
        { tags: 'a;
          typ: Type.t;
          args: Argument.t list } 
    | Conditional of
        { tags: 'a;
          cond: Expression.t;
          tru: t;
          fls: t option } 
    | BlockStatement of
        { tags: 'a;
          block: Block.t } 
    | Exit of { tags: 'a } 
    | EmptyStatement of { tags: 'a } 
    | Return of
        { tags: 'a;
          expr: Expression.t option } 
    | Switch of
        { tags: 'a;
          expr: Expression.t;
          cases: switch_case list } 
    | DeclarationStatement of
        { tags: 'a;
          decl: Declaration.t } 
end

and Block : sig 
  type t = Info.t t'
  and 'a t' =
    { tags: 'a;
      annotations: Annotation.t list;
      statements: Statement.t list }
end = struct
  type t = Info.t t'
  and 'a t' =
    { tags: 'a;
      annotations: Annotation.t list;
      statements: Statement.t list }
end

type program = Program of Declaration.t list
