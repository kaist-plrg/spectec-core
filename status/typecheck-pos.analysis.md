# A. Fix Logic

## 1. Parser Errors

### \[DONE\] (1) ~~Parsing `_` (don't care)~~

```p4
f(x = 1, y = _);
```

Don't care for named argument was added [PR#1074](https://github.com/p4lang/p4-spec/pull/1074).

```ocaml
argument:
| value = expression
    { let tags = Expression.tags value in
      Argument.Expression { tags; value } }
| key = name ASSIGN value = expression
    { let tags = Source.merge (Text.tags key) (Expression.tags value) in
      Argument.KeyValue { tags; key; value } }
| info = DONTCARE
    { Argument.Missing { tags = info } }
;
```

### \[REVISIT\] (2) Operator precedence (1)

The spec mentions:

> This grammar does not indicate the precedence of the various operators. The precedence mostly follows the C precedence rules, with one change and some additions. (8)

```p4
extern T f<T>(T x);
extern Object {
    T foo<T>();
}
...
if (4 + d.f < 10) { ... }
```

But this is parsed as, `4 + (d.f < 10)`, due to how the lexer and parser cooperates.
Here, `f` is a generic extern function.
And while lexing `d.f`, the token `f` erroneously suggests the lexer that the following `<` is a type specialization symbol (`L_ANGLE_ARGS`), rather than a comparison operator (`L_ANGLE`).
i.e., the lexer falsely falls into the `STemplate` state, not the `SRegular` state.

```ocaml
{ let rec lexer (lexbuf:lexbuf): token = 
   match !lexer_state with
    | SIdent(id, next) ->
      begin match get_kind id with
      | TypeName true ->
        lexer_state := STemplate;
        TYPENAME
      (* the 'f' token hits here, leading the lexer state to STemplate *)
      | Ident true ->
        print_context ();
        lexer_state := STemplate;
        IDENTIFIER
      | TypeName false ->
        lexer_state := next;
        TYPENAME
      | Ident false ->
        lexer_state := next;
        IDENTIFIER
      end
```

This in turn somehow causes the overall result to be parsed as `4 + (d.f < 10)`, but not sure exactly why.
Using `%prec L_ANGLE` does not help here.

```ocaml
(* we have to actually inline this into expression rule to add the %prec directive *)
| info = l_angle %prec L_ANGLE
    { Op.Lt { tags = info } }
```

There can be basically two ways to fix this issue.

(1) Use lookahead parsing
(2) Parse the program twice: First match the angle brackets `<` and `>`, to first correctly identify whether it is a type specialization or not. Then, parse the whole program with the correct `L_ANGLE` and `L_ANGLE_ARGS`.

<details>
<summary>Tests</summary>

* precedence-lt.p4
</details>

## 2. Overlooked Features (requires structural change)

### \[DONE\] (1) ~~Sequence type~~

Currently, p4cherry treats `{ expr }` as tuple types.
So, `{ expr }` *cannot* be coerced to struct/header types.
But they are generally sequence types, where coercion to struct/header types are allowed.

Also, tuple types are restrictive, especially for its restriction on type nesting.
Strictly speaking, `{ 1 }` is an illegal expression because a tuple does not allow nesting an arbitrary precision integer.

To solve this issue, we need to introduce a new type, `SeqT`, which is a sequence type.
`SeqT` is an internal type like `RecordT`, i.e., user *cannot* declare a sequence type.

#### \[DONE\] (a) ~~Sequence coercion~~

```p4
struct headers {
    ipv4_option_timestamp_t ipv4_option_timestamp;
}
extern bit<16> get<T>(in T data);
get<headers>({ hdr.ipv4_option_timestamp });
```

#### \[DONE\] (b) ~~Sequence well-formedness~~

```p4
struct S {
    bit<32> x;
}
...
S s2;
s2 = { 0 };
```

### \[DONE\] (2) ~~Type Coercion~~

#### \[DONE\] (a) ~~More type coercion for equality check~~

Current coercion rule for equality check only assumes numeric types.
But it should be extended to other types, such as record and sequence types.

```p4
typedef tuple<bit<32>, bit<32>> pair;
struct S {
    bit<32> l;
    bit<32> r;
}
...
pair p = { 4, 5 };
S q = { 2, 3 };
zout = p == { 4, 5 };
```

#### \[DONE\] (b) ~~Type coercion for record types, with reordered fields~~

```p4
header h2_t {
    bit<8> f1;
    bit<8> f2;
}
...
hdr.h2 = { f2 = 53, f1 = 54 };
```

#### \[DONE\] (c) ~~Type coercion for conditional expression~~

Coerce then and else branches of a conditional expression to a same type.

```p4
h.eth_hdr.eth_type = (bit<16>) (-(h.eth_hdr.src_addr == 1) ? 2 : 3w1);
```

#### \[DONE\] (d) ~~Type coercion between serializable enum and its underlying type~~

May need to revisit sometime later, but at least the tests pass now.

```p4
enum bit<32> X { ... }
transition select (o.b.x) {
    X.Zero &&& 0x01: accept;
    ...
}
```

### (3) Type Inference

#### (a) Type inference when `_` was used on the parameter side (1)

```p4
control c (inout S s) { ... }
control cproto<T> (inout T v);
package top(cproto<_> _c);
top(c()) main;
```

<details>
<summary>Tests</summary>

* unused.p4
</details>

#### (b) Mixture of type inference and coercion (1)

The current naive type inference algorithm assumes type equality, and is not flexible enough to handle coercion.

```p4
extern void random<T>(out T result), in T lo);
...
bit<8> rand_val;
random(rand_val, 0);
```

<details>
<summary>Tests</summary>

* issue1586.p4
</details>

### \[DONE\] (4) ~~Overload resolution by name~~

The current implementation only uses arity to disambiguate overloaded functions.

```ocaml
(* (TODO) resolve overloaded functions with argument names *)
let find_overloaded_opt (fid, args) fenv =
    let arity = List.length args in
    let funcs =
    List.filter
        (fun ((fid', params), _) -> fid = fid' && arity = List.length params)
        (bindings fenv)
    in
    assert (List.length funcs <= 1);
    match funcs with [] -> None | _ -> Some (List.hd funcs |> snd)
```

```p4
bit<8> add_1(in bit<8> a, in bit<8> b) { return 1; }
bit<8> add_1(in bit<8> c, in bit<8> d) { return 2; }
```

</details>

### (5) Instantiation block

#### (a) Instantiation declaration within an instantiation block (1)

When an instantiation block has an instantiation declaration, which the current transformer assumes as invalid.

```p4
extern Virtual {
    Virtual();
    void run(in bit<16> ix);
    @synchronous(run) abstract bit<16> f(in bit<16> ix);
}
...
Virtual() cntr = {
    State(1024) state;
    bit<16> f(in bit<16> ix) {
        return state.get(ix);
    }
};
```

<details>
<summary>Tests</summary>

* virtual2.p4
</details>

#### \[DONE\] (b) ~~Instantiation block with an abstract method~~

The abstract method instantiation logic does not seem to work.
And we have to take `this` into account.

```p4
extern Virtual {
    Virtual();
    abstract bit<16> f();
    abstract void g(inout data ix);
}
...
Virtual() cntr = {
    bit<16> f() { return 1; }
    void g(inout data x) {}
};
```

```p4
extern X {
    X();
    bit<32> b();
    abstract void a(inout bit<32> arg);
}
...
X() x = {
    void a(inout bit<32> arg) { arg = arg + this.b(); }
};
```

### \[DONE\] (6) ~~Default parameter~~

```p4
extern void f(bit<32> a = 0, bit<32> b);
...
f(b = binit);
```

### \[DONE\] (7) ~~Built-in methods applied directly on type variables~~

The transformer logic assumes that `func` in a call expression `func<targs>(args)` is either a name or a field access, but not a type access.

Resolved by introducing a new type of call expression, `CallTypeE`.

```p4
typedef bit<32> T;
...
T.minSizeInBits();
```

### \[DONE\] (8) ~~Support direct application~~

Transform direct application.

```p4
control c() { ... }
control d() {
    apply { c.apply(); }
}
```

### \[DONE\] (9) ~~`value_set` declaration~~

```p4
value_set<bit<16>>(8) ipv4_ethertypes;
```

## 3. Devils are in the Details

### \[DONE\] (1) ~~Support `maxSizeInBytes` and `maxSizeInBits`~~

Logic only exists for `minSizeInBytes` and `minSizeInBits`.

```p4
hdrs.ipv4[0].length = (hdrs.ipv4[0].maxSizeInBytes() + umeta.L2_packet_len_bytes);
```

### \[DONE\] (2) ~~Allow serializable enum member initializers refer to other serializable enum members~~

```p4
enum bit<4> e {
    a = 0,
    b = 0,
    c = (bit<4>) a,
    d = a
}
```

### \[DONE\] ~~(3) `error` types can be `exact` matched~~

`error` types can be `exact` matched, but the current type checker rejects it.

The spec mentions:

> * "The `error` type only supports equality (`==`) and inequality (`!=`) comparisons."(8.2)
> * an `exact` match kind on a key field ... This is applicable for all legal key fields whose types support equality comparisons. (14.2.1.1)

Quickly patched by adding `error` to the list of types that support equality comparisons.
But later it would be desirable to have a clear list of types that are allowed for each match kind.

```p4
table t_exact {
    key = { m.my_err : exact; }
    ...
}
```

</details>

# C. Feature Extension since Petr4

## 1. Flexible syntax

### \[DONE\] (1) ~~Parsing `if`~~

Conditional statement can be used in a parser block.

The spec mentions:

> Added support for conditional statements and empty statements in parsers (Section 13.4). (A.3)

i.e., it was added in v1.2.2.

```ocaml
parserStatement:
| s = assignmentOrMethodCallStatement
| s = directApplication
| s = emptyStatement
| s = parserBlockStatement // Added
| s = conditionalStatement
    { s }
| decl = constantDeclaration
| decl = variableDeclaration
    { let tags = Declaration.tags decl in
      Statement.DeclarationStatement { tags; decl } }
;
```

### \[DONE\] (2) ~~Support trailing comma~~

```p4
enum A {
    X,
    Y,
}
```

<details>
<summary>Tests</summary>

* trailing-comma.p4
</details>

### \[DONE\] (3) ~~Allow parentheses in lvalues~~

Since [issue#1273](https://github.com/p4lang/p4-spec/issues/1273).

```p4
(x) = 1;
```

## 2. Feature Extension

### \[DONE\] (1) ~~Support `list` type~~

`list` should be a primitive type.

```p4
extern E {
    E(list<bit<32>> data);
    ...
}
```

### \[DONE\] (2) ~~Support generic structs and headers~~

```p4
struct S<T> {
    tuple<T, T> t;
}
```

But need to resolve: do we expect type inference when specializing generic types?
Also, how to deal with no-op casts? (when the target type already equals the source type)

### \[DONE\] (3) ~~Support `match_kind` as a primitive type~~

Spec v1.2.3 adds `match_kind` as a base type that can be parsed.

```p4
const tuple<match_kind> exact_once = ...;
```

### \[DONE\] (4) ~~Support `...` default grammar~~

The spec says:

> A left-value can be initialized automatically with default value of the suitable type using the syntax `...` (see Section 7.3). (8.26)

Implemented it with introducing new types and values, namely: `DefaultT`, `SeqDefaultT`, `RecordDefaultT`, `DefaultV`, `SeqDefaultV`, and `RecordDefaultV`.

`DefaultT` and `DefaultV` represents `...` used outside the context of a sequence or a record.
`SeqDefaultT` and `SeqDefaultV` represents `...` used in the context of a sequence, `{ (* expressions *) }'.
I made a parser hack to *syntactically disallow* `...` happening in the middle of a sequence.
`RecordDefaultT` and `RecDefaultV` represents `...` used in the context of a record, `{ (* key-value pairs *) }`.

Introduction of these default types and values interplay with the subtyping rules and cast evaluation.
`DefaultT` is a subtype of any type that has a default value.
`SeqDefaultT` is a subtype of `SeqT` when the former is a proper subset of the latter, and similar for `RecordDefaultT` and `RecordT`.

Yet, p4cherry's implementation may be more permissive than what was intended in the spec.
For example, `...`, `{ (* expressions *) ... }`, and `{ (* key-value pairs *) ... }` can be used as a function argument, like `f(...)` (assuming it is an in-direction argument; using `...` for out/inout-direction arguments would not make sense already at the type level, for it is not a lvalue).

```ocaml
(* (HACK) To syntactically disallow dots in the middle of a tuple expression *)
| info1 = L_BRACE values = expressionOptTrailingList info2 = R_BRACE
    { let tags = Source.merge info1 info2 in
      let is_dots expr = match expr with Expression.Dots _ -> true | _ -> false in
      if List.length values = 0 then Expression.List { tags; values }
      else
        let values, value_last =
          List.rev values |> List.tl |> List.rev, List.rev values |> List.hd
        in
        if List.exists is_dots values then
          raise Parsing.Parse_error;
        if is_dots value_last then Expression.ListDots { tags; values }
        else Expression.List { tags; values = values @ [value_last] } }
```

### (5) Support `priority` of table entry (2)

```p4
entries = {
    const priority=10: ...;
    ...
}
```

<details>
<summary>Tests</summary>

* entries-prio.p4
* init-entries-bmv2.p4
</details>

### \[DONE\] (6) ~~Support general switch statement~~

The old version of P4 assumes that switch only matches against table apply results, but the current version allows general switch statements.

```p4
switch (hdr.h1.data) {
    0: ...
    ...
}
```

### \[DONE\] (7) ~~Support `{#}` syntax~~

"The expression `{#}` represents an invalid header of some type, but it can be any header or header union type. A P4 compiler may require an explicit cast on this expression in case where it cannot determine the particular header of header union type from the context." (8.26).

```p4
h = (H) {#};
```

# D. Need Spec Clarification

## \[REPORTED\] 1. Should we add implicit cast for directionless parameter?: [directionless-implicit-cast](../test/program/well-typed-excluded/spec-clarify/directionless-implicit-cast)

Waiting for spec clarification, [Issue#1312](https://github.com/p4lang/p4-spec/issues/1312) and [PR#1330](https://github.com/p4lang/p4-spec/pull/1330).

I think we should, especially for constructor invocations.

Below apply for methods and functions.

Note that directionless argument for action can be implicitly cast, as per the spec.
The spec mentions,

> Actions can also be explicitly invoked using function call syntax, either from a control block or from another action. In this case, values for all action parameters must be supplied explicitly, including values for the directionless parameters. The directionless parameters in this case behave like in parameters. See Section 14.1.1 for further details. (6.8.1)

```p4
action a(inout bit<32> b, bit<32> d) { ... }
a(x, 0);
```

Below apply for constructors.

```p4
extern BFD_Offload {
    BFD_Offload(bit<16> size);
    ...
}
BFD_Offload(32768) bfd_session_liveness_tracker = ...;
```

## 2. How to match abstract methods when initializing an instance?: [abstract-method-overload](../test/program/well-typed-excluded/spec-clarify/abstract-method-overload)

When initializing an instance with an abstract method, to determine if the method was declared as abstract, I believe we should match the method using both the method name and argument names.
Mainly because P4 allows overloading of methods through argument names.
But the test cases below seem to use only method names for matching.

```p4
extern Virtual {
    Virtual();
    abstract bit<16> f();
    abstract void g(inout data ix);
}
...
Virtual() cntr = {
    bit<16> f() {
        return 1;
    }
    void g(inout data x) {}
};
```

This can get confusing with the presence of overloading.

```p4
extern Virtual {
    Virtual();
    abstract bit<16> f();
    abstract void g(inout data ix);
    abstract void g(inout data x);
}
...
Virtual() cntr = {
    bit<16> f() {
        return 1;
    }
    void g(inout data x) {}
};
```

## 3. Some extern functions seem to produce a (local) compile-time known value, but syntax does not reveal this: [buitin-local-compile-time-known](../test/program/well-typed-excluded/spec-clarify/builtin-local-compile-time-known)

```p4
@pure extern HashAlgorithm_t random_hash(bool msb, bool extend);
...
hdr.hash = Hash<big<16>>(random_hash(false, false)).get(hdr.h1);
```
```p4
const bool test = static_assert(V1MODEL_VERSION >= 20160101, "V1 model version is not >= 20160101");
```
```p4
extern widget createWidget<T, U>(U a, T b);
parser P();
parser p1()(widget w) { ... }
package sw0(P p);
sw0(p1(createWidget(16w0, 8w0))) main;
```

## \[REPORTED (not by me)\] 4. Is it legal to divide a fixed-width integer?: [fixed-width-div-and-mod](../test/program/well-typed-excluded/spec-clarify/fixed-width-div-and-mod)

[Issue#1327](https://github.com/p4lang/p4-spec/issues/1327), seems like a PR will be made soon.

```p4
bit<4> tmp = 4w0 - 4w1;
h.rshift.a = tmp / 4w2;
```

<details>
<summary>Tests</summary>

* gauntlet_various_ops-bmv2.p4
* issue2190.p4
* issue2287-bmv2.p4
* precedence.p4
* strength.p4
</details>

Note that implicit cast is allowed for arbitrary precision integer to fixed-width integer, and not the other way around.

```p4
x = 32w5 / 3;
```

## 5. Equivalence of table actions: [table-action-syntactic-eq](../test/program/well-typed-excluded/spec-clarify/table-action-syntactic-eq)

For default action, the spec mentions:

> In particular, the expressions passed as `in`, `out`, or `inout` parameters must be syntactically identical to the expressions used in one of the elements of the `actions` list. (14.2.1.3)

But the test cases below seem to violate this.

```p4
actions = { a1({ f0 = ext(), f1 = ext() } ); }
default_action = a1({ f1 = ext(), f0 = ext() });
```
```p4
action a() {}
control c() {
    table t {
        actions = { .a; }
        default_action = a;
    }
    apply {}
}
```

## \[REPORTED\] 6. Are accesses compile-time known?: [access-compile-time-known](../test/program/well-typed-excluded/spec-clarify/access-compile-time-known)

Waiting for spec clarification, [Issue#1323](https://github.com/p4lang/p4-spec/issues/1323) and [PR#1329](https://github.com/p4lang/p4-spec/pull/1329).

### \[REPORTED\] (1) Accessing a tuple element with a local compile-time known index is also a local compile-time known value?

```p4
const tuple<bit<32>, bit<32>> t = { 0, 1 };
const bit<32> f = t[0];
```

### \[REPORTED\] (2) Accessing a field of a local compile-time known struct is also a local compile-time known value?

```p4
const T t = { 32s10, 32s20 };
const int<32> x = t.t1;
```

## \[REPORTED\] 7. Type aliasing allowed for externs?: [typedef-objects](../test/program/well-typed-excluded/spec-clarify/typedef-objects)

Waiting for spec clarification, [Issue#1314](https://github.com/p4lang/p4-spec/issues/1314) and [PR#1328](https://github.com/p4lang/p4-spec/pull/1328)

Spec section 7.2.8 lists type nesting rules, but it does not mention whether it is legal to make a type alias of an extern object type via `typedef`.

```p4
extern MyCounter<I> {
    MyCounter(bit<32> size);
    void count(in I index);
}
typedef bit<10> my_counter_index_t;
typedef MyCounter<my_counter_index_t> my_counter_t;
```

## 8. Constraints on size of a value set?

The spec does not mention if the size given to a value set declaration should be local compile-time known, compile-time known, or neither.
I suspect it should be at least compile-time known, and it is already reflected in the current p4cherry implementation.

## 9. A generic type that imposes (or implies) a type constraint: [generic-constrained](../test/program/well-typed-excluded/spec-clarify/generic-constrained)

```p4
control nothing(
    inout empty_t hdr,
    inout empty_t meta,
    in intrinsic_metadata_t imeta) { apply {} }

control C<H, M>(
    inout H hdr,
    inout M meta,
    in intrinsic_metadata_t intr_md);

package P<H, M>(C<H, M> c = nothing());
```

Here, the package type is declared as a generic type that takes two type parameters, `H` and `M`.
But, the default argument to `c` is `nothing()`, which imposes a type constraint that `H` should be `empty_t` and `M` should be `empty_t`.

## 10. Matching control type in the presence of default parameter: [matching-control-type-decl-with-default](../test/program/well-typed-excluded/spec-clarify/matching-control-type-decl-with-default)

A control and package type declaration declares the template of a control or package.
The test below expects that a control that omits the default parameter should match the control type declaration that includes the default parameter.
I am not sure if this should be allowed.
Because, a user may just look at the type declarations and try to supply `intr_md` to the `MyC()` instance explicitly. But this would result in an error since `MyC()` does not expect `intr_md`.

```p4
control C<H, M>(
    inout H hdr,
    inout M meta,
    in intrinsic_metadata_t intr_md = {0, 0});

package P<H, M>(C<H, M> c);

struct hdr_t { }
struct meta_t { }

control MyC(inout hdr_t hdr, inout meta_t meta) {
   apply {}
}

P(MyC()) main;
```

## 11. Scope when instantiating with an initialization block

In P4, initialization block is used to initialize abstract methods when instantiating an extern object.
The spec mentions that:

> The abstract methods can only use the supplied arguments or refer to values that are in the top-level scope. When calling another method of the same instance the this keyword is used to indicate the current object instance. (11.3.1)

But also the grammar allows a local instantiation in the initialization block.

```
instantiation:
      ...
      | annotations typeRef "(" argumentList ")" name "=" objInitializer ";"
      | typeRef "(" argumentList ")" name "=" objInitializer ";"

objInitializer
    : "{" objDeclarations "}"
    ;

objDeclarations
    : /* empty */
    | objDeclarations objDeclaration
    ;

objDeclaration
    : functionDeclaration
    | instantiation
    ;
```

Thus, it also suggests that the local instance can be referred by the initialized abstract methods.
(Otherwise, there is no way to refer to those extern-local instances.)

```p4
extern Virtual {
    Virtual();
    void run(in bit<16> ix);
    @synchronous(run) abstract bit<16> f(in bit<16> ix);
}
extern State {
    State(int<16> size);
    bit<16> get(in bit<16> index);
}
...
Virtual() cntr = {
    State(1024) state;
    bit<16> f(in bit<16> ix) {
        return state.get(ix);
    }
};
```

This is implemented in current p4cherry, but it would be nice to have a clear spec on this.

# E. Need Test Clarification

## 1. Scope of abstract method when initializing an instance: [abstract-method-scoping](../test/program/well-typed-excluded/test-clarify/abstract-method-scoping)

When initializing an instance with an abstract method, it can only refer to its arguments or identifiers in the top-level scope.
The spec mentions:

> The abstract methods can only use the supplied arguments or refer to values that are in the top-level scope. When calling another method of the same instance the this keyword is used to indicate the current object instance. (11.3.1)

But the test cases below seem to violate this.

```p4
control ingress(inout headers hdr) {
    Stack<bit<16>>(2048) stack;
    StackAction<bit<16>, bit<16>>(stack) write = {
        void apply(inout bit<16> value) {
            value = hdr.data.h1; // illegal to refer to hdr
        }
        void overflow(inout bit<16> value, out bit<16> rv) {
            rv = 0x0f0f;
        }
    };
```

## 2. Syntax for select keyset: [select-keyset-syntax](../test/program/well-typed-excluded/test-clarify/select-keyset-syntax)

This should be a negative test.

```p4
transition select (hdr.h.f1) {
    0x8100  : p1; /* Works */
    (0x9100) : p1; /* Also Works */
    0xA100 &&& 0xEFFF  : p1; /* Works */
    (0xC100 &&& 0xEFFF) : p1; /* Syntax error: ',' expected */
    _ : p1; /* Works */
    (_): reject; /* Syntax error: ',' expected */
}
```

## 3. Shift by signed integer: [shift-by-signed-int](../test/program/well-typed-excluded/test-clarify/shift-by-signed-int)

This shifts by a signed integer, which is illegal.
This should be a negative test.

```p4
bit<4> func(in bit<4> l) {
  const int<6> tt = 1;
  return l << tt;
}
```

## 4. Duplicate definition of `match_kind`: [duplicate-match-kind](../test/program/well-typed-excluded/test-clarify/duplicate-match-kind)

`ternary` is defined twice.
This should be a negative test.

```p4
#include <core.p4>
match_kind {
    ternary,
    exact
}
```

## 5. Mask expressions for `exact` key: [mask-exact-key](../test/program/well-typed-excluded/test-clarify/mask-exact-key)

We cannot use mask expressions for `exact` key.
This should be a negative test.

```p4
table unit {
    key = { x: exact; }
    const entries = {
        ...
        32w0x0B_00_00_00 &&& 32w0xFF_00_00_00: drop();
    }
    ...
}
```

## 6. Nesting `match_kind` or `int` inside a tuple type: [tuple-nesting](../test/program/well-typed-excluded/test-clarify/tuple-nesting)

`match_kind` and `int` *cannot* be nested inside a tuple type.
This should be a negative test.

```p4
const tuple<match_kind> exact_once = { exact };
```

```p4
tuple<int> t = { t1 };
```

## 7. Implicit cast of `value_set` in `select` expression: [value-set-implicit-cast](../test/program/well-typed-excluded/test-clarify/value-set-implicit-cast)

When a value set, of type `set<T>` is used as a select label, it can be implicitly cast to the select key type `set<T'>`.
However, below programs expect loose type casting rules.

```p4
header data_h {
  bit<32> da;
  bit<32> db;
}
struct my_packet {
  data_h data;
}
struct my_metadata {
  data_h[2] data;
}
struct value_set_t {
  bit<32> field;
}
...
value_set<value_set_t>(4) pvs;
state start {
    b.extract(p.data);
    transition select(p.data.da) {
        pvs: accept;
        0x810 : foo;
    }
}
```

Here, we *cannot* implicitly cast `value_set_t` (which a struct type) to `bit<32>`.

## 8. Implicit cast of a singleton sequence to a scalar in table entry: [aggregate-to-scalar-implicit-cast](../test/program/well-typed-excluded/test-clarify/aggregate-to-scalar-implicit-cast)

Some programs expect implicit cast of a singleton sequence to a scalar in table entry, which is illegal, in a strict sense.

```p4
typedef bit<32> IPv4Address;
header ipv4_t {
    ...
    IPv4Address  dstAddr;
}

table ingress_tbl {
    key = { hdr.ipv4.dstAddr : exact; }
    actions = {actTbl; drop;}
    const default_action = drop;
    const entries = {
        {(8w0x20++8w0x02++8w0x04++8w0x20)} :
            actTbl(24w42, (8w0x20++8w0x02++8w0x42++8w0x00));
    }
}
```

Here, `{(8w0x20++8w0x02++8w0x04++8w0x20)}` is a singleton sequence, and it should not be implicitly cast to a scalar type `IPv4Address`, or `bit<32>`.

## 9. Implicit cast of newtype: [newtype-implicit-cast](../test/program/well-typed-excluded/test-clarify/newtype-implicit-cast)

New types introduced by keyword `type` *cannot* be implicitly cast to its underlying type.
However, below programs seem to violate this restriction.

p4c accepts these as valid.

## 10. Coercion from a fixed width integer to an arbitrary precision integer: [fixed-to-arbitrary-implicit-cast](../test/program/well-typed-excluded/test-clarify/fixed-to-arbitrary-implicit-cast)

This is illegal, but the test case below seem to violate this.

```p4
const int z1 = 2w1;
```

## 11. Equality check (`==`) on a variable type: [type-variable-equality-op](../test/program/well-typed-excluded/test-clarify/type-variable-equality-op)

The spec only allows assignment (`=`) for types that are type variables.
But the test case below seems to violate this.
This also implies some type constraint that the type variable should be a type that supports equality checks.

```p4
bool g<t>(in t a) {
    h<t> v;
    v.f = a;
    return v.f == a;
}
```

# F. Future extension

## 1. For loops: [forloop](../test/program/well-typed-excluded/future/forloop)

## 2. Generic parser/control declaration: [generic-parser-control](../test/program/well-typed-excluded/future/generic-parser-control)

```p4
parser p1<T>(in T a) { ... }
```

## 3. Concatenation of string literals: [concat-string](../test/program/well-typed-excluded/future/concat-string)

```p4
log("Log message" ++ " text");
```

# G. Unsupported features

## 1. Custom table element: [custom-table-element](../test/program/well-typed-excluded/unsupported/custom-table-element)

### (1) `implementation`

```p4
table indirect_ws {
  ...
  implementation = ...;
}
```

### (2) `counters`

```p4
table ipv4_da_lpm {
    ...
    counters = ...;
}
```

### (3) `junk`

```p4
table t {
    ...
    junk = ...;
}
```

### (4) `meters`

```p4
table m_table {
    ...
    meters = ...;
}
```

### (5) `add_on_miss`

```p4
table ipv4_da {
    ...
    add_on_miss = ...;
}
```

### (6) `psa_direct_counter`

```p4
table tbl {
    ...
    psa_direct_counter = ...;
}
```

### (7) `psa_direct_meter`

```p4
table tbl {
    ...
    psa_direct_meter = ...;
}
```

### (8) `psa_idle_timeout`

```p4
table tbl_idle_timeout {
    ...
    psa_idle_timeout = ...;
}
```

## 2. `selector` match kind: [selector-match-kind](../test/program/well-typed-excluded/unsupported/selector-match-kind)

This is specific to V1Model architecture.

```p4
table indirect_ws {
    key = { meta.hash1 : selector; }
    ...
}
```

## 3. Optional argument: [optional-param](../test/program/well-typed-excluded/unsupported/optional-param)

```p4
extern Checksum {
    ...
    bit<16> update<T>(in T data, @optional in bool zeros_as_ones);
}
... 
h.h.result = ipv4_checksum.update({ h.eth_hdr.dst_addr, h.eth_hdr.src_addr, h.eth_hdr.eth_type });
```
