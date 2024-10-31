# A. Fix Logic

## 1. Parser Errors

### \[DONE\] (1) ~~Parsing `_` (don't care)~~

```p4
f(x = 1, y = _);
```

Don't care for named argument was added [pr#1074](https://github.com/p4lang/p4-spec/pull/1074).

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

### (2) Operator precedence (1)

The spec mentions "This grammar does not indicate the precedence of the various operators. The precedence mostly follows the C precedence rules, with one change and some additions." (8).

```p4
if (4 + d.f < 10) { ... }
```

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

### (2) Type Coercion

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

#### (d) Type coercion between serializable enum and its underlying type (9)

But it is difficult to determine 'when' it should occur.

```p4
enum bit<32> X { ... }
transition select (o.b.x) {
    X.Zero &&& 0x01: accept;
    ...
}
```

<details>
<summary>Tests</summary>

* issue3056.p4
* issue3288.p4
* issue3635.p4
* psa-dpdk-binary-operations-1.p4
* psa-dpdk-binary-operations.p4
* psa-dpdk-header-union-typedef.p4
* psa-variable-index.p4
* serEnumImplCast.p4
</details>

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

### (6) Keyset and sequence type (Need investigation) (3)

<details>
<summary>Tests</summary>

* action-two-params.p4
* op_bin.p4
* table-entries-no-arg-actions.p4
</details>

### (7) Default parameter (6)

```p4
package P<H, M>(C<H, M> c = nothing());
P<_, _>() main;
```

<details>
<summary>Tests</summary>

* default-package-argument.p4
* issue1333.p4
* issue1638.p4
* issue1937-1-bmv2.p4
* issue1937-2-bmv2.p4
* issue1937-3-bmv2.p4
* issue2303.p4
* issue2599.p4
</details>

### (8) Built-in methods applied directly on type variables (1)

The transformer logic assumes that `func` in a call expression `func<targs>(args)` is either a name or a field access, but not a type access.

```p4
typedef bit<32> T;
...
T.minSizeInBits();
```

<details>
<summary>Tests</summary>

* minsize.p4
</details>

### \[DONE\] (9) ~~Support direct application~~

Transform direct application.

```p4
control c() { ... }
control d() {
    apply { c.apply(); }
}
```

### \[DONE\] (10) ~~`value_set` declaration~~

```p4
value_set<bit<16>>(8) ipv4_ethertypes;
```

### (11) Instances must be compile-time known (3)

But are they local compile-time known or compile-time known?
Also, does a directionless parameter expect a local compile-time known value or a compile-time known value?

```p4
extern widget createWidget<T, U>(U a, T b);
parser P();
parser p1()(widget w) { ... }
package sw0(P p);
sw0(p1(createWidget(16w0, 8w0))) main;
```

<details>
<summary>Tests</summary>

* factory1.p4
* factory2.p4
* pna-example-SelectByDirection2.p4
</details>

## 3. Devils are in the Details

### \[DONE\] (1) ~~Support `maxSizeInBytes` and `maxSizeInBits`~~

Logic only exists for `minSizeInBytes` and `minSizeInBits`.

```p4
hdrs.ipv4[0].length = (hdrs.ipv4[0].maxSizeInBytes() + umeta.L2_packet_len_bytes);
```

### (2) Allow serializable enum member initializers refer to other serializable enum members (1)

But I think it is a terrible idea to allow it.

```p4
enum bit<4> e {
    a = 0,
    b = 0,
    c = (bit<4>) a,
    d = a
}
```

<details>
<summary>Tests</summary>

* issue3616.p4
</details>

### \[DONE\] ~~(3) `error` types can be `exact` matched~~

`error` types can be `exact` matched, but the current type checker rejects it.

The spec mentions:
* "The `error` type only supports equality (`==`) and inequality (`!=`) comparisons." (8.2).
* "an `exact` match kind on a key field ... This is applicable for all legal key fields whose types support equality comparisons." (14.2.1.1).

Quickly patched by adding `error` to the list of types that support equality comparisons.
But later it would be desirable to have a clear list of types that are allowed for each match kind.

```p4
table t_exact {
    key = { m.my_err : exact; }
    ...
}
```

<details>
<summary>Tests</summary>

</details>

### (4) `selector` match kind (10)

```p4
table indirect_ws {
    key = { meta.hash1 : selector; }
    ...
}
```

<details>
<summary>Tests</summary>

* action_selector_shared-bmv2.p4
* issue1560-bmv2.p4
* pna-action-selector-1.p4
* pna-action-selector.p4
* psa-action-selector1.p4
* psa-action-selector2.p4
* psa-action-selector3.p4
* psa-action-selector4.p4
* psa-action-selector5.p4
* psa-action-selector6.p4
</details>

# C. Feature Extension since Petr4

## 1. Flexible syntax

### \[DONE\] (1) ~~Parsing `if`~~

Conditional statement can be used in a parser block.

The spec mentions, "Added support for conditional statements and empty statements in parsers (Section 13.4). (A.3)", i.e., it was added in v1.2.2.

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

### (2) Support trailing comma (1)

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

### (3) Allow parentheses in lvalues (1)

Since [issue#1273](https://github.com/p4lang/p4-spec/issues/1273).

```p4
(x) = 1;
```

<details>
<summary>Tests</summary>

* lvalue-parens.p4
</details>

## 2. Feature Extension

### \[DONE\] (1) ~~Support `list` type~~

`list` should be a primitive type.

```p4
extern E {
    E(list<bit<32>> data);
    ...
}
```

### (2) Support generic structs and headers (12)

```p4
struct S<T> {
    tuple<T, T> t;
}
```

<details>
<summary>Tests</summary>

* generic-struct-tuple.p4
* generic-struct.p4
* issue2627.p4
* issue2635.p4
* issue3091.p4
* issue3203.p4
* issue3204.p4
* issue3291-1.p4
* issue3292.p4
* list9.p4
* p4rt_digest_complex.p4
* stack-init.p4
</details>

### \[DONE\] (3) ~~Support `match_kind` as a primitive type~~

Spec v1.2.3 adds `match_kind` as a base type that can be parsed.

```p4
const tuple<match_kind> exact_once = ...;
```

### (4) Support `...` default grammar (1)

The spec says, "A left-value can be initialized automatically with default value of the suitable type using the syntax `...` (see Section 7.3)." (8.26).

<details>
<summary>Tests</summary>

* default-initializer.p4
</details>

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

### (6) Support general switch statement (15)

The old version of P4 assumes that switch only matches against table apply results, but the current version allows general switch statements.

```p4
switch (hdr.h1.data) {
    0: ...
    ...
}
```

<details>
<summary>Tests</summary>

* invalid-hdr-warnings3-bmv2.p4
* issue2617.p4
* issue3374.p4
* issue3619-1.p4
* issue3619.p4
* issue3623-1.p4
* issue3623.p4
* issue3650.p4
* issue4656_const_fold_generic_switch_label_expr.p4
* issue4661_non_pure_extern_function_const_args.p4
* issue4661_pure_extern_function_const_args.p4
* pna-example-varIndex-2.p4
* psa-example-switch-with-constant-expr.p4
* psa-switch-expression-without-default.p4
* switch-expression.p4
</details>

### \[DONE\] (7) ~~Support `{#}` syntax~~

"The expression `{#}` represents an invalid header of some type, but it can be any header or header union type. A P4 compiler may require an explicit cast on this expression in case where it cannot determine the particular header of header union type from the context." (8.26).

```p4
h = (H) {#};
```

# D. Need Spec Clarification

## \[REPORTED\] 1. Should we add implicit cast for directionless parameter? (73)

I think we should, especially for constructor invocations.
Waiting for the spec clarification, [issue#1312](https://github.com/p4lang/p4-spec/issues/1312).

Below apply for methods and functions.

Note that directionless argument for action can be implicitly cast, as per the spec.
The spec mentions, "Actions can also be explicitly invoked using function call syntax, either from a control block or from another action. In this case, values for all action parameters must be supplied explicitly, including values for the directionless parameters. The directionless parameters in this case behave like in parameters. See Section 14.1.1 for further details. (6.8.1)".

```p4
action a(inout bit<32> b, bit<32> d) { ... }
a(x, 0);
```

<details>
<summary>Tests</summary>

* extern-funcs-bmv2.p4
* extern2.p4
* gauntlet_extern_arguments_2.p4
* gauntlet_hdr_in_value-bmv2.p4
* issue1001-1-bmv2.p4
* issue1001-bmv2.p4
* issue1043-bmv2.p4
* issue1334.p4
* issue1642-bmv2.p4
* issue1653-bmv2.p4
* issue1653-complex-bmv2.p4
* issue1660-bmv2.p4
* issue1765-1-bmv2.p4
* issue2648.p4
* issue3246-1.p4
* issue383-bmv2.p4
* issue562-bmv2.p4
* issue933-1.p4
* named-arg1.p4
* v1model-special-ops-bmv2.p4
</details>

Below apply for constructors.

```p4
extern BFD_Offload {
    BFD_Offload(bit<16> size);
    ...
}
BFD_Offload(32768) bfd_session_liveness_tracker = ...;
```

<details>
<summary>Tests</summary>

* bfd_offload.p4
* constructor_cast.p4
* issue1006.p4
* issue1097-2-bmv2.p4
* issue1097-bmv2.p4
* issue1814-1-bmv2.p4
* issue1814-bmv2.p4
* issue1958.p4
* issue2844-enum.p4
* issue298-bmv2.p4
* issue4288.p4
* issue754.p4
* list7.p4
* pr1363.p4
* psa-action-profile1.p4
* psa-action-profile3.p4
* psa-action-profile4.p4
* psa-basic-counter-bmv2.p4
* psa-counter1.p4
* psa-counter2.p4
* psa-counter3.p4
* psa-custom-type-counter-index.p4
* psa-end-of-ingress-test-bmv2.p4
* psa-example-dpdk-byte-alignment_1.p4
* psa-example-dpdk-byte-alignment_2.p4
* psa-example-dpdk-byte-alignment_3.p4
* psa-example-dpdk-byte-alignment_5.p4
* psa-example-dpdk-byte-alignment_6.p4
* psa-example-dpdk-byte-alignment_7.p4
* psa-example-dpdk-byte-alignment_8.p4
* psa-example-dpdk-byte-alignment_9.p4
* pse-example-dpdk-counter.p4
* psa-example-dpdk-externs.p4
* psa-example-dpdk-meter-execute-err.p4
* psa-example-dpdk-meter.p4
* psa-example-dpdk-meter1.p4
* psa-example-dpdk-varbit-bmv2.p4
* psa-meter1.p4
* psa-meter3.p4
* psa-meter7-bmv2.p4
* psa-random.p4
* psa-register-complex-bmv2.p4
* psa-register-read-write-2-bmv2.p4
* psa-register-read-write-bmv2.p4
* psa-register1.p4
* psa-register2.p4
* psa-register3.p4
* rcp.p4
* rcp1.p4
* register-serenum-bmv2.p4
* simple-firewall_ubpf.p4
* unused-counter-bmv2.p4
* value-sets.p4
</details>

## 2. Should an argument to directionless action parameter be compile-time known? (4)

I think it should be, but the test cases below seem to violate this.

```p4
action Reject(bool rej) { ... }
bool x = true;
Reject(x);
```

<details>
<summary>Tests</summary>

* action_call_ebpf.p4
* calc-ebpf.p4
* crc32-bmv2.p4
* direct-action1.p4
</details>

## 3. How to match abstract methods when initializing an instance? (4)

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

<details>
<summary>Tests</summary>

* issue2175-1.p4
* issue2175-3.p4
* issue2175-4.p4
* virtual.p4
</details>

## 4. Some extern functions seem to produce a (local) compile-time known value, but syntax does not reveal this (2)

```p4
@pure extern HashAlgorithm_t random_hash(bool msb, bool extend);
...
hdr.hash = Hash<big<16>>(random_hash(false, false)).get(hdr.h1);
```
```p4
const bool test = static_assert(V1MODEL_VERSION >= 20160101, "V1 model version is not >= 20160101");
```

<details>
<summary>Tests</summary>

* hashext3.p4
* issue3531.p4
</details>

## 5. Is it legal to divide a fixed-width integer?  (8)

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

<details>
<summary>Tests</summary>

* constant_folding.p4
* issue1879-bmv2.p4
* issue2279_4.p4
</details>

## 6. Is it legal to coerce a fixed width integer to an arbitrary precision integer? (2)

I think it is illegal, but the test case below seem to violate this.

```p4
const int z1 = 2w1;
```

<details>
<summary>Tests</summary>

* issue2444.p4
* issue3283.p4
</details>

## 7. Equivalence of table actions (2)

For default action, the spec mentions "In particular, the expressions passed as `in`, `out`, or `inout` parameters must be syntactically identical to the expressions used in one of the elements of the `actions` list. (14.2.1.3)".

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

<details>
<summary>Tests</summary>

* issue2037.p4
* issue3671.p4
</details>

## 8. Accessing a tuple element with a local compile-time known index is also a local compile-time known value? (1)

```p4
const tuple<bit<32>, bit<32>> t = { 0, 1 };
const bit<32> f = t[0];
```

<details>
<summary>Tests</summary>

* tuple3.p4
</details>

## 9. Accessing a field of a local compile-time known struct is also a local compile-time known value? (2)

```p4
const T t = { 32s10, 32s20 };
const int<32> x = t.t1;
```

<details>
<summary>Tests</summary>

* struct.p4
* struct1.p4
</details>

## 10. Type aliasing allowed for externs? (4)

Spec section 7.2.8 lists type nesting rules, but it does not mention whether it is legal to make a type alias of an extern object type via `typedef`.

```p4
extern MyCounter<I> {
    MyCounter(bit<32> size);
    void count(in I index);
}
typedef bit<10> my_counter_index_t;
typedef MyCounter<my_counter_index_t> my_counter_t;
```

<details>
<summary>Tests</summary>

* extern-inst-as-param.p4
* issue2735-bmv2.p4
* issue2735.p4
* typedef-constructor.p4
</details>

## 11. Constraints on size of a value set?

The spec does not mention if the size given to a value set declaration should be local compile-time known, compile-time known, or neither.
I suspect it should be at least compile-time known, and it is reflected in the current implementation.

# E. Unsupported features

## 1. Custom table element (47)

### (1) `implementation`

```p4
table indirect_ws {
  ...
  implementation = ...;
}
```

<details>
<summary>Tests</summary>

* action_profile-bmv2.p4
* action_profile_max_group_size_annotation.p4
* action_profile_sum_of_members_annotation.p4
* hit_ebpf.p4
* init_ebpf.p4
* issue2791_ebpf.p4
* issue2793_ebpf.p4
* issue2816-1_ebpf.p4
* issue2816_ebpf.p4
* issue297-bmv2.p4
* issue870_ebpf.p4
* key-issue-1020_ebpf.p4
* key_ebpf.p4
* lpm_ebpf.p4
* stack_ebpf.p4
* switch_ebpf.p4
* ternary_ebpf.p4
* test_ebpf.p4
* two_ebpf.p4
* valid_ebpf.p4
</details>

### (2) `counters`

```p4
table ipv4_da_lpm {
    ...
    counters = ...;
}
```

<details>
<summary>Tests</summary>

* issue364-bmv2.p4
* issue461-bmv2.p4
</details>

### (3) `junk`

```p4
table t {
    ...
    junk = ...;
}
```

<details>
<summary>Tests</summary>

* junk-prop-bmv2.p4
</details>

### (4) `meters`

```p4
table m_table {
    ...
    meters = ...;
}
```

<details>
<summary>Tests</summary>

* named_meter_1-bmv2.p4
* named_meter_bmv2.p4
</details>

### (5) `add_on_miss`

```p4
table ipv4_da {
    ...
    add_on_miss = ...;
}
```

<details>
<summary>Tests</summary>

* pna-add-on-miss.p4
* pna-add_on_miss_action_name.p4
* pna-dpdk-add_on_miss0.p4
* pna-dpdk-add_on_miss1.p4
* pna-dpdk-direct-counter-learner.p4
* pna-dpdk-direct-meter-learner.p4
* pna-example-tcp-connection-tracking.p4
* pna-dpdk-parser-state-err.p4
* pna-dpdk-table-key-consolidation-learner-1.p4
* pna-dpdk-table-key-consolidation-learner-3.p4
* pna-dpdk-table-key-consolidation-learner-4.p4
* pna-dpdk-table-key-consolidation-learner-5.p4
* pna-dpdk-table-key-consolidation-learner-6.p4
* pna-dpdk-table-key-consolidation-learner-7.p4
* pna-dpdk-table-key-use-annon.p4
* pna-mux-dismantle.p4
</details>

### (6) `psa_direct_counter`

```p4
table tbl {
    ...
    psa_direct_counter = ...;
}
```

<details>
<summary>Tests</summary>

* psa-counter4.p4
* psa-example-counters-bmv2.p4
* psa-example-parser-checksum.p4
</details>

### (7) `psa_direct_meter`

```p4
table tbl {
    ...
    psa_direct_meter = ...;
}
```

<details>
<summary>Tests</summary>

* psa-example-dpdk-directmeter.p4
* psa-meter4.p4
* psa-meter5.p4
</details>

### (8) `psa_idle_timeout`

```p4
table tbl_idle_timeout {
    ...
    psa_idle_timeout = ...;
}
```

<details>
<summary>Tests</summary>

* psa-idle-timeout.p4
</details>

## 2. Optional argument (10)

```p4
extern Checksum {
    ...
    bit<16> update<T>(in T data, @optional in bool zeros_as_ones);
}
... 
h.h.result = ipv4_checksum.update({ h.eth_hdr.dst_addr, h.eth_hdr.src_addr, h.eth_hdr.eth_type });
```

<details>
<summary>Tests</summary>

* gauntlet_optional-bmv2.p4
* issue1304.p4
* issue2273-1.p4
* issue2492.p4
* issue2630.p4
* issue2664-bmv2.p4
* issue2810.p4
* issue3051.p4
* issue3417.p4
* pna-dpdk-direct-counter.p4
</details>

# F. Future extension

## 1. For loops (9)

<details>
<summary>Tests</summary>

* forloop1.p4
* forloop2.p4
* forloop3.p4
* forloop4.p4
* forloop5.p4
* forloop5a.p4
* forloop6.p4
* forloop7.p4
* issue4739.p4
</details>

## 2. Generic parser/control declaration (13)

```p4
parser p1<T>(in T a) { ... }
```

<details>
<summary>Tests</summary>

* functors6.p4
* functors7.p4
* functors8.p4
* functors9.p4
* generic.p4
* generic1.p4
* issue1914-1.p4
* issue1914.p4
* issue2019-1.p4
* issue2019.p4
* issue2265.p4
* issue344.p4
* spec-issue1068.p4
</details>

## 3. Concatenation of string literals (2)

```p4
log("Log message" ++ " text");
```

<details>
<summary>Tests</summary>

* issue4932.p4
* spec-issue1297-string-cat.p4
</details>

# G. Should be a negative test instead?

## 1. Scope of abstract method when initializing an instance (2)

When initializing an instance with an abstract method, it can only refer to its arguments or identifiers in the top-level scope.
The spec mentions: "The abstract methods can only use the supplied arguments or refer to values that are in the top-level scope. When calling another method of the same instance the this keyword is used to indicate the current object instance. (11.3.1)".
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

<details>
<summary>Tests</summary>

* issue2273-1.p4
* virtual3.p4
</details>

## 2. Syntax for select keyset (1)

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

<details>
<summary>Tests</summary>

* issue2514.p4
</details>

## 3. Shift by signed integer (1)

This shifts by a signed integer, which is illegal.
This should be a negative test.

```p4
bit<4> func(in bit<4> l) {
  const int<6> tt = 1;
  return l << tt;
}
```

<details>
<summary>Tests</summary>

* issue3287.p4
</details>

## 4. Duplicate definition of `match_kind` (1)

`ternary` is defined twice.
This should be a negative test.

```p4
#include <core.p4>
match_kind {
    ternary,
    exact
}
```

<details>
<summary>Tests</summary>

* pipe.p4
</details>

## 5. Mask expressions for `exact` key (1)

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

<details>
<summary>Tests</summary>

* spec-ex25.p4
</details>

## 6. Nesting `match_kind` or `int` inside a tuple type (2)

`match_kind` and `int` *cannot* be nested inside a tuple type.
This should be a negative test.

```p4
const tuple<match_kind> exact_once = { exact };
```

```p4
tuple<int> t = { t1 };
```

<details>
<summary>Tests</summary>

* issue3091-1.p4
* issue3238.p4
* list3.p4
* list4.p4
</details>

## 7. Implicit cast of `value_set` in `select` expression (7)

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

<details>
<summary>Tests</summary>

* pvs-nested-struct.p4
* pvs-struct-1-bmv2.p4
* pvs-struct-2-bmv2.p4
* pvs-struct-3-bmv2.p4
* pvs.p4
* v1model-p4runtime-enumint-types1.p4
* v1model-p4runtime-most-types1.p4
</details>
