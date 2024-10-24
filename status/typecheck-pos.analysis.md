# Fix Logic

## Parser Errors

### Parsing `if`

The parser seems to expect an artificial `THEN` token, while it should not in concrete grammar.

```ocaml
| info1 = IF L_PAREN cond = expression R_PAREN tru = statement   %prec THEN
```

<details>
<summary>Tests</summary>
* guantlet_parser_test_1.p4
* guantlet_parser_test_3.p4
* guantlet_parser_test_4.p4
* invalid-hdr-warnings1.p4
* issue2409.p4
* parser-if.p4
* pna-direction-main-parser-err.p4
* pna-example-pass-parser.p4
* psa-dpdk-header-union-typedef.p4
</details>

### Parsing `_` (don't care)

```p4
f(x = 1, y = _);
```

<details>
<summary>Tests</summary>
* issue3274-2.p4
* issue3274.p4
</details>

### Operator precedence

The spec mentions "This grammar does not indicate the precedence of the various operators. The precedence mostly follows the C precedence rules, with one change and some additions." (8).

```p4
if (4 + d.f < 10) { ... }
```

<details>
<summary>Tests</summary>
* precedence-lt.p4
</details>

## Overlooked Features (requires structural change)

### Type Coercion

#### Type coercion for conditional expression

```p4
h.eth_hdr.eth_type = (bit<16>) (-(h.eth_hdr.src_addr == 1) ? 2 : 3w1);
```

<details>
<summary>Tests</summary>
* gauntlet_mux_typecasting-bmv2.p4
* issue-2123-2-bmv2.p4
* issue-2123-3-bmv2.p4
* parser-conditional.p4
</details>

### Type Inference

#### Type inference when `_` was explicitly used

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

#### Type coercion between serializable enum and its underlying type

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
* enumCast.p4
* issue1001-1-bmv2.p4
* issue3056.p4
* issue3288.p4
* issue3333.p4
* issue3635.p4
* psa-dpdk-binary-operations-1.p4
* psa-dpdk-binary-operations.p4
* psa-variable-index.p4
* serEnumImplCast.p4
</details>

#### Mixture of type inference and coercion

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

### Overload resolution by name

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

<details>
<summary>Tests</summary>
* issue1334.p4
* issue4775-2.p4
</details>

### Instantiation block

#### Instantiation declaration within an instantiation block

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

#### Instantiation block with an abstract method

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

<details>
<summary>Tests</summary>
* issue2175-1.p4
* issue2175-2.p4
* issue2175-3.p4
* issue2175-4.p4
* issue2175-5.p4
* issue2273-1.p4
* issue2273.p4
* issue304-1.p4
* issue3307.p4
* issue3417.p4
* virtual.p4
* virtual3.p4
</details>

### Keyset and tuple type (Need investigation)

<details>
<summary>Tests</summary>
* action-two-params.p4
* op_bin.p4
</details>

### Default parameter

```p4
package P<H, M>(C<H, M> c = nothing());
P<_, _>() main;
```

<details>
<summary>Tests</summary>
* default-package-argument.p4
* issue1333.p4
* issue1937-1-bmv2.p4
* issue2599.p4
</details>

### Built-in methods

The transformer logic assumes that `func` in a call expression `func(args)` is either a name or a field access.

```p4
header H {}
...
H[0] h;
h[0].minSizeInBits();
```

<details>
<summary>Tests</summary>
* minsize.p4
</details>

### Support direct application

Transform direct application.

```p4
control c() { ... }
control d() {
    apply { c.apply(); }
}
```

<details>
<summary>Tests</summary>
* direct-call.p4
* direct-call1.p4
* direct-call2.p4
* extern-inst-as-param.p4
* gauntlet_infinite_loop.p4
* issue1107.p4
* issue1470-bmv2.p4
* issue1566-bmv2.p4
* issue1937-2-bmv2.p4
* issue1937-3-bmv2.p4
* issue2844-enum.p4
* issue3394.p4
* issue4883_dup_has_returned_decl.p4
* issue4883_dup_has_returned_decl2.p4
* issue561-bmv2.p4
* nonstandard_table_names-bmv2.p4
* pna-dpdk-add_on_miss1.p4
* pna-example-tunnel.p4
* psa-end-of-ingress-test-bmv2.p4
* psa-multicast-basic-2-bmv2.p4
* psa-parser-error-test-bmv2.p4
* psa-resubmit-bmv2.p4
* psa-unicast-or-drop-bmv2.p4
* redundant_parsers_dangling_unused_parser_decl.p4
* use-priority-as-name.p4
</details>

### `value_set` declaration

```p4
value_set<bit<16>>(8) ipv4_ethertypes;
```

<details>
<summary>Tests</summary>
* issue1955.p4
* issue3343.p4
* psa-test.p4
* pvs-bitstring-bmv2.p4
* pvs-nested-struct.p4
* pvs-struct-1-bmv2.p4
* pvs-struct-2-bmv2.p4
* pvs-struct-3-bmv2.p4
* pvs.p4
* v1model-p4runtime-enumint-types1.p4
* v1model-p4runtime-most-tupes1.p4
* value_set_ebpf.p4
</details>

### Instances must be compile-time known

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

## Devils are in the Details

### Support `maxSizeInBytes` and `maxSizeInBits`

Logic only exists for `minSizeInBytes` and `minSizeInBits`.

```p4
hdrs.ipv4[0].length = (hdrs.ipv4[0].maxSizeInBytes() + umeta.L2_packet_len_bytes);
```

<details>
<summary>Tests</summary>
* pna-dpdk-header-stack-assignment.p4
</details>

### Allow serializable enum member initializers refer to other serializable enum members

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

### Well-formedness of nested externs

```p4
typedef bit<(48 + 12 + 9)> Mac_entry;
typedef register<Mac_entry> Mac_table;
```

<details>
<summary>Tests</summary>
* issue2735-bmv2.p4
* issue2735.p4
* typedef-constructor.p4
</details>

### `error` types can be `exact` matched

"The `error` type only supports equality (`==`) and inequality (`!=`) comparisons." (8.2).

"an `exact` match kind on a key field ... This is applicable for all legal key fields whose types support equality comparisons." (14.2.1.1).

```p4
table t_exact {
    key = { m.my_err : exact; }
    ...
}
```

<details>
<summary>Tests</summary>
* issue1062-1-bmv2.p4
* issue1062-bmv2.p4
* issue1304.p4
* psa-example-parser-checksum.p4
</details>

## `selector` match kind

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

# Feature Extension since Petr4

## Flexible syntax

### Support trailing comma

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

### Allow parentheses in lvalues

Since [issue#1273](https://github.com/p4lang/p4-spec/issues/1273).

```p4
(x) = 1;
```

<details>
<summary>Tests</summary>
* lvalue-parens.p4
</details>

## Feature Extension

### Support list type

#### List as a primitive type

List should be a primitive type.

```p4
extern E {
    E(list<bit<32>> data);
    ...
}
```

<details>
<summary>Tests</summary>
* list.p4
* list1.p4
* list2.p4
* list3.p4
* list4.p4
* list5.p4
* list6.p4
* list7.p4
* list8.p4
* list9.p4
</details>

#### List coercion

Currently, { expr } are treated as tuple types.
So, { expr } *cannot* be coerced to struct/header types.
But they are generally list types, where coercion to struct/header types are allowed.
Also the syntax has to be extended to support list types.

```p4
struct headers {
    ipv4_option_timestamp_t ipv4_option_timestamp;
}
extern bit<16> get<T>(in T data);
get<headers>({ hdr.ipv4_option_timestamp });
```

<details>
<summary>Tests</summary>
* annotation-bug.p4
* assign.p4
* const.p4
* gauntlet_hdr_function_cast-bmv2.p4
* gauntlet_hdr_in_value-bmv2.p4
* gauntlet_hdr_init-bmv2.p4
* gauntlet_hdr_set_valid-bmv2.p4
* gauntlet_set_valid_in_function-bmv2.p4
* gauntlet_uninitialized_bool_struct-bmv2.p4
* invalid-hdr-warnings4.p4
* issue1210.p4
* issue2355.p4
* issue2487.p4
* issue2648.p4
* issue2657-bmv2.p4
* issue2957.p4
* issue3671-1.p4
* issue3672.p4
* issue4133.p4
* issue841.p4
* issue933.p4
* issue982.p4
* names.p4
* nested-tuple1.p4
* next-def-use.p4
* pna-dpdk-add_on_miss0.p4
* pna-dpdk-wrong-warning.p4
* pna-example-tcp-connection-tracking.p4
* spec-ex14.p4
* specialization.p4
* struct.p4
* struct_assignment_optimization.p4
* struct_init.p4
* structure-valued-expr-ok-1-bmv2.p4
* version.p4
* wrong-warning.p4
</details>

#### List well-formedness

Also, list types accept more nested types than a tuple type does.
Current implementation treats `{ expr }` as tuples, resulting in bogus well-formedness errors.

```p4
struct S {
    bit<32> x;
}
...
S s2;
s2 = { 0 };
```

<details>
<summary>Tests</summary>
* copy.p4
* default-control-argument.p4
* gauntlet_complex_initialization-bmv2.p4
* gauntlet_extern_arguments_2.p4
* gauntlet_function_if_hdr_return-bmv2.p4
* gauntlet_hdr_int_initializer-bmv2.p4
* guantlet_instance_overwrite-bmv2.p4
* guantlet_int_slice-bmv2.p4
* gauntlet_list_as_in_argument-bmv2.p4
* gauntlet_variable_shadowing-bmv2.p4
* invalid-hdr-warnings2.p4
* invalid-hdr-warnings5.p4
* invalid-hdr-warnings6.p4
* issue1006.p4
* issue1638.p4
* issue1670-bmv2.p4
* issue1863.p4
* issue2036-3.p4
* issue2261.p4
* issue2289.p4
* issue2303.p4
* issue232-bmv2.p4
* issue2383-bmv2.p4
* issue2383-bmv2.p4
* issue242.p4
* issue2488-bmv2.p4
* issue2543-1.p4
* issue2543-2.p4
* issue2795.p4
* issue2958.p4
* issue3238.p4
* issue3806.p4
* issue396.p4
* issue4288.p4
* issue529.p4
* issue696-bmv2.p4
* issue907-bmv2.p4
* issue933-1.p4
* list-compare.p4
* nested-struct.p4
* nested-tuple.p4
* nested_select.p4
* pna-dpdk-header-union-stack.p4
* pna-dpdk-header-union-stack1.p4
* pna-dpdk-header-union-stack2.p4
* pna-dpdk-invalid-hdr-warnings5.p4
* pna-dpdk-invalid-hdr-warnings6.p4
* select-struct.p4
* simplify_method_calls.p4
* struct1.p4
* table-entriees-no-arg-actions.p4
* tuple.p4
* tuple0.p4
* tuple1.p4
* tuple3.p4
* tuple4.p4
</details>

### Support generic structs and headers

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
* p4rt_digest_complex.p4
* stack-init.p4
</details>

### Support `match_kind` as a primitive type

Spec v1.2.3 adds `match_kind` as a base type that can be parsed.

```p4
const tuple<match_kind> exact_once = ...;
```

<details>
<summary>Tests</summary>
* issue3091-1.p4
</details>

### Support `...` default grammar

The spec says, "A left-value can be initialized automatically with default value of the suitable type using the syntax `...` (see Section 7.3)." (8.26).

### Support `priority` of table entry

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

### Support general switch statement

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

### Support `{#}` syntax

"The expression `{#}` represents an invalid header of some type, but it can be any header or header union type. A P4 compiler may require an explicit cast on this expression in case where it cannot determine the particular header of header union type from the context." (8.26).

```p4
h = (H) {#};
```

<details>
<summary>Tests</summary>
* invalid-header.p4
* invalid-union.p4
* issue3779.p4
* issue4625_remove_compile_time_bool_methodcall_of_mcs.p4
</details>

# Need Spec Clarification

## Should we add implicit cast for directionless parameter?

I think we should, especially for constructor invocations.
Waiting for the spec clarification, [issue#1312](https://github.com/p4lang/p4-spec/issues/1312).

Below apply for actions, methods, and functions.

```p4
action a(inout bit<32> b, bit<32> d) { ... }
a(x, 0);
```

<details>
<summary>Tests</summary>
* action-bind.p4
* action_call_table_ebpf.p4
* bvec-hdr-bmv2.p4
* default-action-arg-bmv2.p4
* default_action-bmv2.p4
* default_action-ubpf.p4
* extern-funcs-bmv2.p4
* extern2.p4
* issue1001-bmv2.p4
* issue1043-bmv2.p4
* issue1642-bmv2.p4
* issue1653-bmv2.p4
* issue1653-complex-bmv2.p4
* issue1660-bmv2.p4
* issue1765-1-bmv2.p4
* issue1834-bmv2.p4
* issue323.p4
* issue3246-1.p4
* issue3488-1-bmv2.p4
* issue364-bmv2.p4
* issue383-bmv2.p4
* issue3884.p4
* issue562-bmv2.p4
* lpm_ubpf.p4
* m_psa-dpdk-non-zero-arg-default-action-08.p4
* named-arg1.p4
* pna-example-ipsec.p4
* psa-dpdk-non-zero-arg-default-action-01.p4
* psa-dpdk-non-zero-arg-default-action-02.p4
* psa-dpdk-non-zero-arg-default-action-08.p4
* psa-dpdk-non-zero-arg-default-action-09.p4
* psa-dpdk-table-entries-exact-ternary.p4
* table-entries-exact-bmv2.p4
* table-entries-lpm-bmv2.p4
* table-entries-optional-bmv2.p4
* table-entries-priority-bmv2.p4
* table-entries-range-bmv2.p4
* table-entries-ser-enum-bmv2.p4
* table-entries-ternary-bmv2.p4
* table-entries-valid-bmv2.p4
* table-key-serenum-bmv2.p4
* v1model-const-entries-bmv2.p4
* v1model-special-ops-bmv2.p4
* xor_test.p4
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
* issue1097-2-bmv2.p4
* issue1097-bmv2.p4
* issue1814-1-bmv2.p4
* issue1814-bmv2.p4
* issue1958.p4
* issue298-bmv2.p4
* issue754.p4
* pr1363.p4
* psa-action-profile1.p4
* psa-action-profile3.p4
* psa-action-profile4.p4
* psa-basic-counter-bmv2.p4
* psa-counter1.p4
* psa-counter2.p4
* psa-counter3.p4
* psa-custom-type-counter-index.p4
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

## Should an argument to directionless action parameter be compile-time known?

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

## Some extern functions seem to produce a (local) compile-time known value, but syntax does not reveal this

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

## Is it legal to divide a fixed-width integer? 

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

## Is it legal to coerce a fixed width integer to an arbitrary precision integer?

I think it is illegal, but the test case below seem to violate this.

```p4
const int z1 = 2w1;
```

<details>
<summary>Tests</summary>
* issue2444.p4
* issue3283.p4
</details>

## Is it legal to coerce a structure-valued expression to a struct type?

I think it is allowed, but the spec does not explicitly mention this.

```p4
struct S {
    bit<32> a;
    bit<32> b;
}
bool b5 = (S) { a = 1, b = 2 } == { a = 1, b = 2 };
```

<details>
<summary>Tests</summary>
* issue3057-2.p4
</details>

## Equivalence of table actions

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

# Unsupported features

## Custom table element

### `implementation`

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

### `counters`

```p4
table ipv4_da_lpm {
    ...
    counters = ...;
}
```

<details>
<summary>Tests</summary>
* issue461-bmv2.p4
</details>

### `junk`

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

### `meters`

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

### `add_on_miss`

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
* pna-dpdk-direct-counter-learner.p4
* pna-dpdk-direct-meter-learner.p4
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

### `psa_direct_counter`

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
</details>

### `psa_direct_meter`

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

### `psa_idle_timeout`

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

## Annotations in general

### Optional argument

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
* issue2492.p4
* issue2630.p4
* issue2664-bmv2.p4
* issue2810.p4
* issue3051.p4
* pna-dpdk-direct-counter.p4
</details>

# Future extension

## For loops

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

## Generic parser/control declaration

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

## Concatenation of string literals

```p4
log("Log message" ++ " text");
```

<details>
<summary>Tests</summary>
* issue4932.p4
* spec-issue1297-string-cat.p4
</details>

# Should be a negative test instead?

## issue2514.p4

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

## issue3287.p4

This shifts by a signed integer, which is illegal.
This should be a negative test.

```p4
bit<4> func(in bit<4> l) {
  const int<6> tt = 1;
  return l << tt;
}
```

## pipe.p4

`ternary` is defined twice.
This should be a negative test.

```p4
#include <core.p4>
match_kind {
    ternary,
    exact
}
```

## spec-ex25.p4

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
