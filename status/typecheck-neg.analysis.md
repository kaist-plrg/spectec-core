# A. Fix Logic

## 1. Function definition and calls

Restricting function well-formedness (parameter types and return types) and valid call-sites.

### (1) Function well-formedness

e.g., action should not take an `int` parameter.

```p4
action a(int x) {}
```

Also, type inference may specialize an ill-formed function.

```p4
control C() { apply { } }
void f<T>(T t) {}
control D() {
    C() c;
    apply { f(c); }
}
```

<details>
<summary>Tests</summary>

* issue2260-1.p4
* issue2354-1.p4
* issue2354.p4
* issue2454.p4
* issue3273.p4
* issue584.p4
* issue764.p4
* issue816.p4
* issue818.p4
* param.p4
* parser-arg.p4
* string-e.p4
* string-e2.p4
</details>

For actions, the spec mentions:

> Action parameters that have no direction (e.g., port in the previous example) indicate “action data.” All such parameters must appear at the end of the parameter list. (14.1)

```p4
action a(bit x0, out bit y0) {
    bit x = x0;
    y0 = x0 & x;
}
```

<details>
<summary>Tests</summary>

* directionless.p4
</details>

### (2) Function call-sites

Relating to valid call-sites,

```p4
control c() {
    action a() {}
    table t {
        actions = { a(); }
        default_action = a();
    }
    action b() {
        t.apply();  // cannot invoke table from action
    }
    apply {}
}
```

<details>
<summary>Tests</summary>

* call-table.p4
* issue1331.p4
* issue2597.p4
* issue2835-bmv2.p4
* issue388.p4
* issue413.p4
</details>

Also table application is disallowed in action argument.
But not sure how to enforce this.

```p4
a_two(
    t_two.apply().hit ?
        bump_val(hdr.ethernet.etherType) :
        hdr.ethernet.etherType,
    bump_val(hdr.ethernet.etherType));
```

<details>
<summary>Tests</summary>

* issue2835-bmv2.p4
</details>

## \[DONE\] 2. ~~Duplicate declarations in the same namespace~~

### \[DONE\] (1) ~~Duplicate id~~

```p4
control c(bit<32> p)(bool p) {
  apply {}
}
```

### \[DONE\] (2) ~~Duplicate constants~~

```p4
const bit<4> a = 1;
const bit<4> a = 2;
```

### \[DONE\] (3) ~~Duplicate action/functions~~

```p4
action foo (in bit<8> x, out bit<8> y) { y = (x >> 2); }
action foo (inout bit<8> x) { x = (x >> 3); }
```

### \[DONE\] (4) ~~Duplicate switch label~~

```p4
apply {
    switch (t.apply().action_run) {
        a: { arun = 1; }
        a: { arun = 1; }  // duplicate label
    }
}
```

## 3. Table property

### (1) Missing table action property

```p4
table t {
    key = { x : exact; }
}
```

<details>
<summary>Tests</summary>

* missing_actions.p4
</details>

### (2) Specifying table entry with empty key

```p4
table t {
    key = {}
    actions = { a; }
    const entries = {
      _ : a();
    }
}
```

<details>
<summary>Tests</summary>

* issue3442.p4
</details>

### (3) Table properties are sensitive to the order of declaration

```p4
table badtable {
    default_action = nop;
    actions = { nop; }
}

apply {
    badtable.apply();
}
```

<details>
<summary>Tests</summary>

* issue2033.p4
* table-entries-decl-order.p4
</details>

### (4) Bad mask for longest prefix match

```p4
// value has 1s outside of field bit width
0x100 &&& 0xF0 : a_with_control_params(11);

// mask has 1s outside of field bit width
0x11 &&& 0x1F0 : a_with_control_params(12);

// mask that is not a prefix mask
0x11 &&& 0xE1 : a_with_control_params(13);

// another mask that is not a prefix mask, and has 1 bits
// outside of field bit width.
0x11 &&& 0x181 : a_with_control_params(14);

// exact match value with value having 1s outside of field
// bit width
0x100 : a_with_control_params(15);
```

<details>
<summary>Tests</summary>

* table-entries-lpm-2.p4
</details>

## 4. Compile-time evaluation

### (1) Stack bounds check if compile-time known

```p4
// Compiler should give error for out of bounds index,
// if due to compile-time known value index.
h.hs[-1].setValid();
h.hs[-1].f1 = 5;
h.hs[-1].f2 = 8;

// No error or warning for these -- they are good.
h.hs[0].setValid();
h.hs[0].f1 = 5;
h.hs[0].f2 = 8;
h.hs[2].setValid();
h.hs[2].f1 = 5;
h.hs[2].f2 = 8;

// Compiler should give error for out of bounds index,
// if due to compile-time known value index.
h.hs[3].setValid();
h.hs[3].f1 = 5;
h.hs[3].f2 = 8;
```

<details>
<summary>Tests</summary>

* stack-const-index-out-of-bounds-bmv2.p4
</details>

### (2) Bitslice width check

```p4
bit<8> n = 8w0b11111111;
n[7:4][5:2] = 4w0;
```

<details>
<summary>Tests</summary>

* slice_out_of_bound.p4
</details>

### (3) Division or modulo with negative value

P4 forbids division or modulo with negative values.
But the question is, can we guarantee this at compile time?

```p4
action act() {
    bit<8> a;
    a = - 8 / -2;
    a = -10 % 2;
    a = 10 % -2;
}
```

<details>
<summary>Tests</summary>

* div.p4
</details>

## (4) Select cases should be compile-time known?

```p4
transition select(hdr.ethernet.etherType) {
    0x0806 .. 0x0800 : parse_ipv4;
    2054 .. 2048 : parse_ipv4;
    hdr.ipv4.totalLen .. 0x0800 : parse_ipv4;
    0x0800 .. hdr.ipv4.totalLen : parse_ipv4;
    default: accept;
}
```

```p4
transition select(hdr.eth_hdr.eth_type) {
    simple_action(): reject;
    default: accept;
}
```

```p4
bit<8> x;
state start {
    transition select(8w0) {
        x &&& x: accept;
        default: reject;
    }
}
```

<details>
<summary>Tests</summary>

* issue-2123_e.p4
* issue122.p4
* issue3430.p4
</details>

## 5. Context-sensitivity (some impose implicit domain-specific P4 knowledge)

### (1) `.last`, `.lastIndex` only allowed within parser

```p4
action a(in s v) {
    bit<32> t = v.field.lastIndex;
}
```

```
control c() {
    h[10] stack;

    apply {
        stack.last = { 10 };
    }
}
```

<details>
<summary>Tests</summary>

* issue3522.p4
* next.p4
</details>

### (2) Calling `verify` within a control

But this is specific to P4 invariant.
Not something that can be inferred from the code itself.

```p4
control C() {
  apply {
    verify(8w0 == 8w1, error.Oops);
   }
}
```

<details>
<summary>Test</summary>

* control-verify.p4
</details>

### (3) No switch inside action

But maybe this is only specific to `p4test`.

```p4
action a0() {
	switch (m.m0) {
	    8w0x0: {}
	}
}
```

<details>
<summary>Tests</summary>

* issue3299.p4
</details>

### (4) No instantiation within function body

```p4
package myp(bit a);
void func() {
    myp(1w1) a;
}
```

<details>
<summary>Tests</summary>

* issue3271.p4
</details>

### (5) Already expecting `NoAction`

```p4
action NoAction(bit t) {}
control c() {
    table t {
        actions = {}
    }
    apply {}
}
```

```
issue3644-1.p4(1): [--Werror=Target model error] error: NoAction: Expected an action with no parameters; did you include core.p4?
action NoAction(bit t) {}
       ^^^^^^^^
```

```p4
const bit NoAction = 1;
control c() {
    table t {
        actions = {}
    }
    apply {}
}
```

<details>
<summary>Tests</summary>

* issue3644-1.p4
* issue3644-2.p4
* issue3644.p4
</details>

### (6) `main` should be a package

```p4
extern MyExtern {
   MyExtern();
}
MyExtern() main;
```

<details>
<summary>Tests</summary>

* issue4140.p4
* issue4144.p4
</details>

## 6. Devils are in the details

### (1) Type inference should fail

```p4
b.extract(_);
```

<details>
<summary>Tests</summary>

* issue774-2.p4
</details>

### (2) Table invocation results are incomparable

```p4
table tt {
    actions = {}
}
apply {
    if (tt.apply() == tt.apply()) {}
}
```

```p4
if (tt.apply().action_run == tt.apply().action_run)
```

The spec mentions:

> table.apply().action_run, which can only appear as the expression of a switch statement (see Section 12.7), and when it appears there, it must be the entire expression. (12.5)
> Any expression containing table.apply().hit or table.apply().miss (see Section 14.2.2), which can be part of arbitrarily complex expressions in many places of a P4 program. (12.5)

<details>
<summary>Tests</summary>

* issue3846.p4
* issue3847.p4
</details>

### (3) Nesting `int` inside a value set

```p4
value_set<int>(4) myvs;
```

<details>
<summary>Tests</summary>

* issue3346.p4
</details>

### (4) Implicit cast omitting an intermediate step

```p4
typedef bit<1> b1;
typedef bit<2> b2;
type b1 t1;
type b2 t2;

t1 func(b2 a) {
  return (t1)a;
}
```

Here, we are trying to cast `bit<2>` to `t1`, which is a new type for `bit<1>`. But we *cannot* implicitly cast `bit<2>` to `bit<1>`.

<details>
<summary>Tests</summary>

* issue3221.p4
</details>

### \[DONE\] (5) ~~Parser swallows the unary `+`~~

```ocaml
| info1 = PLUS exp = expression %prec PREFIX
    { (*let info2,exp = exp in*)
      let tags = Source.merge info1 (Expression.tags exp) in
      Expression.update_tags exp tags }
```

So the below program is checked as correct.

```p4
const bool b = +false;
```

### (6) Parser not ending in `accept` nor `reject`

```p4
state start {
    transition state_1;
}
state state_1 {
    transition state_2;
}
state state_2 {
    transition state_3;
}
state state_3 {
    transition state_2;
}
```

<details>
<summary>Tests</summary>

* issue2373.p4
</details>

### \[DONE\] (7) ~~Instantiation without abstract method~~

```p4
extern g {
  g();
  abstract void h();
}
package p(g a);
p(g()) main;
```

### (8) Shift and arbitrary precision integer

```p4
header H {
    bit<8> a;
    bit<8> b;
    bit<8> c;
}
struct Headers {
    H h;
}
control ingress(inout Headers h, inout Meta m, inout standard_metadata_t sm) {
    apply {
        h.h.a = (1 << h.h.c) + 8w2;
    }
}
```

```p4
const int a = 5;
hdr.v = (bit<8>)(a >> b);
```

<details>
<summary>Tests</summary>

* issue2206.p4
* shift-int-non-const.p4
</details>

### \[DONE\] (9) ~~Method with same name as object~~

```p4
extern X {
    void X();
}
```

# B. Need spec clarification

## 1. Test too strict on explicit cast

```p4
struct s {
  bit t;
  bit t1;
}

s func(bit t, bit t1) {
  return (s)(s)(s){t, t1};
}
```

[Related Discussion](https://github.com/p4lang/p4c/issues/3233)

Rationale for disallowing this: the P4 spec does not allow explicit cast to a struct type, with one exception, when initializing a struct variable.

But maybe this is too strict?

<details>
<summary>Tests</summary>

* issue3233.p4
</details>

## 2. Directionless action arguments in a program acts like `in`?

Partly yes, because they can be implicitly cast (ongoing PR exists).
Partly no, because they must be compile-time known.

```p4
action b(inout bit<32> x, bit<8> data) {
    meta.a = meta.a ^ (bit<4>) x ^ (bit<4>) data;
}
table t1 {
    key = { hdr.ethernet.srcAddr : exact; }
    actions = { b(meta.c); }
    default_action = b(meta.c, (bit<8>) meta.d);
}
```

<details>
<summary>Tests</summary>

* issue473.p4
</details>

# C. Need test clarification

## 1. Implicit cast between a serializable enum and its underlying type should be allowed

```p4
enum bit<32> X {
    Zero = 0,
    One = 1
}
...
X y = 1;    // Error: no implicit cast to enum
y = 32w1;   // Error: no implicit cast to enum
```

```p4
enum bit<8> myEnum { One = 1 }
struct S {
    myEnum val;
}
...
S s1 = { val = (bit<8>)0 };
```

```p4
enum bit<2> e{ t = 1 }

e f(in bool c, in bit<2> v0, in bit<2> v1) {
  return c ? v0 : v1;
}
e f1(in bool c, in bit<2> v0, in e v1) {
  return c ? v0 : v1;
}
e f2(in bool c, in e v0, in bit<2> v1) {
  return c ? v0 : v1;
}
```

```p4
enum bit<4> e { a = 1 }
control c(in e v) {
  apply {
    switch(v) {
      4w1:
      default: {}
    }
  }
}
```

<details>
<summary>Tests</summary>

* enumCast.p4
* issue2220.p4
* issue3534.p4
* issue3623-2.p4
* issue3623-3.p4
* serEnumImplCast.p4
</details>

## 2. Comparison between sequence and record types

```p4
bool b1 = { 1, 2 } == { 1, 2 };
bool b2 = { a = 32w1, b = 32w2 } == { a = 32w1, b = 32w2 };
bool b2_ = { a = 1,b = 2 } == { a = 1, b = 2 };
```

I think this should be allowed.

<details>
<summary>Tests</summary>

* issue3057-1.p4
</details>

## 3. Too strict for indirect recursive type?

```p4
struct h<t>{
  t f;
}
typedef h<bit> tt;
typedef h<tt> t;
```

<details>
<summary>Tests</summary>

* issue3291.p4
</details>

## 4. Struct parameter not allowed for an action?

```p4
struct choices_t {
  s1_t entry0;
  s1_t entry1;
  s1_t entry2;
  s1_t entry3;
}
...
action select_entry(choices_t choices) { ... }
```

<details>
<summary>Tests</summary>

* issue532.p4
</details>

## 5. Constructors and functions live in the same namespace?

```p4
control foo (in bit<8> x, out bit<8> y) { apply { y = x + 7; } }
bool foo() { return true; }
```

p4c rejects this program because:

```
p4c/testdata/p4_16_errors/issue1932.p4(2): [--Werror=duplicate] error: Re-declaration of foo with different type:
bool foo(
     ^^^
p4c/testdata/p4_16_errors/issue1932.p4(1)
control foo (in bit<8> x, out bit<8> y) { apply { y = x + 7; } }
        ^^^
[--Werror=overlimit] error: 1 errors encountered, aborting compilation
```

But we can distinguish the use of an identifier as a function or a constructor from the syntax. So we may consider them as living in different namespaces, so the above program should (or can) be accepted.

<details>
<summary>Tests</summary>
* issue1932.p4
</details>

## 6. Scope of a control parameter

What is the scope of a control parameter?
Does it live in the same level as the local declarations, or does it live in the same level as the `apply` block?

```p4
control ingress(inout Headers h) {
    apply {
        Headers h = h;
    }
}
```

The above case is categorized as ill-typed, by the p4c compiler.
But the below case passes the type check.

```p4
control ingress(inout Headers h) {
    Headers h = h;
    apply {}
}
```

They imply that the control parameter is in the same level as the `apply` block, but not in the same level as the control local declarations.

So it suggests two cases:

(i) Local declarations are above the level of control parameters and `apply` block. (`local` > `control parameter` = `apply`)

This way, we are viewing

```p4
control ingress(inout Headers h) {
    Headers local = h;
    apply {
        Headers app = local;
    }
}
```

as,

```p4
control ingress {
    Headers local;
    apply (inout Headers h) {
        local = h;
        Headers app = local;
    }
}
```

This gives one explanation for the above case.
However, this will no longer justify the below case.

```p4
control ingress(inout Headers h) { // 1. if we move this to apply,
    Headers local = h;
    action a() {
        Headers act = h; // 2. error: h is not defined
    }
    apply {
        Headers app = local;
    }
}
```

Adding more to its strangeness,

```p4
control C(inout bit<32> x) {
   action a() { bit<32> x = x; }
   apply {}
}
```

```
dup.p4(7): [--Werror=shadow] error: declaration of 'x' shadows a parameter 'x'
   action a() { bit<32> x = x; }
                ^^^^^^^^^^^^^
dup.p4(6)
control C(bit<32> x) {
```

```p4
control C(inout bit<32> x) {
   bit<32> x = 3;
   action a() { bit<32> x = x; }
   apply {}
}
```

```
dup.p4(7): [--Wwarn=shadow] warning: 'x' shadows 'x'
   bit<32> x = 3;
   ^^^^^^^^^^^^^
dup.p4(6)
control C(bit<32> x) {
                  ^
dup.p4(8): [--Wwarn=shadow] warning: 'x' shadows 'x'
   action a() { bit<32> x = x; }
                ^^^^^^^^^^^^^
dup.p4(7)
   bit<32> x = 3;
   ^^^^^^^^^^^^^
```

above is rejected and below is accepted. (above implies that `param` = `apply` and below implies that `param` > `local` > `apply`)

(ii) Local declarations are below the level of control parameters and `apply` block. (`local` < `control parameter` = `apply`)

This is not true because the `apply` block can access the local declarations.

<details>
<summary>Tests</summary>
* issue2544_shadowing1.p4
* issue2545.p4
</details>

# D. More than a type check? (Requiring domain-specific knowledge)

## 1. Semantics of packet extraction

Some tests impose implicit (at least in the code syntax level) restrictions.

```p4
parser P(packet_in p, out bit<32> h) {
    state start {
        p.extract(h);  // error: not a header
        transition accept;
    }
}
```

```p4
parser P(packet_in p, out H h) {
    state start {
        p.extract(h, 32);  // error: not a variable-sized header
        transition accept;
    }
}
```

```p4
struct my_packet {
    h_t[10] h;
}
parser MyParser(packet_in b, out my_packet p) {
    state start {
        b.extract(p.h);  // error: not a header
        transition accept;
    }
}
```

```p4
header h_t {
    bit<8> f;
    varbit<8> g;
}
struct my_packet {
    h_t h;
}
parser MyParser(packet_in b, out my_packet p) {
    state start {
        b.extract(p.h);  // error: variable-sized header
        transition accept;
    }
}
```

<details>
<summary>Tests</summary>

* extract.p4
* extract1.p4
* issue477.p4
* issue478.p4
* twovarbit.p4
</details>

## 2. Semantics of packet lookahead

```p4
packet.lookahead<void>();
```

```p4
header H {
    varbit<120> x;
}
...
h = pkt.lookahead<H>();
```

<details>
<summary>Tests</summary>

* issue4146.p4
* issue600.p4
</details>

# E. Unsupported

## 1. Practical concerns: using `exact` match kind internally in a `switch` implementation

```p4
control c(in bit<4> a) {
  apply {
    switch (a) {
      4w0: {}
    }
  }
}
```

```
[--Werror=not-found] error: Could not find declaration for 'match_kind.exact', which is needed to implement switch statements; did you include core.p4?
```

But this is unnecessary for p4cherry.

<details>
<summary>Tests</summary>

* issue3613.p4
</details>

## 2. Practical concerns: large number

```p4
// this expression will slow the compiler to a crawl to print a warning
h.eth_hdr.eth_type =  1985245330 << 903012108;
```

This is irrelevant for p4cherry.

<details>
<summary>Tests</summary>

* issue2496.p4
</details>

## 3. Annotation

```p4
@pkginfo
const bit<32> x = 0;
```

```p4
table t1 {
    key = {
        hdr.ethernet.etherType: exact;
    }
    actions = {
        @tableonly a1;
        a2;
        @defaultonly a3;
    }
    const entries = {
        // Ideally the following line should cause an error during
        // compilation because action a3 is annotated @defaultonly
        3 : a3();
    }
    // Ideally the following line should cause an error during
    // compilation because action a1 is annotated @tableonly
    default_action = a1;
}
```

```p4
@match(1+1) bit<16> f16;
```

```p4
@name(".t0")
table t0 {
    key = { smeta.ingress_port : exact; }
    actions = { drop; NoAction; }
    const default_action = NoAction();
}
```

```p4
key = { h.a + h.a : exact; }
```

```
key-name.p4(28): [--Werror=expected] error: h.a + h.a: Complex key expression requires a @name annotation
        key = { h.a + h.a : exact; }
                ^^^^^^^^^
```

```p4
header Hdr {
    varbit<256> data0;
    @length(data0) // illegal: expression is not uint<32>
    varbit<12> data1;
    @length(size2) // illegal: cannot use size2, defined after data2
    varbit<256> data2;
    int<32> size2;
}
```

<details>
<summary>Tests</summary>

* annotation.p4
* issue1580.p4
* issue1732.p4
* issue1803_same_table_name.p4
* issue2283_2-bmv2.p4
* key-name.p4
* spec-ex32_e.p4
* structured-annotation-e1.p4
* structured-annotation-e2.p4
* structured-annotation-e3.p4
</details>
