# A. Fix Logic

## 1. Typecheck error

* table-entries-ser-enum-bmv2.p4

Refer to **A.1.(3) Implicit cast involving serializable enums** in [typechecker error analysis](typecheck-pos.analysis.md).

## 2. Stf parser error

* ipv6-switch-ml-bmv2.stf

## 3. Unknown STF statement

```plaintext
add simple_table  hdr.h.b(0x55) do_something()
add test_ingress.pre_tbl1  hdr.ipv4.dst_addr(0xcb0b3bfe) test_ingress.action3()
add c.t  e(0) c.a()
add t1  ethernet.srcAddr.slice(0x1f) dstAddr_lsbs(0x010101010101) etherType_less_10(0xf00f) ingressImpl.foo((out_port, 3))
add test1 0 data.f1(0x****0101) ingress.setb1((val, 0x7f), (port, 2))
```

* issue2153-bmv2.stf
* issue3488-bmv2.stf
* key-bmv2.stf
* match-on-exprs-bmv2.stf
* ternary2-bmv2.stf

## 4. Unknown Extern

```plaintext
verify_checksum(condition, data, checksum, algo)
update_checksum(condition, data, checksum, algo)
hash(result, algo, base, data, max)
```

* checksum-l4-bmv2.p4
* checksum1-bmv2.p4
* checksum2-bmv2.p4
* checksum3-bmv2.p4
* constant-in-calculation-bmv2.p4
* issue1049-bmv2.p4
* issue655-bmv2.p4

## 5. Out-of-bounds stack access

* gauntlet_index_5-bmv2.p4

## 6. Handle ParserInvalidArgumentError

* test-parserinvalidargument-error-bmv2.p4

## 7. Requires better l-value handling, related to direct invocation

* issue1566-bmv2.p4

Direct invocation is transformed into a call to `ValueE` in the instantiation phase, but there is no support for l-values in `ValueE`.
Maybe instantiation should rather translate this into first a binding to a temporary variable and then a call to that variable instead.

## 8. `push_front` and `pop_back` built-in methods

* header-stack-ops-bmv2.p4

# B. Need Test Clarification

## 1. gauntlet_invalid_hdr_short_circuit-bmv2

```plaintext
[FAIL] Expected: 0 0000000000000000000000000000
       Got:      0 0000000000000000000000000000000000
```

The trailing 3 bytes should be the payload, i.e., should not be extracted by the P4 program.
The program defines a header

```p4
struct Headers {
    ethernet_t eth_hdr;
    H h;
}
```

and only extracts the `eth_hdr` header:

```p4
pkt.extract(hdr.eth_hdr);
```

Adding `pkt.extract(hdr.h);` would extract the `h` header and the test would pass.

# C. Unsupported Features

## 1. table-entries-priority-bmv2

```p4
const entries = {
    0x1111 &&& 0xF    : a_with_control_params(1) @priority(3);
    0x1181            : a_with_control_params(2);
    0x1181 &&& 0xF00F : a_with_control_params(3) @priority(1);
}
```

Specifying entry priority via `@priority` annotation is not supported in p4cherry.
But patching the test as below would make it pass:

```p4
largest_priority_wins = false;
entries = {
    priority=3: 0x1111 &&& 0xF    : a_with_control_params(1);
                0x1181            : a_with_control_params(2);
    priority=1: 0x1181 &&& 0xF00F : a_with_control_params(3);
}
```
