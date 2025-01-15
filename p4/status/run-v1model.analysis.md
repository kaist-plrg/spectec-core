# A. Fix Logic

## 1. Typecheck error

* table-entries-ser-enum-bmv2.p4

Refer to **A.1.(3) Implicit cast involving serializable enums** in [typechecker error analysis](typecheck-pos.analysis.md).

## 2. Stf parser error

* ipv6-switch-ml-bmv2.stf

## 3. Unknown STF statement

```plaintext
add test1 0 data.f1(0x****0101) ingress.setb1((val, 0x7f), (port, 2))
```

* ternary2-bmv2.stf

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
