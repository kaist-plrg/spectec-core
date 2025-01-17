# A. Fix Logic

## 1. Typecheck error

* table-entries-ser-enum-bmv2.p4

Refer to **A.1.(3) Implicit cast involving serializable enums** in [typechecker error analysis](typecheck-pos.analysis.md).

## 2. Stf parser error

* ipv6-switch-ml-bmv2.stf

# B. Need Test Clarification

## 1. Payload is not extracted

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

* gauntlet_invalid_hdr_short_circuit-bmv2.p4

## 2. Header is not emitted

```plaintext
expect 2 00000101 ******** **** 7f 66
// expect 2 00000101 ******** **** 7f 66 **** ** **
packet 0 00000101 00000202 0303 55 66 7777 88 00
```

The test does not expect the header stack to be emitted.
After patching the SFT test as in the comment, the test would pass.

* ternary2-bmv2.stf

# C. Unsupported Features

## 1. Table priority annotation

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

* table-entries-priority-bmv2
