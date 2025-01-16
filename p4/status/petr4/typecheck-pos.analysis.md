# Nees Spec Clarification

## \[REPORTED\] 1. Restrictions on call sites: [call-site-restrictions](../test/petr4/program/well-typed-excluded/spec-clarify/call-site-restrictions)

Waiting for spec clarification, [Issue#1349](https://github.com/p4lang/p4-spec/issues/1349) and [PR#1356](https://github.com/p4lang/p4-spec/pull/1356).

```p4
control MyIngress(inout hdr_t hdr,
                  inout metadata meta,
                  inout standard_metadata_t standard_metadata) {
  bit<8> y = 0xFF;
  bit<8> z = true ? f(y) : g(y);
  // ...
}
```

Above test expect that functions are callable from a control top level.

# Need Test Clarification

## 1. Petr4 is permissive, in accepting duplicate declarations: [duplicate-decl](../test/petr4/program/well-typed-excluded/test-clarify/duplicate-decl)

```p4
bit<8> x = hdr.h.x;
action nop() { hdr.h.x = x; }
bool x;
```

```p4
control C(inout h_t h, in bool x) {
  bit<8> x = 0xFF;
  // ...
}
```

## 2. Passing a dynamic value to a directionless parameter: [dynamic-value-to-directionless](../test/petr4/program/well-typed-excluded/test-clarify/dynamic-value-to-directionless)

```p4
parser MySubParser(packet_in packet,
                   inout head[11] hdr,
                   standard_metadata_t standard_metadata) {
   // ...
}

parser MyParser(packet_in packet,
                out head[11] hdr,
                inout metadata meta,
                inout standard_metadata_t standard_metadata) {
    MySubParser() subparser;
    // ...
    state evoke_subparser {
        subparser.apply(packet, hdr, standard_metadata);
        // ...
    }
}
```
