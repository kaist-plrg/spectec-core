# Need Test Clarification

## 1. Using `egress_port` instead of `egress_spec` for output port

```p4
standard_metadata.egress_port = 9;
// and expects output port as 9
```

* equality.p4

## 2. Wrong output port

```p4
control MyEgress(inout head[13] hdr,
                 inout metadata meta,
                 inout standard_metadata_t standard_metadata) {
    ...
    apply {
        standard_metadata.egress_spec = 6;
        standard_metadata.ingress_port = 1;
        my_table.apply();
        exit;
    }
}
// and expects output port as 1
```

* table2.p4
* table3.p4

## 3. Seemingly incorrect behavior of saturating arithmetic

```p4
int<8> w = 8s117 |-| 8s75 |-| 8s128 |-| 8s128;
// test expects -128
// but think it should be 127 because 8s128 is actually -127
```

* int.p4

With a patched STF test, the test would pass.
