# fuzz-05-07-19-26-11

##### 1. [ERROR] reduced_250_issue807.stderr

```p4
control C1();
extern E {
  void set1(C1 c1);
}
```

A control object cannot be a run-time parameter to an extern.

> Appendix F
> Externs are not allowed to call parsers or controls, so there is no use in passing objects of those types to them.

##### 2. [NOERROR] reduced_49_list8_F1_P178_1_M0.stderr

```p4
E(((list<_>) ({ ((list<bit<32>>) ({ 10, 6, 3 })), ((list<bit<32>>) ({ 5, 2 })) }))) e;
```

P4-SpecTec is deficient with don't care types.

##### 3. [ERROR] reduced_520_issue1897-bmv2_F1_P269_0_M1.stderr

```p4
enum MeterType { packetsbytes };
extern direct_meter< T > {
  direct_meter(MeterType type);
  direct_meter(MeterType type);
}
```

Fails to reject a duplicate declaration of a constructor.

##### 4. [ERROR] reduced_846_issue4661_pure_extern_function_const_args_F1_P846_2_M0.stderr

```p4
switch (baz()) {
    1: {}
    1: {}
}
```

> 12.7.3. Notes common to all switch statements
> It is a compile-time error if two labels of a switch statement equal each other.

##### 5. [ERROR] reduced_1019_issue3045.stderr

```p4
extern register< T > { void write(in bit< 32 > index, in T value); }
void f< T >(register< T > r, in T t) { r.write(0, t); }
```

An extern cannot be called at run time from a function.

> Appendix F

##### 6. [ERROR] reduced_928_issue816-1.stderr

```p4
package P(C c1, C c2);
control MyC1()(P p) {
    apply {}
}
```

A package cannot be a constructor parameter for a control type.

> Appendix F

##### 7. [ERROR] reduced_171_issue1352-bmv2_F1_P18_0_M0.stderr

```p4
typedef match_kind ip4Addr_t;
```

It is an error to alias `match_kind` with a typedef.

> 7.2.8. Type nesting rules

##### 8. [ERROR] reduced_1224_issue1230.stderr

```p4
table t {
    actions = { NoAction; }
    size = true;
}
```

> 14.2.1.5. Size
> The size is an optional property of a table. When present, its value must always be a compile-time known value that is an integer. The size property is specified in units of number of table entries.

##### 9. [ERROR] reduced_914_function_e1.stderr

```p4
bit<16> max(in bit<16> left, in bit<16> right) {}
```

> 10. Function declarations
> A function with a non-void return type must return a value of the suitable type on all possible execution paths.

##### 10. [DUPLICATE] reduced_1144_issue807.stderr

Same as `reduced_250_issue807.stderr`.

##### 11. [ERROR] reduced_854_issue1025-bmv2_F1_P6_1_M0.stderr

```p4
control VerifyChecksum<H, M>(error a = {#}, inout M meta);
```

Invalid expression `{#}` does not have type `error`.

##### 12. [FUZZY] copy_717_reduced_991_issue2795_F7_P991_2_M0.stderr

```p4
header H {
  bit<32> a;
  bit<32> b;
}

control c(packet_out p)() {
    apply {
        p.emit<H>({ 0, 1, ... });
    }
}
```

Do we allow `...` acting as nothing?

##### 13. [FUZZY] copy_646_reduced_639_default-initializer_F10_P639_1_M1.stderr

```p4
header H {}
control C(out bit<32> x)() {
  H[2] stack;
  apply {
    stack = ((H[2]) ({ { ... }, ... }));
  }
}
```

Do we allow `...` acting as nothing?

##### 14. [FUZZY] copy_1185_reduced_1185_issue3550_F1_P1180_2_M1_F8_P1185_2_M0.stderr

```p4
table tbl {
    key = {
        hdr.ethernet.isValid() : exact;
        hdr.ethernet.dstAddr : exact;
        hdr.ethernet.srcAddr : exact;
        hdr.ipv4.protocol : exact;
        user_meta.key1 : ternary;
        user_meta.key2 : range;
        user_meta.key4 : optional;
    }
    actions = {
        NoAction;
        execute;
    }
    const entries = {
        (true, 48w1, 48w2, default, 48w2 &&& 48w3, 48w2 .. 48w4, default) : execute(48w1);
        (true, 48w1, 48w2, Proto.proto1, 48w2 &&& 48w3, 48w2 .. 48w5, 48w10) : execute(48w1);
        (true, 48w3, 48w3, Proto.proto1, 48w2 &&& 48w3, 48w2 .. 48w6, 48w10) : execute(48w1);
    }
}
```

`exact` match does not allow `default` as a key.

##### 15. [ERROR] reduced_995_issue1331.stderr

```p4
parser P(packet_in pkt);
control MyC(packet_in pkt, inout ethernet ether)(P p) {
    apply { p.apply(pkt); }
}
```

A parser object cannot be called at run time from a control apply method.

> Appendix F

##### 16. [FUZZY] reduced_711_default-initializer_F1_P711_0_M2.stderr

```p4
header_union HU {}
control C(out bit<32> x)() {
    HU[0] hustack;
    apply { hustack = { ... }; }
}
```

Do we allow `...` acting as nothing?

##### 17. [NOERROR] reduced_930_issue3808-1.stderr

```p4
extern f<T> {}
control eq0(f<_> tt) {
    apply {}
}
```

Due to P4-SpecTec's deficiency with don't care types.
But questionable: this does open up the possibility of making controls generic.

##### 18. [DUPLICATE] reduced_971_issue1331.stderr

Same as `reduced_995_issue1331.stderr`.

##### 19. [ERROR] reduced_178_list9_F1_P178_1_M0.stderr

```p4
struct S<T> { T t; }
extern E {
    E(list<S<match_kind>> data);
}
```

`match_kind` cannot be nested inside a struct.

> 7.2.8. Type nesting rules

##### 20. [DUPLICATE] reduced_283_issue816-1.stderr

Same as: `reduced_928_issue816-1.stderr`.

##### 21. [DUPLICATE] reduced_989_issue3045.stderr

Same as: `reduced_928_issue816-1.stderr`.

##### 22. [Error] reduced_1136_issue818.stderr

```p4
control C();
extern WrapControl {
    WrapControl(C c);
}
```

A control object cannot be a constructor parameter to an extern.

> Appendix F

##### 23. [NOERROR] reduced_1244_methodArgDontcare.stderr

```p4
extern E<T> {
    E();
}
control c() {
	E<_>() e;
	apply {
	}
}
```

P4-SpecTec is deficient with don't care types.

##### 24. [DUPLICATE] reduced_906_issue1352-bmv2_F1_P18_0_M0.stderr

Same as: `reduced_171_issue1352-bmv2_F1_P18_0_M0.stderr`.

##### 25. [DUPLICATE] reduced_272_issue818.stderr

Same as: `reduced_1136_issue818.stderr`.

##### 26. [NOERROR] reduced_1097_precedence-lt.stderr

Due to P4-SpecTec parser bug.

##### 27. copy_860_reduced_860_issue1333_F8_P860_0_M0.stderr

```p4
extern E {
  E(tuple<match_kind> x = 0);
  void f(in bit<16> z = 2);
}
```

`0` is not of type `tuple<match_kind>`.
Similar to `reduced_854_issue1025-bmv2_F1_P6_1_M0.stderr`, but this deserves to be a separate test case.

##### 28. [DUPLICATE] copy_639_reduced_639_default-initializer_F10_P639_1_M1.stderr

Same as: `copy_646_reduced_639_default-initializer_F10_P639_1_M1.stderr`.

##### 29. [DUPLICATE] copy_714_reduced_718_default-initializer_F8_P718_1_M1.stderr

Same as: `copy_646_reduced_639_default-initializer_F10_P639_1_M1.stderr`.

##### 30. [ERROR] reduced_643_default-initializer_F1_P643_2_M0.stderr

```p4
struct S {}
control C(out bit<32> x)() {
  apply {    
    S s7 = ((S) ({ 2, 2, ... }));   
  }
}
```

It is clearly an error to provide more values in the initializer.

##### 31. [DUPLICATE] reduced_968_issue3045.stderr

Same as: `reduced_1019_issue3045.stderr`.

##### 32. [FUZZY] copy_1186_reduced_1185_issue3550_F1_P1180_2_M1_F10_P1185_2_M0.stderr

```p4
table tbl {
    key = {
        hdr.ethernet.isValid() : exact;
        hdr.ethernet.dstAddr : exact;
        hdr.ethernet.srcAddr : exact;
        hdr.ipv4.protocol : exact;
        user_meta.key1 : ternary;
        user_meta.key2 : range;
        user_meta.key4 : optional;
    }
    actions = {
        NoAction;
        execute;
    }
    const entries = {
        (true, 48w1, 48w2, _, 48w2 &&& 48w3, 48w2 .. 48w4, default) : execute(48w1);
        (true, 48w1, 48w2, Proto.proto1, 48w2 &&& 48w3, 48w2 .. 48w5, 48w10) : execute(48w1);
        (true, 48w3, 48w3, Proto.proto1, 48w2 &&& 48w3, 48w2 .. 48w6, 48w10) : execute(48w1);
    }
}
```

Similar to `copy_1185_reduced_1185_issue3550_F1_P1180_2_M1_F8_P1185_2_M0.stderr`.
Can we use `_` for an `exact` match?

##### 33. [DUPLICATE] reduced_738_checksum1-bmv2_F1_P504_2_M0.stderr

Similar to: `reduced_854_issue1025-bmv2_F1_P6_1_M0.stderr`.

##### 34. [DUPLICATE] reduced_278_issue816-1.stderr

Same as: `reduced_928_issue816-1.stderr`.

##### 35. [DUPLICATE] reduced_266_issue816-1.stderr

Same as: `reduced_928_issue816-1.stderr`.

##### 36. [DUPLICATE] reduced_264_issue818.stderr

Same as: `reduced_1136_issue818.stderr`.

##### 37. [FUZZY] reduced_1145_gauntlet_set_valid_in_function-bmv2_F1_P9_1_M2.stderr

```p4
extern packet_out {
   list<_> emit<T>(in T hdr);
 }
```

P4-SpecTec is deficient with don't care types.
But seems odd to allow `_` in the return type of a function.

##### 38. [DUPLICATE] reduced_515_v1model-digest-custom-type_F1_P19_2_M1.stderr

Simliar to: `reduced_520_issue1897-bmv2_F1_P269_0_M1.stderr`.

##### 39. [ERROR] reduced_858_annotation-bug_F1_P28_1_M0.stderr

```p4
control C();
package top(C ck = true);
```

Similar to other default parameter bugs.

##### 40. [DUPLICATE] reduced_739_issue1025-bmv2_F1_P6_1_M0.stderr

Same as: `reduced_854_issue1025-bmv2_F1_P6_1_M0.stderr`.

# fuzz-05-18-13-21-59

r_1019_r_233_issue3779_F10_P233_12_M0.stderr
r_884_r_23_issue2795_F10_P23_1_M0.stderr
r_1072_r_233_issue3779_F10_P233_0_M0.stderr
r_764_r_523_intType_F10_P523_11_M0.stderr
r_873_r_773_issue4656_const_fold_generic_switch_label_expr_F10_P773_8_M1.stderr
r_936_r_9_issue3307_F10_P9_0_M2.stderr
r_1170_r_1053_issue1386_F10_P1053_0_M0.stderr
r_718_r_718_default-initializer_F10_P718_3_M0.stderr
r_882_r_9_issue3307_F10_P9_13_M1.stderr
r_180_r_707_default-initializer_F2_P707_8_M0.stderr
r_252_r_75_issue2795_F10_P75_8_M2.stderr
r_252_r_8_issue3307_F5_P8_7_M0.stderr
r_900_r_785_tuple4_F10_P785_1_M0.stderr
r_2_r_1022_methodArgs_F9_P1022_6_M0.stderr
r_871_r_200_crash-typechecker_F10_P200_3_M1.stderr
r_723_r_717_default-initializer_F10_P717_2_M0.stderr
r_1170_r_1053_issue2795_F7_P1053_5_M0.stderr
r_514_r_24_issue2260-2_F10_P24_11_M0.stderr
r_910_r_18_issue3672_F9_P18_0_M1.stderr
r_1021_issue3045-1.stderr
r_1021_issue3045.stderr
r_2_r_1098_hashext2_F9_P1098_14_M1.stderr
r_955_r_229_issue1937_F10_P229_3_M1.stderr
r_1170_r_8_methodArgs_F8_P8_11_M0.stderr
r_171_r_9_issue3307_F10_P9_0_M2.stderr
r_515_r_23_issue2127_F10_P23_12_M0.stderr
r_2_r_831_methodArgs_F10_P831_5_M1.stderr
r_243_r_63_crash-typechecker_F10_P63_6_M1.stderr
r_871_r_6_issue2265-1_F10_P6_4_M1.stderr
r_723_r_716_default-initializer_F10_P716_5_M1.stderr
r_283_issue816-1.stderr
r_515_r_9_list9_F10_P9_6_M0.stderr
r_1268_methodArgDontcare.stderr
r_168_r_23_issue3324_F9_P23_9_M0.stderr
r_514_r_59_issue3671-1_F10_P59_6_M1.stderr
r_1003_issue1331.stderr
r_1019_r_1017_issue3779_F10_P1017_6_M0.stderr
r_1167_r_23_issue2795_F10_P23_3_M1_F10_P1058_0_M0.stderr
r_896_r_849_stack-bmv2_F9_P849_0_M0.stderr
r_1167_r_8_issue2795_F10_P8_12_M0_F10_P1002_14_M1.stderr
r_197_r_88_issue803_F3_P88_11_M0.stderr
r_715_r_494_issue1863_F10_P494_1_M1.stderr
r_955_r_70_invalid-union_F10_P70_7_M0.stderr
r_248_r_23_issue3324_F9_P23_9_M0.stderr
r_884_r_229_issue2599_F10_P229_1_M0.stderr
r_501_enumCast.stderr
r_723_r_716_default-initializer_F10_P716_12_M1.stderr
r_1027_issue1331.stderr
r_896_r_56_stack-init_F10_P56_8_M0.stderr
r_644_r_642_default-initializer_F10_P642_5_M0.stderr
r_185_r_180_generic-struct-tuple_F10_P180_2_M0.stderr
r_873_r_1143_issue3623-1_F9_P1143_9_M0.stderr
r_748_precedence-lt.stderr
r_1170_r_167_issue2795_F7_P167_1_M0.stderr
r_283_issue818.stderr
r_514_r_13_issue3324_F10_P13_5_M0.stderr
r_2_r_1023_methodArgs_F9_P1023_11_M0.stderr
r_243_r_474_issue2599_F9_P474_9_M0.stderr
r_936_r_170_newtype1_F10_P170_0_M0.stderr
r_753_div3.stderr
r_772_div3.stderr
r_1019_r_233_issue3779_F10_P233_4_M0.stderr
r_887_r_81_issue1205-bmv2_F10_P81_2_M0.stderr
r_1073_r_831_issue3779_F10_P831_1_M1.stderr
r_884_r_884_issue2599_F8_P884_1_M1.stderr
r_1169_issue807.stderr
r_715_r_495_issue2036-3_F10_P495_2_M0.stderr
r_1021_issue3045-2.stderr
r_709_r_494_generic-struct-tuple_F10_P494_3_M1.stderr
r_873_r_498_issue3623-1_F8_P498_2_M0.stderr
r_171_r_33_newtype1_F10_P33_2_M2.stderr
r_1000_issue3045.stderr
r_514_r_58_issue3671-1_F10_P58_14_M0.stderr
r_938_r_559_v1model-const-entries-bmv2_F2_P559_2_M0.stderr
r_955_r_1134_issue3343_F9_P1134_14_M0.stderr
r_887_r_77_issue822_F10_P77_4_M1.stderr
r_179_r_75_issue2795_F10_P75_10_M0.stderr
r_683_precedence-lt.stderr
r_715_r_691_issue2036-3_F10_P691_7_M2.stderr
r_278_issue816-1.stderr
r_955_r_951_issue3343_F10_P951_10_M2.stderr
r_2_r_831_methodArgs_F10_P831_5_M1_F9_P2_1_M1.stderr
r_887_r_86_issue1208_F10_P86_6_M0.stderr
r_910_r_488_issue3671-1_F9_P488_12_M0.stderr
r_1167_r_1155_issue3307_F10_P1155_5_M0.stderr
r_958_issue816-1.stderr
r_712_r_710_default-initializer_F10_P710_4_M0.stderr
r_1051_issue3045-1.stderr
r_944_function_e1.stderr
r_955_r_1078_issue1937_F9_P1078_6_M0.stderr
r_910_r_1010_issue1452-1_F7_P1010_4_M0.stderr
r_882_r_9_issue3307_F10_P9_14_M0.stderr
r_590_factory-err2.stderr
r_700_r_494_generic-struct-tuple_F10_P494_9_M1.stderr
r_868_r_799_verify_disjunction_F1_P799_3_M0.stderr
r_709_r_279_issue2265-1_F10_P279_12_M1.stderr
r_272_issue818.stderr
r_887_r_85_issue1208_F10_P85_3_M0.stderr
r_185_r_180_generic-struct-tuple_F10_P180_1_M0.stderr
r_168_r_1048_issue3324_F6_P1048_7_M0.stderr
r_179_r_9_issue3307_F10_P9_6_M0.stderr
r_264_issue818.stderr
r_179_r_23_issue2795_F10_P23_1_M0.stderr
r_515_r_8_issue3307_F10_P8_3_M2.stderr
r_718_r_716_default-initializer_F10_P716_5_M0.stderr
r_647_r_645_default-initializer_F10_P645_3_M0.stderr
r_252_r_72_issue2795_F10_P72_7_M0_F10_P1056_2_M2.stderr
r_886_r_86_issue1208_F10_P86_1_M0.stderr
r_886_r_8_methodArgs_F10_P8_6_M1.stderr
r_171_r_9_issue3307_F10_P9_6_M0.stderr
r_1073_r_1018_issue3779_F10_P1018_10_M0.stderr
r_723_r_716_default-initializer_F10_P716_13_M0.stderr
r_882_r_8_issue3307_F10_P8_11_M1.stderr
r_1051_issue3045-2.stderr
r_718_r_494_issue2795_F10_P494_4_M0.stderr
r_772_precedence-lt.stderr
r_515_r_8_issue3307_F10_P8_12_M1.stderr
r_257_issue807.stderr
r_171_r_170_newtype1_F10_P170_0_M0.stderr
r_243_r_200_crash-typechecker_F10_P200_12_M0_F10_P203_2_M2.stderr
r_882_r_24_issue2260-2_F10_P24_4_M1.stderr
r_886_r_85_lvalue-parens_F10_P85_2_M0.stderr
r_168_r_23_issue3324_F2_P23_2_M0.stderr
r_712_r_710_default-initializer_F10_P710_4_M1.stderr
r_888_r_888_issue1333_F5_P888_0_M1.stderr
r_945_r_546_issue1830_F10_P546_3_M0.stderr
r_590_issue2441.stderr
r_640_r_640_default-initializer_F10_P640_1_M0.stderr
r_1000_issue3045-1.stderr
r_936_r_9_issue3307_F10_P9_6_M0.stderr
r_1061_issue1331.stderr
r_252_r_904_issue2175-2_F8_P904_8_M1.stderr
r_777_r_619_list2_F10_P619_1_M0.stderr
r_896_r_711_default-initializer_F5_P711_4_M0.stderr
r_250_issue807.stderr
r_886_r_267_issue1208_F10_P267_0_M1.stderr
r_1170_r_1253_issue1386_F3_P1253_3_M0.stderr
r_723_r_716_default-initializer_F10_P716_5_M0.stderr
r_1000_issue3045-2.stderr
r_938_r_252_issue3307_F10_P252_10_M0.stderr
r_1019_r_233_issue3779_F10_P233_0_M0.stderr
r_947_r_242_default-switch_F10_P242_14_M0.stderr
r_715_r_494_issue1863_F10_P494_4_M0.stderr
r_718_r_494_issue2795_F10_P494_8_M1.stderr
r_501_table-entries-ser-enum-bmv2.stderr
r_947_r_861_direct-call2_F8_P861_8_M1.stderr
r_514_r_59_issue3671-1_F10_P59_3_M1.stderr
r_910_r_551_issue3671-1_F7_P551_7_M0.stderr
r_168_r_1269_issue3324_F1_P1269_1_M0.stderr
r_778_r_619_list2_F10_P619_1_M1.stderr
r_171_r_23_issue3324_F10_P23_1_M0.stderr
r_767_r_43_inverted-range_F10_P43_14_M0.stderr
r_1167_r_8_issue3307_F8_P8_10_M0.stderr
r_715_r_495_issue2036-3_F10_P495_8_M1.stderr
r_938_r_170_emptyTuple_F10_P170_4_M0.stderr
r_945_r_1014_issue3779_F10_P1014_2_M1.stderr
r_960_issue819.stderr
r_249_r_73_issue2795_F10_P73_4_M0.stderr
r_249_r_1253_issue933_F10_P1253_6_M0.stderr
r_1051_issue3045.stderr
r_873_r_1143_issue3623-1_F10_P1143_9_M0.stderr
r_1167_r_72_issue2795_F10_P72_7_M0_F8_P158_0_M0.stderr
r_266_issue816-1.stderr
r_936_r_23_issue3324_F10_P23_1_M0.stderr
r_1161_issue818.stderr
r_644_r_642_default-initializer_F10_P642_5_M2.stderr
r_936_r_33_newtype1_F10_P33_2_M2.stderr
r_640_r_640_default-initializer_F10_P640_4_M1.stderr
r_868_r_915_pvs-bitstring-bmv2_F9_P915_5_M0.stderr
r_979_issue1777-bmv2.stderr
r_709_r_494_generic-struct-tuple_F10_P494_3_M0.stderr
