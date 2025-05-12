1. [ERROR] reduced_250_issue807.stderr

```p4
control C1();
extern E {
  void set1(C1 c1);
}
```

A control object cannot be a run-time parameter to an extern.

> Appendix F
> Externs are not allowed to call parsers or controls, so there is no use in passing objects of those types to them.

2. [NOERROR] reduced_49_list8_F1_P178_1_M0.stderr

```p4
E(((list<_>) ({ ((list<bit<32>>) ({ 10, 6, 3 })), ((list<bit<32>>) ({ 5, 2 })) }))) e;
```

P4-SpecTec is deficient with don't care types.

3. [ERROR] reduced_520_issue1897-bmv2_F1_P269_0_M1.stderr

```p4
enum MeterType { packetsbytes };
extern direct_meter< T > {
  direct_meter(MeterType type);
  direct_meter(MeterType type);
}
```

Fails to reject a duplicate declaration of a constructor.

4. [ERROR] reduced_846_issue4661_pure_extern_function_const_args_F1_P846_2_M0.stderr

```p4
switch (baz()) {
    1: {}
    1: {}
}
```

> 12.7.3. Notes common to all switch statements
> It is a compile-time error if two labels of a switch statement equal each other.

5. [ERROR] reduced_1019_issue3045.stderr

```p4
extern register< T > { void write(in bit< 32 > index, in T value); }
void f< T >(register< T > r, in T t) { r.write(0, t); }
```

An extern cannot be called at run time from a function.

> Appendix F

6. [ERROR] reduced_928_issue816-1.stderr

```p4
package P(C c1, C c2);
control MyC1()(P p) {
    apply {}
}
```

A package cannot be a constructor parameter for a control type.

> Appendix F

7. [ERROR] reduced_171_issue1352-bmv2_F1_P18_0_M0.stderr

```p4
typedef match_kind ip4Addr_t;
```

It is an error to alias `match_kind` with a typedef.

> 7.2.8. Type nesting rules

8. [ERROR] reduced_1224_issue1230.stderr

```p4
table t {
    actions = { NoAction; }
    size = true;
}
```

> 14.2.1.5. Size
> The size is an optional property of a table. When present, its value must always be a compile-time known value that is an integer. The size property is specified in units of number of table entries.

9. [ERROR] reduced_914_function_e1.stderr

```p4
bit<16> max(in bit<16> left, in bit<16> right) {}
```

> 10. Function declarations
> A function with a non-void return type must return a value of the suitable type on all possible execution paths.

10. [DUPLICATE] reduced_1144_issue807.stderr

Same as `reduced_250_issue807.stderr`.

11. [ERROR] reduced_854_issue1025-bmv2_F1_P6_1_M0.stderr

```p4
control VerifyChecksum<H, M>(error a = {#}, inout M meta);
```

Invalid expression `{#}` does not have type `error`.

12. [FUZZY] copy_717_reduced_991_issue2795_F7_P991_2_M0.stderr

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

13. [FUZZY] copy_646_reduced_639_default-initializer_F10_P639_1_M1.stderr

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

14. [FUZZY] copy_1185_reduced_1185_issue3550_F1_P1180_2_M1_F8_P1185_2_M0.stderr

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

15. [ERROR] reduced_995_issue1331.stderr

```p4
parser P(packet_in pkt);
control MyC(packet_in pkt, inout ethernet ether)(P p) {
    apply { p.apply(pkt); }
}
```

A parser object cannot be called at run time from a control apply method.

> Appendix F

16. [FUZZY] reduced_711_default-initializer_F1_P711_0_M2.stderr

```p4
header_union HU {}
control C(out bit<32> x)() {
    HU[0] hustack;
    apply { hustack = { ... }; }
}
```

Do we allow `...` acting as nothing?

17. [NOERROR] reduced_930_issue3808-1.stderr

```p4
extern f<T> {}
control eq0(f<_> tt) {
    apply {}
}
```

Due to P4-SpecTec's deficiency with don't care types.
But questionable: this does open up the possibility of making controls generic.

18. [DUPLICATE] reduced_971_issue1331.stderr

Same as `reduced_995_issue1331.stderr`.

19. [ERROR] reduced_178_list9_F1_P178_1_M0.stderr

```p4
struct S<T> { T t; }
extern E {
    E(list<S<match_kind>> data);
}
```

`match_kind` cannot be nested inside a struct.

> 7.2.8. Type nesting rules

20. [DUPLICATE] reduced_283_issue816-1.stderr

Same as: `reduced_928_issue816-1.stderr`.

21. [DUPLICATE] reduced_989_issue3045.stderr

Same as: `reduced_928_issue816-1.stderr`.

22. [Error] reduced_1136_issue818.stderr

```p4
control C();
extern WrapControl {
    WrapControl(C c);
}
```

A control object cannot be a constructor parameter to an extern.

> Appendix F

23. [NOERROR] reduced_1244_methodArgDontcare.stderr

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

24. [DUPLICATE] reduced_906_issue1352-bmv2_F1_P18_0_M0.stderr

Same as: `reduced_171_issue1352-bmv2_F1_P18_0_M0.stderr`.

25. [DUPLICATE] reduced_272_issue818.stderr

Same as: `reduced_1136_issue818.stderr`.

26. [NOERROR] reduced_1097_precedence-lt.stderr

Due to P4-SpecTec parser bug.

27. copy_860_reduced_860_issue1333_F8_P860_0_M0.stderr

```p4
extern E {
  E(tuple<match_kind> x = 0);
  void f(in bit<16> z = 2);
}
```

`0` is not of type `tuple<match_kind>`.
Similar to `reduced_854_issue1025-bmv2_F1_P6_1_M0.stderr`, but this deserves to be a separate test case.

28. [DUPLICATE] copy_639_reduced_639_default-initializer_F10_P639_1_M1.stderr

Same as: `copy_646_reduced_639_default-initializer_F10_P639_1_M1.stderr`.

29. [DUPLICATE] copy_714_reduced_718_default-initializer_F8_P718_1_M1.stderr

Same as: `copy_646_reduced_639_default-initializer_F10_P639_1_M1.stderr`.

30. [ERROR] reduced_643_default-initializer_F1_P643_2_M0.stderr

```p4
struct S {}
control C(out bit<32> x)() {
  apply {    
    S s7 = ((S) ({ 2, 2, ... }));   
  }
}
```

It is clearly an error to provide more values in the initializer.

31. [DUPLICATE] reduced_968_issue3045.stderr

Same as: `reduced_1019_issue3045.stderr`.

32. [FUZZY] copy_1186_reduced_1185_issue3550_F1_P1180_2_M1_F10_P1185_2_M0.stderr

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

33. [DUPLICATE] reduced_738_checksum1-bmv2_F1_P504_2_M0.stderr

Similar to: `reduced_854_issue1025-bmv2_F1_P6_1_M0.stderr`.

34. [DUPLICATE] reduced_278_issue816-1.stderr

Same as: `reduced_928_issue816-1.stderr`.

35. [DUPLICATE] reduced_266_issue816-1.stderr

Same as: `reduced_928_issue816-1.stderr`.

36. [DUPLICATE] reduced_264_issue818.stderr

Same as: `reduced_1136_issue818.stderr`.

37. [FUZZY] reduced_1145_gauntlet_set_valid_in_function-bmv2_F1_P9_1_M2.stderr

```p4
extern packet_out {
   list<_> emit<T>(in T hdr);
 }
```

P4-SpecTec is deficient with don't care types.
But seems odd to allow `_` in the return type of a function.

38. [DUPLICATE] reduced_515_v1model-digest-custom-type_F1_P19_2_M1.stderr

Simliar to: `reduced_520_issue1897-bmv2_F1_P269_0_M1.stderr`.

39. [ERROR] reduced_858_annotation-bug_F1_P28_1_M0.stderr

```p4
control C();
package top(C ck = true);
```

Similar to other default parameter bugs.

40. [DUPLICATE] reduced_739_issue1025-bmv2_F1_P6_1_M0.stderr

Same as: `reduced_854_issue1025-bmv2_F1_P6_1_M0.stderr`.
