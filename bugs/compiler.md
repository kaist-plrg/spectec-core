# fuzz-05-07-19-26-11

##### 1. [BUG] reduced_662_table-key-serenum-bmv2_F1_P43_2_M0.stderr

```p4
enum bit<16> EthTypes {
    b = ((bool) ({ , ... })),
}
```
Raises `Compiler Bug: /usr/src/packages/BUILD/frontends/p4/typeChecking/typeChecker.cpp:214: Null srcType`
Explicit casting in Serializable Enum fields cause cras

##### 2. [DUPLICATE] reduced_599_v1model-digest-containing-ser-enum_F1_P43_1_M0.stderr

```p4
enum bit<8> MySerEnum1 {
  foo = 28,
  bar = ((string) (true)),
  gah = 42
};
```
Same as: `reduced_662_table-key-serenum-bmv2_F1_P43_2_M0.stderr`, but here casting to string is not allowed in the first place.

##### 3. [DUPLICATE] reduced_667_v1model-digest-containing-ser-enum_F1_P43_1_M0.stderr

Same as: `reduced_599_v1model-digest-containing-ser-enum_F1_P43_1_M0.stderr`

# fuzz-05-18-13-21-59

##### 1. [BUG] r_180_r_180_generic-struct-tuple_F10_P180_2_M0.stderr

```p4
struct S<T> {
  tuple<T, tuple<match_kind, int>> t;
}
```

```
[--Werror=type-error] error: Field 'f1' of 'struct tuple_0' cannot have type 'int'
  tuple<T, tuple<match_kind, int>> t;
                             ^^^
[--Werror=type-error] error: Error while analyzing struct tuple_0
In file: /p4c/lib/crash.cpp:299
Compiler Bug: Exiting with SIGSEGV

```

##### 2. [Bug] r_700_r_494_generic-struct-tuple_F10_P494_9_M1.stderr

```p4
struct S<T> {
  tuple<T, T> t;
}
const S<bit<32>> x = { a = ... };
```

```
In file: /p4c/frontends/p4/typeChecking/typeUnification.cpp:414
Compiler Bug: /p4c/frontends/p4/typeChecking/typeUnification.cpp:414: Null dotsField
```

##### 3. [Bug] r_2_r_1022_methodArgs_F9_P1022_6_M0.stderr

```p4
extern Random<T> {
  Random(T min);
  T read();
}
control c()() {
  Random({#}) r2;
  apply {
    bit<16> v = r2.read();
  }
}
```

```
In file: /p4c/frontends/p4/typeChecking/typeChecker.cpp:547
Compiler Bug: Unknown destination type
```

[Compiler Bug] r_2_r_1098_hashext2_F9_P1098_14_M1.stderr
Compiler Bug: Unknown destination type

[Compiler Bug] r_2_r_831_methodArgs_F10_P831_5_M1.stderr
Compiler Bug: Unknown destination type

[Compiler Bug] r_2_r_831_methodArgs_F10_P831_5_M1_F9_P2_1_M1.stderr
Compiler Bug: Unknown destination type

[Compiler Bug] r_2_r_1023_methodArgs_F9_P1023_11_M0.stderr
Compiler Bug: Unknown destination type

[Compiler Bug] r_600_r_41_issue3623-1_F10_P41_10_M1.stderr
Compiler Bug: /p4c/frontends/p4/typeChecking/typeChecker.cpp:172: Null srcType

[Compiler Bug] r_45_r_778_issue3616_F10_P778_5_M1.stderr
Compiler Bug: /p4c/frontends/p4/typeChecking/typeChecker.cpp:172: Null srcType

[Compiler Bug] r_1077_r_43_issue3635_F10_P43_3_M1.stderr
Compiler Bug: /p4c/frontends/p4/typeChecking/typeChecker.cpp:172: Null srcType

[Compiler Bug] r_616_r_177_issue3616_F10_P177_6_M1.stderr
Compiler Bug: /p4c/frontends/p4/typeChecking/typeChecker.cpp:172: Null srcType

[Compiler Bug] r_678_r_175_issue3616_F10_P175_6_M0.stderr
Compiler Bug: /p4c/frontends/p4/typeChecking/typeChecker.cpp:172: Null srcType

[Compiler Bug] r_616_r_175_issue3616_F10_P175_0_M0.stderr
Compiler Bug: /p4c/frontends/p4/typeChecking/typeChecker.cpp:172: Null srcType

[Compiler Bug] r_616_r_175_issue3616_F10_P175_0_M1.stderr
Compiler Bug: /p4c/frontends/p4/typeChecking/typeChecker.cpp:172: Null srcType

[Compiler Bug] r_635_r_18_issue3635_F10_P18_9_M0.stderr
Compiler Bug: /p4c/frontends/p4/typeChecking/typeChecker.cpp:172: Null srcType

[Compiler Bug] r_616_r_175_issue3616_F10_P175_2_M0.stderr
Compiler Bug: /p4c/frontends/p4/typeChecking/typeChecker.cpp:172: Null srcType

[Compiler Bug] r_600_r_42_issue3623-1_F10_P42_6_M0.stderr
Compiler Bug: /p4c/frontends/p4/typeChecking/typeChecker.cpp:172: Null srcType

[Compiler Bug] r_592_r_925_issue3616_F10_P925_8_M1.stderr
Compiler Bug: /p4c/frontends/p4/typeChecking/typeChecker.cpp:172: Null srcType

[Compiler Bug] r_762_r_42_inverted-range_F10_P42_14_M2.stderr
Compiler Bug: /p4c/frontends/p4/typeChecking/typeChecker.cpp:172: Null srcType

[Compiler Bug] r_764_r_175_issue3364_F10_P175_8_M0.stderr
Compiler Bug: /p4c/frontends/p4/typeChecking/typeChecker.cpp:172: Null srcType

[Compiler Bug] r_45_r_177_issue3616_F10_P177_7_M0.stderr
Compiler Bug: /p4c/frontends/p4/typeChecking/typeChecker.cpp:172: Null srcType

[Compiler Bug] r_750_r_44_issue3056_F10_P44_8_M1.stderr
Compiler Bug: /p4c/frontends/p4/typeChecking/typeChecker.cpp:172: Null srcType

[Compiler Bug] r_822_r_175_issue3288_F10_P175_5_M0.stderr
Compiler Bug: /p4c/frontends/p4/typeChecking/typeChecker.cpp:172: Null srcType

[Compiler Bug] r_759_r_20_issue3635_F10_P20_10_M0.stderr
Compiler Bug: /p4c/frontends/p4/typeChecking/typeChecker.cpp:172: Null srcType

[Compiler Bug] r_649_r_20_issue3056_F10_P20_9_M1.stderr
Compiler Bug: /p4c/frontends/p4/typeChecking/typeChecker.cpp:172: Null srcType

[Compiler Bug] r_668_r_41_issue3623-1_F10_P41_10_M1.stderr
Compiler Bug: /p4c/frontends/p4/typeChecking/typeChecker.cpp:172: Null srcType

[Compiler Bug] r_790_r_43_inverted-range_F10_P43_6_M1.stderr
Compiler Bug: /p4c/frontends/p4/typeChecking/typeChecker.cpp:172: Null srcType

[Compiler Bug] r_822_r_20_issue3635_F10_P20_10_M0.stderr
Compiler Bug: /p4c/frontends/p4/typeChecking/typeChecker.cpp:172: Null srcType

[Compiler Bug] r_595_r_925_issue3616_F10_P925_8_M1.stderr
Compiler Bug: /p4c/frontends/p4/typeChecking/typeChecker.cpp:172: Null srcType

[Compiler Bug] r_744_r_175_issue3288_F10_P175_10_M1.stderr
Compiler Bug: /p4c/frontends/p4/typeChecking/typeChecker.cpp:172: Null srcType

[Compiler Bug] r_668_r_42_issue3623-1_F10_P42_6_M0.stderr
Compiler Bug: /p4c/frontends/p4/typeChecking/typeChecker.cpp:172: Null srcType

[Compiler Bug] r_744_r_44_inverted-range_F10_P44_2_M2.stderr
Compiler Bug: /p4c/frontends/p4/typeChecking/typeChecker.cpp:172: Null srcType
