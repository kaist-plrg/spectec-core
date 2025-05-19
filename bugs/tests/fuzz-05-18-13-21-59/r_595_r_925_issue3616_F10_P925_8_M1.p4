# 1 "fuzz/fuzz0/illtyped/r_925_issue3616_F10_P925_8_M1.p4"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "fuzz/fuzz0/illtyped/r_925_issue3616_F10_P925_8_M1.p4"
# 12 "fuzz/fuzz0/illtyped/r_925_issue3616_F10_P925_8_M1.p4"
enum bit<4> e {
a = 0,
b = 0,
c = ((a) (a)),
d = a
};
