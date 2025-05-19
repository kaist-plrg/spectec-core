# 1 "fuzz/fuzz1/illtyped/r_18_issue3635_F10_P18_9_M0.p4"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "fuzz/fuzz1/illtyped/r_18_issue3635_F10_P18_9_M0.p4"
# 12 "fuzz/fuzz1/illtyped/r_18_issue3635_F10_P18_9_M0.p4"
enum bit<4> e {
a = ((bool) ({ })),
b = 2,
c = 3
};
void f() {
  bit<8> good1;
  good1 = ((e.a) ++ (e.b));
}
