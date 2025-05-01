extern a {}
extern b {}
struct c {
} struct d {
} struct e {
} parser f<g, h, i, j>(a aa, out g k, inout h l, in c a, in i m, in j n);
control o<g, h>(inout g b, inout h l, in d a, inout e c);
control ab<g, h, p, i, ac>(b aa, out p q, out i m, out ac r, inout g b, in h f,
                           in e a);
package s<t, u, ac, p, i, j>(f<t, u, i, j> d, o<t, u> e, ab<t, u, p, i, ac> f);
struct v {
} struct w {
} struct x {
} parser y(a d, out x b, inout w l, in c a, in v m, in v n) {
  state start {}
}
control z(inout x b, inout w l, in d a, inout e c) {
  apply {}
}
control ad(b aa, out v q, out v m, out v r, inout x b, in w f, in e a) {
  apply {}
}
s(y(), z(), ad()) d;
