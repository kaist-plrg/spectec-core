extern a {}
extern b {}
struct c {
} struct d {
} struct e {
} struct f {
} struct g {
} control h<i, j>(in i k, inout j l, in c a, inout d m);
parser n<o, p>(a b, out o c, inout p q, in e a);
control r<o, p>(inout o c, inout p q, in f a, inout g m);
control s<o, p>(b b, in o c, in p q, in g m);
package t<i, j, o, p>(n<o, p> u, h<i, j> v, r<o, p> w, s<o, p> x);
struct y {
} struct z {
} parser aa(a b, out y e, inout z f, in e a) {
  state start {}
}
control ab(inout y e, inout z f, in f a, inout g m) {
  apply {}
}
control ac(b d, in y e, in z f, in g m) {
  apply {}
}
control ad(in y e, inout z f, in c a, inout d m) {
  apply {}
}
t(aa(), ad(), ab(), ac()) f;
