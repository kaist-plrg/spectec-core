extern a {}
extern c {}
struct d {
} parser e<f, g>(a b, out f h, inout g i, inout d v);
control j<f, g>(inout f a, inout g i);
control k<f, g>(inout f a, inout g i, inout d v);
control l<f, g>(inout f a, inout g i, inout d v);
control m<f, g>(inout f a, inout g i);
control n<f>(c b, in f a);
package o<f, g>(e<f, g> p, j<f, g> b, k<f, g> c, l<f, g> d, m<f, g> e, n<f> f);
struct w {
} struct x {
} parser q(a b, out w p, inout x i, inout d r) {
  state start {}
}
control s(inout w a, inout x i, inout d r) {
  apply {}
}
control y(inout w b, inout x i, inout d r) {
  apply {}
}
control z(c b, in w p) {
  apply {}
}
control t(inout w b, inout x i) {
  apply {}
}
control u(inout w a, inout x i) {
  apply {}
}
o(q(), t(), s(), y(), u(), z()) c;
