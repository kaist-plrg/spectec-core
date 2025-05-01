extern a {}
extern c {}
struct d {
} parser e<f, g>(a b, out f h, inout g i, inout d u);
control j<f, g>(inout f a, inout g i);
control k<f, g>(inout f a, inout g i, inout d u);
control l<f, g>(inout f a, inout g i, inout d u);
control m<f, g>(inout f a, inout g i);
control n<f>(c b, in f a);
package o<f, g>(e<f, g> p, j<f, g> v, k<f, g> b, l<f, g> c, m<f, g> d, n<f> e);
struct w {
} struct x {
} parser q(a b, out w f, inout x i, inout d u) {
  state start {}
}
control r(inout w a, inout x i) {
  apply {}
}
control s(inout w f, inout x i, inout d u) {
  apply {}
}
control y(inout w a, inout x i, inout d u) {
  apply {}
}
control z(inout w a, inout x i) {
  apply {}
}
control t(c b, in w a) {
  apply {}
}
o(q(), r(), s(), y(), z(), t()) a;
