extern a {}
extern c {}
struct d {
} parser e<f, g>(a b, out f i, inout g j, inout d k);
control l<f, g>(inout f a, inout g j);
control n<f, g>(inout f a, inout g j, inout d k);
control o<f, g>(inout f a, inout g j, inout d k);
control aa<f, g>(inout f a, inout g j);
control ab<f>(c b, in f a);
package q<f, g>(e<f, g> p, l<f, g> r, n<f, g> d, o<f, g> e, aa<f, g> f,
                ab<f> c);
typedef d t;
struct f {
} struct g {
} parser u(a b, out f h, inout g m, inout t s) {
  state start {}
}
control v(inout f a, inout g j, inout t w) {
  apply {}
}
control x(inout f a, inout g j, inout t w) {
  apply {}
}
control y(inout f a, inout g j) {
  apply {}
}
control z(inout f a, inout g j) {
  apply {}
}
control ac(c b, in f a) {
  apply {}
}
q(u(), y(), v(), x(), z(), ac()) e;
