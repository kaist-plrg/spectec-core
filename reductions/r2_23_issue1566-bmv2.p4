extern a { void c<d>(d e); }
extern f {}
struct g {
} parser h<i, j>(a b, out i k, inout j l, inout g m);
control n<i, j>(inout i e, inout j l);
control o<i, j>(inout i e, inout j l, inout g m);
control z<i, j>(inout i e, inout j l, inout g m);
control aa<i, j>(inout i e, inout j l);
control q<i>(f b, in i e);
package r<i, j>(h<i, j> p, n<i, j> s, o<i, j> b, z<i, j> c, aa<i, j> d, q<i> e);
struct t {
} struct u {
} parser v(a f, out t e, inout u l, inout g b) {
  state start {}
}
control w(inout t e, inout u l, inout g b) {
  apply {}
}
control x(inout t e, inout u l, inout g b) {
  apply {}
}
control y(f c, in t e) {
  apply {}
}
control ab(inout t e, inout u l) {
  apply {}
}
control ac(inout t e, inout u l) {
  apply {}
}
r(v(), ab(), w(), x(), ac(), y()) f;
