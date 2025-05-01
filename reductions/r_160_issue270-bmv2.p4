extern a { bit c(); }
extern d {}
struct e {
} parser f<g, h>(a b, out g i, inout h j, inout e k);
control l<g, h>(inout g a, inout h j);
control m<g, h>(inout g a, inout h j, inout e k);
control n<g, h>(inout g a, inout h j, inout e k);
control o<g, h>(inout g a, inout h j);
control y<g>(d b, in g a);
package z<g, h>(f<g, h> p, l<g, h> q, m<g, h> b, n<g, h> c, o<g, h> d, y<g> e);
struct g {
} struct h {
} parser r(a b, out g p, inout h j, inout e s) {
  state start {}
}
control t(inout g f, inout h j, inout e s) {
  apply {}
}
control u(inout g a, inout h j, inout e s) {
  apply {}
}
control v(inout g a, inout h j) {
  apply {}
}
control w(inout g a, inout h j) {
  apply {}
}
control x(d b, in g p) {
  apply {}
}
z(r(), v(), t(), u(), w(), x()) b;
