extern a {}
extern c { void d(); }
struct e {
} parser f<g, h>(a b, out g i, inout h j, inout e k);
control l<g, h>(inout g a, inout h j);
control m<g, h>(inout g a, inout h j, inout e k);
control n<g, h>(inout g a, inout h j, inout e k);
control o<g, h>(inout g a, inout h j);
control y<g>(c b, in g a);
package z<g, h>(f<g, h> p, l<g, h> q, m<g, h> b, n<g, h> c, o<g, h> d, y<g> e);
struct r {
} struct s {
} parser t(a f, out r a, inout s j, inout e u) {
  state start {}
}
control v(c f, in r a) {
  apply {}
}
control w(inout r a, inout s j, inout e u) {
  apply {}
}
control x(inout r a, inout s j) {
  apply {}
}
z(t(), x(), w(), w(), x(), v()) a;
