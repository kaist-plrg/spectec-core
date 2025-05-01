extern a { void c(); }
extern d {}
struct e {
} parser f<g, h>(a b, out g i, inout h j, inout e k);
control l<g, h>(inout g a, inout h j);
control m<g, h>(inout g a, inout h j, inout e k);
control n<g, h>(inout g a, inout h j, inout e k);
control o<g, h>(inout g a, inout h j);
control z<g>(d b, in g a);
package aa<g, h>(f<g, h> p, l<g, h> q, m<g, h> c, n<g, h> d, o<g, h> e, z<g> f);
struct r {
} struct s {
} parser t(a b, out r a, inout s j, inout e c) {
  state start {}
}
control u(inout r a, inout s j) {
  apply {}
}
control v(inout r a, inout s j, inout e c) {
  apply {}
}
control w(inout r a, inout s j, inout e c) {
  apply {}
}
control x(inout r a, inout s j) {
  apply {}
}
control y(d b, in r a) {
  apply {}
}
aa(t(), u(), v(), w(), x(), y()) d;
