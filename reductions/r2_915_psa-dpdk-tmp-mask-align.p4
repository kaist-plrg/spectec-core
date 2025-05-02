extern a {}
extern b {}
struct c {
} struct d {
} struct e {
} parser f<g, h, i, j>(a a, out g k, inout h l, in c b, in i m, in j n);
control o<g, h>(inout g c, inout h l, in d b, inout e d);
control y<g, h, p, i, z>(b a, out p q, out i m, out z r, inout g c, in h e,
                         in e b);
package s<t, u, z, p, i, j>(f<t, u, i, j> f, o<t, u> a, y<t, u, p, i, z> b);
struct v {
} struct aa {
} struct ab {
} parser w(a a, out ab c, inout aa l, in c b, in v m, in v n) {
  state start {}
}
control ac(inout ab c, inout aa l, in d b, inout e d) {
  apply {}
}
control x(b a, out v q, out v m, out v r, inout ab c, in aa e, in e b) {
  apply {}
}
s(w(), ac(), x()) f;
