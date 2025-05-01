extern a {}
extern b {}
enum c { d } struct e {
} struct f {
} struct g {
} struct h {
} struct i {
} control j<k, l>(in k m, inout l n, in e a, inout f o);
parser p<aa, q>(a c, out aa r, inout q s, in g a);
control t<aa, q>(inout aa r, inout q s, in h a, inout i o);
control u<aa, q>(b c, in aa r, in q s, in i o);
package v<k, l, aa, q>(p<aa, q> w, j<k, l> x, t<aa, q> y, u<aa, q> z);
struct ab {
} struct ac {
} control ad(in e f, inout c e, in e a, inout f o) {
  apply {}
}
parser ae(a c, out ac af, inout ab ag, in g a) {
  state start {}
}
control ah(inout ac af, inout ab ai, in h a, inout i o) {
  apply {}
}
control aj(b c, in ac af, in ab a, in i o) {
  apply {}
}
v(ae(), ad(), ah(), aj()) a;
