extern a {}
extern b {}
struct c {
} struct e {
} struct f {
} struct g {
} struct h {
} struct i {
} struct j {
} extern k {
  k();
}
extern l { l(); }
parser m<n, o, p, aa>(a b, out n q, inout o r, in c c, in p s, in aa t);
control u<n, o>(inout n d, inout o r, in f c, inout g e);
control v<n, o, w, p, x>(b b, out w y, out p s, out x z, inout n d, in o f,
                         in g c);
parser ab<n, o, x, w, ac>(a b, out n q, inout o r, in e c, in x z, in w y,
                          in ac ad);
control ae<n, o>(inout n d, inout o r, in h c, inout j e);
control af<n, o, ac, aa>(b b, out ac ad, out aa t, inout n d, in o f, in j c,
                         in i a);
package ag<ah, ai, x, w, p, aa>(m<ah, ai, p, aa> a, u<ah, ai> aj,
                                v<ah, ai, w, p, x> ak);
package am<an, ao, x, w, ac, aa>(ab<an, ao, x, w, ac> ap, ae<an, ao> aq,
                                 af<an, ao, ac, aa> d);
package ar<ah, ai, an, ao, x, w, ac, p, aa>(ag<ah, ai, x, w, p, aa> as, k at,
                                            am<an, ao, x, w, ac, aa> a, l b);
struct au {
} struct av {
} struct aw {
} parser ax(a b, out aw q, inout av r, in c c, in au s, in au t) {
  state start {}
}
control as(inout aw d, inout av r, in f c, inout g e) {
  apply {}
}
parser ay(a b, out aw q, inout av r, in e c, in au z, in au y, in au e) {
  state start {}
}
control az(inout aw d, inout av r, in h c, inout j e) {
  apply {}
}
control ba(b b, out au y, out au s, out au z, inout aw d, in av f, in g c) {
  apply {}
}
control bb(b b, out au e, out au t, inout aw d, in av f, in j c, in i a) {
  apply {}
}
ag(ax(), as(), ba()) al;
am(ay(), az(), bb()) ap;
ar(al, k(), ap, l()) e;
