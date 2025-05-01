extern g {}
extern h {}
struct i {
} struct j {
} struct k {
} struct l {
} struct m {
} struct n {
} struct o {
} extern p {
  p();
}
extern q { q(); }
parser r<s, t, u, v>(g a, out s w, inout t x, in i b, in u y, in v z);
control ab<s, t>(inout s d, inout t x, in k b, inout l e);
control ac<s, t, ae, u, af>(h a, out ae ag, out u y, out af ah, inout s d,
                            in t ai, in l b);
parser aj<s, t, af, ae, ak>(g a, out s w, inout t x, in j b, in af ah, in ae ag,
                            in ak al);
control am<s, t>(inout s d, inout t x, in m b, inout o e);
control an<s, t, ak, v>(h a, out ak al, out v z, inout s d, in t ai, in o b,
                        in n ao);
package ap<aq, ar, af, ae, u, v>(r<aq, ar, u, v> a, ab<aq, ar> as,
                                 ac<aq, ar, ae, u, af> at);
package av<aw, ax, af, ae, ak, v>(aj<aw, ax, af, ae, ak> a, am<aw, ax> b,
                                  an<aw, ax, ak, v> d);
package ay<aq, ar, aw, ax, af, ae, ak, u, v>(ap<aq, ar, af, ae, u, v> d, p e,
                                             av<aw, ax, af, ae, ak, v> f, q az);
struct bb {
} struct bc {
} struct bd {
} parser be(g a, out bd ad, inout bc b, in i c, in bb d, in bb e) {
  state start {}
}
parser bf(g aa, out bb a, inout bb b, in j c, in bb d, in bb e, in bb f) {
  state start {}
}
control bg(inout bd ad, inout bc b, in k c, inout l d) {
  apply {}
}
control bh(inout bb a, inout bb b, in m c, inout o d) {
  apply {}
}
control bi(h aa, out bb a, out bb b, out bb c, inout bd d, in bc e, in l f) {
  apply {}
}
control bj(h aa, out bb a, out bb b, inout bb c, in bb d, in o e, in n f) {
  apply {}
}
ap(be(), bg(), bi()) au;
av(bf(), bh(), bj()) ba;
ay(au, p(), ba, q()) bk;
