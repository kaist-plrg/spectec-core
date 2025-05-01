extern g {}
extern i {}
struct j {
} struct k {
} struct l {
} struct n {
} struct o {
} struct q {
} struct r {
} extern s {
  s();
}
extern t { t(); }
parser u<v, w, y, z>(g a, out v b, inout w c, in j d, in y e, in z f);
control ab<v, w>(inout v ac, inout w c, in l d, inout n ad);
control ae<v, w, af, y, ag>(i a, out af ah, out y e, out ag ai, inout v c,
                            in w aj, in n d);
parser ak<v, w, ag, af, al>(g a, out v b, inout w c, in k d, in ag ai, in af ah,
                            in al am);
control an<v, w>(inout v ac, inout w c, in o d, inout r ad);
control ao<v, w, al, z>(i a, out al am, out z f, inout v c, in w aj, in r d,
                        in q ap);
package aq<ar, as, ag, af, y, z>(u<ar, as, y, z> at, ab<ar, as> au,
                                 ae<ar, as, af, y, ag> a);
package av<aw, ax, ag, af, al, z>(ak<aw, ax, ag, af, al> e, an<aw, ax> f,
                                  ao<aw, ax, al, z> d);
package ay<ar, as, aw, ax, ag, af, al, y, z>(aq<ar, as, ag, af, y, z> az, s a,
                                             av<aw, ax, ag, af, al, z> b, t c);
header ba {}
struct bb {
} struct bc {
} struct bd {
} struct bf {
} struct bg {
} struct bh {
} parser p(g b, out bg h, inout bh m, in j x, in bb bi, in bf bj) {
  state start {}
}
parser bk(g aa, out ba a, inout bh b, in k c, in bd d, in bc e, in bc f) {
  state start {}
}
control az(inout bg h, inout bh m, in l d, inout n ad) {
  apply {}
}
control bl(inout ba h, inout bh aj, in o x, inout r d) {
  apply {}
}
control bm(i b, out bc ah, out bb e, out bd ai, inout bg h, in bh bn, in n d) {
  apply {}
}
control bo(i aa, out bc a, out bf b, inout ba c, in bh d, in r e, in q f) {
  apply {}
}
aq(p(), az(), bm()) at;
av(bk(), bl(), bo()) be;
ay(at, s(), be, t()) bp;
