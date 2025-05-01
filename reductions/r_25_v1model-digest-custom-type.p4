match_kind{lpm} type bit a;
header b { a h; }
struct c {
  b d;
} control e(c f) {
  table g {
    key = { f.d.h : lpm;
  }
  actions = {}
}
apply {}
}
