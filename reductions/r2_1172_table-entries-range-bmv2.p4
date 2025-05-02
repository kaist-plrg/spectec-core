match_kind{range} header a { bit b; }
struct h {
  a c;
} control d(h c) {
  action e(bit f) {}
  table g {
    key = { c.c.b : range;
  }
  actions = { e;
}
entries = { 1..8 : e(1);
}
}
apply {}
}
