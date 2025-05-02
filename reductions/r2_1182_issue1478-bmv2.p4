action a() {}
match_kind { exact }
struct b {
  bit c;
} control d(b e) {
  table f {
    key = { e.c : exact;
  }
  actions = { a;
}
entries = { 0 : a;
}
}
apply {}
}
