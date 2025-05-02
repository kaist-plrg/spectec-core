match_kind { ternary }
enum bit a { b = 1 } control c() {
  action g(a d) {}
  bit e;
  table f {
    key = { e : ternary;
  }
  actions = { g;
}
entries = { 0 : g(b);
}
}
apply {}
}
