match_kind { ternary }
enum bit a { b = 0, g = 1 } control c() {
  action h(a d) {}
  bit e;
  table f {
    key = { e : ternary;
  }
  actions = { h;
}
entries = { 0 : h(a.b);
0 : h(g);
}
}
apply {}
}
