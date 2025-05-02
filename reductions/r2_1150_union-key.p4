match_kind{exact} header a { bit b; }
header_union d { a e; }
struct i {
  d f;
} control c(i g) {
  table h {
    key = { g.f.e.b : exact;
  }
  actions = {}
}
apply {}
}
