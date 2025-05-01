match_kind { lpm }
typedef bit a;
type a b;
header c { b d; }
struct e {
  c f;
} control g(e h) {
  table i {
    key = { h.f.d : lpm;
  }
  actions = {}
}
apply {}
}
