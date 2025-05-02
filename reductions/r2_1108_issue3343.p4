parser a() {
  value_set<bit<6>>(4) b;
  state start {
    transition select(6w1) {
    b:
      accept;
    }
  }
}
