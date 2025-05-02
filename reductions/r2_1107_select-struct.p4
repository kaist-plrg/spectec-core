parser a() {
  state start {
    bit b;
    transition select(b, b, {b, b}, b) { (0, 0, {0, 0}, 0) : accept; }
  }
}
