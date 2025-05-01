control a(out bit<16> b) {
  apply {
    tuple<bit, bit<16>> c;
    b = c[1];
  }
}
