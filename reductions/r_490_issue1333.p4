extern void c(bit a, bit<32> b);
control d(bit<32> binit) {
  apply { c(b = binit); }
}
