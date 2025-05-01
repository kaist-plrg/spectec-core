control a() {
  bit<32> c;
  action b(bit<32> arg) {}
  apply { b(c); }
}
