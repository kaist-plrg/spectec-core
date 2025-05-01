extern bool a<b>(in b c);
control d() {
  action e() { a(c = {}); }
  apply {}
}
