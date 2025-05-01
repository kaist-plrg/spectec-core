extern a<b> { a(b c); }
extern d<e> { d(a<_> f); }
control g() {
  d<bit>(a(6)) h;
  apply {}
}
