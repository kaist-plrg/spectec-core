enum bit a { b = 7 } header d { a e; }
struct f {
  d eth;
} control c(inout f g) {
  apply { g.eth.e = (a)0; }
}
