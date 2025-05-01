extern a { void b<c>(out c d); }
enum bit e { f = 0 } header g { e h; }
struct l {
  g i;
} parser j(a k, out l hdrs) {
  state start { k.b(hdrs); }
}
