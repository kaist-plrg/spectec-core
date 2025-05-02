header a {}
struct b {
  a[4] c;
} control ctrl(b d) {
  action act() { d.c.pop_front(1); }
  apply {}
}
