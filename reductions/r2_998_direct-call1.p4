parser a() {
  state start {}
}
parser b() {
  state start { a.apply(); }
}
