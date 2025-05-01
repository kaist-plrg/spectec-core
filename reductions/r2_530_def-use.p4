control b() {
  action a() {}
  table c {
    actions = { a;
  }
}
apply {
  switch (c.apply().action_run) {
  a:
  }
}
}
