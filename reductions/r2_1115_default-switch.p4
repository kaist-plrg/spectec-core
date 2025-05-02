control a() {
  action b() {}
  table c {
    actions = { b;
  }
}
apply {
  switch (c.apply().action_run) {
  b:
  }
}
}
