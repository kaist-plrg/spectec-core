struct a {
  bit b;
  bit h;
} extern bit d();
control c() {
  action e(in a f) {}
  table g {
    actions = { e((a){d(), d()});
  }
  default_action = e((a){d(), d()});
}
apply {}
}
