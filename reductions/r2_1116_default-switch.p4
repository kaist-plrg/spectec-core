control a() {
  table b {
    actions = {}
  }
  apply {
    switch (b.apply().action_run) { default: }
  }
}
