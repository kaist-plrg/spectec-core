extern b {
  b();
  abstract void a(inout bit<32> arg);
}
control c() { b() d = {void a(inout bit<32> arg){; }
}
;
apply {}
}
