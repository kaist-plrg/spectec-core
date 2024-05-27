control Callee() {
  action drop() {}
  table t {
    actions = { drop; }
    default_action = drop;
  }
  apply { t.apply(); }
}

control Caller() {
  Callee() c1;
  Callee() c2;
  apply {
    c1.apply();
    c2.apply();
  }
}

parser P() {
    state start {
        transition accept;
    }
}

parser Q() {
    P() p;
    state start {
        p.apply();
        transition accept;
    }
}

parser Foo();
control Simple();
package Top(Foo f, Simple s);

Top(Q(), Caller()) main;
