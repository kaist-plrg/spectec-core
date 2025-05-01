 match_kind {                          lpm }
                         header a {                          bit b;                      }
                         struct h {                          a c;                      }
                         control d(h c) {                          action e(bit f) {      }                          table g {                      key = {                            c.c.b : lpm;                        }                    actions = {                            e;                        }                           entries = {                            1 &&& 0 : e(1);                        }                       }                          apply {      }                      }
