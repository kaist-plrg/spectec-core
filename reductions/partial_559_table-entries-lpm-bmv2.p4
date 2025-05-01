       match_kind {
                        lpm }
                      header a {
                        bit b;
                    }
                      struct Header_t {
                        a c;
                    }
                      control d(Header_t c) {
                        action e(bit f) {
     }
                        table g {
                    key = {
                           c.c.b : lpm;
                       }
                  actions = {
                           e;
                       }
                         entries = {
                           1 &&& 0 : e(1);
                       }
                     }
                        apply {
     }
                    }
