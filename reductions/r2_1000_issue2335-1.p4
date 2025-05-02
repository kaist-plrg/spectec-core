control b(inout bit< 8 > c)(int a) {   apply {} }
 control e(inout bit< 8 > a) {   b(0) d;   apply { d.apply(a); } }
