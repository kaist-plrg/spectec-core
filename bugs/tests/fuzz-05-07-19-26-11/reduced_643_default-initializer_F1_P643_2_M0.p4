 struct S {
 }
 enum bit<8> myEnum {
 One = 1, };
 enum myEnum1 {
   e1, }
 control C(out bit<32> x)() {
   apply {     S s7 = ((S) ({ 2, 2, ... }));   }
 }
