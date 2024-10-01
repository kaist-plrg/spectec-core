#include <core.p4>
#include <v1model.p4>

header hdr {
  bit<8> a;
  bit<8> b;
  bit<8> c;
  bit<16> d;
}

<<<<<<< HEAD
error {
	Noerror}
=======
>>>>>>> main
struct headers {
    hdr h;
}

struct metadata {}

<<<<<<< HEAD
typedef bit<9> T;

type bit<9> new_T;


enum E {
	aa}

=======
>>>>>>> main
parser p(packet_in b, out headers h, inout metadata m, inout standard_metadata_t sm) {
    state start {
        b.extract<hdr>(h.h);
        transition accept;
    }
}

control vrfy(inout headers h, inout metadata m) { apply {} }

control ingress(inout headers h, inout metadata m, inout standard_metadata_t sm) {
<<<<<<< HEAD
    new_T z = (new_T)9w0;
    bit<9> l = 9w1;
    action add()
    { h.h.d = (bit<16>)(h.h.a + h.h.b + h.h.c); sm.egress_spec = (bit<9>)0; }
    action sub()
    { h.h.c = (bit<8>)(h.h.b - h.h.a); 
    sm.egress_spec = (bit<9>)1; }
    action a_with_control_params(in bit<9> x = 9w2, in bit<9> y, bit<9> k,  bit<10> j) { sm.egress_spec = (bit<9>)x; }

    table t {
        key = { z : exact; h.h.b : exact; h.h.c : exact; h.h.d : lpm;}
        actions = { add; sub; a_with_control_params(9w1,1);}
        const entries = {
          ((new_T)9w0, 8w1+8w2, 8w2, 16w14) : sub;
=======
    action add()
    { h.h.d = (bit<16>)(h.h.a + h.h.b + h.h.c); sm.egress_spec = 0; }
    action sub()
    { h.h.c = (bit<8>)(h.h.b - h.h.a); sm.egress_spec = 1; }

    table t {
        key = { h.h.a : exact; h.h.b : exact; h.h.c : exact; h.h.d : exact;}
        actions = { add; sub; }
        const entries = {
          (32w0, 32w1, 32w2, 32w14) : sub;
          (32w7, 32w7, 32w7, 32w49) : sub;
          (32w1, 32w2, 32w3, _) : sub;
>>>>>>> main
          
        }
        const default_action = add;
    }
<<<<<<< HEAD
    table tt {
        key = { h.h.a : exact; h.h.b : exact; h.h.c : exact; h.h.d : ternary;}
        actions = { add; sub; a_with_control_params(l,2);}
        const entries = {
          (0, 1, 2, 14) : sub;
          (8w0, 8w1, 8w2, 16w14) : add;
          (8w9, 8w7, 8w7, 16w49) : a_with_control_params(l,2,9w1,10w2);
          (8w1, 8w2, 8w3, 16w0 &&& 0xFFFF) : a_with_control_params(l,2,9w1,10w2);
          
        }
        const default_action = a_with_control_params(l,2, 9w2 , (bit<10>) 1);
    }
    apply {  
	    if (tt.apply().miss) {
	    	t.apply();
	    }    
    }
=======
    apply { t.apply(); }
>>>>>>> main
}

control update(inout headers h, inout metadata m) { apply {} }

control egress(inout headers h, inout metadata m, inout standard_metadata_t sm) { apply {} }

control deparser(packet_out b, in headers h) {
    apply { b.emit<hdr>(h.h); }
}

<<<<<<< HEAD
=======
V1Switch(p(), vrfy(), ingress(), egress(), update(), deparser()) main;
>>>>>>> main
