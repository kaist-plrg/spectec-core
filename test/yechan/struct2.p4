/*
Copyright 2013-present Barefoot Networks, Inc.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
*/
#include <core.p4>
#include <v1model.p4>

struct P {
    bit<32> f1;
    bit<32> f2;
}

struct T {
    int<32> t1;
    int<32> t2;
}

struct S <X> {
    X s1;
    T s2;
}

struct Empty {};

const T t = { 32s10, 32s20 };

// Shadowing
struct T {
    bool t1;
}

const S<T> s = { {true} , t };


const int<32> x = 1;
const int<32> y = s.s2.t2;

const int<32> w = .t.t1;

const T t1 = s.s1;
const Empty e = {};

header hdr {
  bit<4> a;
  bit<4> b;
  bit<4> c;
}

struct headers {
    hdr h;
}

struct metadata {}

parser p(packet_in b, out headers h, inout metadata m, inout standard_metadata_t sm) {
    state start {
        b.extract<hdr>(h.h);
        transition accept;
    }
}

control vrfy(inout headers h, inout metadata m) { apply {} }

control ingress(inout headers h, inout metadata m, inout standard_metadata_t sm) {
    action add()
    { h.h.c = (bit<4>)(h.h.a + h.h.b); sm.egress_spec = x; }
    action sub()
    { h.h.c = (bit<4>)(h.h.a - h.h.b); sm.egress_spec = y; }
    action a()
    { h.h.c = (bit<4>)(h.h.a + h.h.b); sm.egress_spec = w; }

    table t {
        key = { h.h.a : exact; h.h.b : exact; h.h.c : exact; }
        actions = { add; sub; a;}
        const entries = {
          (32w0, 32w1, 32w2) : add;
          (32w7, 32w7, 32w7) : sub;
        }
        const default_action = a;
    }
    apply { t.apply(); }
}

control update(inout headers h, inout metadata m) { apply {} }

control egress(inout headers h, inout metadata m, inout standard_metadata_t sm) { apply {} }

control deparser(packet_out b, in headers h) {
    apply { b.emit<hdr>(h.h); }
}

V1Switch(p(), vrfy(), ingress(), egress(), update(), deparser()) main;
