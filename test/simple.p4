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

const bit<32> glob = 32w42;

struct S {
    bit<32> x;
}

control c(inout bit<32> b) {
    bit<32> y = 1;

    action a(inout bit<32> b, bit<32> d) {
        b = d;
    }

    table t1 {
        actions = { a(x); }
        default_action = a(x, 0);
    }

    bit<32> z = 2;

    table t2 {
        actions = { a(x); }
        default_action = a(x, 0);
    }



    apply {
        S s1;
        S s2;
        s2 = { 0 };
        s1 = s2;
        s2 = s1;
        b = s2.x;
    }
}

control proto(inout bit<32> _b);
package top(proto _p);

top(c()) main;
