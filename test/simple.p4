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

control c(inout bit<32> arg)(bit<32> carg) {
    bit<32> x = 1;

    action a1(inout bit<32> b, bit<32> d) {
        b = d;
    }

    table t1 {
        actions = { a1(x); }
        default_action = a1(x, 0);
    }

    bit<32> x = 2;

    action a2(inout bit<32> b, bit<32> d) {
        b = d;
    }

    table t2 {
        actions = { a2(x); }
        default_action = a2(x, 0);
    }

    apply {
      bit<32> z = 3;
    }
}

control proto(inout bit<32> arg);
package top(proto _p);

top(c(32w77)) main;
