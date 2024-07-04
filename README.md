# P4Cherry

(WIP) A to-be interpreter for P4, cherry-picked from the Petr4 project.

A [todo-list](https://docs.google.com/document/d/1qWz9xwa-DNAfd_Y1pCb6zkRcxO5536zCc4aw5ExA0W8/edit?usp=sharing) of features to be implemented.

## Building

### Prerequisites

You will need `ocaml` installed with `dune` and `menhir` library using `opam`.

* Install `opam` version 2.0.5 or higher.
  ```
  $ apt-get install opam
  $ opam init
  ```

* Set `ocaml` as version 4.14.0.
  ```
  $ opam switch create 4.14.0
  ```
  
* Install `dune` version 3.13.0 and `menhir` version 20231231 and `core` version 15.0.0 via `opam` .
  ```
  $ opam install dune menhir core.15.0.0
  ```

* Install the project.

```shell
$ dune build p4cherry.opam 
$ opam install .
```

### Building the Project

```shell
$ make
```

This creates an executable `p4cherry` in the project root.

## Run parser (full), desugarer (WIP), instantiation (WIP), and interpreter (WIP) for an Example File

Instantiation and interpreter are work-in-progress.
To name a few major missing features:
- Does not handle direct application of objects (e.g., `apply`).
- No type inference for generics (type params); all generics must be explicitly specified.
- Tables evaluate to the default action regardless of the key.
- Adding table entries with STF is not supported.
- Incomplete support for builtin core, v1model builtin functions.

```shell
$ ./p4cherry parse -i test/arch test/petr4_sandbox_explicit-bmv2.p4
$ ./p4cherry desugar -i test/arch test/petr4_sandbox_explicit-bmv2.p4
$ ./p4cherry instantiate -i test/arch test/petr4_sandbox_explicit-bmv2.p4
$ ./p4cherry interp -a v1model -i test/arch test/petr4_sandbox_explicit-bmv2.p4 test/petr4_sandbox_explicit-bmv2.stf
```
