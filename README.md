# P4-SpecTec

An interpreter for P4, cherry-picked from the Petr4 project.
Also a to-be formal specification for P4, using the SpecTec framework.

## Building

This uses SpecTec as its submodule.

```shell
$ git submodule init
$ git submodule update
```

### Prerequisites for p4cherry

* Install `opam` version 2.0.5 or higher.
  ```shell
  $ apt-get install opam
  $ opam init
  ```

* Create OCaml switch for version 4.14.0.
  Install `dune` version 3.13.0 and `menhir` version 20231231 and `core` version `v0.15.0` via `opam`.
  ```shell
  $ opam switch create 4.14.0
  $ eval $(opam env)
  $ opam install dune menhir core.v0.15.0
  ```

* Install the project.
  ```shell
  $ cd p4
  $ dune build p4cherry.opam 
  $ opam install .
  $ cd ..
  ```

### Prerequisites for SpecTec

* Create OCaml switch for version 5.0.0.
  Install `dune` version 3.11.0, `menhir` version 20230608, `mdx` version 2.3.1, and `zarith` version 1.13, via `opam` (default versions).
  ```shell
  $ opam switch create 5.0.0
  $ eval $(opam env)
  $ opam install dune menhir mdx zarith
  ```

### Building the Project

```shell
$ make build
```

This creates executables `p4cherry`, `p4cherry-test`, and `watsup` in the project root.

### Additional Notes

You may also need `libgmp-dev` and `pkg-config`, if the error message says so.

## p4cherry: A language implementation for P4

### To Run p4cherry

```shell
$ ./p4cherry parse -i p4/test/arch [FILENAME].p4
$ ./p4cherry typecheck -i p4/test/arch [FILENAME].p4
$ ./p4cherry instantiate -i p4/test/arch [FILENAME].p4
$ ./p4cherry run -a v1model -i p4/test/arch [FILENAME].p4 [TESTNAME].stf
```

Note that p4cherry currently only supports the V1Model architecture.

### Current Test Status

You can run the tests against the p4c compiler test suite and petr4 custom test suite with:

```shell
$ make test
```

#### Parser

Parsing, pretty-printing, and roundtripping [p4c](p4/status/p4c/parser.log) [petr4](p4/status/petr4/parser.log)

#### Type checker

* Positive type checker tests (well-typed programs should be accepted) [p4c](p4/status/p4c/typecheck-pos.log) [petr4](p4/status/petr4/typecheck-pos.log)
* Excluded positive type checker tests [p4c](p4/status/p4c/typecheck-pos-excluded.log)
* Negative type checker tests (ill-typed programs should be rejected) [p4c](p4/status/p4c/typecheck-neg.log)
* Excluded negative type checker tests [p4c](p4/status/p4c/typecheck-neg-excluded.log)

Analysis of test failures: [p4c-pos](p4/status/p4c/typecheck-pos.analysis.md) [p4c-neg](p4/status/p4c/typecheck-neg.analysis.md) [petr4-pos](p4/status/petr4/typecheck-pos.analysis.md)

#### Instantiator

Instantiation of stateful objects [p4c](p4/status/p4c/instantiate.log) [petr4](p4/status/petr4/instantiate.log)

## P4-SpecTec: A language specification for P4

Running STF tests against the p4c compiler test suite and petr4 custom test suite (for V1Model) [p4c](p4/status/p4c/run-v1model.log) [petr4](p4/status/petr4/run-v1model.log)

Analysis of test failures: [p4c](p4/status/p4c/run-v1model.analysis.md) [petr4](p4/status/petr4/run-v1model.analysis.md)

### To Build the Spec and Output in LaTeX

```shell
$ make spec
```

This creates a PDF spec in spec/spec.pdf.
