# P4-SpecTec

An implementation of the P4 language.
This reuses parts of the [Petr4](https://github.com/verified-network-toolchain/petr4) codebase, especially the parser and numerics implementation.

Also a to-be formal specification for P4, using the SpecTec framework.

## Building

This uses Wasm-SpecTec as its submodule.

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
  Install `dune` version 3.13.0 and `menhir` version 20231231, `core` version v0.15.0, and `bisect_ppx` version 2.8.3 via `opam`.
  ```shell
  $ opam switch create 4.14.0
  $ eval $(opam env)
  $ opam install dune menhir core.v0.15.0 bisect_ppx
  ```

* Install the project.
  ```shell
  $ cd p4
  $ dune build p4cherry.opam 
  $ opam install .
  $ cd ..
  ```

### Prerequisites for Wasm-SpecTec

* Create OCaml switch for version 5.0.0.
  Install `dune` version 3.11.0, `menhir` version 20230608, `mdx` version 2.3.1, and `zarith` version 1.13, via `opam` (default versions).
  ```shell
  $ opam switch create 5.0.0
  $ eval $(opam env)
  $ opam install dune menhir mdx zarith
  ```

### Prerequisites for P4-SpecTec

The dependencies are the same as for p4cherry.

### Building the Project

```shell
$ make build
```

This creates executables `p4cherry`, `watsup`, and `p4spectec` from p4cherry, Wasm-SpecTec, and P4-SpecTec, respectively, in the project root.

### Additional Notes

You may also need `libgmp-dev` and `pkg-config`, if the error message says so.

## p4cherry: A language implementation for P4

### To Run p4cherry

```shell
$ ./p4cherry parse -i p4/testdata/arch [FILENAME].p4
$ ./p4cherry typecheck -i p4/testdata/arch [FILENAME].p4
$ ./p4cherry instantiate -i p4/testdata/arch [FILENAME].p4
$ ./p4cherry run -a v1model -i p4/testdata/arch [FILENAME].p4 [TESTNAME].stf
```

Note that p4cherry currently only supports the V1Model architecture.

### Current Test Status

You can run the tests against the p4c compiler test suite and petr4 custom test suite with:

```shell
$ make test-p4
```

To measure the coverage of the tests, run:

```shell
$ make coverage-p4
```

This will generate `index.html` in `p4/_coverage`.

#### Parser

Parsing, pretty-printing, and roundtripping ([p4c](p4/test/parse_p4c.expected) / [petr4](p4/test/parse_petr4.expected))

#### Type checker

* Positive type checker tests (well-typed programs should be accepted) ([p4c](p4/test/typecheck_pos_p4c.expected) / [petr4](p4/status/petr4/typecheck_pos_petr4.expected))
* Excluded positive type checker tests ([p4c](p4/test/typecheck_pos_p4c_excluded.expected))
* Negative type checker tests (ill-typed programs should be rejected) ([p4c](p4/test/typecheck_neg_p4c.expected) / [petr4](p4/test/typecheck_neg_petr4.expected))
* Excluded negative type checker tests ([p4c](p4/test/typecheck_neg_p4c_excluded.expected))

Analysis of test failures: [p4c-pos](p4/status/p4c/typecheck-pos.analysis.md) / [p4c-neg](p4/status/p4c/typecheck-neg.analysis.md) / [petr4-pos](p4/status/petr4/typecheck-pos.analysis.md)

#### Instantiator

Instantiation of stateful objects ([p4c](p4/test/instantiate_p4c.expected) / [petr4](p4/test/instantiate_petr4.expected))

#### Interpreter

Running STF tests against the p4c compiler test suite and petr4 custom test suite (for V1Model) ([p4c](p4/test/run_v1model_p4c.expected) / [petr4](p4/test/run_v1model_petr4.expected))

Analysis of test failures: [p4c](p4/status/p4c/run-v1model.analysis.md) / [petr4](p4/status/petr4/run-v1model.analysis.md)

## P4-SpecTec: A language specification framework for P4

### To Elaborate the P4-SpecTec Specification

```shell
$ ./p4spectec elab spec/*.watsup
```

### To Run the P4-SpecTec Specification

Currently, the P4-SpecTec specification defines the static semantics of P4.
Given a P4 program, below command runs the typing rules of the P4 language.

```shell
$ ./p4spectec run-typing spec/*.watsup -i p4/testdata/arch -p [FILENAME].p4
```

### To Initiate a Fuzz Loop for Generating P4 Programs

```shell
$ ./p4spectec testgen spec/*.watsup -i p4/testdata/arch -seed [SEED DIR] -gen [GEN DIR] -fuel [NUM]
```

This will generate P4 programs in the directory `[GEN DIR]` using the seed files in the directory `[SEED DIR]`.
`[NUM]` is the number of fuzz cycles to run.

After the fuzz loop, you may find the generated P4 programs in the directory `[GEN DIR]`, with the log file `fuzz.log`,
query files for mutations `query.log` and an initial coverage file `boot.cov`.

In later runs with the same seed directory, you can use warm boot to skip the pre-loop phase.

```shell
$ ./p4spectec testgen spec/*.watsup -i p4/testdata/arch -seed [SEED DIR] -gen [GEN DIR] -fuel [NUM] -warm [COV FILE]
```

At a high level, the fuzz loop will:

(#) Pre-loop: Measure the initial coverage of the phantom nodes

(#) Loop

    1. For each phantoms that were missed:
        A. Identify close-miss filenames
        B. Randomly sample N(=10) close-miss filenames
        C. For each close-miss filename:
            i. Run SL interpreter on the program
            ii. Fetch derivations, i.e., a set of close-ASTs for the phantom
            iii. Randomly sample M(=10) close-ASTs
            iv. For each close-AST:
                (1) Mutate the close-AST up to K(=10)
                (2) For each mutated close-AST:
                    a. Reassemble the program with the mutated AST
                    b. Run the SL interpreter on the mutated program
                    c. See if it has covered any new phantom or any new close-miss
    2. Repeat the loop until the fuel is exhausted *)

### Contributing

p4cherry is an open-source project. Please feel free to contribute by opening issues or pull requests.

### License

p4cherry is released under the [Apache 2.0 license](LICENSE).
