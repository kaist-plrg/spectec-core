# P4-SpecTec

A mechanized formal specification for the P4 programming language, using the SpecTec framework.
This reuses parts of the [Petr4](https://github.com/verified-network-toolchain/petr4) codebase, especially the parser and numerics implementation.

### Prerequisites

* Install `opam` version 2.0.5 or higher.
  ```shell
  $ apt-get install opam
  $ opam init
  ```

* Create OCaml switch for version 4.14.0
  Install `dune` version 3.16.1, `bignum` version v0.17.0, `menhir` version 20240715, `core` version v0.17.1, `core_unix` version v0.17.0, and `bisect_ppx` version 2.8.3 via `opam`.
  ```shell
  $ opam switch create 4.14.0
  $ eval $(opam env)
  $ opam install dune bignum menhir core core_unix bisect_ppx
  ```

### Building the Project

```shell
$ make build-spec
```

This creates an executable `p4spectec` in the project root.

### Additional Notes

You may also need `libgmp-dev` and `pkg-config`, if the error message says so.

## P4-SpecTec: A language specification framework for P4

### To Elaborate the P4-SpecTec Specification

```shell
$ ./p4spectec elab spec/*.watsup
```

### To Run the P4-SpecTec Specification

Currently, the P4-SpecTec specification defines the static semantics of P4.
Given a P4 program, below command runs the typing rules of the P4 language.

```shell
$ ./p4spectec run-sl spec/*.watsup -i p4c/p4include -p [FILENAME].p4
```

### To Initiate a Fuzz Loop for Generating P4 Programs

```shell
$ ./p4spectec testgen spec/*.watsup -i p4c/p4include -seed [SEED DIR] -gen [GEN DIR] -fuel [NUM]
```

This will generate P4 programs in the directory `[GEN DIR]` using the seed files in the directory `[SEED DIR]`.
`[NUM]` is the number of fuzz cycles to run.

After the fuzz loop, you may find the generated P4 programs in the directory `[GEN DIR]`, with the log file `fuzz.log`,
query files for mutations `query.log` and an initial coverage file `boot.cov`.

In later runs with the same seed directory, you can use warm boot to skip the pre-loop phase.

```shell
$ ./p4spectec testgen spec/*.watsup -i p4c/p4include -seed [SEED DIR] -gen [GEN DIR] -fuel [NUM] -warm [COV FILE]
```

At a high level, the fuzz loop will:

### Debugging the Derivation

To see what values are derived from a given P4 program and a phantom id, run:

```
$ ./p4spectec testgen-dbg spec/*.watsup -i p4c/p4include -p [FILENAME].p4 -pid [PID] -debug [DEBUG DIR]
```

### Contributing

P4-SpecTec is an open-source project. Please feel free to contribute by opening issues or pull requests.

### License

P4-SpecTec is released under the [Apache 2.0 license](LICENSE).
