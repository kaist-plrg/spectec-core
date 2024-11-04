# P4Cherry

(WIP) A to-be interpreter for P4, cherry-picked from the Petr4 project.

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
  
* Install `dune` version 3.13.0 and `menhir` version 20231231 and `core` version `v0.15.0` via `opam` .
  ```
  $ opam install dune menhir core.v0.15.0
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

## Run parser and type checker for an Example File

```shell
$ ./p4cherry parse -i test/arch [FILENAME].p4
$ ./p4cherry typecheck -i test/arch [FILENAME].p4
```

## Current Test Status

### Parser

**[Parsing, pretty-printing, and roundtripping](status/parser.log)**

### Type checker
* **[Positive type checker tests](status/typecheck-pos.log)** (well-typed programs should be accepted)
* **[Negavie type checker tests](status/typecheck-neg.log)** (ill-typed programs should be rejected)
