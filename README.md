# P4Cherry

(WIP) A to-be interpreter for P4, cherry-picked from the Petr4 repository.

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

## Run parser (full), instantiation (WIP), and interpreter (WIP) for an Example File

Instantiation and interpreter are work-in-progress.
To name a few major missing features:
- No type inference for generics (type params); all generics must be explicitly specified.
- Tables evaluate to the default action regardless of the key.
- Packets contents are hardcoded to match the format in test/petr4_sandbox_explicit-bmv2.p4.
    - A temporary solution until STF is implemented.

```shell
$ ./p4cherry v1model test/arch test/petr4_sandbox_explicit-bmv2.p4
```
