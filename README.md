# SpecTec-Core

A spec programming framework.
SpecTec was originally developed for WebAssembly (Wasm-SpecTec), then adapted/generalized for P4 (P4-SpecTec). SpecTec Core is a stripped down version of P4-SpecTec's algorithmic flavor, meant to serve as a base for adaptation to other languages or domains.

### Installation

* Install `opam` version 2.0.5 or higher.
  ```bash
  apt-get install opam
  opam init
  ```

* Create OCaml switch for version 5.1.0
  Install `dune` version 3.16.1, `bignum` version v0.17.0, `menhir` version 20240715, `core` version v0.17.1, `core_unix` version v0.17.0, and `bisect_ppx` version 2.8.3 via `opam`.
  ```bash
  opam switch create 5.1.0
  eval $(opam env)
  opam install dune bignum menhir core core_unix bisect_ppx
  ```

### Building the Project

```bash
$ make exe
```

This creates an executable `spectec-core` in the project root.

### Structure

SpecTec-Core currently consists of five main components.
* SpecTec EL is the surface language in which the spec is authored.
* SpecTec IL (internal language). EL -> IL conversion is called "elaboration". Elaboration makes the spec more algorithmic and unambiguous.
* SpecTec SL (structured langauge). IL -> SL conversion is called "structuring". Structuring uses syntactic over-approximation to create a more structured control-flow.
* Interpreter backends for IL and SL.

### Contributing

SpecTec-Core is an open-source project. Please feel free to contribute by opening issues or pull requests.

### License

SpecTec-Core is released under the [Apache 2.0 license](LICENSE).
