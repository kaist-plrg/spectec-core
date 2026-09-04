# SpecTecX: An executable subset of SpecTec

SpecTec is a spec programming framework, originally developed for WebAssembly (Wasm-SpecTec), then adapted/generalized for P4 (P4-SpecTec). SpecTecX is a stripped-down version of P4-SpecTec that decouples language semantics from target definition, meant to serve as an adaptation base for other languages or domains.

### Installation

* Install `opam` version 2.0.5 or higher.
  ```bash
  apt-get install opam
  opam init
  ```

* Create an OCaml switch:
  ```bash
  opam switch create spectecx 5.1.1
  eval $(opam env)
  ```

* Install the core libraries and target-independent `spectec` executable from the checkout:
  ```bash
  opam install ./spectec.opam
  ```

* Install the target packages you need:
  ```bash
  opam install ./spectec-target-p4.opam
  opam install ./spectec-target-miniml.opam
  opam install ./spectec-target-impty.opam
  ```
  Each target package installs its command plugin and default specification. The `spectec` executable discovers installed target plugins at startup.

For development, install every package's pinned dependency versions without installing the packages themselves:

```bash
opam install . --deps-only --locked
```

The lockfile (`spectec.opam.locked`) records the exact transitive dependency set CI uses. The unlocked constraints live in `dune-project` and surface in the generated opam files.

### Building the Project

```bash
make exe
```

This creates an executable `spectecx` in the project root.

### Structure

SpecTecX currently consists of four main components.

* SpecTec EL is the surface language in which the spec is authored.
* SpecTec IL (internal language). EL -> IL conversion is called "elaboration". Elaboration makes the spec more algorithmic and unambiguous.
* SpecTec SL (structured language). IL -> SL conversion is called "structuring". Structuring groups related execution paths into explicit branching with over-approximation. This minimizes backtracking, making the SL interpreter much faster than the IL interpreter.
* Interpreter backends for IL/SL.
  * Needs to be coupled with a parser that converts an input file into a SpecTec IL value.

Repository layout:

```
spectec/lib/lang/        ASTs for el / il / sl / xl
spectec/lib/pass/        parse, elaborate (EL→IL), structure (IL→SL)
spectec/lib/interp/      IL and SL interpreters, builtins, target interface
spectec/lib/cli/         reusable CLI machinery and target plugin loading
spectec/lib/spectec.ml   public facade (pipeline + eval + Error/Task/Target)
spectec/targets/<t>/     per-target code, CLI modules, and plugin registration
spectec/bin/             target-independent command-line entrypoint
spectec/test/            diff-based test drivers
spectec/testdata/        test inputs
```

### Commands

The target-specific examples require the corresponding target package.

```bash
# print out the IL representation of a SpecTec spec
./spectecx elab spec/*.spectec
# print the SL representation of a SpecTec spec
./spectecx struct spec/*.spectec

## P4-specific commands

# parse a P4 program to an IL value (-r to do a roundtrip test)
./spectecx p4 parse spec/*.spectec -i spectec/testdata/interp/p4-tests/includes -p target/file.p4 [-r]

# run a P4 program based on SpecTec IL/SL
./spectecx p4 typecheck -i spectec/testdata/interp/p4-tests/includes -p target/file.p4
./spectecx p4 typecheck -i spectec/testdata/interp/p4-tests/includes -p target/file.p4 --sl
```

### Editor support

Integrations for `.spectec` files live in `editors/`:

- **Syntax highlighting** for VS Code, Emacs, and Vim/Neovim, one per subdirectory.
- **Diagnostics**: `make lsp` builds `spectecx-lsp`, a language server that reports parse and elaboration errors as you edit.

See [editors/README.md](editors/README.md) for installing a highlighter and turning on the language server.

### Testing
```bash
make test
```

- Checks parsing, elaboration and structuring using the `spectec/examples/p4-concrete` spec corpus.
- Checks IL/SL interpreter coupled with the P4 parser using `spectec/testdata/interp/p4-tests` files.

### Adding a New Target

Targets live in `spectec/targets/<name>/`, separate from `spectec/lib/`. The reusable CLI infrastructure (`Target_cli`, `Task_cli`, `Subcommand` constructors) lives in `spectec/lib/cli/`. To add a target:

1. Implement `Spectec.Target.S` and one or more `Spectec.Task.S` in `spectec/targets/<name>/`.
2. Add target-specific built-ins under `spectec/targets/<name>/builtins/`.
3. For each task, implement a `Cli.Task_cli.S` module that parses command-line flags into the task's input.
4. Compose those task-CLIs into a `Cli : Cli.Target_cli.S` module using `Cli.Subcommand` constructors (`make_task`, `make_parse`, `make_batch`, `make_checkpoint`).
5. Add a plugin entry module that calls `Cli.Target_registry.register (module Your_target.Cli)` when loaded.
6. Declare a target package in `dune-project`, including any named installation directories for packaged specifications.
7. Add a Dune `plugin` stanza that installs the entry module in the core package's `target_plugins` directory. Use `generate_sites_module` when target code needs to locate packaged specifications.

The P4, Mini-ML, and Impty targets under `spectec/targets/` are working examples. Each is packaged independently, so adding a target does not require changing `spectec/bin/main.ml`.

### Contributing

Contributions are welcome — open an issue or pull request. See [CONTRIBUTING.md](CONTRIBUTING.md) for code conventions, commit and PR format, and rebase guidance.

### License

SpecTecX is released under the [Apache 2.0 license](LICENSE).

### Credits

Most of the current codebase is derived from [P4-SpecTec](https://github.com/kaist-plrg/p4-spectec), which in turn is largely based on [Wasm-SpecTec](https://github.com/Wasm-DSL/spectec/tree/main).
