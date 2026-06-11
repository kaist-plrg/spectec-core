# SpecTecX hands-on session

A small typed imperative language ("Typed Imp"), specified in SpecTecX. Follow along and copy-paste commands from this file as you go.

The `spectecx` binary is obtained separately -- see [Getting the tool](#getting-the-tool). Run every command from this directory; they invoke the binary as `./spectecx`.

## Layout

```
.
|-- impty.spectec        the language spec: base + first-class-function stubs
|-- spectecx.config      project-local CLI defaults
|-- Makefile             command helpers: tests + prose (`make help`)
|-- Dockerfile           build-from-source fallback
|-- tests/
|   |-- base/             base-language programs (run as-is)
|   `-- functions/        programs using functions (run once the stubs are filled in)
|-- recursion/           optional exercise for early finishers (self-contained -- see its README)
|   |-- recursion.spectec  a separate skeleton adding conditionals + recursion
|   |-- README.md          the exercise
|   |-- Makefile           `make test-rec`
|   `-- tests/             base + functions + recursion programs
`-- documentation/
    |-- impty.adoc        prose document source (AsciiDoc + splice directives)
    `-- docinfo.html      stylesheet for the rendered output
```

The commands below read `spectecx.config` from the current directory for their `--spec` and `--batch-dir` defaults, so run them from this directory.

## Getting the tool

Installation -- prebuilt binary, Docker, or build from source -- is on the
project's [GitHub Releases](https://github.com/kaist-plrg/spectecx/releases)
page, the same release you downloaded this bundle from. The release also has
an optional VS Code extension (`spectecx.vsix`) that highlights the
`.spectec` spec you will be editing.

## 1. Typed Imp: executable inference rules

`impty.spectec` defines the language as inference rules -- syntax, typing, and evaluation. The rules are executable: the tool runs a program by building a derivation from them.

```sh
# typecheck, then run hello.imp
./spectecx impty typecheck -p tests/base/hello.imp
./spectecx impty eval      -p tests/base/hello.imp

# show the full derivation tree (premises and all) the run was built from
./spectecx impty eval -p tests/base/hello.imp --tree.level premise
```

## 2. Documentation: generated prose

The prose document splices straight from `impty.spectec`. Needs [asciidoctor](https://asciidoctor.org/) (`gem install asciidoctor asciidoctor-pdf`), or use Docker (see the release page).

```sh
make doc-html    # -> documentation/impty.html
make doc-pdf     # -> documentation/impty.pdf
```

Then open `documentation/impty.html` (or `documentation/impty.pdf`).

## 3-a. Adding first-class functions (test-driven)

`impty.spectec` has the syntax for functions but leaves four rules as `-- TODO` stubs. The base programs already pass; the function programs fail until the stubs are filled in. Fill in a rule, re-run, repeat.

```sh
# base programs pass; the full suite has the 4 function programs failing
make test-base
make test

# after editing a rule, type-check the spec: errors and warnings, no IL dump
./spectecx elab --check impty.spectec

# debug one program at a time
./spectecx impty typecheck -p tests/functions/closure.imp
./spectecx impty eval      -p tests/functions/closure.imp

```
## 3-b. Optional: recursion (for early finishers)

Finished early? `recursion/` is a separate, self-contained exercise that extends the language with a conditional expression and recursive function declarations. It builds on the function rules you just wrote. `cd` into it and follow its README:

```sh
cd recursion
make test-rec      # runs base + functions + recursion programs
```

## 4. Testing: coverage and property-based testing

```sh
# coverage: which rules the suite exercises (summary lists the uncovered ones)
./spectecx impty batch --branch-coverage.level summary

# property-based testing of type safety
./spectecx impty quickcheck
./spectecx impty quickcheck --generalize
./spectecx impty quickcheck --num-tests 400 --branch-coverage.level summary
```

