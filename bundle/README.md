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
`-- documentation/
    |-- impty.adoc        prose document source (AsciiDoc + splice directives)
    `-- docinfo.html      stylesheet for the rendered output
```

The commands below read `spectecx.config` from the current directory for their `--spec` and `--batch-dir` defaults, so run them from this directory.

## Getting the tool

Installation -- prebuilt binary, Docker, or build from source -- is on the
project's [GitHub Releases](https://github.com/kaist-plrg/spectecx/releases)
page, the same release you downloaded this bundle from.

## 1. Typed Imp: executable inference rules

`impty.spectec` defines the language as inference rules -- syntax, typing, and evaluation. The rules are executable: the tool runs a program by building a derivation from them.

```sh
# typecheck, then run hello.imp
./spectecx impty typecheck -p tests/base/hello.imp
./spectecx impty eval      -p tests/base/hello.imp

# show the derivation tree the run was built from, in spec syntax
./spectecx impty eval -p tests/base/hello.imp --tree.level conclusion
```

## 2. Adding first-class functions (test-driven)

`impty.spectec` has the syntax for functions but leaves four rules as `-- TODO` stubs. The base programs already pass; the function programs fail until the stubs are filled in. Fill in a rule, re-run, repeat.

```sh
# base programs pass; the full suite has the 4 function programs failing
make test-base
make test

# debug one program at a time
./spectecx impty typecheck -p tests/functions/closure.imp
./spectecx impty eval      -p tests/functions/closure.imp
```

## 3. Testing: coverage and property-based testing

```sh
# coverage: which rules the suite exercises (summary lists the uncovered ones)
./spectecx impty batch --branch-coverage.level summary

# property-based testing of type safety
./spectecx impty quickcheck
./spectecx impty quickcheck --generalize
./spectecx impty quickcheck --num-tests 400 --branch-coverage.level summary
```

## 4. Documentation: generated prose

The prose document splices straight from `impty.spectec`. Needs [asciidoctor](https://asciidoctor.org/) (`gem install asciidoctor asciidoctor-pdf`), or use Docker (see the release page).

```sh
make splice-html    # -> documentation/impty.html
make splice-pdf     # -> documentation/impty.pdf
```

Then open `documentation/impty.html` (or `documentation/impty.pdf`).
