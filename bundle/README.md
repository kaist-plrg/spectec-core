# SpecTecX hands-on session

A small typed imperative language ("Typed Imp"), specified in SpecTecX. You read the rules, run programs against them, and fill in the ones left as stubs. Run every command from this directory; they all invoke the binary as `./spectecx` (obtained separately -- see [Getting the tool](#getting-the-tool)).

## Layout

```
.
|-- impty.spectec        the language spec: base + first-class-function stubs
|-- spectecx.config      project-local CLI defaults
|-- Makefile             command helpers (`make help`)
|-- Dockerfile           build-from-source fallback
|-- tests/
|   |-- base/             base-language programs (run as-is)
|   `-- functions/        programs using functions (run once the stubs are filled in)
|-- recursion/           optional exercise for early finishers (self-contained -- see its README)
|-- skeleton/
|   `-- impty.adoc        prose document source (AsciiDoc + splice directives)
`-- documentation/
    `-- docinfo.html      stylesheet; `make doc` writes impty.adoc here
```

Commands read `spectecx.config` from this directory for their `--spec` and `--batch-dir` defaults, so run them from here.

## Getting the tool

Installation -- prebuilt binary, Docker, or build from source -- is on the
project's [GitHub Releases](https://github.com/kaist-plrg/spectecx/releases)
page, the same release you downloaded this bundle from. For highlighting while
you edit, this bundle ships a VS Code extension (`code --install-extension
spectecx.vsix`), an Emacs mode (`emacs -l spectec-ts-mode.el impty.spectec`),
and a regex Vim highlighter (`vim --cmd 'set rtp^=editors/vim' impty.spectec`);
the release page also links a Neovim plugin.

## The spec is executable

`impty.spectec` defines Typed Imp as inference rules -- syntax, typing, and evaluation. The rules are executable: the tool typechecks or runs a program by building a derivation from them.

```sh
./spectecx impty typecheck -p tests/base/hello.imp
./spectecx impty eval      -p tests/base/hello.imp
```

## Your task: first-class functions

`impty.spectec` has the syntax for functions but leaves four rules -- typing and evaluation for `fun` and for calls -- as `-- TODO` stubs. The base programs already pass; the function programs fail until you fill the stubs in. Work the loop below: edit a rule, then check, test, quickcheck, and debug.

### 1. Check the spec

After each edit, typecheck the spec itself -- errors and warnings, no IL dump:

```sh
make check       # ./spectecx elab --check impty.spectec
```

### 2. Run the provided tests

`test-base` is the fast inner loop; `test` is the whole corpus -- the four function programs fail until you are done:

```sh
make test-base   # ./spectecx impty batch --batch-dir tests/base
make test        # ./spectecx impty batch
```

`./spectecx impty batch --branch-coverage.level summary` also reports which rules the suite exercises -- handy for confirming your new rules actually fire.

### 3. Property-test type safety

The corpus is positive-only, so an over-permissive rule can pass every test and still be unsound. Quickcheck generates programs and hunts for a type-safety violation:

```sh
make quickcheck  # ./spectecx impty quickcheck --save-dir counterexamples
make verify      # make test, then make quickcheck
```

### 4. Debug one program

Copy a failing program's path from the test output and inspect its full derivation -- the tree of premises, and on a failure the pruned trace pointing at the rule that did not hold:

```sh
./spectecx impty debug -p tests/functions/closure.imp
```

(`typecheck` / `eval` run the same program without the tree.)

### 5. Generate the prose document

`make doc` splices the spec into an AsciiDoc document -- edit the spec, and the prose updates for free:

```sh
make doc         # -> documentation/impty.adoc
```

Read it in any editor, or preview it with the VS Code / JetBrains AsciiDoc extension. For HTML or PDF, install [asciidoctor](https://asciidoctor.org/) (`gem install asciidoctor asciidoctor-pdf`, already in the Docker image):

```sh
make doc-html    # -> documentation/impty.html
make doc-pdf     # -> documentation/impty.pdf
```

## Optional: recursion (for early finishers)

Finished early? `recursion/` is a separate, self-contained exercise that extends the language with a conditional expression and recursive function declarations, building on the function rules you just wrote. `cd` into it and follow its README:

```sh
cd recursion
make test        # base + functions + recursion programs
```
