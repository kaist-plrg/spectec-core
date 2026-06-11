# SpecTecX for Emacs

`spectec-ts-mode`, a tree-sitter major mode for SpecTecX spec files (`.spectec`). Needs Emacs 29.1+ with tree-sitter and a C compiler.

## Install

To try it on a spec, run Emacs with the mode loaded -- it offers to build the grammar on first use (a one-time compile):

```bash
emacs -l spectec-ts-mode.el yourspec.spectec
```

For everyday use, load it from your init instead, so every `.spectec` file opens highlighted:

```elisp
(add-to-list 'load-path "/path/to/editors/emacs")
(require 'spectec-ts-mode)
```

Decoration follows `treesit-font-lock-level` (default 3); raise it to 4 to also highlight the meta-notation, which otherwise recedes.
