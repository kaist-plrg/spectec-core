# Editor support for SpecTecX

Integrations for `.spectec` files: syntax highlighting, and diagnostics from a
language server.

## Syntax highlighting

Each subdirectory is a standalone highlighter; see its README to install.

- `vscode/` — a VS Code extension (TextMate grammar).
- `emacs/` — a tree-sitter major mode (`spectec-ts-mode`).
- `vim/` — a Vim/Neovim syntax file with filetype detection.

## Language server

`spectecx-lsp` checks the whole spec on open and save, and while typing with
expensive checks throttled. It serves
diagnostics (the same set the CLI prints, with a note naming the undeclared
metavariables behind an elaboration failure), hover, completion, go-to-definition,
find all references, and the document outline. Build it from the repository root and put
it on your `PATH`:

```bash
make lsp                                               # produces ./spectecx-lsp
ln -sf "$PWD/spectecx-lsp" ~/.local/bin/spectecx-lsp   # if ~/.local/bin is on PATH
```

The plugins below already launch `spectecx-lsp` from `PATH`, so that is the only
machine-specific step.

### Neovim (0.11+)

The `vim/` plugin carries both filetype detection and the language-server config
(`vim/lsp/spectecx.lua`). Add `vim/` to your runtimepath (e.g. via your plugin
manager), then enable the server in your config:

```lua
vim.lsp.enable("spectecx")
```

Open a `.spectec` file and errors are underlined. (Classic Vim ignores the Lua
config and just uses the syntax file.)

### Emacs (eglot)

`spectec-ts-mode` registers the server with eglot, so no separate LSP config is
needed:

```elisp
(add-to-list 'load-path "/path/to/editors/emacs")
(require 'spectec-ts-mode)
```

Open a `.spectec` file and run `M-x eglot`.

### VS Code

The `vscode/` extension carries both the highlighting and a language client, so
installing it is all that is needed:

```bash
ln -s "$PWD/editors/vscode" ~/.vscode/extensions/spectecx   # or: make vsix
```

It looks for `spectecx-lsp` at the root of an open workspace folder before
falling back to `PATH`, so a checkout of this repository works with no
configuration once `make lsp` has run. Set `spectec.languageServer.path` to
point somewhere else, or `spectec.languageServer.enable` to `false` for
highlighting only. See [vscode/README.md](vscode/README.md) for the full list.

This extension also handles `.watsup` files, superseding the standalone WatSup
highlighter from the p4-spectec repository; installing both makes them compete
for the same file extensions.

It ships two grammars. The WatSup one is the default for both `.watsup` and
`.spectec`; the block-structured SpecTecX one is opt-in per file through the
status-bar language picker, or globally with
`"files.associations": { "*.spectec": "spectec" }`.

## Trying it without changing your config

```bash
printf 'syntax t = foo\n' > /tmp/t.spectec

# Neovim
nvim --cmd "set rtp^=$PWD/editors/vim" -c "lua vim.lsp.enable('spectecx')" /tmp/t.spectec

# Emacs (then M-x eglot)
emacs -Q -l editors/emacs/spectec-ts-mode.el /tmp/t.spectec
```

Either should underline `foo` as an undefined type.
