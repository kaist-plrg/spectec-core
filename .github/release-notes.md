# SpecTecX hands-on session

Tool and materials for the SpecTecX hands-on: the `spectecx` binary and a small typed imperative language ("Typed Imp") to read, run, and extend.

## 1. Get the materials

Download **`spectecx-tutorial.tar.gz`** (spec skeleton, tests, README, and prose document) and unpack it:

```bash
curl -L -O https://github.com/kaist-plrg/spectecx/releases/download/${TAG}/spectecx-tutorial.tar.gz
tar -xzf spectecx-tutorial.tar.gz && cd spectecx-tutorial
```

The unpacked `README.md` drives the session; the steps below just put the `spectecx` binary alongside it first.

## 2. Install the tool

Pick the binary for your platform:

- **macOS Apple Silicon (M series):** `spectecx-darwin-arm64`
- **Linux x86_64:** `spectecx-linux-x64`
- **macOS Intel:** no native binary; use Docker or build from source (below).
- **Windows:** use WSL2 with the Linux binary, or Docker.

Download it into the unpacked bundle directory as `spectecx`, make it executable, and check it runs:

```bash
curl -L -o spectecx https://github.com/kaist-plrg/spectecx/releases/download/${TAG}/spectecx-<your-platform>
chmod +x spectecx
./spectecx help
```

Downloaded with the browser instead? Rename the file to `spectecx` (`mv spectecx-<your-platform> spectecx`) before `chmod`. On macOS, clear the quarantine flag with `xattr -d com.apple.quarantine spectecx`.

Every command runs the binary as `./spectecx`, so there is no `PATH` setup to do.

`make doc` splices the spec into AsciiDoc at `documentation/impty.adoc`. Read it in any editor, or preview it with the VS Code / JetBrains AsciiDoc extension. For HTML/PDF (`make doc-html` / `make doc-pdf`), install asciidoctor (`gem install asciidoctor asciidoctor-pdf`, already in the Docker image).

### Alternatives

**Docker** -- a prebuilt, self-contained image (tool, materials, an editor, and asciidoctor), for anyone without a native binary. No download needed -- pull and run:

```bash
docker pull kaistplrg/spectecx:tutorial
docker run -it kaistplrg/spectecx:tutorial
```

This opens a shell in the bundle; from there step 3 and the whole README run exactly as written (`./spectecx ...`, `make test`, ...).

**Build from source** -- opam, OCaml >= 5.1, GMP headers:

```bash
git clone https://github.com/kaist-plrg/spectecx.git && cd spectecx
opam switch create spectecx 5.1.0
opam install -y --switch=spectecx --deps-only ./spectec
make exe                           # produces ./spectecx
```

### Optional: editor syntax highlighting

**VS Code** -- download **`spectecx.vsix`** from this release page and install it:

```bash
code --install-extension spectecx.vsix
```

If `code` is not on your PATH, run "Shell Command: Install 'code' command in PATH" from the command palette first. For VSCodium, use `codium --install-extension spectecx.vsix`.

**Neovim** (0.9+) -- self-contained tree-sitter plugin (needs a C compiler; no nvim-treesitter required). With lazy.nvim:

```lua
{ "KunJeong/tree-sitter-spectec", build = "make parser" }
```

See [the grammar repo](https://github.com/KunJeong/tree-sitter-spectec) for other plugin managers.

**Vim, or Neovim before 0.9** -- a lightweight regex highlighter without build or dependencies. It ships in the bundle; run it on a spec directly:

```bash
vim --cmd 'set rtp^=editors/vim' impty.spectec
```

Or install it permanently: download **`spectec-vim.tar.gz`** and unpack it into your Vim runtime (`~/.config/nvim` for Neovim):

```bash
mkdir -p ~/.vim && tar -xzf spectec-vim.tar.gz -C ~/.vim
```

**Emacs** -- (29+ with tree-sitter, plus a C compiler) `spectec-ts-mode.el` ships in the bundle (and on this release page). Run it on a spec; it offers to build the grammar on first use:

```bash
emacs -l spectec-ts-mode.el impty.spectec
```

To make it permanent, load it from your init instead, so every `.spectec` file opens highlighted:

```elisp
(add-to-list 'load-path "/directory/holding/spectec-ts-mode.el")
(require 'spectec-ts-mode)
```

## 3. Start the session

From the unpacked bundle, follow `README.md` from the top:

```bash
./spectecx impty eval -p tests/base/hello.imp
```
