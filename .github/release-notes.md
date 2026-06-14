# SpecTecX hands-on session

Everything you need for the SpecTecX hands-on. You'll read, run, and extend **Typed Imp** (a small typed imperative language) using the `spectecx` tool.

Setup is three steps: **(1)** download the materials, **(2)** install the `spectecx` binary, **(3)** follow the bundled `README.md`.

## Pick your setup

Decide based on what your machine already has:

| Path | Works on | What you need | Effort |
|------|----------|---------------|--------|
| **Prebuilt binary** (recommended) | macOS Apple Silicon, Linux x86_64 | a code editor | ~2 min |
| **Docker image** | any platform | Docker | 3.16GB pull |
| **Build from source** | any platform | opam, OCaml >= 5.1, GMP headers | ~10 min |

If a native binary covers your platform, take the prebuilt path. On macOS Intel or Windows, choose Docker (easiest) or build from source.

## 1. Get the materials

Download **`spectecx-tutorial.tar.gz`** (spec skeleton, tests, README, and prose document) and unpack it:

```bash
curl -L -O https://github.com/kaist-plrg/spectecx/releases/download/${TAG}/spectecx-tutorial.tar.gz
tar -xzf spectecx-tutorial.tar.gz && cd spectecx-tutorial
```

The unpacked `README.md` drives the session; the steps below just put the `spectecx` binary alongside it first. (Using Docker? Skip ahead, the image already contains the materials.)

## 2. Install the tool

Two prebuilt binaries are attached. Pick yours and download it into the unpacked bundle directory as `spectecx`:

**macOS (Apple Silicon / M-series):**

```bash
curl -L -o spectecx https://github.com/kaist-plrg/spectecx/releases/download/${TAG}/spectecx-darwin-arm64
```

**Linux (x86_64):**

```bash
curl -L -o spectecx https://github.com/kaist-plrg/spectecx/releases/download/${TAG}/spectecx-linux-x64
```

*macOS Intel or Windows:* no native binary, use Docker or build from source (below).

Then make it executable and check it runs:

```bash
chmod +x spectecx
./spectecx help
```

Downloaded with the browser instead? The file keeps its asset name, so rename it to `spectecx` (e.g. `mv spectecx-darwin-arm64 spectecx`) before `chmod`. On macOS, clear the quarantine flag with `xattr -d com.apple.quarantine spectecx`.

Every command runs the binary as `./spectecx`.

### Alternatives

**Docker:** a prebuilt, self-contained image (tool, materials, vim, and asciidoctor), for anyone without a native binary. Pull and run:

```bash
docker pull kaistplrg/spectecx:tutorial
docker run -it kaistplrg/spectecx:tutorial
```

This opens a shell in the bundle; from there step 3 and the whole README run exactly as written (`./spectecx ...`, `make test`, ...).

**Build from source:** opam, OCaml >= 5.1, GMP headers:

```bash
git clone -b tutorial-pldi26 https://github.com/kaist-plrg/spectecx.git
cd spectecx
opam switch create spectecx 5.1.0
opam install -y --switch=spectecx --deps-only ./spectec
make exe                              # produces ./spectecx in the repo root
```

Then copy the binary into your unpacked bundle, so `./spectecx` works there:

```bash
cp spectecx /path/to/spectecx-tutorial/spectecx
```

## 3. Editor syntax highlighting (recommended)

Highlighters for four editors come with this release. **The VS Code, Emacs, and Vim highlighters already sit in your unpacked bundle**. All three are also attached to this release page as standalone assets (`spectecx.vsix`, `spectec-ts-mode.el`, `spectec-vim.tar.gz`) if you'd rather grab them directly; Neovim builds from its own grammar repo.

**VS Code:** install the bundled **`spectecx.vsix`** (also on this release page):

```bash
code --install-extension spectecx.vsix
```

If `code` is not on your PATH, run "Shell Command: Install 'code' command in PATH" from the command palette first. For VSCodium, use `codium --install-extension spectecx.vsix`.

**Neovim (0.9+):** self-contained tree-sitter plugin (needs a C compiler; no nvim-treesitter required). With lazy.nvim:

```lua
{ "KunJeong/tree-sitter-spectec", build = "make parser" }
```

See [the grammar repo](https://github.com/KunJeong/tree-sitter-spectec) for other plugin managers.

**Vim, or Neovim before 0.9**: a lightweight regex highlighter without build or dependencies. It ships in the bundle; run it on a spec directly:

```bash
vim --cmd 'set rtp^=editors/vim' impty.spectec
```

Or install it permanently: download **`spectec-vim.tar.gz`** and unpack it into your Vim runtime (`~/.config/nvim` for Neovim):

```bash
mkdir -p ~/.vim && tar -xzf spectec-vim.tar.gz -C ~/.vim
```

**Emacs:** (29+ with tree-sitter, plus a C compiler) `spectec-ts-mode.el` ships in the bundle (and on this release page). Run it on a spec; it offers to build the grammar on first use:

```bash
emacs -l spectec-ts-mode.el impty.spectec
```

To make it permanent, load it from your init instead, so every `.spectec` file opens highlighted:

```elisp
(add-to-list 'load-path "/directory/holding/spectec-ts-mode.el")
(require 'spectec-ts-mode)
```

## 4. Prose document (optional)

`make doc` splices the spec into AsciiDoc at `documentation/impty.adoc` with no extra tools -- read it in any editor, or preview it with the VS Code / JetBrains AsciiDoc extension. For HTML or PDF (`make doc-html` / `make doc-pdf`), install asciidoctor:

```bash
gem install asciidoctor asciidoctor-pdf
```

It is already present in the Docker image.

## 5. Verification

From the unpacked bundle, run:

```bash
./spectecx impty eval -p tests/base/hello.imp
```

The hands-on exercise will proceed after the demo.
