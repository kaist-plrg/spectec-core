# SpecTecX for VS Code

Syntax highlighting for SpecTecX specification files (`.spectec`): declaration
keywords, premises, comments, object-syntax atoms and brackets, hints, and
operators. Also configures comment toggling and bracket pairs for the language.

## Install

Grab `spectecx.vsix` from the [release page](https://github.com/kaist-plrg/spectecx/releases)
and install it:

```bash
code --install-extension spectecx.vsix
```

(For VSCodium, use `codium --install-extension spectecx.vsix`.)

## Build from source

```bash
npx @vscode/vsce package -o spectecx.vsix
```

or `make vsix` from the repository root.
