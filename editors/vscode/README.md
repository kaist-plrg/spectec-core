# SpecTecX for VS Code

## What is this?

Editor support for SpecTecX (`.spectec`) files in
VS Code and VSCodium. The extension provides syntax highlighting, comment
toggling, and bracket pairs. Connecting `spectecx-lsp` adds diagnostics,
completion, navigation, renaming, and pipeline previews.

SpecTecX highlighting is enabled by default. The alternative
**SpecTecX (Contextual)** grammar highlights each declaration's body in context.

## How do I use it?

1. Download `spectecx.vsix` from the
   [releases page](https://github.com/kaist-plrg/spectecx/releases) and install it:

   ```bash
   code --install-extension spectecx.vsix
   ```

   For VSCodium, replace `code` with `codium`.

2. Build the language server from this repository's root:

   ```bash
   make lsp
   ```

   This creates `spectecx-lsp`. The extension searches open workspace roots,
   then `PATH`. For another location, set `spectec.languageServer.path`:

   ```json
   {
     "spectec.languageServer.path": "/path/to/spectecx-lsp"
   }
   ```

   The path also accepts `~` and `${workspaceFolder}`. Highlighting works
   without the server.

3. Open a spec folder and a `.spectec` file. Use the editor's completion,
   navigation, and rename actions, or open the Command Palette and search
   **SpecTecX** for previews and server controls.

To use the contextual grammar, choose **SpecTecX (Contextual)** from the status
bar's language picker, or add this setting for all `.spectec` files:

```json
{
  "files.associations": { "*.spectec": "spectec" }
}
```

The language server attaches to either grammar. It uses saved file paths;
untitled buffers receive highlighting only. Automatic spec discovery collects
sibling `.spectec` files.

To package the extension from source, run `make vsix` at the repository root.
The package is written to `editors/vscode/spectecx.vsix`.

## What can I do?

| Action | Result |
| --- | --- |
| Check errors | See parse and elaboration diagnostics, with related locations and suggested declarations. |
| Hover a name | Read its declaration, source documentation, and constructor arguments. |
| Go to Definition | Jump to declarations across the spec. |
| Find All References | Find uses across files, including subscripted metavariables. |
| Rename Symbol | Rename declarations and uses while preserving metavariable subscripts. |
| Complete a name | Get suggestions for the current context, ranked by expected type where available. |
| Show parameter hints | See the current application's signature and active argument. |
| Go to Symbol | Browse the current file's declarations and cases. |

Checks cover the whole spec, including unsaved buffers. The server checks on
open and save, and during typing with expensive checks throttled. Navigation
keeps the last successfully parsed symbols when parsing fails; type hints
retain the last successful elaboration.

### Preview the pipeline

Open a spec file, then run one of these Command Palette actions:

| Command | Shows the output of |
| --- | --- |
| **SpecTecX: Show IL Preview** | `spectecx elab` |
| **SpecTecX: Show SL Preview** | `spectecx struct` |
| **SpecTecX: Show PL Preview** | `spectecx annotate` |

Each stage opens its own pane for the whole spec. Move the source cursor to
highlight corresponding output; click mapped output to return to its source.
When rendering fails, the pane keeps its previous output marked **Stale** and
shows the error.

Previews refresh after typing pauses by default. For large specs, set
`spectec.preview.refresh` to `"save"` to refresh on saves instead.

### Adjust the extension

| Setting | Default | Purpose |
| --- | --- | --- |
| `spectec.languageServer.enable` | `true` | Disable for highlighting only. |
| `spectec.languageServer.path` | `""` | Override workspace and `PATH` discovery. |
| `spectec.languageServer.arguments` | `[]` | Pass extra server arguments. |
| `spectec.trace.server` | `"off"` | Log protocol traffic: `off`, `messages`, or `verbose`. |
| `spectec.preview.refresh` | `"live"` | Refresh after typing pauses or on `save`. |
| `spectec.preview.debounce` | `400` | Wait this many milliseconds before live refresh. |
| `spectec.preview.highlight` | `true` | Colour preview text. |
| `spectec.preview.sourceLabels` | `"short"` | Show source paths as `short`, `full`, or `hidden`. |

Changing a `spectec.languageServer.*` setting restarts the server. You can also
run **SpecTecX: Restart Language Server**. For startup failures, use **Open
Settings** or **Show Log** in the warning, or inspect the **SpecTecX** output
channel.

Legacy `spectec.ilPreview.refresh` and `spectec.ilPreview.debounce` values still
apply when their replacement settings are unset.

Uppercase metavariables may look like atoms because highlighting cannot track
parser state. This does not affect diagnostics.
