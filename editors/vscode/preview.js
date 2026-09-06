// The preview panes: a webview beside the editor showing what the CLI would
// print for the spec being edited, kept in step with the buffer and scrolled to
// whatever definition the cursor is in.
//
// There is one pane per pipeline stage -- IL as `spectecx elab` prints it, SL as
// `spectecx struct` does, PL as `spectecx annotate` does -- and they are
// independent, so two of them can sit side by side on the same spec. Everything
// below is per stage except the workspace events, which are subscribed to once
// and fanned out.
//
// The server does the rendering, over the custom `spectec/preview` request. It
// answers on demand and never pushes, so *when* to re-render is decided here --
// see `scheduleRefresh`, which is the debounce that keeps whole-spec elaboration
// off the per-keystroke path.

const vscode = require("vscode");

const CONFIG_SECTION = "spectec";
const REQUEST = "spectec/preview";
const SPEC_LANGUAGES = ["spectec", "watsup"];

// Each stage is the one before it plus a pass, so a pane costs what the panes
// left of it cost and then some; the labels are the CLI's own names for them.
const STAGES = [
  { id: "il", label: "IL", command: "showIlPreview" },
  { id: "sl", label: "SL", command: "showSlPreview" },
  { id: "pl", label: "PL", command: "showPlPreview" },
];

/** @type {() => any} */
let getClient = () => undefined;
/** @type {vscode.OutputChannel | undefined} */
let log;

function isSpecDocument(document) {
  return !!document && SPEC_LANGUAGES.includes(document.languageId);
}

function settings() {
  return vscode.workspace.getConfiguration(CONFIG_SECTION);
}

// These settings were `spectec.ilPreview.*` back when the IL pane was the only
// one. A value the user actually set under the old name still wins over the new
// default, so upgrading does not silently change their refresh mode.
function setting(name, fallback) {
  const config = settings();
  const explicit = (key) => {
    const info = config.inspect(key);
    if (!info) return undefined;
    return (
      info.workspaceFolderValue ?? info.workspaceValue ?? info.globalValue
    );
  };
  return (
    explicit(`preview.${name}`) ?? explicit(`ilPreview.${name}`) ?? fallback
  );
}

// How the pane paints what it is given -- read fresh on every render, so
// changing either setting takes effect on the next one.
function options() {
  return {
    highlight: setting("highlight", true),
    sourceLabels: setting("sourceLabels", "short"),
  };
}

function toRange(range) {
  return new vscode.Range(
    range.start.line,
    range.start.character,
    range.end.line,
    range.end.character
  );
}

function nonce() {
  let text = "";
  const alphabet =
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789";
  for (let i = 0; i < 32; i += 1) {
    text += alphabet.charAt(Math.floor(Math.random() * alphabet.length));
  }
  return text;
}

function html(label) {
  const key = nonce();
  return `<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="UTF-8">
<meta http-equiv="Content-Security-Policy"
      content="default-src 'none'; style-src 'unsafe-inline'; script-src 'nonce-${key}';">
<style>
  /* One rendered row per line of the render, of exactly this height: the
     entries the server sends are line numbers into that text, so the cursor
     sync and the click-to-source below are arithmetic on it. */
  :root { --row: 1.5em; }
  html, body { height: 100%; margin: 0; padding: 0; }
  body {
    display: flex; flex-direction: column;
    color: var(--vscode-editor-foreground);
    background: var(--vscode-editor-background);
    font-family: var(--vscode-editor-font-family, monospace);
    font-size: var(--vscode-editor-font-size, 13px);
  }
  #banner {
    flex: none; padding: 4px 10px;
    background: var(--vscode-inputValidation-warningBackground, #5a4a1e);
    color: var(--vscode-inputValidation-warningForeground, inherit);
    border-bottom: 1px solid var(--vscode-inputValidation-warningBorder, #7a6a3e);
    font-family: var(--vscode-font-family); font-size: 12px;
  }
  #banner[hidden] { display: none; }
  #scroll { flex: 1 1 auto; overflow: auto; }
  #canvas { position: relative; min-width: 100%; width: max-content; }
  #highlight {
    position: absolute; left: 0; right: 0;
    background: var(--vscode-editor-selectionHighlightBackground, rgba(255,255,255,0.08));
    pointer-events: none;
  }
  #highlight[hidden] { display: none; }
  pre {
    margin: 0; padding: 0 10px;
    white-space: pre; line-height: var(--row);
    cursor: pointer;
  }
  .ln { display: block; height: var(--row); }
  /* The band a definition starts with, so a long render breaks up visually.
     Background only -- anything that took space would cost a row. */
  .ln-src { background: var(--vscode-textCodeBlock-background, rgba(127,127,127,0.09)); }
  #empty { padding: 12px; font-family: var(--vscode-font-family); opacity: 0.8; }
  #empty[hidden] { display: none; }

  /* Three hues and no more: what a thing is named, what stands for a value,
     and what is a literal. Everything else earns its distinction from weight or
     from being dimmed -- a render is nearly all identifiers, so colouring by
     token kind paints the whole pane and tells you nothing.

     The hues come from the theme's own foreground tokens rather than being
     picked here, so they stay readable against whatever the pane is drawn on
     and in a light theme. */
  .t-def   { color: var(--vscode-symbolIcon-classForeground, #4ec9b0); font-weight: 600; }
  .t-fn    { color: var(--vscode-symbolIcon-classForeground, #4ec9b0); }
  .t-var   { color: var(--vscode-symbolIcon-variableForeground, #9cdcfe); }
  .t-str   { color: var(--vscode-debugTokenExpression-string, #ce9178); }
  .t-pat   { color: var(--vscode-debugTokenExpression-string, #ce9178); }
  /* Keywords, the prose SL and PL are written in, and the percent hole: weight
     only. The hole in particular has to stay at full contrast -- it is a
     placeholder standing in for an argument, and a tint of its own lost it
     against the background. */
  .t-kw, .t-prose, .t-hole { font-weight: 600; }
  .t-con, .t-typ, .t-num { color: inherit; }
  .t-op, .t-dash, .t-ord { opacity: 0.7; }
  .t-phantom { opacity: 0.7; font-style: italic; }
  .t-src {
    color: var(--vscode-descriptionForeground, #8a8a8a);
    font-style: italic;
  }
  .t-src:hover { text-decoration: underline; }
</style>
</head>
<body>
  <div id="banner" hidden></div>
  <div id="empty" hidden></div>
  <div id="scroll"><div id="canvas"><div id="highlight" hidden></div><pre id="text"></pre></div></div>
<script nonce="${key}">
  const api = acquireVsCodeApi();
  const label = ${JSON.stringify(label)};
  const scroll = document.getElementById("scroll");
  const pre = document.getElementById("text");
  const highlight = document.getElementById("highlight");
  const banner = document.getElementById("banner");
  const empty = document.getElementById("empty");
  let entries = [];
  let lines = 0;
  let lineHeight = 0;

  // --- Highlighting ---------------------------------------------------------
  //
  // The panes show what a CLI prints, and it stays what the CLI prints: nothing
  // below changes a character of the render, only how each character is
  // painted. That matters because the entries are line numbers into that text,
  // so every line in becomes exactly one row out.
  //
  // The one exception is the region comment heading each definition, whose path
  // is absolute and swamps the line it sits on. It is shortened to a basename,
  // or dropped, per the \`sourceLabels\` setting; the full text stays in the
  // tooltip either way.

  // ";; /path/to/file.spectec:8:1-15:17:", or the same with one position. The
  // path is matched greedily so the split lands on the rightmost line:column
  // pair -- a path may itself contain a colon, the range that follows may not.
  const SOURCE = /^(\\s*);;\\s+(.*):(\\d+):(\\d+)(?:-(\\d+):(\\d+))?:?$/;

  // What a line leads with, which the scan below cannot see for itself: an
  // ordinal in prose, the keyword opening a definition and the name it binds, a
  // premise dash, a variant bar.
  const ORDINAL = /^(\\s*)(\\d+\\.)(\\s*)/;
  const HEAD =
    /^(\\s*)(syntax|relation|rule|clause|grammar|prose|def|dec|var)(\\s+)([A-Za-z_$][\\w'$]*)?/;
  const PREMISE = /^(\\s*)(--)(\\s+)(if|let|rel|else|otherwise|iff)?\\b/;
  const VARIANT = /^(\\s*)(\\|)(\\s+)([A-Za-z_][\\w']*)?/;

  // Everything else, in priority order at each position: a literal before the
  // words inside it, a prose phrase before the words it is made of, a keyword
  // before the identifier it would otherwise be taken for.
  const TOKEN = new RegExp(
    [
      '(?<str>"(?:[^"\\\\\\\\]|\\\\\\\\.)*")',
      "(?<pat>\`[^\`]*\`)",
      "(?<prose>\\\\b(?:Case analysis on|Check let|Result in|The relation holds" +
        "|does not hold|matches pattern|has type|is in|Destruct|Otherwise" +
        "|Return|Debug|Case|Else|Then|then|If|Let|Try|Arm|into|holds|be)\\\\b)",
      "(?<kw>\\\\b(?:syntax|relation|rule|clause|grammar|prose|hint|def|dec|var" +
        "|from|matches|otherwise|iff|if|let|rel|else)\\\\b)",
      "(?<fn>\\\\$[A-Za-z_][\\\\w'$]*)",
      "(?<phantom>\\\\bPhantom#\\\\d+\\\\b)",
      "(?<hole>%(?:latex)?)",
      "(?<num>\\\\b\\\\d+\\\\b)",
      "(?<typ>\\\\b(?:bool|nat|int|rat|real|text)\\\\b)",
      "(?<con>\\\\b[A-Z][\\\\w']*)",
      "(?<var>\\\\b[a-z_][\\\\w']*)",
      "(?<op>\\\\|-|==>|-->|->|=>|<-|<:|::|=\\\\/=|>=|<=|::=|:=|\\\\+\\\\+|\\\\||;|--)",
    ].join("|"),
    "g"
  );

  function esc(text) {
    return text.replace(/[&<>"]/g, (c) =>
      c === "&" ? "&amp;" : c === "<" ? "&lt;" : c === ">" ? "&gt;" : "&quot;"
    );
  }

  function paint(cls, text) {
    return '<span class="t-' + cls + '">' + esc(text) + "</span>";
  }

  // A match's class is whichever named group took part in it.
  function classOf(groups) {
    for (const name in groups) {
      if (groups[name] !== undefined) return name;
    }
    return "op";
  }

  function scan(text) {
    let out = "";
    let last = 0;
    let match;
    TOKEN.lastIndex = 0;
    while ((match = TOKEN.exec(text))) {
      if (match.index > last) out += esc(text.slice(last, match.index));
      out += paint(classOf(match.groups), match[0]);
      last = match.index + match[0].length;
    }
    return out + esc(text.slice(last));
  }

  // A leading match painted with one class per capture after the indent.
  function prefix(match, classes) {
    let out = esc(match[1]);
    for (let i = 0; i < classes.length; i += 1) {
      const piece = match[i + 2];
      if (piece === undefined) continue;
      out += classes[i] ? paint(classes[i], piece) : esc(piece);
    }
    return out;
  }

  function sourceHtml(match, mode) {
    if (mode === "hidden") return "";
    const whole = match[0];
    const path = match[2];
    const where =
      match[5] === undefined
        ? match[3] + ":" + match[4]
        : match[3] + ":" + match[4] + "-" + match[5] + ":" + match[6];
    const shown =
      mode === "full" ? path : path.slice(path.lastIndexOf("/") + 1) || path;
    return (
      esc(match[1]) +
      '<span class="t-src" title="' + esc(whole.trim()) + '">' +
      esc(";; " + shown + ":" + where) +
      "</span>"
    );
  }

  function renderLine(raw, options) {
    const source = SOURCE.exec(raw);
    if (source) {
      // The band marks where a definition starts, so only an unindented header
      // gets one -- the ones nested inside a definition would only stripe it.
      const band =
        !source[1] && options.sourceLabels !== "hidden" ? " ln-src" : "";
      return {
        cls: "ln" + band,
        html: sourceHtml(source, options.sourceLabels),
      };
    }
    if (!options.highlight) return { cls: "ln", html: esc(raw) };

    let out = "";
    let rest = raw;
    const take = (match, classes) => {
      out += prefix(match, classes);
      rest = rest.slice(match[0].length);
    };

    const ordinal = ORDINAL.exec(rest);
    if (ordinal) take(ordinal, ["ord", ""]);

    const head = HEAD.exec(rest);
    const premise = PREMISE.exec(rest);
    const variant = VARIANT.exec(rest);
    if (head) take(head, ["kw", "", "def"]);
    else if (premise) take(premise, ["dash", "", "kw"]);
    else if (variant) take(variant, ["dash", "", "def"]);

    return { cls: "ln", html: out + scan(rest) };
  }

  function indentOf(line) {
    let i = 0;
    while (i < line.length && line[i] === " ") i += 1;
    return i;
  }

  function paintInto(text, options) {
    const raw = text ? text.split("\\n") : [];
    lines = raw.length;
    indents = raw.map(indentOf);
    pre.innerHTML = raw
      .map((line) => {
        const { cls, html } = renderLine(line, options);
        return '<span class="' + cls + '">' + html + "</span>";
      })
      .join("");
  }

  // --- Geometry -------------------------------------------------------------

  // Every row is \`--row\` tall by construction, so one of them is the measure.
  function measure() {
    const first = pre.firstElementChild;
    lineHeight = first ? first.getBoundingClientRect().height : 0;
    if (!lineHeight && lines > 0) {
      lineHeight = pre.getBoundingClientRect().height / lines;
    }
  }

  // How many rows the entry starting at that line covers: everything under it,
  // which is everything up to the next entry indented no deeper. For a branch
  // of a case analysis that is the whole branch, premises and all.
  function spanOf(line) {
    const index = entries.findIndex((entry) => entry.line === line);
    if (index < 0) return 1;
    const depth = indents[line] || 0;
    for (let i = index + 1; i < entries.length; i += 1) {
      if ((indents[entries[i].line] || 0) <= depth) {
        return Math.max(1, entries[i].line - line);
      }
    }
    return Math.max(1, lines - line);
  }

  function scrollToLine(line) {
    if (!lineHeight) return;
    const top = line * lineHeight;
    highlight.style.top = top + "px";
    highlight.style.height = spanOf(line) * lineHeight + "px";
    highlight.hidden = false;
    // Only when it has gone out of view, so scrolling the preview by hand is
    // not fought by every cursor move.
    const above = top < scroll.scrollTop;
    const below = top > scroll.scrollTop + scroll.clientHeight - lineHeight;
    if (above || below) {
      scroll.scrollTop = Math.max(0, top - scroll.clientHeight / 3);
    }
  }

  window.addEventListener("message", (event) => {
    const message = event.data;
    if (message.type === "render") {
      entries = message.entries || [];
      paintInto(message.text || "", message.options || {});
      measure();
      highlight.hidden = true;
      const reason = message.reason ? message.reason.message : "";
      banner.textContent = reason ? "Stale \\u2014 " + reason : "Stale";
      banner.hidden = !message.stale;
      empty.hidden = !!message.text;
      empty.textContent = message.text
        ? ""
        : reason
          ? "No " + label + " yet: " + reason
          : "Nothing to show yet.";
    } else if (message.type === "scrollTo") {
      scrollToLine(message.line);
    }
  });

  pre.addEventListener("click", (event) => {
    if (!lineHeight) return;
    const top = pre.getBoundingClientRect().top;
    api.postMessage({
      type: "reveal",
      line: Math.floor((event.clientY - top) / lineHeight),
    });
  });
</script>
</body>
</html>`;
}

// One pane. The state below is exactly what was module-level when IL was the
// only stage; a stage owns a copy of it rather than sharing one.
function createPreview(stage) {
  /** @type {vscode.WebviewPanel | undefined} */
  let panel;
  /** @type {vscode.Uri | undefined} */
  let tracked;
  /** @type {any[]} */
  let entries = [];
  /** @type {NodeJS.Timeout | undefined} */
  let pending;

  // The line to scroll to for a cursor at `line` of `path`: the innermost region
  // containing it, so a rule wins over the relation it belongs to, and a
  // premise over the rule. Two regions can be equally small -- a rule's
  // conclusion is both what a branch of a case analysis is guarded by and what
  // it results in -- and then the first wins, which is the one printed higher
  // up and so the head of what the cursor is on.
  // Falling back to the last definition that starts above the cursor keeps the
  // preview roughly in place when the cursor sits between definitions.
  function entryFor(path, line) {
    let best;
    let bestSpan = Infinity;
    let fallback;
    for (const entry of entries) {
      if (entry.path !== path) continue;
      const { start, end } = entry.region.range;
      if (start.line <= line) fallback = entry;
      if (start.line <= line && line <= end.line) {
        const span = end.line - start.line;
        if (span < bestSpan) {
          best = entry;
          bestSpan = span;
        }
      }
    }
    return best || fallback;
  }

  // Entries arrive in rendered order, so the definition a preview line belongs
  // to is the last one at or above it.
  function entryAtPreviewLine(line) {
    let target;
    for (const entry of entries) {
      if (entry.line > line) break;
      target = entry;
    }
    return target;
  }

  async function reveal(previewLine) {
    const target = entryAtPreviewLine(previewLine);
    if (!target) return;
    const range = toRange(target.region.range);
    const document = await vscode.workspace.openTextDocument(
      vscode.Uri.parse(target.region.uri)
    );
    const editor = await vscode.window.showTextDocument(document, {
      viewColumn: vscode.ViewColumn.One,
    });
    editor.selection = new vscode.Selection(range.start, range.start);
    editor.revealRange(
      range,
      vscode.TextEditorRevealType.InCenterIfOutsideViewport
    );
  }

  function syncToCursor() {
    if (!panel || !tracked) return;
    const editor = vscode.window.visibleTextEditors.find(
      (candidate) => candidate.document.uri.toString() === tracked.toString()
    );
    if (!editor) return;
    const target = entryFor(tracked.fsPath, editor.selection.active.line);
    if (target) {
      panel.webview.postMessage({ type: "scrollTo", line: target.line });
    }
  }

  async function refresh() {
    if (!panel || !tracked) return;
    const client = getClient();
    if (!client) {
      panel.webview.postMessage({
        type: "render",
        text: "",
        stale: true,
        reason: { message: "the language server is not running" },
        entries: [],
        options: options(),
      });
      return;
    }
    let result;
    try {
      result = await client.sendRequest(REQUEST, {
        textDocument: { uri: tracked.toString() },
        stage: stage.id,
      });
    } catch (err) {
      log?.appendLine(`${stage.label} preview request failed: ${err}`);
      return;
    }
    if (!result) return;
    // Compare by path, not by URI string: the server spells a URI from a
    // filesystem path and VS Code spells its own, and the two need not match
    // character for character.
    entries = (result.entries || []).map((entry) => ({
      ...entry,
      path: vscode.Uri.parse(entry.region.uri).fsPath,
    }));
    panel.webview.postMessage({
      type: "render",
      ...result,
      entries,
      options: options(),
    });
    syncToCursor();
  }

  // Whole-spec elaboration is the very thing the server refuses to do per
  // keystroke, so the preview coalesces: one render once typing pauses. Setting
  // the refresh mode to `save` opts out of this entirely.
  function scheduleRefresh() {
    if (setting("refresh", "live") !== "live") return;
    clearTimeout(pending);
    pending = setTimeout(refresh, setting("debounce", 400));
  }

  // The pane names the file it is showing, which is only ever a spec file.
  function retitle() {
    if (!panel || !tracked) return;
    panel.title = `${stage.label}: ${tracked.path.split("/").pop()}`;
  }

  function track(document) {
    if (!isSpecDocument(document)) return;
    tracked = document.uri;
    retitle();
  }

  function show() {
    const editor = vscode.window.activeTextEditor;
    if (editor) track(editor.document);
    if (!tracked) {
      vscode.window.showInformationMessage(
        `SpecTecX: open a SpecTecX file to preview its ${stage.label}.`
      );
      return;
    }
    if (panel) {
      panel.reveal(vscode.ViewColumn.Beside, true);
      refresh();
      return;
    }
    panel = vscode.window.createWebviewPanel(
      `spectecPreview.${stage.id}`,
      `${stage.label} Preview`,
      { viewColumn: vscode.ViewColumn.Beside, preserveFocus: true },
      { enableScripts: true, retainContextWhenHidden: true }
    );
    panel.webview.html = html(stage.label);
    panel.webview.onDidReceiveMessage((message) => {
      if (message && message.type === "reveal") reveal(message.line);
    });
    panel.onDidDispose(() => {
      panel = undefined;
      entries = [];
      clearTimeout(pending);
    });
    // `track` above had no panel to name yet.
    retitle();
    refresh();
  }

  return {
    show,
    refreshIfOpen: () => {
      if (panel) refresh();
    },
    onActiveEditor: (editor) => {
      if (!panel || !isSpecDocument(editor.document)) return;
      if (editor.document.uri.toString() === tracked?.toString()) return;
      track(editor.document);
      refresh();
    },
    onSave: () => {
      if (panel) refresh();
    },
    onChange: () => {
      if (panel) scheduleRefresh();
    },
    onSelection: (event) => {
      if (!panel || !tracked) return;
      if (event.textEditor.document.uri.toString() === tracked.toString()) {
        syncToCursor();
      }
    },
  };
}

const previews = STAGES.map((stage) => [stage, createPreview(stage)]);

/**
 * @param {vscode.ExtensionContext} context
 * @param {() => any} client  the running language client, if there is one
 * @param {vscode.OutputChannel} channel
 */
function activate(context, client, channel) {
  getClient = client;
  log = channel;
  for (const [stage, preview] of previews) {
    context.subscriptions.push(
      vscode.commands.registerCommand(
        `${CONFIG_SECTION}.${stage.command}`,
        preview.show
      )
    );
  }
  const each = (method) => (argument) => {
    for (const [, preview] of previews) preview[method](argument);
  };
  context.subscriptions.push(
    vscode.window.onDidChangeActiveTextEditor((editor) => {
      if (editor) each("onActiveEditor")(editor);
    }),
    // Any file of the spec, not just the tracked one: the render covers the
    // whole spec, so a sibling's edit changes it too.
    vscode.workspace.onDidSaveTextDocument((document) => {
      if (isSpecDocument(document)) each("onSave")(document);
    }),
    vscode.workspace.onDidChangeTextDocument((event) => {
      if (isSpecDocument(event.document)) each("onChange")(event);
    }),
    vscode.window.onDidChangeTextEditorSelection(each("onSelection")),
    // Only the render options are read per render; a pane already showing text
    // is holding the old ones, so it is asked again.
    vscode.workspace.onDidChangeConfiguration((event) => {
      if (event.affectsConfiguration(CONFIG_SECTION)) each("refreshIfOpen")();
    })
  );
}

// The client is replaced on restart, so the previews are told about the new one
// rather than holding the old.
function setClient(client) {
  getClient = client;
  for (const [, preview] of previews) preview.refreshIfOpen();
}

module.exports = { activate, setClient };
