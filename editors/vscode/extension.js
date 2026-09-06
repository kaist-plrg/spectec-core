// What: connect VS Code to spectecx-lsp.
// Use: build with make lsp; open specs.
// Features: language assistance, previews, and server controls.

const fs = require("fs");
const os = require("os");
const path = require("path");
const vscode = require("vscode");
const { LanguageClient } = require("vscode-languageclient/node");
const preview = require("./preview");

const SERVER_NAME = "spectecx-lsp";
const CONFIG_SECTION = "spectec";

/** @type {LanguageClient | undefined} */
let client;
/** @type {vscode.OutputChannel | undefined} */
let log;

function config() {
  return vscode.workspace.getConfiguration(CONFIG_SECTION);
}

function expand(p) {
  let out = p.trim();
  if (out === "~" || out.startsWith(`~${path.sep}`) || out.startsWith("~/")) {
    out = path.join(os.homedir(), out.slice(1));
  }
  const folder = (vscode.workspace.workspaceFolders || [])[0];
  if (folder) {
    out = out.replace(/\$\{workspaceFolder\}/g, folder.uri.fsPath);
  }
  return out;
}

function isExecutableFile(p) {
  try {
    if (!fs.statSync(p).isFile()) return false;
    fs.accessSync(p, fs.constants.X_OK);
    return true;
  } catch {
    return false;
  }
}

function findOnPath(name) {
  const exts =
    process.platform === "win32"
      ? (process.env.PATHEXT || ".EXE").split(path.delimiter)
      : [""];
  for (const dir of (process.env.PATH || "").split(path.delimiter)) {
    if (!dir) continue;
    for (const ext of exts) {
      const candidate = path.join(dir, name + ext);
      if (isExecutableFile(candidate)) return candidate;
    }
  }
  return undefined;
}

// Find the binary produced by make lsp.
function findInWorkspace() {
  for (const folder of vscode.workspace.workspaceFolders || []) {
    const candidate = path.join(folder.uri.fsPath, SERVER_NAME);
    if (isExecutableFile(candidate)) return candidate;
  }
  return undefined;
}

/** @returns {{command: string} | {error: string}} */
function resolveServer() {
  const configured = config().get("languageServer.path", "").trim();
  if (configured) {
    const p = expand(configured);
    // Explicit paths must resolve without fallback discovery.
    if (p.includes(path.sep)) {
      return isExecutableFile(p)
        ? { command: p }
        : {
            error: `\`${CONFIG_SECTION}.languageServer.path\` is not an executable file: ${p}`,
          };
    }
    const found = findOnPath(p);
    return found
      ? { command: found }
      : {
          error: `\`${CONFIG_SECTION}.languageServer.path\` is set to \`${p}\`, which is not on PATH`,
        };
  }
  const found = findInWorkspace() || findOnPath(SERVER_NAME);
  return found
    ? { command: found }
    : { error: `\`${SERVER_NAME}\` is not in this workspace or on PATH` };
}

async function reportMissing(message) {
  log?.appendLine(`server not started: ${message}`);
  const choice = await vscode.window.showWarningMessage(
    `SpecTecX: ${message}. Build it with \`make lsp\` and put it on your PATH, ` +
      `or set \`${CONFIG_SECTION}.languageServer.path\`. Syntax highlighting still works.`,
    "Open Settings",
    "Show Log"
  );
  if (choice === "Open Settings") {
    vscode.commands.executeCommand(
      "workbench.action.openSettings",
      `${CONFIG_SECTION}.languageServer.path`
    );
  } else if (choice === "Show Log") {
    log?.show();
  }
}

// Trigger hints when accepting snippets with arguments.
function triggerParameterHints(result) {
  const items = Array.isArray(result) ? result : result?.items;
  for (const item of items || []) {
    const snippet = item.insertText?.value;
    // Only snippets containing placeholders need parameter hints.
    if (snippet && /\$\{\d/.test(snippet) && !item.command) {
      item.command = {
        command: "editor.action.triggerParameterHints",
        title: "Parameter hints",
      };
    }
  }
  return result;
}

async function start() {
  if (!config().get("languageServer.enable", true)) {
    log?.appendLine(`disabled by ${CONFIG_SECTION}.languageServer.enable`);
    return;
  }
  const resolved = resolveServer();
  if ("error" in resolved) {
    await reportMissing(resolved.error);
    return;
  }
  log?.appendLine(`starting ${resolved.command}`);

  const serverOptions = {
    command: resolved.command,
    args: config().get("languageServer.arguments", []),
  };
  const clientOptions = {
    // Spec discovery requires files with filesystem paths.
    documentSelector: [
      { scheme: "file", language: "spectec" },
      { scheme: "file", language: "watsup" },
    ],
    outputChannel: log,
    middleware: {
      provideCompletionItem: async (document, position, context, token, next) =>
        triggerParameterHints(
          await next(document, position, context, token)
        ),
    },
  };

  client = new LanguageClient(
    CONFIG_SECTION,
    "SpecTecX Language Server",
    serverOptions,
    clientOptions
  );
  try {
    await client.start();
    preview.setClient(() => client);
  } catch (err) {
    client = undefined;
    preview.setClient(() => undefined);
    log?.appendLine(`failed to start: ${err}`);
    vscode.window.showErrorMessage(
      `SpecTecX: could not start ${resolved.command}. See the SpecTecX output channel.`
    );
  }
}

async function stop() {
  const running = client;
  client = undefined;
  preview.setClient(() => undefined);
  await running?.stop();
}

async function restart() {
  await stop();
  await start();
}

/** @param {vscode.ExtensionContext} context */
async function activate(context) {
  log = vscode.window.createOutputChannel("SpecTecX");
  preview.activate(context, () => client, log);
  context.subscriptions.push(
    log,
    vscode.commands.registerCommand(
      `${CONFIG_SECTION}.restartLanguageServer`,
      restart
    ),
    vscode.workspace.onDidChangeConfiguration((event) => {
      if (event.affectsConfiguration(`${CONFIG_SECTION}.languageServer`)) {
        restart();
      }
    })
  );
  await start();
}

async function deactivate() {
  await stop();
}

module.exports = { activate, deactivate };
