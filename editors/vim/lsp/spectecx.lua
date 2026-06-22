-- Neovim 0.11+ language-server config, auto-discovered on the runtimepath.
-- Enable with `vim.lsp.enable("spectecx")`; `spectecx-lsp` is found on PATH.
return {
  cmd = { "spectecx-lsp" },
  filetypes = { "spectec" },
  root_markers = { ".git" },
}
