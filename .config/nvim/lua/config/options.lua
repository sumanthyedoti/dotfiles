-- Options are automatically loaded before lazy.nvim startup
-- Default options that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/options.lua
-- Add any additional options here

-- File path at top right
-- vim.opt.winbar = "%= %f"

local border = "rounded" -- or "single", "double", "solid"
vim.diagnostic.config({
  float = { border = border },
})

vim.lsp.handlers["textDocument/hover"] = vim.lsp.with(vim.lsp.handlers.hover, {
  border = "rounded", -- Options: "none", "single", "double", "rounded", "solid", "shadow"
})

-- Also configure signature help border (triggered by <C-k> in insert mode)
vim.lsp.handlers["textDocument/signatureHelp"] = vim.lsp.with(vim.lsp.handlers.signature_help, {
  border = "rounded",
})
