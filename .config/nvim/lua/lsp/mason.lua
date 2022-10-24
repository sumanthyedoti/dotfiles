-- ## available servers list
-- 爵https://github.com/williamboman/mason-lspconfig.nvim#available-lsp-servers
-- ## servers configuraion
-- 爵https://github.com/neovim/nvim-lspconfig/blob/master/doc/server_configurations.md
--  packages are stored in '~/.local/share/nvim/mason/'


local m_status_ok, mason = pcall(require, "mason")
if not m_status_ok then
  return
end

local mlc_status_ok, mason_lspconfig = pcall(require, "mason-lspconfig")
if not mlc_status_ok then
  return
end

local servers = { -- HEREL: LSP servers
  "sumneko_lua",
  "rust_analyzer",
  "jsonls",
  "tsserver",
  "html",
  "cssls",
  "tailwindcss",
}

local settings = {
  ui = {
    border = "none",
    icons = {
      package_installed = "◍",
      package_pending = "◍",
      package_uninstalled = "◍",
    },
  },
  log_level = vim.log.levels.INFO,
  max_concurrent_installers = 4,
}

mason.setup(settings)
mason_lspconfig.setup{
  ensure_installed = servers,
  automatic_installation = true,
}
