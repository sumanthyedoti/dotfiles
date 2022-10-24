-- ## available servers list
-- 爵https://github.com/williamboman/mason-lspconfig.nvim#available-lsp-servers
-- ## servers configuraion
-- 爵https://github.com/neovim/nvim-lspconfig/blob/master/doc/server_configurations.md
--  packages are stored in '~/.local/share/nvim/mason/'

local servers = {
  "sumneko_lua",
  "rust_analyzer",
  "jsonls",
  "cssls",
  "tsserver",
  "html",
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

require("mason").setup(settings)
require("mason-lspconfig").setup({
  ensure_installed = servers,
  automatic_installation = true,
})

local lspconfig_status_ok, lspconfig = pcall(require, "lspconfig")
if not lspconfig_status_ok then
  return
end

local opts = {}

for _, server in pairs(servers) do
  opts = {
    on_attach = require("lsp.handlers").on_attach,
    capabilities = require("lsp.handlers").capabilities,
  }

  server = vim.split(server, "@")[1]

  local require_ok, conf_opts = pcall(require, "lsp.settings." .. server)
  if require_ok then
    opts = vim.tbl_deep_extend("force", conf_opts, opts)
  end

  lspconfig[server].setup(opts)
end