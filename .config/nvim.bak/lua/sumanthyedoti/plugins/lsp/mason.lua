-- ## available servers list
-- 爵 https://github.com/williamboman/mason-lspconfig.nvim#available-lsp-servers
--   packages are stored in '~/.local/share/nvim/mason/'

local m_status_ok, mason = pcall(require, "mason")
if not m_status_ok then
  return
end

local mlc_status_ok, mason_lspconfig = pcall(require, "mason-lspconfig")
if not mlc_status_ok then
  return
end

local mason_tool_installer = require("mason-tool-installer")

--[[
-- Mason provides 'Mason*' commands
-- Mason LSP config provides 'Lsp*' commands
--]]

local servers = { -- HERE: LSP servers
  "marksman",
  "mdx_analyzer", -- mdx
  "terraformls",
  --[[ lua ]]
  "lua_ls",
  "clangd",
  "clojure_lsp",
  "rust_analyzer",
  "gopls",
  --[[ Python ]]
  "pyright",
  "elixirls", -- if 'elixir-tools' not enabled
  --[[ JS, HTML, CSS ]]
  "jsonls",
  "ts_ls",
  "html",
  "cssls",
  "astro",
  "emmet_ls",
  "tailwindcss",
  "prismals",
  "graphql",
  "solidity",
  --[[ bash ]]
  "bashls",
  --[[ java ]]
  "jdtls",
  --
  "fsautocomplete",
}

local tools = { -- HERE: Linters and Formatters
  --[[ Lua ]]
  "stylua",
  --[[ Python ]]
  "pylint",
  "isort",
  "black",
  --[[ JS ]]
  "eslint-lsp",
  "eslint_d",
  "prettierd",
  "fantomas",
}

local settings = {
  ui = {
    border = "none",
    width = 0.8,
    height = 0.9,
    icons = {
      package_installed = "",
      package_pending = "⊠",
      package_uninstalled = "⊞",
    },
  },
  log_level = vim.log.levels.INFO,
  max_concurrent_installers = 4,
  keymaps = {
    -- Keymap to expand a package
    toggle_package_expand = "<CR>",
    -- Keymap to install the package under the current cursor position
    install_package = "i",
    -- Keymap to reinstall/update the package under the current cursor position
    update_package = "u",
    -- Keymap to check for new version for the package under the current cursor position
    check_package_version = "c",
    -- Keymap to update all installed packages
    update_all_packages = "U",
    -- Keymap to check which installed packages are outdated
    check_outdated_packages = "C",
    -- Keymap to uninstall a package
    uninstall_package = "X",
    -- Keymap to cancel a package installation
    cancel_installation = "<C-c>",
    -- Keymap to apply language filter
    apply_language_filter = "<C-f>",
  },
}

mason.setup(settings) -- :Mason
mason_lspconfig.setup({ -- :LspInstall
  ensure_installed = servers,
  automatic_installation = true,
})
mason_tool_installer.setup({
  ensure_installed = tools,
})
