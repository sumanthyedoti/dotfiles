-- ## available servers list
-- 爵 https://github.com/williamboman/mason-lspconfig.nvim#available-lsp-servers
-- ## servers configuraion
-- 爵 https://github.com/neovim/nvim-lspconfig/blob/master/doc/server_configurations.md
--   packages are stored in '~/.local/share/nvim/mason/'

local m_status_ok, mason = pcall(require, "mason")
if not m_status_ok then
	return
end

local mlc_status_ok, mason_lspconfig = pcall(require, "mason-lspconfig")
if not mlc_status_ok then
	return
end

local mnl_status_ok, mason_null_ls = pcall(require, "mason-null-ls")
if not mnl_status_ok then
	return
end

local servers = { -- HEREL: LSP servers
	"sumneko_lua",
	"rust_analyzer",
	"gopls",
	"golangci_lint_ls",
	"jsonls",
	"tsserver",
	"html",
	"cssls",
	"tailwindcss",
	"marksman",
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

mason.setup(settings) -- :Mason
mason_lspconfig.setup({ -- :LspInstall
	ensure_installed = servers,
	automatic_installation = true,
})

-- 🌐 https://github.com/jayp0521/mason-null-ls.nvim#available-null-ls-sources
mason_null_ls.setup({ -- :NullLsInstall
	ensure_installed = {
		"prettierd",
		"stylua",
		"eslint_d",
		"gofmt",
		"golangci_lint_ls",
	},
})
