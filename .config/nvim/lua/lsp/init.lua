local status_ok, lspconfig = pcall(require, "lspconfig")
if not status_ok then
	return
end

local cnl_status_ok, cmp_nvim_lsp = pcall(require, "cmp_nvim_lsp")
if not cnl_status_ok then
	return
end

local ts_status_ok, typescript = pcall(require, "typescript")
if not ts_status_ok then
	return
end

local util = require("lspconfig/util")

-- Change the Diagnostic symbols in the sign column (gutter)
local signs = {
	{ name = "DiagnosticSignError", text = "" },
	{ name = "DiagnosticSignWarn", text = "" },
	{ name = "DiagnosticSignHint", text = "⚡" },
	{ name = "DiagnosticSignInfo", text = "" },
}

for _, sign in ipairs(signs) do
	vim.fn.sign_define(sign.name, { texthl = sign.name, text = sign.text, numhl = "" })
end

local config = {
	virtual_text = false,
	-- show signs
	signs = {
		active = signs,
	},
	update_in_insert = true,
	underline = true,
	severity_sort = true,
	float = {
		focusable = false,
		style = "minimal",
		border = "rounded",
		source = "always",
		header = "",
		prefix = "",
	},
}
vim.diagnostic.config(config)
vim.lsp.handlers["textDocument/hover"] = vim.lsp.with(vim.lsp.handlers.hover, {
	border = "rounded",
})
vim.lsp.handlers["textDocument/signatureHelp"] = vim.lsp.with(vim.lsp.handlers.signature_help, {
	border = "rounded",
})

local keymap = vim.keymap
-- local keymap = vim.api.nvim_set_keymap

require("lsp.saga")

-- enable keybinds only for when lsp server available
local on_attach = function(client, bufnr)
	-- keybind options
	local opts = { noremap = true, silent = true, buffer = bufnr }
	-- HERE: LSP keybinds
	keymap.set("n", "gD", "<Cmd>lua vim.lsp.buf.declaration()<CR>", opts) -- got to declaration
	-- keymap.set("n", "gd", "<cmd>lua vim.lsp.buf.definition()<CR>") -- see definition and make edits in window
	keymap.set("n", "gd", "<cmd>Lspsaga goto_definition<CR>") -- see definition and make edits in window
	keymap.set("n", "gt", "<cmd>Lspsaga goto_type_definition<CR>") -- see definition and make edits in window
	keymap.set("n", "gi", "<cmd>lua vim.lsp.buf.implementation()<CR>", opts) -- go to implementation

	--[[ Documentation and signature help ]]
	-- keymap("n", "K", "<cmd>lua vim.lsp.buf.hover()<CR>", opts)
	keymap.set("n", "K", "<cmd>Lspsaga hover_doc<CR>", opts) -- show documentation for what is under cursor
	keymap.set("n", "<leader>K", "<cmd>Lspsaga hover_doc ++keep<CR>", opts) -- show documentation for what is under cursor
	keymap.set("n", "<leader>k", "<cmd>lua vim.lsp.buf.signature_help()<CR>", opts)

	keymap.set("n", "<leader>pd", "<cmd>Lspsaga peek_definition<CR>", opts) -- see definition and make edits in window
	keymap.set("n", "<leader>pt", "<cmdkk>Lspsaga peek_type_definition<CR>", opts) -- see definition and make edits in window

	-- keymap("n", "<leader>la", "<cmd>lua vim.lsp.buf.code_action()<CR>")
	-- keymap("n", "<leader>lr", "<cmd>lua vim.lsp.buf.rename()<CR>")
	keymap.set({ "n", "v" }, "<leader>la", "<cmd>Lspsaga code_action<CR>", opts) -- see available code actions
	keymap.set("n", "<leader>lr", "<cmd>Lspsaga rename<CR>", opts) -- smart rename

	--[[ Diagnostics ]]
	-- keymap.set( "n", "dl", '<cmd>lua vim.lsp.diagnostic.show_line_diagnostics({ border = "rounded" })<CR>', opts)
	-- keymap.set( "n", "dc", '<cmd>lua vim.lsp.diagnostic.show_cursor_diagnostics({ border = "rounded" })<CR>', opts)
	keymap.set("n", "<leader>dl", "<cmd>Lspsaga show_line_diagnostics ++unfocus<CR>", opts) -- show  diagnostics for line
	keymap.set("n", "<leader>dc", "<cmd>Lspsaga show_cursor_diagnostics ++unfocus<CR>", opts) -- show diagnostics for cursor
	keymap.set("n", "<leader>df", "<cmd>lua vim.diagnostic.open_float()<CR>", opts)
	keymap.set("n", "<leader>db", "<cmd>Lspsaga show_buf_diagnostics<CR>", opts) -- show diagnostics for buffer
	keymap.set("n", "<leader>dw", "<cmd>Lspsaga show_workspace_diagnostics<CR>", opts) -- show diagnostics for workspace
	-- keymap.set("n", "[d", '<cmd>lua vim.diagnostic.goto_prev({ border = "rounded" })<CR>')
	-- keymap.set("n", "]d", '<cmd>lua vim.diagnostic.goto_next({ border = "rounded" })<CR>')
	keymap.set("n", "[d", "<cmd>Lspsaga diagnostic_jump_prev<CR>", opts) -- jump to previous diagnostic in buffer
	keymap.set("n", "]d", "<cmd>Lspsaga diagnostic_jump_next<CR>", opts) -- jump to next diagnostic in buffer
	---- Diagnostic jump with filters such as only jumping to an error
	keymap.set("n", "[e", function()
		require("lspsaga.diagnostic"):goto_prev({ severity = vim.diagnostic.severity.ERROR })
	end, opts)
	keymap.set("n", "]e", function()
		require("lspsaga.diagnostic"):goto_next({ severity = vim.diagnostic.severity.ERROR })
	end, opts)

	keymap.set("n", "<leader>lf", "<cmd>Lspsaga lsp_finder<CR>", opts) -- show definition, references

	keymap.set("n", "<leader>ol", "<cmd>Lspsaga outline<CR>", opts) -- toggle outline

	keymap.set("n", "gr", "<cmd>lua vim.lsp.buf.references()<CR>", opts)
	keymap.set("n", "<leader>q", "<cmd>lua vim.diagnostic.setloclist()<CR>", opts)

	-- Call hierarchy
	keymap.set("n", "<leader>Ci", "<cmd>Lspsaga incoming_calls<CR>", opts)
	keymap.set("n", "<leader>CO", "<cmd>Lspsaga outgoing_calls<CR>")

	-- typescript specific keymaps (e.g. rename file and update imports)
	if client.name == "tsserver" then -- HERE: typescript LSP keymaps
		keymap.set("n", "<leader>ltf", ":TypescriptRenameFile<CR>", opts) -- rename file and update imports
		keymap.set("n", "<leader>lto", ":TypescriptOrganizeImports<CR>", opts) -- organize imports (not in youtube nvim video)
		keymap.set("n", "<leader>ltx", ":TypescriptRemoveUnused<CR>", opts) -- remove unused variables (not in youtube nvim video)
		keymap.set("n", "<leader>lti", ":TypescriptAddMissingImports<CR>", opts)
		keymap.set("n", "<leader>lta", ":TypescriptFixAll<CR>", opts)
	end

	-- == cursor hover
	if client.server_capabilities.documentHighlight then
		vim.api.nvim_exec(
			[[
      augroup lsp_document_highlight
        autocmd! * <buffer>
        autocmd CursorHold <buffer> lua vim.lsp.buf.document_highlight()
        autocmd CursorMoved <buffer> lua vim.lsp.buf.clear_references()
      augroup END
    ]],
			false
		)
	end
end

-- used to enable autocompletion (assign to every lsp server config)
local capabilities = cmp_nvim_lsp.default_capabilities()

-- 🌐 https://github.com/neovim/nvim-lspconfig/blob/master/doc/server_configurations.md
-- configure html server
lspconfig["html"].setup({
	capabilities = capabilities,
	on_attach = on_attach,
})

-- configure typescript server with plugin
typescript.setup({
	capabilities = capabilities,
	server = {
		capabilities = capabilities,
		on_attach = on_attach,
	},
})

-- lspconfig.tsserver.setup({
-- 	capabilities = capabilities,
-- 	on_attach = on_attach,
-- 	filetypes = { "typescript", "typescriptreact", "typescript.tsx" },
-- 	cmd = { "typescript-language-server", "--stdio" },
-- })

lspconfig.jsonls.setup({
	settings = {
		json = {
			schemas = require("schemastore").json.schemas({
				select = {},
				ignore = {},
			}),
			validate = { enable = true },
		},
	},
})

-- configure css server
lspconfig["cssls"].setup({
	capabilities = capabilities,
	on_attach = on_attach,
})

-- configure tailwindcss server
lspconfig["tailwindcss"].setup({
	capabilities = capabilities,
	on_attach = on_attach,
})

-- configure lua server (with special settings)
lspconfig["lua_ls"].setup({
	capabilities = capabilities,
	on_attach = on_attach,
	settings = { -- custom settings for lua
		Lua = {
			-- make the language server recognize "vim" global
			diagnostics = {
				globals = { "vim" },
			},
			workspace = {
				-- make language server aware of runtime files
				library = {
					[vim.fn.expand("$VIMRUNTIME/lua")] = true,
					[vim.fn.stdpath("config") .. "/lua"] = true,
				},
			},
		},
	},
})

lspconfig.clojure_lsp.setup({
	capabilities = capabilities,
	on_attach = on_attach,
	cmd = { "clojure-lsp" },
	filetypes = { "clojure", "edn" },
	root_pattern = { "project.clj", "deps.edn", "build.boot", "shadow-cljs.edn", ".git", "bb.edn" },
})

lspconfig.elixirls.setup({
	capabilities = capabilities,
	on_attach = on_attach,
	cmd = { "elixir-ls" },
	flags = {
		debounce_text_changes = 150,
	},
	settings = {
		elixirLS = {
			dialyzerEnabled = false,
			fetchDeps = false,
		},
	},
})

lspconfig.gopls.setup({
	capabilities = capabilities,
	on_attach = on_attach,
	cmd = { "gopls" },
	filetypes = { "go", "gomod" },
	root_dir = util.root_pattern("go.work", "go.mod", ".git"),
	settings = {
		gopls = {
			analyses = {
				unusedparams = true,
			},
			staticcheck = true,
		},
	},
})

lspconfig["clangd"].setup({
	capabilities = capabilities,
	on_attach = on_attach,
	cmd = { "clangd" },
	filetypes = { "c", "cpp" },
	offset_encoding = "utf-32",
})

lspconfig["rust_analyzer"].setup({
	capabilities = capabilities,
	on_attach = on_attach,
	cmd = { "rust-analyzer" },
	filetypes = { "rust" },
	root_dir = util.root_pattern("Cargo.toml", "rust-project.json"),
	settings = {
		["rust-analyzer"] = {
			imports = {
				granularity = {
					group = "module",
				},
				prefix = "self",
			},
			cargo = {
				buildScripts = {
					enable = true,
				},
			},
			procMacro = {
				enable = true,
			},
		},
	},
})

require("lsp.mason")
require("lsp.null-ls")
