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
	{ name = "DiagnosticSignHint", text = "ﴞ" },
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
	keymap.set("n", "gf", "<cmd>Lspsaga lsp_finder<CR>", opts) -- show definition, references
	keymap.set("n", "gD", "<Cmd>lua vim.lsp.buf.declaration()<CR>", opts) -- got to declaration
	keymap.set("n", "gd", "<cmd>lua vim.lsp.buf.definition()<CR>") -- see definition and make edits in window
	keymap.set("n", "gpd", "<cmd>Lspsaga peek_definition<CR>", opts) -- see definition and make edits in window
	keymap.set("n", "gi", "<cmd>lua vim.lsp.buf.implementation()<CR>", opts) -- go to implementation
	-- keymap("n", "K", "<cmd>lua vim.lsp.buf.hover()<CR>", opts)
	keymap.set("n", "K", "<cmd>Lspsaga hover_doc<CR>", opts) -- show documentation for what is under cursor
	keymap.set("n", "<C-k>", "<cmd>lua vim.lsp.buf.signature_help()<CR>", opts)

	-- keymap("n", "<leader>ca", "<cmd>lua vim.lsp.buf.code_action()<CR>")
	-- keymap("n", "<leader>rn", "<cmd>lua vim.lsp.buf.rename()<CR>")
	keymap.set("n", "<leader>la", "<cmd>Lspsaga code_action<CR>", opts) -- see available code actions
	keymap.set("n", "<leader>ca", "<cmd>CodeActionMenu<CR>", opts) -- see available code actions
	keymap.set("n", "<leader>lr", "<cmd>Lspsaga rename<CR>", opts) -- smart rename

	-- keymap.set( "n", "dl", '<cmd>lua vim.lsp.diagnostic.show_line_diagnostics({ border = "rounded" })<CR>', opts)
	-- keymap.set( "n", "dc", '<cmd>lua vim.lsp.diagnostic.show_cursor_diagnostics({ border = "rounded" })<CR>', opts)
	keymap.set("n", "<leader>ll", "<cmd>Lspsaga show_line_diagnostics<CR>", opts) -- show  diagnostics for line
	keymap.set("n", "<leader>lc", "<cmd>Lspsaga show_cursor_diagnostics<CR>", opts) -- show diagnostics for cursor

	keymap.set("n", "<leader>do", "<cmd>lua vim.diagnostic.open_float()<CR>", opts)

	-- keymap.set("n", "[d", '<cmd>lua vim.diagnostic.goto_prev({ border = "rounded" })<CR>')
	-- keymap.set("n", "]d", '<cmd>lua vim.diagnostic.goto_next({ border = "rounded" })<CR>')
	keymap.set("n", "[d", "<cmd>Lspsaga diagnostic_jump_prev<CR>", opts) -- jump to previous diagnostic in buffer
	keymap.set("n", "]d", "<cmd>Lspsaga diagnostic_jump_next<CR>", opts) -- jump to next diagnostic in buffer

	keymap.set("n", "<leader>ol", "<cmd>LSoutlineToggle<CR>", opts) -- see outline on right hand side

	keymap.set("n", "gr", "<cmd>lua vim.lsp.buf.references()<CR>", opts)
	keymap.set("n", "<leader>q", "<cmd>lua vim.diagnostic.setloclist()<CR>", opts)

	-- typescript specific keymaps (e.g. rename file and update imports)
	if client.name == "tsserver" then -- HERE: typescript LSP keymaps
		keymap.set("n", "<leader>rf", ":TypescriptRenameFile<CR>", opts) -- rename file and update imports
		keymap.set("n", "<leader>io", ":TypescriptOrganizeImports<CR>", opts) -- organize imports (not in youtube nvim video)
		keymap.set("n", "<leader>ru", ":TypescriptRemoveUnused<CR>", opts) -- remove unused variables (not in youtube nvim video)
		keymap.set("n", "<leader>ai", ":TypescriptAddMissingImports<CR>", opts)
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
local capabilities = cmp_nvim_lsp.default_capabilities(vim.lsp.protocol.make_client_capabilities())

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
lspconfig["sumneko_lua"].setup({
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

lspconfig.elixirls.setup({
	capabilities = capabilities,
	on_attach = on_attach,
	cmd = { "elixir-ls" },
	settings = {
		elixirls = {
			analyses = {
				unusedparams = true,
			},
			staticcheck = true,
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

require("lsp.mason")
require("lsp.null-ls")
