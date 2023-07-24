local wk = require("which-key")
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

-- local keymap = vim.api.nvim_set_keymap

require("lsp.saga")

-- enable keybinds only for when lsp server available
local on_attach = function(client, bufnum)
	-- keybind options
	-- keymap.set("n", "gd", "<cmd>lua vim.lsp.buf.definition()<CR>") -- see definition and make edits in window
	wk.register({
		g = {
			name = "Goto",
			d = { "<cmd>Lspsaga goto_definition<CR>", "Goto type definition" },
			t = { "<cmd>Lspsaga goto_type_definition<CR>", "Goto definition" },
		},
	}, { prefix = "" })

	wk.register({
		l = {
			name = "LSP",
			a = { "<cmd>Lspsaga code_action<CR>", "Code actions" },
			r = { "<cmd>Lspsaga rename<CR>", "Rename" },
			-- keymap.set("n", "gr", "<cmd>lua vim.lsp.buf.references()<CR>", opts)
			f = { "<cmd>Lspsaga lsp_finder<CR>", "LSP definition and references" },
			o = { "<cmd>Lspsaga outline<CR>", "Outline ↔" },
			-- i = { "<cmd>lua vim.lsp.buf.implementation()<CR>", "Go to implementation" },
			-- D = { "<Cmd>lua vim.lsp.buf.declaration()<CR>", "Go to declaration" },

			--[[ Diagnostics ]]
			-- keymap.set( "n", "dl", '<cmd>lua vim.lsp.diagnostic.show_line_diagnostics({ border = "rounded" })<CR>', opts)
			-- keymap.set("n", "<leader>lq", "<cmd>lua vim.diagnostic.setloclist()<CR>", opts)
			-- keymap.set( "n", "dc", '<cmd>lua vim.lsp.diagnostic.show_cursor_diagnostics({ border = "rounded" })<CR>', opts)
			-- keymap.set("n", "[d", '<cmd>lua vim.diagnostic.goto_prev({ border = "rounded" })<CR>')
			-- keymap.set("n", "]d", '<cmd>lua vim.diagnostic.goto_next({ border = "rounded" })<CR>')
			j = { "<cmd>Lspsaga diagnostic_jump_prev<CR>", "Previous Diagnostic" },
			k = { "<cmd>Lspsaga diagnostic_jump_next<CR>", "Next Diagnostic" },
			l = { "<cmd>Lspsaga show_line_diagnostics ++unfocus<CR>", "Line Diagnostics" },
			c = { "<cmd>Lspsaga show_cursor_diagnostics ++unfocus<CR>", "Cursor diagnostics" },
			-- f = { "<cmd>lua vim.diagnostic.open_float()<CR>", "Float diafnostics" },
			b = { "<cmd>Lspsaga show_buf_diagnostics<CR>", "Buffer Diagnostics" },
			w = { "<cmd>Lspsaga show_workspace_diagnostics<CR>", "Workspace Diagnostics" },

			--[[ Help ]]
			-- keymap("n", "K", "<cmd>lua vim.lsp.buf.hover()<CR>", opts)
			h = { "<cmd>Lspsaga hover_doc<CR>", "Hover Doc" },
			H = { "<cmd>Lspsaga hover_doc ++keep<CR>", "Hover Doc static ↔" },
			s = { "<cmd>lua vim.lsp.buf.signature_help()<CR>", "Signature help" },

			--[[ Peek ]]
			p = { "<cmd>Lspsaga peek_definition<CR>", "Peek definition" },
			t = { "<cmd>Lspsaga peek_type_definition<CR>", "Peek type definition" },
		},
	}, { prefix = "<leader>", buffer = bufnum })

	wk.register({
		lc = {
			name = "LSP Calls hierarchy",
			i = { "<cmd>Lspsaga incoming_calls<CR>", "Incoming calls" },
			o = { "<cmd>Lspsaga outgoing_calls<CR>", "Outgoing calls" },
		},
	}, { prefix = "<leader>", buffer = bufnum })
	-- typescript specific keymaps
	if client.name == "tsserver" then -- HERE: typescript LSP keymaps
		wk.register({
			lT = {
				name = "TypeScript", -- optional group name
				f = { ":TypescriptRenameFile<CR>", "Rename file" },
				o = { ":TypescriptOrganizeImports<CR>", "Organize imports" },
				x = { ":TypescriptRemoveUnused<CR>", "Remove unused" },
				i = { ":TypescriptAddMissingImports<CR>", "Add missing imports" },
				a = { ":TypescriptFixAll<CR>", "Fix all" },
			},
		}, { prefix = "<leader>", buffer = bufnum })
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
			completion = {
				callSnippet = "Replace",
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

lspconfig["pyright"].setup({
	capabilities = capabilities,
	on_attach = on_attach,
	cmd = { "pyright-langserver", "--stdio" },
	filetypes = { "python" },
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
