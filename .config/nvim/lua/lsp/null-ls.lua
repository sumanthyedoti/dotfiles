local null_ls_status_ok, null_ls = pcall(require, "null-ls")
if not null_ls_status_ok then
	return
end

local formatting = null_ls.builtins.formatting
local diagnostics = null_ls.builtins.diagnostics
local completion = null_ls.builtins.completion

vim.cmd([[ command! Format execute 'lua vim.lsp.buf.format({async = true})' ]])

local augroup = vim.api.nvim_create_augroup("LspFormatting", { clear = false })
local event = "BufWritePre" -- "BufWritePre" or "BufWritePost"
local async = event == "BufWritePost"

null_ls.setup({
	debug = false,
	-- 🌐 https://github.com/jose-elias-alvarez/null-ls.nvim/tree/main/lua/null-ls/builtins
	-- 🌐 https://github.com/jose-elias-alvarez/null-ls.nvim/blob/main/doc/BUILTINS.md
	sources = { -- HERE: list
		-- = JS/TS =
		-- formatting.prettier.with({ extra_args = { "--no-semi", "--single-quote", "--jsx-single-quote" } }),
		formatting.prettierd,
		formatting.stylua,
		diagnostics.eslint_d,

		-- = c/cpp =
		formatting.clang_format,

		-- = go =
		formatting.gofmt,
		formatting.goimports,
		formatting.golines,
		diagnostics.golangci_lint,

		-- = elixir =
		formatting.mix,
		-- diagnostics.credo,

		completion.spell,
		completion.luasnip,
		completion.tags,
		completion.vsnip,
		-- require("typescript.extensions.null-ls.code-actions"),
	},
	-- format on save
	on_attach = function(client, bufnr)
		if client.supports_method("textDocument/formatting") then
			vim.keymap.set("n", "<Leader>F", function()
				vim.lsp.buf.format({ bufnr = vim.api.nvim_get_current_buf() })
			end, { buffer = bufnr, desc = "[lsp] format" })

			vim.api.nvim_clear_autocmds({ group = augroup, buffer = bufnr })
			vim.api.nvim_create_autocmd(event, {
				group = augroup,
				buffer = bufnr,
				callback = function()
					vim.lsp.buf.format({
						async = false,
						bufnr = bufnr,
						filter = function(client)
							-- apply whatever logic you want (in this example, we'll only use null-ls)
							return client.name == "null-ls" -- only use null-ls formatter
						end,
					})
				end,
				desc = "[lsp] format on save",
			})
		end
		if client.supports_method("textDocument/rangeFormatting") then
			vim.keymap.set("x", "<Leader>F", function()
				vim.lsp.buf.format({ bufnr = vim.api.nvim_get_current_buf() })
			end, { buffer = bufnr, desc = "[lsp] format" })
		end
	end,
})
