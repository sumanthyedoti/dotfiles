local status_ok, elixir = pcall(require, "elixir")
if not status_ok then
	return
end

local elixirls = require("elixir.elixirls")

elixir.setup({
	nextls = { enable = true },
	credo = {},
	elixirls = {
		enable = true,
		settings = elixirls.settings({
			dialyzerEnabled = false,
			enableTestLenses = false,
		}),
		on_attach = function(client, bufnr)
			vim.keymap.set("n", "<localleader>eP", ":ElixirFromPipe<cr>", { buffer = true, noremap = true })
			vim.keymap.set("n", "<localleader>ep", ":ElixirToPipe<cr>", { buffer = true, noremap = true })
			vim.keymap.set("v", "<localleader>ee", ":ElixirExpandMacro<cr>", { buffer = true, noremap = true })
		end,
	},
})
