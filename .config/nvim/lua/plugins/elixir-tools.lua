local status_ok, elixir = pcall(require, "elixir")
if not status_ok then
	return
end

local elixirls = require("elixir.elixirls")
elixir.setup({
	nextls = {
		enable = false, -- defaults to false
		on_attach = function(client, bufnr)
			-- custom keybinds
		end,
	},
	credo = {
		enable = true, -- defaults to true
		on_attach = function(client, bufnr)
			-- custom keybinds
		end,
	},
	elixirls = {
		enable = true,
		-- cmd = "/usr/local/bin/elixir-ls.sh",
		settings = elixirls.settings({
			dialyzerEnabled = true,
			fetchDeps = false,
			enableTestLenses = false,
			suggestSpecs = false,
		}),
		on_attach = function(client, bufnr)
			vim.keymap.set("n", "<localleader>eP", ":ElixirFromPipe<cr>", { buffer = true, noremap = true })
			vim.keymap.set("n", "<localleader>ep", ":ElixirToPipe<cr>", { buffer = true, noremap = true })
			vim.keymap.set("v", "<localleader>ee", ":ElixirExpandMacro<cr>", { buffer = true, noremap = true })
		end,
	},
})
