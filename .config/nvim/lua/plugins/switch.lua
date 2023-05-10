vim.api.nvim_set_keymap("n", "<leader>`", ":Switch<CR>", OPTS)
vim.g.switch_mapping = "" -- avoid defalt mapping
vim.g.switch_custom_definitions = {
	{ "_", "-" },
	{ "null", "undefined" },
}
