vim.api.nvim_set_keymap("n", "<leader>`", ":Switch<CR>", OPTS)
vim.api.nvim_set_keymap("n", "<leader>~", ":SwitchReverse<CR>", OPTS)
vim.g.switch_mapping = "" -- avoid defalt mapping
vim.g.switch_custom_definitions = {
	{ "_", "-" },
	{ "always", "never" },
	{ "null", "undefined" },
	{ "true", "false" },
	{ "True", "False" },
	{ "t", "nil" },
	{ "foo", "bar", "baz" },
	{ "one", "two" },
	{ "One", "Two" },
}

vim.cmd([[
let b:switch_custom_definitions = [
      \   {
      \     '\<[a-z0-9]\+_\k\+\>': {
      \       '_\(.\)': '\U\1'
      \     },
      \     '\<[a-z0-9]\+[A-Z]\k\+\>': {
      \       '\([A-Z]\)': '_\l\1'
      \     },
      \   }
      \ ]
]])
