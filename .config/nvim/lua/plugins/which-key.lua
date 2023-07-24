local status_ok, wk = pcall(require, "which-key")
if not status_ok then
	return
end

vim.cmd([[nnoremap <leader>xd "_dd]])
vim.cmd([[nnoremap <leader>xD "_D]])
vim.cmd([[nnoremap <leader>xc "_cc]])
vim.cmd([[nnoremap <leader>xC "_C]])
vim.cmd([[nnoremap <leader>xx "_x]])
vim.cmd([[vnoremap <leader>xd "_d]])
vim.cmd([[vnoremap <leader>xc "_c]])
wk.register({
	x = {
		name = "Delete without yank",
		d = { '"_dd', "Delete line" },
		D = { '"_D', "Delete line from cursor" },
		c = { '"_cc', "Change line" },
		x = { '"_xx', "Delete char" },
	},
}, { prefix = "<leader>" })

wk.register({
	f = {
		name = "Find",
		-- keys are defined in `plugins/init.lua`
	},
}, { prefix = "," })

wk.register({
	d = {
		name = "DAP",
		-- keys are defined in `plugins/init.lua`
	},
}, { prefix = "," })

wk.register({
	s = {
		name = "Slime",
	},
}, { prefix = "," })

wk.register({
	c = {
		name = "Conjure",
	},
}, { prefix = "," })

wk.register({
	e = {
		name = "Elixir",
	},
}, { prefix = "," })

wk.register({
	e = {
		name = "Conjure",
	},
}, { prefix = "," })

wk.register({
	a = {
		name = "AI",
	},
}, { prefix = "<leader>" })

wk.register({
	y = {
		name = "Terminal",
	},
}, { prefix = "<leader>" })

wk.register({
	t = {
		name = "Terminal",
	},
}, { prefix = "<leader>" })

wk.register({
	T = {
		name = "Toggle",
		w = { "<cmd>set wrap!<CR>", "Line Wrap ↔" },
		r = { "<cmd>set number<cr><cmd>set relativenumber!<CR>", "Relative Number ↔" },
	},
}, { prefix = "<leader>" })

wk.register({
	w = {
		name = "Windfow",
	},
}, { prefix = "<leader>" })
