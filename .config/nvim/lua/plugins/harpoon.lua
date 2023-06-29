local status_ok, harpoon = pcall(require, "harpoon")
if not status_ok then
	return
end

local function map(mode, l, r, opts)
	opts = opts or {}
	opts.buffer = bufnr
	vim.keymap.set(mode, l, r, opts)
end

local mark = require("harpoon.mark")
local ui = require("harpoon.ui")

map("n", "<leader>ha", mark.add_file, OPTS)
map("n", "<C-n>", ui.nav_next, OPTS)
map("n", "<C-m>", ui.nav_prev, OPTS)
map("n", "<leader>hm", ui.toggle_quick_menu, OPTS)
map("n", "<leader>hs", ":Telescope harpoon marks<cr>", OPTS)
