local status_ok, harpoon = pcall(require, "harpoon")
if not status_ok then
	return
end

local function map(mode, l, r, opts)
	opts = opts or {}
	vim.keymap.set(mode, l, r, opts)
end

local mark = require("harpoon.mark")
local ui = require("harpoon.ui")

map("n", "<leader>ma", mark.add_file, OPTS)
map("n", "<leader>mm", ui.nav_next, OPTS)
map("n", "<leader>mn", ui.nav_prev, OPTS)
map("n", "<leader>m ", ui.toggle_quick_menu, OPTS)
map("n", "<leader>ms", ":Telescope harpoon marks<cr>", OPTS)
