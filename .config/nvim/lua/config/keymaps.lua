-- Keymaps are automatically loaded on the VeryLazy event
-- Default keymaps that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/keymaps.lua
-- Add any additional keymaps here

local utils = require("sumanthyedoti.utils")
local map = utils.map_key
-- Navigate buffers
--map("n", "<S-TAB>", "<cmd>bprevious<CR>")
--map("n", "<TAB>", "<cmd>bnext<CR>")

-- INSERT --
map("i", "jj", "<ESC>")

-- Disable s (substitute) in normal and visual mode
vim.keymap.set({ "n", "x" }, "s", "<Nop>")

-- Navigate tabs
map("n", "<leader><TAB>n", "<cmd>tabnew<CR>", { desc = "New Tab" })
map("n", "<leader><TAB>1", "1gt")
map("n", "<leader><TAB>2", "2gt")
map("n", "<leader><TAB>3", "3gt")
map("n", "<leader><TAB>4", "4gt")
map("n", "<leader><TAB>5", "5gt")

-- map("n", ";", ":lua require('snacks').picker.registers()<CR>", { desc = "Registers" })

-- Toggle tabs
local last_tab = nil
vim.api.nvim_create_autocmd("TabLeave", {
  callback = function()
    last_tab = vim.fn.tabpagenr()
  end,
})
vim.keymap.set("n", "<leader><TAB><TAB>", function()
  if last_tab then
    vim.cmd("tabnext " .. last_tab)
  end
end, { desc = "Toggle tab" })
