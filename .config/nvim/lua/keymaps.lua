local opts = { noremap = true, silent = true } -- { nowait = true }
local term_opts = { silent = true }
-- Shorten function names
local keymap = vim.api.nvim_set_keymap
local is_mac = vim.fn.has('macunix')

--Remap space as leader key
keymap("", "<Space>", "<Nop>", opts)
vim.g.mapleader = " "
-- vim.g.maplocalleader = " "

-- Modes
--   normal_mode = "n",
--   insert_mode = "i",
--   visual_mode = "v",
--   visual_block_mode = "x",
--   term_mode = "t",
--   command_mode = "c",

-- NORMAL --
-- loops throught windowso
if is_mac then
  keymap("n", "ø", "<C-w><C-w>", opts) -- next pane
else
  keymap("n", "<A-o>", "<C-w><C-w>", opts) -- next pane
end
keymap("n", "<leader>a", "@q", opts) -- macro @q
keymap("n", "<leader> ", ":noh<CR>", opts) -- clear search hightlight with <space><space>
-- NVim-tree
keymap("n", "<leader>e ", ":NvimTreeToggle<cr>", opts)

-- Navigate buffers
keymap("n", "<TAB>", ":bprevious<CR>", opts)
keymap("n", "<S-TAB>", ":bnext<CR>", opts)
-- new line
keymap("n", "<leader>o", "o<ESC>", opts)
keymap("n", "<leader>O", "O<ESC>", opts)
-- undo all changes in the buffer
keymap("n", "<leader>U", "<cmd>edit!<CR>", opts)
-- reg menu
keymap("n", "<leader>R", "<cmd>:reg<CR>", opts)
-- Keep only current window (Split) and Tab
keymap("n", "<leader>ks", "<cmd>:only<CR>", opts) -- kill all remaining splits
keymap("n", "<leader>kt", "<cmd>BufferLineCloseLeft<CR><cmd>BufferLineCloseRight<CR>", opts) -- kill all remaining BufferLine tabs
keymap("n", "<leader>kb", "<cmd>bd<CR>", opts) -- kill current buffer
-- keep pick buffer
keymap("n", "<leader>gb", "<cmd>BufferLinePick<CR>", opts)
-- execute previous command
keymap("n", "<leader>cp", ":<Up><CR>", opts)

-- INSERT --
keymap("i", "jj", "<ESC>", opts)

-- VISUAL --
keymap("v", "<leader> ", "<ESC>", opts) -- clear search hightlight with <space><space>
-- Stay in indent mode
keymap("v", "<", "<gv", opts)
keymap("v", ">", ">gv", opts)
-- Move text up and down
keymap("n", "<C-j>", ":m '>+1<CR>gv=gv", opts)
keymap("n", "<C-k>", ":m '<-2<CR>gv=gv", opts)
keymap("v", "<C-j>", ":m '>+1<CR>gv=gv", opts)
keymap("v", "<C-k>", ":m '<-2<CR>gv=gv", opts)
-- hightlight and paste, without copying
keymap("v", "p", '"_dP', opts)

-- TERMINAL --
-- Better terminal navigation
keymap("t", "<C-h>", "<C-\\><C-N><C-w>h", term_opts)
keymap("t", "<C-j>", "<C-\\><C-N><C-w>j", term_opts)
keymap("t", "<C-k>", "<C-\\><C-N><C-w>k", term_opts)
keymap("t", "<C-l>", "<C-\\><C-N><C-w>l", term_opts)

-- ## Telescope
keymap("n", "<leader>f", "<cmd>Telescope find_files<cr>", opts)
-- keymap("n", "<leader>f", "<cmd>lua require'telescope.builtin'.find_files(require('telescope.themes').get_dropdown({ previewer = 10 }))<cr>", opts)
keymap("n", "<leader>F", "<cmd>Telescope live_grep<cr>", opts)
-- == PLUGINS == --
-- ## icon-picker
vim.keymap.set("n", "<Leader>ii", "<cmd>IconPickerNormal<cr>", opts)
vim.keymap.set("n", "<Leader>iy", "<cmd>IconPickerYank<cr>", opts) --> Yank the selected icon into register
-- ## buffers
vim.keymap.set("n", "<Leader>tc", "<cmd>tabclose<cr>", opts)
-- Packer, package manager
vim.keymap.set("n", "<leader>pp", "<cmd>source ~/.config/nvim/lua/plugins/init.lua | PackerSync<CR>", opts)
-- Zen
vim.keymap.set("n", "<leader>zz", "<cmd>:ZenMode<CR>", opts)
vim.keymap.set("n", "<leader>zt", "<cmd>:Twilight<CR>", opts)