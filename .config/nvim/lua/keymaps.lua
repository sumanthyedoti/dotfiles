OPTS = { noremap = true, silent = true } -- { nowait = true }
local term_OPTS = { silent = true }
-- Shorten function names
local keymap = vim.api.nvim_set_keymap
local is_mac = vim.fn.has("macunix")

--Remap space as leader key
keymap("", "<Space>", "<Nop>", OPTS)
vim.g.mapleader = " "
vim.g.maplocalleader = "z"
-- vim.g.maplocalleader = " "

-- Modes
--   normal_mode = "n",
--   insert_mode = "i",
--   visual_mode = "v",
--   visual_block_mode = "x",
--   term_mode = "t",
--   command_mode = "c",

-- NORMAL --
-- loops throught windows
if is_mac then
	keymap("n", "ø", "<C-w><C-w>", OPTS) -- next pane
else
	keymap("n", "<A-o>", "<C-w><C-w>", OPTS) -- next pane
end
keymap("n", "<leader>a", "@q", OPTS) -- macro @q
keymap("n", "<leader> ", "<cmd>noh<CR>", OPTS) -- clear search hightlight with <space><space>

-- NVim-tree
keymap("n", "<leader>e ", ":NvimTreeToggle<cr>", OPTS)

-- Navigate buffers
keymap("n", "<S-TAB>", "<cmd>bprevious<CR>", OPTS)
keymap("n", "<TAB>", "<cmd>bnext<CR>", OPTS)

-- new line
keymap("n", "<leader>o ", "o<ESC>", OPTS)
keymap("n", "<leader>oo", "O<ESC>", OPTS)
keymap("n", "<leader>cl", "0d$", OPTS)
keymap("n", "<leader>P", "@:<CR>", OPTS) -- last/previous terminal command

-- add semi-colon
keymap("n", "<leader>;", "A;<esc>", OPTS)

-- Disbale 'ZZ' command to save and quit¬
keymap("n", "Z", ':echom "--> :w :q <-- "<CR>', OPTS)
keymap("n", "ZZ", ':echom "--> :w :q <-- "<CR>', OPTS)

-- undo all changes in the buffer
keymap("n", "<leader>U", "<cmd>edit!<CR>", OPTS)

-- reg menu
keymap("n", "<leader>R", "<cmd>:reg<CR>", OPTS)

-- splits
keymap("n", "<leader>sv", "<cmd>vsplit<CR>", OPTS) -- split right
keymap("n", "<leader>sx", "<cmd>split<CR>", OPTS) -- split left
keymap("n", "<leader>s-", "<cmd>only<CR>", OPTS) -- kill all remaining splits

-- window.split navigation
keymap("n", "<leader>jh", "<C-w>h<CR>", OPTS)
keymap("n", "<leader>jj", "<C-w>j<CR>", OPTS)
keymap("n", "<leader>jk", "<C-w>k<CR>", OPTS)
keymap("n", "<leader>jh", "<C-w>h<CR>", OPTS)

-- Buffers
keymap("n", "<leader>bo", "<cmd>BufferLineCloseLeft<CR><cmd>BufferLineCloseRight<CR>", OPTS) -- kill all remaining BufferLine tabs
keymap("n", "<leader>bd", "<cmd>BufferLinePickClose<CR>", OPTS) -- pick close current buffer
keymap("n", "<leader>bc", "<cmd>Bdelete<CR>", OPTS) -- close buffer (by vim-bbye)
keymap("n", "<leader>bR", "<cmd>BufferLineCloseRight<CR>", OPTS)
keymap("n", "<leader>bL", "<cmd>BufferLineCloseLeft<CR>", OPTS)
keymap("n", "<leader>gb", "<cmd>BufferLinePick<CR>", OPTS) -- pick buffer
keymap("n", "C-b", "<cmd>bd<CR>", OPTS)

-- treesitter playground
keymap("n", "<leader>tp", "<cmd>TSPlaygroundToggle<CR>", OPTS) -- kill all remaining splits

-- buffer list
keymap("n", "<leader>bl", "<cmd>ls<cr>:b", OPTS)

-- execute previous command
keymap("n", "<leader>cp", ":<Up><CR>", OPTS)
keymap("n", "<C-s>", ":w<CR>", OPTS)

-- increment/descrement
keymap("n", "-", "<C-x>", OPTS)
keymap("n", "=", "<C-a>", OPTS)

-- INSERT --
keymap("i", "jj", "<ESC>", OPTS)
keymap("i", "<C-j>", "<ESC>i", OPTS) -- move backwards
keymap("i", "<C-k>", "<ESC>la", OPTS) -- move forwards
keymap("i", "<C-h>", "<ESC>dbxi", OPTS) -- clear one word backwards
keymap("i", "<C-l>", "<ESC>ldei", OPTS) -- clear one word forwards

-- VISUAL --
keymap("v", "<leader> ", "<ESC>", OPTS) -- clear search hightlight with <space><space>
-- Stay in indent mode
keymap("v", "<", "<gv", OPTS)
keymap("v", ">", ">gv", OPTS)
-- Move text up and down
keymap("n", "<C-j>", ":m '>+1<CR>gv=gv", OPTS)
keymap("n", "<C-k>", ":m '<-2<CR>gv=gv", OPTS)
keymap("v", "<C-j>", ":m '>+1<CR>gv=gv", OPTS)
keymap("v", "<C-k>", ":m '<-2<CR>gv=gv", OPTS)
-- hightlight and paste, without copying
keymap("v", "p", '"_dP', OPTS)

-- TERMINAL --
-- Better terminal navigation
keymap("t", "<C-h>", "<C-\\><C-N><C-w>h", term_OPTS)
keymap("t", "<C-j>", "<C-\\><C-N><C-w>j", term_OPTS)
keymap("t", "<C-k>", "<C-\\><C-N><C-w>k", term_OPTS)
keymap("t", "<C-l>", "<C-\\><C-N><C-w>l", term_OPTS)

-- Sustitute --
-- yank and mark at 'x'
keymap("n", "<leader>s ", "mx:%s/<C-r><C-w>/<C-r>0/g<cr>`x", term_OPTS)
keymap("v", "<leader>s ", '"xymx:%s/<C-r>x/<C-r>0/g<cr>`x', term_OPTS)

-- Miscellaneous
keymap("n", "<leader>so", "<cmd>source %<CR>", OPTS) -- source current file

-- == PLUGINS == --
-- ## icon-picker
vim.keymap.set("n", "<Leader>ii", "<cmd>IconPickerNormal<cr>", OPTS)
vim.keymap.set("n", "<Leader>iy", "<cmd>IconPickerYank<cr>", OPTS) --> Yank the selected icon into register
-- ## buffers
vim.keymap.set("n", "<Leader>tc", "<cmd>tabclose<cr>", OPTS)
-- package manager
vim.keymap.set("n", "<leader>pp", ":Lazy sync<CR>", OPTS)
-- Slime
vim.keymap.set("n", "<C-c><C-v>", "<cmd>%SlimeSend<CR>", OPTS)
-- LazyGit
vim.keymap.set("n", "<leader>gg", "<cmd>LazyGit<CR>", OPTS)
-- ## Telescope
keymap("n", "<leader>f ", "<cmd>Telescope find_files<cr>", OPTS)
keymap("n", "<leader>f.", "<cmd>Telescope find_files hidden=true<cr>", OPTS)
keymap("n", "<leader>ff", "<cmd>Telescope live_grep<cr>", OPTS)
keymap("n", "<leader>F.", "<cmd>Telescope live_grep hidden=true<cr>", OPTS)
