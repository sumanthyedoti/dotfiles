local opts = { noremap = true, silent = true }

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
-- Better window navigation
if is_mac then
  keymap("n", "ø", "<C-w><C-w>", opts) -- next pane
else
  keymap("n", "<A-o>", "<C-w><C-w>", opts) -- next pane
end
keymap("n", "<leader> ", ":noh<CR>", opts) -- clear search hightlight with <space><space>
-- NVim-tree
keymap("n", "<leader>e ", ":NvimTreeToggle<cr>", opts)
keymap("n", "<leader>er", ":NvimTreeCollapse<cr>", opts)

-- Resize with arrows
keymap("n", "<C-j>", ":resize -1<CR><ESC>", opts) -- v -1
keymap("n", "<C-k>", ":resize +1<CR><ESC>", opts) -- v +1
keymap("n", "<C-h>", ":vertical resize -1<CR><ESC>", opts) -- h -1
keymap("n", "<C-l>", ":vertical resize +1<CR><ESC>", opts) -- h +1

-- Navigate buffers
keymap("n", "<S-l>", ":bnext<CR>", opts)
keymap("n", "<S-Left>", ":bnext<CR>", opts)
keymap("n", "<S-Right>", ":bprevious<CR>", opts)

-- INSERT --
keymap("i", "jj", "<ESC>", opts)

-- VISUAL --
-- Stay in indent mode
keymap("v", "<", "<gv", opts)
keymap("v", ">", ">gv", opts)
-- Move text up and down
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

-- ## icon-picker
vim.keymap.set("n", "<Leader>ii", "<cmd>IconPickerNormal<cr>", opts)
vim.keymap.set("n", "<Leader>iy", "<cmd>IconPickerYank<cr>", opts) --> Yank the selected icon into register
-- ## buffers
vim.keymap.set("n", "<Leader>bc", "<cmd>Bdelete<cr>", opts)
