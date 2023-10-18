local utils = require("sumanthyedoti.utils")
local map = utils.map_key
local is_mac = vim.fn.has("macunix")

--Remap space as leader key
map("", "<Space>", "<Nop>")
vim.g.mapleader = " "
vim.g.maplocalleader = "z"

-- Modes
--
--   normal_mode = "n",
--   insert_mode = "i",
--   visual_mode = "v",
--   visual_block_mode = "x",
--   term_mode = "t",
--   command_mode = "c",

-- NORMAL --
-- loops throught windows
if is_mac then
  map("n", "ø", "<C-w><C-w>") -- next pane
else
  map("n", "<A-o>", "<C-w><C-w>") -- next pane
end
map("n", "<leader>q", "@q") -- macro @q
map("n", "<leader> ", "<cmd>noh<CR>") -- clear search hightlight with <space><space>

-- Navigate buffers
map("n", "<S-TAB>", "<cmd>bprevious<CR>")
map("n", "<TAB>", "<cmd>bnext<CR>")

-- execute previous command
map("n", "<leader>P", ":<Up><CR>")

-- add semi-colon
map("n", "<leader>;", "A;<esc>")

-- Disbale 'ZZ' command to save and quit¬
map("n", "Z", ':echom "--> :w :q <-- "<CR>')
map("n", "ZZ", ':echom "--> :w :q <-- "<CR>')

map("n", "<leader>U", "<cmd>edit!<CR>", { desc = "undo all changes in the buffer" })

-- reg menu
map("n", "<leader>or", "<cmd>reg<CR>", { desc = "Registers" })
-- reload nvim conig
map("n", "<leader>R", "<cmd>luafile ~/.config/nvim/init.lua<CR>", { desc = "Reload config" })

-- windows
map("n", "<C-w>_", "<cmd>only<CR>") -- kill all remaining splits
map("n", "<C-w>-", "<cmd>vertical resize -4<CR>") -- decrease width
map("n", "<C-w>=", "<cmd>vertical resize +4<CR>") -- increase width
map("n", "<C-w>.", "<cmd>resize -4<CR>") -- decrease height
map("n", "<C-w>,", "<cmd>resize +4<CR>") -- increase height
map("n", "<leader>w2", "<C-w><C-w><C-w><C-w>") -- kill all remaining splits
map("n", "<leader>w1", "<C-w><C-w>") -- kill all remaining splits

-- window.split navigation
map("n", "<leader>jh", "<C-w>h<CR>")
map("n", "<leader>jj", "<C-w>j<CR>")
map("n", "<leader>jk", "<C-w>k<CR>")
map("n", "<leader>jh", "<C-w>h<CR>")
-- Buffers
map("n", "<leader>bo", "<cmd>BufferLineCloseLeft<CR><cmd>BufferLineCloseRight<CR>") -- kill all remaining BufferLine tabs
map("n", "<leader>bd", "<cmd>BufferLinePickClose<CR>") -- pick close current buffer
map("n", "<leader>bw", "<cmd>Bdelete<CR>") -- close buffer (by vim-bbye)
map("n", "<leader>bR", "<cmd>BufferLineCloseRight<CR>")
map("n", "<leader>bL", "<cmd>BufferLineCloseLeft<CR>")
map("n", "<leader>bg", "<cmd>BufferLinePick<CR>") -- pick buffer / go to selcted buffer
map("n", "C-b", "<cmd>bd<CR>")

-- new line without insert mode
map("n", "[ ", "mzO<Esc>`z", { desc = "Add line above" })
map("n", "] ", "mzo<Esc>`z", { desc = "Add line below" })

-- buffer list
map("n", "<leader>bl", "<cmd>ls<cr>:b")
-- save
map("n", "<C-s>", ":w<CR>")
-- quit
map("n", "<leader>Q", ":q<CR>")

-- increment/descrement
map("n", "-", "<C-x>")
map("n", "=", "<C-a>")

-- reload file
map("n", "<leader>bR", "mZ:Bdelete<CR>`Z", { desc = "Reload the current file" })

-- INSERT --
map("i", "jj", "<ESC>")
map("i", "<C-j>", "<ESC>la") -- move forwards
map("i", "<C-k>", "<ESC>i") -- move backwards
map("i", "<C-h>", "<ESC>dbxi") -- clear one word backwards
map("i", "<C-l>", "<ESC>ldei") -- clear one word forwards

-- VISUAL --
map("v", "<leader> ", "<ESC>") -- clear search hightlight with <space><space>
-- Stay in indent mode
map("v", "<", "<gv")
map("v", ">", ">gv")
-- Move text up and down
map("n", "<a-j>", ":m .+1<CR>==")
map("n", "<a-k>", ":m .-2<CR>==")
map("i", "<a-j>", "<Esc>:m .+1<CR>==gi")
map("i", "<a-k>", "<Esc>:m .-2<CR>==gi")
map("v", "<a-j>", ":m '>+1<CR>gv=gv")
map("v", "<a-k>", ":m '<-2<CR>gv=gv")
-- hightlight and paste, without copying
map("v", "p", '"_dP')

-- TERMINAL --
-- Better terminal navigation
map("t", "<C-h>", "<C-\\><C-N><C-w>h")
map("t", "<C-j>", "<C-\\><C-N><C-w>j")
map("t", "<C-k>", "<C-\\><C-N><C-w>k")
map("t", "<C-l>", "<C-\\><C-N><C-w>l")

-- Sustitute --
-- yank and mark at 'x'
--map("n", "<leader>s ", "mx:%s/<C-r><C-w>/<C-r>0/g<cr>`x")
--map("v", "<leader>s ", '"xymx:%s/<C-r>x/<C-r>0/g<cr>`x')

-- Miscellaneous
map("n", "<leader>so", "<cmd>source %<CR>", { desc = "source current file" })
map("n", "<leader>Y", "<cmd>%y<CR>", { desc = "yank current file" })

-- numerical operations
map(
  "n",
  "<localleader>a",
  "<cmd>s/-\\?\\d\\+/\\=submatch(0) + 1/g<CR>:noh<Cr>",
  { desc = "increase numbers in the line by 1" }
)
map(
  "n",
  "<localleader>x",
  "<cmd>s/-\\?\\d\\+/\\=submatch(0) - 1/g<CR>:noh<Cr>",
  { desc = "descrease numbers in the line by 1" }
)

-- == PLUGINS == --
-- ## icon-picker
map("n", "<Leader>ii", "<cmd>IconPickerNormal<cr>")
map("n", "<Leader>iy", "<cmd>IconPickerYank<cr>") --> Yank the selected icon into register
-- ## buffers
map("n", "<Leader>tc", "<cmd>tabclose<cr>")
-- package manager
map("n", "<leader>pp", ":Lazy sync<CR>")
