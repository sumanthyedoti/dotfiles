require("plugins")
require("options")
require("keymaps")
require("theme")
require("lsp")
require("autocommands")
require("plugins.dashboard")

-- Delete without yanking if preperded by <leader>
vim.cmd([[nnoremap <leader>dd "_dd]])
vim.cmd([[nnoremap <leader>D "_D]])
vim.cmd([[nnoremap <leader>cc "_cc]])
vim.cmd([[nnoremap <leader>C "_C]])
vim.cmd([[nnoremap <leader>x "_x]])
vim.cmd([[vnoremap <leader>d "_d]])
vim.cmd([[vnoremap <leader>c "_c]])

vim.cmd([[let g:slime_target = "tmux"]])
