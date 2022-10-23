require("options")
require("keymaps")
require("plugins")
require("theme")
require("lsp")
require("autocommands")
require("plugins.dashboard")

-- Delete without yanking if preperded by <leader>
vim.cmd [[nnoremap <leader>dd "_dd]]
