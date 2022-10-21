require("user.options")
require("user.keymaps")
require("user.plugins")
require("user.colorscheme")
require("user.cmp")
require("user.lsp")
require("user.telescope")
require("user.dressing")
require("user.treesitter")
require("user.nvim-tree")
require("user.bufferline")
require("user.toggleterm")
require("user.indent")
require("user.autocommands")
require('lualine').setup {
  options = { theme = 'gruvbox' },
}

-- Delete without yanking if preperded by <leader>
vim.cmd [[nnoremap <leader>dd "_dd]]
